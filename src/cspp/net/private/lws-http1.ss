(library (cspp net private lws-http1)
  (export make-lws-http1-client
          lws-http1-client-close!
          lws-http1-request/nonblocking
          lws-http1-release-operation!
          lws-http1-client-pool-metrics)
  (import (cspp chez)
          (only (cspp utils) define-who natural? pcheck positive-natural?)
          (only (cspp concurrency) abox abox-add1&get!)
          (cspp net errors)
          (cspp net operation)
          (cspp net poll)
          (cspp net private http)
          (cspp net private lws-reactor)
          (cspp net private lws-transport))

  (define-record-type (lws-http1-client %make-lws-http1-client lws-http1-client?)
    (sealed #t)
    (opaque #t)
    (fields (immutable reactor lws-http1-client-reactor)
            (immutable next-id lws-http1-client-next-id)
            (mutable pool lws-http1-client-pool lws-http1-client-pool-set!)
            (mutable active lws-http1-client-active lws-http1-client-active-set!)
            (mutable max-active lws-http1-client-max-active lws-http1-client-max-active-set!)
            (mutable max-idle lws-http1-client-max-idle lws-http1-client-max-idle-set!)
            (mutable idle-timeout-ms lws-http1-client-idle-timeout-ms
                     lws-http1-client-idle-timeout-ms-set!)
            (mutable operations lws-http1-client-operations lws-http1-client-operations-set!)
            (mutable closed? lws-http1-client-closed? lws-http1-client-closed?-set!)))

  (define-record-type (lws-http1-state %make-lws-http1-state lws-http1-state?)
    (sealed #t)
    (opaque #t)
    (fields (immutable client lws-http1-state-client)
            (immutable operation lws-http1-state-operation)
            (immutable request lws-http1-state-request)
            (immutable response-sink lws-http1-state-response-sink)
            (immutable redirect-body? lws-http1-state-redirect-body?)
            (immutable finish lws-http1-state-finish)
            (immutable ready lws-http1-state-ready)
            (immutable connection-id lws-http1-state-connection-id)
            (immutable stream-id lws-http1-state-stream-id)
            (immutable generation lws-http1-state-generation)
            (immutable deadline-ms lws-http1-state-deadline-ms)
            (mutable event-index lws-http1-state-event-index lws-http1-state-event-index-set!)
            (mutable headers lws-http1-state-headers lws-http1-state-headers-set!)
            (mutable status lws-http1-state-status lws-http1-state-status-set!)
            (mutable reason lws-http1-state-reason lws-http1-state-reason-set!)
            (mutable body-parts lws-http1-state-body-parts lws-http1-state-body-parts-set!)
            (mutable body-length lws-http1-state-body-length lws-http1-state-body-length-set!)
            (mutable trailers lws-http1-state-trailers lws-http1-state-trailers-set!)
            (mutable body-source lws-http1-state-body-source lws-http1-state-body-source-set!)
            (mutable body-sent? lws-http1-state-body-sent? lws-http1-state-body-sent?-set!)
            (mutable response-finished? lws-http1-state-response-finished?
                     lws-http1-state-response-finished?-set!)
            (mutable response lws-http1-state-response lws-http1-state-response-set!)
            (mutable observed-version lws-http1-state-observed-version
                     lws-http1-state-observed-version-set!)
            (mutable protocol-failure lws-http1-state-protocol-failure
                     lws-http1-state-protocol-failure-set!)
            (mutable reusable? lws-http1-state-reusable? lws-http1-state-reusable?-set!)
            (mutable pool-key lws-http1-state-pool-key lws-http1-state-pool-key-set!)))

  (define pool-entry
    (lambda (key connection-id idle-at)
      (vector key connection-id idle-at)))

  (define pool-entry-key (lambda (entry) (vector-ref entry 0)))
  (define pool-entry-connection-id (lambda (entry) (vector-ref entry 1)))
  (define pool-entry-idle-at (lambda (entry) (vector-ref entry 2)))

  (define request-field
    (lambda (request index)
      (case index
        [(0) (normalized-http-request-method request)]
        [(1) (normalized-http-request-host request)]
        [(2) (normalized-http-request-port request)]
        [(3) (normalized-http-request-tls? request)]
        [(4) (normalized-http-request-path request)]
        [(5) (normalized-http-request-headers request)]
        [(6) (normalized-http-request-body-factory request)]
        [(8) (if (eq? 'h2
                       (http-request-policy-version
                        (normalized-http-request-policy request)))
                 "h2" "http/1.1")]
        [(9) (http-request-policy-deadline-ms (normalized-http-request-policy request))])))
  (define request-requires-h2?
    (lambda (request)
      (eq? 'h2 (http-request-policy-version (normalized-http-request-policy request)))))
  (define origin-key
    (lambda (request)
      (vector (request-field request 1) (request-field request 2) (request-field request 3)
              (request-field request 8))))

  (define same-origin-key?
    (lambda (left right)
      (and (equal? (vector-ref left 0) (vector-ref right 0))
           (= (vector-ref left 1) (vector-ref right 1))
           (eqv? (vector-ref left 2) (vector-ref right 2))
           (string=? (vector-ref left 3) (vector-ref right 3)))))

  (define remove-idle-expired
    (lambda (client now timeout)
      (lws-http1-client-pool-set!
       client
       (filter (lambda (entry) (< (- now (pool-entry-idle-at entry)) timeout))
               (lws-http1-client-pool client)))))

  (define take-idle!
    (lambda (client key)
      (let loop ([rest (lws-http1-client-pool client)] [kept '()])
        (cond
         [(null? rest)
          (lws-http1-client-pool-set! client (reverse kept))
          #f]
         [(same-origin-key? key (pool-entry-key (car rest)))
          (lws-http1-client-pool-set! client
                                      (append (reverse kept) (cdr rest)))
          (pool-entry-connection-id (car rest))]
         [else (loop (cdr rest) (cons (car rest) kept))]))))

  (define trim-idle!
    (lambda (client)
      (let loop ([rest (lws-http1-client-pool client)] [count 0] [kept '()])
        (if (or (null? rest)
                (fx>= count (lws-http1-client-max-idle client)))
            (lws-http1-client-pool-set! client (reverse kept))
            (loop (cdr rest) (fx1+ count) (cons (car rest) kept))))))

  (define next-generation (abox 0))

  (define current-time-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))
  #|proc:make-lws-http1-client
The `make-lws-http1-client` procedure creates a reactor-backed HTTP/1 client transport.
`event-capacity`, `payload-capacity`, and `command-capacity` bound native and reactor pools.
`tls-context-handle` is zero or a native TLS context. `proxy-address` and `proxy-port` select the
immutable proxy policy for the transport.
The return value is an internal transport client.
|#
  (define make-lws-http1-client
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-http1-client event-capacity payload-capacity command-capacity 0 "" 0 64 8 30000)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (make-lws-http1-client event-capacity payload-capacity command-capacity
                              tls-context-handle "" 0 64 8 30000)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port)
       (make-lws-http1-client event-capacity payload-capacity command-capacity
                              tls-context-handle proxy-address proxy-port 64 8 30000)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port max-active)
       (make-lws-http1-client event-capacity payload-capacity command-capacity
                              tls-context-handle proxy-address proxy-port max-active 8 30000)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port max-active max-idle idle-timeout-ms)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
                [natural? tls-context-handle proxy-port]
                [positive-natural? max-active] [natural? max-idle idle-timeout-ms]
                [string? proxy-address])
         (let ([reactor (make-lws-reactor event-capacity payload-capacity command-capacity
                                          tls-context-handle proxy-address proxy-port)])
           (lws-reactor-start! reactor)
           (%make-lws-http1-client reactor (abox 0) '() 0 max-active max-idle idle-timeout-ms '() #f)))]))

  (define decode-event-headers
    (lambda (state bytes)
      (lws-http1-state-headers-set! state (lws-transport-decode-headers bytes))))

  (define append-response-body!
    (lambda (state bytes)
      (let ([count (bytevector-length bytes)]
            [sink (lws-http1-state-response-sink state)])
        (if sink
            (unless ((lws-http1-state-redirect-body? state)
                     (lws-http1-state-status state) (lws-http1-state-headers state))
              (sink bytes 0 count))
            (begin
              (lws-http1-state-body-parts-set!
               state (cons bytes (lws-http1-state-body-parts state)))
              (lws-http1-state-body-length-set!
               state (+ count (lws-http1-state-body-length state))))))))

  (define fail-operation!
    (lambda (state condition)
      (unless (lws-http1-state-protocol-failure state)
        (lws-http1-state-protocol-failure-set! state condition)
        (guard (ignored [else (void)])
          (net-operation-cancel! (lws-http1-state-operation state))))))

  (define finish-response!
    (lambda (state)
      (unless (lws-http1-state-response-finished? state)
        (let* ([sink (lws-http1-state-response-sink state)]
               [body (if sink
                         #f
                         (let ([answer (make-bytevector
                                        (lws-http1-state-body-length state) 0)])
                           (let fill ([parts (reverse (lws-http1-state-body-parts state))]
                                      [offset 0])
                             (unless (null? parts)
                               (let ([part (car parts)])
                                 (bytevector-copy! part 0 answer offset
                                                   (bytevector-length part))
                                 (fill (cdr parts)
                                       (+ offset (bytevector-length part))))))
                           answer))])
          (when sink ((lws-http1-state-finish state)))
          (lws-http1-state-response-set!
           state
           (make-transport-response
            (lws-http1-state-status state)
            (lws-http1-state-reason state)
            (lws-http1-state-headers state)
            body
            (lws-http1-state-trailers state)
            (or (lws-http1-state-observed-version state) 'h1)
            (lws-http1-state-connection-id state)))
          (lws-http1-state-response-finished?-set! state #t)))))

  (define process-events!
    (lambda (state)
      (let* ([reactor (lws-http1-client-reactor (lws-http1-state-client state))]
             [operation (lws-http1-state-operation state)]
             [events (lws-reactor-drain-operation-events! reactor operation)])
        (when (and (getenv "CSPP_LWS_TRACE") (pair? events))
          (fprintf (current-error-port) "events ~a: ~s\n" (lws-http1-state-stream-id state)
                   (map (lambda (event) (vector-ref event 0)) events)))
        (let loop ([rest events])
          (unless (or (null? rest) (lws-http1-state-protocol-failure state))
            (let* ([event (car rest)]
                   [tag (vector-ref event 0)]
                   [payload (vector-ref event 6)])
              (guard (condition [else (fail-operation! state condition)])
               (cond
               [(eq? tag 'headers)
                (if (negative? (vector-ref event 5))
                    (lws-http1-state-trailers-set!
                     state (lws-transport-decode-headers payload))
                    (begin
                      (lws-http1-state-status-set! state (vector-ref event 5))
                      (decode-event-headers state payload)
                      (let ([ready (lws-http1-state-ready state)])
                        (when ready (ready)))))]
               [(and (eq? tag 'connected) (= (vector-ref event 5) -2000))
                (lws-http1-state-observed-version-set! state 'h2)
                (void)]
               [(eq? tag 'connected)
                (let* ([metadata (vector-ref event 7)]
                       [protocol (and (vector? metadata) (vector-ref metadata 0))])
                  (when (eq? protocol 'http1)
                    (lws-http1-state-observed-version-set! state 'h1)))]
               [(eq? tag 'writable)
                (let ([source (lws-http1-state-body-source state)])
                  (when (and source (not (lws-http1-state-body-sent? state)))
                    (guard (condition
                            [else (fail-operation! state condition)])
                      (let ([chunk (source 65536)])
                        (if (eof-object? chunk)
                          (begin
                            (lws-transport-submit-body!
                             reactor
                             (lws-http1-state-connection-id state)
                             (lws-http1-state-stream-id state)
                             (lws-http1-state-generation state)
                             #vu8() #t)
                            (lws-http1-state-body-sent?-set! state #t))
                          (lws-transport-submit-body!
                           reactor
                           (lws-http1-state-connection-id state)
                           (lws-http1-state-stream-id state)
                           (lws-http1-state-generation state)
                           chunk #f))))))]
               [(eq? tag 'readable)
                (guard (condition
                        [else (fail-operation! state condition)])
                  (append-response-body! state payload)
                  (lws-transport-consume-body!
                   reactor
                   (lws-http1-state-connection-id state)
                   (lws-http1-state-stream-id state)
                   (lws-http1-state-generation state)
                   (bytevector-length payload)))]
               [(eq? tag 'complete)
                (let ([metadata (vector-ref event 7)])
                  (when (and (vector? metadata) (= (vector-length metadata) 4))
                    (lws-http1-state-reusable?-set! state (vector-ref metadata 1))))
                (if (and (request-requires-h2? (lws-http1-state-request state))
                         (not (eq? 'h2 (lws-http1-state-observed-version state))))
                    (lws-http1-state-protocol-failure-set!
                     state
                     (make-net-error 'lws-http1 'http
                                     "HTTP/2 ALPN negotiation failed"
                                     (lws-http1-state-request state)))
                    (finish-response! state))]
               [(memq tag '(failed closed reset goaway))
                (when (and (request-requires-h2? (lws-http1-state-request state))
                           (not (eq? 'h2 (lws-http1-state-observed-version state))))
                  (lws-http1-state-protocol-failure-set!
                   state
                   (make-net-error 'lws-http1 'http
                                   "HTTP/2 ALPN negotiation failed"
                                   (lws-http1-state-request state))))]
               [else (void)]))
              (loop (cdr rest))))))))

  #|proc:lws-http1-request/nonblocking
The `lws-http1-request/nonblocking` procedure starts normalized request record `request` on
`client`. `response-sink` is `#f` or a write/finish procedure vector. The optional `ready`
procedure has signature `() -> unspecified` and runs after final response headers are available.
The return value is
a `net-operation` completing with a transport response record.
|#
  (define-who lws-http1-request/nonblocking
    (case-lambda
      [(client request response-sink)
       (lws-http1-request/nonblocking client request response-sink #f)]
      [(client request response-sink ready)
      (pcheck ([lws-http1-client? client] [normalized-http-request? request]
               [(lambda (value) (or (not value) (vector? value))) response-sink]
               [(lambda (value) (or (not value) (procedure? value))) ready])
        (when (lws-http1-client-closed? client)
          (raise-net-error who 'http "HTTP client transport is closed" client))
        (remove-idle-expired client (current-time-ms)
                             (lws-http1-client-idle-timeout-ms client))
        (when (and (not (pair? (lws-http1-client-pool client)))
                   (fx>= (lws-http1-client-active client)
                         (lws-http1-client-max-active client)))
          (raise-net-error who 'pool "HTTP/1 connection pool is exhausted" request))
        (let* ([body-source (request-field request 6)]
               [alpn (request-field request 8)]
               [key (origin-key request)]
               ;; LWS 4.5.8 closes completed client HTTP transactions; no supported API restarts
               ;; a transaction on an idle WSI, so every HTTP/1 request gets a new connection.
               [reused-id #f]
               [selected-id (abox-add1&get! (lws-http1-client-next-id client))]
               [connection-id selected-id]
               [stream-id selected-id]
               [generation (abox-add1&get! next-generation)]
               [deadline-ms (request-field request 9)]
               [reactor (lws-http1-client-reactor client)]
               [inner (make-lws-reactor-operation reactor 'http1 connection-id stream-id
                                                   generation deadline-ms #t)]
               [state #f]
               [operation #f])
          (set! state
                (%make-lws-http1-state client inner request
                                        (and response-sink (vector-ref response-sink 0))
                                        (if (and response-sink (fx> (vector-length response-sink) 2))
                                            (vector-ref response-sink 2) (lambda (s h) #f))
                                        (if response-sink (vector-ref response-sink 1) void)
                                        ready
                                        connection-id stream-id generation deadline-ms
                                        0 '() #f "" '() 0 '() body-source #f #f #f #f #f #f key))
          (unless (lws-transport-start!
                   reactor connection-id stream-id generation request alpn)
            (raise-net-error who 'http "libwebsockets rejected HTTP request" request))
          (lws-http1-client-active-set!
           client (fx1+ (lws-http1-client-active client)))
          (net-operation-step! inner)
          (set! operation
                (make-net-operation
                 'http1
                 (lambda ()
                   (when (eq? 'pending (net-operation-state inner))
                     (net-operation-step! inner))
                   (process-events! state)
                   (case (net-operation-state inner)
                     [(completed)
                      (if (lws-http1-state-protocol-failure state)
                          (net-operation-failed
                           (lws-http1-state-protocol-failure state))
                          (begin
                            (unless (lws-http1-state-response-finished? state)
                              (finish-response! state))
                            (net-operation-completed
                             (lws-http1-state-response state))))]
                     [(failed cancelled)
                      (net-operation-failed
                       (or (lws-http1-state-protocol-failure state)
                           (net-operation-condition inner)))]
                     [else
                      (net-operation-pending
                       (list (make-poll-target inner '(read))) deadline-ms)]))
                 (lambda ()
                   (net-operation-cancel! inner))
                 (lambda ()
                   (lws-reactor-release-operation! reactor inner))))
          (lws-http1-client-operations-set!
           client (cons (cons operation state) (lws-http1-client-operations client)))
          operation))]))

  #|proc:lws-http1-release-operation!
The `lws-http1-release-operation!` procedure releases terminal operation routing state owned by
`client`. The return value is unspecified.
|#
  (define lws-http1-release-operation!
    (lambda (client operation)
      (pcheck ([lws-http1-client? client] [net-operation? operation])
        (let ([entry (find (lambda (item) (eq? (car item) operation))
                           (lws-http1-client-operations client))])
          (when entry
            (let ([state (cdr entry)]
                  [now (current-time-ms)]
                  [key (lws-http1-state-pool-key (cdr entry))])
              (lws-http1-client-operations-set!
               client (remp (lambda (item) (eq? (car item) operation))
                            (lws-http1-client-operations client)))
              (lws-reactor-release-operation! (lws-http1-client-reactor client) operation)
              ;; Clear all transaction-owned mutable state before dropping the lease.
              (lws-http1-state-event-index-set! state 0)
              (lws-http1-state-headers-set! state '())
              (lws-http1-state-body-parts-set! state '())
              (lws-http1-state-body-length-set! state 0)
              (lws-http1-state-trailers-set! state '())
              (lws-http1-state-body-source-set! state #f)
              (lws-http1-state-response-set! state #f)
              (lws-http1-state-pool-key-set! state #f)
              (lws-http1-client-active-set!
               client (max 0 (fx1- (lws-http1-client-active client))))
              (when (and (lws-http1-state-reusable? state)
                         (not (lws-http1-state-protocol-failure state))
                         (eq? 'completed (net-operation-state operation)))
                (remove-idle-expired client now
                                     (lws-http1-client-idle-timeout-ms client))
                (trim-idle! client)
                (lws-http1-client-pool-set!
                 client
                 (cons (pool-entry key
                                   (lws-http1-state-connection-id state)
                                   now)
                       (lws-http1-client-pool client))))))))))

  #|proc:lws-http1-client-close!
The `lws-http1-client-close!` procedure stops `client` and releases its reactor resources.
The return value is `client`.
|#
  (define lws-http1-client-close!
    (lambda (client)
      (pcheck ([lws-http1-client? client])
        (unless (lws-http1-client-closed? client)
          (lws-http1-client-closed?-set! client #t)
          (lws-reactor-shutdown! (lws-http1-client-reactor client)))
        client)))

  #|proc:lws-http1-client-pool-metrics
The `lws-http1-client-pool-metrics` procedure returns reactor pool metrics for `client`.
|#
  (define lws-http1-client-pool-metrics
    (lambda (client)
      (pcheck ([lws-http1-client? client])
        (lws-reactor-pool-metrics (lws-http1-client-reactor client)))))
  )
