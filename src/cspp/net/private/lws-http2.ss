(library (cspp net private lws-http2)
  (export make-lws-http2-client
          lws-http2-client-reactor
          lws-http2-client-close!
          lws-http2-request/nonblocking
          lws-http2-release-operation!
          lws-http2-client-pool-metrics)
  (import (cspp chez)
          (cspp utils)
          (cspp net errors)
          (cspp net operation)
          (cspp net private http)
          (cspp net private lws-reactor)
          (cspp net private lws-transport))

  (define-record-type (lws-http2-client %make-lws-http2-client lws-http2-client?)
    (sealed #t)
    (opaque #t)
    (fields (immutable reactor %lws-http2-client-reactor)
            (immutable local-stream-limit lws-http2-client-local-stream-limit)
            (immutable native-start? lws-http2-client-native-start?)
            (mutable next-connection-id lws-http2-client-next-connection-id
                     lws-http2-client-next-connection-id-set!)
            (mutable next-stream-id lws-http2-client-next-stream-id
                     lws-http2-client-next-stream-id-set!)
            (mutable next-generation lws-http2-client-next-generation
                     lws-http2-client-next-generation-set!)
            (mutable origins lws-http2-client-origins lws-http2-client-origins-set!)
            (mutable operations lws-http2-client-operations
                     lws-http2-client-operations-set!)
            (mutable closed? lws-http2-client-closed? lws-http2-client-closed?-set!)))

  (define-record-type (h2-origin %make-h2-origin h2-origin?)
    (sealed #t)
    (opaque #t)
    (fields (immutable key h2-origin-key)
            (immutable connection-id h2-origin-connection-id)
            (immutable mutex h2-origin-mutex)
            (mutable ready? h2-origin-ready? h2-origin-ready?-set!)
            (mutable closing? h2-origin-closing? h2-origin-closing?-set!)
            (mutable failure h2-origin-failure h2-origin-failure-set!)
            (mutable queued h2-origin-queued h2-origin-queued-set!)
            (mutable active h2-origin-active h2-origin-active-set!)))

  (define-record-type (h2-stream %make-h2-stream h2-stream?)
    (sealed #t)
    (opaque #t)
    (fields (immutable client h2-stream-client)
            (immutable origin h2-stream-origin)
            (immutable connection-id h2-stream-connection-id)
            (immutable stream-id h2-stream-stream-id)
            (immutable generation h2-stream-generation)
            (immutable deadline-ms h2-stream-deadline-ms)
            (mutable request h2-stream-request h2-stream-request-set!)
            (mutable response-sink h2-stream-response-sink h2-stream-response-sink-set!)
            (mutable redirect-body? h2-stream-redirect-body? h2-stream-redirect-body?-set!)
            (mutable response-finish h2-stream-response-finish
                     h2-stream-response-finish-set!)
            (mutable body-source h2-stream-body-source h2-stream-body-source-set!)
            (mutable inner h2-stream-inner h2-stream-inner-set!)
            (mutable outer h2-stream-outer h2-stream-outer-set!)
            (mutable headers h2-stream-headers h2-stream-headers-set!)
            (mutable status h2-stream-status h2-stream-status-set!)
            (mutable body-parts h2-stream-body-parts h2-stream-body-parts-set!)
            (mutable body-length h2-stream-body-length h2-stream-body-length-set!)
            (mutable trailers h2-stream-trailers h2-stream-trailers-set!)
            (mutable observed-h2? h2-stream-observed-h2? h2-stream-observed-h2?-set!)
            (mutable body-finished? h2-stream-body-finished? h2-stream-body-finished?-set!)
            (mutable response-finished? h2-stream-response-finished?
                     h2-stream-response-finished?-set!)
            (mutable response h2-stream-response h2-stream-response-set!)
            (mutable failure h2-stream-failure h2-stream-failure-set!)
            (mutable terminal? h2-stream-terminal? h2-stream-terminal?-set!)
            (mutable released? h2-stream-released? h2-stream-released?-set!)))

  (define remove-eq
    (lambda (value value*)
      (remp (lambda (item) (eq? value item)) value*)))

  (define request-deadline
    (lambda (request)
      (http-request-policy-deadline-ms (normalized-http-request-policy request))))

  (define stream-deadline-expired?
    (lambda (stream)
      (let ([deadline (h2-stream-deadline-ms stream)]
            [time (current-time 'time-monotonic)])
        (and deadline
             (>= (+ (* (time-second time) 1000)
                    (quotient (time-nanosecond time) 1000000))
                 deadline)))))

  (define request-origin-key
    (lambda (request)
      (vector (normalized-http-request-host request)
              (normalized-http-request-port request)
              (normalized-http-request-tls? request))))

  (define same-origin-key?
    (lambda (left right)
      (and (string=? (vector-ref left 0) (vector-ref right 0))
           (= (vector-ref left 1) (vector-ref right 1))
           (eqv? (vector-ref left 2) (vector-ref right 2)))))


  (define find-origin
    (lambda (client key)
      (find (lambda (origin) (same-origin-key? key (h2-origin-key origin)))
            (lws-http2-client-origins client))))

  (define ensure-origin!
    (lambda (client request)
      (let ([key (request-origin-key request)])
        (or (find-origin client key)
            (let* ([connection-id (fx1+ (lws-http2-client-next-connection-id client))]
                   [origin (%make-h2-origin key connection-id
                                            (make-mutex 'lws-http2-origin)
                                            #f #f #f '() '())])
              (lws-http2-client-next-connection-id-set! client connection-id)
              (lws-http2-client-origins-set!
               client (cons origin (lws-http2-client-origins client)))
              origin)))))

  (define next-stream-id!
    (lambda (client)
      (let ([identity (fx1+ (lws-http2-client-next-stream-id client))])
        (lws-http2-client-next-stream-id-set! client identity)
        identity)))

  (define next-generation!
    (lambda (client)
      (let ([generation (fx1+ (lws-http2-client-next-generation client))])
        (lws-http2-client-next-generation-set! client generation)
        generation)))

  (define stream-condition
    (lambda (stream message)
      (make-net-error 'lws-http2 'http message (h2-stream-request stream))))

  (define origin-poll-targets
    (lambda (origin)
      (with-mutex (h2-origin-mutex origin)
        (fold-left
         (lambda (targets stream)
           (let ([inner (h2-stream-inner stream)])
             (if (and inner (eq? 'pending (net-operation-state inner)))
                 (append targets (net-operation-poll-targets inner))
                 targets)))
         '()
         (h2-origin-active origin)))))

  (define start-stream!
    (lambda (stream)
      (let* ([client (h2-stream-client stream)]
             [reactor (%lws-http2-client-reactor client)]
             [request (h2-stream-request stream)]
             [inner
              (make-lws-reactor-operation
               reactor 'http2 (h2-stream-connection-id stream)
               (h2-stream-stream-id stream) (h2-stream-generation stream)
               (h2-stream-deadline-ms stream) #t)])
        (h2-stream-inner-set! stream inner)
        (unless (if (lws-http2-client-native-start? client)
                    (lws-transport-start!
                     reactor (h2-stream-connection-id stream)
                     (h2-stream-stream-id stream) (h2-stream-generation stream)
                     request "h2")
                    (lws-reactor-client-acquire!
                     reactor (h2-stream-connection-id stream)
                     (h2-stream-stream-id stream) (h2-stream-generation stream)))
          (h2-stream-failure-set!
           stream (stream-condition stream "libwebsockets rejected HTTP/2 stream")))
        (net-operation-step! inner))))

  (define promote-streams-locked!
    (lambda (origin client)
      (when (and (h2-origin-ready? origin) (not (h2-origin-closing? origin)))
        (let loop ()
          (when (and (pair? (h2-origin-queued origin))
                     (fx< (length (h2-origin-active origin))
                          (lws-http2-client-local-stream-limit client)))
            (let ([stream (car (h2-origin-queued origin))])
              (h2-origin-queued-set! origin (cdr (h2-origin-queued origin)))
              (h2-origin-active-set!
               origin (append (h2-origin-active origin) (list stream)))
              (start-stream! stream)
              (loop)))))))

  (define append-response-body!
    (lambda (stream payload)
      (let ([sink (h2-stream-response-sink stream)]
            [count (bytevector-length payload)])
        (if sink
            (unless ((h2-stream-redirect-body? stream)
                     (h2-stream-status stream) (h2-stream-headers stream))
              (sink payload 0 count))
            (begin
              (h2-stream-body-parts-set!
               stream (cons payload (h2-stream-body-parts stream)))
              (h2-stream-body-length-set!
               stream (fx+ count (h2-stream-body-length stream))))))))

  (define collected-body
    (lambda (stream)
      (let ([body (make-bytevector (h2-stream-body-length stream) 0)])
        (let loop ([part* (reverse (h2-stream-body-parts stream))] [offset 0])
          (unless (null? part*)
            (let ([part (car part*)])
              (bytevector-copy! part 0 body offset (bytevector-length part))
              (loop (cdr part*) (fx+ offset (bytevector-length part))))))
        body)))

  (define finish-response!
    (lambda (stream)
      (unless (h2-stream-response-finished? stream)
        (if (not (h2-stream-observed-h2? stream))
            (h2-stream-failure-set!
             stream (stream-condition stream "HTTP/2 ALPN negotiation failed"))
            (begin
              (when (h2-stream-response-sink stream)
                ((h2-stream-response-finish stream)))
              (h2-stream-response-set!
               stream
               (make-transport-response
                (or (h2-stream-status stream) 0) "" (h2-stream-headers stream)
                (and (not (h2-stream-response-sink stream))
                     (collected-body stream))
                (h2-stream-trailers stream) 'h2 (h2-stream-connection-id stream)))
              (h2-stream-response-finished?-set! stream #t))))))

  (define mark-connection-failed!
    (lambda (stream condition)
      (let ([origin (h2-stream-origin stream)])
        (with-mutex (h2-origin-mutex origin)
          (unless (h2-origin-failure origin)
            (h2-origin-failure-set! origin condition))
          (h2-origin-closing?-set! origin #t)))))

  (define process-writable!
    (lambda (stream)
      (let ([source (h2-stream-body-source stream)])
        (when (and source (not (h2-stream-body-finished? stream)))
          ;; The source runs only for this stream and outside the origin mutex.
          (let ([chunk (source 65536)]
                [reactor (%lws-http2-client-reactor (h2-stream-client stream))])
            (cond
             [(eof-object? chunk)
              (lws-transport-submit-body!
               reactor (h2-stream-connection-id stream)
               (h2-stream-stream-id stream) (h2-stream-generation stream)
               #vu8() #t)
              (h2-stream-body-finished?-set! stream #t)]
             [(bytevector? chunk)
              (lws-transport-submit-body!
               reactor (h2-stream-connection-id stream)
               (h2-stream-stream-id stream) (h2-stream-generation stream)
               chunk #f)]
             [else
              (errorf 'lws-http2 "request body source returned invalid value ~s" chunk)]))))))

  (define process-events!
    (lambda (stream)
      (let* ([reactor (%lws-http2-client-reactor (h2-stream-client stream))]
             [inner (h2-stream-inner stream)]
             [events (if inner (lws-reactor-drain-operation-events! reactor inner) '())])
        (let loop ([event* events])
          (unless (or (null? event*) (h2-stream-failure stream))
            (let* ([event (car event*)]
                   [tag (vector-ref event 0)]
                   [status (vector-ref event 5)]
                   [payload (vector-ref event 6)]
                   [metadata (vector-ref event 7)]
                   [protocol (vector-ref metadata 0)]
                   [scope (vector-ref metadata 3)])
              (guard (condition
                      [else
                       (h2-stream-failure-set! stream condition)
                       (lws-reactor-close-stream!
                        reactor (h2-stream-connection-id stream)
                        (h2-stream-stream-id stream) (h2-stream-generation stream) 1)])
                (when (eq? protocol 'http2)
                  (h2-stream-observed-h2?-set! stream #t))
                (cond
                 [(and (eq? tag 'connected) (fx= status -2000))
                  (let ([origin (h2-stream-origin stream)])
                    (with-mutex (h2-origin-mutex origin)
                      (h2-origin-ready?-set! origin #t)
                      (promote-streams-locked! origin (h2-stream-client stream))))]
                 [(eq? tag 'headers)
                  (if (negative? status)
                      (h2-stream-trailers-set! stream (lws-transport-decode-headers payload))
                      (begin
                        (h2-stream-status-set! stream status)
                        (h2-stream-headers-set! stream (lws-transport-decode-headers payload))))]
                 [(eq? tag 'writable) (process-writable! stream)]
                 [(eq? tag 'readable)
                  ;; The sink runs outside the origin mutex; consumption is per stream.
                  (append-response-body! stream payload)
                  (lws-transport-consume-body!
                   reactor (h2-stream-connection-id stream)
                   (h2-stream-stream-id stream) (h2-stream-generation stream)
                   (bytevector-length payload))]
                 [(eq? tag 'complete) (finish-response! stream)]
                 [(memq tag '(failed closed reset goaway))
                  (let ([condition
                         (stream-condition
                          stream (format "HTTP/2 stream ended with ~a (~a)" tag status))])
                    (h2-stream-failure-set! stream condition)
                    (when (eq? scope 'connection)
                      (mark-connection-failed! stream condition)))]
                 [else (void)]))
              (loop (cdr event*))))))))

  (define advance-one-stream!
    (lambda (stream)
      (let ([inner (h2-stream-inner stream)])
        (when (and inner (eq? 'pending (net-operation-state inner)))
          (net-operation-step! inner))
        (when inner (process-events! stream)))))

  (define advance-stream!
    (lambda (stream)
      (let* ([origin (h2-stream-origin stream)]
             [leader
              (with-mutex (h2-origin-mutex origin)
                (and (not (h2-origin-ready? origin))
                     (pair? (h2-origin-active origin))
                     (car (h2-origin-active origin))))])
        (when (and leader (not (eq? leader stream)))
          (advance-one-stream! leader))
        (advance-one-stream! stream)
        (cond
         [(h2-stream-response-finished? stream)
          (h2-stream-terminal?-set! stream #t)
          (net-operation-completed (h2-stream-response stream))]
         [(h2-stream-failure stream)
          (h2-stream-terminal?-set! stream #t)
          (net-operation-failed (h2-stream-failure stream))]
         [(h2-origin-failure origin)
          (h2-stream-terminal?-set! stream #t)
          (net-operation-failed (h2-origin-failure origin))]
         [(not (h2-stream-inner stream))
          (if (stream-deadline-expired? stream)
              (begin
                (h2-stream-terminal?-set! stream #t)
                (net-operation-failed
                 (make-net-error 'lws-http2 'timeout "HTTP/2 admission deadline expired"
                                 (h2-stream-stream-id stream))))
              (net-operation-pending (origin-poll-targets origin)
                                     (h2-stream-deadline-ms stream)))]
         [(memq (net-operation-state (h2-stream-inner stream)) '(failed cancelled))
          (h2-stream-terminal?-set! stream #t)
          (net-operation-failed (net-operation-condition (h2-stream-inner stream)))]
         [else
          (net-operation-pending (net-operation-poll-targets (h2-stream-inner stream))
                                 (h2-stream-deadline-ms stream))]))))

  (define cancel-stream!
    (lambda (stream)
      (let* ([reactor (%lws-http2-client-reactor (h2-stream-client stream))]
             [origin (h2-stream-origin stream)]
             [inner (h2-stream-inner stream)])
        (if inner
            (begin
              (lws-reactor-close-stream!
               reactor (h2-stream-connection-id stream) (h2-stream-stream-id stream)
               (h2-stream-generation stream) 8)
              (when (eq? 'pending (net-operation-state inner))
                (net-operation-cancel! inner)))
            (with-mutex (h2-origin-mutex origin)
              (h2-origin-queued-set!
               origin (remove-eq stream (h2-origin-queued origin))))))))

  (define release-stream!
    (lambda (stream)
      (unless (h2-stream-released? stream)
        (let* ([client (h2-stream-client stream)]
               [reactor (%lws-http2-client-reactor client)]
               [origin (h2-stream-origin stream)]
               [inner (h2-stream-inner stream)])
          (when (and inner (eq? 'pending (net-operation-state inner)))
            (lws-reactor-close-stream!
             reactor (h2-stream-connection-id stream) (h2-stream-stream-id stream)
             (h2-stream-generation stream) 1)
            (net-operation-cancel! inner))
          (when inner
            (lws-reactor-release-operation! reactor inner)
            (lws-reactor-client-release!
             reactor (h2-stream-connection-id stream) (h2-stream-stream-id stream)
             (h2-stream-generation stream)))
          (with-mutex (h2-origin-mutex origin)
            (h2-origin-active-set!
             origin (remove-eq stream (h2-origin-active origin)))
            (h2-origin-queued-set!
             origin (remove-eq stream (h2-origin-queued origin)))
            (promote-streams-locked! origin client))
          (h2-stream-inner-set! stream #f)
          (h2-stream-request-set! stream #f)
          (h2-stream-response-sink-set! stream #f)
          (h2-stream-response-finish-set! stream #f)
          (h2-stream-body-source-set! stream #f)
          (h2-stream-body-parts-set! stream '())
          (h2-stream-released?-set! stream #t)))))

  #|proc:make-lws-http2-client
The `make-lws-http2-client` procedure creates a direct reactor-backed HTTP/2 transport.
The first three parameters bound native event, payload, and reactor command pools.
`tls-context-handle` is zero or a native TLS context handle. `local-stream-limit` bounds active
logical streams on each origin connection. The optional `native-start?` flag is `#f` only for
deterministic injected-event tests. The return value is an HTTP/2 client.
|#
  (define make-lws-http2-client
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-http2-client event-capacity payload-capacity command-capacity 0 10 #t)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (make-lws-http2-client event-capacity payload-capacity command-capacity
                              tls-context-handle 10 #t)]
      [(event-capacity payload-capacity command-capacity tls-context-handle local-stream-limit)
       (make-lws-http2-client event-capacity payload-capacity command-capacity
                              tls-context-handle local-stream-limit #t)]
      [(event-capacity payload-capacity command-capacity tls-context-handle local-stream-limit
                       native-start?)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity
                                   local-stream-limit]
                [natural? tls-context-handle] [boolean? native-start?])
         (let ([reactor (make-lws-reactor event-capacity payload-capacity command-capacity
                                          tls-context-handle)])
           (lws-reactor-start! reactor)
           (%make-lws-http2-client
            reactor local-stream-limit native-start? 0 0 0 '() '() #f)))]))

  #|proc:lws-http2-client-reactor
The `lws-http2-client-reactor` procedure returns the direct reactor owned by `client`.
The returned reactor is the transport used for all of the client's logical streams.
|#
  (define lws-http2-client-reactor
    (lambda (client)
      (pcheck ([lws-http2-client? client])
        (%lws-http2-client-reactor client))))

  #|proc:lws-http2-request/nonblocking
The `lws-http2-request/nonblocking` procedure enqueues normalized request record `request` on
`client`. `response-sink` is `#f` or a vector containing a write procedure with signature
`(bytevector start count) -> unspecified` and a finish procedure with signature
`() -> unspecified`. The return value is a network operation completing with a transport response
record after LWS observes HTTP/2.
|#
  (define-who lws-http2-request/nonblocking
    (lambda (client request response-sink)
      (pcheck ([lws-http2-client? client] [normalized-http-request? request]
               [(lambda (value) (or (not value) (vector? value))) response-sink])
        (when (lws-http2-client-closed? client)
          (raise-net-error who 'http "HTTP/2 client transport is closed" client))
        (unless (eq? 'h2
                     (http-request-policy-version
                      (normalized-http-request-policy request)))
          (raise-net-error who 'http "expected a normalized HTTP/2 request" request))
        (let* ([origin (ensure-origin! client request)]
               [stream-id (next-stream-id! client)]
               [generation (next-generation! client)]
               [stream
                (%make-h2-stream
                 client origin (h2-origin-connection-id origin) stream-id generation
                 (request-deadline request) request
                 (and response-sink (vector-ref response-sink 0))
                 (if (and response-sink (fx> (vector-length response-sink) 2))
                     (vector-ref response-sink 2) (lambda (s h) #f))
                 (if response-sink (vector-ref response-sink 1) void)
                 (normalized-http-request-body-factory request)
                 #f #f '() #f '() 0 '() #f #f #f #f #f #f #f)]
               [operation #f])
          (set! operation
                (make-net-operation
                 'http2
                 (lambda () (advance-stream! stream))
                 (lambda () (cancel-stream! stream))
                 (lambda () (release-stream! stream))))
          (h2-stream-outer-set! stream operation)
          (lws-http2-client-operations-set!
           client (cons (cons operation stream) (lws-http2-client-operations client)))
          (with-mutex (h2-origin-mutex origin)
            (if (and (not (h2-origin-closing? origin))
                     (or (null? (h2-origin-active origin))
                         (and (h2-origin-ready? origin)
                              (fx< (length (h2-origin-active origin))
                                   (lws-http2-client-local-stream-limit client)))))
                (begin
                  (h2-origin-active-set!
                   origin (append (h2-origin-active origin) (list stream)))
                  (start-stream! stream))
                (h2-origin-queued-set!
                 origin (append (h2-origin-queued origin) (list stream)))))
          operation))))

  #|proc:lws-http2-release-operation!
The `lws-http2-release-operation!` procedure releases terminal `operation` owned by `client`.
The return value is unspecified.
|#
  (define lws-http2-release-operation!
    (lambda (client operation)
      (pcheck ([lws-http2-client? client] [net-operation? operation])
        (let ([entry (assq operation (lws-http2-client-operations client))])
          (when entry
            (release-stream! (cdr entry))
            (lws-http2-client-operations-set!
             client
             (remp (lambda (item) (eq? operation (car item)))
                   (lws-http2-client-operations client)))))
        (void))))

  #|proc:lws-http2-client-close!
The `lws-http2-client-close!` procedure cancels pending streams and closes `client`.
The return value is the same client. Repeated calls are inert.
|#
  (define lws-http2-client-close!
    (lambda (client)
      (pcheck ([lws-http2-client? client])
        (unless (lws-http2-client-closed? client)
          (lws-http2-client-closed?-set! client #t)
          (for-each
           (lambda (entry)
             (let ([operation (car entry)])
               (when (eq? 'pending (net-operation-state operation))
                 (net-operation-cancel! operation))))
           (lws-http2-client-operations client))
          (lws-reactor-shutdown! (%lws-http2-client-reactor client))
          (lws-http2-client-operations-set! client '())
          (lws-http2-client-origins-set! client '()))
        client)))

  #|proc:lws-http2-client-pool-metrics
The `lws-http2-client-pool-metrics` procedure returns reactor pool metrics for `client`.
|#
  (define lws-http2-client-pool-metrics
    (lambda (client)
      (pcheck ([lws-http2-client? client])
        (lws-reactor-pool-metrics (%lws-http2-client-reactor client)))))
  )
