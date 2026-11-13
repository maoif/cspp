(library (cspp net websocket)
  (export websocket-server?
          websocket-listen
          websocket-server-close
          websocket-accept
          websocket-accept/nonblocking
          websocket-options? make-websocket-options
          websocket-options-tls-context websocket-options-subprotocols
          websocket-options-fragment-size
          websocket-options-ping-interval-ms websocket-options-pong-timeout-ms
          websocket-connection?
          websocket-connect
          websocket-close
          websocket-negotiated-subprotocol
          websocket-close-code websocket-close-reason
          websocket-send-text
          websocket-send-binary
          websocket-send-ping
          websocket-send-pong
          websocket-cancel-pending-send!
          websocket-send/nonblocking
          websocket-send-fragment/nonblocking
          websocket-finish-message/nonblocking
          websocket-ping-operation
          websocket-recv
          websocket-recv/nonblocking
          websocket-next-message
          websocket-message?
          websocket-message-type
          websocket-message-data
          call-with-websocket)
  (import (cspp chez)
          (cspp system)
          (cspp utils)
          (only (cspp queue) make-queue queue-empty? queue-push! queue-pop! queue-clear!)
          (cspp net uri)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net poll)
          (cspp net operation)
          (cspp net tls))

  (define websocket-default-timeout-ms 30000)
  (define websocket-no-timeout -1)

  #|record:websocket-options
The `websocket-options` record is immutable connection and listener policy.
It contains an optional TLS context, ordered subprotocol strings, positive fragment size,
and optional positive ping interval and pong timeout in milliseconds.
|#
  (define-record-type (websocket-options %make-websocket-options websocket-options?)
    (sealed #t)
    (opaque #f)
    (fields (immutable tls-context websocket-options-tls-context)
            (immutable subprotocols websocket-options-subprotocols)
            (immutable fragment-size websocket-options-fragment-size)
            (immutable ping-interval-ms websocket-options-ping-interval-ms)
            (immutable pong-timeout-ms websocket-options-pong-timeout-ms)))

  #|record:websocket-server
The `websocket-server` record owns a listener for one host, port, protocol, and options value.
`websocket-server-close` releases the native listener once; later accepts raise an error.
The TLS context in its options remains caller-owned.
|#
  (define-record-type (websocket-server %make-websocket-server websocket-server?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle websocket-server-handle websocket-server-handle-set!)
            (immutable host websocket-server-host)
            (immutable port websocket-server-port)
            (immutable protocol websocket-server-protocol)
            (immutable options websocket-server-options)
            (mutable closed? websocket-server-closed? websocket-server-closed?-set!)))

  #|record:websocket-connection
The `websocket-connection` record owns one client or accepted WebSocket transport.
It records endpoint and negotiated subprotocol, then close code and reason during
shutdown. `websocket-close` releases the handle and makes later send or receive operations fail.
|#
  (define-record-type (websocket-connection %make-websocket-connection websocket-connection?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle websocket-connection-handle websocket-connection-handle-set!)
            (immutable host websocket-connection-host)
            (immutable port websocket-connection-port)
            (immutable path websocket-connection-path)
            (immutable secure? websocket-connection-secure?)
            (immutable protocol websocket-connection-protocol)
            (immutable options websocket-connection-options)
            (mutable negotiated-subprotocol websocket-connection-negotiated-subprotocol
                     websocket-connection-negotiated-subprotocol-set!)
            (mutable close-code websocket-connection-close-code
                     websocket-connection-close-code-set!)
            (mutable close-reason websocket-connection-close-reason
                     websocket-connection-close-reason-set!)
            (immutable deferred websocket-connection-deferred)
            (mutable closed? websocket-connection-closed? websocket-connection-closed?-set!)))

  (define default-websocket-options
    (%make-websocket-options #f '("cspp-websocket") 65536 #f 30000))

  #|record:websocket-message-record
The `websocket-message-record` record is an immutable complete WebSocket message.
Type is `text`, `binary`, `ping`, or `pong`, and data is the copied message bytevector.
|#
  (define-record-type (websocket-message-record %make-websocket-message websocket-message?)
    (sealed #t)
    (opaque #f)
    (fields (immutable type websocket-message-type)
            (immutable data websocket-message-data)))

  (define ensure-success
    (lambda (who x)
      (cond
       [(ffi-error? x)
        (raise-net-error who 'websocket (ffi-error-message x) x)]
       [else x])))

  (define ensure-server-open
    (lambda (who server)
      (when (websocket-server-closed? server)
        (raise-net-error who 'websocket "websocket server is closed" server))))

  (define ensure-connection-open
    (lambda (who conn)
      (when (websocket-connection-closed? conn)
        (raise-net-error who 'websocket "websocket connection is closed" conn))))

  (define normalize-uri
    (lambda (who value)
      (let ([u (cond
                [(uri? value) value]
                [(string? value)
                 (or (string->uri value)
                     (errorf who "invalid websocket URI ~s" value))]
                [else
                 (errorf who "expected websocket URI object or string, given ~s" value)])])
        (unless (member (uri-scheme u) '("ws" "wss"))
          (errorf who "expected ws or wss URI, given ~s" (uri-scheme u)))
        (unless (uri-host u)
          (errorf who "websocket URI is missing a host"))
        u)))

  (define uri-default-port
    (lambda (u)
      (or (uri-port u)
          (if (string=? (uri-scheme u) "wss") 443 80))))

  (define uri-path*
    (lambda (u)
      (let ([path (uri-raw-path u)]
            [query (uri-raw-query u)])
        (string-append
         (if (or (not path) (string=? path "")) "/" path)
         (if query (string-append "?" query) "")))))

  (define websocket-type->int
    (lambda (who type)
      (case type
        [(text) 1]
        [(binary) 2]
        [(ping) 3]
        [(pong) 4]
        [else
         (errorf who "invalid websocket message type ~s" type)])))

  (define message-from-ffi
    (lambda (v)
      (let ([type (vector-ref v 0)]
            [data (vector-ref v 1)])
        (%make-websocket-message
         type
         (if (eq? type 'text)
             (utf8->string data)
             data)))))

  (define recv-result
    (lambda (who x)
      (cond
       [(ffi-would-block? x) (websocket-would-block-result who x)]
       [(ffi-error? x) (ensure-success who x)]
       [(eof-object? x) x]
       [(vector? x) (message-from-ffi x)]
       [else (ensure-success who x)])))

  (define accept-result
    (lambda (who server x)
      (cond
       [(ffi-error? x) (ensure-success who x)]
       [(ffi-would-block? x) (websocket-would-block-result who x)]
       [(eof-object? x) x]
       [else
        (let ([state (ensure-success who (ffi-net-websocket-state x))])
          (%make-websocket-connection
           x (websocket-server-host server) (websocket-server-port server) "/"
           (and (websocket-options-tls-context (websocket-server-options server)) #t)
           (websocket-server-protocol server) (websocket-server-options server)
           (vector-ref state 0) (vector-ref state 1)
           (vector-ref state 2) (make-queue) #f))])))

  (define send-result
    (lambda (who x)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) (websocket-would-block-result who x)]
       [else (ensure-success who x)])))

  (define websocket-would-block-result
    (lambda (who answer)
      (let ([detail (vector-ref answer 1)])
        (unless (and (vector? detail)
                     (fx= (vector-length detail) 2)
                     (fixnum? (vector-ref detail 0))
                     (list? (vector-ref detail 1)))
          (raise-net-error who 'websocket
                           "invalid websocket readiness result"
                           answer))
        (make-net-would-block (vector-ref detail 0) (vector-ref detail 1)))))

  (define normalize-binary-payload
    (lambda (who payload)
      (unless (bytevector? payload)
        (errorf who "expected bytevector payload, given ~s" payload))
      payload))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "timeout must be a fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-monotonic-ms
    (lambda ()
      (let ([t (current-time 'time-monotonic)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (+ (current-monotonic-ms) timeout-ms)))

  (define remaining-timeout-ms
    (lambda (deadline-ms)
      (max 0 (- deadline-ms (current-monotonic-ms)))))

  (define websocket-service-deadline
    (lambda (deadline-ms)
      ;; LWS's public adjustment reports forced work, but does not expose all
      ;; future SUL deadlines. Bound timer latency as the HTTP reactor does.
      (min deadline-ms (+ (current-monotonic-ms) 50))))

  (define websocket-service-targets
    (lambda (handle server?)
      (map (lambda (item)
             (let ([mask (vector-ref item 1)])
               (make-poll-target
                (vector-ref item 0)
                (append (if (zero? (fxlogand mask (net-pollin))) '() '(read))
                        (if (zero? (fxlogand mask (net-pollout))) '() '(write))))))
           (vector->list
            (ensure-success 'websocket
              (ffi-net-websocket-poll-targets handle (if server? 1 0)))))))

  (define wait-for-websocket-result
    (case-lambda
      [(who timeout-ms timeout-message thunk)
       (wait-for-websocket-result who timeout-ms timeout-message thunk #f)]
      [(who timeout-ms timeout-message thunk targets)
       (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
         (net-operation-wait
          (make-net-operation
           'websocket
           (lambda ()
             (when (fx<= (remaining-timeout-ms deadline-ms) 0)
               (raise-net-error who 'websocket timeout-message))
             (let ([answer (thunk)])
               (if (net-would-block? answer)
                   (net-operation-pending
                    (if targets (targets)
                        (list (make-poll-target (net-would-block-resource answer)
                                                (net-would-block-events answer))))
                    (websocket-service-deadline deadline-ms))
                   (net-operation-completed answer))))
           void)))]))

  (define make-websocket-connect-operation
    (lambda (who host port path secure? protocol options timeout-ms)
      (let* ([answer (ffi-net-websocket-connect host
                                                port
                                                path
                                                protocol
                                                (options-protocol-offer options)
                                                (if secure? 1 0)
                                                (if (websocket-options-tls-context options)
                                                    (tls-context-native-handle
                                                     (websocket-options-tls-context options))
                                                    0)
                                                websocket-no-timeout)]
             [handle (if (ffi-error? answer)
                         (ensure-success who answer)
                         answer)]
             [deadline-ms (timeout->deadline-ms timeout-ms)])
        (make-net-operation
         'websocket-connect
         (lambda ()
           (when (fx<= (remaining-timeout-ms deadline-ms) 0)
             (raise-net-error who 'websocket "websocket connect timed out"))
           (let ([step (ffi-net-websocket-connect-step handle)])
             (cond
              [(eq? step #t)
               (let* ([state (ensure-success who (ffi-net-websocket-state handle))]
                      [connection
                       (%make-websocket-connection
                        handle host port path secure? protocol options
                        (vector-ref state 0) (vector-ref state 1)
                        (vector-ref state 2) (make-queue) #f)])
                 (set! handle 0)
                 (net-operation-completed connection))]
              [(ffi-would-block? step)
               (let ([would-block (websocket-would-block-result who step)])
                 (net-operation-pending
                  (websocket-service-targets handle #f)
                  (websocket-service-deadline deadline-ms)))]
              [else
               (ensure-success who step)
               (assert-unreachable)])))
         (lambda ()
           (unless (zero? handle)
             (ensure-success who (ffi-net-websocket-close handle))
             (set! handle 0)))
         (lambda ()
           (unless (zero? handle)
             (ensure-success who (ffi-net-websocket-close handle))
             (set! handle 0)))))))

  (define do-send
    (lambda (who conn type payload nonblocking? timeout-ms)
      (ensure-connection-open who conn)
      (cond
       [(eq? type 'text)
        (unless (string? payload)
          (errorf who "expected string payload for websocket text message"))
        (let ([bv (string->utf8 payload)])
          (send-result who
                       (ffi-net-websocket-send (websocket-connection-handle conn)
                                               (websocket-type->int who type)
                                               bv
                                               0
                                               (bytevector-length bv)
                                               (if nonblocking? 1 0)
                                               (if nonblocking? websocket-no-timeout timeout-ms))))]
       [else
        (let ([bv (normalize-binary-payload who payload)])
          (send-result who
                       (ffi-net-websocket-send (websocket-connection-handle conn)
                                               (websocket-type->int who type)
                                               bv
                                               0
                                               (bytevector-length bv)
                                               (if nonblocking? 1 0)
                                               (if nonblocking? websocket-no-timeout timeout-ms))))])))

  #|proc:make-websocket-options
The `make-websocket-options` procedure creates WebSocket transport options.
`tls-context` is a TLS context or `#f`; `subprotocols` is a nonempty list of strings.
`fragment-size` is the positive send chunk size. WebSocket extensions are unsupported.
`ping-interval-ms` is a nonnegative interval or `#f`; `pong-timeout-ms` is nonnegative.
The return value is a new options record.
|#
  (define make-websocket-options
    (case-lambda
      [() default-websocket-options]
      [(tls-context subprotocols fragment-size ping-interval-ms pong-timeout-ms)
       (pcheck ([(lambda (value) (or (not value) (tls-context? value))) tls-context]
                [list? subprotocols] [positive? fragment-size]
                [(lambda (value) (or (not value) (natural? value))) ping-interval-ms]
                [natural? pong-timeout-ms])
         (unless (and (pair? subprotocols) (andmap string? subprotocols)
                      (andmap (lambda (value) (positive? (string-length value))) subprotocols))
           (errorf 'make-websocket-options
                   "subprotocols must be a nonempty list of nonempty strings"))
         (%make-websocket-options tls-context subprotocols fragment-size
                                  ping-interval-ms pong-timeout-ms))]))

  (define options-protocol
    (lambda (options)
      (car (websocket-options-subprotocols options))))

  (define options-protocol-offer
    (lambda (options)
      (let loop ([remaining (websocket-options-subprotocols options)] [result ""])
        (if (null? remaining)
            result
            (loop (cdr remaining)
                  (string-append result
                                 (if (zero? (string-length result)) "" ", ")
                                 (car remaining)))))))

  #|proc:websocket-listen
The `websocket-listen` procedure creates a WebSocket server listener.
|#
  (define-who websocket-listen
    (case-lambda
      [(host port) (websocket-listen host port default-websocket-options)]
      [(host port protocol-or-options)
       (pcheck ([string? host] [fixnum? port])
               (check-port who port)
               (let* ([options
                       (cond
                        [(websocket-options? protocol-or-options) protocol-or-options]
                        [(string? protocol-or-options)
                         (%make-websocket-options #f (list protocol-or-options)
                                                  65536 #f 30000)]
                        [else
                         (errorf who "expected protocol string or WebSocket options")])]
                      [protocol (options-protocol options)])
                 (let ([ans (ffi-net-websocket-listen
                             host port protocol
                             (options-protocol-offer options)
                             (if (websocket-options-tls-context options)
                                 (tls-context-native-handle
                                  (websocket-options-tls-context options))
                                 0))])
                 (if (ffi-error? ans)
                     (ensure-success who ans)
                     (%make-websocket-server ans host port protocol options #f)))))]))

  #|proc:websocket-server-close
The `websocket-server-close` procedure closes a WebSocket server listener.
|#
  (define-who websocket-server-close
    (lambda (server)
      (pcheck ([websocket-server? server])
              (unless (websocket-server-closed? server)
                (ensure-success who
                                (ffi-net-websocket-server-close (websocket-server-handle server)))
                (websocket-server-handle-set! server 0)
                (websocket-server-closed?-set! server #t))
              server)))

  #|proc:websocket-accept
The `websocket-accept` procedure accepts an incoming WebSocket connection.
|#
  (define-who websocket-accept
    (case-lambda
      [(server)
       (websocket-accept server websocket-default-timeout-ms)]
      [(server timeout-ms)
       (pcheck ([websocket-server? server])
               (check-timeout-ms who timeout-ms)
               (ensure-server-open who server)
               (wait-for-websocket-result
                who timeout-ms "websocket accept timed out"
                (lambda () (websocket-accept/nonblocking server))
                (lambda () (websocket-service-targets (websocket-server-handle server) #t))))]))

  #|proc:websocket-accept/nonblocking
The `websocket-accept/nonblocking` procedure attempts one WebSocket accept operation.
The `server` parameter is an open WebSocket server.
The return value is a connection, EOF, or a would-block value naming a service descriptor.
|#
  (define-who websocket-accept/nonblocking
    (lambda (server)
      (pcheck ([websocket-server? server])
              (ensure-server-open who server)
              (accept-result who
                             server
                             (ffi-net-websocket-accept (websocket-server-handle server)
                                                       1
                                                       websocket-no-timeout)))))

  #|proc:websocket-connect
The `websocket-connect` procedure connects to a WebSocket endpoint described by a `ws:` or `wss:`
URI.
|#
  (define-who websocket-connect
    (case-lambda
      [(value)
       (websocket-connect value "cspp-websocket" websocket-default-timeout-ms)]
      [(value protocol-or-timeout)
       (cond
        [(websocket-options? protocol-or-timeout)
         (websocket-connect value protocol-or-timeout websocket-default-timeout-ms)]
        [(string? protocol-or-timeout)
         (websocket-connect value protocol-or-timeout websocket-default-timeout-ms)]
        [(fixnum? protocol-or-timeout)
         (websocket-connect value "cspp-websocket" protocol-or-timeout)]
        [else
         (errorf who "expected websocket protocol string or timeout fixnum, given ~s"
                 protocol-or-timeout)])]
      [(value protocol-or-options timeout-ms)
       (pcheck ([fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (let* ([options
                       (cond
                        [(websocket-options? protocol-or-options) protocol-or-options]
                        [(string? protocol-or-options)
                         (%make-websocket-options #f (list protocol-or-options)
                                                  65536 #f 30000)]
                        [else
                         (errorf who "expected protocol string or WebSocket options")])]
                      [protocol (options-protocol options)]
                      [u (normalize-uri who value)]
                      [host (uri-host u)]
                      [port (uri-default-port u)]
                      [path (uri-path* u)]
                      [secure? (string=? (uri-scheme u) "wss")]
                      [operation
                       (make-websocket-connect-operation
                        who host port path secure? protocol options timeout-ms)])
                 (net-operation-wait operation)))]))

  (define valid-websocket-close-code?
    (lambda (code)
      (or (memv code '(1000 1001 1002 1003 1007 1008 1009 1010 1011 1012 1013 1014))
          (<= 3000 code 4999))))

  #|proc:websocket-close
The `websocket-close` procedure closes WebSocket `conn` with optional `code` and UTF-8 `reason`.
The default close code is 1000. The return value is `conn`.
|#
  (define-who websocket-close
    (case-lambda
      [(conn) (websocket-close conn 1000 "")]
      [(conn code reason)
       (pcheck ([websocket-connection? conn] [integer? code] [string? reason])
         (unless (valid-websocket-close-code? code)
           (errorf who "invalid WebSocket close code ~s" code))
         (let ([reason-bytes (string->utf8 reason)])
           (when (> (bytevector-length reason-bytes) 123)
             (errorf who "WebSocket close reason exceeds 123 UTF-8 bytes"))
           (unless (websocket-connection-closed? conn)
             (ensure-success
              who
              (ffi-net-websocket-close-with-reason
               (websocket-connection-handle conn) code reason-bytes))
             (websocket-connection-close-code-set! conn code)
             (websocket-connection-close-reason-set! conn reason)
             (websocket-connection-handle-set! conn 0)
             (queue-clear! (websocket-connection-deferred conn))
             (websocket-connection-closed?-set! conn #t)))
         conn)]))

  (define refresh-websocket-state!
    (lambda (who conn)
      (unless (websocket-connection-closed? conn)
        (let ([state (ensure-success
                      who
                      (ffi-net-websocket-state (websocket-connection-handle conn)))])
          (websocket-connection-negotiated-subprotocol-set! conn (vector-ref state 0))
          (when (vector-ref state 1)
            (websocket-connection-close-code-set! conn (vector-ref state 1)))
          (when (positive? (string-length (vector-ref state 2)))
            (websocket-connection-close-reason-set! conn (vector-ref state 2)))
          state))))

  #|proc:websocket-negotiated-subprotocol
The `websocket-negotiated-subprotocol` procedure returns the selected protocol for `conn`, or `#f`.
|#
  (define websocket-negotiated-subprotocol
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
        (refresh-websocket-state! 'websocket-negotiated-subprotocol conn)
        (websocket-connection-negotiated-subprotocol conn))))

  #|proc:websocket-close-code
The `websocket-close-code` procedure returns the peer or local close code for `conn`, or `#f`.
|#
  (define websocket-close-code
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
        (refresh-websocket-state! 'websocket-close-code conn)
        (websocket-connection-close-code conn))))

  #|proc:websocket-close-reason
The `websocket-close-reason` procedure returns the copied peer or local close reason for `conn`.
|#
  (define websocket-close-reason
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
        (refresh-websocket-state! 'websocket-close-reason conn)
        (websocket-connection-close-reason conn))))

  #|proc:websocket-send-text
The `websocket-send-text` procedure sends a text message on a WebSocket connection.
|#
  (define-who websocket-send-text
    (case-lambda
      [(conn payload)
       (websocket-send-text conn payload websocket-default-timeout-ms)]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [string? payload])
               (check-timeout-ms who timeout-ms)
               (wait-for-websocket-result
                who timeout-ms "websocket send timed out"
                (lambda ()
                  (do-send who conn 'text payload #t websocket-no-timeout))))]))

  #|proc:websocket-send-binary
The `websocket-send-binary` procedure sends a binary message on a WebSocket connection.
|#
  (define-who websocket-send-binary
    (case-lambda
      [(conn payload)
       (websocket-send-binary conn payload websocket-default-timeout-ms)]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (wait-for-websocket-result
                who timeout-ms "websocket send timed out"
                (lambda ()
                  (do-send who conn 'binary payload #t websocket-no-timeout))))]))

  #|proc:websocket-send-ping
The `websocket-send-ping` procedure sends a ping frame on a WebSocket connection.
|#
  (define-who websocket-send-ping
    (case-lambda
      [(conn) (websocket-send-ping conn (make-bytevector 0 0))]
      [(conn payload)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (websocket-send-ping conn payload websocket-default-timeout-ms))]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (wait-for-websocket-result
                who timeout-ms "websocket send timed out"
                (lambda ()
                  (do-send who conn 'ping payload #t websocket-no-timeout))))]))

  #|proc:websocket-send-pong
The `websocket-send-pong` procedure sends a pong frame on a WebSocket connection.
|#
  (define-who websocket-send-pong
    (case-lambda
      [(conn) (websocket-send-pong conn (make-bytevector 0 0))]
      [(conn payload)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (websocket-send-pong conn payload websocket-default-timeout-ms))]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (wait-for-websocket-result
                who timeout-ms "websocket send timed out"
                (lambda ()
                  (do-send who conn 'pong payload #t websocket-no-timeout))))]))

  #|proc:websocket-cancel-pending-send!
The `websocket-cancel-pending-send!` procedure cancels and discards the currently pending
non-blocking send on a WebSocket connection, if any.
|#
  (define-who websocket-cancel-pending-send!
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
              (ensure-connection-open who conn)
              (ensure-success who
                              (ffi-net-websocket-cancel-send
                               (websocket-connection-handle conn)))
              conn)))

  #|proc:websocket-send/nonblocking
The `websocket-send/nonblocking` procedure attempts one WebSocket frame send.
The `conn` parameter is an open WebSocket connection. The `type` parameter is the frame type.
The `payload` parameter is a string for text or a bytevector for other frame types.
The return value is a byte count or a would-block value naming a service descriptor.
|#
  (define-who websocket-send/nonblocking
    (lambda (conn type payload)
      (pcheck ([websocket-connection? conn])
              (do-send who conn type payload #t websocket-no-timeout))))

  (define fragment-payload
    (lambda (who type payload)
      (case type
        [(text)
         (unless (string? payload)
           (errorf who "text WebSocket fragment payload must be a string"))
         (string->utf8 payload)]
        [(binary)
         (unless (bytevector? payload)
           (errorf who "binary WebSocket fragment payload must be a bytevector"))
         payload]
        [else (errorf who "fragment type must be text or binary")])))

  #|proc:websocket-send-fragment/nonblocking
The `websocket-send-fragment/nonblocking` procedure sends a non-final fragment on `conn`.
`type` is `text` or `binary`; `payload` has the corresponding string or bytevector type.
The return value is a byte count or a would-block value.
|#
  (define websocket-send-fragment/nonblocking
    (lambda (conn type payload)
      (pcheck ([websocket-connection? conn] [symbol? type])
        (ensure-connection-open 'websocket-send-fragment/nonblocking conn)
        (let ([bytes (fragment-payload 'websocket-send-fragment/nonblocking type payload)])
          (send-result
           'websocket-send-fragment/nonblocking
           (ffi-net-websocket-send-fragment
            (websocket-connection-handle conn)
            (websocket-type->int 'websocket-send-fragment/nonblocking type)
            bytes 0 (bytevector-length bytes) 0 1 websocket-no-timeout))))))

  #|proc:websocket-finish-message/nonblocking
The `websocket-finish-message/nonblocking` procedure sends final continuation `payload` on `conn`.
`payload` is a string or bytevector matching the open message. It returns a count or would-block.
|#
  (define websocket-finish-message/nonblocking
    (lambda (conn payload)
      (pcheck ([websocket-connection? conn])
        (ensure-connection-open 'websocket-finish-message/nonblocking conn)
        (let ([bytes (cond
                      [(string? payload) (string->utf8 payload)]
                      [(bytevector? payload) payload]
                      [else
                       (errorf 'websocket-finish-message/nonblocking
                               "fragment payload must be a string or bytevector")])])
          (send-result
           'websocket-finish-message/nonblocking
           (ffi-net-websocket-send-fragment
            (websocket-connection-handle conn) 2 bytes 0 (bytevector-length bytes)
            1 1 websocket-no-timeout))))))

  #|proc:websocket-ping-operation
The `websocket-ping-operation` procedure sends `payload` on `conn` and waits for a pong.
`payload` is a bytevector and optional `timeout-ms` is nonnegative. It returns a net operation.
|#
  (define websocket-ping-operation
    (case-lambda
      [(conn) (websocket-ping-operation conn #vu8() websocket-default-timeout-ms)]
      [(conn payload)
       (websocket-ping-operation conn payload websocket-default-timeout-ms)]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload] [fixnum? timeout-ms])
         (check-timeout-ms 'websocket-ping-operation timeout-ms)
         (ensure-connection-open 'websocket-ping-operation conn)
         (let* ([initial-state
                (ensure-success
                  'websocket-ping-operation
                  (ffi-net-websocket-state (websocket-connection-handle conn)))]
                [initial-pong-count (vector-ref initial-state 3)]
                [deadline-ms (timeout->deadline-ms timeout-ms)]
                [sent? #f])
           (make-net-operation
            'websocket-ping
            (lambda ()
              (let advance-receive ([message-budget 16])
                (when (fx<= (remaining-timeout-ms deadline-ms) 0)
                  (websocket-close conn 1001 "pong timeout")
                  (raise-net-error 'websocket-ping-operation 'websocket
                                   "websocket pong timed out" conn))
                (if (not sent?)
                    (let ([answer (do-send 'websocket-ping-operation conn 'ping payload
                                           #t websocket-no-timeout)])
                      (if (net-would-block? answer)
                          (net-operation-pending
                           (list (make-poll-target
                                  (net-would-block-resource answer)
                                  (net-would-block-events answer)))
                           (websocket-service-deadline deadline-ms))
                          (begin
                            (set! sent? #t)
                            (advance-receive message-budget))))
                    (let ([state
                           (ensure-success
                            'websocket-ping-operation
                            (ffi-net-websocket-state
                             (websocket-connection-handle conn)))])
                      (if (> (vector-ref state 3) initial-pong-count)
                          (net-operation-completed #t)
                          (let ([answer
                                 (recv-result
                                  'websocket-ping-operation
                                  (ffi-net-websocket-recv
                                   (websocket-connection-handle conn)
                                   1 websocket-no-timeout))])
                            (cond
                             [(net-would-block? answer)
                              (net-operation-pending
                               (list (make-poll-target
                                      (net-would-block-resource answer)
                                      (net-would-block-events answer)))
                               (websocket-service-deadline deadline-ms))]
                             [(websocket-message? answer)
                              (queue-push! (websocket-connection-deferred conn) answer)
                              (if (fx= message-budget 1)
                                  ;; Yield runnable work without changing the absolute timeout.
                                  (net-operation-pending '() (current-monotonic-ms))
                                  (advance-receive (fx1- message-budget)))]
                             [else
                              (net-operation-failed
                               (make-net-error 'websocket-ping-operation 'websocket
                                               "connection closed before pong" conn))])))))))
            void)))]))

  #|proc:websocket-recv
The `websocket-recv` procedure receives the next complete WebSocket message.
|#
  (define-who websocket-recv
    (case-lambda
      [(conn)
       (websocket-recv conn websocket-default-timeout-ms)]
      [(conn timeout-ms)
       (pcheck ([websocket-connection? conn] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-connection-open who conn)
               (wait-for-websocket-result
                who timeout-ms "websocket receive timed out"
                (lambda () (websocket-recv/nonblocking conn))))]))

  #|proc:websocket-recv/nonblocking
The `websocket-recv/nonblocking` procedure attempts to receive one complete WebSocket message.
The `conn` parameter is an open WebSocket connection.
The return value is a message, EOF, or a would-block value naming a service descriptor.
|#
  (define-who websocket-recv/nonblocking
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
              (ensure-connection-open who conn)
              (let ([deferred (websocket-connection-deferred conn)])
                (if (not (queue-empty? deferred))
                    (queue-pop! deferred)
                    (recv-result who
                                 (ffi-net-websocket-recv
                                  (websocket-connection-handle conn)
                                  1 websocket-no-timeout)))))))

  #|proc:websocket-next-message
The `websocket-next-message` procedure is an alias of `websocket-recv`.
|#
  (define-who websocket-next-message
    (case-lambda
      [(conn)
       (websocket-recv conn)]
      [(conn timeout-ms)
       (pcheck ([websocket-connection? conn] [fixnum? timeout-ms])
               (websocket-recv conn timeout-ms))]))

  #|proc:call-with-websocket
The `call-with-websocket` procedure opens a WebSocket connection, passes it to `proc`, and closes
it afterward.
|#
  (define-who call-with-websocket
    (case-lambda
      [(value proc)
       (call-with-websocket value "cspp-websocket" websocket-default-timeout-ms proc)]
      [(value protocol-or-timeout proc)
       (pcheck ([procedure? proc])
               (cond
                [(websocket-options? protocol-or-timeout)
                 (call-with-websocket value protocol-or-timeout
                                      websocket-default-timeout-ms proc)]
                [(string? protocol-or-timeout)
                 (call-with-websocket value
                                      protocol-or-timeout
                                      websocket-default-timeout-ms
                                      proc)]
                [(fixnum? protocol-or-timeout)
                 (call-with-websocket value
                                      "cspp-websocket"
                                      protocol-or-timeout
                                      proc)]
                [else
                 (errorf who "expected websocket protocol string or timeout fixnum, given ~s"
                         protocol-or-timeout)]))]
      [(value protocol-or-options timeout-ms proc)
       (pcheck ([procedure? proc])
               (check-timeout-ms who timeout-ms)
               (unless (or (string? protocol-or-options)
                           (websocket-options? protocol-or-options))
                 (errorf who "expected websocket protocol string or options"))
               (let ([conn (websocket-connect value protocol-or-options timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc conn))
                   (lambda () (websocket-close conn)))))]))
  )
