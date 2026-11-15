(import (cspp)
        (cspp concurrency)
        (cspp net))

(load "net-common.ss")

(define websocket-test-monotonic-ms
  (lambda ()
    (let ([time (current-time 'time-monotonic)])
      (+ (* (time-second time) 1000)
         (quotient (time-nanosecond time) 1000000)))))

(define websocket-net-error-message?
  (lambda (message thunk)
    (guard (c [else
               (and (net-error? c)
                    (equal? (net-error-message c) message))])
      (thunk)
      #f)))

(define websocket-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-stalled-websocket-handshake-server
  (lambda (delay-ms)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (milisleep delay-ms)
                     (close-socket client)
                     (close-socket listener)))))))))

(define start-websocket-send-with-pending
  (lambda (conn payload)
    (define pending-state
      (lambda (answer state)
        (and (net-would-block? answer)
             (fixnum? (net-would-block-resource answer))
             (memq 'write (net-would-block-events answer))
             state)))
    (let ([first (websocket-send/nonblocking conn 'binary payload)])
      (if (net-would-block? first)
          (pending-state first 'pending-first)
          (let ([second (websocket-send/nonblocking conn 'binary payload)])
            (if (net-would-block? second)
                (pending-state second 'sent-once-then-pending)
                (error 'start-websocket-send-with-pending
                       "WebSocket nonblocking send did not become pending on the second send")))))))

(define websocket-read-would-block?
  (lambda (answer)
    (and (net-would-block? answer)
         (fixnum? (net-would-block-resource answer))
         (not (not (memq 'read (net-would-block-events answer)))))))

(define await-websocket-message
  (lambda (conn)
    (let loop ([i 0])
      (let ([ans (websocket-recv/nonblocking conn)])
        (if (net-would-block? ans)
            (begin
              (when (> i 1000)
                (error 'await-websocket-message
                       "WebSocket message did not arrive"))
              (poll
               (list
                (make-poll-target
                 (net-would-block-resource ans)
                 (net-would-block-events ans)))
               10)
              (loop (+ i 1)))
            ans)))))

(mat net-websocket-concurrent-first-use
     (mat-requires (websockets)
       (let* ([count 8]
              [server* (make-vector count #f)]
              [thread*
               (vector-map
                (lambda (index)
                  (fork-thread
                   (lambda ()
                     (let ([server (websocket-listen "127.0.0.1" 0)])
                       (vector-set! server* index server)))))
                '#(0 1 2 3 4 5 6 7))])
         (dynamic-wind
           (lambda ()
             (vector-for-each thread-join thread*))
           (lambda ()
             (andmap websocket-server? (vector->list server*)))
           (lambda ()
             (vector-for-each
              (lambda (server)
                (when (websocket-server? server)
                  (websocket-server-close server)))
              server*))))))

(mat net-websocket-readiness
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)])
         (dynamic-wind
           void
           (lambda ()
             (let ([answer (websocket-accept/nonblocking server)])
               (and (net-would-block? answer)
                    (fixnum? (net-would-block-resource answer))
                    (not (not
                          (memq 'read
                                (net-would-block-events answer)))))))
           (lambda ()
             (websocket-server-close server))))))

(mat net-websocket-connection-readiness
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/readiness" port)]
              [client #f]
              [accepted #f])
         (dynamic-wind
           void
           (lambda ()
             (set! client (websocket-connect uri))
             (set! accepted (websocket-accept server))
             (let ([accept-answer (websocket-accept/nonblocking server)]
                   [recv-answer (websocket-recv/nonblocking accepted)])
               (and (websocket-read-would-block? accept-answer)
                    (websocket-read-would-block? recv-answer)
                    (not (fx= (net-would-block-resource accept-answer)
                              (net-would-block-resource recv-answer))))))
           (lambda ()
             (when accepted (websocket-close accepted))
             (when client (websocket-close client))
             (websocket-server-close server))))))

(mat net-websocket
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri1 (format "ws://127.0.0.1:~a/chat?room=one" port)]
              [uri2 (format "ws://127.0.0.1:~a/call-with" port)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (websocket-server? server)
              (net-would-block? (websocket-accept/nonblocking server))
              (let ([client (websocket-connect uri1)]
                    [accepted #f])
                (dynamic-wind
                  void
                  (lambda ()
                    (set! accepted (or (websocket-accept/nonblocking server)
                                       (websocket-accept server)))
                    (and
                     (websocket-connection? client)
                     (websocket-connection? accepted)
                     (websocket-read-would-block?
                      (websocket-recv/nonblocking client))
                     (websocket-read-would-block?
                      (websocket-recv/nonblocking accepted))
                     (= (websocket-send-text client "hello websocket") 15)
                     (let ([msg (websocket-next-message accepted)])
                       (and (websocket-message? msg)
                            (eq? (websocket-message-type msg) 'text)
                            (equal? (websocket-message-data msg) "hello websocket")))
                     (let ([ans (websocket-send/nonblocking accepted 'binary #vu8(1 2 3 4))])
                       (or (net-would-block? ans) (= ans 4)))
                     (let ([msg (websocket-recv client)])
                       (and (websocket-message? msg)
                            (eq? (websocket-message-type msg) 'binary)
                            (equal? (websocket-message-data msg) #vu8(1 2 3 4))))))
                  (lambda ()
                    (when accepted
                      (websocket-close accepted))
                    (websocket-close client))))
              (call-with-websocket
               uri2
               (lambda (client)
                 (let ([accepted #f])
                   (dynamic-wind
                     void
                     (lambda ()
                       (set! accepted (websocket-accept server))
                       (and
                        (websocket-connection? client)
                        (websocket-connection? accepted)
                        (let ([ans (websocket-send/nonblocking client 'text "via-call")])
                          (or (net-would-block? ans) (= ans 8)))
                        (let ([msg (websocket-recv accepted)])
                          (and (websocket-message? msg)
                               (eq? (websocket-message-type msg) 'text)
                               (equal? (websocket-message-data msg) "via-call")))
                        (= (websocket-send-binary accepted #vu8(9 8 7)) 3)
                        (let ([msg (websocket-recv client)])
                          (and (websocket-message? msg)
                               (eq? (websocket-message-type msg) 'binary)
                               (equal? (websocket-message-data msg) #vu8(9 8 7))))))
                     (lambda ()
                       (when accepted
                         (websocket-close accepted)))))))))
           (lambda ()
             (websocket-server-close server))))))

(mat net-websocket-timeout
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)])
         (dynamic-wind
           void
           (lambda ()
             (websocket-net-error-message?
              "websocket accept timed out"
              (lambda ()
                (websocket-accept server 50))))
           (lambda ()
             (websocket-server-close server)))))
     (mat-requires (websockets)
       (let-values ([(listener port th)
                     (start-stalled-websocket-handshake-server 150)])
         (dynamic-wind
           void
           (lambda ()
             (websocket-net-error-message?
              "websocket connect timed out"
              (lambda ()
                (websocket-connect (format "ws://127.0.0.1:~a/stall" port) 50))))
           (lambda ()
             (thread-join th)
             (guard (c [else #f])
               (close-socket listener))))))
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/idle" port)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (websocket-connect uri)]
                   [accepted #f])
               (dynamic-wind
                 void
                 (lambda ()
                   (set! accepted (websocket-accept server))
                   (and (websocket-net-error-message?
                         "websocket receive timed out"
                         (lambda ()
                           (websocket-recv client 50)))
                        (websocket-net-error-message?
                         "websocket receive timed out"
                         (lambda ()
                           (websocket-next-message accepted 50)))))
                 (lambda ()
                   (when accepted
                     (websocket-close accepted))
                   (websocket-close client)))))
           (lambda ()
             (websocket-server-close server))))))

(mat net-websocket-server-close-live-connection
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/live-after-close" port)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (websocket-connect uri)]
                   [accepted #f])
               (dynamic-wind
                 void
                 (lambda ()
                   (set! accepted (websocket-accept server))
                   (and
                    (eq? (websocket-server-close server) server)
                    (= (websocket-send-text client "after-server-close") 18)
                    (let ([msg (websocket-recv accepted)])
                      (and (websocket-message? msg)
                           (eq? (websocket-message-type msg) 'text)
                           (equal? (websocket-message-data msg) "after-server-close")))
                    (= (websocket-send-text accepted "server-side-still-live") 22)
                    (let ([msg (websocket-recv client)])
                      (and (websocket-message? msg)
                           (eq? (websocket-message-type msg) 'text)
                           (equal? (websocket-message-data msg) "server-side-still-live")))))
                 (lambda ()
                   (when accepted
                     (websocket-close accepted))
                   (websocket-close client)))))
           (lambda ()
             (guard (c [else #f])
               (websocket-server-close server)))))))

(mat net-websocket-timeout-validation
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/validate" port)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (websocket-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (websocket-accept server -1)))
              (websocket-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (websocket-connect uri -1)))
              (websocket-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (call-with-websocket uri -1 websocket-connection?)))))
           (lambda ()
             (websocket-server-close server)))))
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/io" port)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (websocket-connect uri)]
                   [accepted #f])
               (dynamic-wind
                 void
                 (lambda ()
                   (set! accepted (websocket-accept server))
                   (and
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-send-text client "x" -1)))
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-send-binary accepted #vu8(1) -1)))
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-send-ping client #vu8() -1)))
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-send-pong accepted #vu8() -1)))
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-recv client -1)))
                    (websocket-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (websocket-next-message accepted -1)))))
                 (lambda ()
                   (when accepted
                   (websocket-close accepted))
                   (websocket-close client)))))
           (lambda ()
             (websocket-server-close server))))))

(mat net-websocket-port-validation
     (and
      (websocket-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (websocket-listen "127.0.0.1" -1)))
      (websocket-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (websocket-listen "127.0.0.1" 70000)))))

(mat net-websocket-cancel
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [uri (format "ws://127.0.0.1:~a/cancel" port)]
              [payload (make-bytevector (* 1024 1024) 65)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (websocket-connect uri)]
                   [accepted #f])
               (dynamic-wind
                 void
                 (lambda ()
                   (set! accepted (websocket-accept server))
                   (let ([state (start-websocket-send-with-pending client payload)])
                     (and (begin
                          (websocket-cancel-pending-send! client)
                          #t)
                          (= (websocket-send-text client "after-cancel") 12)
                          (let ([first (await-websocket-message accepted)])
                            (if (eq? state 'pending-first)
                                (and (websocket-message? first)
                                     (eq? (websocket-message-type first) 'text)
                                     (equal? (websocket-message-data first) "after-cancel"))
                                (and (websocket-message? first)
                                     (eq? (websocket-message-type first) 'binary)
                                     (= (bytevector-length (websocket-message-data first))
                                        (bytevector-length payload))
                                     (let ([next (await-websocket-message accepted)])
                                       (and (websocket-message? next)
                                            (eq? (websocket-message-type next) 'text)
                                            (equal? (websocket-message-data next) "after-cancel")))))))))
                 (lambda ()
                   (when accepted
                     (websocket-close accepted))
                   (websocket-close client)))))
           (lambda ()
             (websocket-server-close server))))))

(define wait-websocket-write
  (lambda (answer)
    (when (net-would-block? answer)
      (poll (list (make-poll-target (net-would-block-resource answer)
                                    (net-would-block-events answer)))
            100))))

(mat net-websocket-options
     (let ([options (make-websocket-options #f '("cspp.v2" "cspp.v1") 65536 #f 500)])
       (and (websocket-options? options)
            (not (websocket-options-tls-context options))
            (equal? '("cspp.v2" "cspp.v1") (websocket-options-subprotocols options))
            (= 65536 (websocket-options-fragment-size options))
            (not (websocket-options-ping-interval-ms options))
            (= 500 (websocket-options-pong-timeout-ms options))))

     ;; Error case: the removed compression parameter is no longer an accepted argument.
     (guard (condition [else #t])
       (apply make-websocket-options (list #f '("cspp-websocket") #t 65536 #f 500))
       #f))

(mat net-websocket-subprotocols-fragments-and-close
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [options (make-websocket-options #f '("cspp.v2" "cspp.v1")
                                               65536 #f 500)]
              [server (websocket-listen "127.0.0.1" port options)]
              [uri (format "ws://127.0.0.1:~a/features" port)]
              [client #f]
              [accepted #f])
         (dynamic-wind
           void
           (lambda ()
             (set! client (websocket-connect uri options))
             (set! accepted (websocket-accept server))
             (let ([first (websocket-send-fragment/nonblocking client 'text "one-")])
               (wait-websocket-write first)
               (let ([second (websocket-send-fragment/nonblocking client 'text "two-")])
                 (wait-websocket-write second)
                 (let ([third (websocket-finish-message/nonblocking client "three")])
                   (wait-websocket-write third))))
             (let ([message (websocket-recv accepted)])
               (and (websocket-message? message)
                    (eq? (websocket-message-type message) 'text)
                    (equal? (websocket-message-data message) "one-two-three")
                    (equal? (websocket-negotiated-subprotocol client) "cspp.v2")
                    (equal? (websocket-negotiated-subprotocol accepted) "cspp.v2")
                    (= (websocket-send-ping client #vu8(1 2 3) 500) 3)
                    (begin
                      (websocket-close client 1000 "message complete")
                      (= (websocket-close-code client) 1000))
                    (equal? (websocket-close-reason client) "message complete"))))
           (lambda ()
             (when (and accepted (not (eof-object? accepted)))
               (websocket-close accepted))
             (when client (websocket-close client))
             (websocket-server-close server))))))

(mat net-websocket-negotiated-subprotocol
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server-options (make-websocket-options #f '("selected") 65536 #f 500)]
              [client-options (make-websocket-options #f '("local" "selected") 65536 #f 500)]
              [server (websocket-listen "127.0.0.1" port server-options)]
              [client #f]
              [accepted #f])
         (dynamic-wind
           void
           (lambda ()
             (set! client (websocket-connect (format "ws://127.0.0.1:~a/selected" port)
                                             client-options 1000))
             (set! accepted (websocket-accept server 1000))
             (and (equal? "selected" (websocket-negotiated-subprotocol client))
                  (equal? "selected" (websocket-negotiated-subprotocol accepted))))
           (lambda ()
             (when accepted (websocket-close accepted))
             (when client (websocket-close client))
             (websocket-server-close server))))))

(mat net-websocket-unnegotiated-state
     ;; A peer that offers no subprotocol must not inherit the local handler name.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [socket (open-socket 'inet 'stream)]
              [accepted #f])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! socket (make-socket-address 'inet "127.0.0.1" port))
             (socket-send-all socket
               (string->utf8
                "GET / HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"))
             (set! accepted (websocket-accept server 1000))
             (not (websocket-negotiated-subprotocol accepted)))
           (lambda ()
             (when accepted (websocket-close accepted))
             (close-socket socket)
             (websocket-server-close server))))))

(mat net-websocket-accept-after-partial-handshake
     ;; A stalled handshake must not hide readiness on the listener for a later client.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [partial (open-socket 'inet 'stream)]
              [ready (open-socket 'inet 'stream)]
              [accepted #f]
              [worker #f]
              [failure #f])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! partial (make-socket-address 'inet "127.0.0.1" port))
             (socket-send-all partial (string->utf8 "GET / HTTP/1.1\r\n"))
             (websocket-accept/nonblocking server)
             (websocket-accept/nonblocking server)
             (set! worker
               (fork-thread
                (lambda ()
                  (guard (condition [else (set! failure condition)])
                    (milisleep 50)
                    (socket-connect! ready (make-socket-address 'inet "127.0.0.1" port))
                    (socket-send-all ready
                      (string->utf8
                       "GET / HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Protocol: cspp-websocket\r\n\r\n"))))))
             (set! accepted (websocket-accept server 1000))
             (thread-join worker)
             (and (not failure) (websocket-connection? accepted)))
           (lambda ()
             (when worker (thread-join worker))
             (when accepted (websocket-close accepted))
             (close-socket ready)
             (close-socket partial)
             (websocket-server-close server))))))

(mat net-websocket-idle-handshake-timer
     ;; Error case: an incomplete Upgrade must expire even without another socket event.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [socket (open-socket 'inet 'stream)]
              [deadline (+ (websocket-test-monotonic-ms) 15000)])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! socket (make-socket-address 'inet "127.0.0.1" port))
             (socket-send-all socket
               (string->utf8 "GET / HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\n"))
             (let loop ()
               (websocket-accept/nonblocking server)
               (cond
                [(eof-object? (socket-recv/nonblocking socket 1024)) #t]
                [(>= (websocket-test-monotonic-ms) deadline) #f]
                [else (milisleep 10) (loop)])))
           (lambda ()
             (close-socket socket)
             (websocket-server-close server))))))

(mat net-websocket-pong-timeout
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [client #f]
              [accepted #f])
         (dynamic-wind
           void
           (lambda ()
             (set! client
                   (websocket-connect (format "ws://127.0.0.1:~a/timeout" port)))
             (set! accepted (websocket-accept server))
             (and
              (websocket-net-error-message?
               "websocket pong timed out"
               (lambda ()
                 (net-operation-wait
                  (websocket-ping-operation client #vu8(9) 50))))
              (= (websocket-close-code client) 1001)
              (equal? (websocket-close-reason client) "pong timeout")))
           (lambda ()
             (when accepted (websocket-close accepted))
             (when client (websocket-close client))
             (websocket-server-close server))))))

(mat net-websocket-ping-operation-waits-for-pong
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (websocket-listen "127.0.0.1" port)]
              [client #f]
              [accepted #f]
              [stop? (abox #f)]
              [text-sent? (abox #f)]
              [resume-peer? (abox #f)]
              [worker-failure (abox #f)]
              [worker #f]
              [text* (map (lambda (index) (format "unrelated-~a" index)) (iota 32))])
         (dynamic-wind
           void
           (lambda ()
             (set! client
                   (websocket-connect (format "ws://127.0.0.1:~a/ping-operation" port)))
             (set! accepted (websocket-accept server))
             (set! worker
               (fork-thread
                (lambda ()
                  (guard (failure [else (abox-set! worker-failure failure)])
                    (for-each (lambda (text) (websocket-send-text accepted text 1000)) text*)
                    (abox-set! text-sent? #t)
                    (let ([deadline (+ (websocket-test-monotonic-ms) 2000)])
                      (let wait-for-release ()
                        (unless (or (unabox resume-peer?) (unabox stop?))
                          (when (>= (websocket-test-monotonic-ms) deadline)
                            (errorf 'ping-peer "release gate timed out"))
                          (milisleep 1)
                          (wait-for-release))))
                    (let loop ()
                      (unless (unabox stop?)
                        (let ([answer (websocket-recv/nonblocking accepted)])
                          (when (net-would-block? answer)
                            (poll (list (make-poll-target
                                         (net-would-block-resource answer)
                                         (net-would-block-events answer))) 10)))
                        (loop)))))))
             (let ([deadline (+ (websocket-test-monotonic-ms) 2000)])
               (let wait-for-text ()
                 (cond
                  [(unabox worker-failure) (raise (unabox worker-failure))]
                  [(unabox text-sent?) (void)]
                  [(>= (websocket-test-monotonic-ms) deadline)
                   (errorf 'ping-peer "text send timed out")]
                  [else (milisleep 1) (wait-for-text)])))
             (let ([operation (websocket-ping-operation client #vu8(9) 1000)])
               (net-operation-step! operation)
               ;; The peer cannot pong before release. A pending step must expose readiness
               ;; or an immediate continuation, so queued text cannot hide the pong until expiry.
               (let ([pending? (eq? 'pending (net-operation-state operation))]
                     [runnable? (or (pair? (net-operation-poll-targets operation))
                                    (zero? (net-operation-remaining-timeout-ms operation)))])
                 (abox-set! resume-peer? #t)
                 (let* ([result (net-operation-wait operation)]
                        [received
                         (let read-text ([remaining text*] [messages '()])
                           (if (null? remaining) (reverse messages)
                               (let ([message (websocket-recv client 1000)])
                                 (read-text (cdr remaining)
                                            (cons (websocket-message-data message) messages)))))])
                   (abox-set! stop? #t)
                   (thread-join worker)
                   (and pending? runnable? (eq? result #t)
                        (equal? text* received)
                        (not (unabox worker-failure)))))))
           (lambda ()
             (abox-set! stop? #t)
             (abox-set! resume-peer? #t)
             (when worker (thread-join worker))
             (when accepted (websocket-close accepted))
             (when client (websocket-close client))
             (websocket-server-close server))))))

(mat net-websocket-wss
     (mat-requires (openssl websockets)
       (begin
         (write-test-san-cert-files)
         (let* ([server-context (make-tls-context 'server)]
                [client-context (make-tls-context 'client)]
                [port (reserve-loopback-port)]
                [server-options #f]
                [client-options #f]
                [server #f]
                [client #f]
                [accepted #f])
           (dynamic-wind
             (lambda ()
               (tls-context-load-cert! server-context
                                       "/tmp/cspp-net-test-san-cert.pem")
               (tls-context-load-private-key! server-context
                                              "/tmp/cspp-net-test-san-key.pem")
               (tls-context-load-ca-file! client-context
                                          "/tmp/cspp-net-test-san-cert.pem")
               (set! server-options
                     (make-websocket-options server-context '("cspp-wss")
                                             65536 #f 500))
               (set! client-options
                     (make-websocket-options client-context '("cspp-wss")
                                             65536 #f 500))
               (set! server (websocket-listen "127.0.0.1" port server-options)))
             (lambda ()
               (set! client
                     (websocket-connect (format "wss://127.0.0.1:~a/secure" port)
                                        client-options 2000))
               (set! accepted (websocket-accept server 2000))
               (and (= (websocket-send-text client "secure") 6)
                    (let ([message (websocket-recv accepted 2000)])
                      (and (websocket-message? message)
                           (equal? (websocket-message-data message) "secure")))))
             (lambda ()
               (when accepted (websocket-close accepted))
               (when client (websocket-close client))
               (when server (websocket-server-close server))
               (close-tls-context client-context)
               (close-tls-context server-context)))))))
