(import (cspp)
        (cspp path)
        (cspp net))

(load "net-common.ss")

(mat net-lws-server-source-primary-failure
     ;; Error case: a length failure must survive a second exception from source cleanup.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)] [server (http-listen "127.0.0.1" port)]
              [client (http-open)] [closed 0] [connection #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 1000)
             (let ([operation (http-send/nonblocking client
                                (make-http-request 'get (format "http://127.0.0.1:~a/" port)))])
               (set! connection (http-accept server))
               (and (guard (condition
                            [else (and (message-condition? condition)
                                       (string-contains? (condition-message condition)
                                                         "requires a known length"))])
                      (http-write-response connection
                       (make-http-response 200 "OK" '()
                         (make-http-body-source (lambda (maximum) (eof-object)) #f
                           (lambda () (set! closed (fx1+ closed))
                             (errorf 'closer "secondary failure")))))
                      #f)
                    (= closed 1))))
           (lambda () (when connection (http-connection-close connection))
             (http-close client) (http-server-close server))))))

(mat net-lws-server-write-primary-over-closer
     ;; Error case: a response write failure must survive a second exception from source cleanup.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)] [server (http-listen "127.0.0.1" port)]
              [client (http-open)] [closed 0] [connection #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 1000)
             (let ([operation (http-send/nonblocking client
                                (make-http-request 'get (format "http://127.0.0.1:~a/" port)))])
               (set! connection (http-accept server))
               (http-connection-close connection)
               (and (guard (condition
                            [else (and (message-condition? condition)
                                       (string-contains? (condition-message condition)
                                                         "closed")
                                       (not (string-contains? (condition-message condition)
                                                              "closer")))])
                      (http-write-response connection
                       (make-http-response 200 "OK" '()
                         (make-http-body-source
                          (lambda (maximum) #vu8(1)) 1
                          (lambda () (set! closed (fx1+ closed))
                            (errorf 'closer "secondary failure")))))
                      #f)
                    (= closed 1))))
           (lambda () (when connection (http-connection-close connection))
             (http-close client) (http-server-close server))))))

(define server-test-connect
  (lambda (port)
    (let ([socket (open-socket 'inet 'stream)])
      (guard (condition [else (close-socket socket) (raise condition)])
        (net-operation-wait
         (socket-connect/nonblocking socket (make-socket-address 'inet "127.0.0.1" port) 1000))
        socket))))

(define server-test-receive-until
  (lambda (socket marker)
    (let loop ([text ""] [remaining 3000])
      (cond
       [(string-contains? text marker) text]
       [(fxzero? remaining) (error 'server-test-receive-until "response timed out" text)]
       [else
        (let ([bytes (socket-recv/nonblocking socket 65536)])
          (cond
           [(and (bytevector? bytes) (fxpositive? (bytevector-length bytes)))
            (loop (string-append text (utf8->string bytes)) remaining)]
           [(eof-object? bytes) (error 'server-test-receive-until "unexpected EOF" text)]
           [else
            (milisleep 1)
            (loop text (fx1- remaining))]))]))))

(define server-test-send
  (lambda (socket text) (socket-send-all socket (string->utf8 text))))

(mat net-lws-server-refuses-h2-on-http1-listener
     ;; Error case: explicit H2 must fail against an HTTP/1 listener without hanging or downgrading.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-client-version-set! client 'h2)
             (http-set-timeout! client 200)
             (guard (condition [(condition? condition) #t] [else #f])
               (http-send client
                          (make-http-request 'get
                                             (format "http://127.0.0.1:~a/h2" port)))
               #f))
           (lambda () (http-close client) (http-server-close server))))))

(mat net-lws-server-live-keepalive-and-pipeline
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [socket (server-test-connect port)]
              [worker #f]
              [count 0])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler!
              server 'get "/persistent"
              (lambda (request)
                (set! count (fx1+ count))
                (make-http-response 200 "OK" '() (format "response-~a!" count) '() 'h1)))
             (set! worker (fork-thread (lambda () (do ([i 0 (fx1+ i)]) ((fx= i 3))
                                                  (http-serve server)))))
             (server-test-send socket "GET /persistent HTTP/1.1\r\nHost: localhost\r\n\r\n")
             (let ([first (server-test-receive-until socket "response-1!")])
               (server-test-send socket
                 "GET /persistent HTTP/1.1\r\nHost: localhost\r\n\r\nGET /persistent HTTP/1.1\r\nHost: localhost\r\n\r\n")
               (let ([pipeline (server-test-receive-until socket "response-3!")])
                 (thread-join worker)
                 (and (string-contains? first "200")
                      (string-contains? pipeline "response-2!")
                      (fx= count 3)))))
           (lambda () (close-socket socket) (http-server-close server))))))

(mat net-lws-server-handler-failure-and-recovery
     ;; Error cases: raised conditions and invalid handler returns produce 500 responses.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [client (http-open)]
              [worker #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 1500)
             (http-register-handler! server "/raised" (lambda (request) (error 'handler "failed")))
             (http-register-handler! server "/invalid" (lambda (request) #f))
             (http-register-handler! server "/ok"
               (lambda (request) (make-http-response 200 "OK" '() "recovered" '() 'h1)))
             (set! worker (fork-thread (lambda () (do ([i 0 (fx1+ i)]) ((fx= i 3))
                                                  (http-serve server)))))
             (let ([statuses
                    (map (lambda (path)
                           (http-response-status
                            (http-send client (make-http-request 'get
                              (format "http://127.0.0.1:~a/~a" port path)))))
                         '("raised" "invalid" "ok"))])
               (thread-join worker)
               (equal? statuses '(500 500 200))))
           (lambda () (http-close client) (http-server-close server))))))

(mat net-lws-server-body-pipeline-overrun-is-rejected
     ;; Error case: LWS can include a following pipelined request in its HTTP_BODY callback.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [socket (server-test-connect port)])
         (dynamic-wind
           void
           (lambda ()
             (server-test-send socket
               "POST /body HTTP/1.1\r\nHost: localhost\r\nContent-Length: 3\r\n\r\nabcGET /next HTTP/1.1\r\nHost: localhost\r\n\r\n")
             (let ([connection (http-accept server)])
               (guard (condition [(condition? condition) #t])
                 (http-read-request connection)
                 #f)))
           (lambda () (close-socket socket) (http-server-close server))))))

(mat net-lws-server-partial-headers-do-not-block-accept
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [partial (server-test-connect port)]
              [client (http-open)]
              [worker #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 1500)
             (server-test-send partial "GET /incomplete HTTP/1.1\r\nHost:")
             (http-register-handler! server "/ready"
               (lambda (request) (make-http-response 200 "OK" '() "ready" '() 'h1)))
             (set! worker (fork-thread (lambda () (http-serve server))))
             (let ([response (http-send client (make-http-request 'get
                               (format "http://127.0.0.1:~a/ready" port)))])
               (thread-join worker)
               (equal? (http-response-body response) (string->utf8 "ready"))))
           (lambda () (close-socket partial) (http-close client) (http-server-close server))))))

(mat net-lws-server-close-wakes-accept
     ;; Error case: closing a server interrupts a pending blocking accept.
     (mat-requires (websockets)
       (let* ([server (http-listen "127.0.0.1" (reserve-loopback-port))]
              [started (make-condition)]
              [mutex (make-mutex)]
              [waiting? #f]
              [result #f]
              [worker
               (fork-thread
                (lambda ()
                  (with-mutex mutex (set! waiting? #t) (condition-signal started))
                  (set! result (guard (condition [(condition? condition) #t])
                                 (http-accept server) #f))))])
         (with-mutex mutex (let loop () (unless waiting? (condition-wait started mutex) (loop))))
         (http-server-close server)
         (thread-join worker)
         result)))

(mat net-lws-server-chunked-request-is-rejected
     ;; Error case: LWS does not decode server transfer coding; rejection must not hang.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [socket (server-test-connect port)])
         (dynamic-wind
           void
           (lambda ()
             (server-test-send socket
               "POST /chunks HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n2\r\nde\r\n0\r\n\r\n")
             (let loop ([remaining 1500])
               (let ([answer (socket-recv/nonblocking socket 1024)])
                 (cond
                  [(eof-object? answer) (not (http-accept/nonblocking server))]
                  [(fxzero? remaining) #f]
                  [else (milisleep 1)
                        (loop (fx1- remaining))]))))
           (lambda () (close-socket socket) (http-server-close server))))))

(mat net-lws-server-live-streamed-response
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [client (http-open)]
              [produced 0]
              [closed 0]
              [worker-failure #f]
              [worker #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 3000)
             (http-register-handler! server "/stream"
               (lambda (request)
                 (make-http-response 200 "OK" '()
                   (make-http-body-source
                    (lambda (maximum-bytes)
                      (if (fx= produced 8) (eof-object)
                          (let ([chunk (make-bytevector (min maximum-bytes 32768) 97)])
                            (set! produced (fx1+ produced))
                            chunk)))
                    262144
                    (lambda () (set! closed (fx1+ closed))))
                   '() 'h1)))
             (set! worker
               (fork-thread (lambda ()
                              (guard (condition [else (set! worker-failure condition)])
                                (http-serve server)))))
             (let ([response
                    (guard (condition
                            [else (error 'server-stream-response "response failed"
                                         produced closed condition)])
                      (http-send client (make-http-request 'get
                        (format "http://127.0.0.1:~a/stream" port))))])
               (thread-join worker)
               (when worker-failure (raise worker-failure))
               (and (equal? (http-response-body response) (make-bytevector 262144 97))
                    (fx= produced 8)
                    (fx= closed 1))))
           (lambda ()
             (http-close client)
             (http-server-close server)
             (when worker (thread-join worker)))))))

(mat net-lws-server-live-http2-concurrent-streams
     (mat-requires (openssl websockets)
       (let* ([port (reserve-loopback-port)]
              [server-tls (make-test-http-verified-server-context)]
              [client-tls (make-test-http-verified-client-context)]
              [server (http-listen "127.0.0.1" port server-tls)]
              [client (http-open client-tls)])
         (dynamic-wind
           void
           (lambda ()
             (http-client-version-set! client 'h2)
             (http-set-timeout! client 3000)
             (let ([operations
                    (map (lambda (path)
                           (http-send/nonblocking client
                             (make-http-request 'get (format "https://127.0.0.1:~a/~a" port path)) #f))
                         '("first" "second"))])
               (let loop ([connections '()] [remaining 2500])
                 (for-each (lambda (operation)
                             (if (eq? (net-operation-state operation) 'pending)
                                 (net-operation-step! operation)
                                 (net-operation-wait operation)))
                           operations)
                 (let* ([next (http-accept/nonblocking server)]
                        [connections (if next (cons next connections) connections)])
                   (cond
                    [(fx= (length connections) 2)
                     (for-each
                      (lambda (connection)
                        (http-read-request connection)
                        (http-write-response connection
                          (make-http-response 200 "OK" '() "concurrent" '() 'h2)))
                      connections)
                     (for-all (lambda (response)
                                (and (eq? (http-response-version response) 'h2)
                                     (equal? (http-response-body response)
                                             (string->utf8 "concurrent"))))
                              (map net-operation-wait operations))]
                    [(fxzero? remaining) #f]
                    [else (milisleep 1)
                          (loop connections (fx1- remaining))])))))
           (lambda ()
             (http-close client)
             (http-server-close server)
             (close-tls-context client-tls)
             (close-tls-context server-tls))))))

(mat net-lws-server-live-request-body-backpressure
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [client (http-open)]
              [payload (make-bytevector 262144 113)]
              [received #f]
              [worker-failure #f]
              [worker #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 3000)
             (set! worker
               (fork-thread
                (lambda ()
                 (guard (condition [else (set! worker-failure condition)])
                  (let ([connection (http-accept server)])
                    (milisleep 30)
                    (set! received (http-request-body (http-read-request connection)))
                    (http-write-response connection
                      (make-http-response 200 "OK" '() "received" '() 'h1)))))))
             (let ([response (http-send client (make-http-request 'post
                               (format "http://127.0.0.1:~a/large" port) '() payload))])
               (thread-join worker)
               (when worker-failure (raise worker-failure))
               (and (= (http-response-status response) 200) (equal? payload received))))
           (lambda ()
             (http-close client)
             (http-server-close server)
             (when worker (thread-join worker)))))))

(mat net-lws-server-repeated-body-accounting
     ;; Sixty-five 256 KiB bodies exceed the server's 16 MiB queue limit; consumption must free it.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)] [server (http-listen "127.0.0.1" port)]
              [payload (make-bytevector 262144 114)] [worker #f]
              [server-failure #f])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler! server "/repeat"
               (lambda (request)
                 (make-http-response (if (equal? payload (http-request-body request)) 200 400)
                                     "OK" '() "body-accounted!")))
             (set! worker (fork-thread (lambda ()
                                        (guard (condition [else (set! server-failure condition)])
                                          (http-serve-loop server)))))
             (for-all
              (lambda (index)
                (guard (condition
                        [else (error 'repeated-body "upload failed" index server-failure condition)])
                  (let ([socket (server-test-connect port)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (server-test-send socket
                          "POST /repeat HTTP/1.1\r\nHost: localhost\r\nContent-Length: 262144\r\nConnection: close\r\n\r\n")
                        (socket-send-all socket payload)
                        (string-contains?
                         (server-test-receive-until socket "body-accounted!") "200"))
                      (lambda () (close-socket socket))))))
              (iota 65)))
           (lambda () (http-server-close server)
             (when worker (thread-join worker)))))))

(mat net-lws-server-disconnect-wakes-partial-body
     ;; Error case: a peer disconnects before sending its declared request body.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [socket (server-test-connect port)])
         (dynamic-wind
           void
           (lambda ()
             (server-test-send socket
               "POST /partial HTTP/1.1\r\nHost: localhost\r\nContent-Length: 100\r\n\r\na")
             (let ([connection (http-accept server)])
               (close-socket socket)
               (guard (condition [(condition? condition) #t])
                 (http-read-request connection)
                 #f)))
           (lambda () (close-socket socket) (http-server-close server))))))

(mat net-lws-server-close-wakes-partial-body
     ;; Error case: shutdown interrupts a request whose declared body has not arrived.
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [socket (server-test-connect port)]
              [result #f])
         (dynamic-wind
           void
           (lambda ()
             (server-test-send socket
               "POST /partial HTTP/1.1\r\nHost: localhost\r\nContent-Length: 100\r\n\r\na")
             (let* ([connection (http-accept server)]
                    [worker (fork-thread
                             (lambda ()
                               (set! result (guard (condition [(condition? condition) #t])
                                              (http-read-request connection) #f))))])
               (milisleep 10)
               (http-server-close server)
               (thread-join worker)
               result))
           (lambda () (close-socket socket) (http-server-close server))))))

(mat net-lws-server-handler-registry
     (mat-requires (websockets)
       (let ([server (http-listen "127.0.0.1" (reserve-loopback-port))])
         (dynamic-wind
           void
           (lambda ()
             (let ([handler (lambda (request)
                              (make-http-response 200 "OK" '() "ok" '() 'h1))])
               (and (not (http-register-handler! server 'get "/hello" handler))
                    (eq? handler (http-handler-ref server 'get "/hello" #f))
                    (eq? handler (http-unregister-handler! server 'get "/hello"))
                    (not (http-handler-ref server 'get "/hello" #f)))))
           (lambda () (http-server-close server))))))

(mat net-lws-server-close-is-idempotent
     (mat-requires (websockets)
       (let ([server (http-listen "127.0.0.1" (reserve-loopback-port))])
         (and (eq? server (http-server-close server))
              (eq? server (http-server-close server))))))

(mat net-lws-server-live-http1
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [server-thread #f])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler!
              server 'get "/live"
              (lambda (request)
                (make-http-response 200 "OK" '() "live-ok" '() 'h1)))
             (set! server-thread (fork-thread (lambda () (http-serve server))))
             (let ([response (http-get (format "http://127.0.0.1:~a/live" port))])
               (thread-join server-thread)
               (and (= (http-response-status response) 200)
                    (equal? (utf8->string (http-response-body response)) "live-ok"))))
           (lambda () (http-server-close server))))))

(mat net-lws-server-live-request-body
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [server-thread #f])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler!
              server 'post "/body"
              (lambda (request)
                (make-http-response 200 "result" '()
                                    (format "~s" (http-request-body request)) '() 'h1)))
             (set! server-thread (fork-thread (lambda () (http-serve server))))
             (let ([response
                    (http-request 'post (format "http://127.0.0.1:~a/body" port)
                                  '() #vu8(97 98 99))])
               (thread-join server-thread)
               (and (= (http-response-status response) 200)
                    (equal? (utf8->string (http-response-body response))
                            "#vu8(97 98 99)"))))
           (lambda () (http-server-close server))))))

(mat net-lws-server-live-header-roundtrip
     (mat-requires (websockets)
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port)]
              [client (http-open)]
              [received #f]
              [server-thread #f])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 1000)
             (http-register-handler!
              server 'get "/headers"
              (lambda (request)
                (set! received (http-header-ref (http-request-headers request) "x-input" #f))
                (make-http-response 200 "OK" '(("X-Result" . "kept")
                                               ("Cache-Control" . "no-store"))
                                    "headers-ok" '() 'h1)))
             (set! server-thread (fork-thread (lambda () (http-serve server))))
             (let ([response (http-send client
                              (make-http-request 'get
                                (format "http://127.0.0.1:~a/headers" port)
                                '(("X-Input" . "received")) #f))])
               (thread-join server-thread)
               (and (equal? "received" received)
                    (equal? "kept" (http-header-ref (http-response-headers response)
                                                  "x-result" #f))
                    (equal? "no-store" (http-header-ref (http-response-headers response)
                                                      "cache-control" #f)))))
           (lambda () (http-close client) (http-server-close server))))))
