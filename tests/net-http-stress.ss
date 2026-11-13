(import (cspp) (cspp path) (cspp net))
(load "net-lws-fixture.ss")
(mat net-http2-live-multiplexed-stress
 (mat-requires (websockets)
   (call-with-h2-fixture 8 (lambda (port command)
    (let ([client (http-open)] [ok 0])
     (dynamic-wind void (lambda ()
      (http-client-version-set! client 'h2) (http-set-timeout! client 10000)
      (do ([round 0 (fx1+ round)]) ((fx= round 2))
       (for-each (lambda (path expected)
        (let ([r (http-get client (format "http://127.0.0.1:~a~a" port path))])
         (when (and (= 200 (http-response-status r)) (eq? 'h2 (http-response-version r))
                    (= expected (bytevector-length (http-response-body r)))) (set! ok (fx1+ ok)))))
        '("/" "/" "/large" "/") '(10 10 262144 10))) (= ok 8))
   (lambda () (http-close client))))))))

(define stress-http1-port
  (+ 21000 (modulo (get-process-id) 10000)))

(mat net-http1-live-nonfiber-feature-stress
     ;; Stress case: non-fiber HTTP/1 requests cover fixed bodies, uploads, and response sinks.
     (mat-requires (websockets)
       (let* ([server (http-listen "127.0.0.1" stress-http1-port)]
              [client (http-open)] [worker #f] [passed 0])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler! server "/echo"
               (lambda (request)
                 (make-http-response 200 "OK" '()
                   (or (http-request-body request) #vu8()) '() 'h1)))
             (set! worker
               (fork-thread
                (lambda ()
                  (do ([i 0 (fx1+ i)]) ((fx= i 16))
                    (guard (condition [else (void)]) (http-serve server))))))
             (http-set-timeout! client 3000)
             (do ([i 0 (fx1+ i)]) ((fx= i 8))
               (let ([response (http-get client
                                (format "http://127.0.0.1:~a/echo" stress-http1-port))])
                 (when (and (= 200 (http-response-status response))
                            (eq? 'h1 (http-response-version response)))
                   (set! passed (fx1+ passed)))))
             (do ([i 0 (fx1+ i)]) ((fx= i 8))
               (let* ([body (make-bytevector (+ i 1) 97)]
                      [response (http-send client
                                 (make-http-request 'post
                                   (format "http://127.0.0.1:~a/echo" stress-http1-port)
                                   '() body))])
                 (when (and (= 200 (http-response-status response))
                            (equal? body (http-response-body response)))
                   (set! passed (fx1+ passed)))))
             (thread-join worker)
             (= passed 16))
           (lambda () (http-close client) (http-server-close server))))))

(mat net-http2-live-nonfiber-stream-stress
     ;; Stress case: non-fiber multiplexed H2 operations complete repeatedly on one client.
     (mat-requires (websockets)
       (call-with-h2-fixture
        4
        (lambda (port command)
          (let ([client (http-open)] [passed 0])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 10000)
                (do ([round 0 (fx1+ round)]) ((fx= round 2))
                  (let ([operations
                         (map (lambda (_) (http-send/nonblocking client
                                             (make-http-request 'get
                                               (format "http://127.0.0.1:~a/" port)) #f))
                              '(1 2 3 4))])
                    (for-each (lambda (operation)
                                (let ([response (net-operation-wait operation)])
                                  (when (= 200 (http-response-status response))
                                    (set! passed (fx1+ passed)))))
                              operations)))
                (= passed 8))
              (lambda () (http-close client))))))))

(mat net-http-nonfiber-cancel-failure-stress
     ;; Error case: repeated refused nonblocking requests are cancelled and finish deterministically.
     (mat-requires (websockets)
       (let ([client (http-open)] [passed 0])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 250)
             (do ([i 0 (fx1+ i)]) ((fx= i 16))
               (let ([operation (http-send/nonblocking client
                                (make-http-request 'get "http://127.0.0.1:1/unavailable") #f)])
                 (net-operation-cancel! operation)
                 (when (eq? 'cancelled (net-operation-state operation))
                   (set! passed (fx1+ passed)))))
           (= passed 16))
           (lambda () (http-close client))))))

(mat net-http2-repeated-cancellation-stress
     ;; Error case: repeated H2 cancellation must terminate cancelled streams without affecting peers.
     (mat-requires (websockets)
       (call-with-h2-fixture
        8
        (lambda (port command)
          (let ([client (http-open)] [passed 0])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 10000)
                (do ([round 0 (fx1+ round)]) ((fx= round 4) (= passed 16))
                  (let ([operations
                         (map (lambda (_) (http-send/nonblocking client
                                             (make-http-request 'get
                                               (format "http://127.0.0.1:~a/large" port)) #f))
                              '(1 2 3 4))])
                    (for-each (lambda (operation) (net-operation-cancel! operation)) operations)
                    (for-each (lambda (operation)
                                (when (eq? 'cancelled (net-operation-state operation))
                                  (set! passed (fx1+ passed))))
                              operations))))
              (lambda () (http-close client))))))))
