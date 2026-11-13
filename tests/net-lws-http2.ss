(import (cspp) (cspp path) (cspp net)
        (cspp net private lws-http2)
        (cspp net private lws-ffi)
        (cspp net private http))

(load "net-common.ss")

;; TLS fixture coverage is opt-in because LWS builds differ in client trust/SNI behavior.
(define run-live-h2-tls-tests?
  (guard (condition [else #f])
    (let ([status (lws-status)])
      (and (vector-ref status 0)
           (lws-capability? (vector-ref status 1) lws-cap-http2)
           (lws-capability? (vector-ref status 1) lws-cap-tls)))))

(load "net-lws-fixture.ss")

(define make-h2-tls-test-request
  (lambda (path port)
    (make-normalized-http-request
     "GET" #f 'https "127.0.0.1" port #t path '() #f #f
     (make-http-request-policy '() #f #f #f 0 'h2 #f #f 0
       (let ([now (current-time 'time-monotonic)])
         (+ 3000 (* 1000 (time-second now))
            (quotient (time-nanosecond now) 1000000)))))))

(mat net-lws-http2-live-prior-knowledge
     (mat-requires (websockets)
       (call-with-h2-fixture
        10
        (lambda (port command)
          (let ([client (http-open)])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 2000)
                (let ([response (http-get client (format "http://127.0.0.1:~a/" port))])
                  (and (eq? 'h2 (http-response-version response))
                       (= 200 (http-response-status response))
                       (equal? "xxxxxxxxxx" (utf8->string (http-response-body response)))
                       (= 1 (cadr (command 'stats))))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-live-completion-survives-close
     (mat-requires (websockets)
       (call-with-h2-fixture
        10
        (lambda (port command)
          (let ([client (http-open)] [received 0])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 2000)
                (let ([response
                       (net-operation-wait
                        (http-send/nonblocking
                         client (make-http-request 'get (format "http://127.0.0.1:~a/" port))
                         (make-http-body-sink
                          (lambda (bytes start count)
                            (milisleep 50)
                            (set! received (+ received count))))))])
                  (and (= received 10) (= 200 (http-response-status response)))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-header-observer-cancellation
     ;; Error case: cancellation in the header observer must suppress the current body chunk.
     ;; A large response also checks that already queued later chunks stay suppressed.
     (mat-requires (websockets)
       (andmap
        (lambda (action)
          (call-with-h2-fixture
           10
           (lambda (port command)
             (let ([client (http-open)] [events '()] [operation #f])
               (dynamic-wind
                 void
                 (lambda ()
                   (http-client-version-set! client 'h2)
                   (http-set-timeout! client 2000)
                   (set! operation
                         (http-send/nonblocking
                          client (make-http-request 'get (format "http://127.0.0.1:~a/large" port))
                          (make-http-body-sink
                           (lambda (bytes start count) (set! events (cons 'body events)))
                           (lambda () (set! events (cons 'finished events)))
                           (lambda (status headers)
                             (set! events (cons 'headers events))
                             (case action
                               [(close) (http-close client)]
                               [(cancel) (http-cancel-pending! client)]
                               [(operation) (net-operation-cancel! operation)])))))
                   (guard (failure [else (void)]) (net-operation-wait operation))
                   (http-close client)
                   (and (eq? 'cancelled (net-operation-state operation))
                        (equal? '(headers finished) (reverse events))))
                 (lambda () (http-close client)))))))
        '(close cancel operation))))

(mat net-lws-http2-live-concurrent-streams
     (mat-requires (websockets)
       (call-with-h2-fixture
        2
        (lambda (port command)
          (let ([client (http-open)])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 3000)
                (let* ([first (http-send/nonblocking
                               client (make-http-request 'get
                                                         (format "http://127.0.0.1:~a/hold" port)) #f)]
                       [second (http-send/nonblocking
                                client (make-http-request 'get
                                                          (format "http://127.0.0.1:~a/hold" port)) #f)])
                  (let ([started (await-h2-streams (list first second) command 2)])
                    (command 'release)
                    (let ([responses (map net-operation-wait (list first second))])
                      (and started
                           (= 2 (length responses))
                           (for-all (lambda (response)
                                     (= 200 (http-response-status response)))
                                    responses)
                           (= 1 (cadr started)))))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-live-flow-control
     ;; A large response must cross repeated writable/acknowledgement boundaries intact.
     ;; Avoid splitting every frame into 4 KiB callbacks despite the larger payload buffer.
     (mat-requires (websockets)
       (call-with-h2-fixture
        4
        (lambda (port command)
          (let ([client (http-open)] [received 0] [largest 0])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 4000)
                (let ([response
                       (net-operation-wait
                        (http-send/nonblocking
                         client (make-http-request 'get
                           (format "http://127.0.0.1:~a/large" port))
                         (make-http-body-sink
                          (lambda (bytes start count)
                            (set! largest (max largest count))
                            (set! received (+ received count))))))])
                  (and (= 200 (http-response-status response))
                       (= 262144 received)
                       (> largest 4096)
                       (<= largest 65536))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-live-peer-settings-goaway
     ;; Error case: LWS 4.5.8 rejects oversubscription with GOAWAY instead of queuing the excess.
     (mat-requires (websockets)
       (call-with-h2-fixture
        2
        (lambda (port command)
          (let ([client (http-open)])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 4000)
                (let ([operations
                        (map (lambda (index)
                               (http-send/nonblocking client
                                 (make-http-request 'get
                                   (format "http://127.0.0.1:~a/hold/~a" port index))))
                             (iota 6))])
                  (let drain ([remaining 1500])
                    (for-each (lambda (operation)
                                (when (eq? 'pending (net-operation-state operation))
                                  (net-operation-step! operation))) (reverse operations))
                    (cond
                     [(for-all (lambda (operation)
                                 (not (eq? 'pending (net-operation-state operation)))) operations)
                      (let ([stats (command 'stats)])
                        (and (= 1 (cadr stats)) (positive? (list-ref stats 6))
                             (positive? (list-ref stats 7))
                             (<= (list-ref stats 3) 2)
                             (for-all (lambda (operation)
                                        (and (eq? 'failed (net-operation-state operation))
                                             (net-error? (net-operation-condition operation))
                                             (not (eq? 'timeout
                                                        (net-error-kind
                                                         (net-operation-condition operation))))))
                                      operations)))]
                     [(fxzero? remaining) #f]
                     [else (milisleep 1) (drain (fx1- remaining))]))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-live-network-close
     ;; Error case: closing a physical H2 connection terminates every outstanding child.
     (mat-requires (websockets)
       (call-with-h2-fixture
        4
        (lambda (port command)
          (let ([client (http-open)])
            (dynamic-wind
              void
              (lambda ()
                (http-client-version-set! client 'h2)
                (http-set-timeout! client 1500)
                (let ([operations
                       (map (lambda (index)
                              (http-send/nonblocking client
                                (make-http-request 'get (format "http://127.0.0.1:~a/hold" port))))
                            (iota 3))])
                  (let ([started (await-h2-streams operations command 3)])
                    (command 'close)
                    (and started
                         (for-all
                          (lambda (operation)
                            (guard (condition
                                    [else (and (net-error? condition)
                                               (not (eq? 'timeout (net-error-kind condition))))])
                              (net-operation-wait operation) #f))
                          operations)))))
              (lambda () (http-close client))))))))

(mat net-lws-http2-live-tls-alpn
     ;; A trusted localhost certificate must negotiate H2 through the LWS ALPN path.
     (mat-requires (openssl websockets)
       (if (not run-live-h2-tls-tests?)
           #t
           (call-with-h2-tls-fixture
            4
            (lambda (port command)
              (let ([tls (make-tls-context 'client)] [client #f])
                (dynamic-wind
                  void
                  (lambda ()
                    (tls-context-load-ca-file! tls "/tmp/cspp-net-test-cert.pem")
                    (tls-context-set-verify! tls #t)
                    (set! client (make-lws-http2-client 64 65536 64
                                                          (tls-context-native-handle tls)
                                                          4))
                    (let ([response
                           (net-operation-wait
                            (lws-http2-request/nonblocking
                             client (make-h2-tls-test-request "/" port) #f))])
                      (and (= 200 (transport-response-status response))
                           (eq? 'h2 (transport-response-version response)))))
                  (lambda ()
                    (when client (lws-http2-client-close! client))
                    (close-tls-context tls)))))))))

(mat net-http-live-tls-http1-policy
     (mat-requires (openssl websockets)
       (call-with-h2-tls-fixture
        4
        (lambda (port command)
          (let ([tls (make-tls-context 'client)] [client #f])
            (dynamic-wind
              void
              (lambda ()
                (tls-context-load-ca-file! tls "/tmp/cspp-net-test-cert.pem")
                (tls-context-set-verify! tls #t)
                (set! client (http-open tls))
                (http-client-version-set! client 'http/1.1)
                (http-set-timeout! client 2000)
                (let ([response (http-get client (format "https://127.0.0.1:~a/" port))])
                  (and (= 200 (http-response-status response))
                       (eq? 'h1 (http-response-version response)))))
              (lambda () (when client (http-close client)) (close-tls-context tls))))))))

(mat net-http-live-tls-untrusted
     ;; Error case: an untrusted certificate must fail through the public HTTPS API.
     (mat-requires (openssl websockets)
       (call-with-h2-tls-fixture
        4
        (lambda (port command)
          (let ([tls (make-tls-context 'client)] [client #f])
            (dynamic-wind
              void
              (lambda ()
                (tls-context-set-verify! tls #t)
                (set! client (http-open tls))
                (http-client-version-set! client 'http/1.1)
                (http-set-timeout! client 2000)
                (guard (condition [else (net-error? condition)])
                  (http-get client (format "https://127.0.0.1:~a/" port)) #f))
              (lambda () (when client (http-close client)) (close-tls-context tls))))))))
