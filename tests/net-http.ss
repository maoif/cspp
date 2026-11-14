(import (cspp)
        (cspp file)
        (cspp path)
        (cspp net)
        (cspp net private lws-http1)
        (cspp net private lws-http2)
        (cspp net private lws-reactor)
        (cspp net private lws-transport)
        (cspp net private http)
        (cspp net private lws-ffi))

(load "net-common.ss")

(define lws-http-available?
  (let ([status (lws-status)])
    (and (vector? status) (vector-ref status 0))))

;; These bounded loopback fixtures run against the supported LWS runtime on this host.
(define run-live-http1-tests? #t)

(define read-http-request-head
  (lambda (port)
    (let ([request-line (read-crlf-line port)])
      (let loop ([headers '()])
        (let ([line (read-crlf-line port)])
          (if (string=? line "")
              (values request-line (reverse headers))
              (let ([colon
                     (let find-colon ([index 0])
                       (cond
                        [(fx= index (string-length line))
                         (errorf 'read-http-request-head "header has no colon: ~s" line)]
                        [(char=? (string-ref line index) #\:) index]
                        [else (find-colon (fx1+ index))]))])
                (loop (cons (cons (substring line 0 colon)
                                  (string-trim (substring line (+ colon 1)
                                                          (string-length line))))
                            headers)))))))))

(define fixture-header-ref
  (lambda (headers name default)
    (let ([entry (find (lambda (entry) (string-ci=? (car entry) name)) headers)])
      (if entry (cdr entry) default))))

(define concatenate-bytevectors
  (lambda (bytevector*)
    (let ([result
           (make-bytevector
            (fold-left (lambda (length bytes) (+ length (bytevector-length bytes)))
                       0 bytevector*) 0)])
      (let loop ([rest bytevector*] [offset 0])
        (unless (null? rest)
          (let ([bytes (car rest)])
            (bytevector-copy! bytes 0 result offset (bytevector-length bytes))
            (loop (cdr rest) (+ offset (bytevector-length bytes))))))
      result)))

(define start-http1-fixture
  (lambda (handler)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (socket-set-blocking! listener #f)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values port
                (fork-thread
                 (lambda ()
                   (dynamic-wind
                     void
                     (lambda ()
                       (let-values ([(client peer) (accept-http-fixture listener)])
                         (dynamic-wind
                           void
                           (lambda () (call-with-socket-ports client handler))
                           (lambda () (close-socket client)))))
                     (lambda () (close-socket listener))))))))))

(define accept-http-fixture
  (lambda (listener)
    (let loop ([remaining 2000])
      (call-with-values
        (lambda () (socket-accept/nonblocking listener))
        (case-lambda
          [(client peer)
           (socket-set-blocking! client #t)
           (values client peer)]
          [(pending)
           (when (fxzero? remaining) (errorf 'accept-http-fixture "accept deadline expired"))
           (milisleep 1)
           (loop (fx1- remaining))])))))

(define http-condition-message-contains?
  (lambda (fragment thunk)
    (guard (condition
            [else
             (string-contains?
              (call-with-string-output-port
               (lambda (port) (display-condition condition port)))
              fragment)])
      (thunk)
      #f)))

(define call-with-http1-response
  (lambda (wire procedure)
    (let-values ([(port thread)
                  (start-http1-fixture
                   (lambda (input output)
                     (read-http-request-head input)
                     (put-bytevector output (string->utf8 wire))
                     (flush-output-port output)))])
      (let ([client (http-open)])
        (dynamic-wind
          void
          (lambda ()
            (http-set-timeout! client 1500)
            (procedure client (format "http://127.0.0.1:~a/live" port)))
          (lambda () (http-close client) (thread-join thread)))))))

(mat net-http-live-eof-delimited
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\neof-body"
        (lambda (client uri)
          (let ([response (http-get client uri)])
            (and (eq? 'h1 (http-response-version response))
                 (equal? #vu8(101 111 102 45 98 111 100 121) (http-response-body response))))))))

(mat net-http-live-streaming-receive-buffer
     ;; Large responses should use the payload buffer instead of 4 KiB reactor handoffs.
     (mat-requires (websockets)
       (let ([body (make-string 262144 #\x)])
         (call-with-http1-response
          (string-append "HTTP/1.1 200 OK\r\nContent-Length: 262144\r\n"
                         "Connection: close\r\n\r\n" body)
          (lambda (client uri)
            (let ([received (make-bytevector 262144)] [offset 0] [largest 0])
              (let ([response
                     (net-operation-wait
                      (http-send/nonblocking
                       client (make-http-request 'get uri)
                       (make-http-body-sink
                        (lambda (bytes start count)
                          (bytevector-copy! bytes start received offset count)
                          (set! offset (fx+ offset count))
                          (set! largest (max largest count))))))])
                (and (= 200 (http-response-status response))
                     (= 262144 offset)
                     (equal? received (string->utf8 body))
                     (> largest 4096)
                     (<= largest 65536)))))))))

(mat net-http-live-receive-queue-exhaustion-fails
     ;; Error case: a full event pool must fail a real body callback without losing bytes silently.
     (mat-requires (websockets)
       (let ([phase (box 0)] [context (lws-context-open 2 65536)])
         (let-values ([(port worker)
                       (start-http1-fixture
                        (lambda (input output)
                          (read-http-request-head input)
                          (put-bytevector output
                           (string->utf8 "HTTP/1.1 200 OK\r\nContent-Length: 32\r\n\r\n"))
                          (flush-output-port output)
                          (let wait ([remaining 3000])
                            (when (and (zero? (unbox phase)) (positive? remaining))
                              (milisleep 1) (wait (fx1- remaining))))
                          (when (= 1 (unbox phase))
                            (put-bytevector output (make-bytevector 16 120))
                            (flush-output-port output)
                            (let wait ([remaining 3000])
                              (when (and (= 1 (unbox phase)) (positive? remaining))
                                (milisleep 1) (wait (fx1- remaining)))))))])
           (define await-metric
             (lambda (index predicate)
               (let loop ([remaining 100])
                 (cond
                  [(predicate (vector-ref (lws-context-pool-metrics context) index)) #t]
                  [(zero? remaining) #f]
                  [else (lws-context-service context 10) (loop (fx1- remaining))]))))
           (dynamic-wind
             void
             (lambda ()
               (and (lws-client-start context 1 1 1 "127.0.0.1" port #f
                                      "GET" "127.0.0.1" "/")
                    (await-metric 1 (lambda (count) (= count 2)))
                    (begin (set-box! phase 1) #t)
                    (await-metric 4 positive?)
                    (let drain ([failed? #f])
                      (let ([event (lws-context-next-event context)])
                        (if event
                            (and (not (eq? 'complete (vector-ref event 0)))
                                 (drain (or failed? (eq? 'failed (vector-ref event 0)))))
                            failed?)))))
             (lambda ()
               (set-box! phase 2)
               (lws-context-close context)
               (thread-join worker)))))))

(mat net-http-live-truncated-body
     ;; Error case: EOF before Content-Length must not produce a successful response.
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nContent-Length: 100\r\nConnection: close\r\n\r\nshort"
        (lambda (client uri)
          (guard (condition [else #t]) (http-get client uri) #f)))))

(mat net-http-live-unsupported-trailers
     ;; Error case: LWS 4.5.8 rejects trailer fields; CSPP must not reparse failed wire data.
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n1\r\nx\r\n0\r\nX-End: yes\r\n\r\n"
        (lambda (client uri)
          (guard (condition [else (net-error? condition)]) (http-get client uri) #f)))))

(mat net-http-live-sink-failure
     ;; Error case: preserve the consumer condition even if its finalizer also raises.
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndata"
        (lambda (client uri)
          (let ([failure (condition (make-message-condition "consumer failed"))]
                [finished 0])
            (let ([operation
                   (http-send/nonblocking client (make-http-request 'get uri)
                     (make-http-body-sink
                      (lambda (bytes start count) (raise failure))
                      (lambda () (set! finished (fx1+ finished))
                        (errorf 'finisher "secondary failure"))))])
              (and (guard (condition [else (eq? failure condition)])
                     (net-operation-wait operation) #f)
                   (begin (http-close client) #t)
                   (= finished 1))))))))

(mat net-http-live-source-failure
     ;; Error case: producer failure closes its source once and survives a failing closer.
     (mat-requires (websockets)
       (let ([closed 0] [failure (condition (make-message-condition "producer failed"))])
         (let-values ([(port thread)
                       (start-http1-fixture
                        (lambda (input output)
                          (read-http-request-head input)
                          (get-bytevector-all input)))])
           (let ([client (http-open)])
             (dynamic-wind
               void
               (lambda ()
                 (http-set-timeout! client 1000)
                 (and (guard (condition [else (eq? failure condition)])
                        (http-send client
                         (make-http-request 'post (format "http://127.0.0.1:~a/source" port)
                           '() (make-http-body-source
                                (lambda (maximum) (raise failure)) 8
                                (lambda () (set! closed (fx1+ closed))
                                  (errorf 'closer "secondary failure")))))
                        #f)
                      (= closed 1)))
               (lambda () (http-close client) (thread-join thread))))))))

(mat net-http-live-cancel-finalizers
     ;; Error case: cancellation before body production still closes the source and sink once.
     (mat-requires (websockets)
       (let ([closed 0] [finished 0] [client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let ([operation
                    (http-send/nonblocking client
                      (make-http-request 'post "http://127.0.0.1:1/cancel" '()
                        (make-http-body-source (lambda (maximum) (eof-object)) #f
                                              (lambda () (set! closed (fx1+ closed)))))
                      (make-http-body-sink void (lambda () (set! finished (fx1+ finished)))))])
               (net-operation-cancel! operation)
               (http-cancel-pending! client)
               (http-close client)
               (and (eq? 'cancelled (net-operation-state operation))
                    (= closed 1) (= finished 1))))
           (lambda () (http-close client))))))

(mat net-http-live-download
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nContent-Length: 8\r\nConnection: close\r\n\r\ndownload"
        (lambda (client uri)
          (let ([path (format "/tmp/cspp-http-download-~a" (get-process-id))])
            (dynamic-wind
              void
              (lambda ()
                (let ([response (http-download client uri path)])
                  (and (= 200 (http-response-status response))
                       (not (http-response-body response))
                       (equal? (string->utf8 "download") (read-u8vec path)))))
              (lambda () (when (file-exists? path) (delete-file path)))))))))

(mat net-http-live-header-readiness
     (mat-requires (websockets)
       (call-with-http1-response
        "HTTP/1.1 200 OK\r\nContent-Length: 1\r\nConnection: close\r\n\r\nx"
        (lambda (unused uri)
          (let ([client (make-lws-http1-client 64 65536 64)] [ready 0])
            (dynamic-wind
              void
              (lambda ()
                (let* ([parsed (string->uri uri)]
                       [request
                        (make-normalized-http-request "GET" parsed 'http "127.0.0.1"
                          (uri-port parsed) #f "/ready" '() #f #f
                          (make-http-request-policy '() #f #f #f 0 'http/1.1 #f #f 0 #f))]
                       [response (net-operation-wait
                                  (lws-http1-request/nonblocking client request #f
                                    (lambda () (set! ready (fx1+ ready)))))])
                  (and (= ready 1) (= 200 (transport-response-status response)))))
              (lambda () (lws-http1-client-close! client))))))))

(mat net-http-live-redirect-credentials-and-sink
     ;; Cross-origin redirects must strip credentials and stream only the final response body.
     (mat-requires (websockets)
       (let ([received #f] [chunks '()] [finished 0])
         (let-values ([(target-port target-thread)
                       (start-http1-fixture
                        (lambda (input output)
                          (let-values ([(line headers) (read-http-request-head input)])
                            (set! received headers))
                          (put-bytevector output
                            (string->utf8 "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nfinal"))
                          (flush-output-port output)))])
           (dynamic-wind
             void
             (lambda ()
               (call-with-http1-response
                (format "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:~a/final\r\nContent-Length: 4\r\nConnection: close\r\n\r\nskip"
                        target-port)
                (lambda (client uri)
                  (http-client-auth-set! client 'bearer "private-token")
                  (http-set-header! client "Cookie" "secret=1")
                  (let ([response
                         (net-operation-wait
                          (http-send/nonblocking client
                            (make-http-request 'get uri '(("Authorization" . "Basic private")))
                            (make-http-body-sink
                             (lambda (bytes start count)
                               (set! chunks (cons (bytevector-copy bytes) chunks)))
                             (lambda () (set! finished (fx1+ finished))))))])
                    (and (= 200 (http-response-status response))
                         (= finished 1)
                         (equal? (string->utf8 "final")
                                 (concatenate-bytevectors (reverse chunks)))
                         (not (fixture-header-ref received "Authorization" #f))
                         (not (fixture-header-ref received "Cookie" #f)))))))
             (lambda () (thread-join target-thread)))))))

(mat net-http-live-redirect-deadline
     ;; Error case: a redirect must retain the original deadline rather than start a new one.
     (mat-requires (websockets)
       (let-values ([(target-port target-thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (read-http-request-head input)
                        (milisleep 250)))])
         (dynamic-wind
           void
           (lambda ()
             (call-with-http1-response
              (format "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:~a/slow\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
                      target-port)
              (lambda (client uri)
                (http-set-timeout! client 150)
                (let ([operation (http-send/nonblocking client (make-http-request 'get uri))])
                  (net-operation-step! operation)
                  (let ([deadline (net-operation-deadline-ms operation)])
                    (guard (condition
                            [else (and (net-error? condition)
                                       (eq? 'timeout (net-error-kind condition)))])
                      (net-operation-wait operation)
                      #f))))))
           (lambda () (thread-join target-thread))))))

(mat net-http-live-proxy-transition
     ;; An active direct request keeps its transport when later requests switch to a proxy.
     (mat-requires (websockets)
       (let ([proxy-line #f])
         (let-values ([(proxy-port proxy-thread)
                       (start-http1-fixture
                        (lambda (input output)
                          (let-values ([(line headers) (read-http-request-head input)])
                            (set! proxy-line line))
                          (put-bytevector output
                            (string->utf8 "HTTP/1.1 200 Connection established\r\n\r\n"))
                          (flush-output-port output)
                          (read-http-request-head input)
                          (put-bytevector output
                            (string->utf8 "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nproxy"))
                          (flush-output-port output)))])
           (dynamic-wind
             void
             (lambda ()
               (call-with-http1-response
                "HTTP/1.1 200 OK\r\nContent-Length: 6\r\nConnection: close\r\n\r\ndirect"
                (lambda (client uri)
                  (let ([direct (http-send/nonblocking client (make-http-request 'get uri))])
                    (http-client-proxy-set! client
                      (make-http-proxy (format "http://127.0.0.1:~a" proxy-port)))
                    (let* ([through-proxy (http-get client "http://example.invalid/proxy")]
                           [original (net-operation-wait direct)])
                      (and (equal? (string->utf8 "direct") (http-response-body original))
                           (equal? (string->utf8 "proxy") (http-response-body through-proxy))
                           (string-contains? proxy-line "CONNECT example.invalid:80")))))))
             (lambda () (thread-join proxy-thread)))))))

(mat net-http-live-redirect-limit
     ;; Error case: an endless redirect chain fails explicitly and finishes its sink once.
     (mat-requires (websockets)
       (let* ([port (+ 40000 (modulo (get-process-id) 10000))]
              [server (http-listen "127.0.0.1" port)] [client (http-open)]
              [worker #f] [finished 0])
         (dynamic-wind
           void
           (lambda ()
             (http-register-handler! server "/loop"
               (lambda (request)
                 (make-http-response 302 "Found" '(("Location" . "/loop")) "redirect")))
             (set! worker (fork-thread (lambda ()
                                        (guard (condition [else (void)])
                                          (http-serve-loop server)))))
             (http-set-timeout! client 3000)
             (and (guard (condition
                          [else (and (net-error? condition)
                                     (eq? 'redirect-limit (net-error-kind condition)))])
                    (net-operation-wait
                     (http-send/nonblocking client
                       (make-http-request 'get (format "http://127.0.0.1:~a/loop" port))
                       (make-http-body-sink void (lambda () (set! finished (fx1+ finished))))))
                    #f)
                  (= finished 1)))
           (lambda () (http-close client) (http-server-close server)
             (when worker (thread-join worker)))))))

(define make-h2-test-request
  (case-lambda
    [(path) (make-h2-test-request path #f)]
    [(path deadline)
     (make-normalized-http-request
      "GET" #f 'http "h2.test" 80 #f path '() #f #f
      (make-http-request-policy '() #f #f #f 0 'h2 #f #f 0 deadline))]))

(define wait-for-reactor-commands
  (lambda (reactor)
    (let loop ([remaining 200])
      (cond
       [(zero? (vector-ref (lws-reactor-pool-metrics reactor) 1)) #t]
       [(zero? remaining) #f]
       [else
        (milisleep 1)
        (loop (fx1- remaining))]))))

(define inject-h2-event!
  (lambda (reactor tag connection-id stream-id generation status payload scope)
    (and (lws-reactor-inject-event!
          reactor tag connection-id stream-id generation status payload
          'http2 #f 0 #f scope)
         (wait-for-reactor-commands reactor))))

(define finish-h2-stream!
  (lambda (reactor operation connection-id stream-id generation status body)
    (and (inject-h2-event! reactor 'headers connection-id stream-id generation
                           status #vu8() 'none)
         (or (zero? (bytevector-length body))
             (inject-h2-event! reactor 'readable connection-id stream-id generation
                               0 body 'none))
         (begin
           (net-operation-step! operation)
           #t)
         (inject-h2-event! reactor 'complete connection-id stream-id generation
                           0 #vu8() 'stream)
         (begin
           (net-operation-step! operation)
           (eq? 'completed (net-operation-state operation))))))

(mat net-http-lws-http1-boundary
     (and (procedure? make-lws-http1-client)
          (procedure? lws-http1-client-close!)
          (procedure? lws-http1-request/nonblocking)
          (procedure? lws-http1-client-pool-metrics)))

(mat net-http-stream-body
     (let ([source (make-http-body-source (lambda (maximum-bytes) (eof-object)) #f)]
           [sink (make-http-body-sink (lambda (bytevector start count) (void)))])
       (and (http-body-source? source)
            (not (http-body-source-length source))
            (eof-object? (http-body-source-read source 65536))
            (http-body-sink? sink)
            (begin
              (http-body-sink-write! sink #vu8(1 2 3) 0 3)
              (http-body-sink-finish! sink)
              #t)))
     ;; Error case: a body source read size must be positive.
     (http-condition-message-contains?
      "positive"
      (lambda ()
      (http-body-source-read
         (make-http-body-source (lambda (maximum-bytes) (eof-object)) #f)
         0))))

(mat net-http-framing-validation
     ;; Error case: Content-Length and Transfer-Encoding must not be sent together.
     (mat-requires (websockets)
       (http-condition-message-contains?
        "conflicting"
        (lambda ()
          (http-send/nonblocking
           (http-open)
           (make-http-request 'post "http://127.0.0.1/"
                              '(("Content-Length" . "1")
                                ("Transfer-Encoding" . "chunked"))
                              "x"))))))

(mat net-http-fixed-response
     (mat-requires (websockets)
       (or (not run-live-http1-tests?) (not lws-http-available?)
           (let-values ([(port thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (read-http-request-head input)
                        (put-bytevector output
                         (string->utf8
                          "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello"))
                        (flush-output-port output)))])
           (let ([client (http-open)])
           (dynamic-wind void
             (lambda ()
               ;; Allocation pressure while the fixture awaits accept must not stall GC progress.
               (do ([index 0 (fx1+ index)]) ((fx= index 32))
                 (unless (fx= 17 (bytevector-u8-ref (make-bytevector 1048576 17) 0))
                   (errorf 'net-http-fixed-response "invalid allocation result")))
               (let ([response (http-get client
                                          (format "http://127.0.0.1:~a/fixed" port))])
                 (and (= (http-response-status response) 200)
                      (eq? (http-response-version response) 'h1)
                      (equal? (utf8->string (http-response-body response)) "hello"))))
               (lambda () (http-close client) (thread-join thread))))))))

(mat net-http-segmented-chunked-response
     (mat-requires (websockets)
       (or (not run-live-http1-tests?) (not lws-http-available?)
           (let-values ([(port thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (read-http-request-head input)
                        (for-each
                         (lambda (part)
                           (put-bytevector output (string->utf8 part))
                           (flush-output-port output)
                           (milisleep 10))
                         '("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n"
                           "Connection: close\r\n\r\n"
                           "4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n"))))])
           (let ([client (http-open)])
           (dynamic-wind void
             (lambda ()
               (let ([response (http-get client
                                          (format "http://127.0.0.1:~a/chunked" port))])
                 (and (= (http-response-status response) 200)
                      (equal? (utf8->string (http-response-body response)) "Wikipedia"))))
               (lambda () (http-close client) (thread-join thread))))))))

(mat net-http-complete-response-metadata
     (mat-requires (websockets)
       (let-values ([(port thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (read-http-request-head input)
                        (put-bytevector output
                          (string->utf8
                            (string-append
                              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\nConnection: close\r\n"
                              "ETag: abc\r\nCache-Control: no-store\r\nX-Empty:\r\n"
                              "Set-Cookie: a=1\r\nSet-Cookie: b=2\r\n\r\nx")))
                        (flush-output-port output)))])
         (let ([client (http-open)])
           (dynamic-wind
             void
             (lambda ()
               (let* ([response (http-get client (format "http://127.0.0.1:~a/headers" port))]
                      [headers (http-response-headers response)])
                 (and (equal? "abc" (http-header-ref headers "etag" #f))
                      (equal? "no-store" (http-header-ref headers "cache-control" #f))
                      (equal? "" (http-header-ref headers "x-empty" #f))
                      (equal? '("a=1" "b=2")
                              (map cdr (filter (lambda (entry)
                                                 (string-ci=? (car entry) "set-cookie"))
                                               headers))))))
             (lambda () (http-close client) (thread-join thread)))))))

(mat net-http-streaming-upload
     (mat-requires (websockets)
       (or (not run-live-http1-tests?) (not lws-http-available?)
           (let ([received #f])
         (let-values ([(port thread)
                       (start-http1-fixture
                        (lambda (input output)
                          (let-values ([(request-line headers)
                                        (read-http-request-head input)])
                            (let ([length (string->number
                                           (fixture-header-ref headers "Content-Length" "0"))])
                              (set! received (get-bytevector-n input length))))
                          (put-bytevector output
                           (string->utf8
                            "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                          (flush-output-port output)))])
             (let ([client (http-open)] [payload (string->utf8 "upload-body")])
             (dynamic-wind void
               (lambda ()
                 (let ([response
                        (http-send client
                         (make-http-request 'put
                          (format "http://127.0.0.1:~a/upload" port)
                          `(("Content-Length" . ,(number->string
                                                  (bytevector-length payload))))
                          payload))])
                   (and (= (http-response-status response) 204)
                        (equal? received payload))))
                 (lambda () (http-close client) (thread-join thread)))))))))

(mat net-http-response-sink
     (mat-requires (websockets)
       (or (not run-live-http1-tests?) (not lws-http-available?)
           (let ([chunks '()] [finished 0])
         (let-values ([(port thread)
                       (start-http1-fixture
                        (lambda (input output)
                          (read-http-request-head input)
                          (put-bytevector output
                           (string->utf8
                            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndata"))
                          (flush-output-port output)))])
             (let ([client (http-open)]
                 [sink (make-http-body-sink
                        (lambda (bytes start count)
                          (let ([copy (make-bytevector count 0)])
                            (bytevector-copy! bytes start copy 0 count)
                            (set! chunks (cons copy chunks))))
                        (lambda () (set! finished (+ finished 1))))])
             (dynamic-wind void
               (lambda ()
                 (let ([response
                        (net-operation-wait
                         (http-send/nonblocking client
                          (make-http-request 'get
                           (format "http://127.0.0.1:~a/sink" port)) sink))])
                   (and (not (http-response-body response))
                        (= finished 1)
                        (equal? (utf8->string
                                 (concatenate-bytevectors (reverse chunks))) "data"))))
               (lambda () (http-close client) (thread-join thread)))))))))

(mat net-http-http1-explicit-nonreuse
     (mat-requires (websockets)
       (or (not run-live-http1-tests?) (not lws-http-available?)
           (let ([listener (open-socket 'inet 'stream)] [accept-count 0])
             (socket-set-option! listener 'reuse-address #t)
             (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
             (socket-listen! listener 4)
             (socket-set-blocking! listener #f)
             (let* ([port (socket-address-port (socket-local-address listener))]
                    [thread
                     (fork-thread
                      (lambda ()
                        (let loop ([remaining 2])
                          (unless (fxzero? remaining)
                            (let-values ([(socket peer) (accept-http-fixture listener)])
                              (set! accept-count (fx1+ accept-count))
                              (let ([input (open-socket-input-port socket)]
                                    [output (open-socket-output-port socket)])
                                (dynamic-wind
                                  void
                                  (lambda ()
                                    (read-http-request-head input)
                                    (put-bytevector
                                     output
                                     (string->utf8
                                      "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                                    (flush-output-port output))
                                  (lambda () (close-port input) (close-port output))))
                              (close-socket socket))
                            (loop (fx1- remaining))))
                        (close-socket listener)))])
               (let ([client (http-open)]
                     [uri (format "http://127.0.0.1:~a/nonreuse" port)])
                 (http-set-timeout! client 1000)
                 (dynamic-wind
                   void
                   (lambda ()
                     (and (= 204 (http-response-status (http-get client uri)))
                          (= 204 (http-response-status (http-get client uri)))
                          (begin (thread-join thread) #t)
                          (= accept-count 2)))
                   (lambda () (http-close client)))))))))

(mat net-http-transport-header-roundtrip
     (let ([headers '(("set-cookie" . "one=1") ("set-cookie" . "two=2") ("x-empty" . ""))])
       (and (equal? headers
                    (lws-transport-decode-headers (lws-transport-encode-headers headers)))
            (null? (lws-transport-decode-headers #vu8())))))

(mat net-http-transport-rejects-incomplete-headers
     ;; Error cases: unterminated name/value, an incomplete suffix, and an empty name.
     (for-all
      (lambda (bytes)
        (guard (condition [(net-error? condition) #t] [else #f])
          (lws-transport-decode-headers bytes)
          #f))
      (list #vu8(120) #vu8(120 0) #vu8(120 0 121)
            #vu8(120 0 121 0 122) #vu8(0 121 0))))

(mat net-http-transport-rejects-ambiguous-headers
     ;; Error cases: empty names and embedded NULs cannot roundtrip through native metadata.
     (for-all
      (lambda (headers)
        (guard (condition [(net-error? condition) #t] [else #f])
          (lws-transport-encode-headers headers)
          #f))
      (list '(("" . "value"))
            (list (cons "x" (string #\nul)))
            (list (cons (string #\nul) "value")))))

(mat net-http-transport-command-rejection
     ;; Error case: an unserviced full command queue must reject upload and consumption.
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 8 64 1)])
         (dynamic-wind
           void
           (lambda ()
             (and (lws-reactor-client-acquire! reactor 1 1 1)
                  (guard (condition [(net-error? condition) #t] [else #f])
                    (lws-transport-submit-body! reactor 1 1 1 #vu8(1) #f)
                    #f)
                  (guard (condition [(net-error? condition) #t] [else #f])
                    (lws-transport-submit-body! reactor 1 1 1 #vu8() #t)
                    #f)
                  (guard (condition [(net-error? condition) #t] [else #f])
                    (lws-transport-consume-body! reactor 1 1 1 1)
                    #f)))
           (lambda () (lws-reactor-shutdown! reactor))))))

(mat net-http2-rejects-truncated-metadata
     ;; Error case: a missing header terminator must fail, even with completion already queued.
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [operation (lws-http2-request/nonblocking
                          client (make-h2-test-request "/truncated") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'headers 1 1 1 200 #vu8(120 0 121) 'none)
                  (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                  (begin
                    (net-operation-step! operation)
                    (and (eq? 'failed (net-operation-state operation))
                         (net-error? (net-operation-condition operation))))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-direct-transport-identity
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [operation (lws-http2-request/nonblocking
                          client (make-h2-test-request "/identity") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! operation) #t)
                  (finish-h2-stream! reactor operation 1 1 1 200 #vu8(111 107))
                  (let ([response (net-operation-result operation)])
                    (and (transport-response? response)
                         (= 1 (transport-response-connection-id response))
                         (eq? 'h2 (transport-response-version response))
                         (equal? #vu8(111 107) (transport-response-body response))))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-preserves-headers-and-trailers
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [headers '(("set-cookie" . "one=1") ("set-cookie" . "two=2"))]
              [trailers '(("x-checksum" . "verified"))]
              [operation (lws-http2-request/nonblocking
                          client (make-h2-test-request "/metadata") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'headers 1 1 1 200
                                    (lws-transport-encode-headers headers) 'none)
                  (inject-h2-event! reactor 'headers 1 1 1 -1
                                    (lws-transport-encode-headers trailers) 'none)
                  (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                  (begin
                    (net-operation-step! operation)
                    (and (eq? 'completed (net-operation-state operation))
                         (let ([response (net-operation-result operation)])
                           (and (equal? headers (transport-response-headers response))
                                (equal? trailers (transport-response-trailers response))))))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-rejects-truncated-trailers-before-finishing-sink
     ;; Error case: malformed trailers prevent completion and suppress the sink finish callback.
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [finished 0]
              [operation (lws-http2-request/nonblocking
                          client (make-h2-test-request "/bad-trailers")
                          (vector (lambda (bytes start count) (void))
                                  (lambda () (set! finished (fx1+ finished)))))])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'headers 1 1 1 200 #vu8() 'none)
                  (inject-h2-event! reactor 'headers 1 1 1 -1 #vu8(120 0 121) 'none)
                  (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                  (begin
                    (net-operation-step! operation)
                    (and (eq? 'failed (net-operation-state operation))
                         (net-error? (net-operation-condition operation))
                         (fxzero? finished)))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-independent-streams-all-wait-orders
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/one") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/two") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! first) #t)
                  (wait-for-reactor-commands reactor)
                  (finish-h2-stream! reactor second 1 2 2 202 #vu8(2))
                  (finish-h2-stream! reactor first 1 1 1 201 #vu8(1))
                  (= 201 (transport-response-status (net-operation-result first)))
                  (= 202 (transport-response-status (net-operation-result second)))
                  (= (transport-response-connection-id (net-operation-result first))
                     (transport-response-connection-id (net-operation-result second)))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-local-stream-bound-fifo
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 64 65536 64 0 1 #f)]
              [reactor (lws-http2-client-reactor client)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/first") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/second") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! first) #t)
                  (eq? 'pending (net-operation-state second))
                  (finish-h2-stream! reactor first 1 1 1 200 #vu8())
                  (begin (net-operation-step! second) #t)
                  (wait-for-reactor-commands reactor)
                  (finish-h2-stream! reactor second 1 2 2 200 #vu8())
                  (= 1 (transport-response-connection-id
                        (net-operation-result second)))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-queued-deadline
     ;; Error case: a request waiting for H2 admission must time out without cancelling its leader.
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 1 #f)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/leader") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/queued" 0) #f)])
         (dynamic-wind
           void
           (lambda ()
             (net-operation-step! second)
             (and (eq? 'failed (net-operation-state second))
                  (net-error? (net-operation-condition second))
                  (eq? 'timeout (net-error-kind (net-operation-condition second)))
                  (eq? 'pending (net-operation-state first))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-required-protocol-refusal
     ;; Error case: an H2 request must fail when LWS observes HTTP/1.
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [operation (lws-http2-request/nonblocking
                          client (make-h2-test-request "/refuse") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (lws-reactor-inject-event!
                   reactor 'connected 1 1 1 200 #vu8() 'http1 #f 0 #f 'none)
                  (wait-for-reactor-commands reactor)
                  (lws-reactor-inject-event!
                   reactor 'complete 1 1 1 0 #vu8() 'http1 #f 0 #f 'stream)
                  (wait-for-reactor-commands reactor)
                  (begin (net-operation-step! operation) #t)
                  (eq? 'failed (net-operation-state operation))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-stream-and-connection-failure-scope
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/reset") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/ok") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! first) #t)
                  (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'reset 1 1 1 8 #vu8() 'stream)
                  (begin (net-operation-step! first) #t)
                  (eq? 'failed (net-operation-state first))
                  (finish-h2-stream! reactor second 1 2 2 200 #vu8(2))))
           (lambda () (lws-http2-client-close! client)))))
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
              [reactor (lws-http2-client-reactor client)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/failed") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/sibling") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! first) #t)
                  (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'failed 1 1 1 111 #vu8() 'connection)
                  (begin (net-operation-step! first) #t)
                  (begin (net-operation-step! second) #t)
                  (eq? 'failed (net-operation-state first))
                  (eq? 'failed (net-operation-state second))))
           (lambda () (lws-http2-client-close! client))))))

(mat net-http2-cancellation-and-stale-generation
     (mat-requires (websockets)
       (let* ([client (make-lws-http2-client 64 65536 64 0 1 #f)]
              [reactor (lws-http2-client-reactor client)]
              [first (lws-http2-request/nonblocking client (make-h2-test-request "/cancel") #f)]
              [second (lws-http2-request/nonblocking client (make-h2-test-request "/next") #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (wait-for-reactor-commands reactor)
                  (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                  (begin (net-operation-step! first) #t)
                  (begin (net-operation-cancel! first) #t)
                  (eq? 'cancelled (net-operation-state first))
                  (wait-for-reactor-commands reactor)
                  (begin (net-operation-step! second) #t)
                  (wait-for-reactor-commands reactor)
                  (lws-reactor-inject-event!
                   reactor 'complete 1 1 1 0 #vu8(9) 'http2 #f 0 #f 'stream)
                  (wait-for-reactor-commands reactor)
                  (eq? 'pending (net-operation-state second))
                  (finish-h2-stream! reactor second 1 2 2 200 #vu8(2))))
           (lambda () (lws-http2-client-close! client))))))


(mat net-http-sink-response-headers
     (mat-requires (websockets)
       (let ([events '()])
         (call-with-http1-response
          "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndata"
          (lambda (client uri)
            (net-operation-wait
             (http-send/nonblocking
              client (make-http-request 'get uri)
              (make-http-body-sink
               (lambda (bytes start count) (set! events (cons 'body events)))
               (lambda () (set! events (cons 'finished events)))
               (lambda (status headers)
                 (set! events (cons (list status (http-header-ref headers "Content-Length"))
                                    events))))))
            (let ([ordered (reverse events)])
              (and (equal? '(200 "4") (car ordered))
                   (eq? 'finished (car (reverse ordered)))
                   (memq 'body ordered)
                   (= 1 (length (filter pair? ordered))))))))))


(mat net-http-sink-empty-response-headers
     (mat-requires (websockets)
       (let ([events '()])
         (call-with-http1-response
          "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
          (lambda (client uri)
            (net-operation-wait
             (http-send/nonblocking
              client (make-http-request 'get uri)
              (make-http-body-sink
               (lambda (bytes start count) (set! events (cons 'body events)))
               (lambda () (set! events (cons 'finished events)))
               (lambda (status headers)
                 (set! events (cons (list status (http-header-ref headers "Content-Length"))
                                    events))))))
            (equal? events '(finished (200 "0"))))))))

(mat net-http-sink-header-observer-failure
     ;; Error case: a header observer exception fails the operation and still finishes its sink.
     (mat-requires (websockets)
       (let ([finished 0] [body-seen? #f])
         (call-with-http1-response
          "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndata"
          (lambda (client uri)
            (guard (condition
                    [else (and (= 1 finished) (not body-seen?)
                               (message-condition? condition)
                               (string=? "header observer failed" (condition-message condition)))])
              (net-operation-wait
               (http-send/nonblocking
                client (make-http-request 'get uri)
                (make-http-body-sink
                 (lambda (bytes start count) (set! body-seen? #t))
                 (lambda () (set! finished (fx1+ finished)))
                 (lambda (status headers) (error 'test "header observer failed")))))
              #f))))))

(mat net-http-sink-header-observer-cancellation
     ;; Error case: cancelling from a header observer must never deliver a body after finishing.
     ;; Exercise both body and empty responses, and all public cancellation entry points.
     (mat-requires (websockets)
       (andmap
        (lambda (action)
          (andmap
           (lambda (body)
             (call-with-http1-response
              (format "HTTP/1.1 200 OK\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
                      (string-length body) body)
              (lambda (client uri)
                (let ([events '()] [operation #f])
                  (set! operation
                        (http-send/nonblocking
                         client (make-http-request 'get uri)
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
                       (equal? '(headers finished) (reverse events)))))))
           '("data" "")))
        '(close cancel operation))))
