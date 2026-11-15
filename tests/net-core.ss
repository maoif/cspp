(import (cspp)
        (cspp system)
        (cspp crypto)
        (cspp net))

(load "net-common.ss")

(define tls-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (string-contains? (net-error-message c) "timed out"))])
      (thunk)
      #f)))

(define tls-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                     (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define poll-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define current-monotonic-ms
  (lambda ()
    (let ([time (current-time 'time-monotonic)])
      (+ (* (time-second time) 1000)
         (quotient (time-nanosecond time) 1000000)))))

(define start-stalled-tls-handshake-server
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

(define start-stalled-tls-reader
  (lambda (delay-ms)
    (let ([release? #f]
          [listener (open-socket 'inet 'stream)]
          [ctx (make-tls-context 'server)]
          [cert-path "/tmp/cspp-net-test-cert.pem"]
          [key-path "/tmp/cspp-net-test-key.pem"])
      (write-bytevector-file cert-path tls-test-certificate)
      (write-bytevector-file key-path tls-test-private-key)
      (tls-context-load-cert! ctx cert-path)
      (tls-context-load-private-key! ctx key-path)
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                ctx
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (socket-set-option! client 'recv-buffer 1024)
                     (guard (c [else #f])
                       (let ([session (tls-accept ctx client)])
                         (let loop ()
                           (unless release?
                             (milisleep delay-ms)
                             (loop)))
                         (close-tls-session session)))
                     (close-socket client)
                     (close-socket listener)
                     (close-tls-context ctx))))
                (lambda () (set! release? #t)))))))

(mat net-errors
     (let ([err (make-net-error 'net-test 'parse "bad address" '(1 2 3))])
       (and (net-error? err)
            (eq? (net-error-who err) 'net-test)
            (eq? (net-error-kind err) 'parse)
            (equal? (net-error-message err) "bad address")
            (equal? (net-error-data err) '(1 2 3))))
     (guard (c [else (and (net-error? c)
                          (eq? (net-error-kind c) 'io))])
       (raise-net-error 'net-test 'io "closed")
       #f))

(mat net-ip
     (let ([ipv4 (string->ip-address "127.0.0.1")]
           [ipv6 (string->ip-address "2001:db8::1")])
       (and (ipv4-address? ipv4)
            (ipv6-address? ipv6)
            (equal? (ip-address->string ipv4) "127.0.0.1")
            (equal? (ip-address->string ipv6) "2001:db8::1")
            (ip-address-loopback? ipv4)
            (not (ip-address-loopback? ipv6))
            (ip-address-private? (string->ip-address "192.168.2.10"))
            (ip-address-private? (string->ip-address "fd12:3456::7"))
            (ip-address-multicast? (string->ip-address "239.1.2.3"))
            (ip-address-multicast? (string->ip-address "ff02::1"))))
     (let ([range (cidr-parse "192.168.10.0/24")])
       (and (cidr-contains? range (string->ip-address "192.168.10.42"))
            (not (cidr-contains? range (string->ip-address "192.168.11.42")))
            (equal? (ip-address->string (cidr-network-address range))
                    "192.168.10.0")
            (= (cidr-prefix-length range) 24)))
     (let ([range (cidr-parse "2001:db8::/32")])
       (and (cidr-contains? range (string->ip-address "2001:db8::9"))
            (not (cidr-contains? range (string->ip-address "2001:db9::1")))))
     (not (string->ip-address "999.0.0.1"))
     (not (cidr-parse "192.168.0.1/40")))

(mat net-uri
     (let ([u (string->uri "https://alice@example.com:443/a/../b/c?q=1&x=two#frag")])
       (and (uri? u)
            (equal? (uri-scheme u) "https")
            (equal? (uri-userinfo u) "alice")
            (equal? (uri-host u) "example.com")
            (= (uri-port u) 443)
            (equal? (uri-path u) "/a/../b/c")
            (equal? (uri-query u) "q=1&x=two")
            (equal? (uri-fragment u) "frag")
            (equal? (uri-authority u) "alice@example.com:443")
            (equal? (uri-path-segments u) '("a" ".." "b" "c"))
            (equal? (uri-query-alist u) '(("q" . "1") ("x" . "two")))
            (equal? (uri->string (uri-normalize u))
                    "https://alice@example.com/b/c?q=1&x=two#frag")))
     (let* ([base (string->uri "https://example.com/a/b/index.html")]
            [ref (string->uri "../api?q=test")]
            [resolved (uri-resolve base ref)])
       (equal? (uri->string resolved)
               "https://example.com/a/api?q=test"))
     (equal? (uri-encode "hello world/ok") "hello%20world%2Fok")
     (equal? (uri-decode "hello%20world%2Fok") "hello world/ok")
     (equal? (form-urlencode '(("q" . "hello world") ("lang" . "scheme")))
             "q=hello+world&lang=scheme")
     (equal? (form-urldecode "q=hello+world&lang=scheme")
             '(("q" . "hello world") ("lang" . "scheme"))))

(mat net-http
     (let* ([req (make-http-request 'get "https://example.com/api?q=1"
                                    '(("Accept" . "application/json")
                                      ("X-Test" . "one"))
                                    #vu8(1 2 3))]
            [headers0 (http-request-headers req)]
            [headers1 (http-header-add headers0 "X-Test" "two")]
            [headers2 (http-header-set headers1 'accept "text/plain")]
            [resp (make-http-response 200 "OK" headers2 "done")])
       (and (http-request? req)
            (equal? (http-request-method req) "GET")
            (equal? (uri->string (http-request-uri req))
                    "https://example.com/api?q=1")
            (equal? (http-header-ref headers0 "accept") "application/json")
            (equal? (http-header-ref headers1 "x-test") "one")
            (equal? (http-header-ref headers2 "accept") "text/plain")
            (equal? (http-request-body req) #vu8(1 2 3))
            (http-response? resp)
            (= (http-response-status resp) 200)
            (equal? (http-response-reason resp) "OK")
            (eq? (http-response-version resp) 'h1)
            (equal? (http-response-body resp) "done"))))

(mat net-address-dns
     (let ([addr (make-socket-address 'inet "127.0.0.1" 8080)])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'inet)
            (equal? (socket-address-host addr) "127.0.0.1")
            (= (socket-address-port addr) 8080)
            (not (socket-address-path addr))))
     (let ([addr (make-socket-address 'unix "/tmp/cspp-net.sock")])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'unix)
            (equal? (socket-address-path addr) "/tmp/cspp-net.sock")))
     (let ([addr (resolve-address "127.0.0.1" 80 'inet 'stream)])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'inet)))
     (let ([addrs (resolve-addresses "localhost" 80)])
       (and (pair? addrs)
            (andmap socket-address? addrs)))
     (mat-requires (cares)
       (let ([res (dns-resolve "localhost")])
         (and (dns-result? res)
              (pair? (dns-result-addresses res))
              (or (not (dns-result-canonname res))
                  (string? (dns-result-canonname res))))))
     (let ([name (dns-reverse-resolve (make-socket-address 'inet "127.0.0.1" 80))])
       (string? name)))

(mat net-address-validation
     (and
      (tls-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (make-socket-address 'inet "127.0.0.1" -1)))
      (tls-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (make-socket-address 'inet "127.0.0.1" 70000)))
      (tls-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (resolve-address "localhost" -1)))
      (tls-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (resolve-addresses "localhost" 70000)))))

(mat net-socket
     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (let ([payload (socket-recv client 32)])
                        (socket-send-all client payload))))])
       (let ([client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([local (socket-local-address client)]
               [peer (socket-peer-address client)]
               [payload (string->utf8 "ping")])
           (and (socket-address? local)
                (socket-address? peer)
                (socket-send-all client payload)
                (equal? (socket-recv client 32) payload)
                (begin
                  (close-socket client)
                  (thread-join th)
                  #t)))))
     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (let ([payload (socket-recv client 32)])
                        (socket-send-all client payload))))])
       (let ([client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (call-with-socket-ports
          client
          (lambda (ip op)
            (put-bytevector op (string->utf8 "port"))
            (flush-output-port op)
            (equal? (get-bytevector-n ip 4) (string->utf8 "port"))))
         (close-socket client)
         (thread-join th)
         #t))
     (let ([server (open-socket 'inet 'stream)])
       (socket-set-option! server 'reuse-address #t)
       (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
       (socket-listen! server 4)
       (let ([no-client (socket-accept/nonblocking server)]
             [reuse? (socket-get-option server 'reuse-address)])
         (close-socket server)
         (and (net-would-block? no-client)
              (equal? '(read) (net-would-block-events no-client))
              reuse?))))

(mat net-socket-readiness
     (let ([listener (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! listener 1)
           (let ([answer (socket-accept/nonblocking listener)])
             (and (net-would-block? answer)
                  (eq? listener (net-would-block-resource answer))
                  (equal? '(read) (net-would-block-events answer)))))
         (lambda () (close-socket listener))))

     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (milisleep 100)))])
       (let ([client (open-socket 'inet 'stream)])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (let ([answer (socket-recv/nonblocking client 1)]
                   [buffer (make-bytevector 1)])
               (and (net-would-block? answer)
                    (eq? client (net-would-block-resource answer))
                    (equal? '(read) (net-would-block-events answer))
                    (let ([into-answer (socket-recv!/nonblocking client buffer)])
                      (and (net-would-block? into-answer)
                           (eq? client (net-would-block-resource into-answer))
                           (equal? '(read) (net-would-block-events into-answer)))))))
           (lambda ()
             (close-socket client)
             (thread-join th)))))

     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (milisleep 500)))])
       (let ([client (open-socket 'inet 'stream)]
             [payload (make-bytevector 65536 0)])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (socket-set-option! client 'send-buffer 4096)
             (let loop ([attempts 10000])
               (if (fx= attempts 0)
                   #f
                   (let ([answer (socket-send/nonblocking client payload)])
                     (if (net-would-block? answer)
                         (and (eq? client (net-would-block-resource answer))
                              (equal? '(write) (net-would-block-events answer)))
                         (loop (fx1- attempts)))))))
           (lambda ()
             (close-socket client)
             (thread-join th)))))

     (let ([listener (open-socket 'inet 'stream)]
           [client (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! listener 1)
           (let* ([port (socket-address-port (socket-local-address listener))]
                  [operation
                   (socket-connect/nonblocking
                    client (make-socket-address 'inet "127.0.0.1" port) 1000)])
             (net-operation-step! operation)
             (and (net-operation? operation)
                  (case (net-operation-state operation)
                    [(completed) #t]
                    [(pending)
                     (let ([target (car (net-operation-poll-targets operation))])
                       (and (equal? '(write) (poll-target-events target))
                            (memq 'write
                                  (poll-target-ready-events
                                   (car (poll (list target) 1000))))
                            (begin (net-operation-step! operation) #t)))]
                    [else #f])
                  (eq? 'completed (net-operation-state operation))
                  (eq? #t (net-operation-result operation))
                  (socket-address? (socket-peer-address client)))))
         (lambda ()
           (close-socket client)
           (close-socket listener))))

     (let ([client (open-socket 'inet 'stream)])
       (let ([operation
              (socket-connect/nonblocking
               client (make-socket-address 'inet "127.0.0.1" 9) 100)])
         (and (net-operation? operation)
              (eq? 'pending (net-operation-state operation))
              (begin (net-operation-cancel! operation) #t)
              (eq? 'cancelled (net-operation-state operation))
              (socket-closed? client))))

     ;; Error case: SO_ERROR reports a refused nonblocking connection as a failed operation.
     (let ([listener (open-socket 'inet 'stream)])
       (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
       (let ([port (socket-address-port (socket-local-address listener))])
         (close-socket listener)
         (let ([client (open-socket 'inet 'stream)])
           (dynamic-wind
             void
             (lambda ()
               (let ([operation
                      (socket-connect/nonblocking
                       client (make-socket-address 'inet "127.0.0.1" port) 1000)])
                 (net-operation-step! operation)
                 (case (net-operation-state operation)
                   [(pending)
                    (let* ([target (car (net-operation-poll-targets operation))]
                           [ready-events
                            (poll-target-ready-events
                             (car (poll (list target) 1000)))])
                      (net-operation-step! operation)
                      (and (memq 'write ready-events)
                           (or (memq 'error ready-events) (memq 'hup ready-events))
                           (eq? 'failed (net-operation-state operation))
                           (net-error? (net-operation-condition operation))))]
                   [(failed) (net-error? (net-operation-condition operation))]
                   [else #f])))
             (lambda ()
               (unless (socket-closed? client)
                 (close-socket client))))))))

(mat net-socket-listen-validation
     (let ([sock (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (tls-error-message-contains?
            "backlog must be non-negative"
            (lambda ()
              (socket-listen! sock -1))))
         (lambda ()
           (close-socket sock)))))

(mat net-poll
     (let ([server (open-socket 'inet 'stream)])
       (socket-set-option! server 'reuse-address #t)
       (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
       (socket-listen! server 2)
       (let* ([target (make-poll-target server '(read))]
              [before (poll/nonblocking (list target))]
              [port (socket-address-port (socket-local-address server))]
              [client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([after (poll (list target) 100)])
           (close-socket client)
           (close-socket server)
           (and (null? (poll-target-ready-events (car before)))
                (not (not (memq 'read (poll-target-ready-events (car after))))))))))

(mat net-poll-allows-collection
     ;; A waiting foreign thread must not prevent collection in the peer that supplies readiness.
     (let* ([receiver (open-socket 'inet 'datagram)]
            [sender (open-socket 'inet 'datagram)]
            [worker #f]
            [failure #f])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! receiver (make-socket-address 'inet "127.0.0.1" 0))
           (let ([address (socket-local-address receiver)])
             (set! worker
               (fork-thread
                (lambda ()
                  (guard (condition [else (set! failure condition)])
                    (milisleep 50)
                    (collect)
                    (socket-send-to sender #vu8(1) address)))))
             (let ([ready (poll (list (make-poll-target receiver '(read))) 1000)])
               (thread-join worker)
               (and (not failure)
                    (not (not (memq 'read (poll-target-ready-events (car ready)))))))))
         (lambda ()
           (when worker (thread-join worker))
           (close-socket receiver)
           (close-socket sender)))))

(mat net-poll-resources
     (let ([listener (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (let ([descriptor-target (make-poll-target (socket-fd listener) '(read))]
                 [port (open-socket-input-port listener)])
             (dynamic-wind
               void
               (lambda ()
                 (let ([port-target (make-poll-target port '(read))])
                   (and (fx= (socket-fd listener) (poll-target-fd descriptor-target))
                        (eq? port (poll-target-resource port-target))
                        (fx= (port-file-descriptor port) (poll-target-fd port-target)))))
               (lambda () (close-port port)))))
         (lambda () (close-socket listener))))

     (let* ([started-ms (current-monotonic-ms)]
            [deadline-ms (+ started-ms 30)])
       (and (null? (poll-until '() deadline-ms))
            (let ([elapsed-ms (- (current-monotonic-ms) started-ms)])
              (and (>= elapsed-ms 20) (< elapsed-ms 2000)))))

     (let ([socket (open-socket 'inet 'stream)])
       (let ([descriptor (socket-fd socket)])
         (close-socket socket)
         (let ([answer (poll/nonblocking
                        (list (make-poll-target descriptor '(read write))))])
           (not (not (memq 'invalid (poll-target-ready-events (car answer))))))))

     ;; Error case: a transparent record with socket-shaped fields is not a socket resource.
     (let* ([record-type
             (make-record-type-descriptor
              'socket #f #f #f #f '#((immutable fd)))]
            [constructor
             (record-constructor
              (make-record-constructor-descriptor record-type #f #f))]
            [lookalike (constructor 0)])
       (guard (failure [else #t])
         (make-poll-target lookalike '(read))
         #f))
     )

(mat net-poll-validation
     (and
      (poll-error-message-contains?
       "poll timeout must be -1 or non-negative"
       (lambda ()
         (poll '() -2)))
      (poll-error-message-contains?
       "poll events must be a list"
       (lambda ()
         (make-poll-target 0 'read)))
      (poll-error-message-contains?
       "invalid poll event"
       (lambda ()
         (make-poll-target 0 '(bogus))))))

(mat net-socket-validation
     (let ([sock (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (and
            (tls-error-message-contains?
             "size must be non-negative"
             (lambda ()
               (socket-recv sock -1)))
            (tls-error-message-contains?
             "size must be non-negative"
             (lambda ()
               (socket-recv/nonblocking sock -1)))))
         (lambda ()
           (close-socket sock)))))

(mat net-tls
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
           (tls-context-set-verify! ctx #t)
           (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
           (let ([session (tls-connect ctx client "localhost")]
                 [payload (string->utf8 "tls")])
             (and (tls-session? session)
                  (tls-verified? session)
                  (certificate? (tls-peer-certificate session))
                  (list? (tls-peer-certificate-chain session))
                  (string? (tls-protocol-version session))
                  (not (tls-negotiated-alpn session))
                  (string? (tls-cipher-name session))
                  (tls-write-all session payload)
                  (equal? (tls-read session 32) payload)
                  (begin
                    (close-tls-session session)
                    (close-tls-context ctx)
                    (close-socket client)
                    (thread-join th)
                    #t))))))
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
           (tls-context-set-verify! ctx #t)
           (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
           (let ([session (tls-connect ctx client "localhost")])
             (and
              (call-with-tls-ports
               session
               (lambda (ip op)
                 (put-bytevector op (string->utf8 "port"))
                 (flush-output-port op)
                 (equal? (get-bytevector-n ip 4) (string->utf8 "port"))))
              (begin
                (close-tls-session session)
                (close-tls-context ctx)
                (close-socket client)
                (thread-join th)
                #t))))))
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
           (tls-context-set-verify! ctx #t)
           (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
           (let ([session (tls-connect ctx client "localhost")]
                 [buf (make-bytevector 8 0)])
             (socket-set-blocking! client #f)
             (let ([idle (tls-read/nonblocking session 8)]
                   [write-ready (poll (list (make-poll-target client '(write))) 100)])
               (let ([sent (tls-write/nonblocking session (string->utf8 "nb"))]
                     [read-target (make-poll-target client '(read))])
                 (let loop ([attempt 8])
                   (let ([n (tls-read!/nonblocking session buf 0 2)])
                     (if (net-would-block? n)
                         (if (fx= attempt 0)
                             (begin
                               (close-tls-session session)
                               (close-tls-context ctx)
                               (close-socket client)
                               (thread-join th)
                               #f)
                             (begin
                               (poll (list read-target) 100)
                               (loop (fx1- attempt))))
                         (begin
                           (close-tls-session session)
                           (close-tls-context ctx)
                           (close-socket client)
                           (thread-join th)
                           (and (net-would-block? idle)
                                (equal? '(read) (net-would-block-events idle))
                                (eq? client (net-would-block-resource idle))
                                (memq 'write (poll-target-ready-events (car write-ready)))
                                sent
                                (= n 2)
                                (equal? (slice-bytevector buf 0 2) (string->utf8 "nb"))))))))))))))

(mat net-tls-handshake-readiness
     (mat-requires (openssl)
       (let-values ([(listener port th)
                     (start-stalled-tls-handshake-server 200)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)]
               [operation #f])
           (dynamic-wind
             void
             (lambda ()
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (set! operation (tls-connect/nonblocking ctx client #f 100))
               (net-operation-step! operation)
               (and (eq? 'pending (net-operation-state operation))
                    (let ([target* (net-operation-poll-targets operation)])
                      (and (= (length target*) 1)
                           (eq? client (poll-target-resource (car target*)))
                           (not (not
                                 (memq 'read
                                       (poll-target-events (car target*)))))))
                    (begin (net-operation-cancel! operation) #t)
                    (eq? 'cancelled (net-operation-state operation))))
             (lambda ()
               (when (and operation
                          (eq? 'pending (net-operation-state operation)))
                 (net-operation-cancel! operation))
               (close-tls-context ctx)
               (guard (c [else #f])
                 (close-socket client))
               (thread-join th)
               (guard (c [else #f])
                 (close-socket listener))))))))

(mat net-tls-write-all-readiness
     (mat-requires (openssl)
       (let-values ([(listener server-ctx port th release-server)
                     (start-stalled-tls-reader 10)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)]
               [session #f]
               [payload (make-bytevector 65536 65)])
           (dynamic-wind
             void
             (lambda ()
               (socket-set-option! client 'send-buffer 1024)
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (set! session (tls-connect ctx client "localhost"))
               (let loop ([attempt 500])
                 (when (fx= attempt 0)
                   (error 'net-tls-write-all-readiness
                          "TLS writes did not reach backpressure"))
                 (let ([answer (tls-write/nonblocking session payload)])
                   (if (net-would-block? answer)
                       (let ([all-answer
                              (tls-write-all/nonblocking session payload)])
                         (and (net-would-block? all-answer)
                              (eq? client
                                   (net-would-block-resource all-answer))
                              (not (not
                                    (memq
                                     'write
                                     (net-would-block-events all-answer))))))
                       (loop (fx1- attempt))))))
             (lambda ()
               (release-server)
               (when session (close-tls-session session))
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-timeout
     (mat-requires (openssl)
       (let-values ([(listener port th)
                     (start-stalled-tls-handshake-server 200)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (dynamic-wind
             void
             (lambda ()
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (tls-net-error-timeout?
                (lambda ()
                  (call-with-tls-client ctx client 50 tls-session?))))
             (lambda ()
               (close-tls-context ctx)
               (guard (c [else #f])
                 (close-socket client))
               (thread-join th)
               (guard (c [else #f])
                 (close-socket listener)))))))
     (mat-requires (openssl)
       (let ([listener (open-socket 'inet 'stream)]
             [server-ctx (make-tls-context 'server)])
         (write-test-cert-files)
         (tls-context-load-cert! server-ctx "/tmp/cspp-net-test-cert.pem")
         (tls-context-load-private-key! server-ctx "/tmp/cspp-net-test-key.pem")
         (socket-set-option! listener 'reuse-address #t)
         (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
         (socket-listen! listener 4)
         (let ([port (socket-address-port (socket-local-address listener))]
               [client (open-socket 'inet 'stream)])
           (dynamic-wind
             void
             (lambda ()
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let-values ([(accepted peer) (socket-accept listener)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (tls-net-error-timeout?
                      (lambda ()
                        (call-with-tls-server server-ctx accepted 50 tls-session?))))
                   (lambda ()
                     (guard (c [else #f])
                       (close-socket accepted))))))
             (lambda ()
               (close-tls-context server-ctx)
               (guard (c [else #f])
                 (close-socket client))
               (guard (c [else #f])
                 (close-socket listener))))))))

(mat net-tls-timeout-validation
     (mat-requires (openssl)
       (let ([client-ctx (make-tls-context 'client)]
             [server-ctx (make-tls-context 'server)]
             [sock (open-socket 'inet 'stream)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (tls-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (tls-connect client-ctx sock #f -1)))
              (tls-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (tls-accept server-ctx sock -1)))
              (tls-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (call-with-tls-client client-ctx sock -1 tls-session?)))
              (tls-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (call-with-tls-server server-ctx sock -1 tls-session?)))))
           (lambda ()
             (close-tls-context client-ctx)
             (close-tls-context server-ctx)
             (close-socket sock)))))
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)]
               [payload (string->utf8 "x")]
               [buf (make-bytevector 1 0)])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let ([session (tls-connect ctx client "localhost")])
                 (dynamic-wind
                   void
                   (lambda ()
                     (and
                      (tls-error-message-contains?
                       "timeout must be non-negative"
                       (lambda ()
                         (tls-read session 1 -1)))
                      (tls-error-message-contains?
                       "timeout must be non-negative"
                       (lambda ()
                         (tls-read! session buf 0 1 -1)))
                      (tls-error-message-contains?
                       "timeout must be non-negative"
                       (lambda ()
                         (tls-write session payload 0 1 -1)))
                      (tls-error-message-contains?
                       "timeout must be non-negative"
                       (lambda ()
                         (tls-write-all session payload 0 1 -1)))))
                   (lambda ()
                     (close-tls-session session)))))
             (lambda ()
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-policy
     (mat-requires (openssl)
       (let ([ctx (make-tls-context 'client)]
             [policy (make-tls-policy 'tls1.2 'tls1.3
                                      "ECDHE+AESGCM" #f
                                      '("h2" "http/1.1") 'disabled)])
         (dynamic-wind
           void
           (lambda ()
             (and (eq? (tls-context-policy-set! ctx policy) policy)
                  (eq? (tls-policy-minimum-version policy) 'tls1.2)
                  (eq? (tls-policy-maximum-version policy) 'tls1.3)
                  (equal? (tls-policy-alpn-protocols policy) '("h2" "http/1.1"))
                  (and (assq 'session-serialization (tls-capabilities)) #t)))
           (lambda () (close-tls-context ctx))))))

(mat net-tls-chain-records
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let ([session (tls-connect ctx client "localhost")])
                 (dynamic-wind
                   void
                   (lambda ()
                     (let ([record* (tls-peer-certificate-chain-records session)])
                       (tls-write-all session (string->utf8 "chain"))
                       (and (equal? (tls-read session 32) (string->utf8 "chain"))
                            (pair? record*)
                            (tls-certificate? (car record*))
                            (zero? (tls-certificate-chain-position (car record*)))
                            (tls-certificate-verified? (car record*))
                            (bytevector? (tls-certificate-der (car record*)))
                            (string? (tls-certificate-subject (car record*)))
                            (pair? (tls-certificate-public-key-summary (car record*))))))
                   (lambda () (close-tls-session session)))))
             (lambda ()
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-required-ocsp
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (tls-context-policy-set!
                ctx (make-tls-policy 'tls1.2 #f #f #f '() 'required))
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               ;; A required OCSP policy rejects a server that sends no staple.
               (tls-error-message-contains?
                "required stapled OCSP response is missing"
                (lambda () (tls-connect ctx client "localhost" 3000))))
             (lambda ()
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-session-resumption
     (mat-requires (openssl)
       (let-values ([(server port th) (start-tls-resumption-server)])
         (let ([ctx (make-tls-context 'client)]
               [ticket #f]
               [reused? #f])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (do ([i 0 (fx1+ i)])
                   ((fx= i 2))
                 (let ([client (open-socket 'inet 'stream)])
                   (dynamic-wind
                     void
                     (lambda ()
                       (socket-connect!
                        client (make-socket-address 'inet "127.0.0.1" port))
                       (let ([session (tls-connect ctx client "localhost" 3000)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (tls-write-all session (string->utf8 "resume"))
                             (unless (equal? (tls-read session 32 3000)
                                             (string->utf8 "resume"))
                               (error 'net-tls-session-resumption "echo mismatch"))
                             (if (fx= i 0)
                                 (begin
                                   (set! ticket (tls-session-export-ticket session))
                                   (tls-context-session-ticket-set! ctx ticket))
                                 (set! reused? (tls-session-reused? session))))
                           (lambda () (close-tls-session session)))))
                     (lambda () (close-socket client)))))
               (and (tls-session-ticket? ticket)
                    (positive? (bytevector-length (tls-session-ticket-data ticket)))
                    reused?))
             (lambda ()
               (close-tls-context ctx)
               (thread-join th)))))))

(mat net-tls-sni-selection
     (mat-requires (openssl)
       (let-values ([(server port th selected-name) (start-tls-sni-echo-server)])
         (let ([ctx (make-tls-context 'client)]
               [client (open-socket 'inet 'stream)]
               [expected (load-certificate tls-test-san-certificate 'pem)]
               [peer-cert #f])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-san-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let ([session (tls-connect ctx client "localhost" 3000)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (set! peer-cert (tls-peer-certificate session))
                     (tls-write-all session (string->utf8 "sni"))
                     (and (equal? (tls-read session 32 3000) (string->utf8 "sni"))
                          (string=? (selected-name) "localhost")
                          (equal? (certificate-fingerprint peer-cert)
                                  (certificate-fingerprint expected))))
                   (lambda () (close-tls-session session)))))
             (lambda ()
               (when peer-cert (destroy-certificate! peer-cert))
               (destroy-certificate! expected)
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-size-validation
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let ([session (tls-connect ctx client "localhost")])
                 (dynamic-wind
                   void
                   (lambda ()
                     (and
                      (tls-error-message-contains?
                       "size must be non-negative"
                       (lambda ()
                         (tls-read session -1)))
                      (tls-error-message-contains?
                       "size must be non-negative"
                       (lambda ()
                         (tls-read/nonblocking session -1)))))
                   (lambda ()
                     (close-tls-session session)))))
             (lambda ()
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))

(mat net-tls-port-closed-session
     (mat-requires (openssl)
       (let-values ([(server server-ctx port th) (start-tls-echo-server)])
         (let ([client (open-socket 'inet 'stream)]
               [ctx (make-tls-context 'client)])
           (dynamic-wind
             void
             (lambda ()
               (tls-context-load-ca-file! ctx "/tmp/cspp-net-test-cert.pem")
               (tls-context-set-verify! ctx #t)
               (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
               (let ([session (tls-connect ctx client "localhost")])
                 (dynamic-wind
                   void
                   (lambda ()
                     (let ([ip (open-tls-input-port session)]
                           [op (open-tls-output-port session)]
                           [bp (open-tls-port session)])
                       (dynamic-wind
                         void
                         (lambda ()
                           (and
                            (begin
                              (close-tls-session session)
                              #t)
                            (tls-error-message-contains?
                             "TLS session is closed"
                             (lambda ()
                               (get-bytevector-n ip 1)))
                            (tls-error-message-contains?
                             "TLS session is closed"
                             (lambda ()
                               (put-bytevector op (string->utf8 "x"))
                               (flush-output-port op)))
                            (tls-error-message-contains?
                             "TLS session is closed"
                             (lambda ()
                               (put-bytevector bp (string->utf8 "y"))
                               (flush-output-port bp)))))
                         (lambda ()
                           (unless (port-closed? ip)
                             (guard (c [else #f])
                               (close-port ip)))
                           (unless (port-closed? op)
                             (guard (c [else #f])
                               (close-port op)))
                           (unless (port-closed? bp)
                             (guard (c [else #f])
                               (close-port bp)))))))
                   (lambda ()
                     (guard (c [else #f])
                       (close-tls-session session))))))
             (lambda ()
               (close-tls-context ctx)
               (close-socket client)
               (thread-join th)))))))
