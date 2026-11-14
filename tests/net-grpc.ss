(import (cspp)
        (cspp path)
        (cspp format protobuf)
        (cspp net))

(load "net-common.ss")

(define with-env
  (lambda (name value proc)
    (let ([old (getenv name)])
      (dynamic-wind
        (lambda () (putenv name value))
        proc
        (lambda () (putenv name (or old "")))))))

(define with-env*
  (lambda (bindings proc)
    (if (null? bindings)
        (proc)
        (with-env (caar bindings)
                  (cdar bindings)
                  (lambda ()
                    (with-env* (cdr bindings) proc))))))

(define with-grpc-env
  (lambda (proc)
    (with-env* '(("http_proxy" . "")
                 ("https_proxy" . "")
                 ("all_proxy" . "")
                 ("HTTP_PROXY" . "")
                 ("HTTPS_PROXY" . "")
                 ("ALL_PROXY" . "")
                 ("no_proxy" . "127.0.0.1,localhost")
                 ("NO_PROXY" . "127.0.0.1,localhost"))
               proc)))

(define call-with-grpc-peer
  (lambda (serve-count register proc)
    (let* ([port (reserve-loopback-port)]
           [server (grpc-open-channel 'server "127.0.0.1" port)]
           [client (grpc-open-channel "127.0.0.1" port)]
           [done? #f])
      (register server)
      (let ([th (fork-thread
                 (lambda ()
                   (if serve-count
                       (let loop ([n serve-count])
                         (unless (= n 0)
                           (grpc-serve server)
                           (loop (- n 1))))
                       (let loop ()
                         (unless done?
                           (guard (c [else #f])
                             (grpc-serve server))
                           (loop))))))])
        (dynamic-wind
          void
          (lambda () (proc server client))
          (lambda ()
            (if serve-count
                (begin
                  (thread-join th)
                  (grpc-close-channel client)
                  (grpc-close-channel server))
                (begin
                  (set! done? #t)
                  (grpc-close-channel client)
                  (grpc-close-channel server)
                (thread-join th)))))))))

(define read-test-pem
  (lambda (path)
    (call-with-port
     (open-file-input-port path)
     (lambda (port)
       (utf8->string (get-bytevector-all port))))))

(define grpc-reflection-descriptor*
  (lambda (response)
    (let ([outer (make-protobuf-decoder response)] [file-response #f])
      (let outer-loop ()
        (let ([field (protobuf-decoder-next-field outer)])
          (when field
            (when (= (protobuf-wire-field-number field) 4)
              (set! file-response (protobuf-wire-field-value field)))
            (outer-loop))))
      (if (not file-response)
          '()
          (let ([inner (make-protobuf-decoder file-response)] [descriptor* '()])
            (let inner-loop ()
              (let ([field (protobuf-decoder-next-field inner)])
                (if (not field)
                    (reverse descriptor*)
                    (begin
                      (when (= (protobuf-wire-field-number field) 1)
                        (set! descriptor*
                              (cons (protobuf-wire-field-value field) descriptor*)))
                      (inner-loop))))))))))

(define call-with-grpc-tls-peer*
  (lambda (mutual? register proc)
    (write-test-san-cert-files)
    (let* ([port (reserve-loopback-port)]
           [certificate (read-test-pem "/tmp/cspp-net-test-san-cert.pem")]
           [private-key (read-test-pem "/tmp/cspp-net-test-san-key.pem")]
           [server-credentials
            (make-grpc-server-credentials
             (and mutual? certificate) certificate private-key)]
           [client-credentials
            (make-grpc-channel-credentials
             certificate
             (and mutual? certificate)
             (and mutual? private-key))]
           [server (grpc-open-channel 'server server-credentials "127.0.0.1" port)]
           [client (grpc-open-channel client-credentials "localhost" port)]
           [done? #f])
      (register server)
      (let ([th
             (fork-thread
              (lambda ()
                (let loop ()
                  (unless done?
                    (guard (c [else #f])
                      (grpc-serve server))
                    (loop)))))])
        ;; Let the serving thread enter its polling loop before the first TLS call.
        ($sleep (make-time 'time-duration 1000000 0))
        (dynamic-wind
          void
          (lambda () (proc server client))
          (lambda ()
            (set! done? #t)
            (grpc-close-channel client)
            (grpc-close-channel server)
            (thread-join th)))))))

(define call-with-grpc-tls-peer
  (lambda (register proc)
    (call-with-grpc-tls-peer* #f register proc)))

(define call-with-grpc-mutual-tls-peer
  (lambda (register proc)
    (call-with-grpc-tls-peer* #t register proc)))

(define wait-grpc-call/nonblocking
  (lambda (client method payload metadata timeout-ms)
    (net-operation-wait
     (grpc-call/nonblocking client method payload metadata timeout-ms))))

(define wait-grpc-stream/nonblocking
  (lambda (proc)
    (net-operation-wait (proc))))

(define grpc-net-error-message?
  (lambda (message thunk)
    (guard (c [else
               (and (net-error? c)
                    (equal? (net-error-message c) message))])
      (thunk)
      #f)))

(define grpc-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define grpc-thread-count
  (lambda ()
    (length (directory-list "/proc/self/task"))))

(define grpc-server-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/server-stream
                   client
                   "/cspp.test.Echo/ServerStream"
                   "watch"
                   '()
                   2000)])
      (let ([ok?
             (let ([m1 (grpc-stream-recv stream)]
                   [m2 (grpc-stream-recv stream)]
                   [done (grpc-stream-recv stream)])
               (and (grpc-stream? stream)
                    (equal? (utf8->string m1) "watch:0")
                    (equal? (utf8->string m2) "watch:1")
                    (eof-object? done)))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-client-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/client-stream
                   client
                   "/cspp.test.Echo/ClientStream"
                   '()
                   2000)])
      (let ([ok?
             (begin
               (grpc-stream-send stream "3")
               (grpc-stream-send stream "5")
               (grpc-stream-send stream "8")
               (grpc-stream-close-send stream)
               (let ([reply (grpc-stream-recv stream)]
                     [done (grpc-stream-recv stream)])
                 (and (equal? (utf8->string reply) "sum:16")
                      (eof-object? done))))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-bidi-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/bidi-stream
                   client
                   "/cspp.test.Echo/Bidi"
                   '()
                   2000)])
      (let ([ok?
             (begin
               (grpc-stream-send stream "one")
               (let ([r1 (grpc-stream-recv stream)])
                 (if (equal? (utf8->string r1) "echo:one")
                     (begin
                       (grpc-stream-send stream "two")
                       (let ([r2 (grpc-stream-recv stream)])
                         (and (equal? (utf8->string r2) "echo:two")
                              (begin
                                (grpc-stream-close-send stream)
                                (eof-object? (grpc-stream-recv stream))))))
                     #f)))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-server-stream-nonblocking-ok?
  (lambda (client)
    (let ([stream (wait-grpc-stream/nonblocking
                   (lambda ()
                     (grpc-call/server-stream/nonblocking
                      client
                      "/cspp.test.Echo/ServerStream"
                      "watch"
                      '()
                      2000)))])
      (let ([ok?
             (let ([m1 (grpc-stream-recv stream)]
                   [m2 (grpc-stream-recv stream)]
                   [done (grpc-stream-recv stream)])
               (and (grpc-stream? stream)
                    (equal? (utf8->string m1) "watch:0")
                    (equal? (utf8->string m2) "watch:1")
                    (eof-object? done)))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-client-stream-nonblocking-ok?
  (lambda (client)
    (let ([stream (wait-grpc-stream/nonblocking
                   (lambda ()
                     (grpc-call/client-stream/nonblocking
                      client
                      "/cspp.test.Echo/ClientStream"
                      '()
                      2000)))])
      (let ([ok?
             (begin
               (grpc-stream-send stream "13")
               (grpc-stream-send stream "21")
               (grpc-stream-close-send stream)
               (let ([reply (grpc-stream-recv stream)]
                     [done (grpc-stream-recv stream)])
                 (and (equal? (utf8->string reply) "sum:34")
                      (eof-object? done))))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-bidi-stream-nonblocking-ok?
  (lambda (client)
    (let ([stream (wait-grpc-stream/nonblocking
                   (lambda ()
                     (grpc-call/bidi-stream/nonblocking
                      client
                      "/cspp.test.Echo/Bidi"
                      '()
                      2000)))])
      (let ([ok?
             (begin
               (grpc-stream-send stream "left")
               (let ([r1 (grpc-stream-recv stream)])
                 (if (equal? (utf8->string r1) "echo:left")
                     (begin
                       (grpc-stream-send stream "right")
                       (let ([r2 (grpc-stream-recv stream)])
                         (and (equal? (utf8->string r2) "echo:right")
                              (begin
                                (grpc-stream-close-send stream)
                                (eof-object? (grpc-stream-recv stream))))))
                     #f)))])
        (grpc-stream-close stream)
        ok?))))

(mat net-grpc-records
     (let ([options (make-grpc-call-options '(("x-test" . "value")) 2500 'gzip)])
       (and (grpc-call-options? options)
            (= (grpc-call-options-timeout-ms options) 2500)
            (eq? (grpc-call-options-compression options) 'gzip)
            (equal? (grpc-call-options-metadata options) '(("x-test" . "value")))))

     (mat-requires (grpc)
       (let ([capabilities (grpc-capabilities)])
         (and (grpc-capabilities? capabilities)
              (boolean? (grpc-capabilities-tls? capabilities))
              (boolean? (grpc-capabilities-compression? capabilities))
              (memq 'identity (grpc-capabilities-compression-algorithms capabilities))
              (grpc-capabilities-deadlines? capabilities)
              (grpc-capabilities-cancellation? capabilities)
              (grpc-capabilities-status-details? capabilities)
              (grpc-capabilities-reflection? capabilities))))

     (let ([registry (make-grpc-reflection-registry)])
       (and (grpc-reflection-registry? registry)
            (eq? registry
                 (grpc-reflection-register-file!
                  registry "file-transfer.proto" #vu8(10 0)
                  '("cspp.examples.transfer.FileChunk")
                  '("cspp.examples.transfer.FileTransfer")))))

     (let ([req (grpc-request "/cspp.test.Meta/Inspect"
                              "payload"
                              '(("x-id" . "42")
                                ("x-bin" . #vu8(1 2 3))))])
       (let ([resp (grpc-response #vu8(9 8)
                                  '(("x-answer" . "ok")
                                    ("x-bin" . #vu8(4 5)))
                                  7
                                  "status-text"
                                  #vu8(0 255 1))])
         (and
          (grpc-request? req)
          (equal? (utf8->string (grpc-request-payload req)) "payload")
          (equal? (grpc-metadata-ref req "x-id") "42")
          (equal? (grpc-metadata-ref req "x-bin") #vu8(1 2 3))
          (grpc-response? resp)
          (equal? (grpc-response-payload resp) #vu8(9 8))
          (= (grpc-status-code resp) 7)
          (equal? (grpc-status-message resp) "status-text")
          (equal? (grpc-status-details resp) #vu8(0 255 1))
          (grpc-status? (grpc-response-status resp))
          (= (grpc-status-code (grpc-response-status resp)) 7)
          (equal? (grpc-metadata-ref resp "x-answer") "ok")
          (equal? (grpc-metadata-ref resp "x-bin") #vu8(4 5)))))

     ;; Error case: caller metadata keys may not contain uppercase ASCII characters.
     (guard (condition [else #t])
       (grpc-response #f '(("X-Upper" . "bad")))
       #f)

     ;; Error case: bytevector metadata values require a binary `-bin` key.
     (guard (condition [else #t])
       (grpc-response #f '(("x-value" . #vu8(1))))
       #f)

     ;; Error case: caller metadata may not use the reserved `grpc-` prefix.
     (guard (condition [else #t])
       (grpc-response #f '(("grpc-test" . "bad")))
       #f)

     ;; Error case: unsupported compression names are rejected before a call starts.
     (guard (condition [else #t])
       (make-grpc-call-options '() 1000 'brotli)
       #f))

(mat net-grpc-reflection
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (let* ([dependency
                  (protobuf-encode-message (list (list 1 'string "dependency.proto")))]
                 [extension
                  (protobuf-encode-message
                   (list (list 1 'string "extra")
                         (list 2 'string ".cspp.test.Base")
                         (list 3 'uint32 100)))]
                 [root
                  (protobuf-encode-message
                   (list (list 1 'string "root.proto")
                         (list 2 'string "cspp.test")
                         (list 3 'string "dependency.proto")
                         (list 7 'message extension)))]
                 [registry (make-grpc-reflection-registry)])
            (grpc-reflection-register-file!
             registry "dependency.proto" dependency '("cspp.test.Base") '())
            (grpc-reflection-register-file!
             registry "root.proto" root '("cspp.test.Root") '())
            (call-with-grpc-peer
             1
             (lambda (server) (grpc-register-reflection! server registry))
             (lambda (server client)
               (let ([stream
                      (grpc-call/bidi-stream
                       client
                       "/grpc.reflection.v1.ServerReflection/ServerReflectionInfo"
                       '()
                       2000)])
                 (grpc-stream-send
                  stream
                  (protobuf-encode-message (list (list 4 'string "cspp.test.Root"))))
                 (let ([symbol-descriptor*
                        (grpc-reflection-descriptor* (grpc-stream-recv stream))])
                   (grpc-stream-send
                    stream
                    (protobuf-encode-message
                     (list
                      (list 5 'message
                            (protobuf-encode-message
                             (list (list 1 'string "cspp.test.Base")
                                   (list 2 'uint32 100)))))))
                   (let ([extension-descriptor*
                          (grpc-reflection-descriptor* (grpc-stream-recv stream))])
                     (grpc-stream-close-send stream)
                     (let ([done (grpc-stream-recv stream)])
                       (grpc-stream-close stream)
                       (and (equal? symbol-descriptor* (list root dependency))
                            (equal? extension-descriptor* (list root dependency))
                            (eof-object? done)))))))))))))

(mat net-grpc-unary
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (let ([seen '()])
            (call-with-grpc-peer
             4
             (lambda (server)
               (grpc-register-service!
                server
                "/cspp.test.Echo/Unary"
                (lambda (req)
                  (set! seen (cons (grpc-metadata-ref req "x-mode" #f) seen))
                  (grpc-response
                   (grpc-request-payload req)
                   '(("x-reply" . "ok"))
                   0
                   ""))))
             (lambda (server client)
               (and
               (grpc-channel? server)
               (grpc-channel? client)
                (let ([resp (grpc-call client
                                       "/cspp.test.Echo/Unary"
                                       "hello grpc"
                                       '(("x-mode" . "blocking"))
                                       2000)])
                  (and
                   (grpc-response? resp)
                   (= (grpc-status-code resp) 0)
                   (equal? (grpc-status-message resp) "")
                   (equal? (utf8->string (grpc-response-payload resp)) "hello grpc")
                   (equal? (grpc-metadata-ref resp "x-reply") "ok")))
                (let ([resp (wait-grpc-call/nonblocking
                             client
                             "/cspp.test.Echo/Unary"
                             #vu8(1 2 3 4)
                             '(("x-mode" . "nonblocking"))
                             2000)])
                  (and
                   (grpc-response? resp)
                   (= (grpc-status-code resp) 0)
                   (equal? (grpc-response-payload resp) #vu8(1 2 3 4))
                   (equal? (grpc-metadata-ref resp "x-reply") "ok")))
                (let ([resp
                       (grpc-call
                        client "/cspp.test.Echo/Unary" "compressed"
                        (make-grpc-call-options '(("x-mode" . "gzip")) 2000 'gzip))])
                  (and
                   (equal? (utf8->string (grpc-response-payload resp)) "compressed")
                   (equal? (grpc-metadata-ref resp "grpc-encoding") "gzip")))
                (let ([resp (grpc-call client
                                       "/cspp.test.Echo/Missing"
                                       #f
                                       '()
                                       2000)])
                  (and
                   (grpc-response? resp)
                   (= (grpc-status-code resp) 12)
                   (equal? (grpc-status-message resp) "unimplemented")
                   (not (grpc-response-payload resp))))
                (equal? (reverse seen) '("blocking" "nonblocking" "gzip"))))))))))

(mat net-grpc-streams
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-peer
           6
           (lambda (server)
             (grpc-register-service!
              server
              "/cspp.test.Echo/ServerStream"
              'server
              (lambda (stream)
                (let ([payload (grpc-stream-recv stream)])
                  (when (bytevector? payload)
                    (let ([text (utf8->string payload)])
                      (grpc-stream-send stream (string-append text ":0"))
                      (grpc-stream-send stream (string-append text ":1"))))
                  #f)))
             (grpc-register-service!
              server
              "/cspp.test.Echo/ClientStream"
              'client
              (lambda (stream)
                (let loop ([parts '()])
                  (let ([payload (grpc-stream-recv stream)])
                    (if (eof-object? payload)
                        (string-append "sum:"
                                       (number->string
                                        (apply +
                                               (map string->number
                                                    (reverse parts)))))
                        (loop (cons (utf8->string payload) parts)))))))
             (grpc-register-service!
              server
              "/cspp.test.Echo/Bidi"
              'bidi
              (lambda (stream)
                (let loop ()
                  (let ([payload (grpc-stream-recv stream)])
                    (unless (eof-object? payload)
                      (grpc-stream-send
                       stream
                       (string-append "echo:" (utf8->string payload)))
                      (loop))))
                #f)))
           (lambda (server client)
             (and
             (grpc-channel? server)
             (grpc-channel? client)
              (grpc-server-stream-ok? client)
              (grpc-client-stream-ok? client)
              (grpc-bidi-stream-ok? client)
              (grpc-server-stream-nonblocking-ok? client)
              (grpc-client-stream-nonblocking-ok? client)
              (grpc-bidi-stream-nonblocking-ok? client))))))))

(mat net-grpc-errors
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-peer
           #f
           (lambda (server)
             (grpc-register-service!
              server
              "/cspp.test.Echo/UnarySlow"
              (lambda (req)
                (when (equal? (utf8->string (grpc-request-payload req)) "slow")
                  (milisleep 100))
                (grpc-response (grpc-request-payload req) '() 0 "")))
             (grpc-register-service!
              server
              "/cspp.test.Echo/ServerStream"
              'server
              (lambda (stream)
                (let ([payload (grpc-stream-recv stream)])
                  (when (bytevector? payload)
                    (grpc-stream-send
                     stream
                     (string-append (utf8->string payload) ":0")))
                  #f)))
             (grpc-register-service!
              server
              "/cspp.test.Echo/ClientStream"
              'client
              (lambda (stream)
                (let loop ([total 0])
                  (let ([payload (grpc-stream-recv stream)])
                    (if (eof-object? payload)
                        (string-append "sum:" (number->string total))
                        (loop (+ total (string->number (utf8->string payload))))))))))
           (lambda (server client)
             (and
              (grpc-channel? server)
              (grpc-channel? client)
              (eq? (grpc-cancel-pending! client) client)
              (net-operation? (grpc-call/nonblocking
                    client
                    "/cspp.test.Echo/UnarySlow"
                    "slow"
                    '()
                    500))
              (eq? (grpc-cancel-pending! client) client)
              (let ([resp (grpc-call
                           client
                           "/cspp.test.Echo/UnarySlow"
                           "fast"
                           '()
                           500)])
                (and (grpc-response? resp)
                     (equal? (utf8->string (grpc-response-payload resp)) "fast")))
              (let ([slow (grpc-call/nonblocking
                           client "/cspp.test.Echo/UnarySlow" "slow" '() 500)]
                    [fast (grpc-call/nonblocking
                           client "/cspp.test.Echo/UnarySlow" "fast" '() 500)])
                (and (net-operation? slow)
                     (net-operation? fast)
                     (let ([slow-response (net-operation-wait slow)]
                           [fast-response (net-operation-wait fast)])
                       (and (equal? (utf8->string
                                     (grpc-response-payload slow-response)) "slow")
                            (equal? (utf8->string
                                     (grpc-response-payload fast-response)) "fast")))))
              (let ([stream (grpc-call/client-stream
                             client
                             "/cspp.test.Echo/ClientStream"
                             '()
                             5000)])
                (let ([ok-stream?
                       (and
                        (grpc-stream? stream)
                        (eq? (grpc-stream-close-send stream) stream)
                        (grpc-net-error-message?
                         "gRPC stream send side is closed"
                         (lambda ()
                           (grpc-stream-send stream "1")))
                        (equal? (utf8->string (grpc-stream-recv stream)) "sum:0")
                        (eof-object? (grpc-stream-recv stream))
                        (eq? (grpc-stream-close stream) stream)
                        (eq? (grpc-stream-close stream) stream)
                        (grpc-net-error-message?
                         "gRPC stream is closed"
                         (lambda ()
                           (grpc-stream-recv stream))))])
                  ok-stream?))
              (let ([stream (grpc-call/server-stream
                             client
                             "/cspp.test.Echo/ServerStream"
                             "watch"
                             '()
                             5000)])
                (let ([ok-stream?
                       (and
                        (grpc-net-error-message?
                         "gRPC stream does not support sending"
                         (lambda ()
                           (grpc-stream-send stream "bad")))
                        (equal? (utf8->string (grpc-stream-recv stream)) "watch:0")
                        (eof-object? (grpc-stream-recv stream)))])
                  (grpc-stream-close stream)
                  ok-stream?)))))))))

(mat net-grpc-stream-cancel
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (let ([port (reserve-loopback-port)])
            (let ([client (grpc-open-channel "127.0.0.1" port)])
              (dynamic-wind
                void
                (lambda ()
                  (and
                   (net-operation? (grpc-call/server-stream/nonblocking
                         client
                         "/cspp.test.Echo/ServerStream"
                         "x"
                         '()
                         100))
                   (eq? (grpc-cancel-pending! client) client)
                   (net-operation? (grpc-call/server-stream/nonblocking
                                    client
                                    "/cspp.test.Echo/ServerStream"
                                    "x" '() 100))
                   (net-operation? (grpc-call/bidi-stream/nonblocking
                                    client "/cspp.test.Echo/Bidi" '() 100))
                   (eq? (grpc-cancel-pending! client) client)))
                (lambda ()
                  (guard (c [else #f])
                    (grpc-close-channel client)))))
            (let ([client (grpc-open-channel "127.0.0.1" port)])
              (dynamic-wind
                void
                (lambda ()
                  (and
                   (net-operation? (grpc-call/server-stream/nonblocking
                         client
                         "/cspp.test.Echo/ServerStream"
                         "x"
                         '()
                         100))
                   (eq? (grpc-close-channel client) client)
                   (grpc-net-error-message?
                    "gRPC channel is closed"
                    (lambda ()
                      (grpc-call client
                                 "/cspp.test.Echo/Unary"
                                 "x"
                                 '()
                                 100)))))
                (lambda ()
                  (guard (c [else #f])
                    (grpc-close-channel client))))))))))

(mat net-grpc-shared-completion-driver
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-peer
             100
             (lambda (server)
               (grpc-register-service!
                server
                "/cspp.test.Echo/UnaryMany"
                (lambda (request)
                  (grpc-response (grpc-request-payload request)))))
             (lambda (server client)
               (let ([before (grpc-thread-count)]
                     [operation*
                      (let loop ([i 0] [out '()])
                        (if (= i 100)
                            (reverse out)
                            (loop
                             (+ i 1)
                             (cons
                              (grpc-call/nonblocking
                               client
                               "/cspp.test.Echo/UnaryMany"
                               (number->string i)
                               '()
                               5000)
                              out))))])
                 (for-each net-operation-step! operation*)
                 (let* ([fd*
                         (map
                          (lambda (operation)
                            (poll-target-fd
                             (car (net-operation-poll-targets operation))))
                          (filter
                           (lambda (operation)
                             (eq? 'pending (net-operation-state operation)))
                           operation*))]
                        [response* (map net-operation-wait operation*)])
                   (and (pair? fd*)
                        (andmap (lambda (fd) (= fd (car fd*))) (cdr fd*))
                        (= (length response*) 100)
                        (<= (- (grpc-thread-count) before) 1))))))))))

(mat net-grpc-timeout-validation
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-peer
           0
           (lambda (server) #t)
           (lambda (server client)
             (and
              (grpc-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (grpc-call client "/cspp.test.Echo/Unary" "x" '() -1)))
              (grpc-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (grpc-call/nonblocking client "/cspp.test.Echo/Unary" "x" '() -1)))
              (grpc-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (grpc-call/server-stream client "/cspp.test.Echo/ServerStream" "x" '() -1)))
              (grpc-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (grpc-call/client-stream client "/cspp.test.Echo/ClientStream" '() -1)))
              (grpc-error-message-contains?
               "timeout must be non-negative"
               (lambda ()
                 (grpc-call/bidi-stream/nonblocking client "/cspp.test.Echo/Bidi" '() -1))))))))))

(mat net-grpc-port-validation
     (mat-requires (grpc)
       (and
        (grpc-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (grpc-open-channel "127.0.0.1" -1)))
        (grpc-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (grpc-open-channel 'server "127.0.0.1" 70000))))))

(mat net-grpc-tls
     ;; Trusted TLS credentials must protect a unary gRPC call and verify localhost.
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-tls-peer
           (lambda (server)
             (grpc-register-service!
              server
              "/cspp.test.Echo/Unary"
              (lambda (request)
                (grpc-response (grpc-request-payload request)))))
           (lambda (server client)
             (let ([response (grpc-call client "/cspp.test.Echo/Unary" "tls-ok" '() 3000)])
               (and (grpc-response? response)
                    (= (grpc-status-code response) 0)
                    (equal? (utf8->string (grpc-response-payload response)) "tls-ok"))))))))

     ;; A server configured with trust roots requires and verifies a client certificate.
     (mat-requires (grpc)
       (with-grpc-env
        (lambda ()
          (call-with-grpc-mutual-tls-peer
           (lambda (server)
             (grpc-register-service!
              server
              "/cspp.test.Echo/MutualTLS"
              (lambda (request)
                (grpc-response (grpc-request-payload request)))))
           (lambda (server client)
             (let ([response
                    (grpc-call client "/cspp.test.Echo/MutualTLS" "mtls-ok" '() 3000)])
               (and (grpc-response? response)
                    (= (grpc-status-code response) 0)
                    (equal? (utf8->string (grpc-response-payload response))
                            "mtls-ok")))))))))
