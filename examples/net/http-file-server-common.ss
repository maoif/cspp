(import (cspp net http)
        (cspp net tls)
        (cspp net uri)
        (cspp path)
        (cspp logging))
(load "examples/common.ss")

(define server-log
  (lambda ()
    (let ([logger (make-logger 'cspp-file-server)])
      (logger-add-sink! logger
                        (make-log-port-sink 'stderr (current-error-port)))
      logger)))

(define reject-symlink-path?
  (lambda (path)
    (let loop ([current (path-normalize (path-parse 'unix path))]
               [parts (reverse (path-components
                                (path-normalize (path-parse 'unix path))))])
      (or (file-symbolic-link? (path-render current))
          (and (pair? parts)
               (loop (path-dirname current) (cdr parts)))))))

(define file-server-response
  (lambda (root request)
    (let* ([request-uri (http-request-uri request)]
           [segments (uri-path-segments request-uri)])
      (cond
       [(not (string=? (http-request-method request) "GET"))
        (make-http-response 405 "Method Not Allowed" '() #vu8())]
       [(or (null? segments)
            (exists (lambda (segment)
                      (or (string=? segment "")
                          (string=? segment ".")
                          (string=? segment "..")
                          (string-contains? segment "/")
                          (string-contains? segment "\\")))
                    segments))
        (make-http-response 400 "Bad Request" '() #vu8())]
       [else
        (let ([path (fold-left path-add-component (path-parse 'unix root) segments)])
          (let ([full-path (path-render path)])
            (if (or (not (file-regular? full-path))
                    (reject-symlink-path? full-path))
                (make-http-response 404 "Not Found" '() #vu8())
                (make-http-response
                 200 "OK"
                 '(("content-type" . "application/octet-stream"))
                 (make-http-file-body-source full-path)))))]))))

(define run-http-file-server!
  (lambda (protocol-version ref)
    (let* ([host (or (ref 'host) "127.0.0.1")]
           [port (example-option-natural 'http-file-server ref 'port 8080)]
           [root-arg (example-required 'http-file-server ref 'dir)]
           [root (path-render (path-normalize (path-absolute
                                                (path-parse 'unix (current-directory))
                                                (path-parse 'unix root-arg))))])
      (unless (<= 1 port 65535)
        (errorf 'http-file-server "--port must be between 1 and 65535"))
      (unless (and (file-directory? root) (not (reject-symlink-path? root)))
        (errorf 'http-file-server "directory is missing or contains a symlink: ~a" root))
      (let ([log (server-log)] [context #f] [server #f] [interrupted? #f]
            [old-interrupt (keyboard-interrupt-handler)])
        (dynamic-wind
          (lambda ()
            (keyboard-interrupt-handler
             (lambda ()
               (set! interrupted? #t)
               (errorf 'http-file-server "interrupted"))))
          (lambda ()
            (when (eq? protocol-version 'https)
              ;; Allocate a server TLS context and load the certificate and matching key.
              (set! context (make-tls-context 'server))
              (tls-context-load-cert! context (example-required 'https-file-server ref 'cert))
              (tls-context-load-private-key! context (example-required 'https-file-server ref 'key)))
            ;; Cleartext H2 uses an explicit prior-knowledge listener; TLS negotiates via ALPN.
            (set! server (http-listen host port context 128
                                     (if (eq? protocol-version 'h2) 'h2 'h1)))
            (with-logger log (log-infof "serving ~a from ~a:~a" root host port))
            (let loop ()
              ;; Accept one logical HTTP request, preserving sibling streams on H2 connections.
              (let ([connection (http-accept server)])
                (dynamic-wind
                  void
                  (lambda ()
                    (guard (condition
                            [interrupted? (raise condition)]
                            [else
                             (with-logger log
                               (log-warnf "request failed: ~a" condition))])
                      ;; Read the request and resolve a regular file under the selected root.
                      (let* ([request (http-read-request connection)]
                             [response (file-server-response root request)])
                        ;; Stream the response; its file source closes after success or failure.
                        (http-write-response connection response))))
                  (lambda ()
                    ;; Cancel only this request if the peer disconnected before completion.
                    (http-connection-close connection))))
              (loop)))
          (lambda ()
            (keyboard-interrupt-handler old-interrupt)
            ;; Close successfully allocated resources even when TLS or listener setup fails.
            (when server (http-server-close server))
            (when context (close-tls-context context))
            (with-logger log (log-info "file server closed"))
            (logger-close! log))))
      0)))
