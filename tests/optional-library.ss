(import (cspp)
        (cspp uuid)
        (cspp hash)
        (cspp optional-library)
        (cspp crypto)
        (cspp net)
        (cspp net private ffi))

(define disabled-api-error?
  (lambda (library operation)
    (if (optional-library-available? (optional-library-info library))
        #t
        (guard (condition
                [else
                 (let ([message (cond [(net-error? condition) (net-error-message condition)]
                                      [(message-condition? condition) (condition-message condition)]
                                      [else #f])])
                   (and (string? message)
                        (string-contains? message "disabled at build time")))])
          (operation)
          #f))))

(mat optional-library-descriptors
  (for-all
   (lambda (library)
     (let ([info (optional-library-info library)])
       (and (optional-library-info? info)
            (eq? library (optional-library-name info))
            (boolean? (optional-library-available? info))
            (list? (optional-library-capabilities info))
            (if (optional-library-available? info)
                (and (or (not (optional-library-version info))
                         (string? (optional-library-version info)))
                     (not (optional-library-error info)))
                (and (not (optional-library-version info))
                     (null? (optional-library-capabilities info))
                     (string-contains? (optional-library-error info) "disabled at build time"))))))
   '(cares curl grpc idn2 ssh websockets zlib openssl uuid xxhash blake3)))

(mat optional-library-call-time-errors
  ;; Error case: disabled DNS fails at the API call after CSPP has imported successfully.
  (disabled-api-error? 'cares (lambda () (dns-resolve "localhost")))

  ;; Error case: disabled FTP fails before opening a session or contacting a server.
  (disabled-api-error? 'curl (lambda () (ftp-open "ftp://127.0.0.1:1")))

  ;; Error case: disabled IDNA fails before converting an otherwise valid domain.
  (disabled-api-error? 'idn2 (lambda () (idna->ascii "example.test")))

  ;; Error case: disabled SSH fails before opening a network session.
  (disabled-api-error? 'ssh (lambda () (ssh-open "127.0.0.1" 1)))

  ;; Error case: disabled WebSocket fails before opening a network connection.
  (disabled-api-error? 'websockets (lambda () (websocket-connect "ws://127.0.0.1:1")))

  ;; Error case: disabled gRPC fails before creating a native channel.
  (disabled-api-error? 'grpc (lambda () (grpc-open-channel "127.0.0.1" 1)))

  ;; Error case: disabled compression raises instead of exposing a zero stream handle.
  (disabled-api-error? 'zlib (lambda () (ffi-zlib-stream-open 1 0)))

  ;; Error case: disabled crypto raises instead of returning random bytes.
  (disabled-api-error? 'openssl (lambda () (random-bytes 8)))

  ;; Error case: disabled UUID raises instead of constructing an invalid UUID record.
  (disabled-api-error? 'uuid (lambda () (make-uuid)))

  ;; Error case: disabled xxHash raises instead of exposing its unsigned zero sentinel.
  (disabled-api-error? 'xxhash (lambda () (xxhash32-string "example")))

  ;; Error case: disabled BLAKE3 raises instead of returning an error vector as a digest.
  (disabled-api-error? 'blake3 (lambda () (blake3-bytevector #vu8(1)))))
