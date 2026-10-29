(import (cspp) (cspp net ffi) (cspp net lws ffi) (cspp crypto ffi))

(define dependencies '(openssl xxhash blake3 curl ssh websockets grpc zlib cares idn2 uuid))

(define contains-build-disabled?
  (lambda (text)
    (let ([needle "disabled at build time"])
      (and (string? text)
           (let loop ([start 0])
             (and (<= (+ start (string-length needle)) (string-length text))
                  (or (string=? needle (substring text start (+ start (string-length needle))))
                      (loop (+ start 1)))))))))

(for-each
 (lambda (dependency)
   (let ([info (optional-library-info dependency)])
     (unless (eq? dependency (optional-library-name info))
       (error 'optional-library-runtime "descriptor name changed" dependency))
     (unless (optional-library-available? info)
       (unless (and (not (optional-library-version info))
                    (null? (optional-library-capabilities info))
                    (contains-build-disabled? (optional-library-error info)))
         (error 'optional-library-runtime "invalid disabled metadata" dependency)))))
 dependencies)

(define check-disabled-error
  (lambda (dependency operation)
    (unless (optional-library-available? (optional-library-info dependency))
      (unless (guard (condition
                     [else (and (message-condition? condition)
                                (contains-build-disabled? (condition-message condition)))])
                (operation)
                #f)
        (error 'optional-library-runtime "disabled API did not raise" dependency)))))

;; Disabled UUID creation must raise before constructing a record from an error vector.
(check-disabled-error 'uuid (lambda () (make-uuid)))

;; Disabled UUID integer comparison must raise before exposing its -1 sentinel.
(check-disabled-error 'uuid
                      (lambda () (uuid=? (bytevector->uuid (make-bytevector 16))
                                         (bytevector->uuid (make-bytevector 16)))))

;; Disabled xxHash unsigned results must raise before exposing zero.
(check-disabled-error 'xxhash (lambda () (xxhash32-bytevector #vu8(1))))

;; Disabled incremental digester creation must raise before exposing a null context.
(check-disabled-error 'blake3 (lambda () (make-digester 'blake3)))

;; Disabled digests must raise instead of exposing an error vector as a digest.
(check-disabled-error 'openssl (lambda () (digest-bytevector 'sha256 #vu8(1))))
(check-disabled-error 'openssl (lambda () (digest-string 'sha256 "a")))
(check-disabled-error 'openssl (lambda () (make-digester 'sha256)))
(check-disabled-error 'blake3 (lambda () (digest-bytevector 'blake3 #vu8(1))))
(check-disabled-error 'blake3 (lambda () (digest-string 'blake3 "a")))

;; Disabled OpenSSL status APIs must raise before exposing -1.
(check-disabled-error 'openssl (lambda () (ffi-random-status)))

;; Disabled OpenSSL cleanup must raise before appearing to free an invalid handle.
(check-disabled-error 'openssl (lambda () (ffi-digest-state-destroy 1)))

;; Disabled OpenSSL handle creation must raise before returning zero.
(check-disabled-error 'openssl (lambda () (ffi-net-tls-context-create 0)))

;; Disabled zlib cleanup must raise before appearing to close an invalid handle.
(check-disabled-error 'zlib (lambda () (ffi-zlib-stream-close 1)))

;; Disabled lws command statuses must raise before exposing -1.
(check-disabled-error 'websockets (lambda () (lws-client-acquire 1 1 1 1)))

;; Disabled lws void operations must raise before appearing to drain a signal.
(check-disabled-error 'websockets (lambda () (lws-signal-drain 1)))

;; Enabled OpenSSL direct links must retain real digest and random behavior.
(when (optional-library-available? (optional-library-info 'openssl))
  (unless (and (= 32 (bytevector-length (sha256-bytevector #vu8(1))))
               (= 8 (bytevector-length (random-bytes 8))))
    (error 'optional-library-runtime "enabled OpenSSL operation failed")))
