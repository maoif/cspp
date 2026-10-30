(import (chezscheme) (cspp crypto) (cspp crypto ffi) (cspp optional-library))

(define rejects-input?
  (lambda (proc)
    (guard (condition
             [else (and (who-condition? condition) (eq? (condition-who condition) 'pcheck))])
      (proc)
      #f)))

(let ([mode (car (command-line-arguments))])
  (cond
    [(string=? mode "random-lengths")
     ;; Error case: inexact lengths must not cross the integer FFI and become huge
     ;; allocation requests. Reject negative and unrepresentable exact lengths too.
     (for-each
       (lambda (length)
         (for-each
           (lambda (make)
             (unless (rejects-input? (lambda () (make length)))
               (error 'crypto-inputs "invalid random output length accepted" length)))
           (list random-bytes random-key-bytes random-nonce ffi-random-bytevector)))
       (list 1.0 0.0 8.0 +inf.0 +nan.0 -1 1/2 'one
             (+ (most-positive-fixnum) 1) (expt 2 64)))
     (when (optional-library-available? (optional-library-info 'openssl))
       (for-each
         (lambda (make)
           (for-each
             (lambda (length)
               (unless (= length (bytevector-length (make length)))
                 (error 'crypto-inputs "wrong random output length" length)))
             '(0 1 32)))
         (list random-bytes random-key-bytes random-nonce ffi-random-bytevector)))]
    [(string=? mode "encrypted-pem")
     (when (optional-library-available? (optional-library-info 'openssl))
       (for-each
         (lambda (fixture)
           (let* ([port (open-file-input-port (string-append "tests/data/crypto/" fixture))]
                  [encrypted (dynamic-wind void (lambda () (get-bytevector-all port))
                                           (lambda () (close-port port)))])
             ;; Error case: encrypted PEM has no supported password argument and must
             ;; fail without invoking OpenSSL's interactive terminal password callback.
             (for-each
               (lambda (load)
                 (unless (guard (condition
                                  [else (and (who-condition? condition)
                                             (eq? (condition-who condition) 'load-private-key))])
                           (load encrypted)
                           #f)
                   (error 'crypto-inputs "encrypted PEM unexpectedly loaded")))
               (list load-private-key pem->private-key))))
         ;; Generated ED25519 fixtures use pass:cspp-test-only and pass: respectively.
         '("encrypted-key.pem" "encrypted-empty-password-key.pem"))
       (let ([private (generate-private-key 'ed25519)])
         (dynamic-wind
           void
           (lambda ()
             (let ([loaded (pem->private-key (store-private-key private))])
               (dynamic-wind void
                 (lambda ()
                   (unless (private-key=? private loaded)
                     (error 'crypto-inputs "unencrypted PEM roundtrip failed")))
                 (lambda () (destroy-private-key! loaded)))))
           (lambda () (destroy-private-key! private)))))]
    [else (error 'crypto-inputs "unknown fixture mode" mode)]))
