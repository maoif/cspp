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
    [(string=? mode "input-objects")
     (let ([bad-bytes (list #f 42 '() '#(0) "text")]
           [bad-text (list #f 42 '() '#(0) #vu8(1))])
       (for-each
         (lambda (invalid)
           ;; Error case: every loader arity must type-check before taking a length,
           ;; accessing native bytes, or checking backend availability.
           (for-each
             (lambda (load)
               (for-each
                 (lambda (arguments)
                   (unless (rejects-input? (lambda () (apply load arguments)))
                     (error 'crypto-inputs "loader failed to type-check" arguments)))
                 (list (list invalid) (list invalid 'pem)
                       (list invalid 'pem 0) (list invalid 'pem 0 0))))
             (list load-private-key load-public-key load-certificate load-cert-chain))
           (for-each
             (lambda (hash)
               (for-each
                 (lambda (arguments)
                   (unless (rejects-input? (lambda () (apply hash arguments)))
                     (error 'crypto-inputs "hash failed to type-check" arguments)))
                 (list (list 'md5 invalid) (list 'md5 invalid 0)
                       (list 'md5 invalid 0 0))))
             (list unsafe-hash-bytevector)))
         bad-bytes)
       ;; Error case: string overloads must reject nonstrings before string-length.
       (for-each
         (lambda (invalid)
           (for-each
             (lambda (arguments)
               (unless (rejects-input? (lambda () (apply unsafe-hash-string arguments)))
                 (error 'crypto-inputs "string hash failed to type-check" arguments)))
             (list (list 'md5 invalid) (list 'md5 invalid 0) (list 'md5 invalid 0 0))))
         bad-text)
       (when (optional-library-available? (optional-library-info 'openssl))
         (let* ([private (generate-private-key 'ed25519)]
                [public (private-key->public-key private)]
                [signer (make-sign-state 'ed25519 private)]
                [verifier (make-verify-state 'ed25519 public)])
           (dynamic-wind
             void
             (lambda ()
               (for-each
                 (lambda (entry)
                   (let ([update (car entry)] [state (cadr entry)] [invalids (caddr entry)])
                     ;; Error case: valid states must not hide missing data checks.
                     (for-each
                       (lambda (invalid)
                         (for-each
                           (lambda (arguments)
                             (unless (rejects-input? (lambda () (apply update arguments)))
                               (error 'crypto-inputs "update failed to type-check" arguments)))
                           (list (list state invalid) (list state invalid 0)
                                 (list state invalid 0 0))))
                       invalids)))
                 (list (list sign-update-bytevector! signer bad-bytes)
                       (list verify-update-bytevector! verifier bad-bytes)
                       (list sign-update-string! signer bad-text)
                       (list verify-update-string! verifier bad-text))))
             (lambda ()
               (sign-destroy! signer)
               (verify-destroy! verifier)
               (destroy-public-key! public)
               (destroy-private-key! private))))))]
    [else (error 'crypto-inputs "unknown fixture mode" mode)]))
