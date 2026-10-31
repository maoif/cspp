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
    [(string=? mode "public-contracts")
     ;; Error case: opaque-record accessors must reject arbitrary Scheme values,
     ;; including records of the wrong kind, before optimized field access.
     (for-each
       (lambda (accessor)
         (for-each
           (lambda (invalid)
             (unless (rejects-input? (lambda () (accessor invalid)))
               (error 'crypto-inputs "accessor failed to type-check" invalid)))
           (list #f 42 '() '#(0) "text")))
       (list private-key-algorithm public-key-algorithm private-key-bits public-key-bits
             envelope-kem envelope-cipher envelope-ephemeral-key envelope-nonce
             envelope-ciphertext envelope-tag))
     ;; Error case: algorithm queries and input unions reject invalid types with pcheck.
     (for-each
       (lambda (proc)
         (unless (rejects-input? proc)
           (error 'crypto-inputs "public argument failed to type-check")))
       (list (lambda () (aead-key-size #f))
             (lambda () (aead-default-nonce-size #f))
             (lambda () (aead-tag-size #f))
             (lambda () (cipher-key-size #f))
             (lambda () (cipher-iv-size #f))
             (lambda () (cipher-block-size #f))
             (lambda () (make-cipher-state #f 'aes-128-ctr
                          (make-bytevector 16) (make-bytevector 16)))
             (lambda () (aead-encrypt #f #vu8() #vu8() #vu8()))
             (lambda () (make-aead-encrypt-state #f #vu8() #vu8()))
             (lambda () (make-hmac-state #f #vu8()))
             (lambda () (hmac 'sha256 #vu8() #f))
             (lambda () (unsafe-hash #f #vu8()))
             (lambda () (unsafe-hash 'md5 #f))
             (lambda () (password-hash #f))
             (lambda () (password-verify? #f #vu8()))
             (lambda () (password-hash/pbkdf2 #f #vu8() 1 1))
             (lambda () (password-hash/scrypt #f #vu8() 16 1 1 1))
             (lambda () (hkdf 'sha256 #f #vu8() #vu8() 1))
             (lambda () (hkdf-extract 'sha256 #vu8() #f))
             (lambda () (hkdf-expand 'sha256 #vu8() #f 1))
             (lambda () (pbkdf2 'sha256 #f #vu8() 1 1))
             (lambda () (scrypt #f #vu8() 16 1 1 1))
             (lambda () (hkdf 'sha256 #vu8() #vu8() #vu8() 1.0))
             (lambda () (pbkdf2 'sha256 #vu8() #vu8() 1.0 1))
             (lambda () (scrypt #vu8() #vu8() 16.0 1 1 1))))
     ;; Error case: inexact or bignum slice indices must fail before fixnum arithmetic.
     (for-each
       (lambda (index)
         (for-each
           (lambda (proc)
             (unless (rejects-input? (lambda () (proc index)))
               (error 'crypto-inputs "slice index failed to type-check" index)))
           (list (lambda (index) (random-bytes! #vu8(1) index 1))
                 (lambda (index) (random-bytes! #vu8(1) 0 index))
                 (lambda (index) (constant-time-subbytevector=?
                                  #vu8(1) index 1 #vu8(1) 0 1))
                 (lambda (index) (unsafe-hash-bytevector 'md5 #vu8(1) index 1))
                 (lambda (index) (unsafe-hash-string 'md5 "x" 0 index))
                 (lambda (index) (make-hmac-state 'sha256 #vu8(1) index 1))
                 (lambda (index) (load-private-key #vu8(1) 'pem index 1))
                 (lambda (index) (load-certificate #vu8(1) 'pem 0 index)))))
       (list #f 0.0 1.0 (+ (most-positive-fixnum) 1)))
     (when (optional-library-available? (optional-library-info 'openssl))
       (let* ([private (generate-private-key 'ed25519)]
              [public (private-key->public-key private)])
         (dynamic-wind
           void
           (lambda ()
             (unless (and (eq? 'ed25519 (private-key-algorithm private))
                          (eq? 'ed25519 (public-key-algorithm public))
                          (= (private-key-bits private) (public-key-bits public)))
               (error 'crypto-inputs "valid key metadata changed"))
             ;; Error case: valid keys must not mask missing algorithm/message checks.
             (for-each
               (lambda (proc)
                 (unless (rejects-input? proc)
                   (error 'crypto-inputs "key operation failed to type-check")))
               (list (lambda () (public-key-bits private))
                     (lambda () (private-key-algorithm public))
                     (lambda () (sign-message #f private #vu8()))
                     (lambda () (sign-message 'ed25519 private #f))
                     (lambda () (verify-signature? 'ed25519 public #f #vu8()))
                     (lambda () (make-sign-state #f private))
                     (lambda () (make-verify-state #f public))
                     (lambda () (sign-message 'ed25519 #t private #vu8()))
                     (lambda () (envelope-tag private))
                     (lambda () (store-private-key private #f))))
             (destroy-public-key! public)
             (destroy-private-key! private)
             ;; Immutable metadata remains available after explicit destruction.
             (unless (and (eq? 'ed25519 (private-key-algorithm private))
                          (= (private-key-bits private) (public-key-bits public)))
               (error 'crypto-inputs "destroyed key metadata changed")))
           (lambda () (destroy-public-key! public) (destroy-private-key! private)))))]
    [else (error 'crypto-inputs "unknown fixture mode" mode)]))
