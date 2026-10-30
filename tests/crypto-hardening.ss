(import (cspp))

(define crypto-rejects?
  (lambda (thunk)
    (guard (condition [else (or (error? condition) (violation? condition))])
      (thunk)
      #f)))

(define read-crypto-fixture
  (lambda (name)
    (let* ([root (if (file-exists? "data/crypto") "data/crypto/" "tests/data/crypto/")]
           [port (open-file-input-port (string-append root name))])
      (dynamic-wind void
                    (lambda () (get-bytevector-all port))
                    (lambda () (close-port port))))))

(mat crypto-password-empty-derived-key
  ;; Error case: empty PBKDF2 output contains no information about the password.
  (mat-requires (openssl)
    (error? (password-hash/pbkdf2 "correct" #vu8(1 2 3 4) 1 0)))

  ;; Error case: empty scrypt output also permits every password to match.
  (mat-requires (openssl)
    (error? (password-hash/scrypt "correct" #vu8(1 2 3 4) 16 1 1 0)))

  ;; Error case: externally supplied records must not bypass constructor validation.
  (let ([encoded (hex->bytevector
                   "4350573101020000000100000000000000000000000000000000")])
    (and (not (password-hash? encoded))
         (not (password-verify? "wrong" encoded))))

  ;; Error case: zero-length scrypt records must be rejected before deriving a candidate.
  (let ([encoded (hex->bytevector
                   "4350573101010000001000000001000000010000000000000000")])
    (and (not (password-hash? encoded))
         (not (password-verify? "wrong" encoded)))))

(mat crypto-aead-full-authentication-tags
  (mat-requires (openssl)
    (for-all
      (lambda (algorithm)
        (let ([key (make-bytevector (aead-key-size algorithm) 7)]
              [nonce (make-bytevector 12 9)]
              [plaintext #vu8(1 2 3)])
          (let-values ([(ciphertext tag) (aead-encrypt algorithm key nonce plaintext)])
            (and (equal? plaintext (aead-decrypt algorithm key nonce ciphertext tag))
                 ;; Error case: an otherwise correct truncated tag must not authenticate.
                 (for-all
                   (lambda (length)
                     (let ([short-tag (make-bytevector length)])
                       (bytevector-copy! tag 0 short-tag 0 length)
                       (and (not (aead-decrypt algorithm key nonce ciphertext short-tag))
                            (not (aead-decrypt/aad algorithm key nonce #vu8()
                                                  ciphertext short-tag))
                            (not (call-with-aead-decrypt-state algorithm key nonce
                                   (lambda (state)
                                     (aead-update-ciphertext-bytevector! state ciphertext)
                                     (aead-finalize-decrypt! state short-tag)))))))
                   '(0 1 15))
                 ;; Error case: longer tags cannot change the required tag size either.
                 (not (aead-decrypt algorithm key nonce ciphertext (make-bytevector 17)))))))
      aead-algorithms)))

(mat crypto-certificate-hostname-validation
  (mat-requires (openssl)
    (let* ([leaf (load-certificate (read-crypto-fixture "leaf.cert.pem"))]
           [root (load-certificate (read-crypto-fixture "root.cert.pem"))]
           [intermediate (load-certificate (read-crypto-fixture "intermediate.cert.pem"))]
           [store (make-cert-store)])
      (dynamic-wind
        (lambda () (cert-store-add! store root))
        (lambda ()
          (and (certificate-hostname-matches? leaf "example.com")
               (verify-certificate-chain leaf (list intermediate) store "example.com")
               ;; Error case: malformed names must not truncate or disable hostname checks.
               (for-all
                 (lambda (hostname)
                   (and (crypto-rejects?
                          (lambda () (certificate-hostname-matches? leaf hostname)))
                        (crypto-rejects?
                          (lambda () (verify-certificate-chain leaf store hostname)))
                        (crypto-rejects?
                          (lambda () (verify-certificate-chain
                                       leaf (list intermediate) store hostname)))))
                 (list "" (string #\nul)
                       (string-append "example.com" (string #\nul) ".evil")))))
        (lambda ()
          (destroy-cert-store! store)
          (for-each destroy-certificate! (list leaf root intermediate)))))))

(mat crypto-signing-key-compatibility
  (mat-requires (openssl)
    (let* ([private (generate-private-key 'rsa)]
           [public (private-key->public-key private)])
      (dynamic-wind
        void
        (lambda ()
          (let ([signature (sign-message 'rsa-pss private #vu8(1 2 3))])
            (and (verify-signature? 'rsa-pss public #vu8(1 2 3) signature)
                 ;; Error case: RSA keys must not sign or verify under an ECDSA label.
                 (crypto-rejects? (lambda () (sign-message 'ecdsa private #vu8(1 2 3))))
                 (crypto-rejects?
                   (lambda () (verify-signature? 'ecdsa public #vu8(1 2 3) signature)))
                 (crypto-rejects? (lambda () (make-sign-state 'ecdsa private)))
                 (crypto-rejects? (lambda () (make-verify-state 'ecdsa public)))
                 ;; Error case: buffered constructors must reject already destroyed keys.
                 (begin
                   (destroy-private-key! private)
                   (destroy-public-key! public)
                   (and (crypto-rejects? (lambda () (make-sign-state 'rsa-pss private)))
                        (crypto-rejects? (lambda () (make-verify-state 'rsa-pss public)))
                        (crypto-rejects?
                          (lambda () (verify-signature?
                                       'rsa-pss public #vu8(1 2 3) signature))))))))
        (lambda () (destroy-public-key! public) (destroy-private-key! private))))))

(mat crypto-agreement-key-compatibility
  (mat-requires (openssl)
    (for-all
      (lambda (entry)
        (let* ([private (generate-private-key (car entry))]
               [public (private-key->public-key private)])
          (dynamic-wind
            void
            (lambda ()
              (and (bytevector? (derive-shared-secret (car entry) private public))
                   ;; Error case: a valid pair cannot be used under another KEM label.
                   (crypto-rejects?
                     (lambda () (derive-shared-secret (cadr entry) private public)))))
            (lambda () (destroy-public-key! public) (destroy-private-key! private)))))
      '((x25519 ecdh) (ecdh x25519)))))

(mat crypto-kdf-integer-validation
  ;; Error case: the low 32 bits must not turn a huge work factor into one iteration.
  (mat-requires (openssl)
    (crypto-rejects? (lambda () (pbkdf2 'sha256 "password" "salt" 4294967297 32))))

  ;; Error case: work factors and output lengths must not truncate at the native boundary.
  (mat-requires (openssl)
    (for-all
      crypto-rejects?
      (list (lambda () (pbkdf2 'sha256 "password" "salt" 0 32))
            (lambda () (pbkdf2 'sha256 "password" "salt" 1.0 32))
            (lambda () (pbkdf2 'sha256 "password" "salt" 1 4294967328))
            (lambda () (hkdf 'sha256 "key" "salt" "info" 4294967328))
            (lambda () (hkdf-expand 'sha256 #vu8(1) "info" 4294967328))
            (lambda () (scrypt "password" "salt" 4294967312 1 1 32))
            (lambda () (scrypt "password" "salt" 16 4294967297 1 32))
            (lambda () (scrypt "password" "salt" 16 1 4294967297 32))
            (lambda () (scrypt "password" "salt" 16 1 1 4294967328))
            (lambda () (password-hash/pbkdf2 "password" #vu8(1) 4294967297 32))
            (lambda () (password-hash/scrypt "password" #vu8(1) 4294967312 1 1 32)))))

  ;; Literal PBKDF2-HMAC-SHA256 vector verifies the valid path after range checks.
  (mat-requires (openssl)
    (string=? "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"
              (bytevector->hex (pbkdf2 'sha256 "password" "salt" 1 32)))))

(mat crypto-kdf-optional-backend
  ;; Error case: absent OpenSSL must raise instead of returning native sentinel vectors.
  (or (optional-library-available? (optional-library-info 'openssl))
      (for-all crypto-rejects?
        (list (lambda () (hkdf 'sha256 "key" "salt" "info" 32))
              (lambda () (hkdf-extract 'sha256 "key" "salt"))
              (lambda () (hkdf-expand 'sha256 #vu8(1) "info" 32))
              (lambda () (pbkdf2 'sha256 "password" "salt" 1 32))
              (lambda () (scrypt "password" "salt" 16 1 1 32))))))
