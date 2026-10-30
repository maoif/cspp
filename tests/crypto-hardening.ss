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
