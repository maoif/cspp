(import (cspp))

(mat crypto-input-validation
  ;; Error case: invalid native arguments must raise without aborting or prompting.
  (= 0 (system (if (file-exists? "crypto-input-process.py")
                   "python3 -I crypto-input-process.py"
                   "python3 -I tests/crypto-input-process.py"))))

(mat crypto-continuation-safety
  ;; Error case: suspending native operations must not permit use-after-free or double-free.
  (= 0 (system (if (file-exists? "crypto-continuation-process.py")
                   "python3 -I crypto-continuation-process.py"
                   "python3 -I tests/crypto-continuation-process.py"))))

(mat crypto-thread-safety
  ;; Error case: shared updates and destruction must be serialized without native corruption.
  (= 0 (system (if (file-exists? "crypto-thread-process.py")
                   "python3 -I crypto-thread-process.py"
                   "python3 -I tests/crypto-thread-process.py"))))

(define crypto-rejects?
  (lambda (thunk)
    (guard (condition [else (or (error? condition) (violation? condition))])
      (thunk)
      #f)))

(mat crypto-rsa-bit-count-validation
  ;; Error case: invalid counts must not wrap into small keys or trigger huge allocations.
  (= 0 (system (if (file-exists? "crypto-thread-process.py")
                   "python3 -I crypto-thread-process.py rsa-parameters"
                   "python3 -I tests/crypto-thread-process.py rsa-parameters"))))

(mat crypto-hmac-string-encoding
  (mat-requires (openssl)
    (let ([expected (if (eq? (native-endianness) (endianness little))
                        "2ffc6ca16617b7c6871c4597ca0454ca4ce259e60dbce969b3a4d10e227c2ecd"
                        "3a18be658803b8d8fbdf8f8dbbcd1b4ad48d04430e471ba67b11882ffb9125d2")])
      (and (string=? expected (bytevector->hex (hmac 'sha256 #vu8(1 2 3) "Aλ😀")))
           (hmac-verify? 'sha256 #vu8(1 2 3) "Aλ😀" (hex->bytevector expected))
           (string=? expected
             (bytevector->hex
               (call-with-hmac-state 'sha256 #vu8(1 2 3)
                 (lambda (state)
                   (hmac-update-string! state "_Aλ😀_" 1 2)
                   (hmac-update-string! state "_Aλ😀_" 2 4)
                   (hmac-finalize! state)))))))))

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

(define crypto-test-password-record
  (lambda (algorithm cost block-size parallelism)
    ;; Hand-built CPW1 record: one salt byte and one derived byte, independent of
    ;; the production record encoder. Parameters are unsigned big-endian integers.
    (let ([record (make-bytevector 28 0)])
      (bytevector-copy! #vu8(67 80 87 49 1) 0 record 0 5)
      (bytevector-u8-set! record 5 algorithm)
      (bytevector-u32-set! record 6 cost (endianness big))
      (bytevector-u32-set! record 10 block-size (endianness big))
      (bytevector-u32-set! record 14 parallelism (endianness big))
      (bytevector-u32-set! record 18 1 (endianness big))
      (bytevector-u32-set! record 22 1 (endianness big))
      (bytevector-u8-set! record 26 17)
      (bytevector-u8-set! record 27 34)
      record)))

(mat crypto-password-record-parameters
  ;; Error case: malformed algorithms, counts, and reserved fields must be rejected
  ;; before a KDF is invoked, even when OpenSSL is unavailable.
  (for-all
    (lambda (parameters)
      (let ([record (apply crypto-test-password-record parameters)])
        (and (not (password-hash? record))
             (not (password-verify? "password" record)))))
    '((0 1 0 0) (3 1 0 0)
      (2 0 0 0) (2 2147483648 0 0) (2 1 1 0) (2 1 0 1)
      (1 0 1 1) (1 1 1 1) (1 3 1 1) (1 16 0 1) (1 16 1 0)
      (1 2147483648 1 1) (1 16 2147483648 1) (1 16 1 2147483648)
      (1 65536 1 1) (1 16 32768 32768) (1 16 1073741824 1)))

  ;; Structural validity must not impose the native provider's current memory limits.
  (for-all
    (lambda (parameters) (password-hash? (apply crypto-test-password-record parameters)))
    '((2 1 0 0) (2 2147483647 0 0) (1 16 1 1) (1 32768 1 1)
      (1 1073741824 2 1) (1 16 1 1073741823)))

  (mat-requires (openssl)
    (let ([pbkdf2-record (password-hash/pbkdf2 "password" #vu8(1 2) 1 16)]
          [scrypt-record (password-hash/scrypt "password" #vu8(1 2) 16 1 1 16)])
      (and (password-hash? pbkdf2-record) (password-hash? scrypt-record)
           (password-verify? "password" pbkdf2-record)
           (password-verify? "password" scrypt-record)
           (not (password-verify? "wrong" pbkdf2-record))
           (not (password-verify? "wrong" scrypt-record))))))

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

(mat crypto-aead-owned-key-and-nonce
  (mat-requires (openssl)
    (for-all
      (lambda (algorithm)
        (let* ([key (make-bytevector (aead-key-size algorithm) 7)]
               [nonce (make-bytevector 12 9)]
               [original-key (bytevector-copy key)]
               [original-nonce (bytevector-copy nonce)]
               [plaintext #vu8(1 2 3)])
          (call-with-aead-encrypt-state algorithm key nonce
            (lambda (encryptor)
              (aead-update-plaintext-bytevector! encryptor plaintext)
              ;; Reusing caller buffers must not change an existing state's parameters.
              (bytevector-u8-set! key 0 8)
              (bytevector-u8-set! nonce 0 10)
              (let-values ([(ciphertext tag) (aead-finalize-encrypt! encryptor)])
                (and (equal? plaintext
                        (aead-decrypt algorithm original-key original-nonce ciphertext tag))
                     (call-with-aead-decrypt-state algorithm original-key original-nonce
                       (lambda (decryptor)
                         (aead-update-ciphertext-bytevector! decryptor ciphertext)
                         (bytevector-u8-set! original-key 0 11)
                         (bytevector-u8-set! original-nonce 0 12)
                         (equal? plaintext (aead-finalize-decrypt! decryptor tag))))))))))
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

(mat crypto-signing-digest-validation
  (mat-requires (openssl)
    (let* ([private (generate-private-key 'ed25519)]
           [public (private-key->public-key private)]
           [signature (sign-message 'ed25519 private #vu8(1 2 3))])
      (dynamic-wind
        void
        (lambda ()
          (and (equal? signature (sign-message 'ed25519 #f private #vu8(1 2 3)))
               (verify-signature? 'ed25519 #f public #vu8(1 2 3) signature)
               ;; Error case: Ed25519 must not silently ignore an explicit external digest.
               (for-all
                 (lambda (digest)
                   (and (crypto-rejects?
                          (lambda () (sign-message 'ed25519 digest private #vu8(1 2 3))))
                        (crypto-rejects?
                          (lambda () (verify-signature?
                                       'ed25519 digest public #vu8(1 2 3) signature)))
                        (crypto-rejects? (lambda () (make-sign-state 'ed25519 digest private)))
                        (crypto-rejects?
                          (lambda () (make-verify-state 'ed25519 digest public)))))
                 '(sha256 unsupported 42))))
        (lambda () (destroy-public-key! public) (destroy-private-key! private)))))

  (mat-requires (openssl)
    (let* ([private (generate-private-key 'ecdsa)]
           [public (private-key->public-key private)])
      (dynamic-wind
        void
        (lambda ()
          ;; Error case: unknown digests are input errors for signing and verification alike.
          (and (crypto-rejects? (lambda () (sign-message 'ecdsa 'unsupported private #vu8())))
               (crypto-rejects?
                 (lambda () (verify-signature? 'ecdsa 'unsupported public #vu8() #vu8())))
               (crypto-rejects? (lambda () (make-sign-state 'ecdsa 'unsupported private)))
               (crypto-rejects? (lambda () (make-verify-state 'ecdsa 'unsupported public)))))
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

(mat crypto-envelope-native-ownership
  ;; Error case: success and failure paths must release every temporary native key.
  (= 0 (system (if (file-exists? "crypto-envelope-memory.py")
                   "python3 -I crypto-envelope-memory.py"
                   "python3 -I tests/crypto-envelope-memory.py"))))

(mat crypto-hkdf-empty-output
  (mat-requires (openssl)
    (for-all
      (lambda (algorithm)
        (and (equal? #vu8() (hkdf algorithm "key" "salt" "info" 0))
             (equal? #vu8()
               (hkdf-expand algorithm (hkdf-extract algorithm "key" "salt") "info" 0))))
      '(sha256 sha512)))

  ;; Error case: an empty request still validates its algorithm, inputs, and exact length.
  (and (crypto-rejects? (lambda () (hkdf 'unknown "key" "salt" "info" 0)))
       (crypto-rejects? (lambda () (hkdf 'sha256 #f "salt" "info" 0)))
       (crypto-rejects? (lambda () (hkdf 'sha256 "key" "salt" "info" 0.0)))
       (crypto-rejects? (lambda () (hkdf-expand 'unknown #vu8(1) "info" 0)))
       (crypto-rejects? (lambda () (hkdf-expand 'sha256 #f "info" 0)))
       (crypto-rejects? (lambda () (hkdf-expand 'sha256 #vu8(1) "info" 0.0)))))
