(library (cspp crypto envelope)
  (export seal-envelope
          seal-envelope/aad
          open-envelope
          open-envelope/aad
          envelope?
          envelope-kem
          envelope-cipher
          envelope-ephemeral-key
          envelope-nonce
          envelope-ciphertext
          envelope-tag)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto private)
          (only (cspp crypto ffi) ffi-pkey-store-public-pem)
          (cspp crypto random)
          (cspp crypto pkey)
          (cspp crypto aead)
          (cspp crypto kdf)
          (cspp crypto key-agreement))

  (define envelope? crypto-envelope?)
  (define envelope-kem crypto-envelope-kem)
  (define envelope-cipher crypto-envelope-cipher)
  (define envelope-ephemeral-key crypto-envelope-ephemeral-key)
  (define envelope-nonce crypto-envelope-nonce)
  (define envelope-ciphertext crypto-envelope-ciphertext)
  (define envelope-tag crypto-envelope-tag)

  (define derive-envelope-key
    (lambda (shared)
      (hkdf 'sha256 shared (bytevector) (string->utf8 "cspp-envelope") 32)))

  (define call-with-temporary-key
    (lambda (key destroy! proc)
      (dynamic-wind void (lambda () (proc key)) (lambda () (destroy! key)))))

  #|proc:seal-envelope
  The `seal-envelope` procedure returns an encrypted envelope for the live X25519
  `recipient-public-key` and string or bytevector `plaintext`. Strings use UTF-8.
  Temporary native keys are released on every exit; the recipient key stays owned
  by the caller. The envelope uses empty associated data.
  |#
  (define-who seal-envelope
    (lambda (recipient-public-key plaintext)
      (seal-envelope/aad recipient-public-key (bytevector) plaintext)))

  #|proc:seal-envelope/aad
  The `seal-envelope/aad` procedure returns an encrypted envelope for the live X25519
  `recipient-public-key`, associated-data bytevector `aad`, and string or bytevector
  `plaintext`. Strings use UTF-8. Temporary native keys are released on every exit;
  the recipient key stays owned by the caller. Native failures raise errors.
  |#
  (define-who seal-envelope/aad
    (lambda (recipient-public-key aad plaintext)
      (pcheck ([public-key? recipient-public-key] [bytevector? aad])
              (unless (eq? (public-key-algorithm recipient-public-key) 'x25519)
                (errorf who "safe envelope currently requires an x25519 recipient public key"))
              (let ([pt (string/bytevector->utf8 plaintext)])
                (call-with-temporary-key (generate-private-key 'x25519) destroy-private-key!
                  (lambda (epriv)
                    (let* ([shared (derive-shared-secret 'x25519 epriv recipient-public-key)]
                           [key (derive-envelope-key shared)]
                           [nonce (random-bytes 12)]
                           ;; EVP can serialize the public component without another key object.
                           [ephemeral (ffi-pkey-store-public-pem
                                        (crypto-private-key-handle epriv))])
                      (unless (bytevector? ephemeral)
                        (errorf who "failed to serialize ephemeral public key"))
                      (let-values ([(ct tag)
                                    (aead-encrypt/aad 'chacha20-poly1305 key nonce aad pt)])
                        (make-crypto-envelope 'x25519 'chacha20-poly1305
                                              ephemeral nonce ct tag)))))))))

  #|proc:open-envelope
  The `open-envelope` procedure decrypts `envelope` using the live X25519
  `recipient-private-key` and empty associated data. It returns a plaintext bytevector
  or `#f` on authentication failure. Temporary native keys are released on every exit;
  the recipient key stays owned by the caller.
  |#
  (define-who open-envelope
    (lambda (recipient-private-key envelope)
      (open-envelope/aad recipient-private-key (bytevector) envelope)))

  #|proc:open-envelope/aad
  The `open-envelope/aad` procedure decrypts `envelope` using the live X25519
  `recipient-private-key` and associated-data bytevector `aad`. It returns a plaintext
  bytevector or `#f` on authentication failure. Temporary native keys are released on
  every exit; the recipient key stays owned by the caller. Native failures raise errors.
  |#
  (define-who open-envelope/aad
    (lambda (recipient-private-key aad envelope)
      (pcheck ([private-key? recipient-private-key] [bytevector? aad] [envelope? envelope])
              (unless (eq? (private-key-algorithm recipient-private-key) 'x25519)
                (errorf who "safe envelope currently requires an x25519 recipient private key"))
              (unless (and (eq? (envelope-kem envelope) 'x25519)
                           (eq? (envelope-cipher envelope) 'chacha20-poly1305))
                (errorf who "unsupported envelope"))
              (call-with-temporary-key (pem->public-key (envelope-ephemeral-key envelope))
                destroy-public-key!
                (lambda (epub)
                  (let* ([shared (derive-shared-secret 'x25519 recipient-private-key epub)]
                         [key (derive-envelope-key shared)])
                    (aead-decrypt/aad 'chacha20-poly1305 key
                                      (envelope-nonce envelope)
                                      aad
                                      (envelope-ciphertext envelope)
                                      (envelope-tag envelope))))))))
  )
