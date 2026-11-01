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
  #|proc:envelope-kem
  The `envelope-kem` procedure returns the key agreement algorithm symbol of `envelope`.
  Values that are not envelopes raise an error.
  |#
  (define envelope-kem
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-kem envelope))))

  #|proc:envelope-cipher
  The `envelope-cipher` procedure returns the authenticated cipher algorithm symbol of `envelope`.
  Values that are not envelopes raise an error.
  |#
  (define envelope-cipher
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-cipher envelope))))

  #|proc:envelope-ephemeral-key
  The `envelope-ephemeral-key` procedure returns the stored PEM public-key bytevector of `envelope`.
  Values that are not envelopes raise an error.
  The bytevector is shared with the envelope; mutating it changes the envelope.
  |#
  (define envelope-ephemeral-key
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-ephemeral-key envelope))))

  #|proc:envelope-nonce
  The `envelope-nonce` procedure returns the stored nonce bytevector of `envelope`.
  Values that are not envelopes raise an error.
  The bytevector is shared with the envelope; mutating it changes the envelope.
  |#
  (define envelope-nonce
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-nonce envelope))))

  #|proc:envelope-ciphertext
  The `envelope-ciphertext` procedure returns the stored ciphertext bytevector of `envelope`.
  Values that are not envelopes raise an error.
  The bytevector is shared with the envelope; mutating it changes the envelope.
  |#
  (define envelope-ciphertext
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-ciphertext envelope))))

  #|proc:envelope-tag
  The `envelope-tag` procedure returns the stored authentication-tag bytevector of `envelope`.
  Values that are not envelopes raise an error.
  The bytevector is shared with the envelope; mutating it changes the envelope.
  |#
  (define envelope-tag
    (lambda (envelope)
      (pcheck ([envelope? envelope]) (crypto-envelope-tag envelope))))

  (define derive-envelope-key
    (lambda (shared)
      (hkdf 'sha256 shared (bytevector) (string->utf8 "cspp-envelope") 32)))

  (define call-with-temporary-key
    (lambda (create destroy! proc)
      (with-crypto-operation
        (let ([key (create)])
          (dynamic-wind void (lambda () (proc key)) (lambda () (destroy! key)))))))

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
      (pcheck ([public-key? recipient-public-key] [bytevector? aad]
               [string/bytevector? plaintext])
              (unless (eq? (public-key-algorithm recipient-public-key) 'x25519)
                (errorf who "safe envelope currently requires an x25519 recipient public key"))
              (let ([pt (string/bytevector->utf8 plaintext)])
                (call-with-temporary-key (lambda () (generate-private-key 'x25519))
                  destroy-private-key!
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
              (call-with-temporary-key
                (lambda () (pem->public-key (envelope-ephemeral-key envelope)))
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
