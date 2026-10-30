(library (cspp crypto aead)
  (export aead-algorithms
          aead-key-size
          aead-default-nonce-size
          aead-tag-size
          aead-encrypt
          aead-encrypt/aad
          aead-encrypt/detached
          aead-encrypt/detached-aad
          aead-decrypt
          aead-decrypt/aad
          make-aead-encrypt-state
          make-aead-decrypt-state
          aead-state?
          aead-update-aad-bytevector!
          aead-update-plaintext-bytevector!
          aead-update-ciphertext-bytevector!
          aead-finalize-encrypt!
          aead-finalize-decrypt!
          aead-destroy!
          call-with-aead-encrypt-state
          call-with-aead-decrypt-state)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  (define aead-algorithms '(aes-128-gcm aes-256-gcm chacha20-poly1305))

  (define check-aead-algorithm
    (lambda (who which)
      (unless (memq which aead-algorithms)
        (errorf who "valid AEAD algorithm is one of ~a" aead-algorithms))))

  (define-record-type (aead-state %make-aead-state aead-state?)
    (opaque #t)
    (fields mutex mode
            algorithm
            key
            nonce
            (mutable aad*)
            (mutable data*)
            (mutable destroyed?)
            (mutable finalized?)))

  (define check-live-state
    (lambda (who st)
      (when (aead-state-destroyed? st)
        (errorf who "AEAD state is destroyed"))
      (when (aead-state-finalized? st)
        (errorf who "AEAD state is finalized"))))

  (define aead-key-size
    (lambda (which)
      (check-aead-algorithm 'aead-key-size which)
      (case which
        [(aes-128-gcm) 16]
        [(aes-256-gcm chacha20-poly1305) 32]
        [else (unreachable!)])))

  (define aead-default-nonce-size
    (lambda (which)
      (check-aead-algorithm 'aead-default-nonce-size which)
      12))

  (define aead-tag-size
    (lambda (which)
      (check-aead-algorithm 'aead-tag-size which)
      16))

  (define check-aead-key/nonce
    (lambda (who which key nonce)
      (unless (fx= (bytevector-length key) (aead-key-size which))
        (errorf who "invalid key length ~a for ~s, expected ~a"
                (bytevector-length key) which (aead-key-size which)))
      (unless (fx= (bytevector-length nonce) (aead-default-nonce-size which))
        (errorf who "invalid nonce length ~a for ~s, expected ~a"
                (bytevector-length nonce) which (aead-default-nonce-size which)))))

  (define list->concat
    (lambda (ls)
      (bytevector-append (apply values ls))))

  (define concat*
    (lambda (ls)
      (if (null? ls)
          (bytevector-empty)
          (apply bytevector-append (reverse ls)))))

  (define do-aead-encrypt
    (lambda (who which key nonce aad plaintext)
      (ensure-openssl who)
      (let ([ans (ffi-aead-encrypt which
                                   key 0 (bytevector-length key)
                                   nonce 0 (bytevector-length nonce)
                                   aad 0 (bytevector-length aad)
                                   plaintext 0 (bytevector-length plaintext)
                                   (aead-tag-size which))])
        (when (eq? ans #f)
          (errorf who "AEAD encryption failed for ~s" which))
        (values (vector-ref ans 0) (vector-ref ans 1)))))

  (define do-aead-decrypt
    (lambda (who which key nonce aad ciphertext tag)
      (ensure-openssl who)
      (and (fx= (bytevector-length tag) (aead-tag-size which))
           (ffi-aead-decrypt which
                             key 0 (bytevector-length key)
                             nonce 0 (bytevector-length nonce)
                             aad 0 (bytevector-length aad)
                             ciphertext 0 (bytevector-length ciphertext)
                             tag 0 (bytevector-length tag)))))

  #|proc:aead-encrypt
The `aead-encrypt` procedure encrypts `plaintext` and returns ciphertext and tag.
|#
  (define-who aead-encrypt
    (lambda (which key nonce plaintext)
      (pcheck ([bytevector? key nonce plaintext])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (do-aead-encrypt who which key nonce (bytevector-empty) plaintext))))

  #|proc:aead-encrypt/aad
The `aead-encrypt/aad` procedure encrypts `plaintext` with associated data and returns ciphertext and tag.
|#
  (define-who aead-encrypt/aad
    (lambda (which key nonce aad plaintext)
      (pcheck ([bytevector? key nonce aad plaintext])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (do-aead-encrypt who which key nonce aad plaintext))))

  #|proc:aead-encrypt/detached
The `aead-encrypt/detached` procedure is an alias for `aead-encrypt`.
|#
  (define aead-encrypt/detached
    aead-encrypt)

  #|proc:aead-encrypt/detached-aad
The `aead-encrypt/detached-aad` procedure is an alias for `aead-encrypt/aad`.
|#
  (define aead-encrypt/detached-aad
    aead-encrypt/aad)

  #|proc:aead-decrypt
  The `aead-decrypt` procedure decrypts bytevector `ciphertext` using algorithm symbol
  `which`, bytevector `key`, and bytevector `nonce`. It returns a plaintext bytevector
  when bytevector `tag` authenticates the message, or `#f` on authentication failure.
  The tag must have exactly `aead-tag-size` bytes; other lengths return `#f`.
  |#
  (define-who aead-decrypt
    (lambda (which key nonce ciphertext tag)
      (pcheck ([bytevector? key nonce ciphertext tag])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (do-aead-decrypt who which key nonce (bytevector-empty) ciphertext tag))))

  #|proc:aead-decrypt/aad
  The `aead-decrypt/aad` procedure decrypts bytevector `ciphertext` using algorithm symbol
  `which`, bytevector `key`, bytevector `nonce`, and associated-data bytevector `aad`.
  It returns a plaintext bytevector if bytevector `tag` authenticates both inputs, or
  `#f` otherwise. The tag must have exactly `aead-tag-size` bytes.
  |#
  (define-who aead-decrypt/aad
    (lambda (which key nonce aad ciphertext tag)
      (pcheck ([bytevector? key nonce aad ciphertext tag])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (do-aead-decrypt who which key nonce aad ciphertext tag))))

  #|proc:make-aead-encrypt-state
  The `make-aead-encrypt-state` procedure returns a buffered encryption state for
  algorithm symbol `which`, bytevector `key`, and bytevector `nonce`. The state owns
  copies of the key and nonce, so later caller mutations do not change its parameters.
  Operations on the state are serialized. Invalid algorithms or lengths raise errors.
  |#
  (define-who make-aead-encrypt-state
    (lambda (which key nonce)
      (pcheck ([bytevector? key nonce])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (%make-aead-state (make-mutex 'aead) 'encrypt which
                                (bytevector-copy key) (bytevector-copy nonce) '() '() #f #f))))

  #|proc:make-aead-decrypt-state
  The `make-aead-decrypt-state` procedure returns a buffered decryption state for
  algorithm symbol `which`, bytevector `key`, and bytevector `nonce`. The state owns
  copies of the key and nonce, so later caller mutations do not change its parameters.
  Operations on the state are serialized. Invalid algorithms or lengths raise errors.
  |#
  (define-who make-aead-decrypt-state
    (lambda (which key nonce)
      (pcheck ([bytevector? key nonce])
              (check-aead-algorithm who which)
              (check-aead-key/nonce who which key nonce)
              (%make-aead-state (make-mutex 'aead) 'decrypt which
                                (bytevector-copy key) (bytevector-copy nonce) '() '() #f #f))))

  #|proc:aead-update-aad-bytevector!
The `aead-update-aad-bytevector!` procedure buffers an AAD bytevector slice into an AEAD state.
|#
  (define-who aead-update-aad-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-aad-bytevector! st bv 0 (bytevector-length bv))))]
      [(st bv start)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-aad-bytevector! st bv start (bytevector-length bv))))]
      [(st bv start stop)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (check-live-state who st)
                 (check-slice who (bytevector-length bv) start stop)
                 (aead-state-aad*-set! st (cons (subbytevector bv start stop) (aead-state-aad* st)))
                 st))]))

  #|proc:aead-update-plaintext-bytevector!
The `aead-update-plaintext-bytevector!` procedure buffers plaintext into an encryption state.
|#
  (define-who aead-update-plaintext-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-plaintext-bytevector! st bv 0 (bytevector-length bv))))]
      [(st bv start)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-plaintext-bytevector! st bv start (bytevector-length bv))))]
      [(st bv start stop)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (check-live-state who st)
                 (unless (eq? (aead-state-mode st) 'encrypt)
                   (errorf who "state is not in encrypt mode"))
                 (check-slice who (bytevector-length bv) start stop)
                 (aead-state-data*-set! st (cons (subbytevector bv start stop) (aead-state-data* st)))
                 st))]))

  #|proc:aead-update-ciphertext-bytevector!
The `aead-update-ciphertext-bytevector!` procedure buffers ciphertext into a decryption state.
|#
  (define-who aead-update-ciphertext-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-ciphertext-bytevector! st bv 0 (bytevector-length bv))))]
      [(st bv start)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (aead-update-ciphertext-bytevector! st bv start (bytevector-length bv))))]
      [(st bv start stop)
       (pcheck ([aead-state? st] [bytevector? bv])
               (with-crypto-mutex (aead-state-mutex st)
                 (check-live-state who st)
                 (unless (eq? (aead-state-mode st) 'decrypt)
                   (errorf who "state is not in decrypt mode"))
                 (check-slice who (bytevector-length bv) start stop)
                 (aead-state-data*-set! st (cons (subbytevector bv start stop) (aead-state-data* st)))
                 st))]))

  #|proc:aead-finalize-encrypt!
The `aead-finalize-encrypt!` procedure finalizes an encryption state and returns ciphertext and tag.
|#
  (define-who aead-finalize-encrypt!
    (lambda (st)
      (pcheck ([aead-state? st])
              (with-crypto-mutex (aead-state-mutex st)
                (check-live-state who st)
                (unless (eq? (aead-state-mode st) 'encrypt)
                  (errorf who "state is not in encrypt mode"))
                (let-values ([(ciphertext tag)
                              (do-aead-encrypt who
                                               (aead-state-algorithm st)
                                               (aead-state-key st)
                                               (aead-state-nonce st)
                                               (concat* (aead-state-aad* st))
                                               (concat* (aead-state-data* st)))])
                  (aead-state-finalized?-set! st #t)
                  (values ciphertext tag))))))

  #|proc:aead-finalize-decrypt!
  The `aead-finalize-decrypt!` procedure finalizes the live decryption state `st` using
  authentication bytevector `tag`. It returns the plaintext bytevector or `#f` if
  authentication fails, including when the tag has a length other than `aead-tag-size`.
  The state becomes terminal after either result.
  |#
  (define-who aead-finalize-decrypt!
    (lambda (st tag)
      (pcheck ([aead-state? st] [bytevector? tag])
              (with-crypto-mutex (aead-state-mutex st)
                (check-live-state who st)
                (unless (eq? (aead-state-mode st) 'decrypt)
                  (errorf who "state is not in decrypt mode"))
                (let ([ans (do-aead-decrypt who
                                            (aead-state-algorithm st)
                                            (aead-state-key st)
                                            (aead-state-nonce st)
                                            (concat* (aead-state-aad* st))
                                            (concat* (aead-state-data* st))
                                            tag)])
                  (aead-state-finalized?-set! st #t)
                  ans)))))

  #|proc:aead-destroy!
The `aead-destroy!` procedure marks an AEAD state as destroyed and releases buffered data.
|#
  (define-who aead-destroy!
    (lambda (st)
      (pcheck ([aead-state? st])
              (with-crypto-mutex (aead-state-mutex st)
                (unless (aead-state-destroyed? st)
                  (aead-state-aad*-set! st '())
                  (aead-state-data*-set! st '())
                  (aead-state-destroyed?-set! st #t))
                st))))

  #|proc:call-with-aead-encrypt-state
  The `call-with-aead-encrypt-state` procedure creates a buffered encryption state for
  algorithm symbol `which` and bytevectors `key` and `nonce`. It calls `(proc state)`
  with the caller's interrupt settings and returns its values. The state is destroyed
  on every exit; reentry after exit raises an error.
  |#
  (define-who call-with-aead-encrypt-state
    (lambda (which key nonce proc)
      (pcheck ([bytevector? key nonce] [procedure? proc])
              (call-with-crypto-resource who
                (lambda () (make-aead-encrypt-state which key nonce)) aead-destroy! proc))))

  #|proc:call-with-aead-decrypt-state
  The `call-with-aead-decrypt-state` procedure creates a buffered decryption state for
  algorithm symbol `which` and bytevectors `key` and `nonce`. It calls `(proc state)`
  with the caller's interrupt settings and returns its values. The state is destroyed
  on every exit; reentry after exit raises an error.
  |#
  (define-who call-with-aead-decrypt-state
    (lambda (which key nonce proc)
      (pcheck ([bytevector? key nonce] [procedure? proc])
              (call-with-crypto-resource who
                (lambda () (make-aead-decrypt-state which key nonce)) aead-destroy! proc))))
  )
