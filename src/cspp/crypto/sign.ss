(library (cspp crypto sign)
  (export sign-message
          verify-signature?
          make-sign-state
          make-verify-state
          sign-state?
          verify-state?
          sign-update-bytevector!
          sign-update-string!
          verify-update-bytevector!
          verify-update-string!
          sign-finalize!
          verify-finalize?
          sign-destroy!
          verify-destroy!
          call-with-sign-state
          call-with-verify-state)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private)
          (cspp crypto pkey))

  (define signing-algorithms '(ed25519 rsa-pss ecdsa))
  (define check-signing-algorithm
    (lambda (who alg)
      (unless (memq alg signing-algorithms)
        (errorf who "valid signing algorithm is one of ~a" signing-algorithms))))

  (define default-digest
    (lambda (alg)
      (case alg
        [(ed25519) #f]
        [else 'sha256])))

  (define check-signing-digest
    (lambda (who algorithm digest)
      (if (eq? algorithm 'ed25519)
          (when digest (errorf who "Ed25519 requires #f for the external digest"))
          (unless (memq digest '(md5 sha1 sha224 sha256 sha384 sha512 sha512-224 sha512-256
                                sha3-224 sha3-256 sha3-384 sha3-512 blake2b-512 blake2s-256))
            (errorf who "unsupported signing digest: ~s" digest)))))

  (define check-signing-key
    (lambda (who algorithm key-algorithm destroyed?)
      (when destroyed? (errorf who "key is destroyed"))
      (unless (eq? key-algorithm
                   (case algorithm [(rsa-pss) 'rsa] [(ecdsa) 'ec] [else algorithm]))
        (errorf who "key algorithm ~s is incompatible with ~s" key-algorithm algorithm))))

  (define-record-type (sign-state %make-sign-state sign-state?)
    (opaque #t)
    (fields mutex algorithm
            digest
            key
            (mutable chunks)
            (mutable destroyed?)
            (mutable finalized?)))

  (define-record-type (verify-state %make-verify-state verify-state?)
    (opaque #t)
    (fields mutex algorithm
            digest
            key
            (mutable chunks)
            (mutable destroyed?)
            (mutable finalized?)))

  (define concat-chunks
    (lambda (chunks)
      (if (null? chunks)
          (make-bytevector 0 0)
          (apply bytevector-append (reverse chunks)))))

  (define check-sign-state-live
    (lambda (who st)
      (when (sign-state-destroyed? st)
        (errorf who "sign state is destroyed"))
      (when (sign-state-finalized? st)
        (errorf who "sign state is finalized"))))

  (define check-verify-state-live
    (lambda (who st)
      (when (verify-state-destroyed? st)
        (errorf who "verify state is destroyed"))
      (when (verify-state-finalized? st)
        (errorf who "verify state is finalized"))))

  #|proc:sign-message
  The `sign-message` procedure returns a signature bytevector for string or bytevector
  `message`, using algorithm symbol `alg` and the live private `key`. The optional
  `digest` defaults to `sha256` for RSA-PSS/ECDSA and `#f` for Ed25519, which uses no
  external digest and rejects any other value. Other algorithms require an OpenSSL
  digest symbol; unknown symbols raise errors. Strings use UTF-8. Mismatched key
  algorithms raise an error. MD5 and SHA-1 are retained only for legacy interoperability.
  |#
  (define-who sign-message
    (case-lambda
      [(alg key message)
       (sign-message alg (default-digest alg) key message)]
      [(alg digest key message)
       (pcheck ([private-key? key])
               (with-crypto-locks ((crypto-private-key-lock key))
                 (check-signing-algorithm who alg)
                 (check-signing-digest who alg digest)
                 (check-signing-key who alg (crypto-private-key-algorithm key)
                                    (crypto-private-key-destroyed? key))
                 (let* ([bv (string/bytevector->utf8 message)]
                        [ans (ffi-sign-message alg digest
                                               (crypto-private-key-handle key)
                                               bv 0 (bytevector-length bv))])
                   (when (eq? ans #f)
                     (errorf who "failed to sign message using ~s" alg))
                   ans)))]))

  #|proc:verify-signature?
  The `verify-signature?` procedure returns `#t` if bytevector `signature` authenticates
  string or bytevector `message`, using algorithm symbol `alg` and live public `key`.
  The optional `digest` defaults to `sha256` for RSA-PSS/ECDSA and `#f` for Ed25519.
  Ed25519 requires `#f`; other algorithms require a known OpenSSL digest symbol.
  Strings use UTF-8. Invalid signatures return `#f`; unknown digests, mismatched key
  algorithms, and destroyed keys raise errors.
  |#
  (define-who verify-signature?
    (case-lambda
      [(alg key message signature)
       (verify-signature? alg (default-digest alg) key message signature)]
      [(alg digest key message signature)
       (pcheck ([public-key? key] [bytevector? signature])
               (with-crypto-locks ((crypto-public-key-lock key))
                 (check-signing-algorithm who alg)
                 (check-signing-digest who alg digest)
                 (check-signing-key who alg (crypto-public-key-algorithm key)
                                    (crypto-public-key-destroyed? key))
                 (let ([bv (string/bytevector->utf8 message)])
                   (not (fx= 0 (ffi-verify-message alg digest
                                                   (crypto-public-key-handle key)
                                                   bv 0 (bytevector-length bv)
                                                   signature 0 (bytevector-length signature)))))))]))

  #|proc:make-sign-state
  The `make-sign-state` procedure returns a buffered signing state for algorithm symbol
  `alg` and live private `key`. The optional `digest` defaults as in `sign-message`.
  It is validated at construction, including requiring `#f` for Ed25519.
  Mismatched key algorithms raise an error. The caller owns the key and must keep it
  live until finalization; destroying the state does not destroy the key.
  |#
  (define-who make-sign-state
    (case-lambda
      [(alg key)
       (make-sign-state alg (default-digest alg) key)]
      [(alg digest key)
       (pcheck ([private-key? key])
               (with-crypto-locks ((crypto-private-key-lock key))
                 (check-signing-algorithm who alg)
                 (check-signing-digest who alg digest)
                 (check-signing-key who alg (crypto-private-key-algorithm key)
                                    (crypto-private-key-destroyed? key))
                 (%make-sign-state (make-mutex 'sign) alg digest key '() #f #f)))]))

  #|proc:make-verify-state
  The `make-verify-state` procedure returns a buffered verification state for algorithm
  symbol `alg` and live public `key`. The optional `digest` defaults as in
  `verify-signature?` and is validated at construction. Mismatched algorithms raise an
  error. The caller owns the key
  and must keep it live until finalization; destroying the state preserves the key.
  |#
  (define-who make-verify-state
    (case-lambda
      [(alg key)
       (make-verify-state alg (default-digest alg) key)]
      [(alg digest key)
       (pcheck ([public-key? key])
               (with-crypto-locks ((crypto-public-key-lock key))
                 (check-signing-algorithm who alg)
                 (check-signing-digest who alg digest)
                 (check-signing-key who alg (crypto-public-key-algorithm key)
                                    (crypto-public-key-destroyed? key))
                 (%make-verify-state (make-mutex 'verify) alg digest key '() #f #f)))]))

  (define update-state!
    (lambda (setter getter len who st bv start stop)
      (check-slice who len start stop)
      (setter st (cons (subbytevector bv start stop) (getter st)))
      st))

  #|proc:sign-update-bytevector!
The `sign-update-bytevector!` procedure appends a bytevector slice to a sign state.
|#
  (define-who sign-update-bytevector!
    (case-lambda
      [(st bv) (sign-update-bytevector! st bv 0 (bytevector-length bv))]
      [(st bv start) (sign-update-bytevector! st bv start (bytevector-length bv))]
      [(st bv start stop)
       (pcheck ([sign-state? st] [bytevector? bv])
               (with-crypto-mutex (sign-state-mutex st)
                 (check-sign-state-live who st)
                 (update-state! sign-state-chunks-set! sign-state-chunks
                                (bytevector-length bv) who st bv start stop)))]))

  #|proc:sign-update-string!
The `sign-update-string!` procedure appends a string slice to a sign state.
|#
  (define-who sign-update-string!
    (case-lambda
      [(st str) (sign-update-string! st str 0 (string-length str))]
      [(st str start) (sign-update-string! st str start (string-length str))]
      [(st str start stop)
       (pcheck ([sign-state? st] [string? str])
               (with-crypto-mutex (sign-state-mutex st)
                 (check-sign-state-live who st)
                 (check-slice who (string-length str) start stop)
                 (sign-state-chunks-set! st
                                         (cons (string->utf8 (substring str start stop))
                                               (sign-state-chunks st)))
                 st))]))

  #|proc:verify-update-bytevector!
The `verify-update-bytevector!` procedure appends a bytevector slice to a verify state.
|#
  (define-who verify-update-bytevector!
    (case-lambda
      [(st bv) (verify-update-bytevector! st bv 0 (bytevector-length bv))]
      [(st bv start) (verify-update-bytevector! st bv start (bytevector-length bv))]
      [(st bv start stop)
       (pcheck ([verify-state? st] [bytevector? bv])
               (with-crypto-mutex (verify-state-mutex st)
                 (check-verify-state-live who st)
                 (update-state! verify-state-chunks-set! verify-state-chunks
                                (bytevector-length bv) who st bv start stop)))]))

  #|proc:verify-update-string!
The `verify-update-string!` procedure appends a string slice to a verify state.
|#
  (define-who verify-update-string!
    (case-lambda
      [(st str) (verify-update-string! st str 0 (string-length str))]
      [(st str start) (verify-update-string! st str start (string-length str))]
      [(st str start stop)
       (pcheck ([verify-state? st] [string? str])
               (with-crypto-mutex (verify-state-mutex st)
                 (check-verify-state-live who st)
                 (check-slice who (string-length str) start stop)
                 (verify-state-chunks-set! st
                                           (cons (string->utf8 (substring str start stop))
                                                 (verify-state-chunks st)))
                 st))]))

  #|proc:sign-finalize!
The `sign-finalize!` procedure finalizes a sign state and returns the signature.
|#
  (define-who sign-finalize!
    (lambda (st)
      (pcheck ([sign-state? st])
              (with-crypto-mutex (sign-state-mutex st)
                (check-sign-state-live who st)
                (let ([ans (sign-message (sign-state-algorithm st)
                                         (sign-state-digest st)
                                         (sign-state-key st)
                                         (concat-chunks (sign-state-chunks st)))])
                  (sign-state-finalized?-set! st #t)
                  ans)))))

  #|proc:verify-finalize?
The `verify-finalize?` procedure finalizes a verify state and checks `signature`.
|#
  (define-who verify-finalize?
    (lambda (st signature)
      (pcheck ([verify-state? st] [bytevector? signature])
              (with-crypto-mutex (verify-state-mutex st)
                (check-verify-state-live who st)
                (let ([ans (verify-signature? (verify-state-algorithm st)
                                              (verify-state-digest st)
                                              (verify-state-key st)
                                              (concat-chunks (verify-state-chunks st))
                                              signature)])
                  (verify-state-finalized?-set! st #t)
                  ans)))))

  #|proc:sign-destroy!
The `sign-destroy!` procedure releases buffered data owned by a sign state.
|#
  (define-who sign-destroy!
    (lambda (st)
      (pcheck ([sign-state? st])
              (with-crypto-mutex (sign-state-mutex st)
                (unless (sign-state-destroyed? st)
                  (sign-state-chunks-set! st '())
                  (sign-state-destroyed?-set! st #t))
                st))))

  #|proc:verify-destroy!
The `verify-destroy!` procedure releases buffered data owned by a verify state.
|#
  (define-who verify-destroy!
    (lambda (st)
      (pcheck ([verify-state? st])
              (with-crypto-mutex (verify-state-mutex st)
                (unless (verify-state-destroyed? st)
                  (verify-state-chunks-set! st '())
                  (verify-state-destroyed?-set! st #t))
                st))))

  #|proc:call-with-sign-state
The `call-with-sign-state` procedure creates a sign state, passes it to `proc`, and always destroys it afterwards.
|#
  (define-who call-with-sign-state
    (case-lambda
      [(alg key proc)
       (call-with-sign-state alg (default-digest alg) key proc)]
      [(alg digest key proc)
       (pcheck ([procedure? proc])
               (let ([st (make-sign-state alg digest key)])
                 (dynamic-wind void
                               (lambda () (proc st))
                               (lambda () (sign-destroy! st)))))]))

  #|proc:call-with-verify-state
The `call-with-verify-state` procedure creates a verify state, passes it to `proc`, and always destroys it afterwards.
|#
  (define-who call-with-verify-state
    (case-lambda
      [(alg key proc)
       (call-with-verify-state alg (default-digest alg) key proc)]
      [(alg digest key proc)
       (pcheck ([procedure? proc])
               (let ([st (make-verify-state alg digest key)])
                 (dynamic-wind void
                               (lambda () (proc st))
                               (lambda () (verify-destroy! st)))))]))
  )
