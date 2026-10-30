(library (cspp crypto pkey)
  (export generate-private-key
          private-key?
          public-key?
          private-key->public-key
          private-key-algorithm
          public-key-algorithm
          private-key-bits
          public-key-bits
          private-key=?
          public-key=?
          load-private-key
          load-public-key
          store-private-key
          store-public-key
          pem->private-key
          pem->public-key
          der->private-key
          der->public-key
          destroy-private-key!
          destroy-public-key!)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  (define asymmetric-algorithms '(rsa ecdsa ecdh ed25519 x25519))
  (define check-algorithm
    (lambda (who alg)
      (unless (memq alg asymmetric-algorithms)
        (errorf who "valid key algorithm is one of ~a" asymmetric-algorithms))))

  (define formats '(pem der))
  (define check-format
    (lambda (who fmt)
      (unless (memq fmt formats)
        (errorf who "valid key format is one of ~a" formats))))

  (define private-key? crypto-private-key?)
  (define public-key? crypto-public-key?)
  (define private-key-algorithm crypto-private-key-algorithm)
  (define public-key-algorithm crypto-public-key-algorithm)
  (define private-key-bits crypto-private-key-bits)
  (define public-key-bits crypto-public-key-bits)

  (define check-private-key-live
    (lambda (who key)
      (when (crypto-private-key-destroyed? key)
        (errorf who "private key is destroyed"))))

  (define check-public-key-live
    (lambda (who key)
      (when (crypto-public-key-destroyed? key)
        (errorf who "public key is destroyed"))))

  (define raw->private-key
    (lambda (who handle)
      (with-crypto-operation
        (let ([alg (ffi-pkey-algorithm handle)]
              [bits (ffi-pkey-bits handle)])
          (when (eq? alg #f)
            (ffi-pkey-free handle)
            (errorf who "unsupported private key"))
          (make-crypto-private-key handle alg bits #f)))))

  (define raw->public-key
    (lambda (who handle)
      (with-crypto-operation
        (let ([alg (ffi-pkey-algorithm handle)]
              [bits (ffi-pkey-bits handle)])
          (when (eq? alg #f)
            (ffi-pkey-free handle)
            (errorf who "unsupported public key"))
          (make-crypto-public-key handle alg bits #f)))))

  #|proc:generate-private-key
  The `generate-private-key` procedure returns an owned private key for symbol `alg`.
  Optional `param` is an exact positive signed-C-int-sized bit count for RSA (default
  2048), an EC curve symbol for ECDSA/ECDH (default `p-256`), or `#f` for Ed25519/X25519.
  Invalid parameters and native failures raise errors. Release the result with
  `destroy-private-key!` when finished.
  |#
  (define-who generate-private-key
    (case-lambda
      [(alg)
       (pcheck ([symbol? alg])
         (check-algorithm who alg)
         (generate-private-key alg
                               (case alg
                                 [(rsa) 2048]
                                 [(ecdsa ecdh) 'p-256]
                                 [else #f])))]
      [(alg param)
       (pcheck ([symbol? alg])
         (check-algorithm who alg)
         (let ([handle
                (cond
                 [(eq? alg 'rsa)
                  (unless (and (natural? param) (exact? param) (positive? param))
                    (errorf who "RSA bit count must be an exact positive integer, given ~s" param))
                  (ffi-pkey-generate alg param #f)]
                 [(memq alg '(ecdsa ecdh))
                  (unless (symbol? param)
                    (errorf who "EC curve must be a symbol, given ~s" param))
                  (ffi-pkey-generate alg 0 param)]
                 [else
                  (when param
                    (errorf who "algorithm ~s does not accept a generation parameter" alg))
                  (ffi-pkey-generate alg 0 #f)])])
           (when (eq? handle 0)
             (errorf who "failed to generate private key for ~s" alg))
           (raw->private-key who handle)))]))

  #|proc:destroy-private-key!
The `destroy-private-key!` procedure releases the foreign resources owned by `key`.
|#
  (define-who destroy-private-key!
    (lambda (key)
      (pcheck ([private-key? key])
              (with-crypto-locks ((crypto-private-key-lock key))
                (unless (crypto-private-key-destroyed? key)
                  (ffi-pkey-free (crypto-private-key-handle key))
                  (crypto-private-key-handle-set! key 0)
                  (crypto-private-key-destroyed?-set! key #t))
                key))))

  #|proc:destroy-public-key!
The `destroy-public-key!` procedure releases the foreign resources owned by `key`.
|#
  (define-who destroy-public-key!
    (lambda (key)
      (pcheck ([public-key? key])
              (with-crypto-locks ((crypto-public-key-lock key))
                (unless (crypto-public-key-destroyed? key)
                  (ffi-pkey-free (crypto-public-key-handle key))
                  (crypto-public-key-handle-set! key 0)
                  (crypto-public-key-destroyed?-set! key #t))
                key))))

  #|proc:private-key->public-key
The `private-key->public-key` procedure derives the corresponding public key.
|#
  (define-who private-key->public-key
    (lambda (key)
      (pcheck ([private-key? key])
              (with-crypto-locks ((crypto-private-key-lock key))
                (check-private-key-live who key)
                (let ([handle (ffi-pkey-public-from-private (crypto-private-key-handle key))])
                  (when (eq? handle 0)
                    (errorf who "failed to derive public key"))
                  (raw->public-key who handle))))))

  (define store-dispatch
    (lambda (who kind key fmt)
      (check-format who fmt)
      (case kind
        [(private)
         (case fmt
           [(pem) (ffi-pkey-store-private-pem (crypto-private-key-handle key))]
           [(der) (ffi-pkey-store-private-der (crypto-private-key-handle key))]
           [else (unreachable!)])]
        [(public)
         (case fmt
           [(pem) (ffi-pkey-store-public-pem (crypto-public-key-handle key))]
           [(der) (ffi-pkey-store-public-der (crypto-public-key-handle key))]
           [else (unreachable!)])]
        [else (unreachable!)])))

  #|proc:store-private-key
The `store-private-key` procedure serializes `key` to PEM or DER as a bytevector.
|#
  (define-who store-private-key
    (case-lambda
      [(key) (store-private-key key 'pem)]
      [(key fmt)
       (pcheck ([private-key? key])
               (with-crypto-locks ((crypto-private-key-lock key))
                 (check-private-key-live who key)
                 (let ([ans (store-dispatch who 'private key fmt)])
                   (when (eq? ans #f)
                     (errorf who "failed to store private key"))
                   ans)))]))

  #|proc:store-public-key
The `store-public-key` procedure serializes `key` to PEM or DER as a bytevector.
|#
  (define-who store-public-key
    (case-lambda
      [(key) (store-public-key key 'pem)]
      [(key fmt)
       (pcheck ([public-key? key])
               (with-crypto-locks ((crypto-public-key-lock key))
                 (check-public-key-live who key)
                 (let ([ans (store-dispatch who 'public key fmt)])
                   (when (eq? ans #f)
                     (errorf who "failed to store public key"))
                   ans)))]))

  (define load-dispatch
    (lambda (who kind bv start stop fmt)
      (check-format who fmt)
      (case kind
        [(private)
         (case fmt
           [(pem) (ffi-pkey-load-private-pem bv start stop)]
           [(der) (ffi-pkey-load-private-der bv start stop)]
           [else (unreachable!)])]
        [(public)
         (case fmt
           [(pem) (ffi-pkey-load-public-pem bv start stop)]
           [(der) (ffi-pkey-load-public-der bv start stop)]
           [else (unreachable!)])]
        [else (unreachable!)])))

  #|proc:load-private-key
  The `load-private-key` procedure returns an owned private key loaded from bytevector
  `bv`. Optional format symbol `fmt` is `pem` (default) or `der`; optional nonnegative
  fixnums `start` and `stop` select a half-open byte slice, defaulting to the whole input.
  Encrypted PEM is unsupported and raises an error without prompting for a password.
  Invalid input, unsupported keys, or native failures raise errors. Release the result
  with `destroy-private-key!`.
  |#
  (define-who load-private-key
    (case-lambda
      [(bv) (load-private-key bv 'pem 0 (bytevector-length bv))]
      [(bv fmt)
       (pcheck ([bytevector? bv])
               (load-private-key bv fmt 0 (bytevector-length bv)))]
      [(bv fmt start)
       (pcheck ([bytevector? bv])
               (load-private-key bv fmt start (bytevector-length bv)))]
      [(bv fmt start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (let ([handle (load-dispatch who 'private bv start stop fmt)])
                 (when (eq? handle 0)
                   (errorf who "failed to load private key"))
                 (raw->private-key who handle)))]))

  #|proc:load-public-key
The `load-public-key` procedure loads a public key from a PEM or DER bytevector slice.
|#
  (define-who load-public-key
    (case-lambda
      [(bv) (load-public-key bv 'pem 0 (bytevector-length bv))]
      [(bv fmt)
       (pcheck ([bytevector? bv])
               (load-public-key bv fmt 0 (bytevector-length bv)))]
      [(bv fmt start)
       (pcheck ([bytevector? bv])
               (load-public-key bv fmt start (bytevector-length bv)))]
      [(bv fmt start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (let ([handle (load-dispatch who 'public bv start stop fmt)])
                 (when (eq? handle 0)
                   (errorf who "failed to load public key"))
                 (raw->public-key who handle)))]))

  #|proc:pem->private-key
  The `pem->private-key` procedure returns an owned private key loaded from PEM bytevector
  `bv`, as in `load-private-key`. Encrypted PEM raises an error without password prompts.
  Release the result with `destroy-private-key!`.
  |#
  (define pem->private-key
    (lambda (bv)
      (load-private-key bv 'pem)))

  #|proc:pem->public-key
The `pem->public-key` procedure loads a PEM public key from `bv`.
|#
  (define pem->public-key
    (lambda (bv)
      (load-public-key bv 'pem)))

  #|proc:der->private-key
The `der->private-key` procedure loads a DER private key from `bv`.
|#
  (define der->private-key
    (lambda (bv)
      (load-private-key bv 'der)))

  #|proc:der->public-key
The `der->public-key` procedure loads a DER public key from `bv`.
|#
  (define der->public-key
    (lambda (bv)
      (load-public-key bv 'der)))

  #|proc:private-key=?
The `private-key=?` procedure compares two private keys by DER serialization.
|#
  (define-who private-key=?
    (lambda (a b)
      (pcheck ([private-key? a b])
              (with-crypto-locks ((crypto-private-key-lock a) (crypto-private-key-lock b))
                (equal? (store-private-key a 'der)
                        (store-private-key b 'der))))))

  #|proc:public-key=?
The `public-key=?` procedure compares two public keys by DER serialization.
|#
  (define-who public-key=?
    (lambda (a b)
      (pcheck ([public-key? a b])
              (with-crypto-locks ((crypto-public-key-lock a) (crypto-public-key-lock b))
                (equal? (store-public-key a 'der)
                        (store-public-key b 'der))))))
  )
