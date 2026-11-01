(library (cspp crypto key-agreement)
  (export derive-shared-secret)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private)
          (cspp crypto pkey))

  (define key-agreement-algorithms '(x25519 ecdh))
  (define check-key-agreement-algorithm
    (lambda (who alg)
      (unless (memq alg key-agreement-algorithms)
        (errorf who "valid key agreement algorithm is one of ~a" key-agreement-algorithms))))

  #|proc:derive-shared-secret
  The `derive-shared-secret` procedure returns a shared-secret bytevector using algorithm
  symbol `alg`, live `private-key`, and live `peer-public-key`. Both keys must be X25519
  keys for `x25519`, or EC keys on compatible curves for `ecdh`. Mismatched algorithms,
  destroyed keys, and native derivation failures raise errors.
  |#
  (define-who derive-shared-secret
    (lambda (alg private-key peer-public-key)
      (pcheck ([symbol? alg] [private-key? private-key] [public-key? peer-public-key])
              (with-crypto-locks ((crypto-private-key-lock private-key) (crypto-public-key-lock peer-public-key))
                (check-key-agreement-algorithm who alg)
                (when (or (crypto-private-key-destroyed? private-key)
                          (crypto-public-key-destroyed? peer-public-key))
                  (errorf who "key is destroyed"))
                (let ([expected (if (eq? alg 'ecdh) 'ec 'x25519)])
                  (unless (and (eq? expected (private-key-algorithm private-key))
                               (eq? expected (public-key-algorithm peer-public-key)))
                    (errorf who "both keys must have algorithm ~s for ~s" expected alg)))
                (let ([ans (ffi-derive-shared-secret alg
                                                     (crypto-private-key-handle private-key)
                                                     (crypto-public-key-handle peer-public-key))])
                  (when (eq? ans #f)
                    (errorf who "failed to derive shared secret using ~s" alg))
                  ans)))))
  )
