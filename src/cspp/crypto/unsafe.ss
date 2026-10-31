(library (cspp crypto unsafe)
  (export unsafe-hash-algorithms
          unsafe-hash
          unsafe-hash-bytevector
          unsafe-hash-string)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  (define unsafe-hash-algorithms '(md5 sha1))

  (define check-unsafe-hash
    (lambda (who alg)
      (unless (memq alg unsafe-hash-algorithms)
        (errorf who "valid unsafe hash algorithm is one of ~a" unsafe-hash-algorithms))))

  #|proc:unsafe-hash-bytevector
  The `unsafe-hash-bytevector` procedure returns a digest bytevector for `bv` using the
  legacy algorithm symbol `alg` (`md5` or `sha1`). Optional `start` and `stop` select the
  half-open byte slice, defaulting to zero and the bytevector length. Invalid inputs or
  unavailable native algorithms raise errors. These algorithms are unsuitable for secure hashing.
  |#
  (define-who unsafe-hash-bytevector
    (case-lambda
      [(alg bv)
       (pcheck ([bytevector? bv])
         (unsafe-hash-bytevector alg bv 0 (bytevector-length bv)))]
      [(alg bv start)
       (pcheck ([bytevector? bv])
         (unsafe-hash-bytevector alg bv start (bytevector-length bv)))]
      [(alg bv start stop)
       (pcheck ([bytevector? bv])
               (check-unsafe-hash who alg)
               (check-slice who (bytevector-length bv) start stop)
               (let ([ans (ffi-digest-bytevector alg bv start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash bytevector using ~s" alg))
                 ans))]))

  #|proc:unsafe-hash-string
  The `unsafe-hash-string` procedure returns a digest bytevector for string `str` using
  the legacy algorithm symbol `alg` (`md5` or `sha1`). Each character uses native-endian
  UTF-32 without a byte-order mark. Optional `start` and `stop` select the half-open
  character slice, defaulting to zero and the string length. Invalid inputs or unavailable
  native algorithms raise errors. These algorithms are unsuitable for secure hashing.
  |#
  (define-who unsafe-hash-string
    (case-lambda
      [(alg str)
       (pcheck ([string? str])
         (unsafe-hash-string alg str 0 (string-length str)))]
      [(alg str start)
       (pcheck ([string? str])
         (unsafe-hash-string alg str start (string-length str)))]
      [(alg str start stop)
       (pcheck ([string? str])
               (check-unsafe-hash who alg)
               (check-slice who (string-length str) start stop)
               (let ([ans (ffi-digest-string alg str start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash string using ~s" alg))
                 ans))]))

  #|proc:unsafe-hash
  The `unsafe-hash` procedure returns the legacy digest bytevector of string or bytevector
  `x` using the algorithm symbol `alg` (`md5` or `sha1`). Strings use native-endian UTF-32
  without a byte-order mark. Invalid inputs and native backend failures raise errors.
  These algorithms are unsuitable for security-sensitive hashing.
  |#
  (define-who unsafe-hash
    (lambda (alg x)
      (pcase x
        [bytevector? (unsafe-hash-bytevector alg x)]
        [string? (unsafe-hash-string alg x)]
        [else (errorf who "expected string or bytevector, given ~s" x)])))
  )
