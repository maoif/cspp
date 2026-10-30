(library (cspp crypto random)
  (export random-bytes
          random-key-bytes
          random-nonce
          random-bytes!
          random-status)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  #|proc:random-status
The `random-status` procedure returns `#t` when the OpenSSL random source is ready.
|#
  (define random-status
    (lambda ()
      (ensure-openssl 'random-status)
      (not (fx= 0 (ffi-random-status)))))

  #|proc:random-bytes
  The `random-bytes` procedure returns a fresh random bytevector of length `len`.
  `len` must be a nonnegative fixnum; inexact or unrepresentable lengths raise an error
  before native allocation. Native random-source failures also raise errors.
  |#
  (define-who random-bytes
    (lambda (len)
      (pcheck ([(lambda (length) (and (fixnum? length) (fx>= length 0))) len])
              (ensure-openssl who)
              (let ([bv (ffi-random-bytevector len)])
                (when (eq? bv #f)
                  (errorf who "failed to obtain random bytes"))
                bv))))

  #|proc:random-key-bytes
  The `random-key-bytes` procedure returns a fresh random key bytevector of length `len`,
  a nonnegative fixnum. It has the same validation and failure behavior as `random-bytes`.
  |#
  (define random-key-bytes
    random-bytes)

  #|proc:random-nonce
  The `random-nonce` procedure returns a fresh random nonce bytevector of length `len`,
  a nonnegative fixnum. It has the same validation and failure behavior as `random-bytes`.
  |#
  (define random-nonce
    random-bytes)

  #|proc:random-bytes!
The `random-bytes!` procedure fills a bytevector slice with random bytes.
|#
  (define-who random-bytes!
    (case-lambda
      [(bv)
       (pcheck ([bytevector? bv])
               (random-bytes! bv 0 (bytevector-length bv)))]
      [(bv start)
       (pcheck ([bytevector? bv])
               (random-bytes! bv start (bytevector-length bv)))]
      [(bv start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (ensure-openssl who)
               (when (fx= 0 (ffi-random-fill! bv start stop))
                 (errorf who "failed to fill bytevector with random bytes"))
               bv)]))
  )
