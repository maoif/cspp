(library (cspp crypto password)
  (export password-hash
          password-hash/scrypt
          password-hash/pbkdf2
          password-hash?
          password-verify?)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto random)
          (cspp crypto kdf)
          (cspp crypto constant-time)
          (cspp crypto private))

  (define password-magic (bytevector #x43 #x50 #x57 #x31))
  (define algo-scrypt 1)
  (define algo-pbkdf2 2)

  (define normalize-password
    (lambda (who password)
      (pcase password
        [bytevector? password]
        [string? (string->utf8 password)]
        [else (errorf who "expected string or bytevector, given ~s" password)])))

  (define make-password-record
    (lambda (algo p1 p2 p3 salt derived)
      (let ([hdr (make-bytevector 26 0)])
        (bytevector-copy! password-magic 0 hdr 0 4)
        (bytevector-u8-set! hdr 4 1)
        (bytevector-u8-set! hdr 5 algo)
        (u32-set! hdr 6 p1)
        (u32-set! hdr 10 p2)
        (u32-set! hdr 14 p3)
        (u32-set! hdr 18 (bytevector-length salt))
        (u32-set! hdr 22 (bytevector-length derived))
        (bytevector-append hdr salt derived))))

  (define parse-password-record
    (lambda (who bv)
      (pcheck ([bytevector? bv])
              (and (fx>= (bytevector-length bv) 26)
                   (bytevector-prefix? password-magic bv)
                   (fx= 1 (bytevector-u8-ref bv 4))
                   (let* ([algo (bytevector-u8-ref bv 5)]
                          [p1 (u32-ref bv 6)]
                          [p2 (u32-ref bv 10)]
                          [p3 (u32-ref bv 14)]
                          [salt-len (u32-ref bv 18)]
                          [derived-len (u32-ref bv 22)]
                          [expect (fx+ 26 salt-len derived-len)])
                     (and (fx> derived-len 0)
                          (fx= expect (bytevector-length bv))
                          (vector algo p1 p2 p3
                                  (subbytevector bv 26 (fx+ 26 salt-len))
                                  (subbytevector bv (fx+ 26 salt-len) expect))))))))

  #|proc:password-hash/scrypt
  The `password-hash/scrypt` procedure returns an encoded scrypt hash of the string or
  bytevector `password`, using bytevector `salt` and cost parameters `n`, `r`, and `p`.
  `outlen` is the positive derived-key length in bytes; zero-length hashes raise an error.
  |#
  (define-who password-hash/scrypt
    (lambda (password salt n r p outlen)
      (pcheck ([bytevector? salt] [natural? n r p outlen])
              (when (zero? outlen) (errorf who "derived-key length must be positive"))
              (let* ([pw (normalize-password who password)]
                     [derived (scrypt pw salt n r p outlen)])
                (make-password-record algo-scrypt n r p salt derived)))))

  #|proc:password-hash/pbkdf2
  The `password-hash/pbkdf2` procedure returns an encoded PBKDF2-SHA256 hash of the string
  or bytevector `password`, using bytevector `salt` and the count `iterations`.
  `outlen` is the positive derived-key length in bytes; zero-length hashes raise an error.
  |#
  (define-who password-hash/pbkdf2
    (lambda (password salt iterations outlen)
      (pcheck ([bytevector? salt] [natural? iterations outlen])
              (when (zero? outlen) (errorf who "derived-key length must be positive"))
              (let* ([pw (normalize-password who password)]
                     [derived (pbkdf2 'sha256 pw salt iterations outlen)])
                (make-password-record algo-pbkdf2 iterations 0 0 salt derived)))))

  #|proc:password-hash
The `password-hash` procedure derives and encodes a default scrypt password hash as a bytevector.
|#
  (define-who password-hash
    (lambda (password)
      (let ([salt (random-bytes 16)])
        (password-hash/scrypt password salt 16384 8 1 32))))

  #|proc:password-hash?
  The `password-hash?` procedure returns `#t` when `x` has the structure of an encoded
  password hash containing a nonempty derived key, and `#f` otherwise.
  |#
  (define password-hash?
    (lambda (x)
      (and (bytevector? x)
           (parse-password-record 'password-hash? x)
           #t)))

  #|proc:password-verify?
  The `password-verify?` procedure returns `#t` when the string or bytevector `password`
  matches the encoded password hash bytevector `encoded`. Malformed records, including
  empty derived keys, return `#f`; native derivation failures raise an error.
  |#
  (define-who password-verify?
    (lambda (password encoded)
      (pcheck ([bytevector? encoded])
              (let ([rec (parse-password-record who encoded)])
                (and rec
                     (let* ([algo (vector-ref rec 0)]
                            [p1 (vector-ref rec 1)]
                            [p2 (vector-ref rec 2)]
                            [p3 (vector-ref rec 3)]
                            [salt (vector-ref rec 4)]
                            [derived (vector-ref rec 5)]
                            [candidate
                             (case algo
                               [(1) (password-hash/scrypt password salt p1 p2 p3
                                                          (bytevector-length derived))]
                               [(2) (password-hash/pbkdf2 password salt p1
                                                          (bytevector-length derived))]
                               [else #f])])
                       (and candidate
                            (constant-time-bytevector=? candidate encoded))))))))
  )
