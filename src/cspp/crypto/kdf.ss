(library (cspp crypto kdf)
  (export kdf-algorithms
          hkdf
          hkdf-extract
          hkdf-expand
          pbkdf2
          scrypt)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  (define kdf-algorithms
    '(hkdf pbkdf2 scrypt))

  (define digest-algorithms
    '(sha224 sha256 sha384 sha512 sha512-224 sha512-256
      sha3-224 sha3-256 sha3-384 sha3-512
      blake2b-512 blake2s-256))

  (define check-digest-algorithm
    (lambda (who which)
      (unless (memq which digest-algorithms)
        (errorf who "valid digest algorithm is one of ~a" digest-algorithms))))

  (define normalized-bytevector
    (lambda (who x)
      (pcase x
        [bytevector? x]
        [string? (string->utf8 x)]
        [else (errorf who "expected string or bytevector, given ~s" x)])))

  #|proc:hkdf
  The `hkdf` procedure returns `len` derived bytes using digest symbol `which`, input
  key material `ikm`, `salt`, and context `info`. Each input may be a bytevector or a
  string, with strings encoded as UTF-8. `len` must be an exact nonnegative integer
  fitting a signed C int. Invalid inputs and native derivation failures raise errors.
  |#
  (define-who hkdf
    (lambda (which ikm salt info len)
      (pcheck ([symbol? which] [natural? len])
        (check-digest-algorithm who which)
        (let* ([ikm-bv (normalized-bytevector who ikm)]
               [salt-bv (normalized-bytevector who salt)]
               [info-bv (normalized-bytevector who info)]
               [ans (ffi-hkdf which
                              ikm-bv 0 (bytevector-length ikm-bv)
                              salt-bv 0 (bytevector-length salt-bv)
                              info-bv 0 (bytevector-length info-bv)
                              len)])
          (when (eq? ans #f)
            (errorf who "HKDF failed for ~s" which))
          ans))))

  #|proc:hkdf-extract
  The `hkdf-extract` procedure returns a pseudorandom-key bytevector using digest symbol
  `which`, input key material `ikm`, and `salt`. Both inputs may be bytevectors or strings;
  strings use UTF-8. Invalid inputs and native derivation failures raise errors.
  |#
  (define-who hkdf-extract
    (lambda (which ikm salt)
      (pcheck ([symbol? which])
        (check-digest-algorithm who which)
        (let* ([ikm-bv (normalized-bytevector who ikm)]
               [salt-bv (normalized-bytevector who salt)]
               [ans (ffi-hkdf-extract which
                                      ikm-bv 0 (bytevector-length ikm-bv)
                                      salt-bv 0 (bytevector-length salt-bv))])
          (when (eq? ans #f)
            (errorf who "HKDF extract failed for ~s" which))
          ans))))

  #|proc:hkdf-expand
  The `hkdf-expand` procedure returns `len` bytes using digest symbol `which`,
  pseudorandom key `prk`, and context `info`. Both inputs may be bytevectors or strings;
  strings use UTF-8. `len` must be an exact nonnegative integer fitting a signed C int.
  Invalid inputs and native derivation failures raise errors.
  |#
  (define-who hkdf-expand
    (lambda (which prk info len)
      (pcheck ([symbol? which] [natural? len])
        (check-digest-algorithm who which)
        (let* ([prk-bv (normalized-bytevector who prk)]
               [info-bv (normalized-bytevector who info)]
               [ans (ffi-hkdf-expand which
                                     prk-bv 0 (bytevector-length prk-bv)
                                     info-bv 0 (bytevector-length info-bv)
                                     len)])
          (when (eq? ans #f)
            (errorf who "HKDF expand failed for ~s" which))
          ans))))

  #|proc:pbkdf2
  The `pbkdf2` procedure returns `len` derived bytes using digest symbol `which`,
  `password`, and `salt`. Both inputs may be bytevectors or strings; strings use UTF-8.
  `iterations` must be a positive exact integer and `len` a nonnegative exact integer,
  both fitting a signed C int. Invalid inputs and native derivation failures raise errors.
  |#
  (define-who pbkdf2
    (lambda (which password salt iterations len)
      (pcheck ([symbol? which] [natural? iterations len])
              (check-digest-algorithm who which)
              (let* ([password-bv (normalized-bytevector who password)]
                     [salt-bv (normalized-bytevector who salt)]
                     [ans (ffi-pbkdf2 which
                                      password-bv 0 (bytevector-length password-bv)
                                      salt-bv 0 (bytevector-length salt-bv)
                                      iterations len)])
                (when (eq? ans #f)
                  (errorf who "PBKDF2 failed for ~s" which))
                ans))))

  #|proc:scrypt
  The `scrypt` procedure returns `len` derived bytes from `password` and `salt`, each a
  bytevector or a string encoded as UTF-8. Cost `n` must be a power of two greater than
  one; block size `r` and parallelization `p` must be positive. All four numeric
  parameters must be exact integers fitting a signed C int, with `len` nonnegative.
  Invalid inputs and native derivation failures raise errors.
  |#
  (define-who scrypt
    (lambda (password salt n r p len)
      (pcheck ([natural? n r p len])
              (ensure-openssl who)
              (let* ([password-bv (normalized-bytevector who password)]
                     [salt-bv (normalized-bytevector who salt)]
                     [ans (ffi-scrypt password-bv 0 (bytevector-length password-bv)
                                      salt-bv 0 (bytevector-length salt-bv)
                                      n r p len)])
                (when (eq? ans #f)
                  (errorf who "scrypt failed"))
                ans))))
  )
