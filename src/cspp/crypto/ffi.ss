(library (cspp crypto ffi)
  (export ffi-blake3-bytevector
          ffi-blake3-string
          ffi-blake3-create
          ffi-blake3-destroy
          ffi-blake3-get
          ffi-blake3-reset!
          ffi-blake3-update-bytevector!
          ffi-blake3-update-string!
          ffi-openssl-load-error
          ffi-random-status
          ffi-random-bytevector
          ffi-random-fill!
          ffi-constant-time-eq
          ffi-digest-bytevector
          ffi-digest-string
          ffi-digest-output-size
          ffi-digest-block-size
          ffi-digest-state-create
          ffi-digest-state-destroy
          ffi-digest-state-get
          ffi-digest-state-reset!
          ffi-digest-state-update-bytevector!
          ffi-digest-state-update-string!
          ffi-hmac-state-create
          ffi-hmac-state-destroy
          ffi-hmac-state-get
          ffi-hmac-state-finalize
          ffi-hmac-state-reset!
          ffi-hmac-state-update-bytevector!
          ffi-hmac-state-update-string!
          ffi-hkdf
          ffi-hkdf-extract
          ffi-hkdf-expand
          ffi-pbkdf2
          ffi-scrypt
          ffi-aead-encrypt
          ffi-aead-decrypt
          ffi-cipher-key-size
          ffi-cipher-iv-size
          ffi-cipher-block-size
          ffi-cipher-state-create
          ffi-cipher-state-destroy
          ffi-cipher-state-update
          ffi-cipher-state-finalize
          ffi-cipher-state-reset!
          ffi-pkey-generate
          ffi-pkey-free
          ffi-pkey-algorithm
          ffi-pkey-bits
          ffi-pkey-public-from-private
          ffi-pkey-store-private-pem
          ffi-pkey-store-public-pem
          ffi-pkey-store-private-der
          ffi-pkey-store-public-der
          ffi-pkey-load-private-pem
          ffi-pkey-load-public-pem
          ffi-pkey-load-private-der
          ffi-pkey-load-public-der
          ffi-sign-message
          ffi-verify-message
          ffi-derive-shared-secret
          ffi-cert-load-pem
          ffi-cert-load-der
          ffi-cert-free
          ffi-cert-subject
          ffi-cert-issuer
          ffi-cert-not-before
          ffi-cert-not-after
          ffi-cert-subject-alt-names
          ffi-cert-hostname-matches
          ffi-cert-public-key-der
          ffi-cert-serial-number
          ffi-cert-fingerprint
          ffi-cert-store-create
          ffi-cert-store-destroy
          ffi-cert-store-add
          ffi-cert-store-load-defaults
          ffi-cert-verify-state-create
          ffi-cert-verify-state-add-chain-cert
          ffi-cert-verify-state-verify
          ffi-cert-verify-state-destroy)
  (import (cspp utils) (cspp optional-library-check) (cspp chez))

  (define ffi-openssl-load-error
    (foreign-procedure "crypto_openssl_load_error" () ptr))
  #|proc:ffi-random-status
  The `ffi-random-status` procedure calls the native openssl operation `crypto_random_status`.
  It returns 1 when the random generator is ready and 0 otherwise.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-random-status
    (let ([native (foreign-procedure "crypto_random_status" () int)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-random-status 'openssl)
                (native )))))
  (define ffi-random-bytevector (foreign-procedure "crypto_random_bytevector" (unsigned-64) ptr))
  #|proc:ffi-random-fill!
  The `ffi-random-fill!` procedure calls the native openssl operation `crypto_random_fill`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the mutable bytevector whose slice receives random bytes.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-random-fill!
    (let ([native (foreign-procedure "crypto_random_fill" (ptr unsigned-64 unsigned-64) int)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-random-fill! 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-constant-time-eq
  The `ffi-constant-time-eq` procedure calls the native openssl operation
  `crypto_constant_time_eq`.
  Parameters `bv1`, `start1`, `stop1`, `bv2`, `start2`, `stop2` are passed to the native operation
  in that order.
  `bv1` is the first bytevector to compare.
  `start1` is the inclusive start index of its bytevector slice.
  `stop1` is the exclusive end index of its bytevector slice.
  `bv2` is the second bytevector to compare.
  `start2` is the inclusive start index of its bytevector slice.
  `stop2` is the exclusive end index of its bytevector slice.
  It returns 1 for equal bytevector slices and 0 for unequal slices or lengths.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-constant-time-eq
    (let ([native (foreign-procedure "crypto_constant_time_eq" (ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64) int)])
      (lambda (bv1 start1 stop1 bv2 start2 stop2)
        (pcheck ([bytevector? bv1] [bytevector? bv2] [natural? start1] [natural? stop1] [natural? start2] [natural? stop2])
                (require-optional-library 'ffi-constant-time-eq 'openssl)
                (native bv1 start1 stop1 bv2 start2 stop2)))))

  #|proc:ffi-digest-bytevector
  The `ffi-digest-bytevector` procedure returns the OpenSSL digest of bytevector `bytes`
  using the symbol `algorithm` and byte indices `start` (inclusive) and `stop` (exclusive).
  The caller validates slice bounds. It returns `#f` on a native algorithm or allocation
  failure and raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-bytevector
    (let ([native (foreign-procedure "crypto_digest_bytevector"
                                    (ptr ptr unsigned-64 unsigned-64) ptr)])
      (lambda (algorithm bytes start stop)
        (pcheck ([symbol? algorithm] [bytevector? bytes] [natural? start stop])
          (require-optional-library 'ffi-digest-bytevector 'openssl)
          (native algorithm bytes start stop)))))

  #|proc:ffi-digest-string
  The `ffi-digest-string` procedure returns the OpenSSL digest of string `text` using the
  symbol `algorithm` and character indices `start` (inclusive) and `stop` (exclusive).
  Characters use native-endian UTF-32 without a byte-order mark. The caller validates
  slice bounds. It returns `#f` on a native algorithm or allocation failure and raises
  an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-string
    (let ([native (foreign-procedure "crypto_digest_string"
                                    (ptr ptr unsigned-64 unsigned-64) ptr)])
      (lambda (algorithm text start stop)
        (pcheck ([symbol? algorithm] [string? text] [natural? start stop])
          (require-optional-library 'ffi-digest-string 'openssl)
          (native algorithm text start stop)))))
  #|proc:ffi-digest-output-size
  The `ffi-digest-output-size` procedure calls the native openssl operation
  `crypto_digest_output_size`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the digest algorithm symbol, such as `sha256`.
  It returns the requested size in bytes, or -1 when the algorithm is unsupported.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-output-size
    (let ([native (foreign-procedure "crypto_digest_output_size" (ptr) int)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-digest-output-size 'openssl)
                (native algorithm)))))
  #|proc:ffi-digest-block-size
  The `ffi-digest-block-size` procedure calls the native OpenSSL operation
  `crypto_digest_block_size`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the digest algorithm symbol, such as `sha256`.
  It returns the requested size in bytes, or -1 when the algorithm is unsupported.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-block-size
    (let ([native (foreign-procedure "crypto_digest_block_size" (ptr) int)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-digest-block-size 'openssl)
                (native algorithm)))))
  #|proc:ffi-digest-state-create
  The `ffi-digest-state-create` procedure calls the native openssl operation
  `crypto_digest_state_create`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the digest algorithm symbol, such as `sha256`.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-create
    (let ([native (foreign-procedure "crypto_digest_state_create" (ptr) void*)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-digest-state-create 'openssl)
                (native algorithm)))))
  #|proc:ffi-digest-state-destroy
  The `ffi-digest-state-destroy` procedure calls the native openssl operation
  `crypto_digest_state_destroy`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-destroy
    (let ([native (foreign-procedure "crypto_digest_state_destroy" (void*) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-digest-state-destroy 'openssl)
                (native handle)))))
  #|proc:ffi-digest-state-get
  The `ffi-digest-state-get` procedure returns the digest bytevector for native OpenSSL
  context `handle`, preserving its state. It returns `#f` on a zero handle or native
  failure and raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-get
    (let ([native (foreign-procedure "crypto_digest_state_get" (void*) ptr)])
      (lambda (handle)
        (pcheck ([natural? handle])
          (require-optional-library 'ffi-digest-state-get 'openssl)
          (native handle)))))
  #|proc:ffi-digest-state-reset!
  The `ffi-digest-state-reset!` procedure calls the native openssl operation
  `crypto_digest_state_reset`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-reset!
    (let ([native (foreign-procedure "crypto_digest_state_reset" (void*) int)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-digest-state-reset! 'openssl)
                (native handle)))))
  #|proc:ffi-digest-state-update-bytevector!
  The `ffi-digest-state-update-bytevector!` procedure calls the native openssl operation
  `crypto_digest_state_update_bytevector`.
  Parameters `handle`, `bytevector`, `start`, `stop` are passed to the native operation in that
  order.
  `handle` is a native handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-update-bytevector!
    (let ([native (foreign-procedure "crypto_digest_state_update_bytevector" (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (handle bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? handle] [natural? start] [natural? stop])
                (require-optional-library 'ffi-digest-state-update-bytevector! 'openssl)
                (native handle bytevector start stop)))))
  #|proc:ffi-digest-state-update-string!
  The `ffi-digest-state-update-string!` procedure calls the native openssl operation
  `crypto_digest_state_update_string`.
  Parameters `handle`, `text`, `start`, `stop` are passed to the native operation in that order.
  `handle` is a native handle.
  `text` is the input string; characters are hashed as native-endian UTF-32 values.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-digest-state-update-string!
    (let ([native (foreign-procedure "crypto_digest_state_update_string" (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (handle text start stop)
        (pcheck ([string? text] [natural? handle] [natural? start] [natural? stop])
                (require-optional-library 'ffi-digest-state-update-string! 'openssl)
                (native handle text start stop)))))

  #|proc:ffi-hmac-state-create
  The `ffi-hmac-state-create` procedure calls the native openssl operation
  `crypto_hmac_state_create`.
  Parameters `algorithm`, `key`, `start`, `stop` are passed to the native operation in that order.
  `algorithm` is the digest algorithm symbol, such as `sha256`.
  `key` is the secret key bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-hmac-state-create
    (let ([native (foreign-procedure "crypto_hmac_state_create" (ptr ptr unsigned-64 unsigned-64) void*)])
      (lambda (algorithm key start stop)
        (pcheck ([symbol? algorithm] [bytevector? key] [natural? start] [natural? stop])
                (require-optional-library 'ffi-hmac-state-create 'openssl)
                (native algorithm key start stop)))))
  #|proc:ffi-hmac-state-destroy
  The `ffi-hmac-state-destroy` procedure calls the native openssl operation
  `crypto_hmac_state_destroy`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-hmac-state-destroy
    (let ([native (foreign-procedure "crypto_hmac_state_destroy" (void*) void)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-hmac-state-destroy 'openssl)
                (native ptr-st)))))
  (define ffi-hmac-state-get
    (foreign-procedure "crypto_hmac_state_get" (void*) ptr))
  (define ffi-hmac-state-finalize
    (foreign-procedure "crypto_hmac_state_finalize" (void*) ptr))
  #|proc:ffi-hmac-state-reset!
  The `ffi-hmac-state-reset!` procedure calls the native openssl operation
  `crypto_hmac_state_reset`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-hmac-state-reset!
    (let ([native (foreign-procedure "crypto_hmac_state_reset" (void*) int)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-hmac-state-reset! 'openssl)
                (native ptr-st)))))
  #|proc:ffi-hmac-state-update-bytevector!
  The `ffi-hmac-state-update-bytevector!` procedure calls the native openssl operation
  `crypto_hmac_state_update_bytevector`.
  Parameters `ptr-st`, `bytevector`, `start`, `stop` are passed to the native operation in that
  order.
  `ptr-st` is a native handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-hmac-state-update-bytevector!
    (let ([native (foreign-procedure "crypto_hmac_state_update_bytevector" (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (ptr-st bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? ptr-st] [natural? start] [natural? stop])
                (require-optional-library 'ffi-hmac-state-update-bytevector! 'openssl)
                (native ptr-st bytevector start stop)))))
  #|proc:ffi-hmac-state-update-string!
  The `ffi-hmac-state-update-string!` procedure calls the native openssl operation
  `crypto_hmac_state_update_string`.
  Parameters `ptr-st`, `text`, `start`, `stop` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  `text` is the input string; characters are hashed as native-endian UTF-32 values.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-hmac-state-update-string!
    (let ([native (foreign-procedure "crypto_hmac_state_update_string" (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (ptr-st text start stop)
        (pcheck ([string? text] [natural? ptr-st] [natural? start] [natural? stop])
                (require-optional-library 'ffi-hmac-state-update-string! 'openssl)
                (native ptr-st text start stop)))))

  (define ffi-hkdf
    (foreign-procedure "crypto_hkdf"
                       (ptr ptr unsigned-64 unsigned-64
                            ptr unsigned-64 unsigned-64
                            ptr unsigned-64 unsigned-64 int)
                       ptr))
  (define ffi-hkdf-extract
    (foreign-procedure "crypto_hkdf_extract"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64)
                       ptr))
  (define ffi-hkdf-expand
    (foreign-procedure "crypto_hkdf_expand"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int)
                       ptr))
  (define ffi-pbkdf2
    (foreign-procedure "crypto_pbkdf2"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int int)
                       ptr))
  (define ffi-scrypt
    (foreign-procedure "crypto_scrypt"
                       (ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int int int int)
                       ptr))

  (define ffi-aead-encrypt
    (foreign-procedure "crypto_aead_encrypt"
                       (ptr
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        int)
                       ptr))
  (define ffi-aead-decrypt
    (foreign-procedure "crypto_aead_decrypt"
                       (ptr
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64)
                       ptr))

  #|proc:ffi-cipher-key-size
  The `ffi-cipher-key-size` procedure calls the native openssl operation `crypto_cipher_key_size`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the cipher algorithm symbol, such as `aes-128-ctr`.
  It returns the requested size in bytes, or -1 when the algorithm is unsupported.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-key-size
    (let ([native (foreign-procedure "crypto_cipher_key_size" (ptr) int)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-cipher-key-size 'openssl)
                (native algorithm)))))
  #|proc:ffi-cipher-iv-size
  The `ffi-cipher-iv-size` procedure calls the native openssl operation `crypto_cipher_iv_size`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the cipher algorithm symbol, such as `aes-128-ctr`.
  It returns the requested size in bytes, or -1 when the algorithm is unsupported.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-iv-size
    (let ([native (foreign-procedure "crypto_cipher_iv_size" (ptr) int)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-cipher-iv-size 'openssl)
                (native algorithm)))))
  #|proc:ffi-cipher-block-size
  The `ffi-cipher-block-size` procedure calls the native openssl operation
  `crypto_cipher_block_size`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the cipher algorithm symbol, such as `aes-128-ctr`.
  It returns the requested size in bytes, or -1 when the algorithm is unsupported.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-block-size
    (let ([native (foreign-procedure "crypto_cipher_block_size" (ptr) int)])
      (lambda (algorithm)
        (pcheck ([symbol? algorithm])
                (require-optional-library 'ffi-cipher-block-size 'openssl)
                (native algorithm)))))
  #|proc:ffi-cipher-state-create
  The `ffi-cipher-state-create` procedure calls the native openssl operation
  `crypto_cipher_state_create`.
  Parameters `algorithm`, `encrypt`, `key`, `key-start`, `key-stop`, `iv`, `iv-start`, `iv-stop`
  are passed to the native operation in that order.
  `algorithm` is the cipher algorithm symbol, such as `aes-128-ctr`.
  `encrypt` selects encryption when nonzero and decryption when zero.
  `key` is the secret key bytevector.
  `key-start` is the inclusive start index of its bytevector slice.
  `key-stop` is the exclusive end index of its bytevector slice.
  `iv` is the initialization-vector bytevector.
  `iv-start` is the inclusive start index of its bytevector slice.
  `iv-stop` is the exclusive end index of its bytevector slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-state-create
    (let ([native (foreign-procedure "crypto_cipher_state_create" (ptr int ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64) void*)])
      (lambda (algorithm encrypt key key-start key-stop iv iv-start iv-stop)
        (pcheck ([symbol? algorithm] [bytevector? key] [bytevector? iv] [integer? encrypt] [natural? key-start] [natural? key-stop] [natural? iv-start] [natural? iv-stop])
                (require-optional-library 'ffi-cipher-state-create 'openssl)
                (native algorithm encrypt key key-start key-stop iv iv-start iv-stop)))))
  #|proc:ffi-cipher-state-destroy
  The `ffi-cipher-state-destroy` procedure calls the native openssl operation
  `crypto_cipher_state_destroy`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-state-destroy
    (let ([native (foreign-procedure "crypto_cipher_state_destroy" (void*) void)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-cipher-state-destroy 'openssl)
                (native ptr-st)))))
  (define ffi-cipher-state-update
    (foreign-procedure "crypto_cipher_state_update"
                       (void* ptr unsigned-64 unsigned-64)
                       ptr))
  (define ffi-cipher-state-finalize
    (foreign-procedure "crypto_cipher_state_finalize" (void*) ptr))
  #|proc:ffi-cipher-state-reset!
  The `ffi-cipher-state-reset!` procedure calls the native openssl operation
  `crypto_cipher_state_reset`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cipher-state-reset!
    (let ([native (foreign-procedure "crypto_cipher_state_reset" (void*) int)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-cipher-state-reset! 'openssl)
                (native ptr-st)))))

  #|proc:ffi-pkey-generate
  The `ffi-pkey-generate` procedure calls the native openssl operation `crypto_pkey_generate`.
  Parameters `alg`, `bits`, `curve` are passed to the native operation in that order.
  `alg` is the asymmetric algorithm symbol.
  `bits` is the RSA key size in bits, or zero for other key algorithms.
  `curve` is an EC curve symbol, or #f for algorithms that do not use a curve.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-generate
    (let ([native (foreign-procedure "crypto_pkey_generate" (ptr int ptr) void*)])
      (lambda (alg bits curve)
        (pcheck ([symbol? alg] [(lambda (value) (or (not value) (symbol? value))) curve] [integer? bits])
                (require-optional-library 'ffi-pkey-generate 'openssl)
                (native alg bits curve)))))
  #|proc:ffi-pkey-free
  The `ffi-pkey-free` procedure calls the native openssl operation `crypto_pkey_free`.
  Parameters `ptr-pkey` are passed to the native operation in that order.
  `ptr-pkey` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-free
    (let ([native (foreign-procedure "crypto_pkey_free" (void*) void)])
      (lambda (ptr-pkey)
        (pcheck ([natural? ptr-pkey])
                (require-optional-library 'ffi-pkey-free 'openssl)
                (native ptr-pkey)))))
  (define ffi-pkey-algorithm
    (foreign-procedure "crypto_pkey_algorithm" (void*) ptr))
  #|proc:ffi-pkey-bits
  The `ffi-pkey-bits` procedure calls the native openssl operation `crypto_pkey_bits`.
  Parameters `ptr-pkey` are passed to the native operation in that order.
  `ptr-pkey` is a native handle.
  It returns the key size in bits, or zero for a null key handle.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-bits
    (let ([native (foreign-procedure "crypto_pkey_bits" (void*) int)])
      (lambda (ptr-pkey)
        (pcheck ([natural? ptr-pkey])
                (require-optional-library 'ffi-pkey-bits 'openssl)
                (native ptr-pkey)))))
  #|proc:ffi-pkey-public-from-private
  The `ffi-pkey-public-from-private` procedure calls the native openssl operation
  `crypto_pkey_public_from_private`.
  Parameters `ptr-pkey` are passed to the native operation in that order.
  `ptr-pkey` is a native handle.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-public-from-private
    (let ([native (foreign-procedure "crypto_pkey_public_from_private" (void*) void*)])
      (lambda (ptr-pkey)
        (pcheck ([natural? ptr-pkey])
                (require-optional-library 'ffi-pkey-public-from-private 'openssl)
                (native ptr-pkey)))))
  (define ffi-pkey-store-private-pem
    (foreign-procedure "crypto_pkey_store_private_pem" (void*) ptr))
  (define ffi-pkey-store-public-pem
    (foreign-procedure "crypto_pkey_store_public_pem" (void*) ptr))
  (define ffi-pkey-store-private-der
    (foreign-procedure "crypto_pkey_store_private_der" (void*) ptr))
  (define ffi-pkey-store-public-der
    (foreign-procedure "crypto_pkey_store_public_der" (void*) ptr))
  #|proc:ffi-pkey-load-private-pem
  The `ffi-pkey-load-private-pem` procedure calls the native openssl operation
  `crypto_pkey_load_private_pem`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-load-private-pem
    (let ([native (foreign-procedure "crypto_pkey_load_private_pem" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-pkey-load-private-pem 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-pkey-load-public-pem
  The `ffi-pkey-load-public-pem` procedure calls the native openssl operation
  `crypto_pkey_load_public_pem`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-load-public-pem
    (let ([native (foreign-procedure "crypto_pkey_load_public_pem" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-pkey-load-public-pem 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-pkey-load-private-der
  The `ffi-pkey-load-private-der` procedure calls the native openssl operation
  `crypto_pkey_load_private_der`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-load-private-der
    (let ([native (foreign-procedure "crypto_pkey_load_private_der" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-pkey-load-private-der 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-pkey-load-public-der
  The `ffi-pkey-load-public-der` procedure calls the native openssl operation
  `crypto_pkey_load_public_der`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-pkey-load-public-der
    (let ([native (foreign-procedure "crypto_pkey_load_public_der" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-pkey-load-public-der 'openssl)
                (native bytevector start stop)))))
  (define ffi-sign-message
    (foreign-procedure "crypto_sign_message" (ptr ptr void* ptr unsigned-64 unsigned-64) ptr))
  #|proc:ffi-verify-message
  The `ffi-verify-message` procedure calls the native openssl operation `crypto_verify_message`.
  Parameters `alg`, `digest`, `ptr-pkey`, `msg`, `msg-start`, `msg-stop`, `sig`, `sig-start`,
  `sig-stop` are passed to the native operation in that order.
  `alg` is the asymmetric algorithm symbol.
  `digest` is the digest algorithm symbol, or #f for Ed25519.
  `ptr-pkey` is a native handle.
  `msg` is the signed message bytevector.
  `msg-start` is the inclusive start index of its bytevector slice.
  `msg-stop` is the exclusive end index of its bytevector slice.
  `sig` is the signature bytevector.
  `sig-start` is the inclusive start index of its bytevector slice.
  `sig-stop` is the exclusive end index of its bytevector slice.
  It returns 1 when the signature verifies and 0 when it is invalid or verification fails.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-verify-message
    (let ([native (foreign-procedure "crypto_verify_message" (ptr ptr void* ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64) int)])
      (lambda (alg digest ptr-pkey msg msg-start msg-stop sig sig-start sig-stop)
        (pcheck ([symbol? alg] [(lambda (value) (or (not value) (symbol? value))) digest] [bytevector? msg] [bytevector? sig] [natural? ptr-pkey] [natural? msg-start] [natural? msg-stop] [natural? sig-start] [natural? sig-stop])
                (require-optional-library 'ffi-verify-message 'openssl)
                (native alg digest ptr-pkey msg msg-start msg-stop sig sig-start sig-stop)))))
  (define ffi-derive-shared-secret
    (foreign-procedure "crypto_derive_shared_secret" (ptr void* void*) ptr))

  #|proc:ffi-cert-load-pem
  The `ffi-cert-load-pem` procedure calls the native openssl operation `crypto_cert_load_pem`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-load-pem
    (let ([native (foreign-procedure "crypto_cert_load_pem" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-cert-load-pem 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-cert-load-der
  The `ffi-cert-load-der` procedure calls the native openssl operation `crypto_cert_load_der`.
  Parameters `bytevector`, `start`, `stop` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-load-der
    (let ([native (foreign-procedure "crypto_cert_load_der" (ptr unsigned-64 unsigned-64) void*)])
      (lambda (bytevector start stop)
        (pcheck ([bytevector? bytevector] [natural? start] [natural? stop])
                (require-optional-library 'ffi-cert-load-der 'openssl)
                (native bytevector start stop)))))
  #|proc:ffi-cert-free
  The `ffi-cert-free` procedure calls the native openssl operation `crypto_cert_free`.
  Parameters `ptr-cert` are passed to the native operation in that order.
  `ptr-cert` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-free
    (let ([native (foreign-procedure "crypto_cert_free" (void*) void)])
      (lambda (ptr-cert)
        (pcheck ([natural? ptr-cert])
                (require-optional-library 'ffi-cert-free 'openssl)
                (native ptr-cert)))))
  (define ffi-cert-subject
    (foreign-procedure "crypto_cert_subject" (void*) ptr))
  (define ffi-cert-issuer
    (foreign-procedure "crypto_cert_issuer" (void*) ptr))
  (define ffi-cert-not-before
    (foreign-procedure "crypto_cert_not_before" (void*) ptr))
  (define ffi-cert-not-after
    (foreign-procedure "crypto_cert_not_after" (void*) ptr))
  (define ffi-cert-subject-alt-names
    (foreign-procedure "crypto_cert_subject_alt_names" (void*) ptr))
  #|proc:ffi-cert-hostname-matches
  The `ffi-cert-hostname-matches` procedure calls the native openssl operation
  `crypto_cert_hostname_matches`.
  Parameters `ptr-cert`, `hostname` are passed to the native operation in that order.
  `ptr-cert` is a native handle.
  `hostname` is a UTF-8 hostname bytevector, or #f to omit hostname matching.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-hostname-matches
    (let ([native (foreign-procedure "crypto_cert_hostname_matches" (void* ptr) int)])
      (lambda (ptr-cert hostname)
        (pcheck ([(lambda (value) (or (not value) (bytevector? value))) hostname] [natural? ptr-cert])
                (require-optional-library 'ffi-cert-hostname-matches 'openssl)
                (native ptr-cert hostname)))))
  (define ffi-cert-public-key-der
    (foreign-procedure "crypto_cert_public_key_der" (void*) ptr))
  (define ffi-cert-serial-number
    (foreign-procedure "crypto_cert_serial_number" (void*) ptr))
  (define ffi-cert-fingerprint
    (foreign-procedure "crypto_cert_fingerprint" (void* ptr) ptr))
  #|proc:ffi-cert-store-create
  The `ffi-cert-store-create` procedure calls the native openssl operation
  `crypto_cert_store_create`.
  Parameters `load-defaults` are passed to the native operation in that order.
  `load-defaults` selects loading default CA paths when nonzero.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-store-create
    (let ([native (foreign-procedure "crypto_cert_store_create" (int) void*)])
      (lambda (load-defaults)
        (pcheck ([integer? load-defaults])
                (require-optional-library 'ffi-cert-store-create 'openssl)
                (native load-defaults)))))
  #|proc:ffi-cert-store-destroy
  The `ffi-cert-store-destroy` procedure calls the native openssl operation
  `crypto_cert_store_destroy`.
  Parameters `ptr-store` are passed to the native operation in that order.
  `ptr-store` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-store-destroy
    (let ([native (foreign-procedure "crypto_cert_store_destroy" (void*) void)])
      (lambda (ptr-store)
        (pcheck ([natural? ptr-store])
                (require-optional-library 'ffi-cert-store-destroy 'openssl)
                (native ptr-store)))))
  #|proc:ffi-cert-store-add
  The `ffi-cert-store-add` procedure calls the native openssl operation `crypto_cert_store_add`.
  Parameters `ptr-store`, `ptr-cert` are passed to the native operation in that order.
  `ptr-store` is a native handle.
  `ptr-cert` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-store-add
    (let ([native (foreign-procedure "crypto_cert_store_add" (void* void*) int)])
      (lambda (ptr-store ptr-cert)
        (pcheck ([natural? ptr-store] [natural? ptr-cert])
                (require-optional-library 'ffi-cert-store-add 'openssl)
                (native ptr-store ptr-cert)))))
  #|proc:ffi-cert-store-load-defaults
  The `ffi-cert-store-load-defaults` procedure calls the native openssl operation
  `crypto_cert_store_load_defaults`.
  Parameters `ptr-store` are passed to the native operation in that order.
  `ptr-store` is a native handle.
  It returns 1 when default CA paths load successfully, and 0 on failure or a zero store handle.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-store-load-defaults
    (let ([native (foreign-procedure "crypto_cert_store_load_defaults" (void*) int)])
      (lambda (ptr-store)
        (pcheck ([natural? ptr-store])
                (require-optional-library 'ffi-cert-store-load-defaults 'openssl)
                (native ptr-store)))))
  #|proc:ffi-cert-verify-state-create
  The `ffi-cert-verify-state-create` procedure calls the native openssl operation
  `crypto_cert_verify_state_create`.
  Parameters `ptr-cert`, `ptr-store`, `hostname` are passed to the native operation in that order.
  `ptr-cert` is a native handle.
  `ptr-store` is a native handle.
  `hostname` is a UTF-8 hostname bytevector, or #f to omit hostname matching.
  It returns an owned native handle, or zero on allocation, parsing, or initialization failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-verify-state-create
    (let ([native (foreign-procedure "crypto_cert_verify_state_create" (void* void* ptr) void*)])
      (lambda (ptr-cert ptr-store hostname)
        (pcheck ([(lambda (value) (or (not value) (bytevector? value))) hostname] [natural? ptr-cert] [natural? ptr-store])
                (require-optional-library 'ffi-cert-verify-state-create 'openssl)
                (native ptr-cert ptr-store hostname)))))
  #|proc:ffi-cert-verify-state-add-chain-cert
  The `ffi-cert-verify-state-add-chain-cert` procedure calls the native openssl operation
  `crypto_cert_verify_state_add_chain_cert`.
  Parameters `ptr-st`, `ptr-cert` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  `ptr-cert` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-verify-state-add-chain-cert
    (let ([native (foreign-procedure "crypto_cert_verify_state_add_chain_cert" (void* void*) int)])
      (lambda (ptr-st ptr-cert)
        (pcheck ([natural? ptr-st] [natural? ptr-cert])
                (require-optional-library 'ffi-cert-verify-state-add-chain-cert 'openssl)
                (native ptr-st ptr-cert)))))
  #|proc:ffi-cert-verify-state-verify
  The `ffi-cert-verify-state-verify` procedure calls the native openssl operation
  `crypto_cert_verify_state_verify`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It returns 1 on success and 0 on failure.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-verify-state-verify
    (let ([native (foreign-procedure "crypto_cert_verify_state_verify" (void*) int)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-cert-verify-state-verify 'openssl)
                (native ptr-st)))))
  #|proc:ffi-cert-verify-state-destroy
  The `ffi-cert-verify-state-destroy` procedure calls the native openssl operation
  `crypto_cert_verify_state_destroy`.
  Parameters `ptr-st` are passed to the native operation in that order.
  `ptr-st` is a native handle.
  It releases the native resource and returns unspecified values; a zero handle is ignored.
  It raises an error when OpenSSL is unavailable.
  |#
  (define ffi-cert-verify-state-destroy
    (let ([native (foreign-procedure "crypto_cert_verify_state_destroy" (void*) void)])
      (lambda (ptr-st)
        (pcheck ([natural? ptr-st])
                (require-optional-library 'ffi-cert-verify-state-destroy 'openssl)
                (native ptr-st)))))
  #|proc:ffi-blake3-bytevector
  The `ffi-blake3-bytevector` procedure returns the BLAKE3 digest of bytevector `bytes`
  between byte indices `start` (inclusive) and `stop` (exclusive). The caller validates
  slice bounds. It raises an error when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-bytevector
    (let ([native (foreign-procedure "crypto_blake3_bytevector"
                                    (ptr unsigned-64 unsigned-64) ptr)])
      (lambda (bytes start stop)
        (pcheck ([bytevector? bytes] [natural? start stop])
          (require-optional-library 'ffi-blake3-bytevector 'blake3)
          (native bytes start stop)))))

  #|proc:ffi-blake3-string
  The `ffi-blake3-string` procedure returns the BLAKE3 digest of string `text` between
  character indices `start` (inclusive) and `stop` (exclusive), using native-endian UTF-32
  without a byte-order mark. The caller validates slice bounds. It raises an error when
  BLAKE3 is unavailable.
  |#
  (define ffi-blake3-string
    (let ([native (foreign-procedure "crypto_blake3_string"
                                    (ptr unsigned-64 unsigned-64) ptr)])
      (lambda (text start stop)
        (pcheck ([string? text] [natural? start stop])
          (require-optional-library 'ffi-blake3-string 'blake3)
          (native text start stop)))))

  #|proc:ffi-blake3-create
  The `ffi-blake3-create` procedure returns a new native BLAKE3 context handle, or zero
  if allocation fails. It raises an error when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-create
    (let ([native (foreign-procedure "crypto_blake3_create" () void*)])
      (lambda ()
        (pcheck ()
          (require-optional-library 'ffi-blake3-create 'blake3)
          (native)))))

  #|proc:ffi-blake3-destroy
  The `ffi-blake3-destroy` procedure releases the native BLAKE3 context `handle` and returns
  unspecified values. A zero handle is ignored. It raises if BLAKE3 is unavailable.
  |#
  (define ffi-blake3-destroy
    (let ([native (foreign-procedure "crypto_blake3_destroy" (void*) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
          (require-optional-library 'ffi-blake3-destroy 'blake3)
          (native handle)))))

  #|proc:ffi-blake3-get
  The `ffi-blake3-get` procedure returns the current BLAKE3 digest bytevector of native
  context `handle`, preserving the context, or `#f` for a zero handle. It raises an error
  when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-get
    (let ([native (foreign-procedure "crypto_blake3_get" (void*) ptr)])
      (lambda (handle)
        (pcheck ([natural? handle])
          (require-optional-library 'ffi-blake3-get 'blake3)
          (native handle)))))

  #|proc:ffi-blake3-reset!
  The `ffi-blake3-reset!` procedure resets native BLAKE3 context `handle`, returning 1
  on success or 0 for a zero handle. It raises an error when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-reset!
    (let ([native (foreign-procedure "crypto_blake3_reset" (void*) int)])
      (lambda (handle)
        (pcheck ([natural? handle])
          (require-optional-library 'ffi-blake3-reset! 'blake3)
          (native handle)))))

  #|proc:ffi-blake3-update-bytevector!
  The `ffi-blake3-update-bytevector!` procedure feeds bytevector `bytes` between byte
  indices `start` (inclusive) and `stop` (exclusive) into native context `handle`.
  The caller validates slice bounds. It returns 1 on success, or 0 for a zero handle,
  and raises an error when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-update-bytevector!
    (let ([native (foreign-procedure "crypto_blake3_update_bytevector"
                                    (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (handle bytes start stop)
        (pcheck ([natural? handle start stop] [bytevector? bytes])
          (require-optional-library 'ffi-blake3-update-bytevector! 'blake3)
          (native handle bytes start stop)))))

  #|proc:ffi-blake3-update-string!
  The `ffi-blake3-update-string!` procedure feeds string `text` between character indices
  `start` (inclusive) and `stop` (exclusive) into native context `handle`, using native-endian
  UTF-32 without a byte-order mark. The caller validates slice bounds. It returns 1 on
  success, or 0 for a zero handle, and raises an error when BLAKE3 is unavailable.
  |#
  (define ffi-blake3-update-string!
    (let ([native (foreign-procedure "crypto_blake3_update_string"
                                    (void* ptr unsigned-64 unsigned-64) int)])
      (lambda (handle text start stop)
        (pcheck ([natural? handle start stop] [string? text])
          (require-optional-library 'ffi-blake3-update-string! 'blake3)
          (native handle text start stop)))))
  )
