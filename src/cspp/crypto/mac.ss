(library (cspp crypto mac)
  (export mac-algorithms
          hmac
          hmac-verify?
          make-hmac-state
          hmac-state?
          hmac-update-bytevector!
          hmac-update-string!
          hmac-get
          hmac-finalize!
          hmac-reset!
          hmac-destroy!
          call-with-hmac-state)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private)
          (cspp crypto constant-time))

  (define mac-algorithms
    '(sha224 sha256 sha384 sha512 sha512-224 sha512-256
      sha3-224 sha3-256 sha3-384 sha3-512
      blake2b-512 blake2s-256))

  (define check-mac-algorithm
    (lambda (who which)
      (unless (memq which mac-algorithms)
        (errorf who "valid MAC algorithm is one of ~a" mac-algorithms))))

  (define-record-type (hmac-state %make-hmac-state hmac-state?)
    (opaque #t)
    (fields mutex (mutable handle)
            algorithm
            (mutable destroyed?)
            (mutable finalized?)))

  (define check-live-state
    (lambda (who st)
      (when (hmac-state-destroyed? st)
        (errorf who "HMAC state is destroyed"))
      (when (hmac-state-finalized? st)
        (errorf who "HMAC state is finalized"))))

  #|proc:make-hmac-state
  The `make-hmac-state` procedure returns an owned incremental HMAC state for digest
  symbol `which` and bytevector `key`. Optional nonnegative fixnums `start` and `stop`
  select the half-open key slice, defaulting to zero and the bytevector length.
  The state copies the key and serializes its operations. Release it with
  `hmac-finalize!` or `hmac-destroy!`. Invalid inputs and native failures raise errors.
  |#
  (define-who make-hmac-state
    (case-lambda
      [(which key)
       (pcheck ([bytevector? key])
               (make-hmac-state which key 0 (bytevector-length key)))]
      [(which key start)
       (pcheck ([bytevector? key])
               (make-hmac-state which key start (bytevector-length key)))]
      [(which key start stop)
       (pcheck ([bytevector? key])
               (check-mac-algorithm who which)
               (check-slice who (bytevector-length key) start stop)
               (with-crypto-operation
                 (let ([handle (ffi-hmac-state-create which key start stop)])
                   (when (eq? handle 0)
                     (errorf who "failed to create HMAC state for ~s" which))
                   (%make-hmac-state (make-mutex 'hmac) handle which #f #f))))]))

  #|proc:hmac-update-bytevector!
The `hmac-update-bytevector!` procedure updates an HMAC state with a bytevector slice.
|#
  (define-who hmac-update-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([hmac-state? st] [bytevector? bv])
               (hmac-update-bytevector! st bv 0 (bytevector-length bv)))]
      [(st bv start)
       (pcheck ([hmac-state? st] [bytevector? bv])
               (hmac-update-bytevector! st bv start (bytevector-length bv)))]
      [(st bv start stop)
       (pcheck ([hmac-state? st] [bytevector? bv])
               (with-crypto-mutex (hmac-state-mutex st)
                 (check-live-state who st)
                 (check-slice who (bytevector-length bv) start stop)
                 (when (fx= 0 (ffi-hmac-state-update-bytevector! (hmac-state-handle st) bv start stop))
                   (errorf who "failed to update HMAC state"))
                 st))]))

  #|proc:hmac-update-string!
  The `hmac-update-string!` procedure feeds string `str` into the live HMAC state `st`
  and returns `st`. Characters use native-endian UTF-32 without a byte-order mark.
  Optional nonnegative fixnums `start` and `stop` select a half-open character slice,
  defaulting to zero and the string length. Invalid or terminal states, invalid slices,
  and native update failures raise errors.
  |#
  (define-who hmac-update-string!
    (case-lambda
      [(st str)
       (pcheck ([hmac-state? st] [string? str])
               (hmac-update-string! st str 0 (string-length str)))]
      [(st str start)
       (pcheck ([hmac-state? st] [string? str])
               (hmac-update-string! st str start (string-length str)))]
      [(st str start stop)
       (pcheck ([hmac-state? st] [string? str])
               (with-crypto-mutex (hmac-state-mutex st)
                 (check-live-state who st)
                 (check-slice who (string-length str) start stop)
                 (when (fx= 0 (ffi-hmac-state-update-string! (hmac-state-handle st) str start stop))
                   (errorf who "failed to update HMAC state"))
                 st))]))

  #|proc:hmac-get
The `hmac-get` procedure returns the current MAC without finalizing the state.
|#
  (define-who hmac-get
    (lambda (st)
      (pcheck ([hmac-state? st])
              (with-crypto-mutex (hmac-state-mutex st)
                (check-live-state who st)
                (let ([ans (ffi-hmac-state-get (hmac-state-handle st))])
                  (when (eq? ans #f)
                    (errorf who "failed to obtain HMAC value"))
                  ans)))))

  #|proc:hmac-finalize!
The `hmac-finalize!` procedure finalizes an HMAC state and returns the MAC value.
|#
  (define-who hmac-finalize!
    (lambda (st)
      (pcheck ([hmac-state? st])
              (with-crypto-mutex (hmac-state-mutex st)
                (check-live-state who st)
                (let ([ans (ffi-hmac-state-finalize (hmac-state-handle st))])
                  (when (eq? ans #f)
                    (errorf who "failed to finalize HMAC state"))
                  (ffi-hmac-state-destroy (hmac-state-handle st))
                  (hmac-state-handle-set! st 0)
                  (hmac-state-finalized?-set! st #t)
                  ans)))))

  #|proc:hmac-reset!
  The `hmac-reset!` procedure clears the accumulated input of live state `st`, retaining
  its original key and algorithm, and returns `st`. Finalized or destroyed states and
  native reset failures raise errors.
  |#
  (define-who hmac-reset!
    (lambda (st)
      (pcheck ([hmac-state? st])
              (with-crypto-mutex (hmac-state-mutex st)
                (check-live-state who st)
                (when (fx= 0 (ffi-hmac-state-reset! (hmac-state-handle st)))
                  (errorf who "failed to reset HMAC state"))
                st))))

  #|proc:hmac-destroy!
The `hmac-destroy!` procedure releases the foreign resources owned by an HMAC state.
|#
  (define-who hmac-destroy!
    (lambda (st)
      (pcheck ([hmac-state? st])
              (with-crypto-mutex (hmac-state-mutex st)
                (unless (hmac-state-destroyed? st)
                  (unless (eq? (hmac-state-handle st) 0)
                    (ffi-hmac-state-destroy (hmac-state-handle st))
                    (hmac-state-handle-set! st 0))
                  (hmac-state-destroyed?-set! st #t))
                st))))

  #|proc:call-with-hmac-state
  The `call-with-hmac-state` procedure creates an HMAC state for digest symbol `which`
  and bytevector `key`, calls `(proc state)`, and returns the callback's values.
  The callback runs without holding a state lock. The state is destroyed on every exit,
  including errors and continuation escapes. Reentry after exit raises an error.
  Invalid inputs or native failures raise errors.
  |#
  (define-who call-with-hmac-state
    (lambda (which key proc)
      (pcheck ([procedure? proc] [bytevector? key])
              (call-with-crypto-resource who
                (lambda () (make-hmac-state which key)) hmac-destroy! proc))))

  #|proc:hmac
  The `hmac` procedure returns an HMAC bytevector for string or bytevector `data`, using
  digest symbol `which` and bytevector `key`. Strings use native-endian UTF-32 without
  a byte-order mark; bytevectors are used as supplied. To authenticate UTF-8 text,
  pass `(string->utf8 text)`. Invalid inputs and native failures raise errors.
  |#
  (define-who hmac
    (lambda (which key data)
      (pcheck ([bytevector? key])
              (call-with-hmac-state
               which key
               (lambda (st)
                 (pcase data
                   [bytevector? (begin (hmac-update-bytevector! st data)
                                       (hmac-finalize! st))]
                   [string? (begin (hmac-update-string! st data)
                                   (hmac-finalize! st))]
                   [else (errorf who "expected string or bytevector, given ~s" data)]))))))

  #|proc:hmac-verify?
  The `hmac-verify?` procedure returns `#t` if bytevector `expected-tag` matches the HMAC
  of string or bytevector `data` using digest symbol `which` and bytevector `key`.
  Strings use native-endian UTF-32 without a byte-order mark. It compares equal-length
  tags in constant time and returns `#f` for a mismatch. Invalid inputs raise errors.
  |#
  (define-who hmac-verify?
    (lambda (which key data expected-tag)
      (pcheck ([bytevector? key expected-tag])
              (constant-time-bytevector=? (hmac which key data) expected-tag))))
  )
