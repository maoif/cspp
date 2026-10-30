(library (cspp crypto cipher)
  (export cipher-algorithms
          cipher-key-size
          cipher-iv-size
          cipher-block-size
          make-cipher-state
          cipher-state?
          cipher-update-bytevector!
          cipher-finalize!
          cipher-reset!
          cipher-destroy!
          call-with-cipher-state)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private))

  (define cipher-algorithms '(aes-128-ctr aes-256-ctr chacha20))

  (define check-cipher-algorithm
    (lambda (who which)
      (unless (memq which cipher-algorithms)
        (errorf who "valid cipher algorithm is one of ~a" cipher-algorithms))))

  (define check-cipher-mode
    (lambda (who mode)
      (unless (memq mode '(encrypt decrypt))
        (errorf who "valid cipher mode is one of (encrypt decrypt), given ~s" mode))))

  (define-record-type (cipher-state %make-cipher-state cipher-state?)
    (opaque #t)
    (fields mutex algorithm
            mode
            (mutable handle)
            (mutable destroyed?)
            (mutable finalized?)))

  (define check-cipher-state-live
    (lambda (who st)
      (when (cipher-state-destroyed? st)
        (errorf who "cipher state is destroyed"))
      (when (cipher-state-finalized? st)
        (errorf who "cipher state is finalized"))))

  (define check-cipher-state-resettable
    (lambda (who st)
      (when (cipher-state-destroyed? st)
        (errorf who "cipher state is destroyed"))))

  #|proc:cipher-key-size
The `cipher-key-size` procedure returns the required key size in bytes for `which`.
|#
  (define-who cipher-key-size
    (lambda (which)
      (check-cipher-algorithm who which)
      (let ([ans (ffi-cipher-key-size which)])
        (when (fx< ans 0)
          (errorf who "failed to query key size for ~s" which))
        ans)))

  #|proc:cipher-iv-size
The `cipher-iv-size` procedure returns the required IV size in bytes for `which`.
|#
  (define-who cipher-iv-size
    (lambda (which)
      (check-cipher-algorithm who which)
      (let ([ans (ffi-cipher-iv-size which)])
        (when (fx< ans 0)
          (errorf who "failed to query IV size for ~s" which))
        ans)))

  #|proc:cipher-block-size
The `cipher-block-size` procedure returns the block size in bytes for `which`.
|#
  (define-who cipher-block-size
    (lambda (which)
      (check-cipher-algorithm who which)
      (let ([ans (ffi-cipher-block-size which)])
        (when (fx< ans 0)
          (errorf who "failed to query block size for ~s" which))
        ans)))

  #|proc:make-cipher-state
The `make-cipher-state` procedure creates a lower-level cipher state in `mode` for `which`.
|#
  (define-who make-cipher-state
    (lambda (mode which key iv)
      (pcheck ([bytevector? key iv])
              (check-cipher-mode who mode)
              (check-cipher-algorithm who which)
              (unless (fx= (bytevector-length key) (cipher-key-size which))
                (errorf who "invalid key length ~a for ~s, expected ~a"
                        (bytevector-length key) which (cipher-key-size which)))
              (unless (fx= (bytevector-length iv) (cipher-iv-size which))
                (errorf who "invalid IV length ~a for ~s, expected ~a"
                        (bytevector-length iv) which (cipher-iv-size which)))
              (with-crypto-operation
                (let ([handle (ffi-cipher-state-create which
                                                      (if (eq? mode 'encrypt) 1 0)
                                                      key 0 (bytevector-length key)
                                                      iv 0 (bytevector-length iv))])
                  (when (eq? handle 0)
                    (errorf who "failed to create cipher state for ~s" which))
                  (%make-cipher-state (make-mutex 'cipher) which mode handle #f #f))))))

  #|proc:cipher-update-bytevector!
The `cipher-update-bytevector!` procedure transforms a bytevector slice and returns the output chunk.
|#
  (define-who cipher-update-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([cipher-state? st] [bytevector? bv])
               (with-crypto-mutex (cipher-state-mutex st)
                 (cipher-update-bytevector! st bv 0 (bytevector-length bv))))]
      [(st bv start)
       (pcheck ([cipher-state? st] [bytevector? bv])
               (with-crypto-mutex (cipher-state-mutex st)
                 (cipher-update-bytevector! st bv start (bytevector-length bv))))]
      [(st bv start stop)
       (pcheck ([cipher-state? st] [bytevector? bv])
               (with-crypto-mutex (cipher-state-mutex st)
                 (check-cipher-state-live who st)
                 (check-slice who (bytevector-length bv) start stop)
                 (let ([ans (ffi-cipher-state-update (cipher-state-handle st) bv start stop)])
                   (when (eq? ans #f)
                     (errorf who "cipher update failed for ~s" (cipher-state-algorithm st)))
                   ans)))]))

  #|proc:cipher-finalize!
The `cipher-finalize!` procedure finalizes a cipher state and returns the trailing output chunk.
|#
  (define-who cipher-finalize!
    (lambda (st)
      (pcheck ([cipher-state? st])
              (with-crypto-mutex (cipher-state-mutex st)
                (check-cipher-state-live who st)
                (let ([ans (ffi-cipher-state-finalize (cipher-state-handle st))])
                  (when (eq? ans #f)
                    (errorf who "cipher finalization failed for ~s" (cipher-state-algorithm st)))
                  (cipher-state-finalized?-set! st #t)
                  ans)))))

  #|proc:cipher-reset!
The `cipher-reset!` procedure reinitializes a cipher state with its original key and IV.
|#
  (define-who cipher-reset!
    (lambda (st)
      (pcheck ([cipher-state? st])
              (with-crypto-mutex (cipher-state-mutex st)
                (check-cipher-state-resettable who st)
                (unless (fx= 1 (ffi-cipher-state-reset! (cipher-state-handle st)))
                  (errorf who "failed to reset cipher state for ~s" (cipher-state-algorithm st)))
                (cipher-state-finalized?-set! st #f)
                st))))

  #|proc:cipher-destroy!
The `cipher-destroy!` procedure releases foreign resources owned by `st`.
|#
  (define-who cipher-destroy!
    (lambda (st)
      (pcheck ([cipher-state? st])
              (with-crypto-mutex (cipher-state-mutex st)
                (unless (cipher-state-destroyed? st)
                  (ffi-cipher-state-destroy (cipher-state-handle st))
                  (cipher-state-handle-set! st 0)
                  (cipher-state-destroyed?-set! st #t))
                st))))

  #|proc:call-with-cipher-state
The `call-with-cipher-state` procedure creates a cipher state, passes it to `proc`, and always destroys it afterwards.
|#
  (define-who call-with-cipher-state
    (lambda (mode which key iv proc)
      (pcheck ([bytevector? key iv] [procedure? proc])
              (let ([st (make-cipher-state mode which key iv)])
                (dynamic-wind
                  void
                  (lambda () (proc st))
                  (lambda () (cipher-destroy! st)))))))
  )
