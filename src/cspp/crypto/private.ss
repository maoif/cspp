(library (cspp crypto private)
  (export check-slice
          make-crypto-lock
          call-with-crypto-locks
          with-crypto-locks
          ensure-openssl
          bytevector-append
          subbytevector
          string/bytevector->utf8
          u32-set!
          u32-ref
          bytevector-prefix?
          bytevector-empty
          make-crypto-private-key
          crypto-private-key?
          crypto-private-key-lock
          crypto-private-key-handle
          crypto-private-key-handle-set!
          crypto-private-key-algorithm
          crypto-private-key-bits
          crypto-private-key-destroyed?
          crypto-private-key-destroyed?-set!
          make-crypto-public-key
          crypto-public-key?
          crypto-public-key-lock
          crypto-public-key-handle
          crypto-public-key-handle-set!
          crypto-public-key-algorithm
          crypto-public-key-bits
          crypto-public-key-destroyed?
          crypto-public-key-destroyed?-set!
          make-crypto-certificate
          crypto-certificate?
          crypto-certificate-lock
          crypto-certificate-handle
          crypto-certificate-handle-set!
          crypto-certificate-destroyed?
          crypto-certificate-destroyed?-set!
          make-crypto-cert-store
          crypto-cert-store?
          crypto-cert-store-lock
          crypto-cert-store-handle
          crypto-cert-store-handle-set!
          crypto-cert-store-destroyed?
          crypto-cert-store-destroyed?-set!
          make-crypto-envelope
          crypto-envelope?
          crypto-envelope-kem
          crypto-envelope-cipher
          crypto-envelope-ephemeral-key
          crypto-envelope-nonce
          crypto-envelope-ciphertext
          crypto-envelope-tag)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi))

  (define-record-type (crypto-lock %make-crypto-lock crypto-lock?)
    (fields rank mutex))
  (define next-lock-rank 0)
  (define lock-rank-mutex (make-mutex 'crypto-lock-rank))

  #|proc:make-crypto-lock
  The `make-crypto-lock` procedure returns a new internal resource lock with a unique
  ordering rank. Acquire multiple locks through `with-crypto-locks` to avoid deadlocks.
  |#
  (define make-crypto-lock
    (lambda ()
      (pcheck ()
        (with-mutex lock-rank-mutex
          (let ([rank next-lock-rank])
            (set! next-lock-rank (+ rank 1))
            (%make-crypto-lock rank (make-mutex 'crypto-resource)))))))

  #|proc:call-with-crypto-locks
  The `call-with-crypto-locks` procedure acquires the internal resource locks in list
  `locks` in creation order, calls the zero-argument procedure `proc`, and returns its
  values. It releases locks on every exit. The callback must not invoke user code.
  |#
  (define call-with-crypto-locks
    (lambda (locks proc)
      (pcheck ([list? locks] [procedure? proc])
        (for-each (lambda (lock)
                    (unless (crypto-lock? lock)
                      (error 'call-with-crypto-locks "invalid internal resource lock")))
                  locks)
        (let loop ([locks (if (or (null? locks) (null? (cdr locks)))
                             locks
                             (list-sort (lambda (left right)
                                          (< (crypto-lock-rank left) (crypto-lock-rank right)))
                                        locks))]
                   [previous #f])
          (cond
            [(null? locks) (proc)]
            [(eq? (car locks) previous) (loop (cdr locks) previous)]
            [else
             (with-mutex (crypto-lock-mutex (car locks))
               (loop (cdr locks) (car locks)))])))))

  #|macro:with-crypto-locks
  The `with-crypto-locks` macro acquires each internal resource lock in `(lock ...)`
  once, in creation order, evaluates `body ...`, and returns its values. Locks are
  released on normal and exceptional exits. No user callbacks should run in the body.
  |#
  (define-syntax with-crypto-locks
    (syntax-rules ()
      [(_ (lock ...) body ...)
       (call-with-crypto-locks (list lock ...) (lambda () body ...))]))

  #|proc:ensure-openssl
The `ensure-openssl` procedure raises an error attributed to `who` when OpenSSL is unavailable.
It returns an unspecified value when the runtime is available.
|#
  (define ensure-openssl
    (lambda (who)
      (let ([message (ffi-openssl-load-error)])
        (when message
          (error who message)))))

  #|proc:bytevector-empty
The `bytevector-empty` procedure returns an empty bytevector.
|#
  (define bytevector-empty
    (lambda ()
      (make-bytevector 0 0)))

  #|proc:check-slice
The `check-slice` procedure validates a `[start, stop)` slice against `len`.
|#
  (define check-slice
    (lambda (who len start stop)
      (unless (natural? start)
        (errorf who "invalid start index ~a" start))
      (unless (natural? stop)
        (errorf who "invalid stop index ~a" stop))
      (when (fx> start stop)
        (errorf who "start index ~a is greater than stop index ~a" start stop))
      (when (fx> stop len)
        (errorf who "stop index ~a is greater than total length ~a" stop len))))

  #|proc:subbytevector
The `subbytevector` procedure copies a bytevector slice.
|#
  (define subbytevector
    (lambda (bv start stop)
      (let ([out (make-bytevector (fx- stop start) 0)])
        (bytevector-copy! bv start out 0 (fx- stop start))
        out)))

  #|proc:bytevector-append
The `bytevector-append` procedure concatenates the given bytevectors.
|#
  (define bytevector-append
    (case-lambda
      [() (bytevector-empty)]
      [(bv) bv]
      [bv*
       (let ([len (fold-left (lambda (acc bv) (fx+ acc (bytevector-length bv))) 0 bv*)])
         (let ([out (make-bytevector len 0)])
           (let loop ([ls bv*] [i 0])
             (if (null? ls)
                 out
                 (let ([bv (car ls)])
                   (bytevector-copy! bv 0 out i (bytevector-length bv))
                   (loop (cdr ls) (fx+ i (bytevector-length bv))))))))]))

  #|proc:string/bytevector->utf8
The `string/bytevector->utf8` procedure converts a string to UTF-8 and leaves bytevectors unchanged.
|#
  (define string/bytevector->utf8
    (lambda (x)
      (pcase x
        [bytevector? x]
        [string? (string->utf8 x)]
        [else (errorf 'string/bytevector->utf8 "expected string or bytevector, given ~s" x)])))

  #|proc:u32-set!
The `u32-set!` procedure writes `n` as a big-endian 32-bit integer.
|#
  (define u32-set!
    (lambda (bv i n)
      (bytevector-u8-set! bv i (fxlogand (fxsrl n 24) #xff))
      (bytevector-u8-set! bv (fx1+ i) (fxlogand (fxsrl n 16) #xff))
      (bytevector-u8-set! bv (fx+ i 2) (fxlogand (fxsrl n 8) #xff))
      (bytevector-u8-set! bv (fx+ i 3) (fxlogand n #xff))))

  #|proc:u32-ref
The `u32-ref` procedure reads a big-endian 32-bit integer.
|#
  (define u32-ref
    (lambda (bv i)
      (fxlogor (fxsll (bytevector-u8-ref bv i) 24)
               (fxsll (bytevector-u8-ref bv (fx1+ i)) 16)
               (fxsll (bytevector-u8-ref bv (fx+ i 2)) 8)
               (bytevector-u8-ref bv (fx+ i 3)))))

  #|proc:bytevector-prefix?
The `bytevector-prefix?` procedure checks whether `prefix` matches the initial bytes of `bv`.
|#
  (define bytevector-prefix?
    (lambda (prefix bv)
      (and (fx<= (bytevector-length prefix) (bytevector-length bv))
           (let loop ([i 0])
             (or (fx= i (bytevector-length prefix))
                 (and (fx= (bytevector-u8-ref prefix i) (bytevector-u8-ref bv i))
                      (loop (fx1+ i))))))))

  (define-record-type (crypto-private-key make-crypto-private-key crypto-private-key?)
    (opaque #t)
    (protocol (lambda (new)
                (lambda (handle algorithm bits destroyed?)
                  (new (make-crypto-lock) handle algorithm bits destroyed?))))
    (fields lock (mutable handle)
            algorithm
            bits
            (mutable destroyed?)))

  (define-record-type (crypto-public-key make-crypto-public-key crypto-public-key?)
    (opaque #t)
    (protocol (lambda (new)
                (lambda (handle algorithm bits destroyed?)
                  (new (make-crypto-lock) handle algorithm bits destroyed?))))
    (fields lock (mutable handle)
            algorithm
            bits
            (mutable destroyed?)))

  (define-record-type (crypto-certificate make-crypto-certificate crypto-certificate?)
    (opaque #t)
    (protocol (lambda (new)
                (lambda (handle destroyed?)
                  (new (make-crypto-lock) handle destroyed?))))
    (fields lock (mutable handle)
            (mutable destroyed?)))

  (define-record-type (crypto-cert-store make-crypto-cert-store crypto-cert-store?)
    (opaque #t)
    (protocol (lambda (new)
                (lambda (handle destroyed?)
                  (new (make-crypto-lock) handle destroyed?))))
    (fields lock (mutable handle)
            (mutable destroyed?)))

  (define-record-type (crypto-envelope make-crypto-envelope crypto-envelope?)
    (opaque #t)
    (fields kem
            cipher
            ephemeral-key
            nonce
            ciphertext
            tag))
  )
