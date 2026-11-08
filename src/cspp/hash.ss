(library (cspp hash)
  (export xxhash32-fixnum
          xxhash32-flonum
          xxhash32-ratnum
          xxhash32-cflonum
          xxhash32-bool
          xxhash32-char
          xxhash32-string
          xxhash32-fxvector
          xxhash32-flvector
          xxhash32-bytevector
          xxhash32-file

          xxhash64-fixnum
          xxhash64-flonum
          xxhash64-ratnum
          xxhash64-cflonum
          xxhash64-bool
          xxhash64-char
          xxhash64-string
          xxhash64-fxvector
          xxhash64-flvector
          xxhash64-bytevector
          xxhash64-file

          xxhash3-64-fixnum
          xxhash3-64-flonum
          xxhash3-64-ratnum
          xxhash3-64-cflonum
          xxhash3-64-bool
          xxhash3-64-char
          xxhash3-64-string
          xxhash3-64-fxvector
          xxhash3-64-flvector
          xxhash3-64-bytevector
          xxhash3-64-file

          make-hasher hasher? hasher-get hasher-finalize! hasher-reset!
          hasher-update-fixnum!
          hasher-update-flonum!
          hasher-update-ratnum!
          hasher-update-cflonum!
          hasher-update-bool!
          hasher-update-char!
          hasher-update-string!
          hasher-update-fxvector!
          hasher-update-bytevector!
          hasher-update-flvector!

          call-with-hasher)
  (import (cspp optional-library-check) (cspp chez)
          (cspp utils)
          (cspp internal)
          (cspp io)
          (cspp file))

  ;; TOOD move to vector.ss
  #|doc
  |#
  (define bytevector->hex
    (lambda (bv)
      (pcheck ([bytevector? bv])
              (let ([str (make-string (fx* 2 (bytevector-length bv)) #\space)])
                (let loop ([i 0] [j 0])
                  (if (fx= i (bytevector-length bv))
                      str
                      (let* ([x (bytevector-u8-ref bv i)]
                             [a (number->string (fxlogand x #xf) 16)]
                             [b (number->string (fxsrl x 4) 16)])
                        (assert (and (= 1 (string-length a)) (= 1 (string-length b))))
                        (string-set! str j (char-downcase (string-ref b 0)))
                        (string-set! str (fx1+ j) (char-downcase (string-ref a 0)))
                        (loop (fx1+ i) (fx+ j 2)))))))))


  (define *hashers* '(xxhash32 xxhash64 xxhash3-64))
  (define check-hasher
    (lambda (who which)
      (unless (memq which *hashers*)
        (errorf who "valid hash algorithm is one of ~a" *hashers*))))

  (define hash-index?
    (lambda (index)
      (and (fixnum? index) (fx>= index 0) (<= index #x7fffffff))))

  (define check-hash-slice
    (lambda (who length start stop)
      (when (fx> start stop)
        (errorf who "start index ~a is greater than stop index ~a" start stop))
      (when (> stop length)
        (errorf who "stop index ~a is greater than total length ~a" stop length))))

  (define ffi-xxhash-load-error
    (foreign-procedure "cspp_xxhash_load_error" () ptr))

  (define ensure-xxhash
    (lambda (who)
      (let ([message (ffi-xxhash-load-error)])
        (when message
          (error who message)))))


;;;;===----------------------------------------------------------------------===
;;;;  specialized interface
;;;;===----------------------------------------------------------------------===


  #|proc:ffi-xxh32
  The `ffi-xxh32` procedure calls the native xxhash operation `hash_XXH32`.
  Parameters `bytevector`, `seed` are passed to the native operation in that order.
  `bytevector` is the input bytevector.
  `seed` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32
    (let ([native (foreign-procedure "hash_XXH32" (ptr unsigned-int) unsigned-int)])
      (lambda (bytevector seed)
        (pcheck ([natural? seed])
                (require-optional-library 'ffi-xxh32 'xxhash)
                (native bytevector seed)))))
  (define ffi-xxh64   (foreign-procedure "hash_XXH64"   (ptr unsigned-long) ptr))
  (define ffi-xxh3-64 (foreign-procedure "hash_XXH3_64" (ptr unsigned-long) ptr))

  #|proc:ffi-xxh32-fixnum
  The `ffi-xxh32-fixnum` procedure calls the native xxhash operation `hash_XXH32_fixnum`.
  Parameters `value`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-fixnum
    (let ([native (foreign-procedure "hash_XXH32_fixnum" (fixnum unsigned-32) unsigned-32)])
      (lambda (value salt)
        (pcheck ([fixnum? value] [natural? salt])
                (require-optional-library 'ffi-xxh32-fixnum 'xxhash)
                (native value salt)))))
  #|proc:ffi-xxh32-flonum
  The `ffi-xxh32-flonum` procedure calls the native xxhash operation `hash_XXH32_flonum`.
  Parameters `value`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-flonum
    (let ([native (foreign-procedure "hash_XXH32_flonum" (double unsigned-32) unsigned-32)])
      (lambda (value salt)
        (pcheck ([flonum? value] [natural? salt])
                (require-optional-library 'ffi-xxh32-flonum 'xxhash)
                (native value salt)))))
  #|proc:ffi-xxh32-ratnum
  The `ffi-xxh32-ratnum` procedure calls the native xxhash operation `hash_XXH32_ratnum`.
  Parameters `value`, `other-value`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-ratnum
    (let ([native (foreign-procedure "hash_XXH32_ratnum" (fixnum fixnum unsigned-32) unsigned-32)])
      (lambda (value other-value salt)
        (pcheck ([fixnum? value] [fixnum? other-value] [natural? salt])
                (require-optional-library 'ffi-xxh32-ratnum 'xxhash)
                (native value other-value salt)))))
  #|proc:ffi-xxh32-cflonum
  The `ffi-xxh32-cflonum` procedure calls the native xxhash operation `hash_XXH32_cflonum`.
  Parameters `value`, `other-value`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-cflonum
    (let ([native (foreign-procedure "hash_XXH32_cflonum" (double double unsigned-32) unsigned-32)])
      (lambda (value other-value salt)
        (pcheck ([flonum? value] [flonum? other-value] [natural? salt])
                (require-optional-library 'ffi-xxh32-cflonum 'xxhash)
                (native value other-value salt)))))
  #|proc:ffi-xxh32-string
  The `ffi-xxh32-string` procedure calls the native xxhash operation `hash_XXH32_string`.
  Parameters `value`, `start`, `stop`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-string
    (let ([native (foreign-procedure "hash_XXH32_string" (ptr int int unsigned-32) unsigned-32)])
      (lambda (value start stop salt)
        (pcheck ([integer? start] [integer? stop] [natural? salt])
                (require-optional-library 'ffi-xxh32-string 'xxhash)
                (native value start stop salt)))))
  #|proc:ffi-xxh32-fxvector
  The `ffi-xxh32-fxvector` procedure calls the native xxhash operation `hash_XXH32_fxvector`.
  Parameters `value`, `start`, `stop`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-fxvector
    (let ([native (foreign-procedure "hash_XXH32_fxvector" (ptr int int unsigned-32) unsigned-32)])
      (lambda (value start stop salt)
        (pcheck ([integer? start] [integer? stop] [natural? salt])
                (require-optional-library 'ffi-xxh32-fxvector 'xxhash)
                (native value start stop salt)))))
  #|proc:ffi-xxh32-flvector
  The `ffi-xxh32-flvector` procedure calls the native xxhash operation `hash_XXH32_flvector`.
  Parameters `value`, `start`, `stop`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-flvector
    (let ([native (foreign-procedure "hash_XXH32_flvector" (ptr int int unsigned-32) unsigned-32)])
      (lambda (value start stop salt)
        (pcheck ([integer? start] [integer? stop] [natural? salt])
                (require-optional-library 'ffi-xxh32-flvector 'xxhash)
                (native value start stop salt)))))
  #|proc:ffi-xxh32-bytevector
  The `ffi-xxh32-bytevector` procedure calls the native xxhash operation `hash_XXH32_bytevector`.
  Parameters `value`, `start`, `stop`, `salt` are passed to the native operation in that order.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `salt` is the hash seed.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-bytevector
    (let ([native (foreign-procedure "hash_XXH32_bytevector" (ptr int int unsigned-32) unsigned-32)])
      (lambda (value start stop salt)
        (pcheck ([integer? start] [integer? stop] [natural? salt])
                (require-optional-library 'ffi-xxh32-bytevector 'xxhash)
                (native value start stop salt)))))

  (define ffi-xxh64-fixnum   (foreign-procedure "hash_XXH64_fixnum" (fixnum unsigned-64) ptr))
  (define ffi-xxh64-flonum   (foreign-procedure "hash_XXH64_flonum" (double unsigned-64) ptr))
  (define ffi-xxh64-ratnum   (foreign-procedure "hash_XXH64_ratnum" (fixnum fixnum unsigned-64) ptr))
  (define ffi-xxh64-cflonum  (foreign-procedure "hash_XXH64_cflonum" (double double unsigned-64) ptr))
  (define ffi-xxh64-string   (foreign-procedure "hash_XXH64_string" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-fxvector (foreign-procedure "hash_XXH64_fxvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-flvector (foreign-procedure "hash_XXH64_flvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-bytevector (foreign-procedure "hash_XXH64_bytevector" (ptr int int unsigned-64) ptr))

  (define ffi-xxh3-64-fixnum     (foreign-procedure "hash_XXH3_64_fixnum" (fixnum unsigned-64) ptr))
  (define ffi-xxh3-64-flonum     (foreign-procedure "hash_XXH3_64_flonum" (double unsigned-64) ptr))
  (define ffi-xxh3-64-ratnum     (foreign-procedure "hash_XXH3_64_ratnum" (fixnum fixnum unsigned-64) ptr))
  (define ffi-xxh3-64-cflonum    (foreign-procedure "hash_XXH3_64_cflonum" (double double unsigned-64) ptr))
  (define ffi-xxh3-64-string     (foreign-procedure "hash_XXH3_64_string" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-fxvector   (foreign-procedure "hash_XXH3_64_fxvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-flvector   (foreign-procedure "hash_XXH3_64_flvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-bytevector (foreign-procedure "hash_XXH3_64_bytevector" (ptr int int unsigned-64) ptr))


  (define-syntax define-hasher-scalar
    (syntax-rules ()
      [(_ name ffi x? cvt tag)
       (define-who name
         (case-lambda
           [(x) (name x 0)]
           [(x salt)
            (pcheck ([x? x] [fixnum? salt])
                    (ensure-xxhash who)
                    ;; salt + tag safe?
                    (ffi (cvt x) (fx+ salt tag)))]))]))

  (define-hasher-scalar xxhash32-fixnum   ffi-xxh32-fixnum fixnum? id 0)
  (define-hasher-scalar xxhash64-fixnum   ffi-xxh64-fixnum fixnum? id 0)
  (define-hasher-scalar xxhash3-64-fixnum ffi-xxh3-64-fixnum fixnum? id 0)

  (define-hasher-scalar xxhash32-bool   ffi-xxh32-fixnum boolean? (lambda (x) (if x 1 0)) 9)
  (define-hasher-scalar xxhash64-bool   ffi-xxh64-fixnum boolean? (lambda (x) (if x 1 0)) 9)
  (define-hasher-scalar xxhash3-64-bool ffi-xxh3-64-fixnum boolean? (lambda (x) (if x 1 0)) 9)

  (define-hasher-scalar xxhash32-char   ffi-xxh32-fixnum char? char->integer 11)
  (define-hasher-scalar xxhash64-char   ffi-xxh64-fixnum char? char->integer 11)
  (define-hasher-scalar xxhash3-64-char ffi-xxh3-64-fixnum char? char->integer 11)

  (define-hasher-scalar xxhash32-flonum   ffi-xxh32-flonum flonum? id 1)
  (define-hasher-scalar xxhash64-flonum   ffi-xxh64-flonum flonum? id 1)
  (define-hasher-scalar xxhash3-64-flonum ffi-xxh3-64-flonum flonum? id 1)


  (define-syntax define-hasher-rat/cfl
    (syntax-rules ()
      [(_ name ffi x? get-p1 get-p2 tag)
       (define-who name
         (case-lambda
           [(x) (name x 0)]
           [(x salt)
            (pcheck ([x? x] [fixnum? salt])
                    (ensure-xxhash who)
                    (let ([p1 (get-p1 x)] [p2 (get-p2 x)])
                      (ffi p1 p2 (fx+ salt tag))))]))]))

  (define-hasher-rat/cfl xxhash32-ratnum   ffi-xxh32-ratnum ratnum? numerator denominator 2)
  (define-hasher-rat/cfl xxhash64-ratnum   ffi-xxh64-ratnum ratnum? numerator denominator 2)
  (define-hasher-rat/cfl xxhash3-64-ratnum ffi-xxh3-64-ratnum ratnum? numerator denominator 2)

  (define-hasher-rat/cfl xxhash32-cflonum   ffi-xxh32-cflonum cflonum? real-part imag-part 3)
  (define-hasher-rat/cfl xxhash64-cflonum   ffi-xxh64-cflonum cflonum? real-part imag-part 3)
  (define-hasher-rat/cfl xxhash3-64-cflonum ffi-xxh3-64-cflonum cflonum? real-part imag-part 3)


  (define-syntax define-hasher-indexable
    (syntax-rules ()
      [(_ name ffi x? x-length tag)
       (define-who name
         (case-lambda
           [(x)
            (pcheck ([x? x]) (name x 0 0 (x-length x)))]
           [(x salt)
            (pcheck ([x? x]) (name x salt 0 (x-length x)))]
           [(x salt start)
            (pcheck ([x? x]) (name x salt start (x-length x)))]
           [(x salt start stop)
            (pcheck ([x? x] [hash-index? start stop] [fixnum? salt])
                    (ensure-xxhash who)
                    (let ([len (x-length x)])
                      (check-hash-slice who len start stop)
                      (ffi x start stop (fx+ salt tag))))]))]))

  (define-hasher-indexable xxhash32-string   ffi-xxh32-string string? string-length 4)
  (define-hasher-indexable xxhash64-string   ffi-xxh64-string string? string-length 4)
  (define-hasher-indexable xxhash3-64-string ffi-xxh3-64-string string? string-length 4)

  (define-hasher-indexable xxhash32-fxvector   ffi-xxh32-fxvector fxvector? fxvector-length 5)
  (define-hasher-indexable xxhash64-fxvector   ffi-xxh64-fxvector fxvector? fxvector-length 5)
  (define-hasher-indexable xxhash3-64-fxvector ffi-xxh3-64-fxvector fxvector? fxvector-length 5)

  (define-hasher-indexable xxhash32-flvector   ffi-xxh32-flvector flvector? flvector-length 6)
  (define-hasher-indexable xxhash64-flvector   ffi-xxh64-flvector flvector? flvector-length 6)
  (define-hasher-indexable xxhash3-64-flvector ffi-xxh3-64-flvector flvector? flvector-length 6)

  (define-hasher-indexable xxhash32-bytevector   ffi-xxh32-bytevector bytevector? bytevector-length 7)
  (define-hasher-indexable xxhash64-bytevector   ffi-xxh64-bytevector bytevector? bytevector-length 7)
  (define-hasher-indexable xxhash3-64-bytevector ffi-xxh3-64-bytevector bytevector? bytevector-length 7)


  (define-syntax define-file-hasher
    (syntax-rules ()
      [(_ name which)
       (define-who name
         (case-lambda
           [(path)
            (pcheck ([file-regular? path]) (name path 0 0 (file-size path)))]
           [(path salt)
            (pcheck ([file-regular? path]) (name path salt 0 (file-size path)))]
           [(path salt start)
            (pcheck ([file-regular? path]) (name path salt start (file-size path)))]
           [(path salt start stop)
            (pcheck ([file-regular? path] [fixnum? salt]
                     [(lambda (index) (and (fixnum? index) (fx>= index 0))) start stop])
              (check-hash-slice who (file-size path) start stop)
              (call-with-hasher 'which salt
                (lambda (hasher)
                  (let* ([buffer (make-bytevector 4096)]
                         [port (open-file-input-port path)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (set-port-position! port start)
                        (let loop ([remaining (fx- stop start)])
                          (unless (fx= remaining 0)
                            (let ([count (get-bytevector-n! port buffer 0
                                                          (fxmin remaining 4096))])
                              (when (or (eof-object? count) (fx= count 0))
                                (errorf who "unexpected end of file: ~s" path))
                              (hasher-update-bytevector! hasher buffer 0 count)
                              (loop (fx- remaining count))))))
                      (lambda () (close-port port)))))))]))]))


  #|doc
  |#
  (define-file-hasher xxhash32-file xxhash32)
  (define-file-hasher xxhash64-file xxhash64)
  (define-file-hasher xxhash3-64-file xxhash3-64)


;;;;===----------------------------------------------------------------------===
;;;;  incremental API
;;;;===----------------------------------------------------------------------===

  (define-record-type (hasher mk-hasher hasher?)
    (opaque #t)
    (fields ffi-ctx
            ffi-get
            ffi-finalize!
            ffi-reset!

            ffi-fixnum-update!
            ffi-flonum-update!
            ffi-ratnum-update!
            ffi-cflonum-update!
            ffi-string-update!
            ffi-fxvector-update!
            ffi-flvector-update!
            ffi-bytevector-update!

            (mutable finalized?)))


  #|proc:ffi-xxh32-create
  The `ffi-xxh32-create` procedure calls the native xxhash operation `hasher_XXH32_create`.
  Parameters `seed` are passed to the native operation in that order.
  `seed` is the hash seed.
  It returns an owned xxHash state handle, or zero when allocation fails.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-create
    (let ([native (foreign-procedure "hasher_XXH32_create" (unsigned-32) void*)])
      (lambda (seed)
        (pcheck ([natural? seed])
                (require-optional-library 'ffi-xxh32-create 'xxhash)
                (native seed)))))
  #|proc:ffi-xxh32-get
  The `ffi-xxh32-get` procedure calls the native xxhash operation `hasher_XXH32_get`.
  Parameters `context` are passed to the native operation in that order.
  `context` is the native context handle.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-get
    (let ([native (foreign-procedure "hasher_XXH32_get" (void*) unsigned-int)])
      (lambda (context)
        (pcheck ([natural? context])
                (require-optional-library 'ffi-xxh32-get 'xxhash)
                (native context)))))
  #|proc:ffi-xxh32-finalize!
  The `ffi-xxh32-finalize!` procedure calls the native xxhash operation `hasher_XXH32_finalize`.
  Parameters `context` are passed to the native operation in that order.
  `context` is the native context handle.
  It returns the unsigned xxHash value of the input using the supplied hash seed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-finalize!
    (let ([native (foreign-procedure "hasher_XXH32_finalize" (void*) unsigned-int)])
      (lambda (context)
        (pcheck ([natural? context])
                (require-optional-library 'ffi-xxh32-finalize! 'xxhash)
                (native context)))))
  #|proc:ffi-xxh32-reset!
  The `ffi-xxh32-reset!` procedure calls the native xxhash operation `hasher_XXH32_reset`.
  Parameters `context`, `seed` are passed to the native operation in that order.
  `context` is the native context handle.
  `seed` is the hash seed.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-reset!
    (let ([native (foreign-procedure "hasher_XXH32_reset" (void* unsigned-32) void)])
      (lambda (context seed)
        (pcheck ([natural? context] [natural? seed])
                (require-optional-library 'ffi-xxh32-reset! 'xxhash)
                (native context seed)))))
  #|proc:ffi-xxh32-fixnum-update!
  The `ffi-xxh32-fixnum-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_fixnum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-fixnum-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_fixnum" (void* fixnum int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [fixnum? value] [integer? tag])
                (require-optional-library 'ffi-xxh32-fixnum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh32-flonum-update!
  The `ffi-xxh32-flonum-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_flonum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-flonum-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_flonum" (void* double int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [flonum? value] [integer? tag])
                (require-optional-library 'ffi-xxh32-flonum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh32-ratnum-update!
  The `ffi-xxh32-ratnum-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_ratnum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-ratnum-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_ratnum" (void* fixnum fixnum int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [fixnum? value] [fixnum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh32-ratnum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh32-cflonum-update!
  The `ffi-xxh32-cflonum-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_cflonum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-cflonum-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_cflonum" (void* double double int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [flonum? value] [flonum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh32-cflonum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh32-string-update!
  The `ffi-xxh32-string-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_string`.
  Parameters `context`, `text`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `text` is the input text.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-string-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_string" (void* ptr int int int) void)])
      (lambda (context text start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh32-string-update! 'xxhash)
                (native context text start stop tag)))))
  #|proc:ffi-xxh32-fxvector-update!
  The `ffi-xxh32-fxvector-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_fxvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-fxvector-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_fxvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh32-fxvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh32-flvector-update!
  The `ffi-xxh32-flvector-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_flvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-flvector-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_flvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh32-flvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh32-bytevector-update!
  The `ffi-xxh32-bytevector-update!` procedure calls the native xxhash operation
  `hasher_XXH32_update_bytevector`.
  Parameters `context`, `bytevector`, `start`, `stop`, `tag` are passed to the native operation in
  that order.
  `context` is the native context handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh32-bytevector-update!
    (let ([native (foreign-procedure "hasher_XXH32_update_bytevector" (void* ptr int int int) void)])
      (lambda (context bytevector start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh32-bytevector-update! 'xxhash)
                (native context bytevector start stop tag)))))

  #|proc:ffi-xxh64-create
  The `ffi-xxh64-create` procedure calls the native xxhash operation `hasher_XXH64_create`.
  Parameters `seed` are passed to the native operation in that order.
  `seed` is the hash seed.
  It returns an owned xxHash state handle, or zero when allocation fails.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-create
    (let ([native (foreign-procedure "hasher_XXH64_create" (unsigned-64) void*)])
      (lambda (seed)
        (pcheck ([natural? seed])
                (require-optional-library 'ffi-xxh64-create 'xxhash)
                (native seed)))))
  (define ffi-xxh64-get       (foreign-procedure "hasher_XXH64_get" (void*) ptr))
  (define ffi-xxh64-finalize! (foreign-procedure "hasher_XXH64_finalize" (void*) ptr))
  #|proc:ffi-xxh64-reset!
  The `ffi-xxh64-reset!` procedure calls the native xxhash operation `hasher_XXH64_reset`.
  Parameters `context`, `seed` are passed to the native operation in that order.
  `context` is the native context handle.
  `seed` is the hash seed.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-reset!
    (let ([native (foreign-procedure "hasher_XXH64_reset" (void* unsigned-64) void)])
      (lambda (context seed)
        (pcheck ([natural? context] [natural? seed])
                (require-optional-library 'ffi-xxh64-reset! 'xxhash)
                (native context seed)))))
  #|proc:ffi-xxh64-fixnum-update!
  The `ffi-xxh64-fixnum-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_fixnum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-fixnum-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_fixnum" (void* fixnum int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [fixnum? value] [integer? tag])
                (require-optional-library 'ffi-xxh64-fixnum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh64-flonum-update!
  The `ffi-xxh64-flonum-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_flonum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-flonum-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_flonum" (void* double int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [flonum? value] [integer? tag])
                (require-optional-library 'ffi-xxh64-flonum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh64-ratnum-update!
  The `ffi-xxh64-ratnum-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_ratnum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-ratnum-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_ratnum" (void* fixnum fixnum int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [fixnum? value] [fixnum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh64-ratnum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh64-cflonum-update!
  The `ffi-xxh64-cflonum-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_cflonum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-cflonum-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_cflonum" (void* double double int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [flonum? value] [flonum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh64-cflonum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh64-string-update!
  The `ffi-xxh64-string-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_string`.
  Parameters `context`, `text`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `text` is the input text.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-string-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_string" (void* ptr int int int) void)])
      (lambda (context text start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh64-string-update! 'xxhash)
                (native context text start stop tag)))))
  #|proc:ffi-xxh64-fxvector-update!
  The `ffi-xxh64-fxvector-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_fxvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-fxvector-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_fxvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh64-fxvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh64-flvector-update!
  The `ffi-xxh64-flvector-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_flvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-flvector-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_flvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh64-flvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh64-bytevector-update!
  The `ffi-xxh64-bytevector-update!` procedure calls the native xxhash operation
  `hasher_XXH64_update_bytevector`.
  Parameters `context`, `bytevector`, `start`, `stop`, `tag` are passed to the native operation in
  that order.
  `context` is the native context handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh64-bytevector-update!
    (let ([native (foreign-procedure "hasher_XXH64_update_bytevector" (void* ptr int int int) void)])
      (lambda (context bytevector start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh64-bytevector-update! 'xxhash)
                (native context bytevector start stop tag)))))

  #|proc:ffi-xxh3-64-create
  The `ffi-xxh3-64-create` procedure calls the native xxhash operation `hasher_XXH3_64_create`.
  Parameters `seed` are passed to the native operation in that order.
  `seed` is the hash seed.
  It returns an owned xxHash state handle, or zero when allocation fails.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-create
    (let ([native (foreign-procedure "hasher_XXH3_64_create" (unsigned-64) void*)])
      (lambda (seed)
        (pcheck ([natural? seed])
                (require-optional-library 'ffi-xxh3-64-create 'xxhash)
                (native seed)))))
  (define ffi-xxh3-64-get       (foreign-procedure "hasher_XXH3_64_get" (void*) ptr))
  (define ffi-xxh3-64-finalize! (foreign-procedure "hasher_XXH3_64_finalize" (void*) ptr))
  #|proc:ffi-xxh3-64-reset!
  The `ffi-xxh3-64-reset!` procedure calls the native xxhash operation `hasher_XXH3_64_reset`.
  Parameters `context`, `seed` are passed to the native operation in that order.
  `context` is the native context handle.
  `seed` is the hash seed.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-reset!
    (let ([native (foreign-procedure "hasher_XXH3_64_reset" (void* unsigned-64) void)])
      (lambda (context seed)
        (pcheck ([natural? context] [natural? seed])
                (require-optional-library 'ffi-xxh3-64-reset! 'xxhash)
                (native context seed)))))
  #|proc:ffi-xxh3-64-fixnum-update!
  The `ffi-xxh3-64-fixnum-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_fixnum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-fixnum-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_fixnum" (void* fixnum int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [fixnum? value] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-fixnum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh3-64-flonum-update!
  The `ffi-xxh3-64-flonum-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_flonum`.
  Parameters `context`, `value`, `tag` are passed to the native operation in that order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-flonum-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_flonum" (void* double int) void)])
      (lambda (context value tag)
        (pcheck ([natural? context] [flonum? value] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-flonum-update! 'xxhash)
                (native context value tag)))))
  #|proc:ffi-xxh3-64-ratnum-update!
  The `ffi-xxh3-64-ratnum-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_ratnum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-ratnum-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_ratnum" (void* fixnum fixnum int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [fixnum? value] [fixnum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-ratnum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh3-64-cflonum-update!
  The `ffi-xxh3-64-cflonum-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_cflonum`.
  Parameters `context`, `value`, `other-value`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `other-value` is a number.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-cflonum-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_cflonum" (void* double double int) void)])
      (lambda (context value other-value tag)
        (pcheck ([natural? context] [flonum? value] [flonum? other-value] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-cflonum-update! 'xxhash)
                (native context value other-value tag)))))
  #|proc:ffi-xxh3-64-string-update!
  The `ffi-xxh3-64-string-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_string`.
  Parameters `context`, `text`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `text` is the input text.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-string-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_string" (void* ptr int int int) void)])
      (lambda (context text start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-string-update! 'xxhash)
                (native context text start stop tag)))))
  #|proc:ffi-xxh3-64-fxvector-update!
  The `ffi-xxh3-64-fxvector-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_fxvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-fxvector-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_fxvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-fxvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh3-64-flvector-update!
  The `ffi-xxh3-64-flvector-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_flvector`.
  Parameters `context`, `value`, `start`, `stop`, `tag` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `value` is the value passed to the native operation.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-flvector-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_flvector" (void* ptr int int int) void)])
      (lambda (context value start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-flvector-update! 'xxhash)
                (native context value start stop tag)))))
  #|proc:ffi-xxh3-64-bytevector-update!
  The `ffi-xxh3-64-bytevector-update!` procedure calls the native xxhash operation
  `hasher_XXH3_64_update_bytevector`.
  Parameters `context`, `bytevector`, `start`, `stop`, `tag` are passed to the native operation in
  that order.
  `context` is the native context handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  `tag` is a number.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-xxh3-64-bytevector-update!
    (let ([native (foreign-procedure "hasher_XXH3_64_update_bytevector" (void* ptr int int int) void)])
      (lambda (context bytevector start stop tag)
        (pcheck ([natural? context] [integer? start] [integer? stop] [integer? tag])
                (require-optional-library 'ffi-xxh3-64-bytevector-update! 'xxhash)
                (native context bytevector start stop tag)))))


  (define check-finalized
    (lambda (who hasher)
      (when (hasher-finalized? hasher)
        (errorf who "hasher is already finalized"))))


  #|proc:make-hasher
  Create a native incremental hasher. Optional symbol `which` selects `xxhash32` (default),
  `xxhash64`, or `xxhash3-64`; fixnum `salt` defaults to zero. Return the owned hasher.
  Call `hasher-finalize!` to release it. A hasher must not be used concurrently.
  |#
  (define-who make-hasher
    (case-lambda
      [() (make-hasher 'xxhash32 0)]
      [(which) (make-hasher which 0)]
      [(which salt)
       (pcheck ([fixnum? salt])
               (check-hasher who which)
               (ensure-xxhash who)
               (case which
                 [xxhash32 (mk-hasher (ffi-xxh32-create salt)
                                      ffi-xxh32-get
                                      ffi-xxh32-finalize!
                                      ffi-xxh32-reset!

                                      ffi-xxh32-fixnum-update!
                                      ffi-xxh32-flonum-update!
                                      ffi-xxh32-ratnum-update!
                                      ffi-xxh32-cflonum-update!
                                      ffi-xxh32-string-update!
                                      ffi-xxh32-fxvector-update!
                                      ffi-xxh32-flvector-update!
                                      ffi-xxh32-bytevector-update!

                                      #f)]
                 [xxhash64 (mk-hasher (ffi-xxh64-create salt)
                                      ffi-xxh64-get
                                      ffi-xxh64-finalize!
                                      ffi-xxh64-reset!

                                      ffi-xxh64-fixnum-update!
                                      ffi-xxh64-flonum-update!
                                      ffi-xxh64-ratnum-update!
                                      ffi-xxh64-cflonum-update!
                                      ffi-xxh64-string-update!
                                      ffi-xxh64-fxvector-update!
                                      ffi-xxh64-flvector-update!
                                      ffi-xxh64-bytevector-update!

                                      #f)]
                 [xxhash3-64 (mk-hasher (ffi-xxh3-64-create salt)
                                        ffi-xxh3-64-get
                                        ffi-xxh3-64-finalize!
                                        ffi-xxh3-64-reset!

                                        ffi-xxh3-64-fixnum-update!
                                        ffi-xxh3-64-flonum-update!
                                        ffi-xxh3-64-ratnum-update!
                                        ffi-xxh3-64-cflonum-update!
                                        ffi-xxh3-64-string-update!
                                        ffi-xxh3-64-fxvector-update!
                                        ffi-xxh3-64-flvector-update!
                                        ffi-xxh3-64-bytevector-update!

                                        #f)]
                 [else (assert-unreachable)]))]))


  #|proc:hasher-get
  Return the current unsigned hash of the live `hasher` without releasing its native state.
  |#
  (define-who hasher-get
    (lambda (hasher)
      (pcheck ([hasher? hasher])
              (check-finalized who hasher)
              ((hasher-ffi-get hasher) (hasher-ffi-ctx hasher)))))


  (define-syntax define-hasher-update-scalar
    (syntax-rules ()
      [(_ name x? get-ffi cvt tag)
       (define-who name
         (lambda (hasher x)
           (pcheck ([hasher? hasher] [x? x])
                   (check-finalized who hasher)
                   ((get-ffi hasher) (hasher-ffi-ctx hasher) (cvt x) tag))))]))

  (define-hasher-update-scalar hasher-update-fixnum! fixnum?  hasher-ffi-fixnum-update! id 0)
  (define-hasher-update-scalar hasher-update-bool!   boolean? hasher-ffi-fixnum-update! (lambda (x) (if x 1 0)) 1)
  (define-hasher-update-scalar hasher-update-char!   char?    hasher-ffi-fixnum-update! char->integer 2)
  (define-hasher-update-scalar hasher-update-flonum! flonum?  hasher-ffi-flonum-update! id 3)


  (define-syntax define-hasher-update-rat/cfl
    (syntax-rules ()
      [(_ name x? get-ffi get-x get-y tag)
       (define-who name
         (lambda (hasher x)
           (pcheck ([hasher? hasher] [x? x])
                   (check-finalized who hasher)
                   ((get-ffi hasher)
                    (hasher-ffi-ctx hasher) (get-x x) (get-y x) tag))))]))

  (define-hasher-update-rat/cfl hasher-update-ratnum!  ratnum?  hasher-ffi-ratnum-update!  numerator denominator 4)
  (define-hasher-update-rat/cfl hasher-update-cflonum! cflonum? hasher-ffi-cflonum-update! real-part imag-part 5)


  (define-syntax define-hasher-update-indexable
    (syntax-rules ()
      [(_ name x? get-ffi x-length tag)
       (define-who name
         (case-lambda
           [(hasher x)
            (pcheck ([hasher? hasher] [x? x])
                    (name hasher x 0 (x-length x)))]
           [(hasher x start)
            (pcheck ([hasher? hasher] [x? x])
                    (name hasher x start (x-length x)))]
           [(hasher x start stop)
            (pcheck ([hasher? hasher] [x? x] [hash-index? start stop])
                    (check-finalized who hasher)
                    (let ([len (x-length x)])
                      (check-hash-slice who len start stop)
                      ((get-ffi hasher) (hasher-ffi-ctx hasher) x start stop tag)))]))]))

  (define-hasher-update-indexable hasher-update-string!   string?   hasher-ffi-string-update!   string-length 4)
  (define-hasher-update-indexable hasher-update-fxvector! fxvector? hasher-ffi-fxvector-update! fxvector-length 5)
  (define-hasher-update-indexable hasher-update-flvector! flvector? hasher-ffi-flvector-update! flvector-length 6)
  (define-hasher-update-indexable hasher-update-bytevector! bytevector? hasher-ffi-bytevector-update! bytevector-length 7)


  #|proc:hasher-finalize!
  Return the final unsigned hash of the live `hasher` and release its native state.
  Further operations on this hasher raise an error.
  |#
  (define-who hasher-finalize!
    (lambda (hasher)
      (pcheck ([hasher? hasher])
              (check-finalized who hasher)
              (let ([res ((hasher-ffi-finalize! hasher) (hasher-ffi-ctx hasher))])
                (hasher-finalized?-set! hasher #t)
                res))))


  #|proc:hasher-reset!
  Reset the live `hasher` to an empty input with optional fixnum `salt` (default zero).
  Return unspecified values. A finalized hasher cannot be reset.
  |#
  (define-who hasher-reset!
    (case-lambda
      [(hasher) (hasher-reset! hasher 0)]
      [(hasher salt)
       (pcheck ([hasher? hasher] [fixnum? salt])
               (check-finalized who hasher)
               ((hasher-ffi-reset! hasher) (hasher-ffi-ctx hasher) salt))]))


  #|proc:call-with-hasher
  Call `proc` with one live hasher and return its unsigned hash, ignoring callback results.
  Optional symbol `which` selects the algorithm (default `xxhash32`); fixnum `salt` defaults
  to zero. `proc` has signature `(lambda (hasher) ...)` and may update or reset its hasher.
  The scope owns and finalizes the hasher on normal return, exception, or continuation exit.
  The callback must not finalize it or use it outside the scope. Reentry raises an error.
  |#
  (define-who call-with-hasher
    (case-lambda
      [(proc) (call-with-hasher 'xxhash32 0 proc)]
      [(which proc) (call-with-hasher which 0 proc)]
      [(which salt proc)
       (pcheck ([fixnum? salt] [procedure? proc])
               (check-hasher who which)
               (let ([hasher (make-hasher which salt)])
                 (dynamic-wind (lambda () (check-finalized who hasher))
                               (lambda ()
                                 (proc hasher)
                                 (hasher-get hasher))
                               (lambda ()
                                 (unless (hasher-finalized? hasher)
                                   (hasher-finalize! hasher))))))]))


  )
