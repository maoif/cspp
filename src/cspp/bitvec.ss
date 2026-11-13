(library (cspp bitvec)
  (export bitvec make-bitvec make-full-bitvec bitvec? bitvec-size bitvec-bound bitvec-empty?
          bitvec-set! bitvec-unset! bitvec-flip!
          bitvec-set? bitvec-clear! bitvec-copy bitvec-copy!

          bitvec-or bitvec-and bitvec-xor bitvec-not

          bitvec-andmap bitvec-ormap
          bitvec-for-each bitvec-fold-left bitvec-fold-right

          bitvec->list)
  (import (cspp chez)
          (cspp utils)
          (cspp private common)
          (cspp vector)
          (only (cspp iter) iter-register-source! make-iter iter-end)
          (only (cspp navigator) nav-register-set!))

  ;; Bitvector: finite dense bit set.
  ;; Use a fxvector rather than reply on bitwise-* procs to
  ;; reduce number of intermediate objects.

  ;; TODO ranged ops


  #|record:$bitvec
  Dense mutable bit-set record with a fixed exclusive bound.
  |#
  (define-record-type ($bitvec mk-bitvec bitvec?)
    (fields
     ;; #bits actually allowed
     (immutable bound $bitvec-bound)
     ;; fxvector
     (mutable data bitvec-data bitvec-data-set!)))

  #|proc:bitvec-bound
  Return the maximum exclusive bit index accepted by `bv`.
  |#
  (define-who bitvec-bound
    (lambda (bv)
      (pcheck ([bitvec? bv])
              ($bitvec-bound bv))))

  (define all-naturals?
    (lambda (args) (andmap natural? args)))
  (define all-bitvecs?
    (lambda (args) (andmap bitvec? args)))

  (define *num-bits* (integer-length (most-positive-fixnum)))

  (define get-index
    (lambda (x) (quotient x *num-bits*)))
  (define get-index/error
    (lambda (who i bd)
      (if (>= i bd)
          (errorf who "index ~a out of bound ~a" i bd)
          (get-index i))))
  (define get-offset
    (lambda (i) (modulo i *num-bits*)))


  #|proc:make-bitvec
  Construct a bitvec with bound `bound`.
  `bound` must be a natural number.
  |#
  (define-who make-bitvec
    (lambda (bound)
      (pcheck ([natural? bound])
              (let ([size (fx1+ (get-index (fx1- bound)))])
                (mk-bitvec bound (make-fxvector size 0))))))


  #|proc:make-full-bitvec
  Construct a full bitvec with bound `bound`, that is,
  all `bound` bits in the bitvec are set.
  `bound` must be a natural number.
  |#
  (define-who make-full-bitvec
    (lambda (bound)
      (pcheck ([natural? bound])
              (let ([size (if (fx= 0 bound) 0 (fx1+ (get-index (fx1- bound))))])
                ;; there may be extra bits set in the last fixnum
                (mk-bitvec bound (make-fxvector size (most-positive-fixnum)))))))


  #|proc:bitvec
  Construct a bitvec from the list of naturals in `args`.
  The bound of the bitvec is set to the largest number in `args` plus 1.
  |#
  (define-who bitvec
    (lambda args
      (pcheck ([all-naturals? args])
              (if (= 0 (length args))
                  (mk-bitvec 0 (make-fxvector 0))
                  (let* ([bd (fx1+ (apply max args))]
                         [bv (mk-bitvec bd (make-fxvector (fx1+ (get-index (fx1- bd))) 0))]
                         [data (bitvec-data bv)])
                    (for-each (lambda (x)
                                (let ([i (get-index x)] [off (get-offset x)])
                                  (fxvector-set! data i (fxlogbit1 off (fxvector-ref data i)))))
                              args)
                    bv)))))


  #|proc:bitvec-set!
  Set the `i`th bit in the bitvec `bv`.
  `i` must be a natural number less than the bound of bv`.
  |#
  (define-who bitvec-set!
    (lambda (bv i)
      (pcheck ([bitvec? bv] [natural? i])
              (let ([data (bitvec-data bv)] [bd (bitvec-bound bv)])
                (let ([idx (get-index/error who i bd)]
                      [off (get-offset i)])
                  (fxvector-set! data idx (fxlogbit1 off (fxvector-ref data idx))))))))


  #|proc:bitvec-unset!
  Unset the `i`th bit in the bitvec `bv`.
  `i` must be a natural number less than the bound of bv`.
  |#
  (define-who bitvec-unset!
    (lambda (bv i)
      (pcheck ([bitvec? bv] [natural? i])
              (let ([data (bitvec-data bv)] [bd (bitvec-bound bv)])
                (let ([idx (get-index/error who i bd)]
                      [off (get-offset i)])
                  (fxvector-set! data idx (fxlogbit0 off (fxvector-ref data idx))))))))


  #|proc:bitvec-set?
  Test if the `i`th bit in the bitvec `bv` is set.
  `i` must be a natural number less than the bound of bv`.
  |#
  (define-who bitvec-set?
    (lambda (bv i)
      (pcheck ([bitvec? bv] [natural? i])
              (let ([data (bitvec-data bv)] [bd (bitvec-bound bv)])
                (let ([idx (get-index/error who i bd)]
                      [off (get-offset i)])
                  (fxlogbit? off (fxvector-ref data idx)))))))


  #|proc:bitvec-flip!
  Flip the `i`th bit in the bitvec `bv`.
  `i` must be a natural number less than the bound of bv`.
  |#
  (define-who bitvec-flip!
    (lambda (bv i)
      (pcheck ([bitvec? bv] [natural? i])
              (let ([data (bitvec-data bv)] [bd (bitvec-bound bv)])
                (let* ([idx (get-index/error who i bd)]
                       [off (get-offset i)]
                       [n (fxvector-ref data idx)])
                  (if (fxlogbit? off n)
                      (fxvector-set! data idx (fxlogbit0 off n))
                      (fxvector-set! data idx (fxlogbit1 off n))))))))


  #|proc:bitvec-size
  Get the number of set bits in the bitvev `bv`.
  |#
  (define-who bitvec-size
    (lambda (bv)
      (pcheck ([bitvec? bv])
              (let* ([data (bitvec-data bv)] [bd (bitvec-bound bv)]
                     [len (fxvector-length data)]
                     [last-i (fx1- bd)]
                     [last-idx (get-index last-i)]
                     [last-off (get-offset last-i)])
                (if (fx= len 0)
                    0
                    (let loop ([i 0] [c 0])
                      (if (fx= i last-idx)
                          ;; handle bound carefully
                          (fx+ c (fxpopcount
                                  (fxbit-field
                                   (fxvector-ref data i) 0 (fx1+ last-off))))
                          (loop (fx1+ i) (fx+ c (fxpopcount (fxvector-ref data i)))))))))))


  #|proc:bitvec-clear!
  Clear all set bits in the bitvec `bv`.
  |#
  (define-who bitvec-clear!
    (lambda (bv)
      (pcheck ([bitvec? bv])
              (fxvector-fill! (bitvec-data bv) 0))))


  #|proc:bitvec-empty?
  Check if none of the bits in the bitvec `bv` is set.
  |#
  (define-who bitvec-empty?
    (lambda (bv)
      (pcheck ([bitvec? bv])
              (fx= 0 (bitvec-size bv)))))


  #|proc:bitvec-copy
  Make a copy of the bitvec `bv`.
  |#
  (define-who bitvec-copy
    (lambda (bv)
      (pcheck ([bitvec? bv])
              (mk-bitvec (bitvec-bound bv)
                         (fxvector-copy (bitvec-data bv))))))


  #|proc:bitvec-copy!
  Copy the bits in `src` from indices src-start, ..., src-start + k - 1
  to consecutive indices in `tgt` starting at `tgt-start`.

  `src` and `tgt` must be bitvecs.
  `src-start`, `tgt-start`, and `k` must be exact nonnegative integers.
  The sum of `src-start` and `k` must not exceed the bound of `src`,
  and the sum of `tgt-start` and `k` must not exceed the bound of `tgt`.

  `src` and `tgt` may or may not be the same bitvec.
  |#
  (define-who bitvec-copy!
    (lambda (src src-start tgt tgt-start k)
      (pcheck ([bitvec? src tgt] [natural? src-start tgt-start k])
              (let ([bd1 (bitvec-bound src)] [data1 (bitvec-data src)]
                    [bd2 (bitvec-bound tgt)] [data2 (bitvec-data tgt)])
                (when (> (fx+ src-start k) bd1)
                  (errorf who "range ~a is too large in source bitvec" k))
                (when (> (fx+ tgt-start k) bd2)
                  (errorf who "range ~a is too large in target bitvec" k))
                (when (fx> k 0)
                  (let* ([same? (eq? src tgt)]
                         [backward? (and same?
                                         (fx> tgt-start src-start)
                                         (fx< tgt-start (fx+ src-start k)))])
                    (if backward?
                        (let loop ([j (fx1- k)])
                          (when (fx>= j 0)
                            (if (bitvec-set? src (fx+ src-start j))
                                (bitvec-set! tgt (fx+ tgt-start j))
                                (bitvec-unset! tgt (fx+ tgt-start j)))
                            (loop (fx1- j))))
                        (let loop ([j 0])
                          (when (fx< j k)
                            (if (bitvec-set? src (fx+ src-start j))
                                (bitvec-set! tgt (fx+ tgt-start j))
                                (bitvec-unset! tgt (fx+ tgt-start j)))
                            (loop (fx1+ j)))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   logical operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:bitvec-and
  Compute the logical and of the given bitvecs.
  If only one bitvec is given, it is returned directly.
  The bound of the resulting bitvec equals the smallest bound of those of the inputs.
  |#
  (define-who bitvec-and
    (case-lambda
      [(bv0)
       (pcheck ([bitvec? bv0]) bv0)]
      [(bv0 bv1)
       (pcheck ([bitvec? bv0 bv1])
               (let* ([newbd (min (bitvec-bound bv0) (bitvec-bound bv1))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [data0 (bitvec-data bv0)]
                      [data1 (bitvec-data bv1)]
                      [last-index (get-index (fx1- newbd))]
                      [remaining-bits (fx1+ (get-offset (fx1- newbd)))])
                 (let loop ([i 0])
                   (if (fx= i last-index)
                       (fxvector-set! newdata i
                                      ;; take only the remaining-bits
                                      (fxlogand (sub1 (ash 1 remaining-bits))
                                                (fxvector-ref data0 i)
                                                (fxvector-ref data1 i)))
                       (begin (fxvector-set! newdata i (fxlogand (fxvector-ref data0 i)
                                                                 (fxvector-ref data1 i)))
                              (loop (fx1+ i)))))
                 newbv))]
      [(bv0 . bv*)
       (pcheck ([bitvec? bv0] [all-bitvecs? bv*])
               (let* ([bv* (cons bv0 bv*)]
                      [newbd (apply min (map bitvec-bound bv*))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [data* (map bitvec-data bv*)]
                      [last-index (get-index (fx1- newbd))]
                      [remaining-bits (fx1+ (get-offset (fx1- newbd)))])
                 (let loop ([i 0])
                   (if (fx= i last-index)
                       (fxvector-set! newdata i
                                      ;; take only the remaining-bits
                                      (apply fxlogand (sub1 (ash 1 remaining-bits))
                                             (map (lambda (d) (fxvector-ref d i)) data*)))
                       (begin (fxvector-set! newdata i (apply fxlogand
                                                              (map (lambda (d) (fxvector-ref d i)) data*)))
                              (loop (fx1+ i)))))
                 newbv))]))


  (define gen-get-data
    (lambda (bv)
      (lambda (i)
        (let* ([bd (bitvec-bound bv)]
               [data (bitvec-data bv)]
               ;; the last index of the last bit in the fxvector
               [last-index (get-index (fx1- bd))]
               ;; one bit to the left of the position of the last in the fixnum
               [remaining-bits (fx1+ (get-offset (fx1- bd)))])
          (cond [(< i last-index) (fxvector-ref data i)]
                [(= i last-index) (fxlogand (fxvector-ref data i)
                                            ;; (ash 1 remaining-bits) may exceed fixnum range
                                            (sub1 (ash 1 remaining-bits)))]
                [else 0])))))


  #|proc:bitvec-or
  Compute the logical or (inclusive) of the given bitvecs.
  If only one bitvec is given, it is returned directly.
  The bound of the resulting bitvec equals the largest bound of those of the inputs.
  |#
  (define-who bitvec-or
    (case-lambda
      [(bv0)
       (pcheck ([bitvec? bv0]) bv0)]
      [(bv0 bv1)
       (pcheck ([bitvec? bv0 bv1])
               (let* ([newbd (max (bitvec-bound bv0) (bitvec-bound bv1))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [last-index (get-index (fx1- newbd))])
                 (define get-data0 (gen-get-data bv0))
                 (define get-data1 (gen-get-data bv1))
                 (let loop ([i 0])
                   (when (fx<= i last-index)
                     (begin (fxvector-set! newdata i (fxlogior (get-data0 i) (get-data1 i)))
                            (loop (fx1+ i)))))
                 newbv))]
      [(bv0 . bv*)
       (pcheck ([bitvec? bv0] [all-bitvecs? bv*])
               (let* ([bv* (cons bv0 bv*)]
                      [newbd (apply max (map bitvec-bound bv*))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [data* (map bitvec-data bv*)]
                      [last-index (get-index (fx1- newbd))])
                 (define get-data* (map gen-get-data bv*))
                 (let loop ([i 0])
                   (when (fx<= i last-index)
                     (fxvector-set! newdata i
                                    (apply fxlogior
                                           (map (lambda (g) (g i)) get-data*)))
                     (loop (fx1+ i))))
                 newbv))]))


  #|proc:bitvec-xor
  Compute the logical xor of the given bitvecs.
  If only one bitvec is given, it is returned directly.
  The bound of the resulting bitvec equals the largest bound of those of the inputs.
  |#
  (define-who bitvec-xor
    (case-lambda
      [(bv0)
       (pcheck ([bitvec? bv0]) bv0)]
      [(bv0 bv1)
       (pcheck ([bitvec? bv0 bv1])
               (let* ([newbd (max (bitvec-bound bv0) (bitvec-bound bv1))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [last-index (get-index (fx1- newbd))])
                 (define get-data0 (gen-get-data bv0))
                 (define get-data1 (gen-get-data bv1))
                 (let loop ([i 0])
                   (when (fx<= i last-index)
                     (begin (fxvector-set! newdata i (fxlogxor (get-data0 i) (get-data1 i)))
                            (loop (fx1+ i)))))
                 newbv))]
      [(bv0 . bv*)
       (pcheck ([bitvec? bv0] [all-bitvecs? bv*])
               (let* ([bv* (cons bv0 bv*)]
                      [newbd (apply max (map bitvec-bound bv*))]
                      [newbv (make-bitvec newbd)]
                      [newdata (bitvec-data newbv)]
                      [data* (map bitvec-data bv*)]
                      [last-index (get-index (fx1- newbd))])
                 (define get-data* (map gen-get-data bv*))
                 (let loop ([i 0])
                   (when (fx<= i last-index)
                     (fxvector-set! newdata i
                                    (apply fxlogxor
                                           (map (lambda (g) (g i)) get-data*)))
                     (loop (fx1+ i))))
                 newbv))]))


  #|proc:bitvec-not
  Return a new bitvec that has all bits in the input bitvec `bv` inverted/flipped.
  |#
  (define-who bitvec-not
    (lambda (bv)
      (pcheck ([bitvec? bv])
              (let* ([newbd (bitvec-bound bv) ]
                     [newbv (make-bitvec newbd)]
                     [newdata (bitvec-data newbv)]
                     [data (bitvec-data bv)]
                     [last-index (get-index (fx1- newbd))]
                     [remaining-bits (fx1+ (get-offset (fx1- newbd)))])
                (let loop ([i 0])
                  (if (fx= i last-index)
                      (fxvector-set! newdata i
                                     (fxlogand (sub1 (ash 1 remaining-bits))
                                               (fxlognot (fxvector-ref data i))))
                      (begin (fxvector-set! newdata i (fxlogand (fxlognot (fxvector-ref data i))
                                                                (most-positive-fixnum)))
                             (loop (fx1+ i)))))
                newbv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; count the number of leading zeros in non-negative fixnum `n`
  ;; Note that the starting position is 59 (leftmost bit of (most-positive-fixnum)).
  (define fxclz+
    (lambda (n)
      ;; TODO optimize
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            (fx- *num-bits* i)
            (loop (fx1+ i) (fxsrl n 1))))))


  ;; iterate indices of set bits inside a fixnum
  (define bits-for-each
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (unless (fx= n 0)
          (when (fxlogbit? 0 n)
            (proc i))
          (loop (fx1+ i) (fxsrl n 1))))))
  (define bits-andmap
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            #t
            (if (fxlogbit? 0 n)
                (and (proc i)
                     (loop (fx1+ i) (fxsrl n 1)))
                (loop (fx1+ i) (fxsrl n 1)))))))
  (define bits-ormap
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            #f
            (if (fxlogbit? 0 n)
                (or (proc i)
                    (loop (fx1+ i) (fxsrl n 1)))
                (loop (fx1+ i) (fxsrl n 1)))))))
  (define bits-fold-left
    (lambda (proc acc n)
      (let loop ([acc acc] [i 0] [n n])
        (if (fx= n 0)
            acc
            (if (fxlogbit? 0 n)
                (loop (proc acc i) (fx1+ i) (fxsrl n 1))
                (loop acc (fx1+ i) (fxsrl n 1)))))))
  (define bits-fold-right
    (lambda (proc acc n)
      (let loop ([acc acc] [i (fx- *num-bits* (fxclz+ n) 1)])
        (if (fx< i 0)
            acc
            (if (fxlogbit? i n)
                (loop (proc i acc) (fx1- i))
                (loop acc (fx1- i)))))))


  #|proc:bitvec-andmap
  Apply `proc` to set bit indexes in ascending order and return their conjunction.
  |#
  (define-who bitvec-andmap
    (lambda (proc bv)
      (pcheck ([procedure? proc] [bitvec? bv])
              (let* ([data (bitvec-data bv)]
                     [last-index (get-index (fx1- (bitvec-bound bv)))])
                (let loop ([i 0])
                  (let ([k (* i *num-bits*)])
                    (if (fx> i last-index)
                        #t
                        (and (bits-andmap (lambda (i) (proc (+ i k)))
                                          (fxvector-ref data i))
                             (loop (fx1+ i))))))))))


  #|proc:bitvec-ormap
  Apply `proc` to set bit indexes in ascending order and return their disjunction.
  |#
  (define-who bitvec-ormap
    (lambda (proc bv)
      (pcheck ([procedure? proc] [bitvec? bv])
              (let* ([data (bitvec-data bv)]
                     [last-index (get-index (fx1- (bitvec-bound bv)))])
                (let loop ([i 0])
                  (let ([k (* i *num-bits*)])
                    (if (fx> i last-index)
                        #f
                        (or (bits-ormap (lambda (i) (proc (+ i k)))
                                        (fxvector-ref data i))
                            (loop (fx1+ i))))))))))


    #|proc:bitvec-for-each
    Apply the unary procedure `proc` to the indices of the set bits
    in bitvec `bv` for side effects, from left to right.
    |#
    (define-who bitvec-for-each
      (lambda (proc bv)
        (pcheck ([procedure? proc] [bitvec? bv])
                (let* ([data (bitvec-data bv)]
                       [last-index (get-index (fx1- (bitvec-bound bv)))])
                  (let loop ([i 0])
                    (let ([k (* i *num-bits*)])
                      (unless (fx> i last-index)
                        (and (bits-for-each (lambda (i) (proc (+ i k)))
                                            (fxvector-ref data i))
                             (loop (fx1+ i))))))))))


    #|proc:bitvec-fold-left
    Fold set bit indexes in ascending order with `proc`, returning the accumulator.
    |#
    (define-who bitvec-fold-left
      (lambda (proc acc bv)
        (pcheck ([procedure? proc] [bitvec? bv])
                (let* ([data (bitvec-data bv)]
                       [last-index (get-index (fx1- (bitvec-bound bv)))])
                  (let loop ([acc acc] [i 0])
                    (let ([k (* i *num-bits*)])
                      (if (fx> i last-index)
                          acc
                          (loop (bits-fold-left (lambda (acc i) (proc acc (+ i k)))
                                                acc (fxvector-ref data i))
                                (fx1+ i)))))))))


    #|proc:bitvec-fold-right
    Fold set bit indexes in descending order with `proc`, returning the accumulator.
    |#
    (define-who bitvec-fold-right
      (lambda (proc acc bv)
        (pcheck ([procedure? proc] [bitvec? bv])
                (let* ([data (bitvec-data bv)]
                       [last-index (get-index (fx1- (bitvec-bound bv)))])
                  (let loop ([acc acc] [i last-index])
                    (let ([k (* i *num-bits*)])
                      (if (fx< i 0)
                          acc
                          (loop (bits-fold-right (lambda (i acc) (proc (+ i k) acc))
                                                 acc (fxvector-ref data i))
                                (fx1- i)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    #|proc:bitvec->list
    Convert the indices of set bits in the bitvec `bv` into a list,
    in ascending order.
    |#
    (define-who bitvec->list
      (lambda (bv)
        (pcheck ([bitvec? bv])
                (let* ([data (bitvec-data bv)]
                       [bd (bitvec-bound bv)]
                       [last-index (get-index bd)]
                       [remaining-bits (get-offset bd)])
                  (fxvfold-right/i
                   (lambda (i x acc)
                     (let ([left-i (* i *num-bits*)])
                       (bits-fold-right (lambda (j acc) (cons (+ left-i j) acc))
                                        acc
                                        (if (fx= i last-index)
                                            (fxlogand x (fx1- (fxsll 1 remaining-bits)))
                                            x))))
                   '() data)))))


;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

    (iter-register-source!
     bitvec?
     (lambda (bv)
       (let ([index 0])
         (make-iter
          (lambda ()
            (let loop ()
              (cond [(fx= index (bitvec-bound bv)) iter-end]
                    [(bitvec-set? bv index)
                     (let ([item index])
                       (set! index (fx1+ index))
                       item)]
                    [else
                     (set! index (fx1+ index))
                     (loop)])))
          (lambda () (set! index 0))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

    (nav-register-set!
     bitvec? bitvec->list
     (lambda (bv members)
       (let ([copy (bitvec-copy bv)])
         (bitvec-clear! copy)
         (for-each (lambda (member) (bitvec-set! copy member)) members)
         copy))
     (lambda (bv members)
       (bitvec-clear! bv)
       (for-each (lambda (member) (bitvec-set! bv member)) members)
       bv)
     (lambda (bv member)
       (let ([copy (bitvec-copy bv)])
         (bitvec-unset! copy member)
         copy))
     (lambda (bv member)
       (bitvec-unset! bv member)
       bv))

    (record-writer
     (type-descriptor $bitvec)
     (lambda (r p wr)
       (display "#[bitvec " p)
       (wr (bitvec->list r) p)
       (display "]" p)))


    (record-type-equal-procedure
     (type-descriptor $bitvec)
     (lambda (bv1 bv2 =?)
       (and (=? (bitvec-bound bv1) (bitvec-bound bv2))
            (=? (bitvec-data bv1) (bitvec-data bv2)))))



    )
