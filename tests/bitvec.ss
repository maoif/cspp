(import (cspp)
        (cspp io)
        (cspp test)
        (cspp bitvec))

(define iota50000 (iota 50000))
(define n-1e5-1 (nums 0 #e1e5 1))
(define n-1e5-2 (nums 0 #e1e5 2))
(define n-1e5-even n-1e5-2)
(define n-1e5-odd  (nums 1 #e1e5 2))

(mat bitvec-copy-ranges
     (let ([bits (bitvec 0 2)] [before '(0 2)])
       (bitvec-copy! bits 0 bits 0 0)
       (equal? before (bitvec->list bits)))

     (let ([source (make-bitvec 61)] [target (make-bitvec 61)])
       (for-each (lambda (index) (bitvec-set! source index)) '(0 1 59 60))
       (bitvec-copy! source 0 target 0 61)
       (equal? '(0 1 59 60) (bitvec->list target)))

     (let ([source (make-bitvec 60)] [target (make-bitvec 61)])
       (for-each (lambda (index) (bitvec-set! source index)) '(0 2 58 59))
       (bitvec-copy! source 0 target 1 60)
       (equal? '(1 3 59 60) (bitvec->list target)))

     (let ([bits (make-bitvec 61)])
       (for-each (lambda (index) (bitvec-set! bits index)) '(0 2 4))
       (bitvec-copy! bits 0 bits 1 5)
       (equal? '(0 1 3 5) (bitvec->list bits)))

     (let ([bits (make-bitvec 61)])
       (for-each (lambda (index) (bitvec-set! bits index)) '(1 3 5))
       (bitvec-copy! bits 1 bits 0 5)
       (equal? '(0 2 4 5) (bitvec->list bits)))

     ;; The source range exceeds its logical bit bound.
     (error? (bitvec-copy! (make-bitvec 59) 58 (make-bitvec 61) 0 2))

     ;; The target range exceeds its logical bit bound.
     (error? (bitvec-copy! (make-bitvec 61) 0 (make-bitvec 59) 58 2))
     )

(mat bitvec

     (error? (make-bitvec))

     (bitvec? (make-bitvec 10))
     (bitvec? (bitvec))
     (bitvec-empty? (bitvec))
     (bitvec? (bitvec 1 3 5))
     (bitvec-empty? (make-bitvec 10))
     (bitvec-empty? (make-full-bitvec 0))
     (not (bitvec-empty? (bitvec 1 3 5 7)))
     (= 64 (bitvec-bound (make-bitvec 64)))
     (error? (bitvec-bound 64))

     (andmap (lambda (n)
               (= n (bitvec-size (make-full-bitvec n))))
             (iota 600))

     (error? (let ([bv (make-bitvec 3)])
               (bitvec-set! bv 4)))
     (error? (bitvec-set! (make-bitvec 4) 4))

     (begin (define (test1 n)
              (let* ([n* (iota n)]
                     [bt (apply bitvec n*)])
                (for-each (lambda (x) (bitvec-set! bt x)) n*)
                (and (andmap (lambda (x) (bitvec-set? bt x)) n*)
                     (= n (bitvec-size bt)))))
            #t)
     (test1 (fx* 58 1))
     (test1 (fx* 59 1))
     (test1 (fx* 60 1))
     (test1 (fx* 61 1))
     (test1 (fx* 58 17))
     (test1 (fx* 59 17))
     (test1 (fx* 60 17))
     (test1 (fx* 61 17))
     (test1 (fx* 58 73))
     (test1 (fx* 59 73))
     (test1 (fx* 60 73))
     (test1 (fx* 61 73))

     (begin (define (test2 n)
              (let ([bt (make-bitvec n)]
                    [n* (iota n)])
                (for-each (lambda (x) (bitvec-set! bt x)) n*)
                (and (andmap (lambda (x) (bitvec-set? bt x)) n*)
                     (= n (bitvec-size bt)))))
            #t)
     (test2 (fx* 58 1))
     (test2 (fx* 59 1))
     (test2 (fx* 60 1))
     (test2 (fx* 61 1))
     (test2 (fx* 58 17))
     (test2 (fx* 59 17))
     (test2 (fx* 60 17))
     (test2 (fx* 61 17))
     (test2 (fx* 58 73))
     (test2 (fx* 59 73))
     (test2 (fx* 60 73))
     (test2 (fx* 61 73))


     (let* ([fxv (random-fxvector 10000 50000)]
            [bd (fx1+ (fxvmax fxv))]
            [bt (make-bitvec bd)])
       (fxvfor-each (lambda (x) (bitvec-set! bt x) (assert (bitvec-set? bt x))) fxv)
       (fxvandmap (lambda (x) (bitvec-set? bt x)) fxv))


     (begin (define bt (make-full-bitvec 64))
            (for-each (lambda (x) (bitvec-unset! bt x)) (iota 64))
            (bitvec-empty? bt))

     )


(mat bitvec-unset!

     (let* ([fxv (random-fxvector 10000 50000)]
            [bd (fx1+ (fxvmax fxv))]
            [bt (make-bitvec bd)])
       (fxvfor-each (lambda (x) (bitvec-set! bt x)
                            (assert (bitvec-set? bt x)))
                    fxv)
       (fxvfor-each (lambda (x) (bitvec-unset! bt x)
                            (assert (not (bitvec-set? bt x))))
                    fxv)
       (and (fxvandmap (lambda (x) (not (bitvec-set? bt x))) fxv)
            (= 0 (bitvec-size bt))))

     )


(mat bitvec-flip!

     (let* ([bt (make-bitvec 50000)])
       (for-each (lambda (x) (when (odd? x)  (bitvec-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (even? x) (bitvec-flip! bt x))) iota50000)
       (andmap (lambda (x) (bitvec-set? bt x)) iota50000))


     (let* ([bt (make-bitvec 50000)])
       (for-each (lambda (x) (when (odd? x)  (bitvec-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (even? x) (bitvec-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (odd? x)  (bitvec-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (even? x) (bitvec-flip! bt x))) iota50000)
       (and (= 0 (bitvec-size bt))
            (bitvec-empty? bt)))

     )


(mat bitvec-copy

     (let* ([fxv (random-fxvector 10000 50000)]
            [bd (fx1+ (fxvmax fxv))]
            [bt (make-bitvec bd)])
       (fxvfor-each (lambda (x) (bitvec-set! bt x)) fxv)
       (and (fxvandmap (lambda (x) (bitvec-set? bt x)) fxv)
            (equal? bt (bitvec-copy bt))))

     (let* ([fxv (random-fxvector 10000 50000)]
            [bd (fx1+ (fxvmax fxv))]
            [bt (make-bitvec bd)])
       (fxvfor-each (lambda (x) (bitvec-set! bt x)) fxv)
       (let ([newbt (bitvec-copy bt)])
         (bitvec-flip! bt 0)
         (not (equal? bt newbt))))

     )


(mat bitvec-and

     (let ([bv1 (bitvec 1 2 3)])
       (equal? bv1 (bitvec-and bv1)))

     (let ([bv1 (bitvec 1 2 3)]
           [bv2 (bitvec 1 2 3 4 5 6)])
       (equal? (bitvec-and bv1 bv2)
               bv1))

     (let ([bv1 (apply bitvec n-1e5-1)]
           [bv2 (apply bitvec n-1e5-odd)])
       (equal? (bitvec->list (bitvec-and bv1 bv2))
               n-1e5-odd))

     (let ([bv1 (apply bitvec (nums 100 200))]
           [bv2 (apply bitvec (nums 0 300))]
           [bv3 (apply bitvec (nums 50 250))]
           [bv4 (apply bitvec (nums 0 300))])
       (equal? (bitvec-and bv1 bv2 bv3 bv4) bv1))

     ;; fixnum boundary
     (let ([bv1 (apply bitvec (nums 0 120))]
           [bv2 (apply bitvec (nums 0 180))]
           [bv3 (apply bitvec (nums 0 240))]
           [bv4 (apply bitvec (nums 0 300))])
       (equal? (bitvec-and bv1 bv2 bv3 bv4) bv1))

     (let ([bv1 (apply bitvec (nums 0 500 4))]
           [bv2 (apply bitvec (nums 1 500 4))]
           [bv3 (apply bitvec (nums 2 500 4))]
           [bv4 (apply bitvec (nums 3 500 4))])
       (= 0 (bitvec-size (bitvec-and bv1 bv2 bv3 bv4))))

     (let ([bv1 (apply bitvec (nums 0 300 4))]
           [bv2 (apply bitvec (nums 1 300 4))]
           [bv3 (apply bitvec (nums 2 300 4))]
           [bv4 (apply bitvec (nums 3 300 4))])
       (= 0 (bitvec-size (bitvec-and bv1 bv2 bv3 bv4))))


     )


(mat bitvec-or

     (let ([bv1 (bitvec 1 2 3)])
       (equal? bv1 (bitvec-or bv1)))

     (let ([bv1 (bitvec 1 2 3)]
           [bv2 (bitvec 1 2 3 4 5 6)])
       (equal? (bitvec-or bv1 bv2) bv2))

     (let ([bv1 (apply bitvec (nums 0 500 4))]
           [bv2 (apply bitvec (nums 1 500 4))]
           [bv3 (apply bitvec (nums 2 500 4))]
           [bv4 (apply bitvec (nums 3 500 4))])
       (equal? (bitvec-or bv1 bv2 bv3 bv4)
               (apply bitvec (nums 0 500))))

     ;; fixnum boundary
     (let ([bv1 (apply bitvec (nums 0 300 4))]
           [bv2 (apply bitvec (nums 1 300 4))]
           [bv3 (apply bitvec (nums 2 300 4))]
           [bv4 (apply bitvec (nums 3 300 4))])
       (equal? (bitvec-or bv1 bv2 bv3 bv4)
               (apply bitvec (nums 0 300))))

     (let ([bv1 (apply bitvec (nums 0   100))]
           [bv2 (apply bitvec (nums 100 200))]
           [bv3 (apply bitvec (nums 200 300))]
           [bv4 (apply bitvec (nums 300 400))])
       (equal? (bitvec-or bv1 bv2 bv3 bv4)
               (apply bitvec (nums 0 400))))


     (begin (define bv1 (apply bitvec (nums 0   100)))
            (define bv2 (apply bitvec (nums 100 200)))
            (define bv3 (apply bitvec (nums 200 300)))
            (define bv4 (apply bitvec (nums 300 400)))
            (define bv (bitvec-or (bitvec-or (bitvec-or bv1 bv2) bv3) bv4))
            (equal? bv (apply bitvec (nums 0 400))))
     )


(mat bitvec-xor

     (let ([bv1 (bitvec 1 2 3)])
       (equal? bv1 (bitvec-xor bv1)))

     (let ([bv1 (bitvec 1 2 3)]
           [bv2 (bitvec 1 2 3 4 5 6)])
       (equal? (bitvec-xor bv1 bv2)
               (bitvec 4 5 6)))

     (let ([bv1 (bitvec 1 2 3 7 8 9)]
           [bv2 (bitvec 1 2 3 4 5 6)])
       (equal? (bitvec-xor bv1 bv2)
               (bitvec 4 5 6 7 8 9)))

     (let ([bv1 (apply bitvec (nums 0 500 4))]
           [bv2 (apply bitvec (nums 1 500 4))]
           [bv3 (apply bitvec (nums 2 500 4))]
           [bv4 (apply bitvec (nums 3 500 4))])
       (equal? (bitvec-xor bv1 bv2 bv3 bv4)
               (apply bitvec (nums 0 500))))

     ;; fixnum boundary
     (let ([bv1 (apply bitvec (nums 0 300 4))]
           [bv2 (apply bitvec (nums 1 300 4))]
           [bv3 (apply bitvec (nums 2 300 4))]
           [bv4 (apply bitvec (nums 3 300 4))])
       (equal? (bitvec-xor bv1 bv2 bv3 bv4)
               (apply bitvec (nums 0 300))))

     (let ([bv1 (apply bitvec (nums 0 200))]
           [bv2 (apply bitvec (nums 100 400))]
           [bv3 (apply bitvec (nums 300 500))])
       (displayln (bitvec-xor bv1 bv2 bv3))
       (equal? (bitvec-xor bv1 bv2 bv3)
               (bitvec-or (apply bitvec (nums 0 100))
                          (apply bitvec (nums 200 300))
                          (apply bitvec (nums 400 500)))))

     )


(mat bitvec-not

     (let ([bv1 (make-bitvec 10)])
       (equal? (apply bitvec (iota 10))
               (bitvec-not bv1)))

     (begin (define (test n)
              (let ([bv1 (make-bitvec n)])
                (equal? bv1
                        (bitvec-not (bitvec-not (bitvec-not (bitvec-not bv1)))))))
            #t)

     (test 10)
     (test 60) ;; fixnum boundary
     (test 119)
     (test 120) ;; fixnum boundary
     (test 121)

     )



(mat bitvec->list

     (error? (bitvec->list '()))

     (let* ([n* (iota 2000)]
            [bv (apply bitvec n*)])
       (equal? n* (bitvec->list bv)))

     (let* ([n* (iota 4000)]
            [bv (apply bitvec n*)])
       (equal? n* (bitvec->list bv)))

     )


(mat bitvec-andmap

     (error? (bitvec-andmap even? 1))
     (error? (bitvec-andmap 1 (make-bitvec 10)))

     (let ([bv (apply bitvec (nums 0 100 2))])
       (bitvec-andmap even? bv))
     (let ([bv (apply bitvec (nums 0 100 2))])
       (not (bitvec-andmap odd? bv)))
     (let ([bv (apply bitvec (nums 1 100 2))])
       (bitvec-andmap odd? bv))

     )


(mat bitvec-ormap

     (error? (bitvec-ormap even? 1))
     (error? (bitvec-ormap 1 (make-bitvec 10)))

     (let ([bv (apply bitvec (nums 0 100 2))])
       (bitvec-set! bv 97)
       (bitvec-ormap odd? bv))
     (let ([bv (apply bitvec (nums 0 100 2))])
       (not (bitvec-ormap odd? bv)))

     )


(mat bitvec-for-each

     (error? (bitvec-for-each 1 (bitvec 1 2 3)))
     (error? (bitvec-for-each + 1))

     (begin (define (test ls)
              (let ([bv (apply bitvec ls)]
                    [lb (make-list-builder)])
                (bitvec-for-each (lambda (x) (lb x)) bv)
                (equal? (lb) ls)))
            #t)
     (test iota50000)
     (test n-1e5-1)
     (test n-1e5-2)


     )


(mat bitvec-fold-left

     (error? (bitvec-fold-left 1 (bitvec 1 2 3)))
     (error? (bitvec-fold-left #f 1 (bitvec 1 2 3)))

     (begin (define (test ls)
              (let ([bv (apply bitvec ls)])
                (equal? (bitvec-fold-left (lambda (acc x) (cons x acc))
                                          '() bv)
                        (reverse ls))))
            #t)
     (test iota50000)
     (test n-1e5-1)
     (test n-1e5-2)

     )


(mat bitvec-fold-right

     (error? (bitvec-fold-right 1 (bitvec 1 2 3)))
     (error? (bitvec-fold-right #f 1 (bitvec 1 2 3)))

     (begin (define (test ls)
              (let* ([bv (apply bitvec ls)]
                     [res (bitvec-fold-right (lambda (x acc) (cons x acc))
                                             '() bv)])
                (equal? (bitvec-fold-right (lambda (x acc) (cons x acc))
                                           '() bv)
                        ls)))
            #t)
     (test iota50000)
     (test n-1e5-1)
     (test n-1e5-2)

     )
