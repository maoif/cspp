(import (cspp) (cspp test)
        (cspp bittree))


(define iota50000 (iota 50000))


(mat bittree

     (bittree? (make-bittree))
     (bittree? (bittree 1 3 5))
     (bittree-empty? (make-bittree))
     (not (bittree-empty? (bittree 1 3 5 7)))

     (= 5 (bittree-size (bittree 1 2 3 4 5)))
     (= 4 (bittree-size (bittree 1 2 3 4 4)))
     (= 100 (bittree-size (apply bittree (iota 100))))

     (begin (define (test1 n)
              (let ([bt (make-bittree)]
                    [n* (iota n)])
                (for-each (lambda (x) (bittree-set! bt x)) n*)
                (and (andmap (lambda (x) (bittree-set? bt x)) n*)
                     (= n (bittree-size bt)))))
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

     (let ([bt (make-bittree)]
           [fxv (random-fxvector 10000)])
       (fxvfor-each (lambda (x) (bittree-set! bt x)) fxv)
       (fxvandmap (lambda (x) (bittree-set? bt x)) fxv))

     (let ([bt (make-bittree)]
           [fxv (random-fxvector 10000)])
       (fxvfor-each (lambda (x) (bittree-set! bt x)) fxv)
       (fxvandmap (lambda (x) (bittree-set? bt x)) fxv))

     ;; bignums
     (let ([bt (make-bittree)]
           [v (random-vector 50000)])
       (vmap! (lambda (x) (+ x (most-positive-fixnum))) v)
       (vfor-each (lambda (x) (bittree-set! bt x)) v)
       (vandmap (lambda (x) (bittree-set? bt x)) v))

     )


(mat bittree-unset!

     (let ([bt (make-bittree)]
           [v (random-fxvector 50000)])
       (fxvfor-each (lambda (x) (bittree-set! bt x)) v)
       (fxvfor-each (lambda (x) (bittree-unset! bt x)) v)
       (and (fxvandmap (lambda (x) (not (bittree-set? bt x))) v)
            (= 0 (bittree-size bt))))

     ;; bignums
     (let ([bt (make-bittree)]
           [v (random-vector 50000)])
       (vmap! (lambda (x) (+ x (most-positive-fixnum))) v)
       (vfor-each (lambda (x) (bittree-set! bt x)) v)
       (vfor-each (lambda (x) (bittree-unset! bt x)) v)
       (and (vandmap (lambda (x) (not (bittree-set? bt x))) v)
            (= 0 (bittree-size bt))))

     )


(mat bittree-flip!

     (let ([bt (make-bittree)])
       (for-each (lambda (x)
                   (when (odd? x)  (bittree-flip! bt x))
                   (when (even? x) (bittree-flip! bt x)))
                 iota50000)
       (andmap (lambda (x) (bittree-set? bt x)) iota50000))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (when (odd? x)  (bittree-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (even? x) (bittree-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (odd? x)  (bittree-flip! bt x))) iota50000)
       (for-each (lambda (x) (when (even? x) (bittree-flip! bt x))) iota50000)
       (= 0 (bittree-size bt)))

     )


(mat bittree-copy

     (let ([bt (make-bittree)]
           [fxv (random-fxvector 10000)])
       (fxvfor-each (lambda (x) (bittree-set! bt x)) fxv)
       (and (fxvandmap (lambda (x) (bittree-set? bt x)) fxv)
            (equal? bt (bittree-copy bt))))

     (let ([bt (make-bittree)]
           [fxv (random-fxvector 10000)])
       (fxvfor-each (lambda (x) (bittree-set! bt x)) fxv)
       (let ([newbt (bittree-copy bt)])
         (bittree-flip! bt 0)
         (not (equal? bt newbt))))

     )


(mat bittree-merge

     (error? (bittree-merge '()))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) iota50000)
       (eq? bt (bittree-merge bt)))

     (let ([bt* (map (lambda (x) (make-bittree)) (iota 10))]
           [fxv* (map (lambda (x) (random-fxvector 10000)) (iota 10))])
       (for-each (lambda (bt fxv)
                   (fxvfor-each (lambda (x)
                                  (bittree-set! bt x))
                                fxv))
                 bt* fxv*)
       (let ([newbt (apply bittree-merge bt*)])
         (andmap (lambda (bt)
                   (bittree-andmap (lambda (x)
                                     (bittree-set? newbt x))
                                   bt))
                 bt*)))

     )


(mat bittree-algebra
     (let ([left (bittree 1 2)] [right (bittree 2 3)])
       (and (equal? '(1 2 3) (bittree->list (bittree-or left right)))
            (equal? '(1 2) (bittree->list left))
            (equal? '(2 3) (bittree->list right))))
     (equal? '(2) (bittree->list (bittree-and (bittree 1 2) (bittree 2 3))))
     (equal? '(1 3) (bittree->list (bittree-xor (bittree 1 2) (bittree 2 3))))
     (error? (bittree-or (bittree) 'not-a-bittree))
     (error? (bittree-and 'not-a-bittree (bittree)))
     (error? (bittree-xor (bittree) 'not-a-bittree)))


(define n-1e5-1 (nums 0 #e1e5 1))
(define n-1e5-2 (nums 0 #e1e5 2))
(define n-1e5-even n-1e5-2)
(define n-1e5-odd  (nums 1 #e1e5 2))


(mat bittree->list

     (begin (define (test ls)
              (let ([bt (make-bittree)])
                (for-each (lambda (x) (bittree-set! bt x)) ls)
                (equal? ls (bittree->list bt))))
            #t)

     (test n-1e5-1)
     (test n-1e5-2)
     (test n-1e5-odd)

     )


(mat bittree-for-each

     (error? (bittree-for-each #f (make-bittree)))
     (error? (bittree-for-each fx1+ #f))

     (let ([bt (make-bittree)] [lb (make-list-builder)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-2)
       (bittree-for-each (lambda (i)
                           (lb i))
                         bt)
       (equal? n-1e5-2 (lb)))

     )


(mat bittree-fold-left

     (error? (bittree-fold-left + 0 1))
     (error? (bittree-fold-left #f 0 1))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-1)
       (let ([x (bittree-fold-left (lambda (acc i)
                                     (+ acc i))
                                   0 bt)]
             [y (apply + n-1e5-1)])
         (= x y)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-2)
       (let ([x (bittree-fold-left (lambda (acc i)
                                     (+ acc i))
                                   0 bt)]
             [y (apply + n-1e5-2)])
         (= x y)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-2)
       (let ([x (bittree-fold-left (lambda (acc i)
                                     (cons i acc))
                                   '() bt)])
         (equal? (reverse x) n-1e5-2)))

     )


(mat bittree-fold-right

     (error? (bittree-fold-right + 0 1))
     (error? (bittree-fold-right #f 0 1))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-1)
       (let ([x (bittree-fold-right (lambda (i acc)
                                      (+ acc i))
                                    0 bt)]
             [y (apply + n-1e5-1)])
         (= x y)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-2)
       (let ([x (bittree-fold-right (lambda (i acc)
                                      (+ acc i))
                                    0 bt)]
             [y (apply + n-1e5-2)])
         (= x y)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-2)
       (let ([x (bittree-fold-right (lambda (i acc)
                                      (cons i acc))
                                    '() bt)])
         (equal? x n-1e5-2)))

     )


(mat bittree-andmap

     (error? (bittree-andmap odd? 1))
     (error? (bittree-andmap 1 (make-bittree)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-even)
       (bittree-andmap (lambda (i) (even? i))
                       bt))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-odd)
       (bittree-andmap (lambda (i) (odd? i))
                       bt))

     )


(mat bittree-ormap

     (error? (bittree-ormap odd? 1))
     (error? (bittree-ormap 1 (make-bittree)))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-even)
       (bittree-set! bt 6665)
       (bittree-ormap (lambda (i) (odd? i))
                      bt))

     (let ([bt (make-bittree)])
       (for-each (lambda (x) (bittree-set! bt x)) n-1e5-odd)
       (bittree-set! bt 6666)
       (bittree-ormap (lambda (i) (even? i))
                      bt))

     )
