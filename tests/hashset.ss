(import (cspp) (cspp hashset))



(define v10000  (fxvshuffle! (fxviota 10000)))
(define v100000 (fxvshuffle! (fxviota 100000)))


(mat hashset-basics

     (hashset-empty? (hashset))
     (hashset-empty? (make-eq-hashset))
     (hashset-empty? (make-eqv-hashset))
     (hashset-empty? (make-symbol-hashset))

     (let ([hs (apply hashset (iota 100))])
       (= 100 (hashset-size hs)))

     (let ([hs (apply hashset (iota 100))])
       (hashset-clear! hs)
       (= 0 (hashset-size hs)))

     )


(mat hashset-equal?

     (equal? (apply hashset (iota 100))
             (apply hashset (iota 100)))

     (not (equal? (apply hashset (iota 20))
                  (apply hashset (iota 10))))

     (let ([hs1 (make-hashset)] [hs2 (make-hashset)])
       (fxvfor-each (lambda (x) (hashset-add! hs1 x)) v100000)
       (fxvfor-each (lambda (x) (hashset-add! hs2 x)) (fxvshuffle! v100000))
       (equal? hs1 hs1))

     )


(mat hashset-delete!

     ;; absent value: deletion is idempotent
     (let ([hs (hashset 1)])
       (hashset-delete! hs 2)
       (and (= 1 (hashset-size hs))
            (hashset-contains? hs 1)))

     (let ([hs (make-hashset)])
       (fxvfor-each (lambda (x) (hashset-add! hs x)) v10000)
       (fxvfor-each (lambda (x) (when (odd? x) (hashset-delete! hs x))) v10000)
       (and (= (hashset-size hs) (fx/ (fxvector-length v10000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (hashset-contains? hs x) (not (hashset-contains? hs x))))
                       v10000)))

     (let ([hs (make-hashset)])
       (fxvfor-each (lambda (x) (hashset-add! hs x)) v100000)
       (fxvfor-each (lambda (x) (when (odd? x) (hashset-delete! hs x))) v100000)
       (and (= (hashset-size hs) (fx/ (fxvector-length v100000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (hashset-contains? hs x) (not (hashset-contains? hs x))))
                       v100000)))
     )


(mat hashset-contains?

     (error? (hashset-contains? 'bla 'bla))

     (let ([hs1 (apply hashset (iota 10))])
       (bool (andmap (lambda (x) (hashset-contains? hs1 x)) (iota 10))))

     (let ([hs1 (hashset 10 11 12)])
       (not (hashset-contains? hs1 1)))

     )


(mat hashset-contains/p?

     (error? (hashset-contains/p? 'bla 'bla))
     (error? (hashset-contains/p? (hashset) 'bla))

     (let ([hs1 (hashset 10 11 12)])
       (and (hashset-contains/p? hs1 odd?)
            (hashset-contains/p? hs1 even?)))

     (let ([hs1 (hashset 2 4 6 8)])
       (not (hashset-contains/p? hs1 odd?)))

     )


(mat hashset-search

     (error? (hashset-search 'bla 'bla))
     (error? (hashset-search (hashset) 'bla))

     (let ([hs1 (hashset 10 11 12)])
       (and (= 11 (hashset-search hs1 odd?))
            (= 10 (hashset-search hs1 even?))))

     (let ([hs1 (hashset 0 2 4 6)])
       (not (hashset-search hs1 odd?)))

     )


(mat hashset-search*

     (error? (hashset-search* 'bla 'bla))
     (error? (hashset-search* (hashset) 'bla))

     (let ([hs (apply hashset (nums 0 100))])
       (and (equal? (hashset-search* hs odd?)
                    (nums 1 100 2))
            (equal? (hashset-search* hs even?)
                    (nums 0 100 2))))


     (error? (hashset-search* 'bla 'bla 'bla))
     (error? (hashset-search* (hashset) odd? 'bla))

     (let ([hs (apply hashset (nums 0 100))]
           [hs1 (hashset)] [hs2 (hashset)])
       (hashset-search* hs odd? (lambda (x) (hashset-add! hs1 x)))
       (hashset-search* hs even? (lambda (x) (hashset-add! hs2 x)))
       (and (equal? hs1 (apply hashset (nums 1 100 2)))
            (equal? hs2 (apply hashset (nums 0 100 2)))))

     )


(mat hashset-filter

     (error? (hashset-filter))
     (error? (hashset-filter 'bla 'bla))
     (error? (hashset-filter (hashset) 42))
     (error? (hashset-filter 42 (hashset)))

     (let ([hs (apply hashset (nums 0 100))])
       (equal? (hashset-filter odd? hs)
               (apply hashset (nums 1 100 2))))

     )


(mat hashset-filter!

     (error? (hashset-filter!))
     (error? (hashset-filter! 'bla 'bla))
     (error? (hashset-filter! (hashset) 42))
     (error? (hashset-filter! 42 (hashset)))

     (let* ([hs (apply hashset (nums 0 100))]
            [hs1 (hashset-filter! odd? hs)])
       (and (equal? hs1 (apply hashset (nums 1 100 2)))
            (eq? hs hs1)))

     )


(mat hashset-partition

     (error? (hashset-partition))
     (error? (hashset-partition 'bla 'bla))
     (error? (hashset-partition (hashset) 42))
     (error? (hashset-partition 42 (hashset)))

     (let* ([hs (apply hashset (nums 0 100))])
       (let-values ([(T F) (hashset-partition odd? hs)])
         (and (equal? T (apply hashset (nums 1 100 2)))
              (equal? F (apply hashset (nums 0 100 2))))))

     )


(mat hashset-map

     (equal? (hashset-map id (hashset))
             (hashset-map id (hashset)))

     (equal? (hashset-map fx1+ (apply hashset (iota 100)))
             (apply hashset (map fx1+ (iota 100))))

     (equal? (hashset-map + (apply hashset (iota 100)) (apply hashset (iota 100)))
             (apply hashset (map + (iota 100) (iota 100))))

     )


(mat hashset-for-each
     #t)


(mat hashset<->list

     (error? (hashset->list))
     (error? (hashset->list '()))

     (let ([hs (apply hashset (iota 20))])
       (equal? hs (list->eqv-hashset (iota 20))))

     )


(mat hashset<->vector

     (error? (hashset->vector))
     (error? (hashset->vector '()))

     (let* ([v (random-vector 50)]
            [hs (vector->eqv-hashset v)])
       (equal? hs (vector->eqv-hashset (hashset->vector hs))))


     )



(mat hashset-set-ops

     ;; union
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9)])
       (equal? (hashset+ hs1 hs2 hs3)
               (hashset 1 2 3 4 5 6 7 8 9)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 0 90))
               (hashset+ hs1 hs2 hs3)))

     ;; difference
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)])
       (equal? (hashset- hs1 hs2)
               (hashset 1 2 3)))

     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9 1)])
       (equal? (hashset- hs1 hs2 hs3)
               (hashset 2 3)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 0 30))
               (hashset- hs1 hs2 hs3)))

     ;; intersection
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9)])
       (equal? (hashset& hs1 hs2 hs3)
               (hashset 4 5 6)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 40 50))
               (hashset& hs1 hs2 hs3)))

     ;; symmetric difference
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)])
       (equal? (hashset^ hs1 hs2)
               (hashset 1 2 3 7 8 9)))


     ;; equations
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 7 8 9 10 11 12 4 5 6)]
           [hs4 (hashset 4 6 7 8 2 5)])
       (equal? (hashset^ hs1 hs2 hs3 hs4)
               (hashset- (hashset+ hs1 hs2 hs3 hs4) (hashset& hs1 hs2 hs3 hs4))))

     (let* ([ls* (map (lambda (i) (random-list (lambda () (random 999)) 1000 2000)) (iota 10))]
            [hs* (map (lambda (ls) (apply hashset ls)) ls*)])
       (equal? (apply hashset^ hs*)
               (hashset- (apply hashset+ hs*) (apply hashset& hs*))))

     )
