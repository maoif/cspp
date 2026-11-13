(import (cspp) (cspp treeset))


(define v100    (fxvshuffle! (fxviota 100)))
(define v10000  (fxvshuffle! (fxviota 10000)))
(define v100000 (fxvshuffle! (fxviota 100000)))

(mat fxtreeset-constructor
     (let ([ts (make-fxtreeset fx= fx<)])
       (fxtreeset-add! ts 1)
       (and (fxtreeset? ts)
            (not (treeset? ts))
            (fxtreeset-contains? ts 1))))

(mat fxtreeset-populate
     (= 2 (fxtreeset-size (fxtreeset fx= fx< 1 2))))

;; Error cases: each public family rejects the other record type.
(mat treeset-family-rejection
     (error? (treeset-contains? (fxtreeset fx= fx< 1) 1))
     (error? (fxtreeset-contains? (treeset = < 1) 1)))

(mat treeset-mutating-algebra
     (let ([set (treeset = < 1 2 3)])
       (and (eq? set (treeset+! set set))
            (equal? '(1 2 3) (treeset->list set))
            (eq? set (treeset-! set set))
            (treeset-empty? set)))

     (let ([set (treeset = < 1 2 3)])
       (and (eq? set (treeset&! set (treeset = < 2 3 4)))
            (equal? '(2 3) (treeset->list set))))

     (let* ([set (treeset = < 1 2)]
            [second (treeset = < 2 3)]
            [third (treeset = < 2 4)]
            [expected (treeset^ set second third)])
       (and (eq? set (treeset^! set second third))
            (equal? set expected))))

;; Error case: fixnum treesets reject non-fixnum items.
(mat fxtreeset-key-validation
     (guard (c [(error? c) #t] [else #f])
       (fxtreeset-add! (make-fxtreeset fx= fx<) 'x)
       #f))

;; Error case: fixnum treeset deletion and navigation validate before backend access.
(mat fxtreeset-key-query-validation
     (error? (fxtreeset-delete! (make-fxtreeset fx= fx<) 'bad))

     (error? (fxtreeset-successor (fxtreeset fx= fx< 1) 'bad))

     (error? (fxtreeset-predecessor (make-fxtreeset fx= fx<) 'bad)))


(mat treeset-equal?

     (equal? (apply treeset = < (iota 100))
             (apply treeset = < (iota 100)))

     (not (equal? (apply treeset = < (iota 20))
                  (apply treeset = < (iota 10))))

     (not (equal? (apply treeset = fx< (iota 100))
                  (apply treeset = < (iota 100))))

     (let ([ts1 (make-treeset fx= fx<)] [ts2 (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts1 x)) v100000)
       (fxvfor-each (lambda (x) (treeset-add! ts2 x)) (fxvshuffle! v100000))
       (equal? ts1 ts1))


     )


(mat treeset-add!

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v100)
            (= (treeset-size ts) (fxvector-length v100))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v10000)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v10000)
            (= (treeset-size ts) (fxvector-length v10000))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v10000)
            (= (treeset-size ts) (fxvector-length v100000))))

     )


(mat treeset-delete!

     ;; absent value: deletion is idempotent
     (let ([ts (treeset = < 1)])
       (treeset-delete! ts 2)
       (and (= 1 (treeset-size ts))
            (treeset-contains? ts 1)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v100)
       (and (= (treeset-size ts) (fx/ (fxvector-length v100) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v100)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v10000)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v10000)
       (and (= (treeset-size ts) (fx/ (fxvector-length v10000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v10000)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v100000)
       (and (= (treeset-size ts) (fx/ (fxvector-length v100000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v100000)))
     )


(mat treeset-search

     (error? (treeset-search 'bla 'bla))
     (error? (treeset-search (treeset = <) 'bla))

     (let ([ts1 (treeset = < 10 11 12)])
       (and (= 11 (treeset-search ts1 odd?))
            (= 10 (treeset-search ts1 even?))))

     (let ([ts1 (treeset = < 0 2 4 6)])
       (not (treeset-search ts1 odd?)))

     )

(mat treeset-query-defaults
     (let ([ts (treeset = < 1)]
           [absent (vector 'absent)])
       (and (= 1 (treeset-min ts))
            (eq? absent (treeset-search ts (lambda (x) #f) absent))
            (eq? absent (treeset-successor ts 1 absent))
            (eq? absent (treeset-predecessor ts 1 absent))
            (eq? absent (treeset-min (treeset = <) absent))
            (eq? absent (treeset-max (treeset = <) absent))))

     (error? (treeset-successor (treeset = < 1) 2)))

(mat treeset-false-item-default
     (let* ([cmp (lambda (a b)
                   (or (and (not a) (not b))
                       (and a b (< a b))))]
            [ts (treeset eq? cmp #f)]
            [absent (vector 'absent)])
       (and (eq? #f (treeset-search ts (lambda (item) (not item))))
            (eq? #f (treeset-min ts))
            (eq? absent (treeset-search ts (lambda (item) item) absent))
            (not (treeset-search ts (lambda (item) item))))))


(mat treeset-search*

     (error? (treeset-search* 'bla 'bla))
     (error? (treeset-search* (treeset = <) 'bla))

     (let ([ts (apply treeset = < (nums 0 100))])
       (and (equal? (treeset-search* ts odd?)
                    (nums 1 100 2))
            (equal? (treeset-search* ts even?)
                    (nums 0 100 2))))


     (error? (treeset-search* 'bla 'bla 'bla))
     (error? (treeset-search* (treeset = <) odd? 'bla))

     (let ([ts (apply treeset = < (nums 0 100))]
           [ts1 (treeset = <)] [ts2 (treeset = <)])
       (treeset-search* ts odd? (lambda (x) (treeset-add! ts1 x)))
       (treeset-search* ts even? (lambda (x) (treeset-add! ts2 x)))
       (and (equal? ts1 (apply treeset = < (nums 1 100 2)))
            (equal? ts2 (apply treeset = < (nums 0 100 2)))))

     )


(mat treeset-contains?

     (error? (treeset-contains? 42))
     (error? (treeset-contains? (treeset fx= fx<)))

     (let ([tm (make-treeset fx= fx<)])
       (fxvandmap (lambda (x)
                    (treeset-add! tm x)
                    (treeset-contains? tm x))
                  v100000))

     ;; incomparable value
     (error? (let ([tm (make-treeset fx= fx<)])
               (fxvandmap (lambda (x) (treeset-add! tm x)) v100)
               (treeset-contains? tm 'x)))

     )


(mat treeset-contains/p?

     (error? (treeset-contains/p? 42 42))
     (error? (treeset-contains/p? (make-treeset fx= fx<)))
     (error? (treeset-contains/p? (make-treeset fx= fx<) 42))

     (let ([ts (make-treeset fx= fx<)])
       (fxvandmap (lambda (x)
                    (treeset-add! ts x)
                    (treeset-contains/p? ts (lambda (y) (= x y))))
                  v10000))

     )


(mat treeset-max/min

     ;; empty
     (not (treeset-max (treeset = <)))
     (not (treeset-min (treeset = <)))

     (begin (define ts0 (let ([ts (make-treeset fx= fx<)])
                          (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
                          ts))
            #t)

     (= (fxvmin v100000)
        (treeset-min ts0))

     (= (fxvmax v100000)
        (treeset-max ts0))

     )


(mat treeset-successor

     (let* ([ls (iota 1000)] [ts (apply treeset = < ls)])
       (andmap (lambda (x) (fx= (treeset-successor ts x) (fx1+ x))) (iota 999)))

     )


(mat treeset-predecessor

     (let* ([ls (iota 1000)] [ts (apply treeset = < ls)])
       (andmap (lambda (x) (fx= (treeset-predecessor ts x) (fx1- x))) (cdr ls)))

     )


(mat treeset-filter

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (andmap odd? (treeset->list (treeset-filter odd? ts))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (andmap odd? (treeset->list (treeset-filter odd? ts))))

     )


(mat treeset-filter!

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (treeset-filter! odd? ts)
       (andmap odd? (treeset->list ts)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (treeset-filter! odd? ts)
       (andmap odd? (treeset->list ts)))

     )


(mat treeset-partition

     (error? (treeset-partition))
     (error? (treeset-partition 'a))
     (error? (treeset-partition 'a 'b))
     (error? (treeset-partition 'a (treeset = <)))


     (let* ([ls (iota 1000)] [odds (filter odd? ls)] [evens (filter even? ls)]
            [ts (apply treeset = < ls)]
            [ts-odd (apply treeset = < odds)]
            [ts-even (apply treeset = < evens)])
       (let-values ([(o e) (treeset-partition odd? ts)])
         (and (equal? o ts-odd)
              (equal? e ts-even))))

     )


(mat treeset->list

     (error? (treeset->list))
     (error? (treeset->list '()))
     (error? (treeset->list (treeset = <) 'bla))

     (let* ([ls (iota 100)]
            [ts (apply treeset = < ls)])
       (equal? ls (treeset->list ts)))

     (let* ([ls (iota 100)]
            [ts (apply treeset = > ls)])
       (equal? ls (reverse (treeset->list ts))))

     )


(mat treeset-andmap

     (error? (treeset-andmap 1))
     (error? (treeset-andmap 1 (treeset = <)))
     (error? (treeset-andmap (lambda (v) #t) 1))


     (let* ([n* (nums 0 2 #e1e1000)]
            [tm (apply treeset = < n*)])
       (treeset-andmap (lambda (k) (even? k)) tm))

     (let* ([n* (nums 1 2 #e1e1000)]
            [tm (apply treeset = < n*)])
       (treeset-andmap (lambda (k) (odd? k)) tm))

     )


(mat treeset-ormap

     (error? (treeset-ormap 1))
     (error? (treeset-ormap 1 (treeset = <)))
     (error? (treeset-ormap (lambda (k) #t) 1))


     (let* ([n* (nums 0 2 #e1e10000)]
            [tm (apply treeset = < n*)])
       (treeset-add! tm 5)
       (treeset-ormap (lambda (k) (odd? k)) tm))

     (let* ([n* (nums 1 2 #e1e10000)]
            [tm (apply treeset = < n*)])
       (treeset-add! tm 6)
       (treeset-ormap (lambda (k) (even? k)) tm))


     )


(mat treeset-map

     (equal? (treeset-map id (treeset = <))
             (treeset-map id (treeset = <)))

     (equal? (treeset-map fx1+ (apply treeset = < (iota 100)))
             (apply treeset = < (map fx1+ (iota 100))))

     (equal? (treeset-map + (apply treeset = < (iota 100)) (apply treeset = < (iota 100)))
             (apply treeset = < (map + (iota 100) (iota 100))))

     )


(mat treeset-map/i


     #t)


(mat treeset-for-each
     #t)


(mat treeset-for-each/i
     #t)


(mat treeset-fold-left
     #t)


(mat treeset-fold-left/i
     #t)


(mat treeset-fold-right
     #t)


(mat treeset-fold-right/i
     #t)


(mat treeset-set-ops

     ;; union
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9)])
       (equal? (treeset+ ts1 ts2 ts3)
               (treeset = < 1 2 3 4 5 6 7 8 9)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 0 90))
               (treeset+ ts1 ts2 ts3)))

     ;; difference
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)])
       (equal? (treeset- ts1 ts2)
               (treeset = < 1 2 3)))

     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9 1)])
       (equal? (treeset- ts1 ts2 ts3)
               (treeset = < 2 3)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 0 30))
               (treeset- ts1 ts2 ts3)))

     ;; intersection
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9)])
       (equal? (treeset& ts1 ts2 ts3)
               (treeset = < 4 5 6)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 40 50))
               (treeset& ts1 ts2 ts3)))

     ;; symmetric difference
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)])
       (equal? (treeset^ ts1 ts2)
               (treeset = < 1 2 3 7 8 9)))


     ;; equations
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 7 8 9 10 11 12 4 5 6)]
           [ts4 (treeset = < 4 6 7 8 2 5)])
       (equal? (treeset^ ts1 ts2 ts3 ts4)
               (treeset- (treeset+ ts1 ts2 ts3 ts4) (treeset& ts1 ts2 ts3 ts4))))

     (let* ([ls* (map (lambda (i) (random-list (lambda () (random 999)) 1000 2000)) (iota 10))]
            [ts* (map (lambda (ls) (apply treeset = < ls)) ls*)])
       (equal? (apply treeset^ ts*)
               (treeset- (apply treeset+ ts*) (apply treeset& ts*))))

     )
