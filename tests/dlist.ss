(import (cspp)
        (cspp io)
        (cspp dlist))



(mat dlist

     ;; invalid dlist: the size observer validates through pcheck
     (guard (condition
             [(and (who-condition? condition)
                   (eq? 'pcheck (condition-who condition))) #t]
             [else #f])
       (dlist-size 42)
       #f)

     (let* ([l (iota 10)]
            [dl (apply dlist l)])
       (= (dlist-size dl) (length l)))

     (let* ([l (random-list 30)]
            [dl (apply dlist l)])
       (equal? l (dlist->list dl)))

     (fx= 0 (dlist-size (dlist)))
     )


(mat make-dlist

     (fx= 0 (dlist-size (make-dlist)))
     (fx= 10 (dlist-size (make-dlist 10)))
     (fx= 100 (dlist-size (make-dlist 100)))

     (error? (make-dlist #f))

     ;; all #f by default
     (let ([dl (make-dlist 10)])
       (andmap not (dlist->list dl)))

     (let ([dl (make-dlist 10 1)])
       (= 10 (apply + (dlist->list dl))))

     )


(mat dlist-add!

     ;; not dlist
     (error? (dlist-add! 1 1))
     (error? (dlist-add! 1 1 1))

     ;; bad index
     (error? (dlist-add! (dlist) 1 0))
     (error? (dlist-add! (dlist 1) 2 0))
     (error? (dlist-add! (dlist) -1 0))
     (error? (dlist-add! (dlist) -2 0))

     ;; front
    (let ([dl (apply dlist (iota 5))])
       (dlist-add! dl 0 -1)
       (dlist-add! dl 0 -2)
       (dlist-add! dl 0 -3)
       (dlist-add! dl 0 -4)
       (and (= 9 (dlist-size dl))
            (equal? '(-4 -3 -2 -1 0 1 2 3 4) (dlist->list dl))))

     ;; end
     (let ([dl (apply dlist (iota 5))])
       (dlist-add! dl (dlist-size dl) -1)
       (dlist-add! dl (dlist-size dl) -2)
       (dlist-add! dl (dlist-size dl) -3)
       (dlist-add! dl (dlist-size dl) -4)
       (and (= 9 (dlist-size dl))
            (equal? '(0 1 2 3 4 -1 -2 -3 -4) (dlist->list dl))))

     ;; middle
     (let ([dl (apply dlist (iota 5))])
       (dlist-add! dl 3 -1)
       (dlist-add! dl 3 -2)
       (dlist-add! dl 3 -3)
       (dlist-add! dl 3 -4)
       (and (= 9 (dlist-size dl))
            (equal? '(0 1 2 -4 -3 -2 -1 3 4) (dlist->list dl))))

     (let ([dl (apply dlist (iota 5))])
       (dlist-add! dl 5)
       (dlist-add! dl 0 -1)
       (dlist-add! dl 2 -2)
       (dlist-add! dl 4 -3)
       (and (= 9 (dlist-size dl))
            (equal? '(-1 0 -2 1 -3 2 3 4 5) (dlist->list dl))))
     )

(mat dlist-add*!
     (let ([dl (dlist)])
       (dlist-add*! dl)
       (dlist-empty? dl))

     (let ([dl (dlist 10 20)])
       (dlist-add*! dl 1 30 40)
       (equal? '(10 30 40 20) (dlist->list dl)))

     (let ([dl (dlist 10 20)])
       (dlist-add*! dl 30 40)
       (equal? '(10 20 30 40) (dlist->list dl)))

     (let ([dl (dlist 10 20)])
       (dlist-add*! dl 0 30 40)
       (equal? '(30 40 10 20) (dlist->list dl)))
     )


(mat dlist-set!

     ;; not dlist
     (error? (dlist-set! 1 1))
     (error? (dlist-set! 1 1 1))

     ;; bad index
     (error? (dlist-set! (dlist) 1 0))
     (error? (dlist-set! (dlist 1) 2 0))
     (error? (dlist-set! (dlist) -1 0))
     (error? (dlist-set! (dlist) -2 0))

     (let ([dl (apply dlist (iota 5))])
       (dlist-set! dl 0 -1)
       (dlist-set! dl 1 -2)
       (dlist-set! dl 2 -3)
       (dlist-set! dl 3 -4)
       (dlist-set! dl 4 -5)
       (equal? '(-1 -2 -3 -4 -5) (dlist->list dl)))

     )


(mat dlist-ref

     ;; not dlist
     (error? (dlist-ref 1 1))
     (error? (dlist-ref #f 1))

     ;; bad index
     (error? (dlist-ref (dlist) 1))
     (error? (dlist-ref (dlist 1) 2))
     (error? (dlist-ref (dlist) -1))
     (error? (dlist-ref (dlist) -2))


     (let* ([ls (random-list 100 200)] [dl (apply dlist ls)])
       (let loop ([i 0] [ls ls])
         (if (null? ls)
             #t
             (and (equal? (car ls) (dlist-ref dl i))
                  (loop (fx1+ i) (cdr ls))))))
     )


(mat dlist-delete!

     ;; not dlist
     (error? (dlist-delete! 1 1))
     (error? (dlist-delete! #f 1))

     ;; bad index
     (error? (dlist-delete! (dlist) 1))
     ;; index exceeds the size
     (error? (dlist-delete! (dlist 1) 2))
     (error? (dlist-delete! (dlist) -1))
     (error? (dlist-delete! (dlist) -2))


     (let ([dl (dlist 1)])
       (dlist-delete! dl 0)
       (and (null? (dlist->list dl))
            (= 0 (dlist-size dl))))

     ;; middle
     (let ([dl (dlist 1 2 3)])
       (dlist-delete! dl 1)
       (and (= 2 (dlist-size dl))
            (equal? '(1 3) (dlist->list dl))))

     ;; first
     (let ([dl (dlist 1 2 3)])
       (dlist-delete! dl 0)
       (and (= 2 (dlist-size dl))
            (equal? '(2 3) (dlist->list dl))))

     ;; last
     (let ([dl (dlist 1 2 3)])
       (dlist-delete! dl 2)
       (and (= 2 (dlist-size dl))
            (equal? '(1 2) (dlist->list dl))))


     (let ([dl (apply dlist (iota 10))])
       (dlist-delete! dl 0)
       (dlist-delete! dl (fx1- (dlist-size dl)))
       (dlist-delete! dl 3)
       (dlist-delete! dl 5)
       (and (dlist-size dl)
            (equal? '(1 2 3 5 6 8) (dlist->list dl))))


     )


(mat dlist-reverse

     (error? (dlist-reverse 42))

     (equal? '()  (dlist->list (dlist-reverse (dlist))))
     (equal? '(1) (dlist->list (dlist-reverse (dlist 1))))

     (let* ([l (iota 10)]
            [dl (apply dlist l)])
       (equal? (reverse l) (dlist->list (dlist-reverse dl))))

     (let* ([l (random-list 100)]
            [dl (apply dlist l)])
       (equal? (reverse l) (dlist->list (dlist-reverse dl))))

     )


(mat dlist-reverse!

     (error? (dlist-reverse! 42))

     (let ([dl (dlist)])
       (dlist-reverse! dl)
       (equal? '() (dlist->list dl)))

     (let ([dl (dlist 1)])
       (dlist-reverse! dl)
       (equal? '(1) (dlist->list dl)))

     (let* ([l (iota 10)]
            [dl (apply dlist l)])
       (dlist-reverse! dl)
       (equal? (reverse l) (dlist->list dl)))

     (let* ([l (random-list 100)]
            [dl (apply dlist l)])
       (dlist-reverse! dl)
       (equal? (reverse l) (dlist->list dl)))

     )


(mat dlist-filter

     (error? (dlist-filter 42 (dlist)))
     (error? (dlist-filter odd? 42))


     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (equal? (filter odd? ls)
               (dlist->list (dlist-filter odd? dl))))


     (let* ([ls (random-list (lambda () (random 9999)) 10 30)]
            [dl (apply dlist ls)]
            [f (lambda (x) (fx> x 999))])
       (equal? (filter f ls)
               (dlist->list (dlist-filter f dl))))


     (let* ([ls (random-list (lambda () (random-string 40)) 10 30)]
            [dl (apply dlist ls)]
            [f (lambda (x) (fx> (string-length x) 10))])
       (equal? (filter f ls)
               (dlist->list (dlist-filter f dl))))

     )


(mat dlist-filter!

     (error? (dlist-filter! 42 (dlist)))
     (error? (dlist-filter! odd? 42))


     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-filter! odd? dl)
       (equal? (filter odd? ls)
               (dlist->list dl)))


     (let* ([ls (random-list (lambda () (random 9999)) 10 30)]
            [dl (apply dlist ls)]
            [f (lambda (x) (fx> x 999))])
       (dlist-filter! f dl)
       (equal? (filter f ls)
               (dlist->list dl)))


     (let* ([ls (random-list (lambda () (random-string 40)) 10 30)]
            [dl (apply dlist ls)]
            [f (lambda (x) (fx> (string-length x) 10))])
       (dlist-filter! f dl)
       (equal? (filter f ls)
               (dlist->list dl)))

     )


(mat dlist-append

     (error? (dlist-append 42))
     (error? (dlist-append (make-dlist 10) 42))

     (let* ([ls (random-list 30)]
            [dl (apply dlist ls)])
       (equal? ls (dlist->list (dlist-append dl))))

     (let ([ls1 (random-list 20)]
           [ls2 (random-list 20)]
           [ls3 (random-list 20)]
           [ls4 (random-list 20)])
       (let ([dl1 (apply dlist ls1)]
             [dl2 (apply dlist ls2)]
             [dl3 (apply dlist ls3)]
             [dl4 (apply dlist ls4)])
         (let ([dl* (dlist-append dl1 dl2 dl3 dl4)])
           (equal? (append ls1 ls2 ls3 ls4)
                   (dlist->list dl*)))))

     )


(mat dlist-append!

     (error? (dlist-append! 42))
     (error? (dlist-append! (make-dlist 10) 42))

     ;; Q: dl1 is empty if this test is placed last?
     (let ([ls1 (random-list 20)]
           [ls2 (random-list 20)]
           [ls3 (random-list 20)]
           [ls4 (random-list 20)])
       (let ([dl1 (apply dlist ls1)]
             [dl2 (apply dlist ls2)]
             [dl3 (apply dlist ls3)]
             [dl4 (apply dlist ls4)])
         (dlist-append! dl1 dl2 dl3 dl4)
         ;;(displayln dl1)
         ;;(displayln (append ls1 ls2 ls3 ls4))
         (equal? (append ls1 ls2 ls3 ls4)
                 (dlist->list dl1))))

     (let* ([ls (random-list 30)]
            [dl (apply dlist ls)])
       (dlist-append! dl)
       (equal? ls (dlist->list dl)))

     )


(mat dlist-contains?

     (error? (dlist-contains? 42 42))
     (error? (dlist-contains? (dlist)))

     (not (dlist-contains? (dlist 10 20) 30))
     (eq? #t (dlist-contains? (dlist 10 20) 10))
     (= 0 (dlist-index-of (dlist 10 20) 10))
     (= 1 (dlist-find-index (dlist 10 20)
                            (lambda (x) (= x 20))))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (bool (andmap (lambda (x) (dlist-contains? dl x)) ls)))

     (let* ([ls (random-list 20 30)]
            [dl (apply dlist ls)])
       (bool (andmap (lambda (x) (dlist-contains? dl x)) ls)))
     )


(mat dlist-contains/p?

     (error? (dlist-contains/p? = 42))
     (error? (dlist-contains/p? odd? (dlist)))

     (eq? #t (dlist-contains/p? (dlist 1 2) odd?))
     (not (dlist-contains/p? (dlist 1 3) even?))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (bool (and (dlist-contains/p? dl odd?)
                  (dlist-contains/p? dl even?))))

     )


(mat dlist-search

     (error? (dlist-search 42 42))
     (error? (dlist-search odd? (dlist)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (and (= 0 (dlist-search dl even?))
            (= 1 (dlist-search dl odd?))))

     (let ([dl (dlist "1" "11" "111" "1111")])
       (and (string=? "1"   (dlist-search dl (lambda (x) (= 1 (string-length x)))))
            (string=? "111" (dlist-search dl (lambda (x) (= 3 (string-length x)))))
            (string=? "11"  (dlist-search dl (lambda (x) (<= 2 (string-length x)))))))

     )

(mat dlist-search-default
     (let ([absent (vector 'absent)])
       (and (eq? absent (dlist-search (dlist 1 2) (lambda (x) (> x 2)) absent))
            (= 1 (dlist-search (dlist 1 2) odd? absent))
            (not (dlist-search (dlist 1 2) (lambda (x) (> x 2)))))))


(mat dlist-search*

     (error? (dlist-search* 42 42))
     (error? (dlist-search* odd? (dlist)))


     (let ([dl (dlist "1" "11" "111" "1111")])
       (and (equal? '("1")
                    (dlist-search* dl (lambda (x) (= 1 (string-length x)))))
            (equal? '("111")
                    (dlist-search* dl (lambda (x) (= 3 (string-length x)))))
            (equal? '("11" "111" "1111")
                    (dlist-search* dl (lambda (x) (<= 2 (string-length x)))))))

     ;; custom collector
     (let ([dl (dlist "1" "11" "111" "1111")])
       (and (equal? (dlist "1")
                    (let ([col (dlist)])
                      (dlist-search* dl (lambda (x) (= 1 (string-length x))) (lambda (x) (dlist-add! col x)))
                      col))
            (equal? (dlist "111")
                    (let ([col (dlist)])
                      (dlist-search* dl (lambda (x) (= 3 (string-length x))) (lambda (x) (dlist-add! col x)))
                      col))
            (equal? (dlist "11" "111" "1111")
                    (let ([col (dlist)])
                      (dlist-search* dl (lambda (x) (<= 2 (string-length x))) (lambda (x) (dlist-add! col x)))
                      col))))

     )


(mat dlist-slice

     (equal? '(1) (dlist->list (dlist-slice (dlist 1) 1)))
     (equal? '(1) (dlist->list (dlist-slice (dlist 1) 2)))
     (equal? '(1) (dlist->list (dlist-slice (dlist 1) 3)))
     (equal? '(1) (dlist->list (dlist-slice (dlist 1) 2 -4 -4)))
     (equal? '()  (dlist->list (dlist-slice (dlist 1) 0 1 -1)))

     (equal? '() (dlist->list (dlist-slice (dlist 1) 2 -4)))
     (equal? '() (dlist->list (dlist-slice (dlist 1) 1 5)))
     (equal? '() (dlist->list (dlist-slice (dlist 1) 3 0 3)))

     (begin (define ls (iota 10))
            (define dl (apply dlist ls))
            #t)


     ;; positive index, forward
     (equal? '(0 1 2 3 4)
             (dlist->list (dlist-slice dl 5)))
     (equal? '(0 2 4 6 8)
             (dlist->list (dlist-slice dl 0 9 2)))
     (equal? '(2 5 8)
             (dlist->list (dlist-slice dl 2 9 3)))


     ;; positive index, backward
     (equal? '(3 2)
             (dlist->list (dlist-slice dl 3 1 -1)))
     (equal? '(8 6 4)
             (dlist->list (dlist-slice dl 8 2 -2)))
     (equal? '(9 6 3)
             (dlist->list (dlist-slice dl 9 1 -3)))
     (equal? '(9 5)
             (dlist->list (dlist-slice dl 9 1 -4)))
     (equal? '(9 5 1)
             (dlist->list (dlist-slice dl 9 0 -4)))


     ;; negative index, forward
     (equal? '(0 1 2 3 4)
             (dlist->list (dlist-slice dl -5)))
     (equal? (iota 9)
             (dlist->list (dlist-slice dl -1)))
     (equal? '(5 6 7 8)
             (dlist->list (dlist-slice dl -5 -1)))
     (equal? '(9)
             (dlist->list (dlist-slice dl -1 -2 -1)))
     (equal? '(1 2 3 4 5 6 7 8)
             (dlist->list (dlist-slice dl -9 -1)))
     (equal? '(1 4 7)
             (dlist->list (dlist-slice dl -9 -1 3)))


     ;; negative index, backward
     (equal? '(9 8 7 6)
             (dlist->list (dlist-slice dl -1 -5 -1)))
     (equal? '(9 7 5 3)
             (dlist->list (dlist-slice dl -1 -9 -2)))
     (equal? '(8 4)
             (dlist->list (dlist-slice dl -2 -9 -4)))
     (equal? '(1)
             (dlist->list (dlist-slice (dlist 1) -1 -2 -1)))
     )


(mat dlist-slice!

     (let ([dl (dlist 1)])
       (dlist-slice! dl 1)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 2)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 3)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 2 -4 -4)
       (equal? '(1) (dlist->list dl)))

     ;; bad indices, so no effect
     (let ([dl (dlist 1)])
       (dlist-slice! dl 0 1 -1)
       (displayln dl)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 2 -4)
       (displayln dl)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 1 5)
       (displayln dl)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 3 0 3)
       (displayln dl)
       (equal? '(1) (dlist->list dl)))


     (let ([dl (dlist 1)])
       (dlist-slice! dl 1)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 2)
       (equal? '(1) (dlist->list dl)))
     (let ([dl (dlist 1)])
       (dlist-slice! dl 3)
       (equal? '(1) (dlist->list dl)))

     ;; positive index, forward
     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 5)
       (equal? '(0 1 2 3 4)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 0 9 2)
       (equal? '(0 2 4 6 8)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 2 9 3)
       (equal? '(2 5 8)
               (dlist->list dl)))

     ;; positive index, backward
     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 3 1 -1)
       (equal? '(3 2)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 8 2 -2)
       (equal? '(8 6 4)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 9 1 -3)
       (equal? '(9 6 3)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 9 1 -4)
       (equal? '(9 5)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl 9 0 -4)
       (equal? '(9 5 1)
               (dlist->list dl)))


     ;; negative index, forward
     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -5)
       (equal? '(0 1 2 3 4)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -1)
       (equal? (iota 9)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -5 -1)
       (equal? '(5 6 7 8)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -1 -2 -1)
       (equal? '(9)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -9 -1)
       (equal? '(1 2 3 4 5 6 7 8)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -9 -1 3)
       (equal? '(1 4 7)
               (dlist->list dl)))


     ;; negative index, backward
     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -1 -5 -1)
       (equal? '(9 8 7 6)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -1 -9 -2)
       (equal? '(9 7 5 3)
               (dlist->list dl)))

     (let* ([ls (iota 10)]
            [dl (apply dlist ls)])
       (dlist-slice! dl -2 -9 -4)
       (equal? '(8 4)
               (dlist->list dl)))
     (let* ([ls '(1)] [dl (apply dlist ls)])
       (dlist-slice! dl -1 -2 -1)
       (equal? ls
               (dlist->list dl)))

     )


(mat dlist-copy

     (error? (dlist-copy))
     (error? (dlist-copy #f))

     (let* ([dl (apply dlist (iota 10))]
            [newdl (dlist-copy dl)])
       (and (equal? dl newdl)
            (not (eq? dl newdl))))

     )


(mat dlist-copy!

     (error?
      (let ([dl (apply dlist (iota 10))])
        (dlist-copy! dl 0 dl 3 9)))
     (error?
      (let ([dl (apply dlist (iota 10))])
        (dlist-copy! dl 3 dl 0 9)))
     (error? (let ([dl (apply dlist (iota 10))])
               (dlist-copy! dl 3 dl 0 -2)))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 3 dl 3 5)
       (equal? dl (apply dlist (iota 10))))


;;;; same dlist

     ;; disjoint, left to right
     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 3 3)
       (equal? (dlist 0 1 2 0 1 2 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 5 3)
       (equal? (dlist 0 1 2 3 4 0 1 2 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 5 5)
       (equal? (dlist 0 1 2 3 4 0 1 2 3 4) dl))

     ;; disjoint, right to left
     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 5 dl 0 3)
       (equal? (dlist 5 6 7 3 4 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 3 dl 0 3)
       (equal? (dlist 3 4 5 3 4 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 5 dl 0 5)
       (equal? (dlist 5 6 7 8 9 5 6 7 8 9) dl))

     ;; overlapping, left to right
     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 2 3)
       (equal? (dlist 0 1 0 1 2 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 0 5)
       (equal? (dlist 0 1 2 3 4 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 1 5)
       (equal? (dlist 0 0 1 2 3 4 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 0 dl 3 5)
       (equal? (dlist 0 1 2 0 1 2 3 4 8 9) dl))

     ;; overlapping , right to left
     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 2 dl 0 3)
       (equal? (dlist 2 3 4 3 4 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 2 dl 0 5)
       (equal? (dlist 2 3 4 5 6 5 6 7 8 9) dl))

     (let ([dl (apply dlist (iota 10))])
       (dlist-copy! dl 5 dl 3 5)
       (equal? (dlist 0 1 2 5 6 7 8 9 8 9) dl))



;;;; different dlist

     (let ([dl (apply dlist (iota 10))]
           [dl1 (make-dlist 10 #f)])
       (dlist-copy! dl 0 dl1 0 3)
       (equal? (dlist 0 1 2 #f #f #f #f #f #f #f) dl1))

     (let ([dl (apply dlist (iota 10))]
           [dl1 (make-dlist 10 #f)])
       (dlist-copy! dl 0 dl1 0 5)
       (equal? (dlist 0 1 2 3 4 #f #f #f #f #f) dl1))

     (let ([dl (apply dlist (iota 10))]
           [dl1 (make-dlist 10 #f)])
       (dlist-copy! dl 0 dl1 3 5)
       (equal? (dlist #f #f #f 0 1 2 3 4 #f #f) dl1))

     (let ([dl (apply dlist (iota 10))]
           [dl1 (make-dlist 10 #f)])
       (dlist-copy! dl 2 dl1 0 5)
       (equal? (dlist 2 3 4 5 6 #f #f #f #f #f) dl1))


     )


(mat dlist-sorted?

     (error? (dlist-sorted? #f (dlist)))
     (error? (dlist-sorted? < '()))
     (error? (dlist-sorted? < (dlist) 1 2))
     (error? (dlist-sorted? < (dlist 1 2 3) 0 4))


     (dlist-sorted? < (dlist))
     (dlist-sorted? < (dlist 1))

     (dlist-sorted? < (apply dlist (iota 10)))
     (not (dlist-sorted? < (apply dlist (reverse (iota 10)))))

     (let ([dl (dlist 9 8 7 1 2 3 4 0)])
       (and (not (dlist-sorted? < dl))
            (dlist-sorted? < dl 3 7)))

     )


(mat dlist-sort!

     ;; bad <?
     (error? (dlist-sort! 1 (dlist)))
     ;; not dlists
     (error? (dlist-sort! <= '(1 2 3)))
     ;; bad range
     (error? (dlist-sort! <= (dlist) 3))
     (error? (dlist-sort! <= (dlist 2 2 2 2 2) 6 3))

     ;; full range, in place
     (begin (define (test1 rand <? sort! sorted? v->dl)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [dl (v->dl v)])
                                    (sort! <? dl)
                                    (sorted? <? dl)))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test1 random-vector fx<= dlist-sort! dlist-sorted? vector->dlist)


     ;; ranged, in place
     (begin (define (test3 rand <? sort! sorted? v->dl)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [dl (v->dl v)])
                                    (sort! <? dl mid)
                                    (sort! <? dl mid #e1e6)
                                    (and (sorted? <? dl 0 mid)
                                         (sorted? <? dl mid #e1e6))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test3 random-vector fx<= dlist-sort! dlist-sorted? vector->dlist)

     )


(mat dlist-sort

     ;; bad <?
     (error? (dlist-sort 1 (dlist)))
     ;; not dlists
     (error? (dlist-sort <= '(1 2 3)))
     ;; bad range
     (error? (dlist-sort <= (dlist) 3))
     (error? (dlist-sort <= (dlist 2 2 2 2 2) 6 3))


     ;; full range, return new
     (begin (define (test2 rand <? sort sorted? v->dl)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [dl (v->dl v)])
                                    (sorted? <? (sort <? dl))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test2 random-vector fx<= dlist-sort dlist-sorted? vector->dlist)


     ;; ranged, return new
     (begin (define (test4 rand <? sort sorted? v->dl)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [dl (v->dl v)])
                                    (and (sorted? <? (sort <? dl mid))
                                         (sorted? <? (sort <? dl mid #e1e6)))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test4 random-vector fx<= dlist-sort dlist-sorted? vector->dlist)

     )


(mat dlist-iota

     (error? (dlist-iota -1))
     (error? (dlist-iota -#f))

     (equal? (apply dlist (iota 10))
             (dlist-iota 10))

     (equal? (dlist)
             (dlist-iota 0))

     )


(mat dlist-nums

     (error? (dlist-nums 'x 'x 'x))
     (error? (dlist-nums 0 10  -1))
     (error? (dlist-nums 0 -10 1))

     (equal? (dlist-nums 0 10)
             (dlist-iota 10))
     (equal? (dlist-nums 5 10)
             (dlist 5 6 7 8 9))
     (equal? (dlist-nums 5 15 3)
             (dlist 5 8 11 14))
     (equal? (dlist-nums 0 -10 -1)
             (dlist 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
     (equal? (dlist-nums 0 -10 -2)
             (dlist 0 -2 -4 -6 -8))
     (equal? (dlist-nums 0 5 0.5)
             (dlist 0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
     (equal? (dlist-nums 0 -5 -0.5)
             (dlist 0 -0.5 -1.0 -1.5 -2.0 -2.5 -3.0 -3.5 -4.0 -4.5))

     (equal? (nums (most-positive-fixnum) (+ 5000 (most-positive-fixnum)) 333.14)
             (dlist->list
              (dlist-nums (most-positive-fixnum) (+ 5000 (most-positive-fixnum)) 333.14)))
     (equal? (nums (most-positive-fixnum) (+ 50 (most-positive-fixnum)) 3.14)
             (dlist->list
              (dlist-nums (most-positive-fixnum) (+ 50 (most-positive-fixnum)) 3.14)))

     )


(mat dlist-stack-ops

     (error? (dlist-pop! (dlist)))
     (error? (dlist-pop-back! (dlist)))

     (let* ([ls (iota 100)] [dl (dlist)])
       (for-each (lambda (x) (dlist-push! dl x)) ls)
       (equal? (dlist->list dl) (reverse ls)))

     (let* ([ls (iota 100)] [dl (dlist)])
       (for-each (lambda (x) (dlist-push! dl x)) ls)
       (equal? (let loop ([res '()])
                 (if (dlist-empty? dl)
                     res
                     (loop (cons (dlist-pop! dl) res))))
               ls))


     (let* ([ls (iota 100)] [dl (dlist)])
       (for-each (lambda (x) (dlist-push-back! dl x)) ls)
       (equal? (dlist->list dl) ls))

     (let* ([ls (iota 100)] [dl (dlist)])
       (for-each (lambda (x) (dlist-push-back! dl x)) ls)
       (equal? (let loop ([res '()])
                 (if (dlist-empty? dl)
                     res
                     (loop (cons (dlist-pop-back! dl) res))))
               ls))

     )


(mat dlist-map

     ;; type error
     ;; length not equal
     ;; proc arity error


     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (map add1 ls0))
               (dlist-map add1 dl0)))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0))
               (dlist-map + dl0 dl0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0))
               (dlist-map + dl0 dl1)))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0 ls0 ls0 ls0))
               (dlist-map + dl0 dl1 dl2 dl3 dl4)))
     )


(mat dlist-map/i

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i (lambda (i x) (list i x)) dl0)
               (apply dlist (zip ls0 ls0))))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-map/i (lambda (i x y) (list i x y)) dl0 dl1)
               (apply dlist (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i (lambda (i x y) (list i x y)) dl0 dl0)
               (apply dlist (zip ls0 ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) dl0 dl1 dl2 dl3 dl4)
               (apply dlist (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) dl0 dl0 dl0 dl0 dl0)
               (apply dlist (zip ls0 ls0 ls0 ls0 ls0 ls0))))


     )


(mat dlist-map!

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (map add1 ls0))
               (dlist-map! add1 dl0)))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0))
               (dlist-map! + dl0 dl1)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (dlist-map! + dl0 dl0)
       (equal? (apply dlist (map + ls0 ls0))
               dl0))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0 ls0 ls0 ls0))
               (dlist-map! + dl0 dl1 dl2 dl3 dl4)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (map + ls0 ls0 ls0 ls0 ls0))
               (dlist-map! + dl0 dl0 dl0 dl0 dl0)))

     )


(mat dlist-map/i!

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i! (lambda (i x) (list i x)) dl0)
               (apply dlist (zip ls0 ls0))))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-map/i! (lambda (i x y) (list i x y)) dl0 dl1)
               (apply dlist (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i! (lambda (i x y) (list i x y)) dl0 dl0)
               (apply dlist (zip ls0 ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) dl0 dl1 dl2 dl3 dl4)
               (apply dlist (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) dl0 dl0 dl0 dl0 dl0)
               (apply dlist (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-for-each

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each (lambda (x) (set! ls (cons x ls)))
                       dl0)
       (equal? ls (reverse ls0)))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       dl0 dl1)
       (equal? ls (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       dl0 dl0)
       (equal? ls (reverse (zip ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                       dl0 dl1 dl2 dl3 dl4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-for-each/i

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i (lambda (i x) (set! ls (cons (list i x) ls)))
                         dl0)
       (equal? ls (reverse (zip ls0 ls0))))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         dl0 dl1)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         dl0 dl0)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                         dl0 dl1 dl2 dl3 dl4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-map-rev

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map add1 ls0)))
               (dlist-map-rev add1 dl0)))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0)))
               (dlist-map-rev + dl0 dl0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0)))
               (dlist-map-rev + dl0 dl1)))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0 ls0 ls0 ls0)))
               (dlist-map-rev + dl0 dl1 dl2 dl3 dl4)))
     )


(mat dlist-map/i-rev

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0)))
               (dlist-map/i-rev + dl0)))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0 ls0)))
               (dlist-map/i-rev + dl0 dl0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0 ls0)))
               (dlist-map/i-rev + dl0 dl1)))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (apply dlist (reverse (map + ls0 ls0 ls0 ls0 ls0 ls0)))
               (dlist-map/i-rev + dl0 dl1 dl2 dl3 dl4)))

     )


(mat dlist-for-each-rev

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each-rev (lambda (x) (set! ls (cons x ls)))
                           dl0)
       (equal? ls ls0))


     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           dl0 dl1)
       (equal? ls (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           dl0 dl0)
       (equal? ls (zip ls0 ls0)))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each-rev (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                           dl0 dl1 dl2 dl3 dl4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0)))

     )


(mat dlist-for-each/i-rev

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i-rev (lambda (i x) (set! ls (cons (list i x) ls)))
                             dl0)
       (equal? ls (zip ls0 ls0)))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             dl0 dl1)
       (equal? ls (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             dl0 dl0)
       (equal? ls (zip ls0 ls0 ls0)))

     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (dlist-for-each/i-rev (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                             dl0 dl1 dl2 dl3 dl4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     )


(mat dlist-andmap

     (error? (dlist-andmap #f (dlist)))

     (dlist-andmap odd? (dlist))
     (dlist-andmap odd? (dlist) (dlist))
     (dlist-andmap odd? (dlist) (dlist) (dlist) (dlist) (dlist))

     (error? (dlist-andmap odd? (dlist) (dlist 1)))
     (error? (dlist-andmap odd? (dlist) (dlist 1) (dlist) (dlist 1 1) (dlist)))

     ;; 1 dl
     (begin (define (test1 dl-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [dl0 (apply dl-proc n*)])
                (andmap-proc odd? dl0)))
            #t)
     (test1 dlist dlist-andmap)


     ;; 2 dls
     (begin (define (test2 dl-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [dl0 (apply dl-proc n*)]
                     [dl1 (apply dl-proc n*)])
                (andmap-proc = dl0 dl1)))
            #t)
     (test2 dlist dlist-andmap)


     ;; more dls
     (begin (define (test* dl-proc andmap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [dl0 (apply dl-proc n*)]
                     [dl1 (apply dl-proc n*)]
                     [dl2 (apply dl-proc n*)]
                     [dl3 (map-proc + dl0 dl1 dl2)])
                (andmap-proc (lambda (a b c d) (= d (+ a b c)))
                             dl0 dl1 dl2 dl3)))
            #t)
     (test* dlist dlist-andmap dlist-map)

     )


(mat dlist-ormap

     (error? (dlist-ormap #f (dlist)))

     (not (dlist-ormap odd? (dlist)))
     (not (dlist-ormap odd? (dlist) (dlist)))
     (not (dlist-ormap odd? (dlist) (dlist) (dlist) (dlist) (dlist)))

     (error? (dlist-ormap odd? (dlist) (dlist 1)))
     (error? (dlist-ormap odd? (dlist) (dlist 1) (dlist) (dlist 1 1) (dlist)))


     ;; 1 dl
     (begin (define (test1 dl-proc ormap-proc)
              (let* ([n* (snoc! (nums 1 100 2) 2)]
                     [dl0 (apply dl-proc n*)])
                (ormap-proc even? dl0)))
            #t)
     (test1 dlist dlist-ormap)

     ;; 2 dls
     (begin (define (test2 dl-proc ormap-proc)
              (let* ([n* (nums 1 100 2)]
                     [dl0 (apply dl-proc n*)]
                     [dl1 (apply dl-proc n*)])
                (ormap-proc (lambda (a b) (= (+ 49 49) (+ a b)))
                            dl0 dl1)))
            #t)
     (test2 dlist dlist-ormap)

     ;; more dls
     (begin (define (test* dl-proc ormap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [dl0 (apply dl-proc n*)]
                     [dl1 (apply dl-proc n*)]
                     [dl2 (apply dl-proc n*)]
                     [dl3 (apply dl-proc n*)])
                (ormap-proc (lambda (a b c d) (= (+ 33 33 33 33) (+ a b c d)))
                            dl0 dl1 dl2 dl3)))
            #t)
     (test* dlist dlist-ormap dlist-map)

     )


(mat dlist-fold-left

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x) (cons x acc))
                                '() dl0)
               (reverse ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x) (fx+ acc x))
                                0 dl0)
               (apply fx+ ls0)))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() dl0 dl1)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() dl0 dl0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 dl0 dl1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 dl0 dl0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (equal? (dlist-fold-left (lambda (acc x0 x1 x2 x3 x4) (cons (list x0 x1 x2 x3 x4) acc))
                                '() dl0 dl1 dl2 dl3 dl4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)]
            [ls '()])
       (equal? (dlist-fold-left (lambda (acc x0 x1 x2 x3 x4) (fx+ acc x0 x1 x2 x3 x4))
                                0 dl0 dl1 dl2 dl3 dl4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-fold-left/i

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x) (cons (list i x) acc))
                                  '() dl0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x) (fx+ i acc x))
                                  0 dl0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() dl0 dl1)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() dl0 dl0)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 dl0 dl1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 dl0 dl0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (cons (list i x0 x1 x2 x3 x4) acc))
                                  '() dl0 dl1 dl2 dl3 dl4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (fx+ i acc x0 x1 x2 x3 x4))
                                  0 dl0 dl1 dl2 dl3 dl4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-fold-right

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x acc) (cons x acc))
                                 '() dl0)
               ls0))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x acc) (fx+ acc x))
                                 0 dl0)
               (apply fx+ ls0)))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() dl0 dl1)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() dl0 dl0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 dl0 dl1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 dl0 dl0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x0 x1 x2 x3 x4 acc) (cons (list x0 x1 x2 x3 x4) acc))
                                 '() dl0 dl1 dl2 dl3 dl4)
               (zip ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-right (lambda (x0 x1 x2 x3 x4 acc) (fx+ acc x0 x1 x2 x3 x4))
                                 0 dl0 dl1 dl2 dl3 dl4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist-fold-right/i

     ;; one dlist
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x acc) (cons (list i x) acc))
                                   '() dl0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x acc) (fx+ i acc x))
                                   0 dl0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() dl0 dl1)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() dl0 dl0)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 dl0 dl1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 dl0 dl0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five dlists
     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (cons (list i x0 x1 x2 x3 x4) acc))
                                   '() dl0 dl1 dl2 dl3 dl4)
               (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [dl0 (apply dlist ls0)]
            [dl1 (apply dlist ls0)]
            [dl2 (apply dlist ls0)]
            [dl3 (apply dlist ls0)]
            [dl4 (apply dlist ls0)])
       (equal? (dlist-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (fx+ i acc x0 x1 x2 x3 x4))
                                   0 dl0 dl1 dl2 dl3 dl4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat dlist->vector

     (error? (dlist->vector #f))

     (eq? '#() (dlist->vector (dlist)))
     (equal? (viota 10)
             (dlist->vector (dlist-iota 10)))
     (equal? (vnums -10 10 2.5)
             (dlist->vector (dlist-nums -10 10 2.5)))

     )


(mat vector->dlist

     (error? (vector->dlist #f))

     (equal? (dlist) (vector->dlist (vector)))
     (equal? (dlist-iota 10)
             (vector->dlist (viota 10)))
     (equal? (dlist-nums -10 10 2.5)
             (vector->dlist (vnums -10 10 2.5)))

     )
