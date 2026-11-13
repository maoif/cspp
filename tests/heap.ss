(import (cspp) (cspp test)
        (cspp heap))


(mat heap

     (error? (heap? (make-heap)))
     (error? (heap? (make-heap 666)))
     (heap? (heap > 1 2 3))
     (= 3 (heap-size (heap > 1 2 3)))
     (not (heap-empty? (heap > 1 2 3)))
     (heap-empty? (heap >))
     (heap-bounded? (make-bounded-heap > 10))
     (not (heap-bounded? (heap <)))


     (begin (define (test1 n)
              (let* ([n* (iota n)]
                     [hp (make-heap <)]
                     [lb (make-list-builder)])
                (for-each (lambda (x) (heap-push! hp x)) n*)
                (for-each (lambda (x) (lb (heap-pop! hp))) n*)
                (and (equal? (lb) n*)
                     (heap-empty? hp))))
            #t)
     (test1 111)
     (test1 1000)
     (test1 3137)
     (test1 4643)
     (test1 39119)

     (begin (define (test2 n)
              (let* ([fxvn* (random-fxvector n)]
                     [n* (fxvector->list fxvn*)]
                     [hp (apply heap < n*)]
                     [lb (make-list-builder)])
                (for-each (lambda (x) (lb (heap-pop! hp))) n*)
                (and (equal? (lb) (sort < n*))
                     (heap-empty? hp))))
            #t)

     (test2 111)
     (test2 1000)
     (test2 3137)
     (test2 4643)
     (test2 39119)


     ;; bounded
     (begin (define (test-bounded n bd)
              (let* ([fxvn* (random-fxvector n)]
                     [n* (fxvector->list fxvn*)]
                     [sorted (sort > n*)]
                     [hp (make-bounded-heap < bd)])
                (for-each (lambda (x) (heap-push! hp x)) n*)
                (equal? (reverse (heap-pop-all! hp))
                        (list-head sorted bd))))
            #t)
     (test-bounded 4643 10)
     (test-bounded 4643 100)
     (test-bounded 4643 1000)


     ;; get top n items
     (begin (define (test-top n bd)
              (let* ([fxvn* (random-fxvector n)]
                     [n* (fxvector->list fxvn*)]
                     [sorted (sort > n*)]
                     [lb (make-list-builder)]
                     [hp (make-heap < bd)])
                (for-each (lambda (x)
                            (let ([s (heap-size hp)])
                              (if (fx< s bd)
                                  (heap-push! hp x)
                                  (when (and (> s 0)
                                             (> x (heap-peek hp)))
                                    (heap-pop! hp)
                                    (heap-push! hp x)))))
                          n*)
                (for-each (lambda (x) (lb (heap-pop! hp))) (iota bd))
                (equal? (reverse (lb)) (list-head sorted bd))))
            #t)
     (test-top 4643 10)
     (test-top 4643 100)
     (test-top 4643 1000)


     ;; push multiple
     (begin (define (test-push-multiple n)
              (let* ([fxvn* (random-fxvector n)]
                     [n* (fxvector->list fxvn*)]
                     [sorted (sort < n*)]
                     [lb (make-list-builder)]
                     [hp (make-heap <)])
                (apply heap-push! hp n*)
                (assert (= n (heap-size hp)))
                (equal? (heap-pop-all! hp) sorted)))
            #t)
     (test-push-multiple 1000)
     (test-push-multiple 3137)
     (test-push-multiple 4643)
     (test-push-multiple 39119)


     (begin (define (test-push-multiple-bounded n bd)
              (let* ([fxvn* (random-fxvector n)]
                     [n* (fxvector->list fxvn*)]
                     [sorted (sort > n*)]
                     [lb (make-list-builder)]
                     [hp (make-bounded-heap < bd)])
                (apply heap-push! hp n*)
                (assert (= bd (heap-size hp)))
                (equal? (reverse (heap-pop-all! hp)) (list-head sorted bd))))
            #t)
     (test-push-multiple-bounded 1000 50)
     (test-push-multiple-bounded 3137 1000)
     (test-push-multiple-bounded 4643 2000)
     (test-push-multiple-bounded 39119 10000)

     )


(mat heap-pop-all!

     (error? (heap-pop-all!))
     (error? (heap-pop-all! '()))
     (equal? '() (heap-pop-all! (make-heap <)))
     (equal? '() (heap-pop-all! (heap <)))

     ;; empty heap pop and peek are invalid
     (error? (heap-pop! (heap <)))
     (error? (heap-peek (heap <)))

     (let* ([n* (iota 1000)]
            [hp (apply heap > n*)])
       (equal? (heap-pop-all! hp)
               (reverse n*)))

     (let* ([n* (iota 1000)]
            [hp (apply heap > n*)]
            [lb (make-list-builder)])
       (heap-pop-all! hp
                      (lambda (x) (lb x)))
       (equal? (lb) (reverse n*)))

     )


(mat heap-copy

     (error? (heap-copy))
     (error? (heap-copy #f))

     (let* ([hp (apply heap > (iota 100))]
            [newhp (heap-copy hp)])
       (and (equal? (heap-pop-all! hp) (heap-pop-all! newhp))
            (not (eq? hp newhp))))

     )


(mat heap-contains?

     (error? (heap-contains?))
     (error? (heap-contains? (heap >)))
     (error? (heap-contains? '() 1))

     (begin (define (test n)
              (let* ([fxvn* (random-fxvector n)]
                     [hp (make-heap <)])
                (fxvfor-each (lambda (x)
                               (heap-push! hp x))
                             fxvn*)
                (assert (= n (heap-size hp)))
                (fxvandmap (lambda (x)
                             (heap-contains? hp x))
                           fxvn*)))
            #t)
     (test 1000)
     (test 3137)
     (test 4643)

     )


(mat heap-contains/p?

     (error? (heap-contains/p?))
     (error? (heap-contains/p? (heap >)))
     (error? (heap-contains/p? (heap >) 1))
     (error? (heap-contains/p? '() 1))

     (begin (define (test n)
              (let* ([fxvn* (random-fxvector n)]
                     [hp (make-heap <)])
                (fxvfor-each (lambda (x)
                               (heap-push! hp x))
                             fxvn*)
                (assert (= n (heap-size hp)))
                (fxvandmap (lambda (x)
                             (heap-contains/p? hp (lambda (y) (fx= x y))))
                           fxvn*)))
            #t)
     (test 1000)
     (test 3137)
     (test 4643)

     )
