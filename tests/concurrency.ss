(import (cspp) (cspp concurrency))



(define runto
  (lambda (i)
    (let ([n 0])
      (let loop ()
        (if (= n i)
            n
            (begin (set! n (add1 n))
                   (loop)))))))

(define loopfor
  (lambda (i proc)
    (let loop ([n 0])
      (unless (fx= n i)
        (proc)
        (loop (fx1+ n))))))


(mat run-threads
     ;; no thread number
     (error? (run-threads
              (lambda () (runto 999999998))
              (lambda () (runto 999999997))))
     ;; bad arity
     (let ([res (run-threads 0
                             (lambda (i) (runto 999999998))
                             (lambda () (runto 999999997)))])
       (and (condition? (vector-ref res 0))
            (= 999999997 (vector-ref res 1))))

     (equal? (run-threads 0
                          (lambda () (runto 999999999))
                          (lambda () (runto 999999998))
                          (lambda () (runto 999999997)))
             '#(999999999 999999998 999999997))

     (equal? (run-threads 0
                          (lambda () (runto 999999999))
                          (lambda () (runto 999999998))
                          (lambda () (runto 999999997))
                          (lambda () (runto 999999996))
                          (lambda () (runto 999999995))
                          (lambda () (runto 999999994))
                          (lambda () (runto 999999993))
                          (lambda () (runto 999999992)))
             '#(999999999 999999998 999999997 999999996 999999995
                          999999994 999999993 999999992))

     (equal? (run-threads 3
                          (lambda () (runto 999999999))
                          (lambda () (runto 999999998))
                          (lambda () (runto 999999997))
                          (lambda () (runto 999999996))
                          (lambda () (runto 999999995))
                          (lambda () (runto 999999994))
                          (lambda () (runto 999999993))
                          (lambda () (runto 999999992)))
             '#(999999999 999999998 999999997 999999996 999999995 999999994 999999993 999999992))

     (equal? (run-threads 3
                          (lambda () (runto 999999999))
                          (lambda () (runto 999999998))
                          (lambda () (runto 999999997))
                          (lambda () (runto 999999996))
                          (lambda () (runto 999999995))
                          (lambda () (runto 999999994))
                          (lambda () (runto 999999993)))
             '#(999999999 999999998 999999997 999999996 999999995 999999994 999999993))

     ;; exceptions
     (let* ([t* (map (lambda (i) (lambda () (/ 0 0))) (iota 10))]
            [res* (apply run-threads 0 t*)])
       (vandmap condition? res*))

     (let* ([t* (map (lambda (i)
                       (lambda ()
                         (raise (condition
                                 (make-error)
                                 (make-message-condition "exception!")
                                 (make-irritants-condition (list i))))))
                     (iota 20))]
            [res* (apply run-threads 0 t*)])
       (andmap (lambda (l)
                 (let ([i (car l)] [c (cadr l)])
                   (and (condition? c)
                        (string=? "exception!" (condition-message c))
                        (= i (car (condition-irritants c))))))
               (zip (iota (vector-length res*)) (vector->list res*))))

     )


(mat abox

     (let ([abx (abox 0)] [n 9999999])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx)))))
       (= (abox-get abx) (* n 5)))
     (let ([abx (abox 0)] [n 9999999])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx)))))
       (= (abox-get abx) (* n 5)))


     (let ([abx (abox 0)] [n 9999999])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-get&add! abx 2))))
                    (lambda () (loopfor n (lambda () (abox-get&add! abx 3))))
                    (lambda () (loopfor n (lambda () (abox-get&add! abx 4))))
                    (lambda () (loopfor n (lambda () (abox-get&add! abx 5))))
                    (lambda () (loopfor n (lambda () (abox-get&add! abx 6)))))
       (= (abox-get abx) (* n (+ 2 3 4 5 6))))
     (let ([abx (abox 0)] [n 9999999])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-add&get! abx 2))))
                    (lambda () (loopfor n (lambda () (abox-add&get! abx 3))))
                    (lambda () (loopfor n (lambda () (abox-add&get! abx 4))))
                    (lambda () (loopfor n (lambda () (abox-add&get! abx 5))))
                    (lambda () (loopfor n (lambda () (abox-add&get! abx 6)))))
       (= (abox-get abx) (* n (+ 2 3 4 5 6))))


     (let* ([n 9999999] [abx (abox (* n 5))])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx)))))
       (= (abox-get abx) 0))
     (let* ([n 9999999] [abx (abox (* n 5))])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx)))))
       (= (abox-get abx) 0))


     (let* ([n 9999999] [abx (abox (* n (+ 2 3 4 5 6)))])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-get&sub! abx 2))))
                    (lambda () (loopfor n (lambda () (abox-get&sub! abx 3))))
                    (lambda () (loopfor n (lambda () (abox-get&sub! abx 4))))
                    (lambda () (loopfor n (lambda () (abox-get&sub! abx 5))))
                    (lambda () (loopfor n (lambda () (abox-get&sub! abx 6)))))
       (= (abox-get abx) 0))
     (let* ([n 9999999] [abx (abox (* n (+ 2 3 4 5 6)))])
       (run-threads 4
                    (lambda () (loopfor n (lambda () (abox-sub&get! abx 2))))
                    (lambda () (loopfor n (lambda () (abox-sub&get! abx 3))))
                    (lambda () (loopfor n (lambda () (abox-sub&get! abx 4))))
                    (lambda () (loopfor n (lambda () (abox-sub&get! abx 5))))
                    (lambda () (loopfor n (lambda () (abox-sub&get! abx 6)))))
       (= (abox-get abx) 0))


     (let ([abx (abox 0)] [n 9999999])
       (run-threads 6
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&add1! abx))))

                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx))))
                    (lambda () (loopfor n (lambda () (abox-get&sub1! abx)))))
       (= (abox-get abx) 0))
     (let ([abx (abox 0)] [n 9999999])
       (run-threads 6
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-add1&get! abx))))

                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx))))
                    (lambda () (loopfor n (lambda () (abox-sub1&get! abx)))))
       (= (abox-get abx) 0))


     ;; TODO get and set, update

     ;; abox-cas!

     )


(mat spinlock

     (let ([sp (make-spinlock)] [n 0])
       (run-threads 4
                    (lambda () (loopfor 9999999 (lambda () (spinlock-acquire sp) (set! n (add1 n)) (spinlock-release sp))))
                    (lambda () (loopfor 9999999 (lambda () (spinlock-acquire sp) (set! n (add1 n)) (spinlock-release sp))))
                    (lambda () (loopfor 9999999 (lambda () (spinlock-acquire sp) (set! n (add1 n)) (spinlock-release sp))))
                    (lambda () (loopfor 9999999 (lambda () (spinlock-acquire sp) (set! n (add1 n)) (spinlock-release sp))))
                    (lambda () (loopfor 9999999 (lambda () (spinlock-acquire sp) (set! n (add1 n)) (spinlock-release sp)))))
       (= n (* 9999999 5)))

     (let ([sp (make-spinlock)] [n 0])
       (run-threads 4
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n)))))))
       (= n (* 9999999 5)))

     (let ([sp (make-spinlock)] [n 0])
       (run-threads 4
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (sub1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (sub1 n))))))

                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n))))))
                    (lambda () (loopfor 9999999 (lambda () (with-spinlock sp (set! n (add1 n)))))))
       (= n 0))



     )



(mat threadpool

     ;; TODO
     ;; bounded bqueue
     ;; shutdown await

     (let* ([tp (make-threadpool 5)]
            [f* (map (lambda (i) (threadpool-add! tp
                                                  (lambda () (runto (+ i 999999999)))))
                     (iota 20))])
       (equal? (map future-get f*)
               (map (lambda (i) (+ i 999999999)) (iota 20))))

     (let* ([tp (make-threadpool 3)]
            [f* (map (lambda (i) (threadpool-add! tp
                                                  (lambda () (runto (+ i 9999999)))))
                     (iota 1000))])
       (equal? (map future-get f*)
               (map (lambda (i) (+ i 9999999)) (iota 1000))))


     ;; add tasks in multiple threads
     (let* ([tp (make-threadpool 5)]
            [n 100]
            [w (lambda ()
                 (let* ([x (+ 9999999 (random 999))]
                        [f* (map (lambda (i) (threadpool-add! tp (lambda () (runto x)))) (iota n))])
                   (cons (* x n) f*)))]
            [res* (run-threads 0 w w w w w w w w w w)])
       (= (vsum (vmap car res*))
          (vsum (vmap (lambda (f*) (apply + (map future-get f*)))
                      (vmap cdr res*)))))


     ;; multiple threads wait for future
     (let* ([n 999999999]
            [f (spawn-thread (lambda () (runto n)))]
            [w (lambda () (future-get f))])
       (equal? (run-threads 0 w w w w w w w w w w)
               (make-vector 10 n)))


     ;; exception handling
     (let* ([tp (make-threadpool 5)]
            [f* (map (lambda (i) (threadpool-add! tp (lambda () (/ 0 0))))
                     (iota 20))])
       (andmap condition? (map future-get f*)))

     (let* ([tp (make-threadpool 5)]
            [f* (map (lambda (i)
                       (threadpool-add! tp
                                        (lambda ()
                                          (raise (condition
                                                  (make-error)
                                                  (make-message-condition "exception!")
                                                  (make-irritants-condition (list i)))))))
                     (iota 20))])
       (andmap (lambda (l)
                 (let ([i (car l)] [c (cadr l)])
                   (and (condition? c)
                        (string=? "exception!" (condition-message c))
                        (= i (car (condition-irritants c))))))
               (zip (iota (length f*)) (map future-get f*))))

     )
