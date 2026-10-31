(import (cspp))

;; A managed type error names its procedure; an invalid memory reference does not.
(define iter-type-error?
  (lambda (run)
    (guard (condition [(who-condition? condition) #t] [else #f])
      (run)
      #f)))

(define collect-iter
  (lambda (iter)
    (let loop ([acc '()])
      (let ([value (iter-next! iter)])
        (if (iter-end? value)
            (reverse acc)
            (loop (cons value acc)))))))

(define reset-sequence?
  (lambda (expected source)
    (let ([iter (source->iter source)])
      (and (equal? expected (collect-iter iter))
           (begin
             (iter-reset! iter)
             (equal? expected (collect-iter iter)))))))

(mat iterator-combinator-expansion
     (equal? '(1 2 3 4)
             (iter->list (iter-concat (range 1 3) (range 3 5))))

     ;; At least one source iterator is required.
     (error? (iter-concat))

     (equal? '(1 2 3)
             (iter->list (iter-distinct equal? (list->iter '(1 2 1 3 2)))))

     (equal? '(1 2 3)
             (iter->list (iter-sorted < (list->iter '(3 1 2)))))

     (equal? '#(1 2 3) (iter->vector (list->iter '(1 2 3))))

     ;; Conversion rejects non-iterators.
     (error? (iter->vector 42))

     (let ([iter (iter-concat (range 1 3) (range 3 5))])
       (and (equal? '(1 2 3 4) (collect-iter iter))
            (begin (iter-reset! iter)
                   (equal? '(1 2 3 4) (collect-iter iter)))))

     (let* ([source (list->iter '(1 2))]
            [iter (iter-concat source source)])
       (equal? '(1 2 1 2) (collect-iter iter)))

     (let ([iter (iter-distinct equal? (list->iter '(1 2 1 3 2)))])
       (and (equal? '(1 2 3) (collect-iter iter))
            (begin (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))

     (let ([iter (iter-sorted < (list->iter '(3 1 2)))])
       (and (equal? '(1 2 3) (collect-iter iter))
            (begin (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     )


(mat registered-source-iterators

     (reset-sequence? '(1 2 3) (array 1 2 3))
     (reset-sequence? '(1 2 3) (fxarray 1 2 3))
     (reset-sequence? '(1 2 3) (bytearray 1 2 3))
     (reset-sequence? '(1 2 3) (dlist 1 2 3))
     (reset-sequence? '(1 2 3) (queue 1 2 3))
     (reset-sequence? '(3 2 1) (stack 1 2 3))
     (reset-sequence? '(1 2 3) (heap < 3 1 2))
     (reset-sequence? '(1 2 3) (treeset = < 3 1 2))
     (reset-sequence? '((1 . a) (2 . b))
                      (treemap = < '(2 . b) '(1 . a)))
     (reset-sequence? '((1 . 10) (2 . 20))
                      (fxtreemap fx= fx< '(2 . 20) '(1 . 10)))
     (reset-sequence? '(1 3 5) (bitvec 5 1 3))
     (reset-sequence? '(1 3 5) (bittree 5 1 3))
     (reset-sequence? '(0 1 2 3) (make-dset 4))

     (let ([source (hashset 3 1 2)]
           [iter #f])
       (set! iter (source->iter source))
       (let ([first (sort < (collect-iter iter))])
         (iter-reset! iter)
         (and (equal? '(1 2 3) first)
              (equal? '(1 2 3) (sort < (collect-iter iter))))))

     ;; invalid registration callbacks are rejected at the public boundary
     (error? (iter-register-source! 1 (lambda (source) source)))
     (error? (iter-register-source! pair? 1)))


(mat registered-array-iterators-are-live

     (let* ([source (array 0 1)]
            [iter (source->iter source)])
       (array-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (array-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))
     (let* ([source (fxarray 0 1)]
            [iter (source->iter source)])
       (fxarray-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (fxarray-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))
     (let* ([source (bytearray 0 1)]
            [iter (source->iter source)])
       (bytearray-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (bytearray-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))

     )

(mat direct-dlist-iterator
     ;; A dlist iterator reads live node values and reset sees current contents.
     (let* ([dl (dlist 1 2)]
            [iter (dlist->iter dl)])
       (dlist-set! dl 0 9)
       (and (equal? '(9 2) (collect-iter iter))
            (begin
              (dlist-push-back! dl 3)
              (iter-reset! iter)
              (equal? '(9 2 3) (collect-iter iter))))))

(mat registered-source-iterators-see-reset-mutations
     (let* ([arr (array 1 2)] [iter (iter-source->iter arr)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (array-push-back! arr 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([dl (dlist 1 2)] [iter (iter-source->iter dl)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (dlist-push-back! dl 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([q (queue 1 2)] [iter (iter-source->iter q)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (queue-push! q 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([stk (stack 1 2)] [iter (iter-source->iter stk)])
       (and (equal? '(2 1) (collect-iter iter))
            (begin (stack-push! stk 3)
                   (iter-reset! iter)
                   (equal? '(3 2 1) (collect-iter iter)))))
     (let* ([hp (heap < 2 1)] [iter (iter-source->iter hp)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (heap-push! hp 0)
                   (iter-reset! iter)
                   (equal? '(0 1 2) (collect-iter iter)))))
     (let* ([hs (hashset 1 2)] [iter (iter-source->iter hs)])
       (and (equal? '(1 2) (sort < (collect-iter iter)))
            (begin (hashset-add! hs 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (sort < (collect-iter iter))))))
     (let* ([ts (treeset = < 2 1)] [iter (iter-source->iter ts)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (treeset-add! ts 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([tm (treemap = < '(2 . b) '(1 . a))]
            [iter (iter-source->iter tm)])
       (and (equal? '((1 . a) (2 . b)) (collect-iter iter))
            (begin (treemap-set! tm 3 'c)
                   (iter-reset! iter)
                   (equal? '((1 . a) (2 . b) (3 . c)) (collect-iter iter)))))
     (let* ([bv (bitvec 1 3)] [iter (iter-source->iter bv)])
       (and (equal? '(1 3) (collect-iter iter))
            (begin (bitvec-set! bv 2)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([bt (bittree 1 3)] [iter (iter-source->iter bt)])
       (and (equal? '(1 3) (collect-iter iter))
            (begin (bittree-set! bt 2)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([ds (make-dset 4)] [iter (iter-source->iter ds)])
       (and (equal? '(0 1 2 3) (collect-iter iter))
            (begin (dset-union! ds 0 1)
                   (iter-reset! iter)
                   (equal? '(0 1 2 3) (collect-iter iter))))))


(mat iterator-dispatch

     (let ([ht (make-hashtable equal-hash equal?)])
       (hashtable-set! ht 'a 1)
       (hashtable-set! ht 'b 2)
       (equal? '(1 2) (sort < (collect-iter (iter-source->iter ht)))))

     (let ([arr (array 1 2 3)])
       (equal? '(1 2 3) (collect-iter (get-iter 'test arr))))

     (let ([ht (make-hashtable equal-hash equal?)])
       (hashtable-set! ht 'a 1)
       (let ([iter (iter-source->iter ht)])
         (and (equal? '(1) (collect-iter iter))
              (begin
                (hashtable-delete! ht 'a)
                (hashtable-set! ht 'b 2)
                (iter-reset! iter)
                (equal? '(2) (collect-iter iter))))))

     )

(mat iterator-lifecycle
  (let ([iterator (range 1)])
    (and (equal? '(0) (iter->list iterator))
         (iter-end? (iter-next! iterator))
         (begin (iter-reset! iterator) #t)
         (equal? '(0) (iter->list iterator))))

  (call-with-port (open-input-string "a\nb\n")
    (lambda (port)
      (iter->list (port-lines->iter port))
      (not (port-closed? port))))

  (let ([path "iter-owned-port-test.txt"])
    (call-with-output-file path (lambda (port) (put-string port "a\nb\n")))
    (let ([iterator (file-lines->iter path)])
      (let ([result (iter->list (iter-concat iterator iterator))])
        (delete-file path)
        (and (equal? '("a" "b" "a" "b") result) (iter-finalized? iterator))))))

(mat duplicate-source-registration
     (let* ([source 424242]
            [predicate (lambda (x) (and (number? x) (= x source)))])
       (iter-register-source!
        predicate
        (lambda (_) (make-iter (lambda () 'old) (lambda () void))))
       (and (guard (condition [else #t])
              (iter-register-source!
               predicate
               (lambda (_) (make-iter (lambda () 'new) (lambda () void))))
              #f)
            (eq? 'old (iter-next! (iter-source->iter source)))))
     (let* ([source 434343]
            [first? (lambda (x) (and (number? x) (= x source)))]
            [second? (lambda (x) (and (number? x) (= x source)))])
       (iter-register-source!
        first?
        (lambda (_) (make-iter (lambda () 'old) (lambda () void))))
       (iter-register-source!
        second?
        (lambda (_) (make-iter (lambda () 'new) (lambda () void))))
       (eq? 'new (iter-next! (iter-source->iter source)))))


(mat iterators

     ;; range
     (let ([r (range 4)])
       (and (eq? 0 (iter-next! r))
            (eq? 1 (iter-next! r))
            (eq? 2 (iter-next! r))
            (eq? 3 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     (let ([r (range 4 8)])
       (and (eq? 4 (iter-next! r))
            (eq? 5 (iter-next! r))
            (eq? 6 (iter-next! r))
            (eq? 7 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     (let ([r (range 4 10 2)])
       (and (eq? 4 (iter-next! r))
            (eq? 6 (iter-next! r))
            (eq? 8 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     ;; A descending range requires an explicitly negative step.
     (error? (range 10 0))


     ;; nums
     (equal? (iota 10) (nums 10))
     (equal? '(1 3 5 7 9) (nums 1 10 2))
     (equal? '(0.0 0.5 1.0 1.5) (nums 0.0 2.0 0.5))

     ;; conversions
     (eq? iter-end (iter-next! (list->iter '())))
     (eq? iter-end (iter-next! (list->iter '(1 2) 0 0)))
     (eq? iter-end (iter-next! (list->iter '(1 2 3) 3 0)))

     (eq? iter-end (iter-next! (vector->iter '#())))
     (eq? iter-end (iter-next! (vector->iter '#(1 2) 0 0)))
     (eq? iter-end (iter-next! (vector->iter '#(1 2 3) 3 0)))

     (eq? iter-end (iter-next! (string->iter "")))
     (eq? iter-end (iter-next! (string->iter "123" 0 0)))
     (eq? iter-end (iter-next! (string->iter "1234" 3 0)))

     (eq? iter-end (iter-next! (bytevector->iter #vu8())))
     (eq? iter-end (iter-next! (fxvector->iter '#vfx())))
     (eq? iter-end (iter-next! (flvector->iter '#vfl())))

     (equal? (iter->list (range 0 100 5))
             (iter->list (list->iter (iota 100) 0 100 5)))
     (equal? (nums 0 100 2)
             (iter->list (list->iter (iota 100) 0 100 2)))
     (equal? (nums 1 100 2)
             (iter->list (list->iter (iota 100) 1 100 2)))

     (equal? (iter->list (range 0 100 5))
             (iter->list (vector->iter (list->vector (iota 100)) 0 100 5)))
     (equal? (nums 0 100 2)
             (iter->list (vector->iter (list->vector (iota 100)) 0 100 2)))
     (equal? (nums 1 100 2)
             (iter->list (vector->iter (list->vector (iota 100)) 1 100 2)))

     (let ([s "abc123345jskljdla"])
       (equal? (string->list s) (iter->list (string->iter s))))
     (let ([s "abc123345jskljdla"])
       (equal? (iter->list (list->iter (string->list s) 1 30 3))
               (iter->list (string->iter s 1 30 3))))

     (equal? '(1 2 3)
             (iter->list (bytevector->iter #vu8(1 2 3))))
     (equal? '(2 4)
             (iter->list (bytevector->iter #vu8(0 1 2 3 4 5) 2 6 2)))

     (equal? '(1 2 3)
             (iter->list (fxvector->iter '#vfx(1 2 3))))
     (equal? '(2 4)
             (iter->list (fxvector->iter '#vfx(0 1 2 3 4 5) 2 6 2)))

     (equal? '(1.0 2.5)
             (iter->list (flvector->iter '#vfl(1.0 2.5))))
     (equal? '(1.5 3.5)
             (iter->list (flvector->iter '#vfl(0.5 1.5 2.5 3.5) 1 4 2)))

     )


(mat iter-directional-indexes

     (equal? '(8 6 4) (iter->list (vector->iter '#(0 1 2 3 4 5 6 7 8 9)
                                                   8 2 -2)))
     (equal? '(#\8 #\6 #\4) (iter->list (string->iter "0123456789" 8 2 -2)))
     (equal? '(8 6 4) (iter->list (bytevector->iter #vu8(0 1 2 3 4 5 6 7 8 9)
                                                       8 2 -2)))
     (equal? '(8 6 4) (iter->list (fxvector->iter '#vfx(0 1 2 3 4 5 6 7 8 9)
                                                       8 2 -2)))
     (equal? '(8.0 6.0 4.0)
             (iter->list
              (flvector->iter '#vfl(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
                              8 2 -2)))
     (equal? '(8 6 4) (iter->list (vector->iter '#(0 1 2 3 4 5 6 7 8 9)
                                                   -2 -8 -2)))
     (equal? '() (iter->list (vector->iter '#(0 1 2) 1 1 -1)))
     (equal? '() (iter->list (vector->iter '#() 1 2)))

     ;; A zero step cannot advance a vector iterator.
     (error? (vector->iter '#(1 2) 0 2 0))

     ;; A zero step cannot advance a string iterator.
     (error? (string->iter "12" 0 2 0))

     ;; List iterators support forward traversal only.
     (error? (list->iter '(1 2) 0 2 -1)))


(mat iter-directional-ranges

     (equal? '(5 3 1) (iter->list (range 5 0 -2)))
     (equal? '(0 2 4) (iter->list (range 0 5 2)))
     (reset-sequence? '(5 3 1) (range 5 0 -2))
     (reset-sequence? '(0 2 4) (range 0 5 2))
     (equal? '() (iter->list (range 2 2 -1)))

     ;; A zero step cannot advance a range.
     (error? (range 0 5 0))

     ;; An ascending range requires a positive step.
     (error? (range 0 5 -1))

     ;; A descending range requires a negative step.
     (error? (range 5 0 1)))


(mat iter-ops

     (equal? '(0 1 2 3)
             (iter->list (range 4)))

     (equal? '(0 1 4 9)
             (iter->list (iter-map (sect expt _ 2) (range 4))))

     (equal? '(0 2 4 6 8)
             (iter->list (iter-filter even? (range 10))))

     (let ([it (range 10)]
           [it1 (range 10)])
       (equal? '((1 0) (3 2) (5 4) (7 6) (9 8))
               (iter->list (iter-zip (iter-filter odd? it)
                                     (iter-filter even? it1)))))

     (let ([it (range 5)]
           [it1 (range 5)])
       (equal? '(1 3 0 2 4)
               (iter->list (iter-append (iter-filter odd? it)
                                        (iter-filter even? it1)))))

     (equal? '(0 1 2) (iter->list (iter-take 3 (range 20))))
     (equal? '(17 18 19) (iter->list (iter-drop 17 (range 20))))

     (let ([it (range 50)]
           [it1 (range 50)])
       (equal? '((1 0) (9 4) (25 16))
               (iter->list (iter-zip (iter-take 3 (iter-filter odd? (iter-map (sect expt _ 2) it)))
                                     (iter-take 3 (iter-filter even? (iter-map (sect expt _ 2) it1)))))))

     ;; iter-interleave
     ;; same length
     (equal? '(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14 17 16 19 18)
             (let ([it1 (range 20)]
                   [it2 (range 20)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)))))
     ;; one shorter
     (equal? '(1 0 3 2 5 4 7 6 9 8 10 12 14 16 18)
             (let ([it1 (range 10)]
                   [it2 (range 20)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)))))
     (equal? '(1 0 -20.0 3 2 -17.0 5 4 -14.0 7 6 -11.0 9 8 -8.0 10 -5.0 12
                 -2.0 14 1.0 16 4.0 18 7.0 10.0 13.0 16.0 19.0)
             (let ([it1 (range 10)]
                   [it2 (range 20)]
                   [it3 (range -20.0 20.0 3)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)
                                            it3))))

     (= 6
        (iter-fold (lambda (acc x) (+ acc x)) 0 (range 4)))

     (= 999
        (iter-fold (lambda (acc x) (if acc (if (> x acc) x acc) x))
                   #f
                   (range 1000)))

     (= 999 (iter-max (range 1000)))
     (= 0 (iter-min (range 1000)))
     (= 999 (iter-min > (range 1000)))
     (= 0 (iter-max < (range 1000)))

     (= (apply + (iota 10000))
        (iter-sum (range 10000)))
     (= (apply * (iota 10000))
        (iter-product (range 10000)))
     (= (/ (apply + (iota 10000)) 10000)
        (iter-avg (range 10000)))

     (fx= (apply fx+ (iota 10000))
          (iter-fxsum (range 10000)))
     (fx= (apply * (iota 10000))
          (iter-fxproduct (range 10000)))
     (= (fx/ (apply fx+ (iota 10000)) 10000)
        (iter-fxavg (range 10000)))


     (fl= (apply fl+ (nums 0.0 10000))
          (iter-flsum (range 0.0 10000)))
     (fl= (apply fl* (nums 0.0 10000))
          (iter-flproduct (range 0.0 10000)))
     (fl= (/ (apply fl+ (nums 0.0 10000)) 10000)
          (iter-flavg (range 0.0 10000)))

     (and (not (iter-avg (range 0)))
          (not (iter-fxavg (range 0)))
          (not (iter-flavg (range 0.0))))

     )


(mat iter-finalize


     (let* ([it (range 10)]
            [ls (iter->list it)])
       (and (equal? ls (iota 10))
            (not (iter-finalized? it))))

     (let* ([it (range 10)]
            [i 0])
       (iter-for-each (lambda (x) (incr! i)) it)
       (= i 10))

     (let ([r (range 10)])
       (iter->list r)
       (iter-finalize! r)
       (not (iter-finalized? r)))

     )


(mat iter-ports/files

     ;; port->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str* (map (lambda (x) (format "~a~n" x)) ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([str* str*])
             (unless (null? str*)
               (put-string p (car str*))
               (loop (cdr str*))))))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (iter-map (lambda (x) (format "~a~n" x))
                                                     (port->iter p)))])
                        (equal? str* s))))])
         (delete-file file)
         res))
     ;;port-chars->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str (format "~a" ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (put-string p str)))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (port-chars->iter p))])
                        (equal? str (apply string s)))))])
         (delete-file file)
         res))
     ;; port-data->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([ls ls])
             (unless (null? ls)
               (put-datum p (car ls))
               (loop (cdr ls))))))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (port-data->iter p))])
                        (equal? ls s))))])
         (delete-file file)
         res))


     ;; file->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str* (map (lambda (x) (format "~a~n" x)) ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([str* str*])
             (unless (null? str*)
               (put-string p (car str*))
               (loop (cdr str*))))))
       (let ([res (let ([s (iter->list (iter-map (lambda (x) (format "~a~n" x))
                                                 (file->iter file)))])
                    (equal? str* s))])
         (delete-file file)
         res))
     ;;file-chars->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str (format "~a" ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (put-string p str)))
       (let ([res (let ([s (iter->list (file-chars->iter file))])
                    (equal? str (apply string s)))])
         (delete-file file)
         res))
     ;; file-data->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([ls ls])
             (unless (null? ls)
               (put-datum p (car ls))
               (loop (cdr ls))))))
       (let ([res (equal? ls (iter->list (file-data->iter file)))])
         (delete-file file)
         res))




     )


(mat iterator-concurrent-registration
  (let* ([mutex (make-mutex)] [changed (make-condition)] [open? #f]
         [accepted 0] [seen 0] [count? #f]
         [predicates
          (list->vector
           (map (lambda (index)
                  (lambda (source)
                    (when count? (set! seen (+ seen 1)))
                    #f))
                (iota 320)))]
         [workers
          (map (lambda (worker)
                 (fork-thread
                  (lambda ()
                    (with-mutex mutex
                      (let loop ()
                        (unless open? (condition-wait changed mutex) (loop))))
                    (do ([index worker (+ index 8)]) ((>= index 320))
                      ;; Two workers compete for each predicate: exactly one must succeed.
                      (guard (exn [else (void)])
                        (iter-register-source! (vector-ref predicates index)
                                               (lambda (source) (list->iter '())))
                        (with-mutex mutex (set! accepted (+ accepted 1))))))))
               '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7))])
    (with-mutex mutex (set! open? #t) (condition-broadcast changed))
    (for-each thread-join workers)
    (set! count? #t)
    (iterable? #f)
    (set! count? #f)
    (and (= accepted 320) (= seen 320)))

  ;; Source predicates run outside the registration lock and scan a stable snapshot.
  (let ([register? #t] [new-seen? #f])
    (iter-register-source!
     (lambda (source)
       (when register?
         (set! register? #f)
         (iter-register-source! (lambda (source) (set! new-seen? #t) #f)
                                (lambda (source) (list->iter '()))))
       #f)
     (lambda (source) (list->iter '())))
    (iterable? #f)
    (let ([snapshot? (not new-seen?)])
      (iterable? #f)
      (and snapshot? new-seen?))))

(mat iter-take-stops-producer
  (let ([pulls 0])
    (let ([source (make-iter
                   (lambda () (set! pulls (fx1+ pulls)) iter-end)
                   (lambda () (set! pulls 0)))])
      (and (iter-end? (iter-next! (iter-take 0 source))) (= pulls 0))))

  (let ([pulls 0])
    (let ([source (make-iter
                   (lambda ()
                     (set! pulls (fx1+ pulls))
                     ;; A third producer pull after take 2 is a library error.
                     (when (> pulls 2) (error 'take-test "unexpected pull"))
                     pulls)
                   (lambda () (set! pulls 0)))])
      (guard (exn [else #f])
        (and (equal? '(1 2) (iter->list (iter-take 2 source))) (= pulls 2)))))

  (let ([source (iter-filter even? (iter-take 3 (range 10)))])
    (equal? '(0 2) (iter->list source)))

  (let ([source (iter-take 3 (iter-filter even? (range 10)))])
    (equal? '(0 2 4) (iter->list source))))

(mat iter-reset-and-resource-chains
  (let ([source (iter-take 2 (iter-drop 1 (iter-map add1 (list->iter '(1 2 3 4)))))])
    (and (equal? '(3 4) (iter->list source))
         (not (iter-finalized? source))
         (begin (iter-reset! source) #t)
         (equal? '(3 4) (iter->list source))))

  (let ([source (list->iter '(1 2))])
    (equal? '(1 2 1 2 1 2) (iter->list (iter-concat source source source))))

  (let ([closed 0] [cursor 0])
    (let* ([source (make-iter
                    (lambda () (if (= cursor 2) iter-end
                                   (begin (set! cursor (+ cursor 1)) cursor)))
                    (lambda () (set! cursor 0))
                    (lambda () (set! closed (+ closed 1))))]
           [output (iter-concat source source)])
      (and (= 1 (iter-next! output))
           (equal? '(2 1 2) (iter->list output))
           (= closed 1) (iter-finalized? source)
           (begin (iter-finalize! output) #t) (= closed 1))))

  (andmap
   (lambda (wrap)
     (let ([closed 0])
       (let* ([source (make-iter (lambda () iter-end) void
                               (lambda () (set! closed (+ closed 1))))]
              [output (wrap source)])
         (iter->list output)
         (= closed 1))))
   (list (lambda (source) (iter-concat source))
         (lambda (source) (iter-append source (range 0)))
         (lambda (source) (iter-zip source (range 0)))
         (lambda (source) (iter-interleave source (range 0)))
         (lambda (source) (iter-distinct equal? source))
         (lambda (source) (iter-sorted < source)))))

(mat iter-indexes-are-exact
  (let ([constructor (make-indexed-iter 'exact-index vector? vector-length
                       (lambda (source index) (exact? index)))])
    (and (equal? '(#t #t #t) (iter->list (constructor '#(a b c) 0.0 3.0 1.0)))
         (equal? '(#t #t) (iter->list (constructor '#(a b c) -1.0 -4.0 -2.0)))))

  ;; Invalid adapter lengths must fail before a native indexed read can occur.
  (error? ((make-indexed-iter 'bad-length vector? (lambda (source) -1) vector-ref) '#(1))))

(mat iter-inexact-native-bounds
  (equal? '(10 30) (iter->list (vector->iter '#(10 20 30) 0.0 3.0 2.0)))

  (equal? '(#\c #\a) (iter->list (string->iter "abc" -1.0 -4.0 -2.0)))

  (equal? '(30 10) (iter->list (bytevector->iter #vu8(10 20 30) -1.0 -4.0 -2.0)))

  (equal? '(10 30) (iter->list (fxvector->iter (fxvector 10 20 30) 0.0 3.0 2.0)))

  (equal? '(30.0 10.0)
          (iter->list (flvector->iter (flvector 10.0 20.0 30.0) -1.0 -4.0 -2.0)))

  (equal? '() (iter->list (vector->iter '#(10 20) 2.0 3.0 1.0)))

  (equal? '() (iter->list (vector->iter '#(10 20) -5.0 -3.0 -1.0))))

(mat iter-terminal-closes-unvisited-sources
  (let ([closed 0])
    (let* ([resource (make-iter (lambda () iter-end) void
                               (lambda () (set! closed (+ closed 1))))]
           [chain (iter-take 1 (iter-concat (range 3) resource))])
      (and (equal? '(0) (iter->list chain)) (= closed 1)))))

(mat iter-shared-chain-finalization
  ;; A repeated-handle graph has many paths but only 65 unique cursor nodes.
  (let loop ([depth 0] [chain (list->iter '(1))])
    (if (= depth 64)
        (begin (iter-finalize! chain) (not (iter-finalized? chain)))
        (loop (+ depth 1) (iter-concat chain chain)))))

(mat iter-resource-lifecycle-boundaries
  ;; A resource cursor is disabled after explicit finalization; plain cursors are not.
  (let ([closed 0])
    (let ([source (make-iter (lambda () 'value) void
                             (lambda () (set! closed (+ closed 1))))])
      (iter-finalize! source)
      (and (= closed 1)
           (guard (exn [else #t]) (iter-next! source) #f)
           (guard (exn [else #t]) (iter-reset! source) #f)
           (begin (iter-finalize! source) #t)
           (= closed 1))))

  (let ([resets 0] [source #f])
    (set! source (make-iter (lambda () iter-end) (lambda () (set! resets (+ resets 1)))))
    (let ([chain (iter-concat source source source)])
      (iter-reset! chain)
      (= resets 1))))

(mat iter-numeric-terminals-finalize
  (andmap
   (lambda (terminal)
     (let ([remaining '(1 2)] [closed 0])
       (let ([source (make-iter
                      (lambda ()
                        (if (null? remaining) iter-end
                            (let ([value (car remaining)])
                              (set! remaining (cdr remaining)) value)))
                      (lambda () (set! remaining '(1 2)))
                      (lambda () (set! closed (+ closed 1))))])
         (terminal source)
         (= closed 1))))
   (list iter-sum iter-product iter-avg iter-fxsum iter-fxproduct iter-fxavg
         iter-max iter-min))

  (andmap
   (lambda (terminal)
     (let ([remaining '(1.0 2.0)] [closed 0])
       (let ([source (make-iter
                      (lambda ()
                        (if (null? remaining) iter-end
                            (let ([value (car remaining)])
                              (set! remaining (cdr remaining)) value)))
                      (lambda () (set! remaining '(1.0 2.0)))
                      (lambda () (set! closed (+ closed 1))))])
         (terminal source)
         (= closed 1))))
   (list iter-flsum iter-flproduct iter-flavg))

  ;; Explicit arithmetic checks must reject overflow instead of wrapping the result.
  (andmap
   (lambda (terminal)
     (iter-type-error?
      (lambda () (terminal (list->iter (list (most-positive-fixnum) 1))))))
   (list iter-fxsum iter-fxavg))

  ;; A product can overflow even though both inputs fit in fixnums.
  (iter-type-error?
   (lambda () (iter-fxproduct (list->iter (list (most-positive-fixnum) 2)))))

  (= -1 (iter-fxavg (list->iter '(-1 -2)))))


(mat iter-review-reset-current-list-slice
  (let* ([data (list 1 2 3)] [cursor (list->iter data 1 3)])
    (and (equal? '(2 3) (iter->list cursor))
         (begin
           (set-cdr! data (list 20 30))
           (iter-reset! cursor)
           (equal? '(20 30) (iter->list cursor))))))

(mat iter-review-false-extrema
  (not (iter-max (lambda (left right) (and (not left) right))
                 (list->iter '(#f #t))))

  (not (iter-min (lambda (left right) (and (not left) right))
                 (list->iter '(#f #t))))

  (eq? #t (iter-max (lambda (left right) (and left (not right)))
                    (list->iter '(#f #t))))

  ;; Comparator validation must not depend on source cardinality.
  (error? (iter-max 42 (list->iter '())))

  ;; Even a singleton requires a procedure comparator.
  (error? (iter-min 42 (list->iter '(1))))

  ;; Extrema require iterator input before starting a terminal operation.
  (error? (iter-max > '(1 2))))

(mat iter-review-zip-exhaustion
  (let ([pulls 0] [position 0])
    (let ([cursor (iter-zip
                   (list->iter '(a))
                   (make-iter (lambda ()
                                (set! pulls (+ pulls 1))
                                (set! position (+ position 1)) position)
                              (lambda () (set! position 0))))])
      (and (equal? '(a 1) (iter-next! cursor))
           (iter-end? (iter-next! cursor))
           (let ([at-end pulls])
             (and (iter-end? (iter-next! cursor))
                  (iter-end? (iter-next! cursor))
                  (= at-end pulls)))
           (begin (iter-reset! cursor) #t)
           (equal? '(a 1) (iter-next! cursor))))))

(mat iter-review-shared-reset-graph
  (let ([resets 0])
    (let loop ([depth 0]
               [left (make-iter (lambda () iter-end)
                                (lambda () (set! resets (+ resets 1))))]
               [right (list->iter '())])
      (if (= depth 16)
          (begin (iter-reset! left) (= resets 1))
          (loop (+ depth 1) (iter-concat left right) (iter-concat left right)))))

  ;; Wrapper-local reset must preserve stage counters and refresh buffered sorting.
  (let* ([data (list 3 1 2)]
         [sorted (iter-sorted < (iter-take 2 (list->iter data)))]
         [cursor (iter-distinct = (iter-concat sorted sorted))])
    (and (equal? '(1 3) (iter->list cursor))
         (begin
           (set-car! data 0)
           (iter-reset! cursor)
           (equal? '(0 1) (iter->list cursor))))))

(mat iter-review-interleave-reset
  (let* ([source (list->iter '(1 2))]
         [cursor (iter-interleave (list->iter '()) source source (list->iter '(3)))])
    (and (equal? '(1 2 3) (iter->list cursor))
         (iter-end? (iter-next! cursor))
         (begin (iter-reset! cursor) #t)
         (equal? '(1 2 3) (iter->list cursor)))))

(mat iter-review-end-cursor
  (and (iter? iter-end) (iterable? iter-end)
       (iter-end? (iter-next! iter-end))
       (begin (iter-reset! iter-end) #t)
       (iter-end? (iter-next! iter-end))
       (equal? '() (iter->list iter-end)))

  ;; The process-wide exhaustion marker cannot carry caller pipeline configuration.
  (error? (iter-map values iter-end))

  ;; An empty limit must not permanently alter the shared exhaustion marker.
  (error? (iter-take 0 iter-end)))

(mat iter-current-exhaustion-on-continuation-resumption
  (let ([saved #f] [phase 0] [pulls 0])
    (let ([cursor
           (iter-take
            2 (iter-map
               (lambda (value)
                 (if saved value
                     ;; Resume upstream control while retaining the current take allowance.
                     (call/cc (lambda (continuation) (set! saved continuation) value))))
               (make-iter (lambda () (set! pulls (+ pulls 1)) pulls) void)))])
      (let ([value (iter-next! cursor)])
        (if (= phase 0)
            (begin (set! phase 1) (iter-next! cursor) (saved 'resumed))
            (and (iter-end? value) (iter-end? (iter-next! cursor)) (= pulls 2))))))

  (let ([saved #f] [phase 0] [pulls 0])
    (let ([cursor
           (iter-interleave
            (make-iter
             (lambda ()
               (set! pulls (+ pulls 1))
               ;; Bound the old exhausted-ring polling defect instead of hanging the suite.
               (when (> pulls 8) (error 'producer "read repeatedly after exhaustion"))
               (if saved iter-end
                   ;; Capture inside the first producer before its slot is removed.
                   (call/cc (lambda (continuation) (set! saved continuation) 'first))))
             void)
            (list->iter '()))])
      (guard (condition [else #f])
        (let ([value (iter-next! cursor)])
          (if (= phase 0)
              (begin (set! phase 1) (iter-next! cursor) (saved iter-end))
              (and (iter-end? value) (iter-end? (iter-next! cursor)) (= pulls 2))))))))

(mat iter-textual-port-input-direction
  ;; Reading constructors reject output-only ports immediately, without consuming anything.
  (andmap
   (lambda (constructor)
     (call-with-port (open-output-string)
       (lambda (port) (guard (condition [else #t]) (constructor port) #f))))
   (list port->iter port-lines->iter port-chars->iter port-data->iter)))

(mat iter-interleave-continuation-removed-slot
  (let ([saved #f] [phase 0])
    (let ([cursor
           (iter-interleave
            (make-iter
             (lambda ()
               (if saved iter-end
                   ;; Replay this slot after it has been removed while another remains live.
                   (call/cc (lambda (continuation) (set! saved continuation) 'a))))
             void)
            (list->iter '(b1 b2 b3)))])
      (let ([value (iter-next! cursor)])
        (if (= phase 0)
            (begin
              (set! phase 1)
              (iter-next! cursor)
              (iter-next! cursor)
              (saved iter-end))
            (and (eq? 'b3 value) (iter-end? (iter-next! cursor))))))))

(mat iter-concat-stale-completion
  (andmap
   (lambda (exhaust-all?)
     (let ([saved #f] [phase 0] [second (list->iter '(second-1 second-2))])
       (let ([cursor
              (iter-concat
               (make-iter
                (lambda ()
                  (if saved iter-end
                      ;; Resume this producer after its occurrence has already ended.
                      (call/cc (lambda (continuation) (set! saved continuation) 'first))))
                void)
               second)])
         (guard (condition [else #f])
           (let ([value (iter-next! cursor)])
             (if (= phase 0)
                 (begin
                   (set! phase 1)
                   (unless (eq? 'first value) (error 'test "missing first value"))
                   (iter-next! cursor)
                   (when exhaust-all? (iter-next! cursor) (iter-next! cursor))
                   (saved iter-end))
                 (if exhaust-all?
                     (and (iter-end? value) (iter-end? (iter-next! cursor)))
                     (and (eq? 'second-2 value) (iter-end? (iter-next! cursor))))))))))
   '(#f #t)))

(mat iter-sparse-last-stride
  (andmap
   (lambda (step)
     (let ([cursor (list->iter '(0 1 2 3 4) 0 1 step)])
       (and (equal? '(0) (collect-iter cursor))
            (begin (iter-reset! cursor) #t)
            (equal? '(0) (collect-iter cursor)))))
   (list 1 5 1000000 (+ (most-positive-fixnum) 1)))

  (equal? '(1 3) (iter->list (list->iter '(0 1 2 3 4) 1 4 2))))

(mat iter-concat-stale-repeated-handle
  ;; Different occurrences can share a handle: identity of that handle is not the schedule position.
  (let ([saved #f] [phase 0] [remaining '(a b)])
    (let* ([source
            (make-iter
             (lambda ()
               (if (null? remaining) iter-end
                   (let ([value (car remaining)])
                     (set! remaining (cdr remaining))
                     (if saved value
                         ;; Save the first occurrence before concat switches and resets the handle.
                         (call/cc (lambda (continuation) (set! saved continuation) value))))))
             (lambda () (set! remaining '(a b))))]
           [cursor (iter-concat source source)])
      (let ([value (iter-next! cursor)])
        (if (= phase 0)
            (begin
              (set! phase 1)
              (iter-next! cursor)
              (iter-next! cursor)
              (saved iter-end))
            (and (eq? value 'b) (iter-end? (iter-next! cursor))))))))


(mat iter-range-inexact-progress
  ;; Finite ascending/descending ranges must reject an increment lost to rounding.
  (andmap
   (lambda (step)
     (let ([cursor (range 1e20 (+ 1e20 (* step 65536.0)) step)])
       (guard (condition
               [(and (who-condition? condition) (eq? 'range (condition-who condition))) #t]
               [else #f])
         (iter-next! cursor)
         #f)))
   '(1.0 -1.0))

  (equal? '(0 1/3 2/3) (iter->list (range 0 1 1/3)))

  (equal? '(0.0 0.5 1.0 1.5) (iter->list (range 0.0 2.0 0.5)))

  (let ([start (expt 10 30)])
    (equal? (list start (+ start 1) (+ start 2))
            (iter->list (range start (+ start 3))))))

(mat iter-empty-list-slices
  (andmap
   (lambda (bounds)
     (let ([cursor (apply list->iter '(0 1 2 3) bounds)])
       (and (iter-end? (iter-next! cursor))
            (begin (iter-reset! cursor) (iter-end? (iter-next! cursor))))))
   '((0 0) (3 0) (4 4) (1000000 0))))


(mat iter-range-ordered-parameters
  ;; Complex bounds and NaN parameters have no ordered range semantics.
  (error? (range 1+2i 4 1))

  ;; Even equal bounds must reject a NaN step rather than silently accepting it.
  (error? (range 0 0 +nan.0))

  ;; A NaN stop must fail at construction, rather than creating an unbounded source.
  (error? (range 0 +nan.0 1))

  (equal? '() (iter->list (range 1.0 1.0 -1.0)))

  (equal? '(1e308) (iter->list (range 1e308 +inf.0 1e308))))


(mat iter-distinct-releases-history
  ;; Generated payloads belong to history, not the configured source or callback.
  (let ([references '()])
    (let ([cursor
           (iter-distinct
            eq?
            (iter-map
             (lambda (value)
               (let ([payload (make-vector 256 value)])
                 (set! references (cons (weak-cons payload #f) references))
                 payload))
             (range 10)))])
      (let pass ([remaining 2])
        (iter-for-each (lambda (value) (void)) cursor)
        (collect (collect-maximum-generation))
        (collect (collect-maximum-generation))
        (and (andmap (lambda (reference) (bwp-object? (car reference))) references)
             ;; Keep the exhausted cursor reachable through collection; reset must still work.
             (iter-end? (iter-next! cursor))
             (or (= remaining 1)
                 (begin (iter-reset! cursor) (pass (- remaining 1)))))))))

(mat iter-distinct-completed-resumption
  (andmap
   (lambda (capture-in-source?)
     (let ([saved #f] [phase 0] [pulls 0])
       (let ([cursor
              (iter-distinct
               (lambda (left right)
                 (if (or capture-in-source? saved) (= left right)
                     ;; Revisit a comparison after its history has been released.
                     (call/cc (lambda (resume) (set! saved resume) #f))))
               (iter-map
                (lambda (value)
                  (set! pulls (+ pulls 1))
                  (if (or (not capture-in-source?) saved) value
                      ;; Revisit a producer return after the wrapper has exhausted.
                      (call/cc (lambda (resume) (set! saved resume) value))))
                (range 3)))])
         (unless capture-in-source? (iter-next! cursor))
         (let ([value (iter-next! cursor)])
           (if (= phase 0)
               (begin
                 (collect-iter cursor)
                 (set! phase 1)
                 (saved (if capture-in-source? 9 #f)))
               (and (iter-end? value) (iter-end? (iter-next! cursor)) (= pulls 3)))))))
   '(#f #t)))

(mat iter-zip-exhausted-continuation-resumption
  (andmap
   (lambda (capture-slot)
     (let ([saved #f] [phase 0] [pulls (vector 0 0 0)])
       (define source
         (lambda (slot)
           (make-iter
            (lambda ()
              (let ([count (vector-ref pulls slot)])
                (vector-set! pulls slot (+ count 1))
                (if (and (= slot capture-slot) (= count 0))
                    ;; Resume a first-tuple producer after a later pull exhausts the zip.
                    (call/cc (lambda (resume) (set! saved resume) slot))
                    (if (= count 1) iter-end slot))))
            (lambda () (vector-set! pulls slot 0)))))
       (let ([cursor (iter-zip (source 0) (source 1) (source 2))])
         (let ([value (iter-next! cursor)])
           (if (= phase 0)
               (begin
                 (unless (equal? value '(0 1 2)) (error 'test "missing initial tuple"))
                 (iter-next! cursor)
                 (set! phase 1)
                 (saved 9))
               (and (iter-end? value)
                    (equal? pulls '#(2 2 2))
                    (iter-end? (iter-next! cursor))
                    (equal? pulls '#(2 2 2))
                    (if (= phase 1)
                        (begin (set! phase 2) (saved 10))
                        (begin
                          (iter-reset! cursor)
                          (and (equal? '(0 1 2) (iter-next! cursor))
                               (iter-end? (iter-next! cursor)))))))))))
   '(0 1 2)))

(mat iter-zip-repeated-handle-order
  (let* ([source (range 7)] [cursor (iter-zip source source source)])
    (and (equal? '((0 1 2) (3 4 5)) (collect-iter cursor))
         (begin (iter-reset! cursor) #t)
         (equal? '((0 1 2) (3 4 5)) (iter->list cursor)))))

(mat iter-list-relative-bounds
  (equal? '(2 3) (iter->list (list->iter '(0 1 2 3) -2 4)))

  (equal? '(0 1 2) (iter->list (list->iter '(0 1 2 3) -1)))

  (equal? '(1 3) (iter->list (list->iter '(0 1 2 3 4) -4 -1 2)))

  (equal? '(0 2) (iter->list (list->iter '(0 1 2 3) -100 100 2)))

  (equal? '() (iter->list (list->iter '(0 1 2 3) -1 -3)))

  (equal? '(1 3) (iter->list (list->iter '(0 1 2 3 4) 1.0 4.0 2.0)))

  (let* ([source (list 0 1 2)] [cursor (list->iter source -2 -1)])
    (and (equal? '(1) (collect-iter cursor))
         (begin
           (set-cdr! (cddr source) (list 3 4))
           (iter-reset! cursor)
           (equal? '(3) (iter->list cursor))))))

(mat iter-list-reset-validates-current-source
  ;; A source made improper between passes must fail at reset, before unchecked traversal.
  (andmap
   (lambda (make-cursor)
     (let* ([source (list 1 2 3)] [cursor (make-cursor source)])
       (collect-iter cursor)
       (set-cdr! (cdr source) 42)
       (iter-type-error? (lambda () (iter-reset! cursor)))))
   (list list->iter
         (lambda (source) (list->iter source 3))
         (lambda (source) (list->iter source 3 4))
         (lambda (source) (list->iter source -2 -1))))

  ;; Cyclic sources must also fail at reset, including through a wrapper's reset graph.
  (andmap
   (lambda (make-cursor)
     (let* ([source (list 1 2 3)] [cursor (make-cursor source)])
       (collect-iter cursor)
       (set-cdr! (cddr source) source)
       (iter-type-error? (lambda () (iter-reset! cursor)))))
   (list list->iter
         (lambda (source) (list->iter source 1 3))
         (lambda (source) (list->iter source -2 -1))
         (lambda (source) (iter-concat (list->iter source)))
         (lambda (source) (sequence (tidentity) (list->iter source))))))

(mat iter-specialized-numeric-type-safety
  ;; Fixnum terminals must reject non-fixnums before using specialized arithmetic.
  (andmap
   (lambda (terminal)
     (andmap (lambda (value)
               (iter-type-error? (lambda () (terminal (list->iter (list value))))))
             (list #f 'bad "bad" (cons 1 2) 1.0 (+ 1 (most-positive-fixnum)))))
   (list iter-fxsum iter-fxproduct iter-fxavg))

  ;; Flonum terminals must reject other numeric representations as well as nonnumbers.
  (andmap
   (lambda (terminal)
     (andmap (lambda (value)
               (iter-type-error? (lambda () (terminal (list->iter (list value))))))
             (list #f 'bad "bad" (cons 1 2) 1 1/2 1+2i)))
   (list iter-flsum iter-flproduct iter-flavg)))

(define-record-type iter-invalid-adapter-source (fields value))
(iter-register-source! iter-invalid-adapter-source?
                       iter-invalid-adapter-source-value)

(mat iter-source-adapter-result-type-safety
  ;; A registered maker must produce an iterator before conversion publishes its result.
  (andmap
   (lambda (value)
     (iter-type-error?
      (lambda () (iter-source->iter (make-iter-invalid-adapter-source value)))))
   (list #f 42 'bad (vector 1) (list 1 2)))

  ;; A rejected deferred conversion must not become a sequence cleanup dependency.
  (let ([cursor (sequence (tidentity) (make-iter-invalid-adapter-source 42))])
    (and (iter-type-error? (lambda () (iter-next! cursor)))
         (begin (iter-finalize! cursor) #t)))

  (let ([cursor (list->iter '(1 2))])
    (eq? cursor (iter-source->iter (make-iter-invalid-adapter-source cursor))))

  ;; The diagnostic caller name must be a symbol even when the source is iterable.
  (iter-type-error? (lambda () (get-iter 42 '(1)))))
