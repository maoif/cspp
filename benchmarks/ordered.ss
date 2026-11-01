(import (cspp))
(load "benchmarks/common.ss")

;; Deterministic permutation, generated outside measured regions.
(define keys
  (lambda (count)
    (let ([items (list->vector (iota count))] [seed 12345])
      (do ([i (fx1- count) (fx1- i)]) ((fx<= i 0) items)
        (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
        (let* ([j (modulo seed (fx1+ i))] [value (vector-ref items i)])
          (vector-set! items i (vector-ref items j))
          (vector-set! items j value))))))

(define elapsed-ns
  (lambda (start stop)
    (+ (* (- (time-second stop) (time-second start)) 1000000000)
       (- (time-nanosecond stop) (time-nanosecond start)))))

(define trial
  (lambda (maker setter ref delete! size items operation)
    (let ([map (maker fx= fx<)] [checksum 0])
      (unless (memq operation '(insert mixed))
        (vector-for-each (lambda (key) (setter map key key)) items))
      (collect)
      (let ([start (current-time 'time-monotonic)])
        (case operation
          [(insert) (vector-for-each (lambda (key) (setter map key key)) items)]
          [(lookup) (vector-for-each
                      (lambda (key) (set! checksum (+ checksum (ref map key)))) items)]
          [(delete) (vector-for-each (lambda (key) (delete! map key)) items)]
          [(mixed)
           (vector-for-each (lambda (key) (setter map key key)) items)
           (vector-for-each
             (lambda (key) (set! checksum (+ checksum (ref map key)))) items)
           (vector-for-each (lambda (key) (delete! map key)) items)])
        (let ([elapsed (elapsed-ns start (current-time 'time-monotonic))]
              [count (vector-length items)])
          (when (memq operation '(lookup mixed))
            (unless (= checksum (/ (* count (- count 1)) 2))
              (error 'benchmark "incorrect lookup results")))
          (unless (= (size map) (if (memq operation '(delete mixed)) 0 count))
            (error 'benchmark "incorrect final map size"))
          elapsed)))))

(define results '())

(define compare
  (lambda (count operation)
    (let ([items (keys count)] [generic '()] [specialized '()])
      ;; Warm both paths; alternate ordering of seven retained trials.
      (trial make-treemap treemap-set! treemap-ref treemap-delete! treemap-size items operation)
      (trial make-fxtreemap fxtreemap-set! fxtreemap-ref fxtreemap-delete! fxtreemap-size items operation)
      (do ([i 0 (fx1+ i)]) ((fx= i 7))
        (if (even? i)
            (begin
              (set! generic (cons (trial make-treemap treemap-set! treemap-ref treemap-delete! treemap-size items operation) generic))
              (set! specialized (cons (trial make-fxtreemap fxtreemap-set! fxtreemap-ref fxtreemap-delete! fxtreemap-size items operation) specialized)))
            (begin
              (set! specialized (cons (trial make-fxtreemap fxtreemap-set! fxtreemap-ref fxtreemap-delete! fxtreemap-size items operation) specialized))
              (set! generic (cons (trial make-treemap treemap-set! treemap-ref treemap-delete! treemap-size items operation) generic)))))
      (set! results
            (cons (list (number->string count)
                        (symbol->string operation)
                        (number->string (list-ref (list-sort < generic) 3))
                        (number->string (list-ref (list-sort < specialized) 3))
                        "median of 7")
                  results)))))

(printf "~a ~a; compiled library settings from Makefile\n" (scheme-version) (machine-type))
(for-each (lambda (count)
            (for-each (lambda (operation) (compare count operation)) '(insert lookup delete mixed)))
          '(1000 10000 100000))
(benchmark-print-table!
 "Ordered Map Benchmarks"
 '(("entries" . right) ("operation" . left) ("generic ns" . right)
   ("fixnum ns" . right) ("samples" . left))
 (reverse results))
