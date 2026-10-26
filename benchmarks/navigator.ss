;; Run from the repository root: ./cspp --script=benchmarks/navigator.ss
;; Five samples after a warmup; each sample starts after a collection.
;; CPU time includes collections. Allocation includes bytes reclaimed by collections.
(import (cspp))
(define measure
  (lambda (label rounds thunk)
    (thunk)
    (let ([samples
           (map (lambda (sample)
                  (collect)
                  (let ([start (current-time 'time-process)] [allocated (+ (bytes-allocated) (bytes-deallocated))])
                    (do ([i 0 (fx+ i 1)]) ((fx= i rounds)) (thunk))
                    (let ([elapsed (time-difference (current-time 'time-process) start)])
                      (cons (+ (* (time-second elapsed) 1000.0)
                               (/ (time-nanosecond elapsed) 1000000.0))
                            (- (+ (bytes-allocated) (bytes-deallocated)) allocated)))))
                '(1 2 3 4 5))])
      (printf "~a (~a rounds): median CPU ~,3f ms; median allocated ~a bytes\n"
              label rounds (list-ref (sort < (map car samples)) 2)
              (list-ref (sort < (map cdr samples)) 2)))))
(define table (make-eqv-hashtable))
(do ([index 0 (fx+ index 1)]) ((fx= index 1000))
  (hashtable-set! table index index))
(measure 'keys-1000 5 (lambda () (nav-transform nav/keys id table)))
(measure 'entries-1000 5 (lambda () (nav-transform nav/entries id table)))
(define vec (make-vector 3000 1))
(measure 'vector-all-3000 10 (lambda () (nav-transform nav/all add1 vec)))
(define items (iota 1000))
(measure 'list-all-1000 5 (lambda () (nav-transform nav/all add1 items)))
(define selection-items (iota 3000))
(measure 'list-select-all-3000 5 (lambda () (nav-select nav/all selection-items)))
(measure 'list-select-entries-3000 5 (lambda () (nav-select nav/entries selection-items)))
(measure 'slice-select-short-1000000 5
         (lambda () (nav-select (nav/slice 0 1000000) '#(1 2 3))))
(define submap-keys (iota 500))
(measure 'submap-update-500-of-1000 5
         (lambda () (nav-transform (apply nav/submap submap-keys) values table)))
(define bulk-table (make-eqv-hashtable))
(do ([index 0 (fx+ index 1)]) ((fx= index 2000))
  (hashtable-set! bulk-table index index))
(measure 'table-all-2000 1 (lambda () (nav-transform nav/all add1 bulk-table)))
(define bulk-vector (make-vector 2000 1))
(measure 'vector-children-2000 1 (lambda () (nav-transform nav/children add1 bulk-vector)))
(define mutable-items (iota 2000))
(measure 'list-all-mutate-2000 1 (lambda () (nav-transform! nav/all add1 mutable-items)))
(measure 'list-slice-mutate-2000 1
         (lambda () (nav-transform! (nav/slice 0 2000 2) add1 mutable-items)))

;; Complete membership replacement avoids repeated copies during bulk set clearing.
(for-each
 (lambda (size)
   (let ([hashed (apply hashset (iota size))]
         [ordered (apply treeset = < (iota size))]
         [mapping (apply treemap = < (map cons (iota size) (iota size)))])
     (measure (list 'hashset-clear size) 1 (lambda () (nav-clearval nav/all hashed)))
     (measure (list 'treeset-clear size) 1 (lambda () (nav-clearval nav/all ordered)))
     (measure (list 'treemap-all size) 1 (lambda () (nav-transform nav/all add1 mapping)))))
 '(500 1000 2000))
