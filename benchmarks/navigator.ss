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
