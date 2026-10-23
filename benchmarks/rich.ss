#!chezscheme
(import (cspp))

;; Run from the repository root: ./cspp --script=benchmarks/rich.ss
;; Each sample prepares its inputs before collecting and measuring. Report the median
;; of five samples; allocation is cumulative allocation, not retained or peak memory.
(define milliseconds
  (lambda (duration)
    (+ (* 1000.0 (time-second duration)) (/ (time-nanosecond duration) 1000000.0))))

(define measure
  (lambda (name size prepare)
    (let ([samples
           (map (lambda (iteration)
                  (let ([run (prepare)])
                    (collect)
                    (let* ([before (statistics)] [result (run)]
                           [delta (sstats-difference (statistics) before)])
                      (unless result (error 'rich-benchmark "invalid result"))
                      (cons (milliseconds (sstats-real delta)) (sstats-bytes delta)))))
                (iota 5))])
      (printf "~a n=~a median-ms=~,3f allocated=~a\n" name size
              (list-ref (sort < (map car samples)) 2)
              (list-ref (sort < (map cdr samples)) 2)))))

(printf "~a ~a; five samples, release build\n" (scheme-version) (machine-type))
(for-each
 (lambda (size)
   (measure 'wrap size
            (lambda ()
              (let ([segments (list (rich-segment (make-string size #\x)))])
                (lambda () (= (quotient (+ size 79) 80)
                              (length (rich-segment-wrap segments 80))))))))
 '(20000 40000 80000 160000))

(for-each
 (lambda (size)
   (measure 'add-rows size
            (lambda ()
              (let ([table (rich-table :columns ("A"))])
                (lambda ()
                  (do ([index 0 (+ index 1)]) ((= index size) #t)
                    (rich-table-add-row! table "x")))))))
 '(1000 2000 4000 8000))

(for-each
 (lambda (size)
   (let ([body (apply string-append (make-list size "x\n"))])
     (for-each
      (lambda (kind)
        (measure kind size
                 (lambda ()
                   (let ([value (case kind
                                  [(tall-table)
                                   (rich-table :show-header? #f :columns ("A")
                                               :rows (list (list body)))]
                                  [(tall-columns) (make-rich-columns (list body "y"))]
                                  [(tall-layout) (make-rich-layout 'row (list body "y"))])]
                         [console (rich-console :width 80)])
                     (lambda () (= (+ size (if (eq? kind 'tall-table) 3 1))
                                   (length (rich-render console value))))))))
      '(tall-table tall-columns tall-layout))))
 '(1000 2000 4000 8000))
