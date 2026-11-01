(import (cspp))
(load "benchmarks/common.ss")

;; Run this same script against the baseline and optimized libraries.
;; Compile the procedures before the measurement; use retained graph size as the primary metric.
;; Subtract an empty set's graph so shared comparator code is not charged per entry.
(define measure-set
  (lambda (count)
    (let* ([empty (make-treeset fx= fx<)]
           [set (make-treeset fx= fx<)]
           [empty-size (compute-size empty)])
      (collect)
      (let ([before (statistics)])
        (do ([item 0 (fx1+ item)]) ((fx= item count))
          (treeset-add! set item))
        (let ([allocated (sstats-bytes (sstats-difference (statistics) before))])
          (collect)
          (let ([retained (- (compute-size set) empty-size)])
            (unless (= count (treeset-size set)) (error 'memory "incorrect set size"))
            (list (number->string count)
                  (number->string retained)
                  (number->string (/ retained count))
                  (number->string allocated))))))))

(printf "~a ~a\n" (scheme-version) (machine-type))
(benchmark-print-table!
 "Tree Set Memory"
 '(("entries" . right) ("retained bytes" . right)
   ("bytes/item" . right) ("allocated bytes" . right))
 (map measure-set '(1000 10000 100000)))
