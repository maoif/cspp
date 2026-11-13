(import (cspp) (cspp array) (cspp benchmark))
(load "benchmarks/common.ss")

(define make-cases
 (lambda (size)
  (list
  ;; Measure array append with the same fixnum domain as its paired case.
  (make-benchmark
   'array-append
   (lambda (state n container) (benchmark-consume (let ([container (make-array 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container index)))))))
  ;; Measure array index with the same fixnum domain as its paired case.
  (make-benchmark
   'array-index
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (array-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container index)))))))
  ;; Measure array map with the same fixnum domain as its paired case.
  (make-benchmark
   'array-map
   (lambda (state n container) (benchmark-consume (array-map (lambda (value) (fx+ value 1)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container index)))))))
  ;; Measure array fold with the same fixnum domain as its paired case.
  (make-benchmark
   'array-fold
   (lambda (state n container) (benchmark-consume (array-fold-left fx+ 0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container index)))))))
  ;; Measure fxarray append with the same fixnum domain as its paired case.
  (make-benchmark
   'fxarray-append
   (lambda (state n container) (benchmark-consume (let ([container (make-fxarray 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxarray-add! container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxarray-add! container index)))))))
  ;; Measure fxarray index with the same fixnum domain as its paired case.
  (make-benchmark
   'fxarray-index
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (fxarray-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxarray-add! container index)))))))
  ;; Measure fxarray map with the same fixnum domain as its paired case.
  (make-benchmark
   'fxarray-map
   (lambda (state n container) (benchmark-consume (fxarray-map (lambda (value) (fx+ value 1)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxarray-add! container index)))))))
  ;; Measure fxarray fold with the same fixnum domain as its paired case.
  (make-benchmark
   'fxarray-fold
   (lambda (state n container) (benchmark-consume (fxarray-fold-left fx+ 0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxarray-add! container index)))))))
  ;; Measure array-flonum append with the same flonum domain as its paired case.
  (make-benchmark
   'array-flonum-append
   (lambda (state n container) (benchmark-consume (let ([container (make-array 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container (exact->inexact index))))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container (exact->inexact index))))))))
  ;; Measure array-flonum index with the same flonum domain as its paired case.
  (make-benchmark
   'array-flonum-index
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (array-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container (exact->inexact index))))))))
  ;; Measure array-flonum map with the same flonum domain as its paired case.
  (make-benchmark
   'array-flonum-map
   (lambda (state n container) (benchmark-consume (array-map (lambda (value) (fl+ value 1.0)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container (exact->inexact index))))))))
  ;; Measure array-flonum fold with the same flonum domain as its paired case.
  (make-benchmark
   'array-flonum-fold
   (lambda (state n container) (benchmark-consume (array-fold-left fl+ 0.0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-array 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (array-add! container (exact->inexact index))))))))
  ;; Measure flarray append with the same flonum domain as its paired case.
  (make-benchmark
   'flarray-append
   (lambda (state n container) (benchmark-consume (let ([container (make-flarray 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flarray-add! container (exact->inexact index))))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flarray-add! container (exact->inexact index))))))))
  ;; Measure flarray index with the same flonum domain as its paired case.
  (make-benchmark
   'flarray-index
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (flarray-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flarray-add! container (exact->inexact index))))))))
  ;; Measure flarray map with the same flonum domain as its paired case.
  (make-benchmark
   'flarray-map
   (lambda (state n container) (benchmark-consume (flarray-map (lambda (value) (fl+ value 1.0)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flarray-add! container (exact->inexact index))))))))
  ;; Measure flarray fold with the same flonum domain as its paired case.
  (make-benchmark
   'flarray-fold
   (lambda (state n container) (benchmark-consume (flarray-fold-left fl+ 0.0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flarray 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flarray-add! container (exact->inexact index)))))))))))

(benchmark-cli
 'arrays-benchmark
 "Compare generic, fixnum, and flonum representations with matching numeric domains."
 (lambda (ref)
   (benchmark-run-cli! 'arrays-benchmark "arrays" (make-cases (benchmark-size ref)) ref))
 (command-line-arguments))
