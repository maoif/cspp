(import (cspp)
        (cspp benchmark))
(load "benchmarks/common.ss")

(define make-cases
 (lambda (size)
  (list
  ;; Measure vector fill with the same fixnum domain as its paired case.
  (make-benchmark
   'vector-fill
   (lambda (state n container) (benchmark-consume (let ([container (make-vector size 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index index)))))))
  ;; Measure vector read with the same fixnum domain as its paired case.
  (make-benchmark
   'vector-read
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (vector-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index index)))))))
  ;; Measure vector map with the same fixnum domain as its paired case.
  (make-benchmark
   'vector-map
   (lambda (state n container) (benchmark-consume (vmap (lambda (value) (fx+ value 1)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index index)))))))
  ;; Measure vector fold with the same fixnum domain as its paired case.
  (make-benchmark
   'vector-fold
   (lambda (state n container) (benchmark-consume (vfold-left fx+ 0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index index)))))))
  ;; Measure fxvector fill with the same fixnum domain as its paired case.
  (make-benchmark
   'fxvector-fill
   (lambda (state n container) (benchmark-consume (let ([container (make-fxvector size 0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxvector-set! container index index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxvector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxvector-set! container index index)))))))
  ;; Measure fxvector read with the same fixnum domain as its paired case.
  (make-benchmark
   'fxvector-read
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (fxvector-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxvector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxvector-set! container index index)))))))
  ;; Measure fxvector map with the same fixnum domain as its paired case.
  (make-benchmark
   'fxvector-map
   (lambda (state n container) (benchmark-consume (fxvmap (lambda (value) (fx+ value 1)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxvector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxvector-set! container index index)))))))
  ;; Measure fxvector fold with the same fixnum domain as its paired case.
  (make-benchmark
   'fxvector-fold
   (lambda (state n container) (benchmark-consume (fxvfold-left fx+ 0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-fxvector size 0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (fxvector-set! container index index)))))))
  ;; Measure vector-flonum fill with the same flonum domain as its paired case.
  (make-benchmark
   'vector-flonum-fill
   (lambda (state n container) (benchmark-consume (let ([container (make-vector size 0.0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index (exact->inexact index))))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index (exact->inexact index))))))))
  ;; Measure vector-flonum read with the same flonum domain as its paired case.
  (make-benchmark
   'vector-flonum-read
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (vector-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index (exact->inexact index))))))))
  ;; Measure vector-flonum map with the same flonum domain as its paired case.
  (make-benchmark
   'vector-flonum-map
   (lambda (state n container) (benchmark-consume (vmap (lambda (value) (fl+ value 1.0)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index (exact->inexact index))))))))
  ;; Measure vector-flonum fold with the same flonum domain as its paired case.
  (make-benchmark
   'vector-flonum-fold
   (lambda (state n container) (benchmark-consume (vfold-left fl+ 0.0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-vector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (vector-set! container index (exact->inexact index))))))))
  ;; Measure flvector fill with the same flonum domain as its paired case.
  (make-benchmark
   'flvector-fill
   (lambda (state n container) (benchmark-consume (let ([container (make-flvector size 0.0)])
           (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flvector-set! container index (exact->inexact index))))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flvector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flvector-set! container index (exact->inexact index))))))))
  ;; Measure flvector read with the same flonum domain as its paired case.
  (make-benchmark
   'flvector-read
   (lambda (state n container) (benchmark-consume (do ([index 0 (fx1+ index)]) ((fx= index n))
           (benchmark-consume (flvector-ref container index)))))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flvector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flvector-set! container index (exact->inexact index))))))))
  ;; Measure flvector map with the same flonum domain as its paired case.
  (make-benchmark
   'flvector-map
   (lambda (state n container) (benchmark-consume (flvmap (lambda (value) (fl+ value 1.0)) container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flvector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flvector-set! container index (exact->inexact index))))))))
  ;; Measure flvector fold with the same flonum domain as its paired case.
  (make-benchmark
   'flvector-fold
   (lambda (state n container) (benchmark-consume (flvfold-left fl+ 0.0 container)))
   `((args (size) (,size))
     (setup . ,(lambda (state)
                 (let ([container (make-flvector size 0.0)])
                   (do ([index 0 (fx1+ index)]) ((fx= index size) container) (flvector-set! container index (exact->inexact index)))))))))))

(benchmark-cli
 'vectors-benchmark
 "Compare generic, fixnum, and flonum representations with matching numeric domains."
 (lambda (ref)
   (benchmark-run-cli! 'vectors-benchmark "vectors" (make-cases (benchmark-size ref)) ref))
 (command-line-arguments))
