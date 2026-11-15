(import (cspp))
(load "examples/common.ss")

(example-run!
 'iter-example
 "Map, filter, zip, and fold iterators with deterministic cleanup."
 '((limit . "Maximum zipped pairs to print, a nonnegative integer (default: 5)."))
 (lambda (ref)
   (let* ([limit (example-option-natural 'iter-example ref 'limit 5)]
          ;; Construct independent sources and map the first one.
          [mapped (iter-map (lambda (x) (* x 2)) (range 10))]
          ;; Filter the second source before zipping it with the first.
          [filtered (iter-filter even? (range 10))]
          ;; Pair values from the two iterators until either source finishes.
          [zipped (iter-zip mapped filtered)])
     ;; Pull and finalize the zipped iterator even when bounded traversal stops early.
     (dynamic-wind
       void
       (lambda ()
         (let loop ([left limit] [out '()])
           (if (zero? left)
               (example-print (reverse out))
               (let ([pair (iter-next! zipped)])
                 (if (iter-end? pair)
                     (example-print (reverse out))
                     (loop (- left 1) (cons pair out)))))))
       (lambda () (iter-finalize! zipped)))
     ;; Fold a separate iterator and let iter-fold finalize it.
     (example-print (iter-fold + 0 (range 6)))
     0))
 (command-line-arguments))
