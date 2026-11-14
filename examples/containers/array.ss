(import (cspp)
        (cspp array))
(load "examples/common.ss")

(example-run!
 'array-example
 "Fill generic, fixnum, and flonum arrays and print their contents."
 '((size . "Number of elements, a nonnegative fixnum (default: 5)."))
 (lambda (ref)
   (let ([size (example-option-natural 'array-example ref 'size 5)])
     (unless (fixnum? size)
       (errorf 'array-example "--size must fit in a fixnum"))
     ;; Construct an empty generic array.
     (let ([generic (make-array)])
       ;; Append a sequence of integers to the generic array.
       (do ([index 0 (+ index 1)]) ((= index size))
         (array-add! generic index))
       ;; Construct an empty fixnum array.
       (let ([fixnums (make-fxarray 0)])
         ;; Append fixnums to the specialized array.
         (do ([index 0 (+ index 1)]) ((= index size))
           (fxarray-add! fixnums index))
         ;; Construct an empty flonum array.
         (let ([flonums (make-flarray)])
           ;; Append finite flonum values to the specialized array.
           (do ([index 0 (+ index 1)]) ((= index size))
             (flarray-add! flonums (exact->inexact index)))
           ;; Map each representation while preserving its numeric element type.
           (example-print (array-map (lambda (value) (* value 2)) generic))
           (example-print (fxarray-map (lambda (value) (fx* value 2)) fixnums))
           (example-print (flarray-map (lambda (value) (fl* value 2.0)) flonums))
           ;; Fold comparable contents with the appropriate numeric arithmetic.
           (example-print (array-fold-left + 0 generic))
           (example-print (fxarray-fold-left fx+ 0 fixnums))
           (example-print (flarray-fold-left fl+ 0.0 flonums))
           ;; Convert to corresponding vectors, then reconstruct the arrays.
           (example-print (vector->array (array->vector generic)))
           (example-print (fxvector->fxarray (fxarray->fxvector fixnums)))
           (example-print (flvector->flarray (flarray->flvector flonums)))
           ;; Convert each representation to a list for comparison.
           (example-print
            (list (array->list generic)
                  (fxarray->list fixnums)
                  (flarray->list flonums))))))
     0))
 (command-line-arguments))
