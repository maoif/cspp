(import (cspp) (cspp heap))
(load "examples/common.ss")

(example-run!
 'heap-example
 "Push values into a priority heap, inspect its top, then pop all values."
 '((values . "Comma-separated integers (default: 4,1,3,2)."))
 (lambda (ref)
   (let ([values (example-option-integer-list 'heap-example ref 'values '(4 1 3 2))])
     ;; Construct a min-heap by ordering values in ascending order.
     (let ([queue (make-heap <)])
       ;; Push all values into the priority heap.
       (for-each (lambda (value) (heap-push! queue value)) values)
       ;; Read the smallest item without removing it.
       (let ([top (heap-peek queue)])
         ;; Remove all items in priority order.
         (example-print (list top (heap-pop-all! queue))))))
   0)
 (command-line-arguments))
