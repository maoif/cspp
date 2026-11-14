(import (cspp)
        (cspp treemap))
(load "examples/common.ss")

(example-run!
 'treemap-example
 "Build generic and fixnum treemaps and print their ordered entries."
 '((values . "Comma-separated fixnums whose squares fit in a fixnum (default: 4,1,3,2)."))
 (lambda (ref)
   (let ([values (example-option-integer-list
                 'treemap-example ref 'values '(4 1 3 2))])
     ;; Construct a generic ordered map using numeric comparators.
     (let ([generic (make-treemap = <)])
       ;; Insert each value as a key with its square as the map value.
       (for-each (lambda (value) (treemap-set! generic value (* value value))) values)
       ;; Construct a map whose keys and values are fixnums.
       (let ([specialized (make-fxtreemap fx= fx<)])
         ;; Insert each fixnum key and value into the specialized map.
         (for-each (lambda (value)
                     (fxtreemap-set! specialized value (fx* value value)))
                   values)
         ;; Print ordered entries and one lookup from each map.
         (example-print
          (list (treemap->list generic)
                (and (pair? values) (treemap-ref generic (car values)))
                (fxtreemap->list specialized)
                (and (pair? values) (fxtreemap-ref specialized (car values)))))))
     0))
 (command-line-arguments))
