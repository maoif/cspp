(import (cspp)
        (cspp treeset))
(load "examples/common.ss")

(example-run!
 'treeset-example
 "Build generic and fixnum treesets and print their ordered contents."
 '((values . "Comma-separated fixnums (default: 4,1,3,2,2)."))
 (lambda (ref)
   (let ([values (example-option-integer-list
                 'treeset-example ref 'values '(4 1 3 2 2))])
     ;; Construct a generic ordered set using numeric comparators.
     (let ([generic (make-treeset = <)])
       ;; Insert each input value into the generic set.
       (for-each (lambda (value) (treeset-add! generic value)) values)
       ;; Construct a fixnum-only ordered set using fixnum comparators.
       (let ([specialized (make-fxtreeset fx= fx<)])
         ;; Insert each input fixnum into the specialized set.
         (for-each (lambda (value) (fxtreeset-add! specialized value)) values)
         ;; Print ordered contents and a membership result for each set.
         (example-print
          (list (treeset->list generic)
                (treeset-contains? generic 3)
                (fxtreeset->list specialized)
                (fxtreeset-contains? specialized 3)))))
     0))
 (command-line-arguments))
