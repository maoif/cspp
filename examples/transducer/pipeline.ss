(import (cspp)
        (cspp transducer))
(load "examples/common.ss")

(example-run!
 'transducer-example
 "Compose a map, filter, and take pipeline over a sequence."
 '((limit . "Maximum results from squared odd values of 0 through 19; nonnegative integer (default: 5)."))
 (lambda (ref)
   (let ([limit (example-option-natural 'transducer-example ref 'limit 5)])
     ;; Compose map, filter, and take transducers in pipeline order.
     (let ([pipeline (tcompose (tmap (lambda (x) (* x x)))
                               (tfilter odd?)
                               (ttake limit))])
       ;; Lazily expose transformed values as a sequence.
       ;; Collect the transformed values for readable output.
       (example-print (iter->list (sequence pipeline (iota 20)))))
     0))
 (command-line-arguments))
