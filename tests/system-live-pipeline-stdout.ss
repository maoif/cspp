(import (cspp))

(define wait-for-pipeline
  (lambda (specs)
    (for-each process-wait (process-spawn-pipeline specs '()))))

(wait-for-pipeline '(("printf" "SINGLE\n")))
(wait-for-pipeline '(("printf" "CHAIN\n") ("cat")))
(process-run-pipeline '(("printf" "DISCARDED\n")))
