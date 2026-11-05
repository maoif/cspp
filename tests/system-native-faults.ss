(import (cspp))

;; This driver runs under strace syscall injection, separately from the test runner.
(case (string->symbol (cadr (command-line)))
  [(host-query)
   ;; Error case: uname failures must raise conditions instead of returning legacy error vectors.
   (for-each
     (lambda (entry)
       (unless
         (guard (condition
                  [(system-permission-error? condition)
                   (and (= 1 (system-error-code condition))
                        (eq? (car entry) (system-error-operation condition)))])
           ((cdr entry)) #f)
         (error 'system-native-faults "host query did not raise the native failure" (car entry))))
     (list (cons 'hostname hostname) (cons 'hostname system-hostname) (cons 'cpu-arch cpu-arch)))]

  [(pipeline-run pipeline-capture pipeline-spawn)
   ;; Error case: EINTR while reaping earlier children must not replace launch-time ENOENT.
   (unless
     (guard (condition [(system-error? condition) (= 2 (system-error-code condition))])
       (let ([specs '(("/bin/sleep" "0.2") (""))])
         (case (string->symbol (cadr (command-line)))
           [(pipeline-run) (process-run-pipeline specs)]
           [(pipeline-capture) (process-capture-pipeline specs)]
           [(pipeline-spawn) (for-each process-wait (process-spawn-pipeline specs '()))]))
       #f)
     (error 'system-native-faults "pipeline lost the original launch error"))]

  [else (error 'system-native-faults "unknown fault mode")])
