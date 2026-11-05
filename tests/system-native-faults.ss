(import (cspp))

;; This driver runs under strace syscall injection, separately from the test runner.
(define mode (string->symbol (cadr (command-line))))
(case mode
  [(filesystem-gc pipeline-run-gc pipeline-capture-gc pipeline-spawn-gc)
   ;; Error case: native queries and failed-launch cleanup must allow other threads to collect.
   (let ([mutex (make-mutex)] [changed (make-condition)]
         [started? #f] [finished? #f] [query-failure #f])
     (fork-thread
       (lambda ()
         (with-mutex mutex
           (set! started? #t)
           (condition-signal changed))
         (let ([failure
                 (guard (condition [else condition])
                   (if (eq? mode 'filesystem-gc)
                       (filesystem-info ".")
                       (unless
                         (guard (condition
                                  [(system-error? condition)
                                   (= 2 (system-error-code condition))])
                           (let ([specs '(("/bin/sleep" "0.2") (""))])
                             (case mode
                               [(pipeline-run-gc) (process-run-pipeline specs)]
                               [(pipeline-capture-gc) (process-capture-pipeline specs)]
                               [(pipeline-spawn-gc) (process-spawn-pipeline specs '())]))
                           #f)
                         (error 'system-native-faults "pipeline lost the launch error")))
                   #f)])
           (with-mutex mutex
             (set! query-failure failure)
             (set! finished? #t)
             (condition-signal changed)))))
     (with-mutex mutex
       (let loop () (unless started? (condition-wait changed mutex) (loop))))
     ;; strace delays the worker's first statfs or cleanup wait4 for two seconds.
     (sleep-milliseconds 100)
     (let* ([failure (guard (condition [else condition]) (collect) #f)]
            [collected-during-query? (with-mutex mutex (not finished?))])
       (with-mutex mutex
         (let loop () (unless finished? (condition-wait changed mutex) (loop))))
       (when query-failure (raise query-failure))
       (when failure (raise failure))
       (unless collected-during-query?
         (error 'system-native-faults "collection waited for native I/O" mode))))]

  [(procfs-without-mountinfo)
   ;; Error case: unavailable mountinfo must not prevent reading any procfs metric file.
   (unless (and (number? (linux-system-uptime)) (pair? (linux-memory-info))
                (for-all number? (linux-load-average)))
     (error 'system-native-faults "procfs query returned an invalid metric"))]

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
