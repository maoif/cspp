(import (cspp))

;; This driver runs under strace syscall injection, separately from the test runner.
(define mode (string->symbol (cadr (command-line))))
(case mode
  [(command-wait)
   ;; Error case: exit readiness must cover unpiped commands and drained pipeline output.
   (unless
     (and (process-exit-success? (process-run /bin/sleep "0.1"))
          (for-all process-exit-success?
            (process-run-pipeline '(("/bin/sleep" "0.1") ("/bin/true"))))
          (for-all process-exit-success?
            (process-run-pipeline '(("/bin/true") ("/bin/sleep" "0.1"))))
          (process-exit-success?
            (process-result-status
              (process-capture-shell "exec 1>&- 2>&-; exec /bin/sleep 0.1"))))
     (error 'system-native-faults "command wait returned an incorrect status"))]

  [(spawn-interrupt)
   ;; Error case: a delayed exec handshake must service handlers before child setup completes.
   (let* ([thread ((foreign-procedure "pthread_self" () uptr))]
          [send (foreign-procedure "pthread_kill" (uptr int) int)]
          [start (current-time 'time-monotonic)] [handled #f] [child #f]
          [fds (length (directory-list "/proc/self/fd"))])
     (parameterize
       ([keyboard-interrupt-handler
          (lambda ()
            (set! handled (time-difference (current-time 'time-monotonic) start)))])
       (fork-thread (lambda () (milisleep 50) (send thread 2)))
       (set! child (process-spawn "/bin/true" '() '())))
     (process-wait child)
     (unless (and handled (= 0 (time-second handled))
                  (< (time-nanosecond handled) 250000000)
                  (= fds (length (directory-list "/proc/self/fd"))))
       (error 'system-native-faults "spawn deferred its handler or leaked descriptors")))]

  [(preexec-signal)
   ;; Error case: a signal during delayed child setup must use exec's default disposition.
   (let* ([children-path (format "/proc/self/task/~a/children" (getpid))]
          [sent-to #f])
     (fork-thread
       (lambda ()
         (milisleep 150)
         (let ([children (call-with-input-file children-path get-line)])
           (let ([pid (string->number (car (string-split children #\space)))])
             (set! sent-to pid)
             ((foreign-procedure "kill" (int int) int) pid 2)))))
     (let* ([child (process-spawn "/bin/true" '() '())] [status (process-wait child)])
       (unless (and (eqv? sent-to (process-pid child))
                    (eq? 'signal (process-exit-status-kind status))
                    (= 2 (process-exit-status-code status)))
         (error 'system-native-faults "child swallowed a pre-exec interrupt"))))]

  [(live-wait)
   (unless (process-exit-success? (process-wait (process-spawn "/bin/sleep" '("1") '())))
     (error 'system-native-faults "live wait lost the exit status"))]

  [(spawn-without-descriptor-enumeration)
   ;; Error case: if reliable closure is unavailable, never inherit descriptors above a lowered limit.
   (let* ([port (open-file-input-port "/dev/null")]
          [duplicate ((foreign-procedure "fcntl" (int int int) int)
                       (port-file-descriptor port) 0 128)]
          [limits (make-bytevector 16 0)])
     (dynamic-wind
       void
       (lambda ()
         (unless (>= duplicate 128) (error 'system-native-faults "cannot duplicate descriptor"))
         ;; This isolated driver alone lowers its hard limit below an already-open descriptor.
         (bytevector-u64-native-set! limits 0 64)
         (bytevector-u64-native-set! limits 8 64)
         (unless (= 0 ((foreign-procedure "setrlimit" (int u8*) int) 7 limits))
           (error 'system-native-faults "cannot lower descriptor limit"))
         (for-each
           (lambda (thunk)
             (unless (guard (failure [(system-error? failure) (= 95 (system-error-code failure))])
                       (thunk) #f)
               (error 'system-native-faults "launch did not reject unreliable descriptor closure")))
           (list
             (lambda () (process-wait (process-spawn "/bin/true" '() '())))
             (lambda () (process-capture /bin/true :stdin inherit))
             (lambda () (process-capture-pipeline '(("/bin/true"))))
             (lambda () (for-each process-wait
                          (process-spawn-pipeline '(("/bin/true")) '()))))))
       (lambda ()
         (when (>= duplicate 0) ((foreign-procedure "close" (int) int) duplicate))
         (close-port port))))]

  [(pipeline-io-failure)
   ;; Error case: capture I/O failure must terminate upstream stages before reporting the error.
   (let ([start (current-time 'time-monotonic)])
     (unless
       (guard (condition [(system-error? condition) (= 5 (system-error-code condition))])
         (process-capture-pipeline '(("/bin/sleep" "3") ("/bin/echo" "out")))
         #f)
       (error 'system-native-faults "pipeline lost the capture error"))
     (let ([elapsed (time-difference (current-time 'time-monotonic) start)])
       (unless (< (+ (* 1000 (time-second elapsed))
                     (quotient (time-nanosecond elapsed) 1000000)) 1500)
         (error 'system-native-faults "pipeline waited for an upstream stage after I/O failure"))))]

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

  [(filesystem-snapshot)
   ;; Error case: replacing a symlink during a query must not mix capacity and device identity.
   (let* ([path (format "/tmp/cspp-filesystem-snapshot-~a" (getpid))]
          [replacement (string-append path "-next")]
          [mutex (make-mutex)] [changed (make-condition)]
          [started? #f] [finished? #f] [observed #f] [failure #f])
     (dynamic-wind
       (lambda () (file-symlink "/" path) (file-symlink "/dev/shm" replacement))
       (lambda ()
         (fork-thread
           (lambda ()
             (with-mutex mutex (set! started? #t) (condition-signal changed))
             (guard (condition [else (set! failure condition)])
               (set! observed (filesystem-info path)))
             (with-mutex mutex (set! finished? #t) (condition-signal changed))))
         (with-mutex mutex
           (let loop () (unless started? (condition-wait changed mutex) (loop))))
         ;; strace holds the first capacity-query return while this path is replaced.
         (sleep-milliseconds 100)
         (rename-file replacement path)
         (with-mutex mutex
           (let loop () (unless finished? (condition-wait changed mutex) (loop))))
         (when failure (raise failure))
         (unless
           (exists
             (lambda (target)
               (let ([expected (filesystem-info target)])
                 (and (= (filesystem-info-device expected) (filesystem-info-device observed))
                      (= (filesystem-info-blocks expected) (filesystem-info-blocks observed)))))
             '("/" "/dev/shm"))
           (error 'system-native-faults "filesystem record combines different targets")))
       (lambda ()
         (delete-file path)
         (when (file-symbolic-link? replacement) (delete-file replacement)))))]

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
