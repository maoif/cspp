(import (cspp))

(define check
  (lambda (value)
    (unless value (error 'system-runtime "regression failed" (cdr (command-line))))))

(define rejects?
  (lambda (thunk)
    (guard (condition [else (or (error? condition) (violation? condition))])
      (thunk)
      #f)))

(define run-threads
  (lambda (thunks)
    (let ([mutex (make-mutex)] [ready (make-condition)]
          [remaining (length thunks)] [failures '()])
      (for-each
        (lambda (thunk)
          (fork-thread
            (lambda ()
              (let ([failure (guard (condition [else condition]) (thunk) #f)])
                (with-mutex mutex
                  (when failure (set! failures (cons failure failures)))
                  (set! remaining (- remaining 1))
                  (condition-signal ready))))))
        thunks)
      (with-mutex mutex
        (let loop ()
          (unless (zero? remaining) (condition-wait ready mutex) (loop))))
      (unless (null? failures) (raise (car failures))))))

(case (string->symbol (cadr (command-line)))
  [(wait-interrupt)
   ;; Error case: native exit notification must promptly return to Scheme for keyboard interrupts.
   (for-each
     (lambda (wait)
       (let* ([child (process-spawn "/bin/sleep" '("0.8") '())]
              [main-thread ((foreign-procedure "pthread_self" () uptr))]
              [send (foreign-procedure "pthread_kill" (uptr int) int)]
              [start (current-time 'time-monotonic)] [handled #f])
         (dynamic-wind
           void
           (lambda ()
             (parameterize ([keyboard-interrupt-handler
                              (lambda ()
                                (set! handled (time-difference
                                                (current-time 'time-monotonic) start)))])
               (fork-thread (lambda () (milisleep 50) (send main-thread 2)))
               (check (process-exit-success? (wait child)))
               (check (and handled (= 0 (time-second handled))
                           (< (time-nanosecond handled) 500000000)))))
           (lambda () (process-kill child 9) (process-wait child)))))
     (list process-wait (lambda (child) (process-wait/timeout child 2000))))
   ;; Error case: servicing an interrupted wait must preserve a caller's disabled interrupts.
   (let* ([child (process-spawn "/bin/sleep" '("0.2") '())]
          [main-thread ((foreign-procedure "pthread_self" () uptr))]
          [send (foreign-procedure "pthread_kill" (uptr int) int)] [handled? #f])
     (dynamic-wind
       void
       (lambda ()
         (parameterize ([keyboard-interrupt-handler (lambda () (set! handled? #t))])
           (with-interrupts-disabled
             (fork-thread (lambda () (milisleep 50) (send main-thread 2)))
             (check (process-exit-success? (process-wait child)))
             (check (not handled?)))
           (check handled?)))
       (lambda () (process-kill child 9) (process-wait child))))]

  [(native-allocation-errors)
   ;; Error case: failed native argument copies must preserve ENOMEM, not report invalid arguments.
   (let* ([argument (make-string 30000000 #\a)]
          [original (make-bytevector 16 0)] [limited (make-bytevector 16 0)]
          [get-limit (foreign-procedure "getrlimit" (int u8*) int)]
          [set-limit (foreign-procedure "setrlimit" (int u8*) int)]
          [page-size ((foreign-procedure "getpagesize" () int))]
          [pages (call-with-input-file "/proc/self/statm"
                   (lambda (port)
                     (string->number (car (string-split (get-line port) #\space)))))]
          [codes #f])
     (check (= 0 (get-limit 9 original)))
     (bytevector-copy! original 0 limited 0 16)
     (bytevector-u64-native-set! limited 0 (+ (* pages page-size) 4194304))
     (dynamic-wind
       (lambda () (check (= 0 (set-limit 9 limited))))
       (lambda ()
         (set! codes
           (map
             (lambda (thunk)
               (guard (failure [(system-error? failure) (system-error-code failure)])
                 (thunk)
                 #f))
             (list (lambda () (process-wait (process-spawn "/bin/true" (list argument) '())))
                   (lambda () (process-capture /bin/true (begin argument)))
                   (lambda () (process-run-pipeline (list (list "/bin/true" argument))))))))
       (lambda () (check (= 0 (set-limit 9 original)))))
     (check (equal? '(12 12 12) codes)))]

  [(localized-errors)
   ;; Error case: UTF-8 native messages must be decoded rather than widened byte by byte.
   ;; Locale mutation is isolated from other test processes and restored before returning.
   (let* ([locale (foreign-procedure "setlocale" (int utf-8) utf-8)]
          [native-message (foreign-procedure "strerror" (int) utf-8)]
          [original (locale 6 #f)])
     (dynamic-wind
       void
       (lambda ()
         (when (exists (lambda (name) (locale 6 name))
                       '("zh_CN.UTF-8" "fr_FR.UTF-8" "de_DE.UTF-8"))
           (check
             (guard (failure
                      [(system-error? failure)
                       (and (string=? (native-message 2) (system-error-message failure))
                            (string=? (native-message 2) (condition-message failure)))])
               (filesystem-info "/definitely-missing-cspp-system-runtime")
               #f))))
       (lambda () (locale 6 original))))]

  [(pipeline-low-descriptor-limit)
   ;; Error case: pipeline length must not require every connection open in the parent at once.
   ;; Only this isolated process lowers its descriptor limit.
   (let ([limits (make-bytevector 16 0)] [specs (make-list 40 '("/bin/true"))])
     (bytevector-u64-native-set! limits 0 64)
     (bytevector-u64-native-set! limits 8 64)
     (check (= 0 ((foreign-procedure "setrlimit" (int u8*) int) 7 limits)))
     (check (for-all process-exit-success? (process-run-pipeline specs)))
     (check (process-exit-success? (process-result-status (process-capture-pipeline specs))))
     (check (for-all process-exit-success? (map process-wait
                                              (process-spawn-pipeline specs '())))))]

  [(loader-list-race)
   ;; Error case: loader callbacks must not acquire Chez locks while holding the loader lock.
   (let* ([root (if (file-exists? "libcspp.so") "." "..")]
          [source (path-build root "libcspp.so")]
          [paths (map (lambda (index)
                        (format "/tmp/cspp-loader-~a-~a.so" (getpid) index))
                      (iota 100))])
     (dynamic-wind
       void
       (lambda ()
         (parameterize ([collect-trip-bytes 1048576])
           (run-threads
             (list
               (lambda ()
                 (do ([index 0 (+ index 1)]) ((= index 30000))
                   (check (for-all string? (shared-object-list)))))
               (lambda ()
                 (for-each
                   (lambda (path)
                     (file-copy source path)
                     (load-shared-object path)
                     (delete-file path))
                   paths))))))
       (lambda ()
         (for-each (lambda (path) (when (file-exists? path) (delete-file path))) paths))))]

  [(vfork-rejected)
   ;; Error case: returning from native vfork corrupts the parent's shared C/FFI stack.
   (check
     (guard (condition
              [(system-unsupported-error? condition)
               (eq? 'vfork (system-error-operation condition))])
       (let ([pid (vfork)])
         ;; Bound the unfixed regression in the isolated child rather than resume Scheme there.
         (when (zero? pid) ((foreign-procedure "_exit" (int) void) 1)))
       #f))]

  [(pipeline-contracts)
   ;; Error case: circular nested argv lists must be rejected before native traversal.
   (let ([spec (list "/bin/true")])
     (set-cdr! spec spec)
     (check (rejects? (lambda () (process-capture-pipeline (list spec))))) )]

  [(user-race)
   (let* ([first (current-user)] [second (user-ref/default "daemon" first)])
     ;; Error case: lookups in other threads must not overwrite any record fields.
     (run-threads
       (map (lambda (expected)
              (lambda ()
                (do ([index 0 (+ index 1)]) ((= index 30000))
                  (let ([actual (user-ref (unix-user-name expected))])
                    (check (and (string=? (unix-user-name expected) (unix-user-name actual))
                                (= (unix-user-uid expected) (unix-user-uid actual))
                                (= (unix-user-gid expected) (unix-user-gid actual))))))))
            (list first second))))]

  [(environment-race)
   ;; An empty lookup key cannot identify an environment variable.
   (check (not (getenv "")))
   (putenv "CSPP_ENV_环境" "é中文")
   (check (string=? "é中文" (getenv "CSPP_ENV_环境")))
   ;; Error case: a spawn must not free libc-owned environment storage after putenv grows it.
   (run-threads
     (list (lambda ()
             (do ([index 0 (+ index 1)]) ((= index 300))
               (let ([child (process-spawn "/bin/true" '() '())])
                 (check (process-exit-success? (process-wait child))))))
           (lambda ()
             (do ([index 0 (+ index 1)]) ((= index 3000))
               (putenv (format "CSPP_RUNTIME_ENV_~a" index) "environment-growth")))
           (lambda ()
             (let ([stat (foreign-procedure "cspp_statx" (utf-8 boolean) ptr)])
               (do ([index 0 (+ index 1)]) ((= index 3000))
                 ;; Error case: native tilde expansion must share environment read ownership.
                 (check (vector? (stat "~" #t))))))))]

  [(boundaries)
   (check (string=? "é中文" (process-result-stdout
                             (process-capture "/usr/bin/printf" "%s" "é中文"))))
   (putenv "CSPP_EMPTY_ENV_TEST" "must-not-inherit")
   (check (string=? "" (process-result-stdout
                         (process-capture "/bin/sh" "-c" "printf %s \"${CSPP_EMPTY_ENV_TEST-}\""
                           :env '() :env-mode replace))))
   ;; Error case: embedded NUL and invalid options must be rejected before spawning.
   (check (rejects? (lambda () (process-capture "printf" "a\x0;b"))))
   (check (rejects? (lambda () (process-capture "true" :timeout 1.0))))
   (check (rejects? (lambda () (process-capture "true" :encoding 'invalid))))]

  [(capture)
   (let* ([input (make-string 1048576 #\a)]
          [result (process-capture "/bin/cat" :stdin input :timeout 3000)])
     (check (string=? input (process-result-stdout result))))
   ;; Error case: early stdin closure is not a timeout.
   (check (process-exit-success? (process-result-status
            (process-capture "/bin/true" :stdin (make-string 1048576 #\a)))))
   ;; Error case: input to a child that never reads must obey the capture deadline.
   (check (guard (condition [(system-timeout-error? condition) #t])
            (process-capture "/bin/sleep" "1" :stdin (make-string 1048576 #\a) :timeout 25)
            #f))
   ;; Error case: a descendant retaining stdout must not trigger a second reap.
   (check (guard (condition [(system-timeout-error? condition) #t])
            (process-capture "/bin/sh" "-c" "sleep 0.2 & exit 0" :timeout 25)
            #f))
   (check (equal? #vu8(0 255 1)
            (process-result-stdout
              (process-capture "/bin/cat" :stdin #vu8(0 255 1) :encoding binary))))]

  [(gc)
   ;; Error case: native capture must permit another Scheme thread to collect.
   (run-threads
     (list (lambda () (process-capture "/bin/sleep" "0.2"))
           ;; Error case: waiting with no native streams must also permit GC in another thread.
           (lambda () (process-run "/bin/sleep" "0.2" :stdout null :stderr null))
           (lambda ()
             (milisleep 50)
             (do ([index 0 (+ index 1)]) ((= index 100))
               (make-bytevector 1000000)))))]

  [(wait)
   (let ([child (process-spawn "/bin/sleep" '("0.2") '())])
     ;; Error case: all concurrent waiters must receive the same successful status.
     (run-threads
       (list (lambda () (check (process-exit-success? (process-wait child))))
             (lambda () (check (process-exit-success? (process-wait child))))
             (lambda () (check (process-exit-success? (process-wait/timeout child 2000)))))))]

  [(wait-notification)
   ;; Error case: a timeout must not hold the process mutex, reap the child, or block another waiter.
   (let ([child (process-spawn "/bin/sleep" '("0.2") '())])
     (run-threads
       (list (lambda () (check (process-exit-success? (process-wait child))))
             (lambda ()
               (check (guard (failure [(system-timeout-error? failure) #t])
                        (process-wait/timeout child 10) #f)))
             (lambda ()
               (milisleep 30)
               (collect)
               (check (process-exit-success? (process-wait/timeout child 2000))))))
     (check (eq? (process-status child) (process-wait/timeout child 0))))
   ;; Error case: repeated timeouts must release each invocation's notification descriptor.
   (let* ([child (process-spawn "/bin/sleep" '("3") '())]
          [before (length (directory-list "/proc/self/fd"))])
     (dynamic-wind
       void
       (lambda ()
         (do ([index 0 (+ index 1)]) ((= index 50))
           (check (guard (failure [(system-timeout-error? failure) #t])
                    (process-wait/timeout child 0) #f)))
         (check (= before (length (directory-list "/proc/self/fd")))))
       (lambda () (process-kill child 9) (process-wait child))))]

  [(ports)
   (let ([child (process-spawn "/bin/cat" '() '((stdin . pipe) (stdout . pipe)))])
     (check (and (output-port? (process-stdin child)) (input-port? (process-stdout child))))
     (put-bytevector (process-stdin child) (string->utf8 "live-é中文"))
     (close-output-port (process-stdin child))
     (check (string=? "live-é中文" (utf8->string (get-bytevector-all (process-stdout child)))))
     (check (process-exit-success? (process-wait child)))
     (process-close-ports! child)
     (process-close-ports! child))
   (let ([child (process-spawn "/bin/sh" '("-c" "printf out; printf err >&2")
                              '((stdout . pipe) (stderr . stdout)))])
     (check (not (process-stderr child)))
     (check (string=? "outerr" (utf8->string (get-bytevector-all (process-stdout child)))))
     (check (process-exit-success? (process-wait child)))
     (process-close-ports! child))
   ;; Error case: live stream modes and duplicate keys must not be silently ignored.
   (check (rejects? (lambda () (process-spawn "/bin/true" '() '((stdout . capture))))))
   (check (rejects? (lambda () (process-spawn "/bin/true" '()
                                           '((stdin . null) (stdin . pipe))))))
   ;; Error case: an unrelated child must not inherit a pipe writer and suppress EOF.
   (call-with-values make-pipe
     (lambda (input output)
       (let ([child (process-spawn "/bin/sleep" '("0.2") '())])
         (close-output-port output)
         (check (eof-object? (get-u8 input)))
         (close-input-port input)
         (process-wait child))))]

  [(inherit)
   (check (string=? "inherited-input"
            (process-result-stdout (process-capture "/bin/cat" :stdin inherit))))]

  [(signals)
   (let ([original (signal-mask)] [sig (signal usr1)])
     (dynamic-wind
       (lambda () (signal-block! (list sig)))
       (lambda ()
         (check (exists (lambda (item) (= (signal-number item) (signal-number sig)))
                        (signal-mask)))
         ;; Thread-directed delivery avoids other runtime threads eligible for SIGUSR1.
         (check (= 0 ((foreign-procedure "pthread_kill" (uptr int) int)
                       ((foreign-procedure "pthread_self" () uptr)) (signal-number sig))))
         (check (= (signal-number sig) (signal-number (wait-signal (list sig)))))
         (signal-unblock! (list sig))
         (check (not (exists (lambda (item) (= (signal-number item) (signal-number sig)))
                             (signal-mask)))))
       (lambda () (signal-mask-set! original))))
   ;; Error case: empty waits, unblocked waits, and unmaskable signals are invalid.
   (check (rejects? (lambda () (wait-signal '()))))
   (check (rejects? (lambda () (wait-signal (list usr2)))))
   (check (rejects? (lambda () (signal-block! (list kill)))))]

  [else (error 'system-runtime "unknown regression mode")])
