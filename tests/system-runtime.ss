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
  [(pipeline-contracts)
   ;; Error case: circular nested argv lists must be rejected before native traversal.
   (let ([spec (list "/bin/true")])
     (set-cdr! spec spec)
     (check (rejects? (lambda () (capture-pipeline (list spec))))) )]

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
               (let ([child (spawn-process "/bin/true" '() '())])
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
                             (capture-process "/usr/bin/printf" "%s" "é中文"))))
   (putenv "CSPP_EMPTY_ENV_TEST" "must-not-inherit")
   (check (string=? "" (process-result-stdout
                         (capture-process "/bin/sh" "-c" "printf %s \"${CSPP_EMPTY_ENV_TEST-}\""
                           :env '() :env-mode replace))))
   ;; Error case: embedded NUL and invalid options must be rejected before spawning.
   (check (rejects? (lambda () (capture-process "printf" "a\x0;b"))))
   (check (rejects? (lambda () (capture-process "true" :timeout 1.0))))
   (check (rejects? (lambda () (capture-process "true" :encoding 'invalid))))]

  [(capture)
   (let* ([input (make-string 1048576 #\a)]
          [result (capture-process "/bin/cat" :stdin input :timeout 3000)])
     (check (string=? input (process-result-stdout result))))
   ;; Error case: early stdin closure is not a timeout.
   (check (process-exit-success? (process-result-status
            (capture-process "/bin/true" :stdin (make-string 1048576 #\a)))))
   ;; Error case: input to a child that never reads must obey the capture deadline.
   (check (guard (condition [(system-timeout-error? condition) #t])
            (capture-process "/bin/sleep" "1" :stdin (make-string 1048576 #\a) :timeout 25)
            #f))
   ;; Error case: a descendant retaining stdout must not trigger a second reap.
   (check (guard (condition [(system-timeout-error? condition) #t])
            (capture-process "/bin/sh" "-c" "sleep 0.2 & exit 0" :timeout 25)
            #f))
   (check (equal? #vu8(0 255 1)
            (process-result-stdout
              (capture-process "/bin/cat" :stdin #vu8(0 255 1) :encoding binary))))]

  [(gc)
   ;; Error case: native capture must permit another Scheme thread to collect.
   (run-threads
     (list (lambda () (capture-process "/bin/sleep" "0.2"))
           ;; Error case: waiting with no native streams must also permit GC in another thread.
           (lambda () (run-process "/bin/sleep" "0.2" :stdout null :stderr null))
           (lambda ()
             (milisleep 50)
             (do ([index 0 (+ index 1)]) ((= index 100))
               (make-bytevector 1000000)))))]

  [(wait)
   (let ([child (spawn-process "/bin/sleep" '("0.2") '())])
     ;; Error case: all concurrent waiters must receive the same successful status.
     (run-threads
       (list (lambda () (check (process-exit-success? (process-wait child))))
             (lambda () (check (process-exit-success? (process-wait child))))
             (lambda () (check (process-exit-success? (process-wait/timeout child 2000)))))))]

  [(ports)
   (let ([child (spawn-process "/bin/cat" '() '((stdin . pipe) (stdout . pipe)))])
     (check (and (output-port? (process-stdin child)) (input-port? (process-stdout child))))
     (put-bytevector (process-stdin child) (string->utf8 "live-é中文"))
     (close-output-port (process-stdin child))
     (check (string=? "live-é中文" (utf8->string (get-bytevector-all (process-stdout child)))))
     (check (process-exit-success? (process-wait child)))
     (process-close-ports! child)
     (process-close-ports! child))
   (let ([child (spawn-process "/bin/sh" '("-c" "printf out; printf err >&2")
                              '((stdout . pipe) (stderr . stdout)))])
     (check (not (process-stderr child)))
     (check (string=? "outerr" (utf8->string (get-bytevector-all (process-stdout child)))))
     (check (process-exit-success? (process-wait child)))
     (process-close-ports! child))
   ;; Error case: live stream modes and duplicate keys must not be silently ignored.
   (check (rejects? (lambda () (spawn-process "/bin/true" '() '((stdout . capture))))))
   (check (rejects? (lambda () (spawn-process "/bin/true" '()
                                           '((stdin . null) (stdin . pipe))))))
   ;; Error case: an unrelated child must not inherit a pipe writer and suppress EOF.
   (call-with-values make-pipe
     (lambda (input output)
       (let ([child (spawn-process "/bin/sleep" '("0.2") '())])
         (close-output-port output)
         (check (eof-object? (get-u8 input)))
         (close-input-port input)
         (process-wait child))))]

  [(inherit)
   (check (string=? "inherited-input"
            (process-result-stdout (capture-process "/bin/cat" :stdin inherit))))]

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
