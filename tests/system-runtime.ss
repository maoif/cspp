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
  [(tilde-lookup-gc)
   ;; Error case: tilde lookups must allow GC without losing other foreign-call arguments.
   (load-shared-object (getenv "LD_PRELOAD"))
   (let ([arm (foreign-procedure "cspp_lookup_fixture_arm" () void)]
         [entered (foreign-procedure "cspp_lookup_fixture_entered" () int)]
         [finished (foreign-procedure "cspp_lookup_fixture_finished" () int)]
         [release (foreign-procedure "cspp_lookup_fixture_release" () void)]
         [home (getenv "HOME")]
         [unsetenv (foreign-procedure "unsetenv" (string) int)]
         [link (foreign-procedure "cspp_link" (string string) ptr)]
         [symlink (foreign-procedure "cspp_symlink" (string string) ptr)]
         [touch (foreign-procedure "cspp_touch" (string ptr ptr int) ptr)]
         [path (format "/tmp/cspp-tilde-gc-~a" (getpid))])
     (dynamic-wind
       (lambda ()
         (check (= 0 (unsetenv "HOME")))
         (call-with-output-file path (lambda (port) (put-string port "tilde GC"))))
       (lambda ()
         (for-each
           (lambda (operation)
             (let ([mutex (make-mutex)] [ready (make-condition)] [done? #f]
                   [failure #f] [collected-during-lookup? #f])
               (arm)
               (fork-thread
                 (lambda ()
                   (guard (condition [else (set! failure condition)])
                     (operation))
                   (with-mutex mutex (set! done? #t) (condition-signal ready))))
               (dynamic-wind
                 void
                 (lambda ()
                   (let loop () (when (= 0 (entered)) (milisleep 1) (loop)))
                   (collect)
                   ;; Reuse nursery storage so dangling C arguments cannot rely on old contents.
                   (do ([index 0 (+ index 1)]) ((= index 256))
                     (make-vector 4096 #f))
                   (set! collected-during-lookup? (= 0 (finished))))
                 (lambda ()
                   (release)
                   (with-mutex mutex
                     (let loop () (unless done? (condition-wait ready mutex) (loop))))))
               (when failure (raise failure))
               (check collected-during-lookup?)))
           (list
             (lambda ()
               (let ([directory (fs-open-directory "~__cspp_tilde_gc__/tmp")])
                 (fs-close-directory directory)))
             (lambda ()
               (let ([directory (fs-open-directory "~/tmp")])
                 (fs-close-directory directory)))
             ;; Error case: expanding src must not invalidate the still-unread dest argument.
             (lambda ()
               (check (eq? #t (link (string-append "~__cspp_tilde_gc__" path)
                                   (string-append path "-hard"))))
               (check (same-file? path (string-append path "-hard"))))
             ;; Symlink targets are literal; expand the destination to exercise GC instead.
             (lambda ()
               (check (eq? #t (symlink (string-append path)
                                      (string-append "~__cspp_tilde_gc__" path "-symbolic"))))
               (check (same-file? path (string-append path "-symbolic"))))
             ;; Error case: timestamp vectors must be decoded before path expansion allows GC.
             (lambda ()
               (check (eq? #t (touch (string-append "~__cspp_tilde_gc__" path)
                                    (vector 123456789 1600000000)
                                    (vector 987654321 1600000001) 1)))
               (let ([accessed (file-access-time path)] [modified (file-modification-time path)])
                 (check (and (= 1600000000 (time-second accessed))
                             (= 123456789 (time-nanosecond accessed))
                             (= 1600000001 (time-second modified))
                             (= 987654321 (time-nanosecond modified)))))))))
       (lambda ()
         (when home (putenv "HOME" home))
         (for-each
           (lambda (suffix)
             (let ([file (string-append path suffix)])
               (when (file-exists? file #f) (delete-file file))))
           '("" "-hard" "-symbolic")))))]

  [(handoff-interrupt-depth)
   ;; Error case: normal resource handoff must preserve every enclosing interrupt-disable count.
   (let ([previous (timer-interrupt-handler)])
     (dynamic-wind
       void
       (lambda ()
         (for-each
           (lambda (operation)
             (let ([handled? #f])
               (timer-interrupt-handler (lambda () (set! handled? #t)))
               (with-interrupts-disabled
                 (with-interrupts-disabled
                   (set-timer 1)
                   (operation)
                   (check (not handled?)))
                 (check (not handled?)))
               (with-interrupts-disabled (void))
               (check handled?)))
           (list
             (lambda ()
               (let-values ([(input output) (make-pipe)])
                 (close-port input)
                 (close-port output)))
             (lambda ()
               (let ([child (process-spawn "/bin/true" '() '((stdout . pipe)))])
                 (process-close-ports! child)
                 (check (process-exit-success? (process-wait child)))))
             (lambda ()
               (for-each
                 (lambda (child) (check (process-exit-success? (process-wait child))))
                 (process-spawn-pipeline '(("/bin/true") ("/bin/true")) '()))))))
       (lambda () (set-timer 0) (timer-interrupt-handler previous))))]

  [(native-interrupts)
   ;; Error case: native steps must service Scheme handlers before long operations complete.
   (let* ([thread ((foreign-procedure "pthread_self" () uptr))]
          [send (foreign-procedure "pthread_kill" (uptr int) int)]
          [milliseconds
            (lambda ()
              (let ([now (current-time 'time-monotonic)])
                (+ (* 1000 (time-second now)) (quotient (time-nanosecond now) 1000000))))])
     (for-each
       (lambda (operation)
         (let ([start (milliseconds)] [handled #f])
           (parameterize ([keyboard-interrupt-handler
                            (lambda () (set! handled (- (milliseconds) start)))])
             (fork-thread (lambda () (milisleep 50) (send thread 2)))
             (operation)
             (with-interrupts-disabled (void))
             (check (and handled (< handled 200))))))
       (list (lambda () (process-run /bin/sleep "0.4"))
             (lambda () (process-capture /bin/sleep "0.4"))
             (lambda () (process-run-pipeline '(("/bin/sleep" "0.4"))))
             (lambda () (process-capture-pipeline '(("/bin/sleep" "0.4"))))))
     ;; Error case: abandoning a capture or pipeline must reap every unreturned immediate child.
     (for-each
       (lambda (operation)
         (let ([sentinel (list 'abandon-operation)] [start (milliseconds)] [fds #f])
           (set! fds (length (directory-list "/proc/self/fd")))
           (parameterize ([keyboard-interrupt-handler (lambda () (raise sentinel))])
             (fork-thread (lambda () (milisleep 50) (send thread 2)))
             (check (guard (failure [else (eq? failure sentinel)]) (operation) #f)))
           (check (< (- (milliseconds) start) 500))
           (check (= fds (length (directory-list "/proc/self/fd"))))
           (let ([status (make-bytevector 4 0)])
             (check (< ((foreign-procedure "waitpid" (int u8* int) int) -1 status 1) 0)))))
       (list (lambda () (process-capture /bin/sleep "3"))
             (lambda ()
               (process-capture-pipeline '(("/bin/sleep" "3") ("/bin/sleep" "3"))))))
     ;; Error case: an unrelated SIGINT must interrupt a wait for an already-blocked USR1.
     (let ([mask (signal-mask)] [handled #f] [start (milliseconds)])
       (dynamic-wind
         (lambda () (signal-block! '(usr1)))
         (lambda ()
           (parameterize ([keyboard-interrupt-handler
                            (lambda () (set! handled (- (milliseconds) start)))])
             (fork-thread
               (lambda () (milisleep 50) (send thread 2) (milisleep 350) (send thread 10)))
             (check (eq? 'usr1 (signal-name (wait-signal '(usr1)))))
             (with-interrupts-disabled (void))
             (check (and handled (< handled 200)))))
         (lambda () (signal-mask-set! mask))))
     ;; Error case: checkpoints must preserve an enclosing interrupt-disable count.
     (let ([handled? #f])
       (parameterize ([keyboard-interrupt-handler (lambda () (set! handled? #t))])
         (with-interrupts-disabled
           (fork-thread (lambda () (milisleep 50) (send thread 2)))
           (process-run /bin/sleep "0.15")
           (check (not handled?)))
         (check handled?)))
     ;; Error case: abandoning a synchronous signal wait must leave its caller's mask intact.
     (let ([mask (signal-mask)] [sentinel (list 'abort-signal-wait)])
       (dynamic-wind
         (lambda () (signal-block! '(usr1)))
         (lambda ()
           (parameterize ([keyboard-interrupt-handler (lambda () (raise sentinel))])
             (fork-thread (lambda () (milisleep 50) (send thread 2)))
             (check (guard (failure [else (eq? failure sentinel)])
                      (wait-signal '(usr1)) #f)))
           (check (exists (lambda (sig) (eq? 'usr1 (signal-name sig))) (signal-mask))))
         (lambda () (signal-mask-set! mask)))))]

  [(native-continuation-lifetime)
   ;; Error case: leaving an operation frees its context; replay must reject that freed state.
   (let ([resume #f] [replaying? #f]
         [fds (length (directory-list "/proc/self/fd"))])
     (let ([result
             (guard (failure [else failure])
               (let/1cc leave
                 (parameterize
                   ([keyboard-interrupt-handler
                      (lambda ()
                        ;; call/cc captures a reusable continuation only to test lifetime rejection.
                        (call/cc (lambda (continuation) (set! resume continuation) (leave 'left))))])
                   (let ([thread ((foreign-procedure "pthread_self" () uptr))])
                     (fork-thread
                       (lambda () (milisleep 50)
                         ((foreign-procedure "pthread_kill" (uptr int) int) thread 2)))
                     (process-capture /bin/sleep "3")))))])
       (if replaying?
           (check (and (error? result)
                       (string=? "cannot resume a disposed native operation"
                                 (condition-message result))))
           (begin
             (check (eq? result 'left))
             (check (= fds (length (directory-list "/proc/self/fd"))))
             (set! replaying? #t)
             (resume)))))]

  [(timeout-grace)
   ;; Error case: making capture resumable must preserve TERM cleanup before forced termination.
   (let ([path (format "/tmp/cspp-timeout-grace-~a" (getpid))])
     (dynamic-wind
       void
       (lambda ()
         (let ([command (format "trap 'printf stopped > ~a; exit 0' TERM; while :; do :; done" path)])
           (check (guard (failure [(system-timeout-error? failure) #t])
                    (process-run /bin/sh "-c" (begin command) :timeout 100) #f)))
         (check (string=? "stopped" (call-with-input-file path get-string-all))))
       (lambda () (when (file-exists? path) (delete-file path)))))]

  [(pending-wait-interrupt)
   ;; Error case: SIGINT already delivered before poll must not wait for the child's exit.
   (let* ([child (process-spawn "/bin/sleep" '("0.4") '())]
          [thread ((foreign-procedure "pthread_self" () uptr))]
          [send (foreign-procedure "pthread_kill" (uptr int) int)]
          [start (current-time 'time-monotonic)] [handled #f])
     (dynamic-wind
       void
       (lambda ()
         (parameterize ([keyboard-interrupt-handler
                          (lambda ()
                            (set! handled (time-difference (current-time 'time-monotonic) start)))])
           (with-interrupts-disabled (void))
           (send thread 2)
           (process-wait child)
           (with-interrupts-disabled (void))
           (check (and handled (= 0 (time-second handled))
                       (< (time-nanosecond handled) 200000000)))))
       (lambda () (process-kill child 9) (process-wait child))))]

  [(ownership-interrupts)
   ;; Error case: handlers escaping resource acquisition must not leak descriptors or children.
   (let ([fds (lambda () (length (directory-list "/proc/self/fd")))]
         [previous (timer-interrupt-handler)] [sentinel (list 'ownership-interrupt)])
     (for-each
       (lambda (kind)
         (let ([baseline (fds)] [armed? #t] [child #f] [ports '()] [children '()])
           (dynamic-wind
             void
             (lambda ()
               (when (eq? kind 'wait)
                 (set! child (process-spawn "/bin/sleep" '("0.1") '())))
               (timer-interrupt-handler
                 (lambda ()
                   (when armed?
                     (if (or (> (fds) baseline)
                             (and (eq? kind 'pipeline)
                                  (call-with-input-file
                                    (format "/proc/self/task/~a/children" (getpid))
                                    (lambda (port) (not (eof-object? (get-line port)))))))
                         (begin (set! armed? #f) (raise sentinel))
                         (set-timer 1)))))
               (guard (failure [else (check (eq? failure sentinel))])
                 (set-timer 1)
                 (case kind
                   [(spawn) (set! child
                              (process-spawn "/bin/sleep" '("0.1") '((stdout . pipe))))]
                   [(pipe) (let-values ([(input output) (make-pipe)])
                             (set! ports (list input output)))]
                   [(wait) (process-wait child)]
                   [(pipeline) (set! children
                                 (process-spawn-pipeline
                                   (make-list 3 '("/bin/sleep" "0.1")) '()))]))
               (set! armed? #f)
               (set-timer 0))
             (lambda ()
               (set! armed? #f)
               (set-timer 0)
               (timer-interrupt-handler previous)
               (for-each close-port ports)
               (when child (process-close-ports! child) (process-wait child))
               (for-each process-wait children)))
           (collect)
           (check (= baseline (fds)))
           (milisleep 150)
           (let ([status (make-bytevector 4 0)])
             (check (< ((foreign-procedure "waitpid" (int u8* int) int) -1 status 1) 0)))))
       '(spawn pipe wait pipeline)))]

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
   ;; Address-space limits do not reliably constrain guest allocations under user-mode emulation.
   (load-shared-object (getenv "LD_PRELOAD"))
   (let ([argument (make-string 1024 #\a)]
         [arm (foreign-procedure "cspp_allocation_fixture_arm" (size_t) void)]
         [failed (foreign-procedure "cspp_allocation_fixture_failed" () int)])
     (for-each
       (lambda (thunk)
         (dynamic-wind
           ;; Native UTF-8 copies reserve four bytes per Scheme character and one terminator.
           (lambda () (arm 4097))
           (lambda ()
             (check (guard (failure [(system-error? failure) (= 12 (system-error-code failure))])
                      (thunk)
                      #f))
             (check (= 1 (failed))))
           (lambda () (arm 0))))
       (list (lambda () (process-wait (process-spawn "/bin/true" (list argument) '())))
             (lambda () (process-capture /bin/true (begin argument)))
             (lambda () (process-run-pipeline (list (list "/bin/true" argument)))))))]

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

  [(legacy-localized-errors)
   ;; Error case: strerror follows the locale encoding, which need not be UTF-8.
   ;; This subprocess alone changes the locale; every opened file is closed by its helper.
   (let* ([locale (foreign-procedure "setlocale" (int utf-8) utf-8)]
          [message (foreign-procedure "strerror" (int) uptr)]
          [length (foreign-procedure "strlen" (uptr) size_t)]
          [copy (foreign-procedure "memcpy" (u8* uptr size_t) uptr)]
          [original (locale 6 #f)]
          [path (format "/tmp/cspp-legacy-message-~a" (getpid))])
     (dynamic-wind
       void
       (lambda ()
         (when (locale 6 "fr_FR")
           (call-with-output-file path (lambda (port) (put-string port "not executable")))
           (let* ([pointer (message 13)] [bytes (make-bytevector (length pointer))])
             (copy bytes pointer (bytevector-length bytes))
             (let ([expected (bytevector->string bytes (make-transcoder (latin-1-codec)))])
               (check
                 (guard (failure [(system-permission-error? failure)
                                  (string=? expected (system-error-message failure))])
                   (process-capture (begin path))
                   #f))))))
       (lambda ()
         (locale 6 original)
         (when (file-exists? path) (delete-file path)))))]

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
