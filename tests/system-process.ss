(import (cspp) (only (cspp system process expert) $wait-status->exit-status))

(mat process-rejected-capture-result

     ;; Error case: a rejected exit must preserve the completed stdout/stderr and process metadata.
     (guard (failure
              [(system-exit-error? failure)
               (let* ([context (system-error-context failure)] [entry (assq 'result context)]
                      [result (and entry (cdr entry))])
                 (and (process-result? result)
                      (string=? "output" (process-result-stdout result))
                      (string=? "diagnostic" (process-result-stderr result))
                      (string=? "/bin/sh" (process-result-command result))
                      (positive-natural? (process-result-pid result))
                      (= 17 (process-exit-status-code (process-result-status result)))
                      (= 17 (cdr (assq 'status-code context)))
                      (eq? 'exit (cdr (assq 'status-kind context)))))])
       (process-capture/check /bin/sh "-c" "printf output; printf diagnostic >&2; exit 17")
       #f)

     ;; Error case: an explicit success predicate may reject a normal exit after seeing it once.
     (let ([calls 0] [observed #f])
       (guard (failure
                [(system-exit-error? failure)
                 (let ([entry (assq 'result (system-error-context failure))])
                   (and entry (= calls 1)
                        (eq? observed (process-result-status (cdr entry)))
                        (equal? #vu8(0 255) (process-result-stdout (cdr entry)))
                        (not (process-result-stderr (cdr entry)))))])
         (process-capture-shell "printf '\\000\\377'"
           :stderr stdout :encoding binary
           :success (lambda (status) (set! calls (+ calls 1)) (set! observed status) #f))
         #f))

     ;; Error case: uncaptured streams remain #f in a recovered rejected result.
     (guard (failure
              [(system-exit-error? failure)
               (let ([entry (assq 'result (system-error-context failure))])
                 (and entry (not (process-result-stdout (cdr entry)))
                      (string=? "diagnostic" (process-result-stderr (cdr entry)))))])
       (process-capture /bin/sh "-c" "printf diagnostic >&2" :stdout null :success '())
       #f)

     ;; Error case: exceptions from a success callback must propagate unchanged.
     (let ([sentinel (list 'callback-failure)])
       (guard (failure [else (eq? sentinel failure)])
         (process-capture /bin/true :success (lambda (status) (raise sentinel)))
         #f)))

(mat process-wait-status-contract

     ;; Error case: expert callers must not feed unchecked arithmetic nonfixnums or negative bits.
     (for-all
       (lambda (raw)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? '$wait-status->exit-status (condition-who condition)))])
           ($wait-status->exit-status raw)
           #f))
       '(#f "bad" -1 1.0 1/2 #x100000000000000000))

     ;; Literal POSIX fixtures cover normal exit, termination, core dump, and suspension.
     (for-all
       (lambda (entry)
         (let ([status ($wait-status->exit-status (car entry))])
           (and (eq? (cadr entry) (process-exit-status-kind status))
                (= (caddr entry) (process-exit-status-code status)))))
       '((0 exit 0) (#x2a00 exit 42) (15 signal 15) (#x8b signal 11) (#x137f stopped 19))))

(mat process-default-sigpipe

     ;; Error case: external commands must not inherit Chez's ignored SIGPIPE disposition.
     (let* ([command "kill -PIPE $$; exit 99"] [specs (list (list "/bin/sh" "-c" command))])
       (for-all
         (lambda (run)
           (let ([status (run)])
             (and (eq? 'signal (process-exit-status-kind status))
                  (= 13 (process-exit-status-code status)))))
         (list
           (lambda ()
             (process-result-status (process-capture "/bin/sh" "-c" (begin command))))
           (lambda () (process-run "/bin/sh" "-c" (begin command)))
           (lambda () (process-wait (process-spawn "/bin/sh" (list "-c" command) '())))
           (lambda () (process-result-status (process-capture-pipeline specs)))
           (lambda () (car (process-run-pipeline specs)))
           (lambda () (process-wait (car (process-spawn-pipeline specs '()))))))))

(mat process-unrelated-descriptors

     ;; Error case: every launcher must keep an unrelated parent file out of the child.
     (call-with-input-file "/etc/hostname"
       (lambda (port)
         (let* ([fd (port-file-descriptor port)]
                [command (format "test ! /proc/self/fd/~a -ef /etc/hostname" fd)]
                [specs (list (list "/bin/sh" "-c" command))])
           (and
             (for-all
               (lambda (run) (process-exit-success? (run)))
               (list
                 (lambda ()
                   (process-result-status (process-capture "/bin/sh" "-c" (begin command))))
                 (lambda () (process-run "/bin/sh" "-c" (begin command)))
                 (lambda ()
                   (process-wait (process-spawn "/bin/sh" (list "-c" command) '())))
                 (lambda ()
                   (process-result-status (process-capture-pipeline specs)))
                 (lambda () (car (process-run-pipeline specs)))
                 (lambda () (process-wait (car (process-spawn-pipeline specs '()))))))
             ;; Closing descriptors in the child must leave the parent's file usable.
             (not (eof-object? (get-char port)))))))

     ;; Error case: descriptors above the launcher's own pipes must also be closed.
     (call-with-input-file "/etc/hostname"
       (lambda (port)
         (let ([fd ((foreign-procedure "fcntl" (int int int) int)
                     (port-file-descriptor port) 0 128)])
           (and (>= fd 128)
                (dynamic-wind
                  void
                  (lambda ()
                    (let ([command (format "test ! /proc/self/fd/~a -ef /etc/hostname" fd)])
                      (process-exit-success? (process-run "/bin/sh" "-c" (begin command)))))
                  (lambda () ((foreign-procedure "close" (int) int) fd))))))))

(mat process-empty-working-directory

     ;; Error case: an explicitly empty cwd must not run in the parent's working directory.
     (for-all
       (lambda (start)
         (guard (condition [else #t]) (start) #f))
       (list
         (lambda () (process-capture "/bin/true" :cwd ""))
         (lambda () (process-run "/bin/true" :cwd ""))
         (lambda () (process-capture-shell "exit 0" :cwd ""))
         (lambda () (process-run-shell "exit 0" :cwd ""))
         (lambda ()
           (process-wait (process-spawn "/bin/true" '() '((cwd . "")))))
         (lambda ()
           (process-wait (process-spawn-shell "exit 0" '((cwd . ""))))))))

(mat process-close-broken-stdin

     ;; Error case: a failed stdin flush must not leave any process ports open.
     (let* ([child (process-spawn "/bin/true" '()
                     '((stdin . pipe) (stdout . pipe) (stderr . pipe)))]
            [ports (list (process-stdin child) (process-stdout child) (process-stderr child))])
       (dynamic-wind
         void
         (lambda ()
           (put-bytevector (process-stdin child) #vu8(1 2 3))
           (process-wait child)
           (let ([failed? (guard (condition [(i/o-error? condition) #t])
                            (process-close-ports! child) #f)])
             (and failed? (for-all port-closed? ports)
                  (begin (process-close-ports! child) #t))))
         (lambda ()
           (for-each
             (lambda (port)
               (unless (port-closed? port)
                 (when (output-port? port) (clear-output-port port))
                 (close-port port)))
             ports)))))

(mat process-port-setup-failures

     ;; Error case: injected port-construction and close failures must complete cleanup.
     (= 0 (system (if (file-exists? "system-process-cleanup-probe.ss")
                      "../cspp --script=system-process-cleanup-probe.ss"
                      "./cspp --script=tests/system-process-cleanup-probe.ss"))))

(mat process-handoff-continuation-exits

     ;; Error case: continuation escapes after native transfer must close ports and reap children.
     (= 0 (system (if (file-exists? "system-process-handoff.ss")
                      "../cspp --script=system-process-handoff.ss"
                      "./cspp --script=tests/system-process-handoff.ss"))))

(mat process-run-discarded-captures

     ;; Error case: status-only runs must drain capture pipes without retaining stream-sized data.
     (for-all
       (lambda (run)
         (run)
         (collect)
         (let* ([before (+ (bytes-allocated) (bytes-deallocated))] [status (run)]
                [allocated (- (+ (bytes-allocated) (bytes-deallocated)) before)])
           (and (process-exit-success? status) (< allocated 1048576))))
       (list
         (lambda ()
           (process-run "/usr/bin/head" "-c" "16777216" "/dev/zero"
             :stdout capture :stderr stdout))
         (lambda ()
           (process-run/check "/usr/bin/head" "-c" "16777216" "/dev/zero" :stdout capture))
         (lambda ()
           (process-run-shell
             "/usr/bin/head -c 16777216 /dev/zero; /usr/bin/head -c 16777216 /dev/zero >&2"
             :stdout capture :stderr capture))))

     ;; Captured run streams remain pipes, preserving child behavior compared with /dev/null.
     (process-exit-success?
       (process-run "/bin/sh" "-c" "test -p /proc/self/fd/1 && test -p /proc/self/fd/2"
         :stdout capture :stderr capture))

     ;; Error case: discarding a busy stream must still enforce the command timeout.
     (guard (condition [(system-timeout-error? condition) #t])
       (process-run "/bin/sh" "-c" "while :; do printf busy; done"
         :stdout capture :timeout 25)
       #f))

(mat process-path-exec-errors

     ;; Error case: exec-time failures must not hide a working command in a later PATH entry.
     (let* ([root (format "/tmp/cspp-path-exec-~a" (getpid))]
            [first (string-append root "/first")]
            [second (string-append root "/second")]
            [first-tool (string-append first "/cspp-path-tool")]
            [second-tool (string-append second "/cspp-path-tool")]
            [path (string-append first ":" second)]
            [original-path (getenv "PATH")]
            [environment (list (cons "PATH" path))]
            [chmod (foreign-procedure "chmod" (string unsigned-32) int)]
            [rmdir (foreign-procedure "rmdir" (string) int)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir first)
           (mkdir second)
           (call-with-output-file first-tool
             (lambda (port) (put-string port "#!/no-such-cspp-interpreter\n")))
           (call-with-output-file second-tool
             (lambda (port) (put-string port "#!/bin/sh\nprintf found\n")))
           (chmod first-tool #o700)
           (chmod second-tool #o700))
         (lambda ()
           (putenv "PATH" path)
           (and (string=? "found"
                  (process-result-stdout
                    (process-capture cspp-path-tool :env (begin environment))))
                (let ([child (process-spawn "cspp-path-tool" '("argument")
                               (list (cons 'env environment) '(stdout . null)))])
                  (process-exit-success? (process-wait child)))
                (string=? "found"
                  (process-result-stdout (process-capture-pipeline '(("cspp-path-tool")))))
                (for-all process-exit-success? (process-run-pipeline '(("cspp-path-tool"))))
                (for-all (lambda (child) (process-exit-success? (process-wait child)))
                  (process-spawn-pipeline
                    '(("cspp-path-tool") ("/bin/sh" "-c" "exec /bin/cat >/dev/null")) '()))
                (string=? "found"
                  (process-result-stdout
                    (process-capture cspp-path-tool :cwd (begin root)
                      :env '(("PATH" . "missing:first:second")))))
                ;; Error case: explicit paths do not fall back to another PATH entry.
                (guard (condition [(system-error? condition)
                                   (= 2 (system-error-code condition))])
                  (process-capture (begin first-tool)) #f)
                (begin
                  (chmod first-tool #o600)
                  (string=? "found" (process-result-stdout (process-capture cspp-path-tool))))
                ;; Error case: preserve EACCES when every candidate fails, even after misses.
                (begin
                  (chmod second-tool #o600)
                  (guard (condition [(system-permission-error? condition)
                                     (= 13 (system-error-code condition))])
                    (process-capture cspp-path-tool :env
                      (list (cons "PATH" (string-append path ":/no-such-cspp-path"))))
                    #f))
                ;; Error case: direct execution must not silently run text through a shell.
                (begin
                  (delete-file first-tool)
                  (call-with-output-file first-tool
                    (lambda (port) (put-string port "exit 0\n")))
                  (chmod first-tool #o700)
                  (guard (condition [(system-error? condition)
                                     (= 8 (system-error-code condition))])
                    (process-capture cspp-path-tool) #f))))
         (lambda ()
           (putenv "PATH" (or original-path "/bin:/usr/bin"))
           (delete-file first-tool)
           (delete-file second-tool)
           (rmdir first)
           (rmdir second)
           (rmdir root)))))

(mat process-path-search

     ;; Error case: a searchable directory must not hide an executable later in PATH.
     (let* ([root (format "/tmp/cspp-path-search-~a" (getpid))]
            [first (string-append root "/first")]
            [second (string-append root "/second")]
            [directory (string-append first "/tool")]
            [program (string-append second "/tool")]
            [path (string-append first ":" second)]
            [environment (list (cons "PATH" path))]
            [chmod (foreign-procedure "chmod" (string unsigned-32) int)]
            [rmdir (foreign-procedure "rmdir" (string) int)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir first)
           (mkdir second)
           (mkdir directory)
           (call-with-output-file program
             (lambda (port) (put-string port "#!/bin/sh\nexit 0\n")))
           (chmod program #o700))
         (lambda ()
           (and (process-exit-success?
                  (process-result-status (process-capture tool :env (begin environment))))
                (process-exit-success?
                  (process-wait (process-spawn "tool" '() (list (cons 'env environment)))))
                ;; Relative PATH entries are resolved in the requested child directory.
                (process-exit-success?
                  (process-result-status
                    (process-capture tool :cwd (begin root)
                      :env '(("PATH" . "first:second")))))))
         (lambda ()
           (delete-file program)
           (rmdir directory)
           (rmdir first)
           (rmdir second)
           (rmdir root)))))

(mat process-api-export-names

     (andmap
       (lambda (library)
         (let ([exports (library-exports library)])
           (andmap (lambda (name) (and (memq name exports) #t))
             '(process-spawn process-spawn-shell process-spawn-pipeline
               process-run process-run/check process-run-shell process-run-pipeline
               process-capture process-capture/check process-capture-shell
               process-capture-pipeline))))
       '((cspp) (cspp system) (cspp system process)))

     ;; Error case: discarded names must not survive as aliases in any process library or facade.
     (andmap
       (lambda (library)
         (let ([exports (library-exports library)])
           (andmap (lambda (name) (not (memq name exports)))
             '(spawn-process spawn-shell-command pipe-processes
               run-process run-process/check shell-command run-pipeline
               capture-process capture-process/check capture-shell-command capture-pipeline
               %run-process %capture-process %run-shell-command %capture-shell-command))))
       '((cspp) (cspp system) (cspp system process)
         (cspp system process expert) (cspp system process macros)
         (cspp system process backend))))

(mat process-argument-domains

     ;; Error case: every pipeline specification must contain a command and string arguments.
     (andmap
       (lambda (specs)
         (and (guard (condition [else #t]) (process-capture-pipeline specs) #f)
              (guard (condition [else #t]) (process-run-pipeline specs) #f)
              (guard (condition [else #t]) (process-spawn-pipeline specs '()) #f)))
       '(() (()) (("true" . "extra")) (("true" 1)) (("true\x0;ignored"))))

     ;; Error case: reserved pipeline options must be rejected before starting a child.
     (guard (condition [else #t])
       (let ([children (process-spawn-pipeline '(("/bin/true")) '((cwd . "/missing")))])
         (for-each process-wait children)
         #f))

     ;; Error case: an environment key cannot contain the separator or be empty.
     (and (guard (condition [else #t])
            (process-capture "/bin/true" :env '(("A=B" . "C"))) #f)
          (guard (condition [else #t])
            (process-wait (process-spawn "/bin/true" '() '((env . (("" . "C")))))) #f)))

(mat process-macro-syntax

     (let ([r (process-capture "printf" "hello"
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "hello" (process-result-stdout r))
            (string=? "" (process-result-stderr r))))

     (let ([r (process-capture printf hello
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "hello" (process-result-stdout r))
            (string=? "" (process-result-stderr r))))

     ;; Error case: duplicate option keyword should be rejected by macro expansion.
     (guard (c [else #t])
       (eval '(process-capture "printf" "x" :stdout capture :stdout capture))
       #f)

     ;; Error case: unknown option keyword should be rejected by macro expansion.
     (guard (c [else #t])
       (eval '(process-capture "printf" "x" :bad-option capture))
       #f))

(mat process-capture-behavior

     (let ([r (process-capture "sh" "-c" "printf out; printf err >&2"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "out" (process-result-stdout r))
            (string=? "err" (process-result-stderr r))))

     (let ([r (process-capture "cat"
                :stdin "abc"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "abc" (process-result-stdout r)))))

(mat process-option-surface

     (let ([r (process-capture "cat"
                :stdin (string->utf8 "bytes")
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "bytes" (process-result-stdout r))))

     (let ([r (process-capture "/bin/sh" "-c" "printf \"$CSPP_PROCESS_TEST\""
                :env '(("CSPP_PROCESS_TEST" . "env-ok"))
                :env-mode replace
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "env-ok" (process-result-stdout r))))

     (let ([r (process-capture "/bin/sh" "-c" "pwd"
                :cwd "/tmp"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "/tmp\n" (process-result-stdout r))))

     (let ([r (process-capture "/bin/sh" "-c" "printf out; printf err >&2"
                :stdout capture
                :stderr stdout)])
       (and (process-exit-success? (process-result-status r))
            (string=? "outerr" (process-result-stdout r))
            (not (process-result-stderr r))))

     (let ([s (process-run "/bin/sh" "-c" "exit 7"
                :stdin null
                :stdout null
                :stderr null
                :success '(7))])
       (and (eq? 'exit (process-exit-status-kind s))
            (= 7 (process-exit-status-code s))))

     (let ([r (process-capture/check "/bin/sh" "-c" "exit 7"
                :stdout null
                :stderr null
                :success (lambda (status)
                           (= 7 (process-exit-status-code status))))])
       (and (process-result? r)
            (= 7 (process-exit-status-code (process-result-status r))))))

(mat process-shell-helpers

     (let ([s (process-run-shell "exit 7"
                :stdout null
                :stderr null
                :success '(7))])
       (and (eq? 'exit (process-exit-status-kind s))
            (= 7 (process-exit-status-code s))))

     (let ([r (process-capture-shell "printf shell"
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "shell" (process-result-stdout r)))))

(mat process-spawn-apis

     (let* ([p (process-spawn "sleep" '("1")
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [running-before (process-running? p)])
       (process-terminate p)
       (let ([status (process-wait p)])
         (and (process? p)
              running-before
              (process-exit-status? status)
              (not (process-running? p)))))

     (let* ([p (process-spawn "sleep" '("1")
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait/no-hang p)])
       (process-kill p 15)
       (let ([final-status (process-wait p)])
         (and (not status)
              (process-exit-status? final-status))))

     (let* ([p (process-spawn "true" '()
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait/timeout p 1000)])
       (and (process-exit-success? status)
            (process-exit-success? (process-wait/no-hang p))))

     (let* ([p (process-spawn-shell "exit 0"
                                    '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait p)])
       (and (process? p)
            (process-exit-success? status)
            (not (process-stdin p))
            (not (process-stdout p))
            (not (process-stderr p)))))

(define raw-process-capture
  (lambda (arguments)
    (let ([handle ((foreign-procedure "cspp_process_operation_create" () uptr))]
          [prepare (foreign-procedure "cspp_process_prepare_capture"
                     (uptr ptr ptr utf-8 ptr int int int int int int) void)]
          [step (foreign-procedure __collect_safe "cspp_process_operation_step" (uptr) int)]
          [result (foreign-procedure "cspp_process_operation_result" (uptr) ptr)]
          [dispose (foreign-procedure __collect_safe "cspp_process_operation_dispose" (uptr) void)])
      (when (= handle 0) (error 'raw-process-capture "native allocation failed"))
      (dynamic-wind
        void
        (lambda ()
          (prepare handle arguments #f "" #f 0 0 0 0 0 -1)
          (let loop ()
            (if (= 0 (step handle)) (loop) (result handle))))
        (lambda () (dispose handle))))))

(mat process-ffi-result-tags

     (let ([raw (raw-process-capture (list "sh" "-c" "true"))])
       (and (vector? raw)
            (eq? 'ok (vector-ref raw 0))))

     ;; Error case: missing executables return errno tags without losing the path error code.
     (let ([spawn raw-process-capture])
       (let ([missing (spawn (list "definitely-not-a-cspp-test-command"))])
         (and (vector? missing)
              (eq? 'errno (vector-ref missing 0))
              (positive? (vector-ref missing 2))
              ;; Error case: preserve path errors other than a missing executable.
              (let* ([path (format "/tmp/cspp-process-file-~a" (get-process-id))]
                     [port (open-output-file path)]
                     [raw #f])
                (put-string port "not-a-directory")
                (close-output-port port)
                (set! raw (spawn (list (string-append path "/child"))))
                (delete-file path)
                (and (eq? 'errno (vector-ref raw 0))
                     (not (= (vector-ref missing 2) (vector-ref raw 2))))))))

     ;; Error case: an executable script with a missing interpreter must fail to spawn.
     (let* ([path (format "/tmp/cspp-missing-interpreter-~a" (get-process-id))]
            [chmod (foreign-procedure "chmod" (string unsigned-32) int)]
            [port (open-output-file path)])
       (put-string port "#!/definitely-no-such-cspp-interpreter\n")
       (close-output-port port)
       (chmod path #o700)
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (guard (c [(system-error? c) #t] [else #f])
             (process-capture path :stdout capture :stderr capture)
             #f))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (let ([status (process-run "/bin/sh" "-c" "exit 127"
                     :stdin null :stdout null :stderr null)])
       (and (eq? 'exit (process-exit-status-kind status))
            (= 127 (process-exit-status-code status)))))

;; Error case: check variant should raise for nonzero exit status.
(mat process-check-errors

     (process-exit-success?
       (process-run/check "/bin/true" :stdout null :stderr null))

     ;; Error case: the renamed checked run form still rejects nonzero exit codes.
     (guard (condition [(system-exit-error? condition) #t] [else #f])
       (process-run/check "/bin/sh" "-c" "exit 7" :stdout null :stderr null)
       #f)

     ;; Error case: shell status rejection identifies the renamed API in its condition.
     (guard (condition
              [(system-exit-error? condition)
               (eq? 'process-capture-shell (system-error-operation condition))]
              [else #f])
       (process-capture-shell "exit 7" :success '(0))
       #f)

     ;; Error case: a native spawn failure must identify the renamed creation procedure.
     (guard (condition
              [(system-error? condition) (eq? 'process-spawn (system-error-operation condition))]
              [else #f])
       (process-spawn "definitely-not-a-cspp-test-command" '() '())
       #f))

;; Error case: process timeout should raise and clean up the child.
(mat process-timeout

     ;; Error case: waiting with no I/O must still honor an explicit deadline.
     (guard (condition [(system-timeout-error? condition) #t] [else #f])
       (process-run "/bin/sleep" "1" :timeout 25 :stdout null :stderr null)
       #f)

     (guard (c [(system-timeout-error? c) #t] [else #f])
       (process-capture/check "sh" "-c" "sleep 2" :timeout 50 :stdout capture :stderr capture)
       #f))

(mat process-pipeline

     ;; Error case: inherited pipeline stderr must not be reported as an empty capture.
     (let ([result (process-capture-pipeline '(("printf" "pipeline")))])
       (and (string=? "pipeline" (process-result-stdout result))
            (not (process-result-stderr result))
            (eq? 'pipeline (process-result-command result))))

     (let ([r (process-capture-pipeline
               (list (list "printf" "abc")
                     (list "tr" "a-z" "A-Z")))])
       (and (process-result? r)
            (string=? "ABC" (process-result-stdout r))))

     (let ([r (process-capture-pipeline
               (list (list "printf" "abc")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "tr" "a-z" "A-Z")))])
       (and (process-result? r)
            (string=? "ABC" (process-result-stdout r)))))

     ;; Error case: a pipeline member that cannot exec must raise a system error.
     (guard (c [(system-error? c) #t] [else #f])
       (process-capture-pipeline (list (list "definitely-not-a-cspp-test-command")))
       #f)

     ;; Error case: a later launch failure must clean up the already-started child.
     (guard (c [(system-error? c) #t] [else #f])
       (process-capture-pipeline
         '(("sleep" "5") ("definitely-not-a-cspp-test-command")))
       #f)

     ;; Error case: status-only pipelines share the same launch cleanup behavior.
     (guard (c [(system-error? c) #t] [else #f])
       (process-run-pipeline
         '(("sleep" "5") ("definitely-not-a-cspp-test-command")))
       #f)

     ;; Error case: live pipeline startup must clean up when a member cannot exec.
     (guard (c [(system-error? c) #t] [else #f])
       (process-spawn-pipeline (list (list "true")
                             (list "definitely-not-a-cspp-test-command"))
                       '())
       #f)

(mat process-expert-pipes

     ;; Error case: process-run-pipeline must discard large stdout without allocating a captured copy.
     (let* ([allocated (+ (bytes-allocated) (bytes-deallocated))]
            [statuses (process-run-pipeline '(("head" "-c" "4194304" "/dev/zero") ("cat")))]
            [used (- (+ (bytes-allocated) (bytes-deallocated)) allocated)])
       (and (process-exit-success? (car statuses)) (< used 1048576)))

     (call-with-values make-pipe
       (lambda (in out)
         (put-bytevector out (string->utf8 "pipe"))
         (close-output-port out)
         (let ([data (get-bytevector-all in)])
           (close-input-port in)
           (string=? "pipe" (utf8->string data)))))

     (if (file-directory? "/proc/self/fd")
         (let ([before (length (directory-list "/proc/self/fd"))])
           (do ([index 0 (fx+ index 1)]) ((fx= index 100))
             (call-with-values make-pipe
               (lambda (input output)
                 (close-input-port input)
                 (close-output-port output))))
           (= before (length (directory-list "/proc/self/fd"))))
         #t)

     (let ([statuses (process-run-pipeline
                      (list (list "printf" "abc")
                            (list "tr" "a-z" "A-Z")))])
       (and (= 1 (length statuses))
            (process-exit-success? (car statuses))))

     (let ([statuses (process-run-pipeline
                      (list (list "printf" "abc")
                            (list "cat")
                            (list "cat")
                            (list "cat")
                            (list "cat")
                            (list "cat")
                            (list "tr" "a-z" "A-Z")))])
       (and (= 1 (length statuses))
            (process-exit-success? (car statuses)))))

(mat process-pipe-processes

     ;; Error case: a live pipeline must inherit final stdout, unlike a status-only pipeline.
     (let* ([root (if (file-exists? "system-live-pipeline-stdout.ss") ".." ".")]
            [path (format "/tmp/cspp-live-pipeline-stdout-~a" (getpid))]
            [command (format "~a/cspp --script=~a/tests/system-live-pipeline-stdout.ss > ~a"
                             root root path)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (and (= 0 (system command))
                (call-with-input-file path
                  (lambda (port)
                    (let ([output (get-string-all port)])
                      (and (string? output) (string=? "SINGLE\nCHAIN\n" output)))))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (for-all
       (lambda (count)
         (let* ([specs (append '(("printf" "abc"))
                              (make-list (- count 2) '("cat"))
                              '(("sh" "-c" "cat >/dev/null")))]
                [processes (process-spawn-pipeline specs '())]
                [statuses (map process-wait processes)])
           (and (= count (length processes))
                (for-all process? processes)
                (for-all process-exit-success? statuses))))
       '(2 6)))

(mat process-closed-standard-descriptors

     ;; Error case: a pipe descriptor can equal stdin or stdout when those descriptors are closed.
     (let ([close (foreign-procedure "close" (int) int)]
           [dup (foreign-procedure "dup" (int) int)]
           [dup2 (foreign-procedure "dup2" (int int) int)])
       (let ([saved-stdin (dup 0)]
             [saved-stdout (dup 1)])
         (let ([r (dynamic-wind
                    (lambda () (close 0) (close 1))
                    (lambda ()
                      (process-capture "sh" "-c" "printf out; printf err >&2"
                                       :stdout capture
                                       :stderr capture))
                    (lambda ()
                      (dup2 saved-stdin 0)
                      (dup2 saved-stdout 1)
                      (close saved-stdin)
                      (close saved-stdout)))])
           (and (process-exit-success? (process-result-status r))
                (string=? "out" (process-result-stdout r))
                (string=? "err" (process-result-stderr r)))))))

(mat process-native-cleanup-interrupts

     ;; Error case: final native disposal must survive exceptions and continuation escapes.
     (= 0 (system (if (file-exists? "system-native-cleanup.ss")
                      "../cspp --script=system-native-cleanup.ss"
                      "./cspp --script=tests/system-native-cleanup.ss"))))

(mat process-mutex-interrupts

     ;; Error case: an interrupted process operation must not block subsequent readers forever.
     (let* ([root (if (file-exists? "system-process-mutex.ss") ".." ".")]
            [launcher (string-append root "/cspp")]
            [script (string-append "--script=" root "/tests/system-process-mutex.ss")])
       (process-exit-success?
         (process-run (begin launcher) (begin script) :timeout 20000))))
