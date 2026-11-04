(import (cspp))

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

;; Error case: a missing executable should return an errno tagged result.
(mat process-ffi-result-tags

     (let ([raw ((foreign-procedure "cspp_spawn_capture"
                                    (ptr ptr string ptr int int int int int int)
                                    ptr)
                 (list "sh" "-c" "true") #f "" #f 0 0 0 0 0 -1)])
       (and (vector? raw)
            (eq? 'ok (vector-ref raw 0))))

     (let ([spawn (foreign-procedure "cspp_spawn_capture"
                                     (ptr ptr string ptr int int int int int int)
                                     ptr)])
       (let ([missing (spawn (list "definitely-not-a-cspp-test-command")
                             #f "" #f 0 0 0 0 0 -1)])
         (and (vector? missing)
              (eq? 'errno (vector-ref missing 0))
              (positive? (vector-ref missing 2))
              ;; Error case: preserve path errors other than a missing executable.
              (let* ([path (format "/tmp/cspp-process-file-~a" (get-process-id))]
                     [port (open-output-file path)]
                     [raw #f])
                (put-string port "not-a-directory")
                (close-output-port port)
                (set! raw (spawn (list (string-append path "/child"))
                                 #f "" #f 0 0 0 0 0 -1))
                (delete-file path)
                (and (eq? 'errno (vector-ref raw 0))
                     (not (= (vector-ref missing 2) (vector-ref raw 2))))))))

     ;; Error case: live spawning must report a missing executable as an error.
     (guard (c [(system-error? c) #t] [else #f])
       (process-spawn "definitely-not-a-cspp-test-command" '() '())
       #f)

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
       #f)

     (guard (c [(system-exit-error? c) #t] [else #f])
       (process-capture/check "sh" "-c" "exit 7" :stdout capture :stderr capture)
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

     (let* ([processes (process-spawn-pipeline
                        (list (list "printf" "abc")
                              (list "sh" "-c" "cat >/dev/null"))
                        '())]
            [statuses (map process-wait processes)])
       (and (= 2 (length processes))
            (andmap process? processes)
            (andmap process-exit-success? statuses)))

     (let* ([processes (process-spawn-pipeline
                        (list (list "printf" "abc")
                              (list "cat")
                              (list "cat")
                              (list "cat")
                              (list "cat")
                              (list "sh" "-c" "cat >/dev/null"))
                        '())]
            [statuses (map process-wait processes)])
       (and (= 6 (length processes))
            (andmap process? processes)
            (andmap process-exit-success? statuses))))

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
