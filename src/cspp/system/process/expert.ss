(library (cspp system process expert)
  (export
          ;; exit status records
          process-exit-status?
          process-exit-status-kind
          process-exit-status-code
          process-exit-success?
          make-process-exit-status

          ;; process records
          process?
          process-pid
          process-command
          process-arguments
          process-stdin
          process-stdout
          process-stderr
          process-status
          process-running?

          ;; process creation and waiting
          process-spawn
          process-spawn-shell
          process-wait
          process-wait/no-hang
          process-wait/timeout
          process-kill
          process-terminate
          process-interrupt
          process-close-ports!

          ;; pipes and pipelines
          make-pipe
          process-spawn-pipeline
          process-run-pipeline

          ;; raw process APIs
          fork
          vfork
          getpid
          gettid
          getppid

          $validate-process-options $wait-status->exit-status)
  (import (cspp chez)
          (cspp system common)
          (only (cspp system time) milisleep)
          (cspp system process status)
          (cspp system process private)
          (cspp system private)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; process records
;;;;===----------------------------------------------------------------------===

  #|proc:process?
  The `process?` procedure returns whether `object` is a CSPP process object.
  The `object` parameter is any Scheme object.
  |#
  #|record:$process
  The process record holds a child pid, command metadata, and optional binary stream ports.
  The exported `process?` predicate accepts any object and returns whether it is a process.
  Exit status publication and reaping use a per-process mutex; port I/O is caller-coordinated.
  |#
  (define-record-type ($process %make-process process?)
    (nongenerative)
    (fields (immutable pid $process-pid)
            (immutable command $process-command)
            (immutable arguments $process-arguments)
            (immutable stdin $process-stdin)
            (immutable stdout $process-stdout)
            (immutable stderr $process-stderr)
            (mutable status $process-status set-process-status!)
            (immutable mutex process-mutex)))

  #|proc:process-pid
  The `process-pid` procedure returns the operating-system process id of `process`.
  The `process` parameter is a process object.
  |#
  (define-who process-pid
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-pid process))))

  #|proc:process-command
  The `process-command` procedure returns the program or command for `process`.
  The `process` parameter is a process object.
  For `process-spawn-shell` this is `"sh"`; shell text is stored among `process-arguments`.
  |#
  (define-who process-command
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-command process))))

  #|proc:process-arguments
  The `process-arguments` procedure returns the argument list stored in `process`.
  The `process` parameter is a process object.
  The list was copied at spawn time, but strings and accessor results remain shared and mutable.
  |#
  (define-who process-arguments
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-arguments process))))

  #|proc:process-stdin
  The `process-stdin` procedure returns the stdin port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define-who process-stdin
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-stdin process))))

  #|proc:process-stdout
  The `process-stdout` procedure returns the stdout port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define-who process-stdout
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-stdout process))))

  #|proc:process-stderr
  The `process-stderr` procedure returns the stderr port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define-who process-stderr
    (lambda (process)
      (pcheck-who who ([process? process]) ($process-stderr process))))

  (define make-process
    (lambda (pid command arguments stdin stdout stderr status)
      (%make-process pid command arguments stdin stdout stderr status (make-mutex))))

  ;; Return the first close error, but finish releasing the port after a failed flush.
  (define $close-port/cleanup
    (lambda (port)
      (if (not port)
          #f
          (guard (failure
                   [else
                    (guard (ignored [else (void)])
                      (when (output-port? port) (clear-output-port port)))
                    (guard (ignored [else (void)])
                      (unless (port-closed? port) (close-port port)))
                    failure])
            (unless (port-closed? port) (close-port port))
            #f))))

  #|proc:process-status
  The `process-status` procedure returns the cached exit status of `process`, or `#f`.
  The `process` argument is a process object. This does not poll the operating system.
  |#
  (define-who process-status
    (lambda (process)
      (pcheck-who who ([process? process])
        (with-mutex (process-mutex process) ($process-status process)))))

  (define $process-spawn-ffi
    (foreign-procedure "cspp_spawn_process" (ptr ptr utf-8 int int int) ptr))

  (define $waitpid-ffi
    (foreign-procedure "cspp_waitpid" (int int) ptr))

  (define $send-signal-ffi
    (foreign-procedure "cspp_send_signal" (int int) ptr))

  (define $make-pipe-ffi
    (foreign-procedure "cspp_make_pipe" () ptr))

  (define $spawn-pipeline-ffi
    (foreign-procedure "cspp_spawn_pipeline" (ptr) ptr))

  (define $process-run-pipeline-ffi
    (foreign-procedure "cspp_run_pipeline" (ptr) ptr))

  (define $stream-mode
    (lambda (options key)
      (case ($option-ref options key 'inherit)
        [(inherit) 0] [(null) 1] [(pipe) 2] [(stdout) 3])))

  #|proc:process-spawn
  The `process-spawn` procedure starts executable string `program` with string-list `arguments`.
  It returns a process object. `options` is an alist; `cwd` is a nonempty directory string or `#f`,
  and `env` is a string-pair alist or `#f` to inherit. An empty `env` clears the environment.
  Program, argument, directory, and environment strings must contain no NUL characters.
  Environment names must be nonempty and must not contain `=`.
  `stdin`, `stdout`, and `stderr` accept `inherit` (default), `null`, or `pipe`.
  `stderr` also accepts `stdout` to merge it into the child's stdout.
  Descriptors other than standard input, output, and error are closed in the child before exec.
  The child's SIGPIPE disposition is restored to default; the caller's signal mask is inherited.
  A piped stdin is exposed as a binary output port; piped outputs as binary input ports.
  Callers own these ports and must close them, including stdin to deliver EOF to the child.
  Waiting does not close ports. Concurrent waiters share one cached exit status.
  |#
  (define process-spawn
    (lambda (program arguments options)
      ($process-spawn 'process-spawn program arguments options)))

  (define $process-spawn
    (lambda (who program arguments options)
      (pcheck-who who ([$os-string? program] [$os-string-list? arguments] [list? options])
        ($validate-process-options options #t who)
        (let* ([argv (cons program arguments)]
               [result (ffi-result-ref
                         ($process-spawn-ffi argv ($option-ref options 'env #f) ($cwd-option options)
                           ($stream-mode options 'stdin) ($stream-mode options 'stdout)
                           ($stream-mode options 'stderr)))]
               [pid (vector-ref result 0)] [ports '()])
          (guard (condition
                   [else
                    (for-each $close-port/cleanup ports)
                    (let ([close-fd (foreign-procedure "close" (int) int)])
                      (do ([index 1 (fx+ index 1)]) ((fx= index 4))
                        (let ([fd (vector-ref result index)])
                          (when (fx>= fd 0) (close-fd fd)))))
                    ($send-signal-ffi pid 9)
                    ($waitpid-ffi pid 0)
                    (raise condition)])
            (let ([open-stream
                    (lambda (index output?)
                      (let ([fd (vector-ref result index)])
                        (if (fx< fd 0) #f
                            (let ([port (if output?
                                            (open-fd-output-port fd 'block #f)
                                            (open-fd-input-port fd 'block #f))])
                              (vector-set! result index -1)
                              (set! ports (cons port ports))
                              port))))])
              (let* ([stdin (open-stream 1 #t)] [stdout (open-stream 2 #f)]
                     [stderr (open-stream 3 #f)])
                (make-process pid program (list-copy arguments) stdin stdout stderr #f))))))))

  #|proc:process-spawn-shell
  The `process-spawn-shell` procedure starts `command` through the host shell.
  The `command` parameter is a NUL-free string interpreted by `sh -c`.
  The `options` parameter is an option alist accepted by `process-spawn`.
  Returns a live process object; callers own its pipe ports and must eventually reap the child.
  |#
  (define-who process-spawn-shell
    (lambda (command options)
      (pcheck-who who ([$os-string? command] [list? options])
              ($process-spawn who "sh" (list "-c" command) options))))

  (define $wait-vector->status
    (lambda (result)
      (if result
          ($wait-status->exit-status (vector-ref result 1))
          #f)))

  #|proc:process-wait/no-hang
  The `process-wait/no-hang` procedure returns `process`'s exit status, or `#f` if still running.
  The `process` argument is a process object. Reaping and status publication are synchronized.
  |#
  (define-who process-wait/no-hang
    (lambda (process)
      (pcheck-who who ([process? process])
        (with-mutex (process-mutex process)
          (or ($process-status process)
              (let ([status ($wait-vector->status
                              (ffi-result-ref ($waitpid-ffi (process-pid process) 1)))])
                (when status (set-process-status! process status))
                status))))))

  #|proc:process-wait
  The `process-wait` procedure waits for `process` and returns its cached exit status.
  The `process` argument is a process object. Waiting leaves its streams open for draining.
  Drain live output concurrently with a running child; waiting first can deadlock on full pipes.
  |#
  (define-who process-wait
    (lambda (process)
      (pcheck-who who ([process? process])
        (let loop ()
          (or (process-wait/no-hang process)
              (begin (milisleep 5) (loop)))))))

  (define $monotonic-milliseconds
    (lambda ()
      (let ([now (current-time 'time-monotonic)])
        (+ (* (time-second now) 1000) (quotient (time-nanosecond now) 1000000)))))

  #|proc:process-wait/timeout
  The `process-wait/timeout` procedure waits for `process` for at most `milliseconds`.
  `process` is a process object; `milliseconds` is an exact nonnegative integer.
  It returns the cached exit status or raises a timeout condition, leaving the child running.
  |#
  (define-who process-wait/timeout
    (lambda (process milliseconds)
      (pcheck-who who ([process? process] [natural? milliseconds])
        (let ([deadline (+ ($monotonic-milliseconds) milliseconds)])
          (let loop ()
            (or (process-wait/no-hang process)
                (let ([remaining (- deadline ($monotonic-milliseconds))])
                  (when (<= remaining 0)
                    (raise (make-system-timeout-error who "process wait timed out"
                             `((pid . ,(process-pid process)) (timeout . ,milliseconds)))))
                  (milisleep (min 5 remaining))
                  (loop))))))))

  #|proc:process-running?
  The `process-running?` procedure returns whether `process` is still running.
  The `process` parameter is a process object.
  |#
  (define-who process-running?
    (lambda (process)
      (pcheck-who who ([process? process])
              (not (process-wait/no-hang process)))))

  #|proc:process-kill
  The `process-kill` procedure sends signal number `signal-number` to `process`.
  The `process` parameter is a process object.
  The `signal-number` parameter is an exact positive signed 32-bit signal number.
  A process with a cached or newly observed exit status is left alone.
  The return value is unspecified. Native signaling failures raise system conditions.
  |#
  (define-who process-kill
    (lambda (process signal-number)
      (pcheck-who who ([process? process]
               [(lambda (value) (and (fixnum? value) (<= 1 value #x7fffffff))) signal-number])
        (with-mutex (process-mutex process)
          ;; A reaped PID may be reused. Never signal it through a completed process object.
          (unless ($process-status process)
            (let ([status ($wait-vector->status
                            (ffi-result-ref ($waitpid-ffi (process-pid process) 1)))])
              (if status
                  (set-process-status! process status)
                  (ffi-result-ref ($send-signal-ffi (process-pid process) signal-number)))))))))

  #|proc:process-terminate
  The `process-terminate` procedure sends `SIGTERM` to `process`.
  The `process` parameter is a process object.
  The return value is unspecified. This does not wait for the process to exit.
  |#
  (define-who process-terminate
    (lambda (process)
      (pcheck-who who ([process? process]) (process-kill process 15))))

  #|proc:process-interrupt
  The `process-interrupt` procedure sends `SIGINT` to `process`.
  The `process` parameter is a process object.
  The return value is unspecified. This does not wait for the process to exit.
  |#
  (define-who process-interrupt
    (lambda (process)
      (pcheck-who who ([process? process]) (process-kill process 2))))

  #|proc:process-close-ports!
  The `process-close-ports!` procedure closes any open ports owned by `process`.
  Repeated calls are harmless. This does not terminate or wait for the child.
  The `process` parameter is a process object.
  All ports are attempted before the first close error is raised. If flushing stdin fails,
  its remaining buffered data is discarded so that the underlying descriptor can be closed.
  The return value is unspecified; callers must coordinate I/O and closing on shared ports.
  |#
  (define-who process-close-ports!
    (lambda (process)
      (pcheck-who who ([process? process])
        (let ([failure #f])
          (for-each
            (lambda (port)
              (let ([condition ($close-port/cleanup port)])
                (when (and condition (not failure))
                  (set! failure condition))))
            (list (process-stdin process) (process-stdout process) (process-stderr process)))
          (when failure (raise failure))))))

  #|proc:make-pipe
  The `make-pipe` procedure returns two binary ports backed by an OS pipe: input, then output.
  The caller owns both ports and must close them. Closing output delivers EOF after buffered data.
  |#
  (define make-pipe
    (lambda ()
      (let ([fds (ffi-result-ref ($make-pipe-ffi))] [input #f] [output #f])
        (guard (failure
                 [else
                  ($close-port/cleanup input)
                  ($close-port/cleanup output)
                  (let ([close-fd (foreign-procedure "close" (int) int)])
                    (do ([index 0 (fx+ index 1)]) ((fx= index 2))
                      (let ([fd (vector-ref fds index)])
                        (when (fx>= fd 0) (close-fd fd)))))
                  (raise failure)])
          (set! input (open-fd-input-port (vector-ref fds 0) 'block #f))
          (vector-set! fds 0 -1)
          (set! output (open-fd-output-port (vector-ref fds 1) 'block #f))
          (vector-set! fds 1 -1)
          (values input output)))))

  #|proc:process-spawn-pipeline
  The `process-spawn-pipeline` procedure starts process specs as a live pipeline.
  The `process-specs` parameter is a nonempty proper list of nonempty proper string lists.
  Command strings must contain no NUL characters.
  The `options` parameter is reserved and should currently be the empty list.
  The procedure returns process objects immediately and does not wait for them.
  Callers should wait for or terminate the returned processes.
  Stdin for the first command, stdout for the final command, and all stderrs are inherited.
  No pipe ports are exposed; the returned list follows specification order.
  |#
  (define-who process-spawn-pipeline
    (lambda (process-specs options)
      (pcheck-who who ([$process-specs? process-specs] [null? options])
              (let ([pids (ffi-result-ref ($spawn-pipeline-ffi process-specs))])
                (let loop ([i 0] [specs process-specs] [out '()])
                  (if (fx= i (vector-length pids))
                      (reverse out)
                      (loop (fx+ i 1)
                            (cdr specs)
                            (cons (make-process (vector-ref pids i)
                                                (car (car specs))
                                                (list-copy (cdr (car specs)))
                                                #f #f #f #f)
                                  out))))))))

  #|proc:process-run-pipeline
  The `process-run-pipeline` procedure runs process specs as a pipeline and waits.
  The `process-specs` parameter is a nonempty proper list of nonempty proper string lists.
  Command strings must contain no NUL characters.
  Unlike `process-spawn-pipeline`, this procedure does not return live process objects.
  It returns a list containing the final process exit status.
  The final process's stdout is discarded without buffering; stderr is inherited.
  |#
  (define-who process-run-pipeline
    (lambda (process-specs)
      (pcheck-who who ([$process-specs? process-specs])
              (let ([result (ffi-result-ref ($process-run-pipeline-ffi process-specs))])
                (list ($wait-status->exit-status (vector-ref result 1)))))))

;;;;===----------------------------------------------------------------------===
;;;; raw process APIs
;;;;===----------------------------------------------------------------------===

  (define $err-process
    (lambda (who message)
      (raise (make-system-error who #f message '()))))

  #|proc:fork
  The `fork` procedure performs a raw operating-system fork.
  This expert API is unsafe in a threaded Scheme runtime.
  Returns zero in the child and the child pid in the parent; failure raises a system condition.
  |#
  (define-who fork
    (let ([ffi (foreign-procedure "cspp_fork" () ptr)])
      (lambda ()
        (let ([result (ffi)])
          (if (string? result) ($err-process who result) result)))))

  #|proc:vfork
  The `vfork` procedure raises a system unsupported condition without creating a child.
  A native vfork child cannot safely return through the C wrapper and Scheme FFI stack.
  Use `process-spawn` to launch an executable.
  |#
  (define-who vfork
    (let ([ffi (foreign-procedure "cspp_vfork" () ptr)])
      (lambda ()
        (ffi-result-ref (ffi)))))

  #|proc:getpid
  The `getpid` procedure returns the current process id.
  |#
  (define getpid get-process-id)

  #|proc:gettid
  The `gettid` procedure returns the calling thread's Chez Scheme thread identifier.
  It is an alias of `get-thread-id`; this is not the native operating-system thread id.
  |#
  (define gettid get-thread-id)

  #|proc:getppid
  The `getppid` procedure returns the parent process id.
  |#
  (define getppid (foreign-procedure "cspp_getppid" () int))

)
