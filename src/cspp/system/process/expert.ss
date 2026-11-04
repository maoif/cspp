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
          spawn-process
          spawn-shell-command
          process-wait
          process-wait/no-hang
          process-wait/timeout
          process-kill
          process-terminate
          process-interrupt
          process-close-ports!

          ;; pipes and pipelines
          make-pipe
          pipe-processes
          run-pipeline

          ;; raw process APIs
          fork
          vfork
          getpid
          gettid
          getppid

          $validate-process-options)
  (import (cspp chez)
          (cspp system common)
          (cspp system info)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; process status records
;;;;===----------------------------------------------------------------------===

  #|proc:process-exit-status?
The `process-exit-status?` procedure returns whether `object` is a status.
The `object` parameter is any Scheme object.
|#
  (define-record-type ($process-exit-status %make-process-exit-status
                                             process-exit-status?)
    (nongenerative)
    (fields (immutable kind $process-exit-status-kind)
            (immutable code $process-exit-status-code)))

  #|proc:process-exit-status-kind
  The `process-exit-status-kind` procedure returns the kind stored in `status`.
  The `status` parameter is a process exit-status record.
  |#
  (define process-exit-status-kind
    (lambda (status)
      (pcheck ([process-exit-status? status]) ($process-exit-status-kind status))))

  #|proc:process-exit-status-code
  The `process-exit-status-code` procedure returns the numeric code in `status`.
  The `status` parameter is a process exit-status record.
  |#
  (define process-exit-status-code
    (lambda (status)
      (pcheck ([process-exit-status? status]) ($process-exit-status-code status))))

  #|proc:make-process-exit-status
  The `make-process-exit-status` procedure returns an exit-status record.
  `kind` is `exit`, `signal`, or `stopped`; `code` is an exact nonnegative integer.
  For `exit` the code is an exit code; for the other kinds it is a signal number.
  |#
  (define make-process-exit-status
    (lambda (kind code)
      (pcheck ([(lambda (value) (memq value '(exit signal stopped))) kind] [natural? code])
        (%make-process-exit-status kind code))))

  #|proc:process-exit-success?
The `process-exit-success?` procedure returns whether `status` is exit code 0.
The `status` parameter is a process exit-status record.
|#
  (define process-exit-success?
    (lambda (status)
      (pcheck ([process-exit-status? status])
              (and (eq? 'exit (process-exit-status-kind status))
                   (= 0 (process-exit-status-code status))))))

;;;;===----------------------------------------------------------------------===
;;;; process records
;;;;===----------------------------------------------------------------------===

  #|proc:process?
The `process?` procedure returns whether `object` is a CSPP process object.
The `object` parameter is any Scheme object.
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
  (define process-pid
    (lambda (process)
      (pcheck ([process? process]) ($process-pid process))))

  #|proc:process-command
  The `process-command` procedure returns the program or command for `process`.
  The `process` parameter is a process object.
  |#
  (define process-command
    (lambda (process)
      (pcheck ([process? process]) ($process-command process))))

  #|proc:process-arguments
  The `process-arguments` procedure returns the argument list stored in `process`.
  The `process` parameter is a process object.
  |#
  (define process-arguments
    (lambda (process)
      (pcheck ([process? process]) ($process-arguments process))))

  #|proc:process-stdin
  The `process-stdin` procedure returns the stdin port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define process-stdin
    (lambda (process)
      (pcheck ([process? process]) ($process-stdin process))))

  #|proc:process-stdout
  The `process-stdout` procedure returns the stdout port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define process-stdout
    (lambda (process)
      (pcheck ([process? process]) ($process-stdout process))))

  #|proc:process-stderr
  The `process-stderr` procedure returns the stderr port for `process`, or `#f`.
  The `process` parameter is a process object.
  |#
  (define process-stderr
    (lambda (process)
      (pcheck ([process? process]) ($process-stderr process))))

  (define make-process
    (lambda (pid command arguments stdin stdout stderr status)
      (%make-process pid command arguments stdin stdout stderr status (make-mutex))))

  #|proc:process-status
  The `process-status` procedure returns the cached exit status of `process`, or `#f`.
  The `process` argument is a process object. This does not poll the operating system.
  |#
  (define process-status
    (lambda (process)
      (pcheck ([process? process])
        (with-mutex (process-mutex process) ($process-status process)))))

  (define $os-string?
    (lambda (text)
      (and (string? text)
           (let loop ([index 0])
             (or (fx= index (string-length text))
                 (and (not (char=? (string-ref text index) #\nul))
                      (loop (fx+ index 1))))))))

  (define $string-list?
    (lambda (object)
      (and (list? object) (andmap $os-string? object))))

  (define $option-alist?
    (lambda (object)
      (list? object)))

  (define $option-ref
    (lambda (options key default)
      (let ([entry (assq key options)])
        (if entry (cdr entry) default))))

  (define $env-option
    (lambda (options)
      (let ([env ($option-ref options 'env #f)])
        (if env env #f))))

  (define $cwd-option
    (lambda (options)
      (let ([cwd ($option-ref options 'cwd #f)])
        (if cwd cwd ""))))

  (define $null-mode?
    (lambda (options key)
      (eq? 'null ($option-ref options key 'inherit))))

  (define $spawn-process-ffi
    (foreign-procedure "cspp_spawn_process" (ptr ptr utf-8 int int int) ptr))

  (define $waitpid-ffi
    (foreign-procedure "cspp_waitpid" (int int) ptr))

  (define $send-signal-ffi
    (foreign-procedure "cspp_send_signal" (int int) ptr))

  (define $make-pipe-ffi
    (foreign-procedure "cspp_make_pipe" () ptr))

  (define $spawn-pipeline-ffi
    (foreign-procedure "cspp_spawn_pipeline" (ptr) ptr))

  (define $spawn-pipeline-capture-ffi
    (foreign-procedure "cspp_spawn_pipeline_capture" (ptr int) ptr))

  (define $wait-status->exit-status
    (lambda (raw)
      (cond [(fx= (fxand raw #x7f) 0)
             (make-process-exit-status 'exit (fxsrl raw 8))]
            [(fx= (fxand raw #x7f) #x7f)
             (make-process-exit-status 'stopped (fxsrl raw 8))]
            [else
             (make-process-exit-status 'signal (fxand raw #x7f))])))

  (define $validate-process-options
    (lambda (options live?)
      (let ([seen '()])
      (for-each
        (lambda (entry)
          (unless (and (pair? entry) (symbol? (car entry)))
            (errorf 'process "invalid option entry: ~s" entry))
          (let ([key (car entry)] [value (cdr entry)])
            (when (memq key seen) (errorf 'process "duplicate option: ~s" key))
            (set! seen (cons key seen))
            (unless
              (case key
                [(cwd) (or (not value) ($os-string? value))]
                [(env) (or (not value)
                           (and (list? value)
                                (andmap (lambda (pair) (and (pair? pair) ($os-string? (car pair))
                                                           ($os-string? (cdr pair)))) value)))]
                [(env-mode) (eq? value 'replace)]
                [(stdin) (if live? (memq value '(inherit null pipe))
                              (or (not value) (memq value '(inherit null))
                             (string? value) (bytevector? value)))]
                [(stdout) (memq value (if live? '(inherit null pipe) '(inherit null capture)))]
                [(stderr) (memq value (if live? '(inherit null pipe stdout)
                                                '(inherit null capture stdout)))]
                [(encoding) (and (not live?) (memq value '(utf-8 binary)))]
                [(timeout) (and (not live?) (or (not value) (and (integer? value) (exact? value)
                                              (<= 0 value #x7fffffff))))]
                [(success) (and (not live?) (or (not value) (procedure? value)
                               (and (list? value)
                                    (andmap (lambda (code) (and (fixnum? code)
                                                               (<= 0 code 255))) value))))]
                [else #f])
              (errorf 'process "invalid ~s option: ~s" key value))))
        options))))

  (define $stream-mode
    (lambda (options key)
      (case ($option-ref options key 'inherit)
        [(inherit) 0] [(null) 1] [(pipe) 2] [(stdout) 3])))

  #|proc:spawn-process
  The `spawn-process` procedure starts executable string `program` with string-list `arguments`.
  It returns a process object. `options` is an alist; `cwd` is a directory string or `#f`,
  and `env` is a string-pair alist or `#f` to inherit. An empty `env` clears the environment.
  `stdin`, `stdout`, and `stderr` accept `inherit` (default), `null`, or `pipe`.
  `stderr` also accepts `stdout` to merge it into the child's stdout.
  A piped stdin is exposed as a binary output port; piped outputs as binary input ports.
  Callers own these ports and must close them, including stdin to deliver EOF to the child.
  Waiting does not close ports. Concurrent waiters share one cached exit status.
  |#
  (define-who spawn-process
    (lambda (program arguments options)
      (pcheck ([$os-string? program] [$string-list? arguments] [$option-alist? options])
        ($validate-process-options options #t)
        (let* ([argv (cons program arguments)]
               [result (ffi-result-ref
                         ($spawn-process-ffi argv ($env-option options) ($cwd-option options)
                           ($stream-mode options 'stdin) ($stream-mode options 'stdout)
                           ($stream-mode options 'stderr)))]
               [pid (vector-ref result 0)] [ports '()])
          (guard (condition
                   [else
                    (for-each close-port ports)
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

  #|proc:spawn-shell-command
The `spawn-shell-command` procedure starts `command` through the host shell.
The `command` parameter is interpreted by the host shell.
The `options` parameter is an option alist accepted by `spawn-process`.
|#
  (define-who spawn-shell-command
    (lambda (command options)
      (pcheck ([string? command] [$option-alist? options])
              (spawn-process "sh" (list "-c" command) options))))

  (define $wait-vector->status
    (lambda (result)
      (if result
          ($wait-status->exit-status (vector-ref result 1))
          #f)))

  #|proc:process-wait/no-hang
  The `process-wait/no-hang` procedure returns `process`'s exit status, or `#f` if still running.
  The `process` argument is a process object. Reaping and status publication are synchronized.
  |#
  (define process-wait/no-hang
    (lambda (process)
      (pcheck ([process? process])
        (with-mutex (process-mutex process)
          (or ($process-status process)
              (let ([status ($wait-vector->status
                              (ffi-result-ref ($waitpid-ffi (process-pid process) 1)))])
                (when status (set-process-status! process status))
                status))))))

  #|proc:process-wait
  The `process-wait` procedure waits for `process` and returns its cached exit status.
  The `process` argument is a process object. Waiting leaves its streams open for draining.
  |#
  (define process-wait
    (lambda (process)
      (pcheck ([process? process])
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
      (pcheck ([process? process] [natural? milliseconds])
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
  (define process-running?
    (lambda (process)
      (pcheck ([process? process])
              (not (process-wait/no-hang process)))))

  #|proc:process-kill
The `process-kill` procedure sends signal number `signal-number` to `process`.
The `process` parameter is a process object.
The `signal-number` parameter is an exact positive signal number.
  A process with a cached or newly observed exit status is left alone.
|#
  (define-who process-kill
    (lambda (process signal-number)
      (pcheck ([process? process]
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
|#
  (define process-terminate
    (lambda (process)
      (process-kill process 15)))

  #|proc:process-interrupt
The `process-interrupt` procedure sends `SIGINT` to `process`.
The `process` parameter is a process object.
|#
  (define process-interrupt
    (lambda (process)
      (process-kill process 2)))

  #|proc:process-close-ports!
The `process-close-ports!` procedure closes any open ports owned by `process`.
  Repeated calls are harmless. This does not terminate or wait for the child.
The `process` parameter is a process object.
|#
  (define process-close-ports!
    (lambda (process)
      (pcheck ([process? process])
              (for-each (lambda (port)
                          (when (and (port? port) (not (port-closed? port)))
                            (close-port port)))
                        (list (process-stdin process)
                              (process-stdout process)
                              (process-stderr process))))))

  #|proc:make-pipe
The `make-pipe` procedure returns input and output ports backed by an OS pipe.
|#
  (define make-pipe
    (lambda ()
      (let ([fds (ffi-result-ref ($make-pipe-ffi))])
        (values (open-fd-input-port (vector-ref fds 0) 'block #f)
                (open-fd-output-port (vector-ref fds 1) 'block #f)))))

  #|proc:pipe-processes
The `pipe-processes` procedure starts process specs as a live pipeline.
The `process-specs` parameter is a list of nonempty string lists.
The `options` parameter is reserved and should currently be the empty list.
The procedure returns process objects immediately and does not wait for them.
Callers should wait for or terminate the returned processes.
|#
  (define pipe-processes
    (lambda (process-specs options)
      (pcheck ([list? process-specs] [list? options])
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

  #|proc:run-pipeline
The `run-pipeline` procedure runs process specs as a pipeline and waits.
The `process-specs` parameter is a list of nonempty string lists.
Unlike `pipe-processes`, this procedure does not return live process objects.
It returns a list containing the final process exit status.
|#
  (define run-pipeline
    (lambda (process-specs)
      (pcheck ([list? process-specs])
              (let ([result (ffi-result-ref ($spawn-pipeline-capture-ffi process-specs -1))])
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
|#
  (define-who fork
    (let ([ffi (foreign-procedure "cspp_fork" () ptr)])
      (lambda ()
        (let ([result (ffi)])
          (if (string? result) ($err-process who result) result)))))

  #|proc:vfork
The `vfork` procedure performs a raw operating-system vfork.
This expert API is unsafe unless the child immediately execs or exits.
|#
  (define-who vfork
    (let ([ffi (foreign-procedure "cspp_vfork" () ptr)])
      (lambda ()
        (let ([result (ffi)])
          (if (string? result) ($err-process who result) result)))))

  #|proc:getpid
The `getpid` procedure returns the current process id.
|#
  (define getpid get-process-id)

  #|proc:gettid
The `gettid` procedure returns the current thread id.
|#
  (define gettid get-thread-id)

  #|proc:getppid
The `getppid` procedure returns the parent process id.
|#
  (define getppid (foreign-procedure "cspp_getppid" () int))

)
