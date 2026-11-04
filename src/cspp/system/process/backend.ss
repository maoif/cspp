(library (cspp system process backend)
  (export
          ;; completed command results
          process-result? process-result-status process-result-stdout process-result-stderr
          process-result-pid process-result-command
          process-capture-pipeline

          ;; implementation entry points for command macros
          %process-run %process-capture %process-run-shell %process-capture-shell)
  (import (cspp chez) (cspp system common) (cspp system private)
          (cspp system process status) (cspp system process private) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; process result records
;;;;===----------------------------------------------------------------------===

  #|proc:process-result?
  The `process-result?` procedure returns `#t` when its argument is a process result record,
  otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:$process-result
  A process-result record stores completed status, stdout/stderr captures, pid, and command.
  The exported `process-result?` predicate accepts any object and returns a boolean.
  Captures are UTF-8 strings or binary bytevectors; uncaptured streams are `#f`.
  |#
  (define-record-type ($process-result make-process-result process-result?)
    (nongenerative)
    (fields (immutable status $process-result-status)
            (immutable stdout $process-result-stdout)
            (immutable stderr $process-result-stderr)
            (immutable pid $process-result-pid)
            (immutable command $process-result-command)))

  #|proc:process-result-status
  The `process-result-status` procedure returns the process exit-status record stored in `result`.
  The `result` parameter is a process result record.
  |#
  (define process-result-status
    (lambda (result)
      (pcheck ([process-result? result]) ($process-result-status result))))

  #|proc:process-result-stdout
  The `process-result-stdout` procedure returns captured standard output as a string or bytevector,
  or `#f` when output was not requested.
  The `result` parameter is a process result record.
  |#
  (define process-result-stdout
    (lambda (result)
      (pcheck ([process-result? result]) ($process-result-stdout result))))

  #|proc:process-result-stderr
  The `process-result-stderr` procedure returns captured standard error as a string or bytevector,
  or `#f` when error output was not requested.
  The `result` parameter is a process result record.
  |#
  (define process-result-stderr
    (lambda (result)
      (pcheck ([process-result? result]) ($process-result-stderr result))))

  #|proc:process-result-pid
  The `process-result-pid` procedure returns the process id stored in `result`, or `#f` when no
  process id is available.
  The `result` parameter is a process result record.
  |#
  (define process-result-pid
    (lambda (result)
      (pcheck ([process-result? result]) ($process-result-pid result))))

  #|proc:process-result-command
  The `process-result-command` procedure returns the program or shell-command string in `result`.
  Pipeline results instead store the symbol `pipeline`.
  The `result` parameter is a process result record.
  |#
  (define process-result-command
    (lambda (result)
      (pcheck ([process-result? result]) ($process-result-command result))))

;;;;===----------------------------------------------------------------------===
;;;; process backend
;;;;===----------------------------------------------------------------------===

  (define $raise-exit-error
    (lambda (who command status)
      (raise (make-system-exit-error who
                                     (format "process exited with status ~a" (process-exit-status-code status))
                                     `((command . ,command)
                                       (status-kind . ,(process-exit-status-kind status))
                                       (status-code . ,(process-exit-status-code status)))))))

  (define $status-accepted?
    (lambda (status success)
      (cond [(not success) #t]
            [(procedure? success) (success status)]
            [(list? success)
             (and (eq? 'exit (process-exit-status-kind status))
                  (memv (process-exit-status-code status) success))]
            [else #f])))

  (define $check-accepted-status
    (lambda (who command status options check?)
      (let ([success ($option-ref options 'success (if check? '(0) #f))])
        (unless ($status-accepted? status success)
          ($raise-exit-error who command status)))))

  (define $stdin-payload
    (lambda (options)
      (let ([stdin ($option-ref options 'stdin 'null)])
        (cond [(eq? stdin 'inherit) 'inherit]
              [(or (not stdin) (eq? stdin 'null)) #f]
              [(string? stdin) (string->utf8 stdin)]
              [(bytevector? stdin) stdin]
              [else (errorf 'process "unsupported stdin option: ~a" stdin)]))))

  (define $timeout-option
    (lambda (options)
      (let ([timeout ($option-ref options 'timeout #f)])
        (if timeout timeout -1))))

  ;; Native capture modes: 0 leaves the stream unpiped, 1 retains bytes for a result,
  ;; and 2 drains the pipe without retaining bytes for a status-only run.
  (define $capture-mode
    (lambda (options key default)
      (if (eq? 'capture ($option-ref options key default))
          (if (eq? default 'capture) 1 2)
          0)))

  (define $null-stdout?
    (lambda (options)
      (eq? 'null ($option-ref options 'stdout 'inherit))))

  (define $null-stderr?
    (lambda (options)
      (eq? 'null ($option-ref options 'stderr 'inherit))))

  (define $stderr-to-stdout?
    (lambda (options)
      (eq? 'stdout ($option-ref options 'stderr 'inherit))))

  (define $spawn-capture-ffi
    (foreign-procedure "cspp_spawn_capture"
                       (ptr ptr utf-8 ptr int int int int int int)
                       ptr))

  (define $spawn-pipeline-capture-ffi
    (foreign-procedure "cspp_spawn_pipeline_capture" (ptr int) ptr))

  (define $decode-output
    (lambda (bytes options)
      (let ([encoding ($option-ref options 'encoding 'utf-8)])
        (if (eq? encoding 'binary) bytes (utf8->string bytes)))))

  (define $capture-vector->result
    (lambda (command result options check? who)
      (let* ([pid (vector-ref result 0)]
             [status ($wait-status->exit-status (vector-ref result 1))]
             [stdout-mode ($option-ref options 'stdout 'capture)]
             [stderr-mode ($option-ref options 'stderr 'capture)])
        ($check-accepted-status who command status options check?)
        (make-process-result status
                             (if (eq? stdout-mode 'capture) ($decode-output (vector-ref result 2) options) #f)
                             (if (eq? stderr-mode 'capture) ($decode-output (vector-ref result 3) options) #f)
                             pid
                             command))))

  (define $run-vector->status
    (lambda (command result options check? who)
      (let ([status ($wait-status->exit-status (vector-ref result 1))])
        ($check-accepted-status who command status options check?)
        status)))

  (define $spawn-capture
    (lambda (argv options capture-default)
      ($validate-process-options options #f)
      (ffi-result-ref
       ($spawn-capture-ffi argv
                           ($option-ref options 'env #f)
                           ($cwd-option options)
                           ($stdin-payload options)
                           ($capture-mode options 'stdout capture-default)
                           ($capture-mode options 'stderr capture-default)
                           (if ($null-stdout? options) 1 0)
                           (if ($null-stderr? options) 1 0)
                           (if ($stderr-to-stdout? options) 1 0)
                           ($timeout-option options)))))

  #|proc:%process-run
  The internal `%process-run` procedure implements direct command forms and returns an exit status.
  `who` is the error-origin symbol; `program` is a NUL-free executable string; `arguments` is a
  proper list of NUL-free strings. `options` is the command option alist documented in
  `process-run`.
  Boolean `check?` chooses whether exit code zero is required when no `success` option is supplied.
  |#
  (define %process-run
    (lambda (who program arguments options check?)
      (pcheck ([symbol? who] [$os-string? program] [$os-string-list? arguments]
               [list? options] [boolean? check?])
              ($run-vector->status program
                                  ($spawn-capture (cons program arguments) options 'inherit)
                                  options
                                  check?
                                  who))))

  #|proc:%process-capture
  The internal `%process-capture` procedure runs a direct command and returns a process result.
  `who` is the error-origin symbol; `program` is a NUL-free executable string; `arguments` is a
  proper list of NUL-free strings. `options` follows `process-capture`, including stream encoding.
  Boolean `check?` chooses whether exit code zero is required when `success` is omitted.
  |#
  (define %process-capture
    (lambda (who program arguments options check?)
      (pcheck ([symbol? who] [$os-string? program] [$os-string-list? arguments]
               [list? options] [boolean? check?])
              ($capture-vector->result program
                                      ($spawn-capture (cons program arguments) options 'capture)
                                      options
                                      check?
                                      who))))

  #|proc:%process-run-shell
  The internal `%process-run-shell` procedure runs `sh -c` and returns an exit status.
  `who` is the error-origin symbol; `command` is a NUL-free shell-command string.
  `options` follows `process-run-shell`; boolean `check?` selects the default zero-exit requirement.
  A supplied `success` option overrides that requirement.
  |#
  (define %process-run-shell
    (lambda (who command options check?)
      (pcheck ([symbol? who] [$os-string? command] [list? options] [boolean? check?])
              ($run-vector->status command
                                  ($spawn-capture (list "sh" "-c" command) options 'inherit)
                                  options
                                  check?
                                  who))))

  #|proc:%process-capture-shell
  The internal `%process-capture-shell` procedure runs `sh -c` and returns a process result.
  `who` is the error-origin symbol; `command` is a NUL-free shell-command string.
  `options` follows `process-capture-shell`; boolean `check?` selects a default zero-exit check.
  A supplied `success` option overrides that requirement.
  |#
  (define %process-capture-shell
    (lambda (who command options check?)
      (pcheck ([symbol? who] [$os-string? command] [list? options] [boolean? check?])
              ($capture-vector->result command
                                      ($spawn-capture (list "sh" "-c" command) options 'capture)
                                      options
                                      check?
                                      who))))

  #|proc:process-capture-pipeline
  The `process-capture-pipeline` procedure runs string-list process specs as a pipeline.
  The `process-specs` parameter is a nonempty proper list of nonempty proper string lists.
  Command strings must contain no NUL characters. Returns a process result with captured stdout.
  Stdin and stderr are inherited. The result's stderr field is `#f`, and its command is `pipeline`.
  The status and pid belong to the final command; all children are waited for before returning.
  |#
  (define process-capture-pipeline
    (lambda (process-specs)
      (pcheck ([$process-specs? process-specs])
              (let ([result (ffi-result-ref ($spawn-pipeline-capture-ffi process-specs -1))])
                ($capture-vector->result 'pipeline result '((stdout . capture) (stderr . inherit)) #f
                                        'process-capture-pipeline)))))

)
