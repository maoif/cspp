(library (cspp system process backend)
  (export process-result? process-result-status process-result-stdout process-result-stderr process-result-pid process-result-command
          %run-process %capture-process %run-shell-command %capture-shell-command capture-pipeline)
  (import (cspp chez) (cspp system common) (cspp system process expert) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; process result records
;;;;===----------------------------------------------------------------------===

  #|proc:process-result?
  The `process-result?` procedure returns `#t` when its argument is a process result record,
  otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|proc:process-result-status
  The `process-result-status` procedure returns the process exit-status record stored in `result`.
  The `result` parameter is a process result record.
  |#
  #|proc:process-result-stdout
  The `process-result-stdout` procedure returns the captured standard output string stored in
  `result`, or `#f` when output was not requested.
  The `result` parameter is a process result record.
  |#
  #|proc:process-result-stderr
  The `process-result-stderr` procedure returns the captured standard error string stored in
  `result`, or `#f` when error output was not requested.
  The `result` parameter is a process result record.
  |#
  #|proc:process-result-pid
  The `process-result-pid` procedure returns the process id stored in `result`, or `#f` when no
  process id is available.
  The `result` parameter is a process result record.
  |#
  #|proc:process-result-command
  The `process-result-command` procedure returns the shell command string stored in `result`.
  The `result` parameter is a process result record.
  |#
  (define-record-type ($process-result make-process-result process-result?)
    (nongenerative)
    (fields (immutable status process-result-status)
            (immutable stdout process-result-stdout)
            (immutable stderr process-result-stderr)
            (immutable pid process-result-pid)
            (immutable command process-result-command)))

;;;;===----------------------------------------------------------------------===
;;;; process backend
;;;;===----------------------------------------------------------------------===

  (define $string-list?
    (lambda (x)
      (and (list? x) (andmap string? x))))

  (define $option-alist?
    (lambda (x)
      (list? x)))

  (define $option-ref
    (lambda (options key default)
      (let ([a (assq key options)])
        (if a (cdr a) default))))

  (define $raise-exit-error
    (lambda (who command status)
      (raise (make-system-exit-error who
                                     (format "process exited with status ~a" (process-exit-status-code status))
                                     `((command . ,command)
                                       (status-kind . ,(process-exit-status-kind status))
                                       (status-code . ,(process-exit-status-code status)))))))

  (define $check-status
    (lambda (who command status)
      (unless (process-exit-success? status)
        ($raise-exit-error who command status))))

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

  (define $wait-status->exit-status
    (lambda (raw)
      (cond [(fx= (fxand raw #x7f) 0)
             (make-process-exit-status 'exit (fxsrl raw 8))]
            [(fx= (fxand raw #x7f) #x7f)
             (make-process-exit-status 'stopped (fxsrl raw 8))]
            [else
             (make-process-exit-status 'signal (fxand raw #x7f))])))

  (define $stdin-payload
    (lambda (options)
      (let ([stdin ($option-ref options 'stdin 'null)])
        (cond [(eq? stdin 'inherit) 'inherit]
              [(or (not stdin) (eq? stdin 'null)) #f]
              [(string? stdin) (string->utf8 stdin)]
              [(bytevector? stdin) stdin]
              [else (errorf 'process "unsupported stdin option: ~a" stdin)]))))

  (define $env-option
    (lambda (options)
      (let ([env ($option-ref options 'env #f)])
        (if env env #f))))

  (define $cwd-option
    (lambda (options)
      (let ([cwd ($option-ref options 'cwd #f)])
        (if cwd cwd ""))))

  (define $timeout-option
    (lambda (options)
      (let ([timeout ($option-ref options 'timeout #f)])
        (if timeout timeout -1))))

  (define $capture-stdout?
    (lambda (options default)
      (eq? 'capture ($option-ref options 'stdout default))))

  (define $capture-stderr?
    (lambda (options default)
      (eq? 'capture ($option-ref options 'stderr default))))

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
                       (ptr ptr string ptr int int int int int int)
                       ptr))

  (define $spawn-pipeline-capture-ffi
    (foreign-procedure "cspp_spawn_pipeline_capture" (ptr int) ptr))

  (define $decode-output
    (lambda (bytes options)
      (let ([encoding ($option-ref options 'encoding 'utf-8)])
        (if (eq? encoding 'binary) bytes (utf8->string bytes)))))

  (define $validate-options
    (lambda (options)
      (for-each
        (lambda (entry)
          (unless (and (pair? entry) (symbol? (car entry)))
            (errorf 'process "invalid option entry: ~s" entry))
          (let ([key (car entry)] [value (cdr entry)])
            (unless
              (case key
                [(cwd) (or (not value) (string? value))]
                [(env) (or (not value)
                           (and (list? value)
                                (andmap (lambda (pair) (and (pair? pair) (string? (car pair))
                                                           (string? (cdr pair)))) value)))]
                [(env-mode) (eq? value 'replace)]
                [(stdin) (or (not value) (memq value '(inherit null))
                             (string? value) (bytevector? value))]
                [(stdout) (memq value '(inherit null capture))]
                [(stderr) (memq value '(inherit null capture stdout))]
                [(encoding) (memq value '(utf-8 binary))]
                [(timeout) (or (not value) (and (integer? value) (exact? value)
                                              (<= 0 value #x7fffffff)))]
                [(success) (or (not value) (procedure? value)
                               (and (list? value)
                                    (andmap (lambda (code) (and (fixnum? code)
                                                               (<= 0 code 255))) value)))]
                [else #f])
              (errorf 'process "invalid ~s option: ~s" key value))))
        options)))

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
      ($validate-options options)
      (ffi-result-ref
       ($spawn-capture-ffi argv
                           ($env-option options)
                           ($cwd-option options)
                           ($stdin-payload options)
                           (if ($capture-stdout? options capture-default) 1 0)
                           (if ($capture-stderr? options capture-default) 1 0)
                           (if ($null-stdout? options) 1 0)
                           (if ($null-stderr? options) 1 0)
                           (if ($stderr-to-stdout? options) 1 0)
                           ($timeout-option options)))))

  (define %run-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              ($run-vector->status program
                                  ($spawn-capture (cons program arguments) options 'inherit)
                                  options
                                  check?
                                  who))))

  (define %capture-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              ($capture-vector->result program
                                      ($spawn-capture (cons program arguments) options 'capture)
                                      options
                                      check?
                                      who))))

  (define %run-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              ($run-vector->status command
                                  ($spawn-capture (list "sh" "-c" command) options 'inherit)
                                  options
                                  check?
                                  who))))

  (define %capture-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              ($capture-vector->result command
                                      ($spawn-capture (list "sh" "-c" command) options 'capture)
                                      options
                                      check?
                                      who))))

  #|proc:capture-pipeline
  The `capture-pipeline` procedure runs string-list process specs as a pipeline.
  The `process-specs` parameter is a list of nonempty string lists.
  |#
  (define capture-pipeline
    (lambda (process-specs)
      (pcheck ([list? process-specs])
              (let ([result (ffi-result-ref ($spawn-pipeline-capture-ffi process-specs -1))])
                ($capture-vector->result 'pipeline result '((stdout . capture)) #f
                                        'capture-pipeline)))))


)
