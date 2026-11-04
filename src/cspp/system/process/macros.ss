(library (cspp system process macros)
  (export run-process run-process/check capture-process capture-process/check
          shell-command capture-shell-command)
  (import (cspp chez) (cspp system process backend))

;;;;===----------------------------------------------------------------------===
;;;; process macros
;;;;===----------------------------------------------------------------------===

  (define-syntax $process-form
    (let ()
      (define known-option-keywords
        '(:cwd :env :env-mode :stdin :stdout :stderr :encoding :timeout :success))
      (define literal-option-values
        '(capture inherit null stdout replace binary utf-8))
      (define colon-symbol?
        (lambda (x)
          (and (symbol? x)
               (let ([s (symbol->string x)])
                 (and (< 0 (string-length s))
                      (char=? #\: (string-ref s 0)))))))
      (define known-option-keyword?
        (lambda (x)
          (and (symbol? x) (memq x known-option-keywords))))
      (define option-key
        (lambda (x)
          (let* ([s (symbol->string x)]
                 [n (string-length s)])
            (string->symbol (substring s 1 n)))))
      (define parse-value
        (lambda (stx)
          (let ([datum (syntax->datum stx)])
            (if (and (symbol? datum) (memq datum literal-option-values))
                #`'#,(datum->syntax stx datum)
                stx))))
      (define parse-command-word
        (lambda (stx)
          (if (identifier? stx)
              #`'#,(datum->syntax stx (symbol->string (syntax->datum stx)))
              stx)))
      (define parse-forms
        (lambda (stx forms)
          (let parse-args ([rest forms] [args '()])
            (if (null? rest)
                (if (null? args)
                    (syntax-error stx "missing process command")
                    (values (reverse args) '()))
                (let ([datum (syntax->datum (car rest))])
                  (cond [(known-option-keyword? datum)
                         (values (reverse args)
                                 (parse-options stx rest '() '()))]
                        [(colon-symbol? datum)
                         (syntax-error (car rest) "unknown process option keyword")]
                        [else
                         (parse-args (cdr rest) (cons (car rest) args))]))))))
      (define parse-options
        (lambda (stx rest seen out)
          (if (null? rest)
              (reverse out)
              (let ([kw-stx (car rest)])
                (let ([kw (syntax->datum kw-stx)])
                  (cond [(not (known-option-keyword? kw))
                         (if (colon-symbol? kw)
                             (syntax-error kw-stx "unknown process option keyword")
                             (syntax-error kw-stx "expected process option keyword"))]
                        [(memq kw seen)
                         (syntax-error kw-stx "duplicate process option keyword")]
                        [(null? (cdr rest))
                         (syntax-error kw-stx "missing process option value")]
                        [else
                         (parse-options stx
                                        (cddr rest)
                                        (cons kw seen)
                                        (cons #`(cons '#,(datum->syntax kw-stx (option-key kw))
                                                      #,(parse-value (cadr rest)))
                                              out))]))))))
      (lambda (stx)
        (syntax-case stx ()
          [(_ kind check? form ...)
           (let ([kind (syntax->datum #'kind)]
                 [check? (syntax->datum #'check?)]
                 [forms (syntax->list #'(form ...))])
             (unless forms
               (syntax-error stx "invalid process form"))
             (let-values ([(command+args option-forms) (parse-forms stx forms)])
               (when (null? command+args)
                 (syntax-error stx "missing process command"))
               (let ([command (car command+args)]
                     [arguments (cdr command+args)])
                 (with-syntax ([command command]
                               [command-expr (parse-command-word command)]
                               [(argument ...) (map parse-command-word arguments)]
                               [(option ...) option-forms]
                               [check check?])
                   (case kind
                     [(run-process)
                      #'(%run-process 'run-process command-expr (list argument ...) (list option ...) check)]
                     [(capture-process)
                      #'(%capture-process 'capture-process command-expr (list argument ...) (list option ...) check)]
                     [(shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "shell-command accepts one command expression"))
                      #'(%run-shell-command 'shell-command command-expr (list option ...) check)]
                     [(capture-shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "capture-shell-command accepts one command expression"))
                      #'(%capture-shell-command 'capture-shell-command command-expr (list option ...) check)]
                     [else
                      (syntax-error #'kind "unknown process macro kind")])))))]))))

  #|macro:run-process
  The `run-process` macro runs `program` with arguments and returns an exit status.
  The `program` form and argument forms are expressions before any option keyword.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
  The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax run-process
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #f form ...)]))

  #|macro:run-process/check
  The `run-process/check` macro runs `program` and raises on an unacceptable exit.
  The `program` form and argument forms are expressions before any option keyword.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
  The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax run-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #t form ...)]))

  #|macro:capture-process
  The `capture-process` macro runs `program` with arguments and returns a result.
  The `program` form and argument forms are expressions before any option keyword.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `capture`, `inherit`, or `null`.
  The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax capture-process
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #f form ...)]))

  #|macro:capture-process/check
  The `capture-process/check` macro runs `program` and raises on an unacceptable exit.
  The `program` form and argument forms are expressions before any option keyword.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `capture`, `inherit`, or `null`.
  The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax capture-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #t form ...)]))

  #|macro:shell-command
  The `shell-command` macro runs `command` through the host shell.
  The `command` form is an expression followed by optional keyword clauses.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
  The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form shell-command #f form ...)]))

  #|macro:capture-shell-command
  The `capture-shell-command` macro runs `command` through the host shell.
  The `command` form is an expression followed by optional keyword clauses.
  The `:cwd` option accepts a string working directory or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
  The `:stdout` option accepts `capture`, `inherit`, or `null`.
  The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
  The `:encoding` option is `utf-8` (default) for strings or `binary` for bytevectors.
  The `:timeout` option accepts `#f` or an exact millisecond timeout.
  The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
  Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
  |#
  (define-syntax capture-shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form capture-shell-command #f form ...)]))

  )
