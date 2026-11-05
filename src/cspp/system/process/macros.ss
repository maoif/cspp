(library (cspp system process macros)
  (export process-run process-run/check process-capture process-capture/check
          process-run-shell process-capture-shell)
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
                     [(process-run)
                      #'(%process-run 'process-run command-expr (list argument ...) (list option ...) check)]
                     [(process-capture)
                      #'(%process-capture 'process-capture command-expr (list argument ...) (list option ...) check)]
                     [(process-run-shell)
                      (unless (null? arguments)
                        (syntax-error stx "process-run-shell accepts one command expression"))
                      #'(%process-run-shell 'process-run-shell command-expr (list option ...) check)]
                     [(process-capture-shell)
                      (unless (null? arguments)
                        (syntax-error stx "process-capture-shell accepts one command expression"))
                      #'(%process-capture-shell 'process-capture-shell command-expr (list option ...) check)]
                     [else
                      (syntax-error #'kind "unknown process macro kind")])))))]))))

  #|macro:process-run
  The `process-run` macro runs `program` with arguments and returns an exit status.
  Bare identifiers in `program` and arguments are literal command words.
  Other forms are expressions; use `(begin argument)` to pass the value of a variable.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `inherit` (default), `null`, or `capture`; captures are discarded.
  The `:stderr` option accepts `inherit` (default), `null`, `capture`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, any status is accepted. A code list accepts only normal exits.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-run
    (syntax-rules ()
      [(_ form ...) ($process-form process-run #f form ...)]))

  #|macro:process-run/check
  The `process-run/check` macro runs `program` and raises on an unacceptable exit.
  Bare identifiers in `program` and arguments are literal command words.
  Other forms are expressions; use `(begin argument)` to pass the value of a variable.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `inherit` (default), `null`, or `capture`; captures are discarded.
  The `:stderr` option accepts `inherit` (default), `null`, `capture`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, only exit code zero is accepted. `#f` accepts any status.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-run/check
    (syntax-rules ()
      [(_ form ...) ($process-form process-run #t form ...)]))

  #|macro:process-capture
  The `process-capture` macro runs `program` with arguments and returns a result.
  Bare identifiers in `program` and arguments are literal command words.
  Other forms are expressions; use `(begin argument)` to pass the value of a variable.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `capture` (default), `inherit`, or `null`.
  The `:stderr` option accepts `capture` (default), `inherit`, `null`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, any status is accepted. A code list accepts only normal exits.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-capture
    (syntax-rules ()
      [(_ form ...) ($process-form process-capture #f form ...)]))

  #|macro:process-capture/check
  The `process-capture/check` macro runs `program` and raises on an unacceptable exit.
  Bare identifiers in `program` and arguments are literal command words.
  Other forms are expressions; use `(begin argument)` to pass the value of a variable.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `capture` (default), `inherit`, or `null`.
  The `:stderr` option accepts `capture` (default), `inherit`, `null`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, only exit code zero is accepted. `#f` accepts any status.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-capture/check
    (syntax-rules ()
      [(_ form ...) ($process-form process-capture #t form ...)]))

  #|macro:process-run-shell
  The `process-run-shell` macro runs `command` through the host shell.
  A bare `command` identifier is literal text; other forms evaluate to a command string.
  Use `(begin command)` for a variable, followed by optional keyword clauses.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `inherit` (default), `null`, or `capture`; captures are discarded.
  The `:stderr` option accepts `inherit` (default), `null`, `capture`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, any status is accepted. A code list accepts only normal exits.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-run-shell
    (syntax-rules ()
      [(_ form ...) ($process-form process-run-shell #f form ...)]))

  #|macro:process-capture-shell
  The `process-capture-shell` macro runs `command` through the host shell.
  A bare `command` identifier is literal text; other forms evaluate to a command string.
  Use `(begin command)` for a variable, followed by optional keyword clauses.
  The `:cwd` option accepts a nonempty working-directory string or `#f`.
  The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
  Environment names are nonempty and contain no `=`; OS strings contain no NUL characters.
  The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
  The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`; default `null`.
  Payload strings are UTF-8 encoded and may contain NUL, as may binary payloads.
  The `:stdout` option accepts `capture` (default), `inherit`, or `null`.
  The `:stderr` option accepts `capture` (default), `inherit`, `null`, or `stdout` to merge.
  The `:encoding` option selects captured output: `utf-8` strings (default) or `binary` bytevectors.
  The `:timeout` option accepts `#f` (default) or an exact integer from 0 to 2147483647 ms.
  The timeout covers I/O and waiting after spawn. Cleanup terminates and reaps the immediate child.
  Descendants are not terminated as a group; see `docs/system.md` for timeout boundaries.
  The `:success` option accepts `#f`, a list of exit codes from 0 to 255, or a callback.
  Its callback has signature `exit-status -> object`; a nonfalse result accepts the exit.
  It runs once after completion; callback errors propagate. Rejection raises a system exit error.
  Without `:success`, any status is accepted. A code list accepts only normal exits.
  Bare `capture`, `inherit`, `null`, `stdout`, `replace`, `binary`, and `utf-8` are symbols.
  |#
  (define-syntax process-capture-shell
    (syntax-rules ()
      [(_ form ...) ($process-form process-capture-shell #f form ...)]))

  )
