(library (cspp cli)
  (export run-cli-command!
          cli-command cli-option cli-option-group cli-enum
          dump-command

          make-command command?
          command-name
          command-help
          command-overview
          command-version
          command-author
          command-copyright
          command-about
          command-style
          command-category
          command-exec
          command-multi-personality?
          command-maybe-no-subcommand?
          command-flatten-help?
          command-trace?
          command-quit?
          command-options
          command-subcommands

          command-name-set!
          command-help-set!
          command-overview-set!
          command-version-set!
          command-author-set!
          command-copyright-set!
          command-about-set!
          command-style-set!
          command-category-set!
          command-exec-set!
          command-multi-personality?-set!
          command-maybe-no-subcommand?-set!
          command-flatten-help?-set!
          command-trace?-set!
          command-quit?-set!
          command-options-add!
          command-subcommands-add!

          make-option option?
          option-name
          option-help
          option-short
          option-long
          option-category
          option-default
          option-number
          option-value-number
          option-value-name
          option-value-parser
          option-value-seperator
          option-callback
          option-alias
          option-conflicts
          option-overrides
          option-requires
          option-positional?
          option-sink?
          option-no-short?
          option-no-long?
          option-no-grouping?
          option-hidden?

          option-name-set!
          option-help-set!
          option-short-set!
          option-long-set!
          option-category-set!
          option-default-set!
          option-number-set!
          option-value-number-set!
          option-value-name-set!
          option-value-parser-set!
          option-value-seperator-set!
          option-callback-set!
          option-alias-set!
          option-conflicts-set!
          option-overrides-set!
          option-requires-set!
          option-positional?-set!
          option-sink?-set!
          option-no-short?-set!
          option-no-long?-set!
          option-no-grouping?-set!
          option-hidden?-set!

          make-option-group option-group? option-group-add! option-group-options

          parser-bool
          parser-natural
          parser-integer
          parser-fixnum
          parser-flonum
          parser-string
          parser-enum)
  (import (cspp chez)
          (cspp utils)
          (cspp string)
          (cspp list)
          (cspp io))



;;===----------------------------------------------------------------------===
;; data model
;;===----------------------------------------------------------------------===


  #|proc:make-command
The `make-command` procedure creates a command object named by `name`.
The `name` argument must be a symbol whose string form does not start with `-`.
|#
  (define-record-type command
    (fields (mutable name command-name $command-name-set!)
            (mutable help command-help $command-help-set!)
            (mutable overview command-overview $command-overview-set!)
            (mutable version command-version $command-version-set!)
            (mutable author command-author $command-author-set!)
            (mutable copyright command-copyright $command-copyright-set!)
            (mutable about command-about $command-about-set!)
            (mutable style command-style $command-style-set!)
            (mutable category command-category $command-category-set!)
            (mutable exec command-exec $command-exec-set!)
            (mutable options command-options $command-options-set!)
            (mutable subcommands command-subcommands $command-subcommands-set!)
            (mutable multi-personality? command-multi-personality? $command-multi-personality?-set!)
            (mutable maybe-no-subcommand? command-maybe-no-subcommand? $command-maybe-no-subcommand?-set!)
            (mutable flatten-help? command-flatten-help? $command-flatten-help?-set!)
            (mutable trace? command-trace? $command-trace?-set!)
            (mutable quit? command-quit? $command-quit?-set!))
    (protocol (lambda (n)
                (case-lambda
                  [() (errorf 'make-command "expected command name")]
                  [(name)
                   (pcheck ([symbol? name])
                     (let ([name-str (symbol->string name)])
                       (when (or (string=? name-str "")
                                 (string-prefix? "-" name-str))
                         (errorf 'make-command "invalid command name: ~a" name))
                       (n name-str "" "" "" "" "" "" "" ""
                          #f
                          (make-list-builder)
                          (make-list-builder)
                          #f #f #f #f #f)))]))))

  #|proc:make-option
The `make-option` procedure creates an option object named by `name`.
Options default to pure boolean flag parsing with inferred short and long names.
|#
  (define-record-type option
    (fields (mutable name option-name $option-name-set!)
            (mutable help option-help $option-help-set!)
            (mutable short option-short $option-short-set!)
            (mutable long option-long $option-long-set!)
            (mutable category option-category $option-category-set!)
            (mutable default option-default $option-default-set!)
            (mutable number option-number $option-number-set!)
            (mutable value-number option-value-number $option-value-number-set!)
            (mutable value-name option-value-name $option-value-name-set!)
            (mutable value-parser option-value-parser $option-value-parser-set!)
            (mutable value-seperator option-value-seperator $option-value-seperator-set!)
            (mutable callback option-callback $option-callback-set!)
            (mutable alias option-alias $option-alias-set!)
            (mutable conflicts option-conflicts $option-conflicts-set!)
            (mutable overrides option-overrides $option-overrides-set!)
            (mutable requires option-requires $option-requires-set!)
            (mutable positional? option-positional? $option-positional?-set!)
            (mutable sink? option-sink? $option-sink?-set!)
            (mutable no-short? option-no-short? $option-no-short?-set!)
            (mutable no-long? option-no-long? $option-no-long?-set!)
            (mutable no-grouping? option-no-grouping? $option-no-grouping?-set!)
            (mutable hidden? option-hidden? $option-hidden?-set!))
    (protocol (lambda (n)
                (case-lambda
                  [(name)
                   (pcheck ([symbol? name])
                     (let ([name-str (symbol->string name)])
                       (when (or (string=? name-str "")
                                 (string-prefix? "-" name-str))
                         (errorf 'make-option "invalid option name: ~a" name))
                       (n name
                          ""
                          (string (string-ref name-str 0))
                          name-str
                          ""
                          #f
                          '*
                          0
                          #f
                          parser-bool
                          #\,
                          #f
                          '()
                          '()
                          '()
                          '()
                          #f #f #f #f #f #f)))]))))

  #|proc:make-option-group
The `make-option-group` procedure creates an option group containing `opts`.
Option groups are reusable collections that the macro API can add to commands.
|#
  (define-record-type ($option-group mk-option-group option-group?)
    (fields (mutable options $option-group-options $option-group-options-set!))
    (protocol (lambda (n)
                (lambda () (n (make-list-builder))))))

  (define-who make-option-group
    (lambda opts
      (let ([grp (mk-option-group)])
        (apply option-group-add! grp opts)
        grp)))

  #|proc:option-group-add!
The `option-group-add!` procedure appends option objects to `grp`.
|#
  (define-who option-group-add!
    (lambda (grp . opts)
      (pcheck ([option-group? grp])
        (unless (andmap option? opts)
          (errorf who "expected option objects: ~s" opts))
        (for-each ($option-group-options grp) opts))))

  #|proc:option-group-options
The `option-group-options` procedure returns the options currently in `grp`.
|#
  (define option-group-options
    (lambda (grp)
      (pcheck ([option-group? grp])
        (($option-group-options grp)))))



;;===----------------------------------------------------------------------===
;; small helpers
;;===----------------------------------------------------------------------===


  (define *numbers* '(1 ? + *))
  (define *value-numbers* '(0 1 ? + *))
  (define *none* (list 'none))
  (define *help* (list 'help))

  (define string-prefix?
    (lambda (prefix str)
      (let ([n (string-length prefix)])
        (and (fx<= n (string-length str))
             (let lp ([i 0])
               (or (fx= i n)
                   (and (char=? (string-ref prefix i) (string-ref str i))
                        (lp (fx1+ i)))))))))

  (define string-contains-char?
    (lambda (str ch)
      (let ([n (string-length str)])
        (let lp ([i 0])
          (and (fx< i n)
               (or (char=? (string-ref str i) ch)
                   (lp (fx1+ i))))))))

  (define ensure-option-ref-list
    (lambda (who v)
      (let ([xs (if (list? v) v (list v))])
        (map (lambda (x)
               (cond [(symbol? x) x]
                     [(option? x) (option-name x)]
                     [else (errorf who "expected option, symbol, or list: ~s" v)]))
             xs))))

  (define option-list
    (lambda (cmd)
      ((command-options cmd))))

  (define subcommand-list
    (lambda (cmd)
      ((command-subcommands cmd))))

  (define option-effective-short
    (lambda (opt)
      (and (not (option-no-short? opt))
           (option-short opt))))

  (define option-effective-long
    (lambda (opt)
      (and (not (option-no-long? opt))
           (option-long opt))))

  (define option-list-valued?
    (lambda (opt)
      (or (eq? (option-value-number opt) '+)
          (eq? (option-value-number opt) '*)
          (option-sink? opt))))

  (define option-default-value
    (lambda (opt)
      (cond [(option-default opt) (option-default opt)]
            [(eq? (option-value-parser opt) parser-bool) #f]
            [else #f])))

  (define selected-option-name?
    (lambda (ht name)
      (eq-hashtable-contains? ht name)))

  (define selected-option-ref
    (lambda (ht name)
      (eq-hashtable-ref ht name *none*)))

  (define selected-option-set!
    (lambda (ht name value)
      (eq-hashtable-set! ht name value)))

  (define selected-option-delete!
    (lambda (ht name)
      (hashtable-delete! ht name)))

  (define split-option-arg
    (lambda (arg prefix-len)
      (let ([pos= (string-search arg "=")]
            [len (string-length arg)])
        (if pos=
            (values (substring arg prefix-len pos=)
                    (substring arg (fx1+ pos=) len))
            (values (substring arg prefix-len len) #f)))))

  (define split-values
    (lambda (who opt raw)
      (let ([parts (string-split raw (option-value-seperator opt))])
        (when (ormap (lambda (x) (string=? x "")) parts)
          (errorf who "empty value for option ~a" (option-name opt)))
        parts)))



;;===----------------------------------------------------------------------===
;; procedural interface
;;===----------------------------------------------------------------------===


  #|proc:command-name-set!
The `command-name-set!` procedure changes the command name used on the command line.
|#
  (define command-name-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [symbol? v])
        (let ([str (symbol->string v)])
          (when (or (string=? str "") (string-prefix? "-" str))
            (errorf 'command-name-set! "invalid command name: ~a" v))
          ($command-name-set! cmd str)))))

  #|proc:command-help-set!
The `command-help-set!` procedure sets the short help text for `cmd`.
|#
  (define command-help-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-help-set! cmd v))))

  #|proc:command-overview-set!
The `command-overview-set!` procedure sets overview text for `cmd`.
|#
  (define command-overview-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-overview-set! cmd v))))

  #|proc:command-version-set!
The `command-version-set!` procedure sets the version text for `cmd`.
|#
  (define command-version-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-version-set! cmd v))))

  #|proc:command-author-set!
The `command-author-set!` procedure sets the author text for `cmd`.
|#
  (define command-author-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-author-set! cmd v))))

  #|proc:command-copyright-set!
The `command-copyright-set!` procedure sets the copyright text for `cmd`.
|#
  (define command-copyright-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-copyright-set! cmd v))))

  #|proc:command-about-set!
The `command-about-set!` procedure sets longer about text for `cmd`.
|#
  (define command-about-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-about-set! cmd v))))

  #|proc:command-style-set!
The `command-style-set!` procedure stores help style metadata for `cmd`.
Stable core stores the value without interpreting it.
|#
  (define command-style-set!
    (lambda (cmd v)
      (pcheck ([command? cmd])
        ($command-style-set! cmd v))))

  #|proc:command-category-set!
The `command-category-set!` procedure sets the help category of `cmd`.
|#
  (define command-category-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
        ($command-category-set! cmd v))))

  #|proc:command-exec-set!
The `command-exec-set!` procedure sets the action to run when `cmd` is selected.
The action receives an option lookup procedure.
|#
  (define command-exec-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [procedure? v])
        ($command-exec-set! cmd v))))

  #|proc:command-multi-personality?-set!
The `command-multi-personality?-set!` procedure controls whether the top command
can dispatch under multiple command names.
|#
  (define command-multi-personality?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
        ($command-multi-personality?-set! cmd v))))

  #|proc:command-maybe-no-subcommand?-set!
The `command-maybe-no-subcommand?-set!` procedure controls whether `cmd` may run
without selecting one of its subcommands.
|#
  (define command-maybe-no-subcommand?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
        ($command-maybe-no-subcommand?-set! cmd v))))

  #|proc:command-flatten-help?-set!
The `command-flatten-help?-set!` procedure stores flattened-help metadata for
`cmd`.
|#
  (define command-flatten-help?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
        ($command-flatten-help?-set! cmd v))))

  #|proc:command-trace?-set!
The `command-trace?-set!` procedure stores trace metadata for `cmd`.
|#
  (define command-trace?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
        ($command-trace?-set! cmd v))))

  #|proc:command-quit?-set!
The `command-quit?-set!` procedure stores quit-after-run metadata for `cmd`.
|#
  (define command-quit?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
        ($command-quit?-set! cmd v))))

  #|proc:command-options-add!
The `command-options-add!` procedure appends option objects to `cmd`.
|#
  (define-who command-options-add!
    (lambda (cmd . opts)
      (pcheck ([command? cmd])
        (unless (andmap option? opts)
          (errorf who "expected option objects: ~s" opts))
        (for-each (command-options cmd) opts))))

  #|proc:command-subcommands-add!
The `command-subcommands-add!` procedure appends subcommands to `cmd`.
|#
  (define-who command-subcommands-add!
    (lambda (cmd . subcmds)
      (pcheck ([command? cmd])
        (unless (andmap command? subcmds)
          (errorf who "expected command objects: ~s" subcmds))
        (for-each (lambda (subcmd)
                    (when (eq? cmd subcmd)
                      (errorf who "cannot add a command as its own subcommand"))
                    ((command-subcommands cmd) subcmd))
                  subcmds))))

  #|proc:option-name-set!
The `option-name-set!` procedure changes the symbolic name used for lookup.
|#
  (define option-name-set!
    (lambda (opt v)
      (pcheck ([option? opt] [symbol? v])
        (let* ([old-name (option-name opt)]
               [old-str (symbol->string old-name)]
               [new-str (symbol->string v)]
               [old-short (and (fx> (string-length old-str) 0)
                               (string (string-ref old-str 0)))])
          (when (or (string=? new-str "") (string-prefix? "-" new-str))
            (errorf 'option-name-set! "invalid option name: ~a" v))
          ($option-name-set! opt v)
          (when (equal? (option-short opt) old-short)
            ($option-short-set! opt (string (string-ref new-str 0))))
          (when (equal? (option-long opt) old-str)
            ($option-long-set! opt new-str))))))

  #|proc:option-help-set!
The `option-help-set!` procedure sets the help text for `opt`.
|#
  (define option-help-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
        ($option-help-set! opt v))))

  #|proc:option-short-set!
The `option-short-set!` procedure sets the one-character short option name, or
`#f` to disable it.
|#
  (define-who option-short-set!
    (lambda (opt v)
      (pcheck ([option? opt] [(p/or string? boolean?) v])
        (when (eq? v #t)
          (errorf who "short option form must be a string or #f"))
        (when (string? v)
          (unless (fx= (string-length v) 1)
            (errorf who "short option form must have length 1: ~s" v))
          (when (or (string=? v "-") (string=? v "="))
            (errorf who "invalid short option form: ~s" v)))
        ($option-short-set! opt v))))

  #|proc:option-long-set!
The `option-long-set!` procedure sets the long option name, or `#f` to disable
it. Long names are used after `--` and do not include the leading dashes.
|#
  (define-who option-long-set!
    (lambda (opt v)
      (pcheck ([option? opt] [(p/or string? boolean?) v])
        (when (eq? v #t)
          (errorf who "long option form must be a string or #f"))
        (when (string? v)
          (when (or (string=? v "")
                    (string-prefix? "-" v)
                    (string-contains-char? v #\=))
            (errorf who "invalid long option form: ~s" v)))
        ($option-long-set! opt v))))

  #|proc:option-category-set!
The `option-category-set!` procedure sets the help category for `opt`.
|#
  (define option-category-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
        ($option-category-set! opt v))))

  #|proc:option-default-set!
The `option-default-set!` procedure sets the value returned when `opt` is not
present.
|#
  (define option-default-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        ($option-default-set! opt v))))

  #|proc:option-number-set!
The `option-number-set!` procedure sets the occurrence count accepted for `opt`.
Valid specifiers are `1`, `?`, `+`, and `*`.
|#
  (define-who option-number-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        (unless (memq v *numbers*)
          (errorf who "invalid option occurrence specifier: ~a" v))
        ($option-number-set! opt v))))

  #|proc:option-value-number-set!
The `option-value-number-set!` procedure sets how many values one occurrence of
`opt` consumes. Stable core named options consume values only with `=`.
|#
  (define-who option-value-number-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        (unless (memq v *value-numbers*)
          (errorf who "invalid option value occurrence specifier: ~a" v))
        ($option-value-number-set! opt v))))

  #|proc:option-value-name-set!
The `option-value-name-set!` procedure sets the display name used for option
values in help output.
|#
  (define option-value-name-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
        ($option-value-name-set! opt v))))

  #|proc:option-value-parser-set!
The `option-value-parser-set!` procedure sets the parser used to convert raw
string values for `opt`.
|#
  (define option-value-parser-set!
    (lambda (opt v)
      (pcheck ([option? opt] [procedure? v])
        ($option-value-parser-set! opt v)
        (when (and (eq? (option-value-number opt) 0)
                   (not (eq? v parser-bool)))
          ($option-value-number-set! opt 1)))))

  #|proc:option-value-seperator-set!
The `option-value-seperator-set!` procedure sets the character used to split
multi-value option payloads.
|#
  (define option-value-seperator-set!
    (lambda (opt v)
      (pcheck ([option? opt] [char? v])
        ($option-value-seperator-set! opt v))))

  #|proc:option-callback-set!
The `option-callback-set!` procedure sets a callback run when `opt` is parsed.
The callback receives the command, option, and parsed value.
|#
  (define option-callback-set!
    (lambda (opt v)
      (pcheck ([option? opt] [procedure? v])
        ($option-callback-set! opt v))))

  #|proc:option-alias-set!
The `option-alias-set!` procedure sets additional symbolic lookup names for
`opt`.
|#
  (define option-alias-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        ($option-alias-set! opt (ensure-option-ref-list 'option-alias-set! v)))))

  #|proc:option-conflicts-set!
The `option-conflicts-set!` procedure sets option names that cannot be present
with `opt`.
|#
  (define option-conflicts-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        ($option-conflicts-set! opt (ensure-option-ref-list 'option-conflicts-set! v)))))

  #|proc:option-overrides-set!
The `option-overrides-set!` procedure sets option names removed when `opt` is
present.
|#
  (define option-overrides-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        ($option-overrides-set! opt (ensure-option-ref-list 'option-overrides-set! v)))))

  #|proc:option-requires-set!
The `option-requires-set!` procedure sets option names that must also be present
when `opt` is present.
|#
  (define option-requires-set!
    (lambda (opt v)
      (pcheck ([option? opt])
        ($option-requires-set! opt (ensure-option-ref-list 'option-requires-set! v)))))

  #|proc:option-positional?-set!
The `option-positional?-set!` procedure controls whether `opt` is parsed as a
positional argument instead of a named option.
|#
  (define option-positional?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        ($option-positional?-set! opt v)
        (when v
          ($option-short-set! opt #f)
          ($option-long-set! opt #f)
          ($option-no-short?-set! opt #t)
          ($option-no-long?-set! opt #t)
          ($option-number-set! opt 1)
          ($option-value-number-set! opt 1)
          ($option-value-parser-set! opt parser-string)))))

  #|proc:option-sink?-set!
The `option-sink?-set!` procedure controls whether positional `opt` consumes all
remaining positional arguments.
|#
  (define-who option-sink?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        (when (and v (not (option-positional? opt)))
          (errorf who "option ~a is not positional" (option-name opt)))
        ($option-sink?-set! opt v)
        (when v
          ($option-number-set! opt '+)
          ($option-value-number-set! opt '+)))))

  #|proc:option-no-short?-set!
The `option-no-short?-set!` procedure disables or enables short-name parsing for
`opt`.
|#
  (define option-no-short?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        ($option-no-short?-set! opt v)
        (when v ($option-short-set! opt #f)))))

  #|proc:option-no-long?-set!
The `option-no-long?-set!` procedure disables or enables long-name parsing for
`opt`.
|#
  (define option-no-long?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        ($option-no-long?-set! opt v)
        (when v ($option-long-set! opt #f)))))

  #|proc:option-no-grouping?-set!
The `option-no-grouping?-set!` procedure stores short-option grouping metadata
for `opt`.
|#
  (define option-no-grouping?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        ($option-no-grouping?-set! opt v))))

  #|proc:option-hidden?-set!
The `option-hidden?-set!` procedure controls whether `opt` is hidden from normal
help output.
|#
  (define option-hidden?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
        ($option-hidden?-set! opt v))))



;;===----------------------------------------------------------------------===
;; builtin value parsers
;;===----------------------------------------------------------------------===


  #|proc:parser-bool
The `parser-bool` procedure parses a pure flag. It accepts no string value and
returns `#t` when the flag is present.
|#
  (define-who parser-bool
    (lambda (cmd opt x)
      (if x
          (errorf who "option ~a does not take a value" (option-name opt))
          #t)))

  #|proc:parser-natural
The `parser-natural` procedure parses an exact nonnegative integer.
|#
  (define-who parser-natural
    (lambda (cmd opt x)
      (let ([v (and (string? x) (string->number x))])
        (if (and (integer? v) (exact? v) (not (negative? v)))
            v
            (errorf who "failed to parse ~s as natural" x)))))

  #|proc:parser-integer
The `parser-integer` procedure parses an exact integer.
|#
  (define-who parser-integer
    (lambda (cmd opt x)
      (let ([v (and (string? x) (string->number x))])
        (if (and (integer? v) (exact? v))
            v
            (errorf who "failed to parse ~s as integer" x)))))

  #|proc:parser-fixnum
The `parser-fixnum` procedure parses a fixnum.
|#
  (define-who parser-fixnum
    (lambda (cmd opt x)
      (let ([v (and (string? x) (string->number x))])
        (if (fixnum? v)
            v
            (errorf who "failed to parse ~s as fixnum" x)))))

  #|proc:parser-flonum
The `parser-flonum` procedure parses a real number and returns an inexact value.
|#
  (define-who parser-flonum
    (lambda (cmd opt x)
      (let ([v (and (string? x) (string->number x))])
        (if (real? v)
            (exact->inexact v)
            (errorf who "failed to parse ~s as flonum" x)))))

  #|proc:parser-string
The `parser-string` procedure returns its string input unchanged.
|#
  (define-who parser-string
    (lambda (cmd opt x)
      (if (string? x)
          x
          (errorf who "expected string value for option ~a" (option-name opt)))))

  #|proc:parser-enum
The `parser-enum` procedure creates a parser from enum specs. A spec is either a
symbol or a list beginning with a symbol and containing `:value` and `:help`
configuration pairs.
|#
  (define parser-enum
    (lambda specs
      (let ([ht (make-hashtable string-hash string=?)])
        (for-each
         (lambda (spec)
           (let-values ([(name value)
                         (cond
                          [(symbol? spec) (values spec spec)]
                          [(and (list? spec) (pair? spec) (symbol? (car spec)))
                           (let lp ([xs (cdr spec)] [value (car spec)])
                             (cond [(null? xs) (values (car spec) value)]
                                   [(and (pair? xs) (eq? (car xs) ':value) (pair? (cdr xs)))
                                    (lp (cddr xs) (cadr xs))]
                                   [(and (pair? xs) (eq? (car xs) ':help) (pair? (cdr xs)))
                                    (lp (cddr xs) value)]
                                   [else (errorf 'parser-enum "invalid enum config: ~s" spec)]))]
                          [else (errorf 'parser-enum "invalid enum spec: ~s" spec)])])
             (hashtable-set! ht (symbol->string name) value)))
        specs)
        (lambda (cmd opt x)
          (let ([v (hashtable-ref ht x *none*)])
            (if (eq? v *none*)
                (errorf 'parser-enum "invalid enum value ~s for option ~a"
                        x (option-name opt))
                v))))))



;;===----------------------------------------------------------------------===
;; validation and parse state
;;===----------------------------------------------------------------------===


  (define command-option-tables
    (lambda (cmd)
      (let ([shorts (make-hashtable string-hash string=?)]
            [longs  (make-hashtable string-hash string=?)]
            [names  (make-eq-hashtable)]
            [positionals '()]
            [sinks '()])
        (for-each
         (lambda (opt)
           (let ([name (option-name opt)])
             (when (eq-hashtable-contains? names name)
               (errorf 'run-cli-command! "duplicate option name in command ~a: ~a"
                       (command-name cmd) name))
             (eq-hashtable-set! names name opt)
             (for-each (lambda (alias)
                         (when (eq-hashtable-contains? names alias)
                           (errorf 'run-cli-command! "duplicate option alias in command ~a: ~a"
                                   (command-name cmd) alias))
                         (eq-hashtable-set! names alias opt))
                       (option-alias opt))
             (if (option-positional? opt)
                 (begin
                   (set! positionals (append positionals (list opt)))
                   (when (option-sink? opt)
                     (set! sinks (cons opt sinks))))
                 (begin
                   (let ([short (option-effective-short opt)]
                         [long  (option-effective-long opt)])
                     (when short
                       (when (hashtable-ref shorts short #f)
                         (errorf 'run-cli-command! "duplicate short option -~a" short))
                       (hashtable-set! shorts short opt))
                     (when long
                       (when (hashtable-ref longs long #f)
                         (errorf 'run-cli-command! "duplicate long option --~a" long))
                       (hashtable-set! longs long opt)))))))
         (option-list cmd))
        (when (and (not (null? positionals))
                   (not (null? (subcommand-list cmd))))
          (errorf 'run-cli-command! "command ~a cannot have both positionals and subcommands"
                  (command-name cmd)))
        (when (fx> (length sinks) 1)
          (errorf 'run-cli-command! "command ~a has more than one sink positional"
                  (command-name cmd)))
        (when (and (not (null? sinks))
                   (not (eq? (list-last positionals) (car sinks))))
          (errorf 'run-cli-command! "sink positional must be last in command ~a"
                  (command-name cmd)))
        (values shorts longs names positionals))))

  (define validate-command-tree!
    (lambda (root)
      (let lp ([cmd root] [ancestors '()] [top? #t])
        (when (memq cmd ancestors)
          (errorf 'run-cli-command! "cycle in command tree at ~a" (command-name cmd)))
        (when (and (not top?) (command-multi-personality? cmd))
          (errorf 'run-cli-command! "multi-personality is only valid on the top command"))
        (let ([names (make-hashtable string-hash string=?)])
          (for-each
           (lambda (subcmd)
             (when (hashtable-ref names (command-name subcmd) #f)
               (errorf 'run-cli-command! "duplicate subcommand ~a" (command-name subcmd)))
             (hashtable-set! names (command-name subcmd) #t)
             (lp subcmd (cons cmd ancestors) #f))
           (subcommand-list cmd))))))

  (define validate-command-path-option-names!
    (lambda (cmds)
      (let ([names (make-eq-hashtable)])
        (for-each
         (lambda (cmd)
           (for-each
            (lambda (opt)
              (for-each
               (lambda (name)
                 (when (eq-hashtable-contains? names name)
                   (errorf 'run-cli-command! "option name is not unique on command path: ~a" name))
                 (eq-hashtable-set! names name opt))
               (cons (option-name opt) (option-alias opt))))
            (option-list cmd)))
         cmds))))

  (define make-parse-state
    (lambda ()
      (vector (make-eq-hashtable) (make-eq-hashtable) (make-eq-hashtable))))

  (define state-values  (lambda (state) (vector-ref state 0)))
  (define state-seen    (lambda (state) (vector-ref state 1)))
  (define state-skipped (lambda (state) (vector-ref state 2)))

  (define state-present?
    (lambda (state name)
      (selected-option-name? (state-seen state) name)))

  (define state-value
    (lambda (state name)
      (selected-option-ref (state-values state) name)))

  (define state-set-value!
    (lambda (state opt value present?)
      (selected-option-set! (state-values state) (option-name opt) value)
      (when present?
        (selected-option-set! (state-seen state) (option-name opt) opt))
      (for-each (lambda (alias)
                  (selected-option-set! (state-values state) alias value)
                  (when present?
                    (selected-option-set! (state-seen state) alias opt)))
                (option-alias opt))))

  (define state-remove-option!
    (lambda (state name)
      (selected-option-delete! (state-seen state) name)
      (selected-option-set! (state-skipped state) name #t)
      (selected-option-set! (state-values state) name #f)))



;;===----------------------------------------------------------------------===
;; parse implementation
;;===----------------------------------------------------------------------===


  (define parse-option-value
    (lambda (cmd opt raw)
      (let ([vn (option-value-number opt)]
            [parser (option-value-parser opt)])
        (case vn
          [(0)
           (when raw
             (errorf 'run-cli-command! "option ~a does not take a value" (option-name opt)))
           (parser cmd opt #f)]
          [(1)
           (when (or (not raw) (string=? raw ""))
             (errorf 'run-cli-command! "option ~a requires a value" (option-name opt)))
           (parser cmd opt raw)]
          [(?)
           (if (or (not raw) (string=? raw ""))
               (option-default-value opt)
               (parser cmd opt raw))]
          [(+)
           (when (or (not raw) (string=? raw ""))
             (errorf 'run-cli-command! "option ~a requires one or more values" (option-name opt)))
           (map (lambda (x) (parser cmd opt x))
                (split-values 'run-cli-command! opt raw))]
          [(*)
           (if (or (not raw) (string=? raw ""))
               '()
               (map (lambda (x) (parser cmd opt x))
                    (split-values 'run-cli-command! opt raw)))]
          [else (errorf 'run-cli-command! "invalid value-number: ~a" vn)]))))

  (define state-add-option!
    (lambda (state cmd opt raw)
      (let ([name (option-name opt)])
        (when (and (memq (option-number opt) '(1 ?))
                   (state-present? state name))
          (errorf 'run-cli-command! "option ~a can appear only once" name))
        (let ([value (parse-option-value cmd opt raw)])
          (if (and (state-present? state name)
                   (option-list-valued? opt))
              (state-set-value! state opt (append (state-value state name) value) #t)
              (state-set-value! state opt value #t))
          (when (option-callback opt)
            ((option-callback opt) cmd opt value))))))

  (define parse-positional-value
    (lambda (cmd opt raw)
      (let ([parser (option-value-parser opt)])
        (if (memq (option-value-number opt) '(+ *))
            (map (lambda (x) (parser cmd opt x))
                 (split-values 'run-cli-command! opt raw))
            (parser cmd opt raw)))))

  (define parse-positionals!
    (lambda (state cmd positionals args)
      (let ([len-pos (length positionals)]
            [len-args (length args)])
        (cond
         [(null? positionals)
          (unless (null? args)
            (errorf 'run-cli-command! "unknown arguments: ~s" args))]
         [(option-sink? (list-last positionals))
          (let* ([fixed-count (fx1- len-pos)]
                 [sink-opt (list-last positionals)]
                 [minimum-count (if (memq (option-number sink-opt) '(1 +))
                                    len-pos
                                    fixed-count)])
            (when (fx< len-args minimum-count)
              (errorf 'run-cli-command! "missing positional arguments for command ~a"
                      (command-name cmd)))
            (let ([fixed-opts (list-head positionals fixed-count)]
                  [fixed-args (list-head args fixed-count)]
                  [sink-opt (list-last positionals)]
                  [sink-args (list-tail args fixed-count)])
              (for-each (lambda (opt arg)
                          (state-set-value! state opt (parse-positional-value cmd opt arg) #t))
                        fixed-opts fixed-args)
              (state-set-value! state sink-opt
                                (map (lambda (arg)
                                       ((option-value-parser sink-opt) cmd sink-opt arg))
                                     sink-args)
                                #t)))]
         [else
          (unless (fx= len-pos len-args)
            (errorf 'run-cli-command! "incorrect number of positional arguments for command ~a"
                    (command-name cmd)))
          (for-each (lambda (opt arg)
                      (state-set-value! state opt (parse-positional-value cmd opt arg) #t))
                    positionals args)]))))

  (define apply-overrides!
    (lambda (state cmd)
      (for-each
       (lambda (opt)
         (when (state-present? state (option-name opt))
           (for-each (lambda (name)
                       (state-remove-option! state name))
                     (option-overrides opt))))
       (option-list cmd))))

  (define finalize-command!
    (lambda (state cmd)
      (apply-overrides! state cmd)
      (for-each
       (lambda (opt)
         (let ([name (option-name opt)])
           (unless (selected-option-name? (state-values state) name)
             (state-set-value! state opt (option-default-value opt) #f))
           (when (and (memq (option-number opt) '(1 +))
                      (not (state-present? state name))
                      (not (selected-option-name? (state-skipped state) name)))
             (errorf 'run-cli-command! "required option or argument missing: ~a" name))))
       (option-list cmd))
      (for-each
       (lambda (opt)
         (when (state-present? state (option-name opt))
           (for-each (lambda (name)
                       (when (state-present? state name)
                         (errorf 'run-cli-command! "option ~a conflicts with ~a"
                                 (option-name opt) name)))
                     (option-conflicts opt))
           (for-each (lambda (name)
                       (unless (state-present? state name)
                         (errorf 'run-cli-command! "option ~a requires ~a"
                                 (option-name opt) name)))
                     (option-requires opt))))
       (option-list cmd))))

  (define option-name-on-path?
    (lambda (cmds name)
      (exists (lambda (cmd)
                (exists (lambda (opt)
                          (or (eq? name (option-name opt))
                              (memq name (option-alias opt))))
                        (option-list cmd)))
              cmds)))

  (define option-lookup-proc
    (lambda (state visible-path)
      (lambda (name)
        (pcheck ([symbol? name])
          (unless (option-name-on-path? visible-path name)
            (errorf 'run-cli-command! "option ~a does not exist on selected command path" name))
          (let ([v (state-value state name)])
            (if (eq? v *none*)
                (errorf 'run-cli-command! "option ~a does not exist on selected command path" name)
                v))))))

  (define run-command-exec
    (lambda (state visible-path cmd)
      (if (command-exec cmd)
          ((command-exec cmd) (option-lookup-proc state visible-path))
          *none*)))

  (define run-command-path
    (lambda (state path)
      (validate-command-path-option-names! path)
      (let lp ([cmds path] [visible '()] [last-result *none*])
        (if (null? cmds)
            (if (eq? last-result *none*) (void) last-result)
            (let* ([cmd (car cmds)]
                   [visible* (append visible (list cmd))]
                   [ans (run-command-exec state visible* cmd)])
              (lp (cdr cmds) visible*
                  (if (eq? ans *none*) last-result ans)))))))

  (define explicit-help-option?
    (lambda (longs shorts arg)
      (cond [(string=? arg "-h") (hashtable-ref shorts "h" #f)]
            [(string=? arg "--help") (hashtable-ref longs "help" #f)]
            [(string=? arg "--help-all") (hashtable-ref longs "help-all" #f)]
            [(string=? arg "--help-commands") (hashtable-ref longs "help-commands" #f)]
            [else #f])))

  (define $print-help
    (lambda (path all? commands?)
      (define option-value-summary
        (lambda (opt)
          (let ([name (or (option-value-name opt)
                          (string-append "<" (string-upcase (symbol->string (option-name opt))) ">"))])
            (case (option-value-number opt)
              [(0) ""]
              [(1) (string-append "=" name)]
              [(?) (string-append "[=" name "]")]
              [(+) (string-append "=" name (string (option-value-seperator opt)) "...")]
              [(*) (string-append "[=" name (string (option-value-seperator opt)) "...]")]
              [else ""]))))
      (define option-summary
        (lambda (opt)
          (let ([short (option-effective-short opt)]
                [long (option-effective-long opt)]
                [val (option-value-summary opt)])
            (cond [(and short long) (string-append "-" short val ", --" long val)]
                  [short (string-append "-" short val)]
                  [long (string-append "--" long val)]
                  [else (or (option-value-name opt)
                            (string-append "<" (string-upcase (symbol->string (option-name opt))) ">"))]))))
      (define positional-summary
        (lambda (opt)
          (string-append
           (or (option-value-name opt)
               (string-append "<" (string-upcase (symbol->string (option-name opt))) ">"))
           (if (option-sink? opt) " ..." ""))))
      (define print-usage
        (lambda (path)
          (display "Usage: ")
          (let lp ([cmds path])
            (unless (null? cmds)
              (let ([cmd (car cmds)])
                (display (command-name cmd))
                (display " ")
                (unless (null? (filter (lambda (opt) (not (option-positional? opt)))
                                       (option-list cmd)))
                  (display "[OPTIONS] "))
                (for-each (lambda (opt)
                            (display (positional-summary opt))
                            (display " "))
                          (filter option-positional? (option-list cmd)))
                (when (and (null? (cdr cmds))
                           (not (null? (subcommand-list cmd))))
                  (display "COMMAND "))
                (lp (cdr cmds)))))
          (newline)))
      (define print-section
        (lambda (title rows)
          (unless (null? rows)
            (println "~a" title)
            (let ([width (fx+ 2 (apply max (map (lambda (row) (string-length (car row)))
                                               rows)))])
              (for-each (lambda (row)
                          (println "    ~a~a~a"
                                   (car row)
                                   (make-string (fx- width (string-length (car row))) #\space)
                                   (cdr row)))
                        rows))
            (newline))))
      (define group-by-category
        (lambda (objects category-proc row-proc)
          (let ([groups '()])
            (for-each
             (lambda (obj)
               (let* ([cat (category-proc obj)]
                      [row (row-proc obj)]
                      [grp (find (lambda (grp)
                                   (string=? (car grp) cat))
                                 groups)])
                 (if grp
                     (set-cdr! grp (append (cdr grp) (list row)))
                     (set! groups (append groups (list (cons cat (list row))))))))
             objects)
            groups)))
      (define print-options-and-commands
        (lambda (cmd all?)
          (let* ([opts (filter (lambda (opt)
                                 (and (not (option-positional? opt))
                                      (or all? (not (option-hidden? opt)))))
                               (option-list cmd))]
                 [pos (filter option-positional? (option-list cmd))]
                 [cmds (subcommand-list cmd)])
            (print-section "ARGUMENTS"
                           (map (lambda (opt)
                                  (cons (positional-summary opt) (option-help opt)))
                                pos))
            (for-each
             (lambda (grp)
               (print-section (if (string=? (car grp) "") "OPTIONS" (car grp))
                              (cdr grp)))
             (group-by-category opts option-category
                                (lambda (opt)
                                  (cons (option-summary opt) (option-help opt)))))
            (for-each
             (lambda (grp)
               (print-section (if (string=? (car grp) "") "COMMANDS" (car grp))
                              (cdr grp)))
             (group-by-category cmds command-category
                                (lambda (cmd)
                                  (cons (command-name cmd) (command-help cmd))))))))
      (define print-command-paths
        (lambda (prefix cmd)
          (let ([path (append prefix (list (command-name cmd)))])
            (unless (null? prefix)
              (println "~a~a~a"
                       (apply string-append
                              (let lp ([xs path])
                                (if (null? xs)
                                    '()
                                    (cons (car xs)
                                          (if (null? (cdr xs))
                                              '()
                                              (cons " " (lp (cdr xs))))))))
                       (if (string=? (command-help cmd) "") "" "  ")
                       (command-help cmd)))
            (for-each (lambda (subcmd)
                        (print-command-paths path subcmd))
                      (subcommand-list cmd)))))
      (let ([cmd (list-last path)]
            [top (car path)])
        (when (eq? cmd top)
          (unless (string=? (command-overview top) "")
            (println "~a" (command-overview top)))
          (unless (string=? (command-version top) "")
            (println "Version: ~a" (command-version top)))
          (unless (string=? (command-author top) "")
            (println "Author: ~a" (command-author top)))
          (unless (and (string=? (command-overview top) "")
                       (string=? (command-version top) "")
                       (string=? (command-author top) ""))
            (newline)))
        (print-usage path)
        (newline)
        (if commands?
            (print-command-paths '() top)
            (begin
              (print-options-and-commands cmd all?)
              (unless (string=? (command-about cmd) "")
                (println "~a" (command-about cmd))))))))

  (define print-help
    (lambda (path all? commands?)
      ($print-help path all? commands?)))

  #|proc:run-cli-command!
The `run-cli-command!` procedure parses command-line arguments for `cmd`.
Parse errors raise Scheme errors. Built-in help options print help and return
without running command exec procedures.
|#
  (define-who run-cli-command!
    (case-lambda
      [(cmd)
       (pcheck ([command? cmd])
         (let ([args (cdr (command-line))])
           (run-cli-command! cmd args)))]
      [(cmd args)
       (pcheck ([command? cmd] [list? args])
         (unless (andmap string? args)
           (errorf who "expected list of strings: ~s" args))
         (validate-command-tree! cmd)
         (let ([state (make-parse-state)])
           (call/cc
            (lambda (return)
              (let parse-command ([cmd cmd] [args args] [path (list cmd)] [last-result *none*])
                (let-values ([(shorts longs names positionals)
                              (command-option-tables cmd)])
                  (define run-current
                    (lambda ()
                      (finalize-command! state cmd)
                      (run-command-path state path)))
                  (let parse-args ([args args])
                    (cond
                     [(null? args)
                      (let ([subcmds (subcommand-list cmd)])
                        (cond [(or (null? subcmds)
                                   (command-maybe-no-subcommand? cmd)
                                   (command-exec cmd))
                               (return (run-current))]
                              [else
                               (errorf who "subcommand expected for command ~a" (command-name cmd))]))]
                     [else
                      (let ([arg (car args)])
                        (cond
                         [(and (or (string=? arg "-h")
                                   (string=? arg "--help")
                                   (string=? arg "--help-all")
                                   (string=? arg "--help-commands"))
                               (not (explicit-help-option? longs shorts arg)))
                          (cond [(string=? arg "--help-all")
                                 (print-help path #t #f)]
                                [(string=? arg "--help-commands")
                                 (print-help path #f #t)]
                                [else
                                 (print-help path #f #f)])
                          (return (void))]
                         [(string=? arg "--")
                          (if (null? (subcommand-list cmd))
                              (begin
                                (parse-positionals! state cmd positionals (cdr args))
                                (return (run-current)))
                              (errorf who "`--` is only valid in a leaf command"))]
                         [(and (string-prefix? "--" arg) (not (string=? arg "--")))
                          (let-values ([(name raw) (split-option-arg arg 2)])
                            (let ([opt (hashtable-ref longs name #f)])
                              (unless opt
                                (errorf who "unknown long option: --~a" name))
                              (when (and (not raw) (not (eq? (option-value-number opt) 0)))
                                (errorf who "option --~a requires `=` value syntax" name))
                              (state-add-option! state cmd opt raw)))
                          (parse-args (cdr args))]
                         [(and (string-prefix? "-" arg) (not (string=? arg "-")))
                          (let-values ([(name raw) (split-option-arg arg 1)])
                            (let ([opt (hashtable-ref shorts name #f)])
                              (unless opt
                                (errorf who "unknown short option: -~a" name))
                              (when (and (not raw) (not (eq? (option-value-number opt) 0)))
                                (errorf who "option -~a requires `=` value syntax" name))
                              (state-add-option! state cmd opt raw)))
                          (parse-args (cdr args))]
                         [(null? (subcommand-list cmd))
                          ;; Remove the separator at the transition to positional parsing.
                          ;; All subsequent arguments, including more separators, are literal.
                          (let ([rest (member "--" args)])
                            (parse-positionals! state cmd positionals
                              (if rest
                                  (append (list-head args (fx- (length args) (length rest)))
                                          (cdr rest))
                                  args)))
                          (return (run-current))]
                         [else
                          (let ([subcmd (find (lambda (subcmd)
                                                (string=? arg (command-name subcmd)))
                                              (subcommand-list cmd))])
                            (unless subcmd
                              (errorf who "unknown subcommand for ~a: ~a" (command-name cmd) arg))
                            (finalize-command! state cmd)
                            (parse-command subcmd (cdr args) (append path (list subcmd))
                                           last-result))]))]))))))))]))



;;===----------------------------------------------------------------------===
;; help and debug output
;;===----------------------------------------------------------------------===

  #|proc:dump-command
The `dump-command` procedure prints command configuration recursively.
|#
  (define-who dump-command
    (case-lambda
      [(cmd) (dump-command cmd #t)]
      [(cmd simple?)
       (pcheck ([command? cmd] [boolean? simple?])
         (let lp ([cmd cmd] [level 0])
           (let ([indent (make-string level #\space)])
             (if simple?
                 (println "~a* ~a" indent (command-name cmd))
                 (begin
                   (println "~acommand: ~a" indent (command-name cmd))
                   (println "~a  help: ~s" indent (command-help cmd))))
             (for-each (lambda (opt)
                         (println "~a  - ~a" indent (option-name opt)))
                       (option-list cmd))
             (for-each (lambda (subcmd)
                         (lp subcmd (+ level 2)))
                       (subcommand-list cmd)))))]))



;;===----------------------------------------------------------------------===
;; syntactic interface
;;===----------------------------------------------------------------------===


  (define $command-add-option-item!
    (lambda (cmd item)
      (cond [(option? item) (command-options-add! cmd item)]
            [(option-group? item) (apply command-options-add! cmd (option-group-options item))]
            [else (errorf '$command-add-option-item! "invalid option item: ~s" item)])))

  (define $option-group-add-item!
    (lambda (grp item)
      (cond [(option? item) (option-group-add! grp item)]
            [(option-group? item) (apply option-group-add! grp (option-group-options item))]
            [else (errorf '$option-group-add-item! "invalid option item: ~s" item)])))

  #|macro:cli-option
The `cli-option` macro constructs an option using the same configuration
keywords accepted inside `cli-command`.
|#
  (define-syntax cli-option
    (lambda (stx)
      (define (parser-form x)
        (let ([d (syntax->datum x)])
          (case d
            [(bool) #'parser-bool]
            [(natural) #'parser-natural]
            [(integer) #'parser-integer]
            [(fixnum) #'parser-fixnum]
            [(flonum) #'parser-flonum]
            [(string) #'parser-string]
            [else x])))
      (define (quoted-datum-form ctx x)
        (datum->syntax ctx (syntax->datum x)))
      (define (forms opt-id cfgs)
        (let lp ([cfgs cfgs] [out '()])
          (if (null? cfgs)
              (reverse out)
              (let ([kw (syntax->datum (car cfgs))])
                (case kw
                  [(:help)
                   (lp (cddr cfgs) (cons #`(option-help-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:default)
                   (lp (cddr cfgs) (cons #`(option-default-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:number)
                   (lp (cddr cfgs) (cons #`(option-number-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [(:value-number)
                   (lp (cddr cfgs) (cons #`(option-value-number-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [(:value-name)
                   (lp (cddr cfgs) (cons #`(option-value-name-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:value-parser)
                   (lp (cddr cfgs) (cons #`(option-value-parser-set! #,opt-id #,(parser-form (cadr cfgs))) out))]
                  [(:value-seperator)
                   (lp (cddr cfgs) (cons #`(option-value-seperator-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:positional)
                   (lp (cdr cfgs) (cons #`(option-positional?-set! #,opt-id #t) out))]
                  [(:sink)
                   (lp (cdr cfgs) (cons #`(option-sink?-set! #,opt-id #t) out))]
                  [(:short)
                   (lp (cddr cfgs)
                       (cons #`(let ([short #,(cadr cfgs)])
                                 (option-short-set! #,opt-id
                                                    (if (char? short) (string short) short)))
                             out))]
                  [(:long)
                   (lp (cddr cfgs) (cons #`(option-long-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:no-short)
                   (lp (cdr cfgs) (cons #`(option-no-short?-set! #,opt-id #t) out))]
                  [(:no-long)
                   (lp (cdr cfgs) (cons #`(option-no-long?-set! #,opt-id #t) out))]
                  [(:callback)
                   (lp (cddr cfgs) (cons #`(option-callback-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:alias)
                   (lp (cddr cfgs) (cons #`(option-alias-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [(:hidden)
                   (lp (cdr cfgs) (cons #`(option-hidden?-set! #,opt-id #t) out))]
                  [(:category)
                   (lp (cddr cfgs) (cons #`(option-category-set! #,opt-id #,(cadr cfgs)) out))]
                  [(:no-grouping)
                   (lp (cdr cfgs) (cons #`(option-no-grouping?-set! #,opt-id #t) out))]
                  [(:conflicts)
                   (lp (cddr cfgs) (cons #`(option-conflicts-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [(:overrides)
                   (lp (cddr cfgs) (cons #`(option-overrides-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [(:requires)
                   (lp (cddr cfgs) (cons #`(option-requires-set! #,opt-id '#,(quoted-datum-form opt-id (cadr cfgs))) out))]
                  [else (syntax-error (car cfgs) "unknown option keyword")])))))
      (syntax-case stx ()
        [(_ name cfg ...)
         (with-syntax ([(form ...) (forms #'opt (syntax->list #'(cfg ...)))])
           #'(let ([opt (make-option 'name)])
               form ...
               opt))])))

  (define-syntax $cli-option-item
    (syntax-rules ()
      [(_ [name cfg ...]) (cli-option name cfg ...)]
      [(_ id) id]))

  #|macro:cli-option-group
The `cli-option-group` macro constructs an option group from inline option specs
and existing option values.
|#
  (define-syntax cli-option-group
    (syntax-rules ()
      [(_ item ...)
       (let ([grp (make-option-group)])
         ($option-group-add-item! grp ($cli-option-item item))
         ...
         grp)]))

  #|macro:cli-enum
The `cli-enum` macro expands enum specs into a value parser.
|#
  (define-syntax cli-enum
    (syntax-rules ()
      [(_ spec ...) (parser-enum 'spec ...)]))

  #|macro:cli-command
The `cli-command` macro constructs a command tree using command, option, and
subcommand configuration keywords.
|#
  (define-syntax cli-command
    (lambda (stx)
      (define (split-body body)
        (let lp ([xs body] [configs '()] [opts '()] [cmds '()] [mode 'config])
          (cond
           [(null? xs) (values (reverse configs) (reverse opts) (reverse cmds))]
           [(eq? (syntax->datum (car xs)) ':options)
            (lp (cdr xs) configs opts cmds 'options)]
           [(eq? (syntax->datum (car xs)) ':subcommands)
            (lp (cdr xs) configs opts cmds 'commands)]
           [(eq? mode 'options)
            (lp (cdr xs) configs (cons (car xs) opts) cmds mode)]
           [(eq? mode 'commands)
            (lp (cdr xs) configs opts (cons (car xs) cmds) mode)]
           [else
            (let ([kw (syntax->datum (car xs))])
              (case kw
                [(:trace :quit :flatten-help :multi-personality :maybe-no-subcommand)
                 (lp (cdr xs) (cons (list (car xs)) configs) opts cmds mode)]
                [else
                 (lp (cddr xs) (cons (list (car xs) (cadr xs)) configs) opts cmds mode)]))])))
      (define (config-forms cmd-id configs)
        (map (lambda (cfg)
               (let ([kw (syntax->datum (car cfg))])
                 (case kw
                   [(:overview) #`(command-overview-set! #,cmd-id #,(cadr cfg))]
                   [(:version) #`(command-version-set! #,cmd-id #,(cadr cfg))]
                   [(:author) #`(command-author-set! #,cmd-id #,(cadr cfg))]
                   [(:copyright) #`(command-copyright-set! #,cmd-id #,(cadr cfg))]
                   [(:about) #`(command-about-set! #,cmd-id #,(cadr cfg))]
                   [(:style) #`(command-style-set! #,cmd-id #,(cadr cfg))]
                   [(:category) #`(command-category-set! #,cmd-id #,(cadr cfg))]
                   [(:exec) #`(command-exec-set! #,cmd-id #,(cadr cfg))]
                   [(:help) #`(command-help-set! #,cmd-id #,(cadr cfg))]
                   [(:trace) #`(command-trace?-set! #,cmd-id #t)]
                   [(:quit) #`(command-quit?-set! #,cmd-id #t)]
                   [(:flatten-help) #`(command-flatten-help?-set! #,cmd-id #t)]
                   [(:multi-personality) #`(command-multi-personality?-set! #,cmd-id #t)]
                   [(:maybe-no-subcommand) #`(command-maybe-no-subcommand?-set! #,cmd-id #t)]
                   [else (syntax-error (car cfg) "unknown command keyword")])))
             configs))
      (define (option-item spec)
        (syntax-case spec ()
          [(name cfg ...)
           (identifier? #'name)
           #'(cli-option name cfg ...)]
          [id
           (identifier? #'id)
           #'id]
          [_ (syntax-error spec "invalid option spec")]))
      (define (command-item spec)
        (syntax-case spec ()
          [(name body ...)
           (identifier? #'name)
           (command-expr spec)]
          [id
           (identifier? #'id)
           #'id]
          [_ (syntax-error spec "invalid command spec")]))
      (define (command-expr spec)
        (syntax-case spec ()
          [(name body ...)
           (identifier? #'name)
           (let-values ([(configs opts cmds) (split-body (syntax->list #'(body ...)))])
             (with-syntax ([(config-form ...) (config-forms #'cmd configs)]
                           [(opt-expr ...) (map option-item opts)]
                           [(cmd-expr ...) (map command-item cmds)])
               #'(let ([cmd (make-command 'name)])
                   config-form ...
                   ($command-add-option-item! cmd opt-expr)
                   ...
                   (command-subcommands-add! cmd cmd-expr ...)
                   cmd)))]
          [id
           (identifier? #'id)
           #'id]
          [_ (syntax-error spec "invalid command spec")]))
      (syntax-case stx ()
        [(_ name body ...)
         (command-expr #'(name body ...))])))

  (define-syntax $cli-command-config
    (syntax-rules (:overview :version :author :copyright :about :style
                  :category :exec :help :trace :quit :flatten-help
                  :multi-personality :maybe-no-subcommand :options :subcommands)
      [(_ cmd) (begin)]
      [(_ cmd :overview v rest ...)
       (begin (command-overview-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :version v rest ...)
       (begin (command-version-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :author v rest ...)
       (begin (command-author-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :copyright v rest ...)
       (begin (command-copyright-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :about v rest ...)
       (begin (command-about-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :style v rest ...)
       (begin (command-style-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :category v rest ...)
       (begin (command-category-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :exec v rest ...)
       (begin (command-exec-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :help v rest ...)
       (begin (command-help-set! cmd v) ($cli-command-config cmd rest ...))]
      [(_ cmd :trace rest ...)
       (begin (command-trace?-set! cmd #t) ($cli-command-config cmd rest ...))]
      [(_ cmd :quit rest ...)
       (begin (command-quit?-set! cmd #t) ($cli-command-config cmd rest ...))]
      [(_ cmd :flatten-help rest ...)
       (begin (command-flatten-help?-set! cmd #t) ($cli-command-config cmd rest ...))]
      [(_ cmd :multi-personality rest ...)
       (begin (command-multi-personality?-set! cmd #t) ($cli-command-config cmd rest ...))]
      [(_ cmd :maybe-no-subcommand rest ...)
       (begin (command-maybe-no-subcommand?-set! cmd #t) ($cli-command-config cmd rest ...))]
      [(_ cmd :options opt :subcommands sub ...)
       (begin
         ($command-add-option-item! cmd ($cli-option-item opt))
         ($cli-command-add-subcommand cmd sub)
         ...)]
      [(_ cmd :options opt ...)
       (begin
         ($command-add-option-item! cmd ($cli-option-item opt))
         ...)]
      [(_ cmd :subcommands sub ...)
       (begin
         ($cli-command-add-subcommand cmd sub)
         ...)]))

  (define-syntax $cli-command-add-subcommand
    (syntax-rules ()
      [(_ cmd [name body ...])
       (command-subcommands-add! cmd (cli-command name body ...))]
      [(_ cmd id)
       (command-subcommands-add! cmd id)]))
  )
