(library (cspp system signal values)
  (export signal? signal signal-name signal-number signal->string string->signal signal-list
          hup int quit kill usr1 usr2 alrm term chld pipe
          $signal-input? $signal-ref)
  (import (cspp chez) (cspp utils) (cspp system common)
          (cspp system private) (only (cspp string) string-startswith?))

;;;;===----------------------------------------------------------------------===
;;;; signal values
;;;;===----------------------------------------------------------------------===

  #|proc:signal?
  The `signal?` procedure returns `#t` when its argument is a signal record, otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:$signal
  Signal records hold a canonical lowercase name symbol and a platform signal number.
  The exported `signal?` predicate accepts any object and returns a boolean.
  Records are obtained through `signal`, `string->signal`, or `signal-list`.
  |#
  (define-record-type ($signal make-signal signal?)
    (nongenerative)
    (fields (immutable name $signal-name)
            (immutable number $signal-number)))

  #|proc:signal-name
  The `signal-name` procedure returns the canonical symbolic name of `sig`.
  The `sig` parameter is a signal record returned by `signal`, `string->signal`, or `signal-list`.
  |#
  (define-who signal-name
    (lambda (sig)
      (pcheck-who who ([signal? sig]) ($signal-name sig))))

  #|proc:signal-number
  The `signal-number` procedure returns the operating-system signal number of `sig`.
  The `sig` parameter is a signal record returned by `signal`, `string->signal`, or `signal-list`.
  |#
  (define-who signal-number
    (lambda (sig)
      (pcheck-who who ([signal? sig]) ($signal-number sig))))

  (define $signal-table
    (guard (condition [(system-unsupported-error? condition) '()])
      (map (lambda (entry) (make-signal (car entry) (cdr entry)))
           (ffi-result-ref ((foreign-procedure "cspp_signal_table" () ptr))))))

  (define hup 'hup)
  (define int 'int)
  (define quit 'quit)
  (define kill 'kill)
  (define usr1 'usr1)
  (define usr2 'usr2)
  (define alrm 'alrm)
  (define term 'term)
  (define chld 'chld)
  (define pipe 'pipe)

  #|proc:$signal-input?
  The internal `$signal-input?` predicate returns whether arbitrary `object` has a signal-spec type.
  It accepts records, symbols, strings, and exact positive signed 32-bit integers.
  A true result does not establish that the specification names an available signal.
  |#
  (define $signal-input?
    (lambda (object)
      (or (signal? object) (symbol? object) (string? object) ($positive-pid? object))))

  (define $normalize-signal-string
    (lambda (str)
      (let* ([up (string-upcase str)]
             [name (if (string-startswith? up "SIG")
                       (substring up 3 (string-length up))
                       up)])
        (string->symbol (string-downcase name)))))

  (define $find-signal
    (lambda (pred)
      (let loop ([signals $signal-table])
        (cond [(null? signals) #f]
              [(pred (car signals)) (car signals)]
              [else (loop (cdr signals))]))))

  (define $symbol->signal
    (lambda (who name)
      (let ([sig ($find-signal (lambda (sig) (eq? name (signal-name sig))))])
        (or sig (errorf who "unknown signal name: ~a" name)))))

  (define $number->signal
    (lambda (who number)
      (let ([sig ($find-signal (lambda (sig) (= number (signal-number sig))))])
        (or sig (errorf who "unknown signal number: ~a" number)))))

  #|proc:$signal-ref
  The internal `$signal-ref` procedure resolves signal specification `spec` to a signal record.
  `who` is a caller-name symbol used for errors; `spec` accepts the forms documented by `signal`.
  Unknown names/numbers raise argument errors. An unavailable signal table raises unsupported.
  |#
  (define $signal-ref
    (lambda (who spec)
      (when (null? $signal-table)
        (raise-system-unsupported who "signal operations are unsupported on this platform"))
      (cond [(signal? spec) spec]
            [(symbol? spec) ($symbol->signal who spec)]
            [(string? spec) ($symbol->signal who ($normalize-signal-string spec))]
            [($positive-pid? spec) ($number->signal who spec)]
            [else (errorf who "invalid signal: ~a" spec)])))

  #|proc:%signal
  The `%signal` procedure returns the signal record named or numbered by `spec`.
  The `spec` parameter is a signal record, symbol, string such as `"TERM"` or `"SIGTERM"`, or
  exact positive signed 32-bit integer signal number.
  |#
  (define-who %signal
    (lambda (spec)
      (pcheck-who who ([$signal-input? spec])
              ($signal-ref who spec))))

  #|macro:signal
  The `signal` macro returns the signal record named or numbered by `spec`.
  The `spec` form may be a bare identifier such as `term`, a quoted symbol such as `'term`, a
  string such as `"TERM"` or `"SIGTERM"`, an exact positive signed 32-bit signal number,
  or an existing signal record. Common names and available realtime names come from the platform.
  Symbol names are lowercase; strings are case-insensitive and accept the optional `SIG` prefix.
  Bare common names work because their exported values are symbols; other variables are evaluated.
  |#
  (define-syntax signal
    (lambda (stx)
      (syntax-case stx ()
        [(_ spec)
         #'(%signal spec)])))

  #|proc:string->signal
  The `string->signal` procedure returns the signal record named by `string`.
  The `string` parameter is a signal name with or without a leading `"SIG"` prefix, such as
  `"TERM"` or `"SIGTERM"`.
  |#
  (define-who string->signal
    (lambda (string)
      (pcheck-who who ([string? string])
              ($signal-ref who string))))

  #|proc:signal->string
  The `signal->string` procedure returns the conventional `"SIG..."` name for `sig`.
  The `sig` parameter is a signal record.
  |#
  (define-who signal->string
    (lambda (sig)
      (pcheck-who who ([signal? sig])
              (string-append "SIG" (string-upcase (symbol->string (signal-name sig)))))))

  #|proc:signal-list
  The `signal-list` procedure returns the list of known platform signal records.
  On Linux it includes ordinary signals and available realtime signals named `rtmin`,
  `rtmin+1`, and so on through `rtmax`. Unavailable platforms return an empty list.
  |#
  (define signal-list
    (lambda ()
      (list-copy $signal-table)))

)
