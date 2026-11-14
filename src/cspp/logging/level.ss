#!chezscheme
(library (cspp logging level)
  (export log-level?
          log-message-level?
          log-level->integer
          integer->log-level
          log-level<?
          log-level>=?
          log-level->string
          string->log-level)
  (import (cspp chez)
          (cspp utils))

  (define $levels '#(trace debug info warn error critical off))

  (define $string-downcase
    (lambda (s)
      (list->string (map char-downcase (string->list s)))))

  #|proc:log-level?
  The `log-level?` procedure returns whether `x` is one of the supported log
  level symbols: `trace`, `debug`, `info`, `warn`, `error`, `critical`, or
  `off`.
  |#
  (define log-level?
    (lambda (x)
      (and (memq x '(trace debug info warn error critical off)) #t)))

  #|proc:log-message-level?
  The `log-message-level?` procedure returns whether `x` is a log level that
  may be emitted as a message. The `off` level is excluded because it is only a
  threshold.
  |#
  (define log-message-level?
    (lambda (x)
      (and (memq x '(trace debug info warn error critical)) #t)))

  #|proc:log-level->integer
  The `log-level->integer` procedure returns the ordinal severity for `level`.
  Lower integers are less severe.
  |#
  (define log-level->integer
    (lambda (level)
      (pcheck ([log-level? level])
              (case level
                [(trace) 0]
                [(debug) 1]
                [(info) 2]
                [(warn) 3]
                [(error) 4]
                [(critical) 5]
                [(off) 6]))))

  #|proc:integer->log-level
  The `integer->log-level` procedure returns the log level symbol for ordinal
  `n`.
  |#
  (define integer->log-level
    (lambda (n)
      (pcheck ([(lambda (x) (and (fixnum? x) (fx<= 0 x 6))) n])
              (vector-ref $levels n))))

  #|proc:log-level<?
  The `log-level<?` procedure returns whether level `a` is less severe than
  level `b`.
  |#
  (define log-level<?
    (lambda (a b)
      (pcheck ([log-level? a b])
              (< (log-level->integer a) (log-level->integer b)))))

  #|proc:log-level>=?
  The `log-level>=?` procedure returns whether level `a` is at least as severe
  as level `b`.
  |#
  (define log-level>=?
    (lambda (a b)
      (pcheck ([log-level? a b])
              (>= (log-level->integer a) (log-level->integer b)))))

  #|proc:log-level->string
  The `log-level->string` procedure returns the lower-case name of `level`.
  |#
  (define log-level->string
    (lambda (level)
      (pcheck ([log-level? level])
              (symbol->string level))))

  #|proc:string->log-level
  The `string->log-level` procedure parses `string` as a log level name.
  `warning` is accepted as an alias for `warn`.
  |#
  (define string->log-level
    (lambda (string)
      (pcheck ([string? string])
              (case (string->symbol ($string-downcase string))
                [(trace) 'trace]
                [(debug) 'debug]
                [(info) 'info]
                [(warn warning) 'warn]
                [(error) 'error]
                [(critical) 'critical]
                [(off) 'off]
                [else (errorf 'string->log-level "invalid log level: ~a" string)]))))

  )
