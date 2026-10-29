#!chezscheme
(library (cspp rich prompt)
  (export rich-prompt
          rich-confirm
          rich-password)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich console))

  (define $default?
    (lambda (x)
      (or (not x) (string? x))))

  (define $choices?
    (lambda (x)
      (or (not x)
          (and (list? x) (rich-list-every? string? x)))))

  (define $read-line
    (lambda (port)
      (let ([line (get-line port)])
        (if (eof-object? line) "" line))))

  (define $choice-member?
    (lambda (value choices)
      (let loop ([choices choices])
        (and (not (null? choices))
             (or (string=? value (car choices))
                 (loop (cdr choices)))))))

  (define $terminal-echo-supported
    (foreign-procedure "cspp_rich_terminal_echo_supported" () int))

  (define $terminal-disable-echo
    (foreign-procedure "cspp_rich_terminal_disable_echo" () int))

  (define $terminal-restore-echo
    (foreign-procedure "cspp_rich_terminal_restore_echo" () int))

  (define $check-terminal-status
    (lambda (who status message)
      (unless (zero? status)
        (errorf who "~a: ~a" message status))))

  (define $prompt-value
    (lambda (who console message default choices)
      (pcheck ([rich-console? console]
               [string? message]
               [$default? default]
               [$choices? choices])
              (rich-print console message ": ")
              (flush-output-port (rich-console-output-port console))
              (let* ([line ($read-line (rich-console-input-port console))]
                     [value (if (and (string=? line "") default)
                                default
                                line)])
                (when (and choices (not ($choice-member? value choices)))
                  (errorf who "invalid prompt choice: ~a" value))
                value))))

  (define $confirm-value
    (lambda (value)
      (let ([value (string-downcase value)])
        (cond [(or (string=? value "y") (string=? value "yes")) #t]
              [(or (string=? value "n") (string=? value "no")) #f]
              [else (errorf 'rich-confirm "invalid confirmation value: ~a" value)]))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Prompt
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-prompt
  The `rich-prompt` procedure writes `message` to `console`, reads one line from
  the console input port, applies an optional default, validates optional
  choices, and returns the selected string.
  |#
  (define rich-prompt
    (case-lambda
      [(console message)
       (rich-prompt console message #f #f)]
      [(console message default)
       (rich-prompt console message default #f)]
      [(console message default choices)
       ($prompt-value 'rich-prompt console message default choices)]))

  #|proc:rich-confirm
  The `rich-confirm` procedure writes string `message` to `console`, reads a yes/no
  answer case-insensitively, and returns a boolean. Optional boolean `default` supplies
  the answer for empty input. Without a default, empty input raises an error, as do
  answers other than `y`, `yes`, `n`, and `no` in any letter case.
  |#
  (define rich-confirm
    (case-lambda
      [(console message)
       ($confirm-value (rich-prompt console message))]
      [(console message default)
       (pcheck ([boolean? default])
               ($confirm-value
                (rich-prompt console
                             message
                             (if default "y" "n"))))]))

  #|proc:rich-password
  The `rich-password` procedure prompts for a password. It reads normally from
  non-current input ports for deterministic tests, and disables terminal echo
  for the current input port when terminal support is available.
  |#
  (define rich-password
    (lambda (console message)
      (pcheck ([rich-console? console] [string? message])
              (let ([input (rich-console-input-port console)])
                (if (eq? input (current-input-port))
                    (begin
                      (when (zero? ($terminal-echo-supported))
                        (errorf 'rich-password
                                "terminal no-echo input is not supported"))
                      (let ([password
                             (dynamic-wind
                               (lambda ()
                                 ($check-terminal-status
                                  'rich-password
                                  ($terminal-disable-echo)
                                  "cannot disable terminal echo"))
                               (lambda ()
                                 (rich-print console message ": ")
                                 (flush-output-port (rich-console-output-port console))
                                 ($read-line input))
                               (lambda ()
                                 ($check-terminal-status
                                  'rich-password
                                  ($terminal-restore-echo)
                                  "cannot restore terminal echo")))])
                        (newline (rich-console-output-port console))
                        password))
                    (rich-prompt console message)))))))
