#!chezscheme
(library (cspp logging formatter)
  (export log-formatter?
          make-log-rich-palette
          log-rich-palette?
          log-rich-palette-ref
          make-log-pattern-formatter
          make-log-rich-formatter
          make-log-json-line-formatter
          log-default-formatter
          log-formatter-format)
  (import (cspp chez)
          (cspp utils)
          (cspp rich)
          (cspp logging level)
          (cspp logging private common))

  (define-record-type $log-formatter
    (fields (immutable kind $log-formatter-kind)
            (immutable data $log-formatter-data)
            (immutable proc $log-formatter-proc)))

  (define-record-type $log-rich-palette
    (fields (immutable trace $log-rich-palette-trace)
            (immutable debug $log-rich-palette-debug)
            (immutable info $log-rich-palette-info)
            (immutable warn $log-rich-palette-warn)
            (immutable error $log-rich-palette-error)
            (immutable critical $log-rich-palette-critical)))

  #|proc:log-formatter?
  The `log-formatter?` procedure returns whether `x` is a log formatter.
  |#
  (define log-formatter? $log-formatter?)

  #|proc:log-rich-palette?
  The `log-rich-palette?` procedure returns whether `x` is a rich logging
  palette.
  |#
  (define log-rich-palette? $log-rich-palette?)

  #|proc:make-log-rich-palette
  The `make-log-rich-palette` procedure creates a rich logging palette. The
  `trace-style`, `debug-style`, `info-style`, `warn-style`, `error-style`, and
  `critical-style` parameters are rich styles used to render messages at the
  corresponding log levels.
  |#
  (define make-log-rich-palette
    (lambda (trace-style debug-style info-style warn-style error-style critical-style)
      (pcheck ([rich-style? trace-style debug-style info-style warn-style error-style critical-style])
              (make-$log-rich-palette trace-style
                                      debug-style
                                      info-style
                                      warn-style
                                      error-style
                                      critical-style))))

  #|proc:log-rich-palette-ref
  The `log-rich-palette-ref` procedure returns the rich style used by `palette`
  for `level`.
  |#
  (define log-rich-palette-ref
    (lambda (palette level)
      (pcheck ([log-rich-palette? palette] [log-message-level? level])
              (case level
                [(trace) ($log-rich-palette-trace palette)]
                [(debug) ($log-rich-palette-debug palette)]
                [(info) ($log-rich-palette-info palette)]
                [(warn) ($log-rich-palette-warn palette)]
                [(error) ($log-rich-palette-error palette)]
                [(critical) ($log-rich-palette-critical palette)]))))

  (define $short-level
    (lambda (level)
      (case level
        [(trace) "T"]
        [(debug) "D"]
        [(info) "I"]
        [(warn) "W"]
        [(error) "E"]
        [(critical) "C"]
        [(off) "O"])))

  (define $token-string
    (lambda (token logger-name level timestamp thread source message)
      (case token
        [(#\T) (if timestamp (log-display->string timestamp) "")]
        [(#\L) (log-level->string level)]
        [(#\l) ($short-level level)]
        [(#\n) (log-display->string logger-name)]
        [(#\t) (if thread (log-display->string thread) "")]
        [(#\s) (if source (log-display->string source) "")]
        [(#\m) message]
        [(#\%) "%"]
        [else (string #\% token)])))

  (define $pattern-format
    (lambda (pattern logger-name level timestamp thread source kind payload args)
      (let ([message (log-message->string kind payload args)]
            [out (open-output-string)]
            [n (string-length pattern)])
        (let loop ([i 0])
          (cond [(= i n) (get-output-string out)]
                [(and (char=? (string-ref pattern i) #\%) (< (+ i 1) n))
                 (display ($token-string (string-ref pattern (+ i 1))
                                         logger-name level timestamp thread source message)
                          out)
                 (loop (+ i 2))]
                [else
                 (write-char (string-ref pattern i) out)
                 (loop (+ i 1))])))))

  (define $json-escape
    (lambda (s)
      (let ([out (open-output-string)])
        (let loop ([i 0])
          (if (= i (string-length s))
              (get-output-string out)
              (let ([ch (string-ref s i)])
                (case ch
                  [(#\") (display "\\\"" out)]
                  [(#\\) (display "\\\\" out)]
                  [(#\newline) (display "\\n" out)]
                  [(#\return) (display "\\r" out)]
                  [(#\tab) (display "\\t" out)]
                  [else (write-char ch out)])
                (loop (+ i 1))))))))

  (define $json-field
    (lambda (out name value first?)
      (unless first? (display "," out))
      (fprintf out "\"~a\":\"~a\"" name ($json-escape (log-display->string value)))))

  (define $json-line-format
    (lambda (logger-name level timestamp thread source kind payload args)
      (let ([out (open-output-string)]
            [message (log-message->string kind payload args)])
        (display "{" out)
        ($json-field out "logger" logger-name #t)
        ($json-field out "level" (log-level->string level) #f)
        ($json-field out "timestamp" (if timestamp timestamp "") #f)
        ($json-field out "thread" (if thread thread "") #f)
        ($json-field out "source" (if source source "") #f)
        ($json-field out "message" message #f)
        (display "}" out)
        (get-output-string out))))

  #|proc:make-log-pattern-formatter
  The `make-log-pattern-formatter` procedure creates a formatter using
  `pattern`. Supported tokens are `%T`, `%L`, `%l`, `%n`, `%t`, `%s`, `%m`, and
  `%%`.
  |#
  (define make-log-pattern-formatter
    (lambda (pattern)
      (pcheck ([string? pattern])
              (make-$log-formatter 'pattern pattern
                                   (lambda (logger-name level timestamp thread source kind payload args)
                                     ($pattern-format pattern logger-name level timestamp thread source kind payload args))))))

  (define $default-rich-palette
    (make-log-rich-palette (rich-style 'dim)
                           (rich-style 'blue)
                           (rich-style 'green)
                           (rich-style 'yellow)
                           (rich-style 'red)
                           (rich-style 'bold 'red)))

  #|proc:make-log-rich-formatter
  The `make-log-rich-formatter` procedure creates a rich formatter. When called
  without arguments, it uses the default logging palette. When called with
  `palette`, it uses that rich logging palette to style messages by level.
  |#
  (define make-log-rich-formatter
    (case-lambda
      [()
       (make-log-rich-formatter $default-rich-palette)]
      [(palette)
       (pcheck ([log-rich-palette? palette])
               (make-$log-formatter 'rich palette
                                    (lambda (logger-name level timestamp thread source kind payload args)
                                      (rich-text
                                       ($pattern-format "[%L] %n %m"
                                                       logger-name level timestamp thread source kind payload args)
                                       (log-rich-palette-ref palette level)))))]))

  #|proc:make-log-json-line-formatter
  The `make-log-json-line-formatter` procedure creates a formatter that returns
  one JSON object string for each log message.
  |#
  (define make-log-json-line-formatter
    (lambda ()
      (make-$log-formatter 'json-line #f $json-line-format)))

  (define $default-formatter (make-log-pattern-formatter "[%L] %n %m"))

  #|proc:log-default-formatter
  The `log-default-formatter` procedure returns the default plain-text
  formatter.
  |#
  (define log-default-formatter
    (lambda () $default-formatter))

  #|proc:log-formatter-format
  The `log-formatter-format` procedure formats direct log data using
  `formatter`. The `logger-name`, `level`, `timestamp`, `thread`, `source`,
  `kind`, `payload`, and `args` parameters are the direct fields supplied by a
  logger.
  |#
  (define log-formatter-format
    (lambda (formatter logger-name level timestamp thread source kind payload args)
      (pcheck ([log-formatter? formatter]
               [log-message-level? level]
               [list? args])
              (($log-formatter-proc formatter) logger-name level timestamp thread source kind payload args))))

  )
