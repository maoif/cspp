#!chezscheme
(library (cspp logging sink)
  (export log-sink?
          log-sink-name
          log-sink-level
          log-sink-level-set!
          log-sink-filter
          log-sink-filter-set!
          log-sink-formatter
          log-sink-formatter-set!
          log-sink-write!
          log-sink-flush!
          log-sink-close!
          log-sink-closed?
          make-log-port-sink
          make-log-rich-console-sink
          make-log-file-sink
          make-log-rotating-file-sink
          make-log-null-sink
          make-log-procedure-sink
          make-log-tee-sink)
  (import (cspp chez)
          (cspp utils)
          (cspp rich)
          (cspp logging level)
          (cspp logging formatter)
          (cspp logging private common))

  (define-record-type $log-sink
    (fields (immutable name $log-sink-name)
            (mutable level $log-sink-level $log-sink-level-set!)
            (mutable filter $log-sink-filter $log-sink-filter-set!)
            (mutable formatter $log-sink-formatter $log-sink-formatter-set!)
            (immutable writer $log-sink-writer)
            (immutable flusher $log-sink-flusher)
            (immutable closer $log-sink-closer)
            (mutable closed? $log-sink-closed? $log-sink-closed?-set!)
            (immutable lock $log-sink-lock)))

  #|proc:log-sink?
  The `log-sink?` procedure returns whether `x` is a log sink.
  |#
  (define log-sink? $log-sink?)

  (define $make-sink
    (lambda (name level filter formatter writer flusher closer)
      (make-$log-sink name level filter formatter writer flusher closer #f (make-mutex name))))

  (define-syntax with-sink-mutex
    (syntax-rules ()
      [(_ sink body ...)
       (with-mutex ($log-sink-lock sink)
         body ...)]))

  (define $all-sinks?
    (lambda (x)
      (and (list? x) (andmap log-sink? x))))

  (define $false-or-rich-palette?
    (lambda (x)
      (or (not x) (log-rich-palette? x))))

  (define $write-formatted-line
    (lambda (port formatter logger-name level timestamp thread source kind payload args)
      (display (log-ensure-newline
                (log-formatter-format formatter logger-name level timestamp thread source kind payload args))
               port)))

  (define $string-byte-length
    (lambda (text)
      (bytevector-length (string->utf8 text))))

  #|proc:log-sink-name
  The `log-sink-name` procedure returns the name of `sink`.
  |#
  (define log-sink-name
    (lambda (sink)
      (pcheck ([log-sink? sink])
              ($log-sink-name sink))))

  #|proc:log-sink-level
  The `log-sink-level` procedure returns the threshold of `sink`, or `#f` when
  the sink inherits the logger threshold.
  |#
  (define log-sink-level
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                ($log-sink-level sink)))))

  #|proc:log-sink-level-set!
  The `log-sink-level-set!` procedure sets the threshold of `sink` to
  `level-or-false`. A false value makes the sink inherit the logger threshold.
  |#
  (define log-sink-level-set!
    (lambda (sink level-or-false)
      (pcheck ([log-sink? sink] [log-false-or-level? level-or-false])
              (with-sink-mutex sink
                ($log-sink-level-set! sink level-or-false)))))

  #|proc:log-sink-filter
  The `log-sink-filter` procedure returns the filter of `sink`, or `#f` when no
  sink filter is configured.
  |#
  (define log-sink-filter
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                ($log-sink-filter sink)))))

  #|proc:log-sink-filter-set!
  The `log-sink-filter-set!` procedure sets the filter of `sink` to
  `filter-or-false`. The filter receives logger name, level, timestamp, thread,
  source, kind, payload, and args.
  |#
  (define log-sink-filter-set!
    (lambda (sink filter-or-false)
      (pcheck ([log-sink? sink] [log-false-or-procedure? filter-or-false])
              (with-sink-mutex sink
                ($log-sink-filter-set! sink filter-or-false)))))

  #|proc:log-sink-formatter
  The `log-sink-formatter` procedure returns the formatter used by `sink`.
  |#
  (define log-sink-formatter
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                ($log-sink-formatter sink)))))

  #|proc:log-sink-formatter-set!
  The `log-sink-formatter-set!` procedure sets the formatter used by `sink`.
  |#
  (define log-sink-formatter-set!
    (lambda (sink formatter)
      (pcheck ([log-sink? sink] [log-formatter? formatter])
              (with-sink-mutex sink
                ($log-sink-formatter-set! sink formatter)))))

  #|proc:log-sink-closed?
  The `log-sink-closed?` procedure returns whether `sink` is closed.
  |#
  (define log-sink-closed?
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                ($log-sink-closed? sink)))))

  #|proc:log-sink-write!
  The `log-sink-write!` procedure writes direct log data to `sink`. The
  `logger-name`, `level`, `timestamp`, `thread`, `source`, `kind`, `payload`,
  and `args` parameters describe one log message without allocating a public
  log-record object.
  |#
  (define log-sink-write!
    (lambda (sink logger-name level timestamp thread source kind payload args)
      (pcheck ([log-sink? sink]
               [log-symbol-or-string? logger-name]
               [log-message-level? level]
               [list? args])
              (with-sink-mutex sink
                (let ([sink-level ($log-sink-level sink)]
                      [filter ($log-sink-filter sink)])
                  (cond [($log-sink-closed? sink)
                         (errorf 'log-sink-write! "log sink is closed: ~a" ($log-sink-name sink))]
                        [(and sink-level (not (log-level>=? level sink-level))) (void)]
                        [(and filter
                              (not (filter logger-name level timestamp thread source kind payload args)))
                         (void)]
                        [else
                         (($log-sink-writer sink)
                          sink logger-name level timestamp thread source kind payload args)]))))))

  #|proc:log-sink-flush!
  The `log-sink-flush!` procedure flushes buffered output owned by `sink`.
  |#
  (define log-sink-flush!
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                (unless ($log-sink-closed? sink)
                  (($log-sink-flusher sink) sink))))))

  #|proc:log-sink-close!
  The `log-sink-close!` procedure flushes and closes resources owned by `sink`.
  Closing a sink more than once has no effect.
  |#
  (define log-sink-close!
    (lambda (sink)
      (pcheck ([log-sink? sink])
              (with-sink-mutex sink
                (unless ($log-sink-closed? sink)
                  (($log-sink-flusher sink) sink)
                  (($log-sink-closer sink) sink)
                  ($log-sink-closed?-set! sink #t))))))

  #|proc:make-log-port-sink
  The `make-log-port-sink` procedure creates a sink named `name` that writes
  formatted plain text to caller-owned textual output `port`. Closing the sink
  flushes but does not close `port`.
  |#
  (define make-log-port-sink
    (lambda (name port)
      (pcheck ([log-symbol-or-string? name] [output-port? port])
              ($make-sink
               name #f #f (log-default-formatter)
               (lambda (sink logger-name level timestamp thread source kind payload args)
                 ($write-formatted-line port ($log-sink-formatter sink)
                                        logger-name level timestamp thread source kind payload args))
               (lambda (sink) (flush-output-port port))
               (lambda (sink) (void))))))

  #|proc:make-log-rich-console-sink
  The `make-log-rich-console-sink` procedure creates a sink named `name` that
  writes through rich `console`. The `name` parameter is the sink name. The
  `console` parameter is the rich console destination. When supplied, the
  `palette` parameter is the rich logging palette used to style log messages by
  level.
  |#
  (define make-log-rich-console-sink
    (case-lambda
      [(name console)
       (make-log-rich-console-sink name console #f)]
      [(name console palette)
       (pcheck ([log-symbol-or-string? name]
                [rich-console? console]
                [$false-or-rich-palette? palette])
               (let ([formatter
                      (if palette
                          (make-log-rich-formatter palette)
                          (make-log-rich-formatter))])
                 ($make-sink
                  name #f #f formatter
                  (lambda (sink logger-name level timestamp thread source kind payload args)
                    (let ([text (log-formatter-format ($log-sink-formatter sink)
                                                      logger-name level timestamp thread source kind payload args)])
                      (rich-println console text)))
                  (lambda (sink) (flush-output-port (rich-console-output-port console)))
                  (lambda (sink) (void)))))]))

  #|proc:make-log-file-sink
  The `make-log-file-sink` procedure opens `path` for appending and creates a
  sink named `name` that owns and closes the opened file port.
  |#
  (define make-log-file-sink
    (lambda (name path)
      (pcheck ([log-symbol-or-string? name] [string? path])
              (let ([port (open-file-output-port path
                                                 (file-options no-fail no-truncate)
                                                 (buffer-mode block)
                                                 (current-transcoder))])
                (file-position port (file-length port))
                ($make-sink
                 name #f #f (log-default-formatter)
                 (lambda (sink logger-name level timestamp thread source kind payload args)
                   ($write-formatted-line port ($log-sink-formatter sink)
                                          logger-name level timestamp thread source kind payload args))
                 (lambda (sink) (flush-output-port port))
                 (lambda (sink) (close-port port)))))))

  #|proc:make-log-rotating-file-sink
  The `make-log-rotating-file-sink` procedure creates a file sink named `name`
  that writes to `path` and rotates backups when writing a line would exceed
  `max-bytes`. At most `backup-count` backups are kept.
  |#
  (define make-log-rotating-file-sink
    (lambda (name path max-bytes backup-count)
      (pcheck ([log-symbol-or-string? name]
               [string? path]
               [positive-natural? max-bytes]
               [natural? backup-count])
              (let ([port (open-file-output-port path
                                                 (file-options no-fail no-truncate)
                                                 (buffer-mode block)
                                                 (current-transcoder))])
                (define backup-path
                  (lambda (n) (format "~a.~a" path n)))
                (define rotate!
                  (lambda ()
                    (flush-output-port port)
                    (close-port port)
                    (if (= backup-count 0)
                        (when (file-exists? path)
                          (delete-file path))
                        (begin
                          (when (file-exists? (backup-path backup-count))
                            (delete-file (backup-path backup-count)))
                          (do ([i (- backup-count 1) (- i 1)])
                              ((<= i 0))
                            (when (file-exists? (backup-path i))
                              (rename-file (backup-path i) (backup-path (+ i 1)))))
                          (when (file-exists? path)
                            (rename-file path (backup-path 1)))))
                    (set! port (open-file-output-port path
                                                      (file-options no-fail no-truncate)
                                                      (buffer-mode block)
                                                      (current-transcoder)))))
                (file-position port (file-length port))
                ($make-sink
                 name #f #f (log-default-formatter)
                 (lambda (sink logger-name level timestamp thread source kind payload args)
                   (let ([line (log-ensure-newline
                                (log-formatter-format ($log-sink-formatter sink)
                                                      logger-name level timestamp thread source kind payload args))])
                     (when (> (+ (file-position port) ($string-byte-length line)) max-bytes)
                       (rotate!))
                     (display line port)))
                 (lambda (sink) (flush-output-port port))
                 (lambda (sink) (close-port port)))))))

  #|proc:make-log-null-sink
  The `make-log-null-sink` procedure creates a sink named `name` that discards
  all log messages.
  |#
  (define make-log-null-sink
    (lambda (name)
      (pcheck ([log-symbol-or-string? name])
              ($make-sink name #f #f (log-default-formatter)
                          (lambda (sink logger-name level timestamp thread source kind payload args) (void))
                          (lambda (sink) (void))
                          (lambda (sink) (void))))))

  #|proc:make-log-procedure-sink
  The `make-log-procedure-sink` procedure creates a sink named `name` backed by
  `proc`. The procedure receives logger name, level, timestamp, thread, source,
  kind, payload, and args.
  |#
  (define make-log-procedure-sink
    (lambda (name proc)
      (pcheck ([log-symbol-or-string? name] [procedure? proc])
              ($make-sink name #f #f (log-default-formatter)
                          (lambda (sink logger-name level timestamp thread source kind payload args)
                            (proc logger-name level timestamp thread source kind payload args))
                          (lambda (sink) (void))
                          (lambda (sink) (void))))))

  #|proc:make-log-tee-sink
  The `make-log-tee-sink` procedure creates a sink named `name` that forwards
  writes, flushes, and closes to each sink in `sinks`.
  |#
  (define make-log-tee-sink
    (lambda (name sinks)
      (pcheck ([log-symbol-or-string? name] [$all-sinks? sinks])
              ($make-sink name #f #f (log-default-formatter)
                          (lambda (sink logger-name level timestamp thread source kind payload args)
                            (for-each
                             (lambda (child)
                               (log-sink-write! child logger-name level timestamp thread source kind payload args))
                             sinks))
                          (lambda (sink)
                            (for-each log-sink-flush! sinks))
                          (lambda (sink)
                            (for-each log-sink-close! sinks))))))

  )
