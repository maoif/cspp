#!chezscheme
(library (cspp logging logger)
  (export make-logger
          logger?
          logger-name
          logger-level
          logger-level-set!
          logger-sinks
          logger-sinks-set!
          logger-add-sink!
          logger-remove-sink!
          logger-filter
          logger-filter-set!
          logger-error-policy
          logger-error-policy-set!
          logger-enabled?
          logger-log
          logger-log/source
          logger-logf
          logger-logf/source
          logger-flush!
          logger-close!
          current-logger
          with-logger
          logger-dispatch!)
  (import (cspp chez)
          (cspp utils)
          (cspp logging level)
          (cspp logging sink)
          (cspp logging private common))

  (define-record-type $logger
    (fields (immutable name $logger-name)
            (mutable level $logger-level $logger-level-set!)
            (mutable sinks $logger-sinks $logger-sinks-set!)
            (mutable filter $logger-filter $logger-filter-set!)
            (mutable error-policy $logger-error-policy $logger-error-policy-set!)
            (mutable closed? $logger-closed? $logger-closed?-set!)
            (immutable lock $logger-lock)))

  #|proc:logger?
  The `logger?` procedure returns whether `x` is a logger.
  |#
  (define logger? $logger?)

  (define $make-logger
    (lambda (name level sinks filter error-policy)
      (make-$logger name level sinks filter error-policy #f (make-mutex name))))

  (define-syntax with-logger-mutex
    (syntax-rules ()
      [(_ logger body ...)
       (with-mutex ($logger-lock logger)
         body ...)]))

  (define $all-sinks?
    (lambda (x)
      (and (list? x) (andmap log-sink? x))))

  (define $handle-sink-error
    (lambda (logger condition)
      (case (logger-error-policy logger)
        [(raise) (raise condition)]
        [(ignore) (void)]
        [(stderr)
         (let ([port (current-error-port)])
           (display "logging sink error: " port)
           (display-condition condition port)
           (newline port))])))

  (define $call-with-sink-errors
    (lambda (logger thunk)
      (call/cc
       (lambda (return)
         (with-exception-handler
           (lambda (condition)
             ($handle-sink-error logger condition)
             (return (void)))
           thunk)))))

  (define $logger-snapshot
    (lambda (logger)
      (with-logger-mutex logger
        (values ($logger-name logger)
                ($logger-level logger)
                (list-copy ($logger-sinks logger))
                ($logger-filter logger)
                ($logger-closed? logger)))))

  #|proc:make-logger
  The `make-logger` procedure creates a synchronous logger named `name` with
  level `info`, no sinks, no filter, and the `stderr` error policy.
  |#
  (define make-logger
    (lambda (name)
      (pcheck ([log-symbol-or-string? name])
              ($make-logger name 'info '() #f 'stderr))))

  #|proc:logger-name
  The `logger-name` procedure returns the name of `logger`.
  |#
  (define logger-name
    (lambda (logger)
      (pcheck ([logger? logger])
              ($logger-name logger))))

  #|proc:logger-level
  The `logger-level` procedure returns the threshold of `logger`.
  |#
  (define logger-level
    (lambda (logger)
      (pcheck ([logger? logger])
              (with-logger-mutex logger
                ($logger-level logger)))))

  #|proc:logger-level-set!
  The `logger-level-set!` procedure sets the threshold of `logger` to `level`.
  |#
  (define logger-level-set!
    (lambda (logger level)
      (pcheck ([logger? logger] [log-level? level])
              (with-logger-mutex logger
                ($logger-level-set! logger level)))))

  #|proc:logger-sinks
  The `logger-sinks` procedure returns a fresh list of the sinks attached to
  `logger`.
  |#
  (define logger-sinks
    (lambda (logger)
      (pcheck ([logger? logger])
              (with-logger-mutex logger
                (list-copy ($logger-sinks logger))))))

  #|proc:logger-sinks-set!
  The `logger-sinks-set!` procedure replaces the sinks attached to `logger`
  with `sinks`.
  |#
  (define logger-sinks-set!
    (lambda (logger sinks)
      (pcheck ([logger? logger] [$all-sinks? sinks])
              (with-logger-mutex logger
                ($logger-sinks-set! logger (list-copy sinks))))))

  #|proc:logger-add-sink!
  The `logger-add-sink!` procedure appends `sink` to `logger`.
  |#
  (define logger-add-sink!
    (lambda (logger sink)
      (pcheck ([logger? logger] [log-sink? sink])
              (with-logger-mutex logger
                ($logger-sinks-set! logger (append ($logger-sinks logger) (list sink)))))))

  #|proc:logger-remove-sink!
  The `logger-remove-sink!` procedure removes every sink from `logger` that is
  `eq?` to `sink`.
  |#
  (define logger-remove-sink!
    (lambda (logger sink)
      (pcheck ([logger? logger] [log-sink? sink])
              (with-logger-mutex logger
                ($logger-sinks-set! logger
                                    (let loop ([sinks ($logger-sinks logger)])
                                      (cond [(null? sinks) '()]
                                            [(eq? (car sinks) sink) (loop (cdr sinks))]
                                            [else (cons (car sinks) (loop (cdr sinks)))])))))))

  #|proc:logger-filter
  The `logger-filter` procedure returns the filter procedure of `logger`, or
  `#f` when no filter is configured.
  |#
  (define logger-filter
    (lambda (logger)
      (pcheck ([logger? logger])
              (with-logger-mutex logger
                ($logger-filter logger)))))

  #|proc:logger-filter-set!
  The `logger-filter-set!` procedure sets the filter of `logger` to
  `filter-or-false`. The filter receives logger name, level, timestamp, thread,
  source, kind, payload, and args.
  |#
  (define logger-filter-set!
    (lambda (logger filter-or-false)
      (pcheck ([logger? logger] [log-false-or-procedure? filter-or-false])
              (with-logger-mutex logger
                ($logger-filter-set! logger filter-or-false)))))

  #|proc:logger-error-policy
  The `logger-error-policy` procedure returns the sink error policy of
  `logger`.
  |#
  (define logger-error-policy
    (lambda (logger)
      (pcheck ([logger? logger])
              (with-logger-mutex logger
                ($logger-error-policy logger)))))

  #|proc:logger-error-policy-set!
  The `logger-error-policy-set!` procedure sets the sink error policy of
  `logger` to `policy`, one of `raise`, `stderr`, or `ignore`.
  |#
  (define logger-error-policy-set!
    (lambda (logger policy)
      (pcheck ([logger? logger] [log-error-policy? policy])
              (with-logger-mutex logger
                ($logger-error-policy-set! logger policy)))))

  #|proc:logger-enabled?
  The `logger-enabled?` procedure returns whether `logger` would emit a message
  at `level`.
  |#
  (define logger-enabled?
    (lambda (logger level)
      (pcheck ([logger? logger] [log-message-level? level])
              (with-logger-mutex logger
                (let ([threshold ($logger-level logger)])
                  (and (not (eq? threshold 'off))
                       (log-level>=? level threshold)
                       (not ($logger-closed? logger))))))))

  #|proc:logger-dispatch!
  The `logger-dispatch!` procedure dispatches direct log fields from `logger`
  to its sinks. The `source`, `kind`, `payload`, and `args` parameters describe
  one log message.
  |#
  (define logger-dispatch!
    (lambda (logger level source kind payload args)
      (pcheck ([logger? logger] [log-message-level? level] [list? args])
              (let-values ([(name threshold sinks filter closed?) ($logger-snapshot logger)])
                (when (and (not closed?)
                           (not (eq? threshold 'off))
                           (log-level>=? level threshold))
                  (let ([timestamp (log-current-timestamp)]
                        [thread (log-current-thread-id)])
                    ($call-with-sink-errors
                     logger
                     (lambda ()
                       (when (or (not filter)
                                 (filter name level timestamp thread source kind payload args))
                         (for-each
                          (lambda (sink)
                            ($call-with-sink-errors
                             logger
                             (lambda ()
                               (log-sink-write! sink name level timestamp thread source kind payload args))))
                          sinks))))))))))

  #|proc:logger-log
  The `logger-log` procedure logs `message` at `level` through `logger`.
  |#
  (define logger-log
    (lambda (logger level message)
      (pcheck ([logger? logger] [log-message-level? level])
              (logger-dispatch! logger level #f 'message message '()))))

  #|proc:logger-log/source
  The `logger-log/source` procedure logs `message` at `level` through `logger`
  with source metadata `source`.
  |#
  (define logger-log/source
    (lambda (logger level source message)
      (pcheck ([logger? logger] [log-message-level? level])
              (logger-dispatch! logger level source 'message message '()))))

  #|proc:logger-logf
  The `logger-logf` procedure logs a formatted message at `level` through
  `logger`. The `format-string` and `args` are formatted by each sink's
  formatter.
  |#
  (define logger-logf
    (lambda (logger level format-string . args)
      (pcheck ([logger? logger] [log-message-level? level] [string? format-string])
              (logger-dispatch! logger level #f 'format format-string args))))

  #|proc:logger-logf/source
  The `logger-logf/source` procedure logs a formatted message at `level`
  through `logger` with source metadata `source`.
  |#
  (define logger-logf/source
    (lambda (logger level source format-string . args)
      (pcheck ([logger? logger] [log-message-level? level] [string? format-string])
              (logger-dispatch! logger level source 'format format-string args))))

  #|proc:logger-flush!
  The `logger-flush!` procedure flushes all sinks attached to `logger`.
  |#
  (define logger-flush!
    (lambda (logger)
      (pcheck ([logger? logger])
              (for-each
               (lambda (sink)
                 ($call-with-sink-errors logger (lambda () (log-sink-flush! sink))))
               (logger-sinks logger)))))

  #|proc:logger-close!
  The `logger-close!` procedure flushes and closes every sink attached to
  `logger`. Closing a logger more than once has no effect.
  |#
  (define logger-close!
    (lambda (logger)
      (pcheck ([logger? logger])
              (let ([sinks
                     (with-logger-mutex logger
                       (if ($logger-closed? logger)
                           #f
                           (begin
                             ($logger-closed?-set! logger #t)
                             (list-copy ($logger-sinks logger)))))])
                (when sinks
                  (for-each
                   (lambda (sink)
                     ($call-with-sink-errors logger (lambda () (log-sink-close! sink))))
                   sinks))))))

  (define $default-logger (make-logger 'default))
  (define $current-logger (make-thread-parameter $default-logger))

  #|proc:current-logger
  The `current-logger` procedure returns the current thread's logger when
  called with no arguments. When called with `logger`, it sets the current
  thread's logger.
  |#
  (define current-logger
    (case-lambda
      [() ($current-logger)]
      [(logger)
       (pcheck ([logger? logger])
               ($current-logger logger))]))

  #|macro:with-logger
  The `with-logger` macro evaluates `body ...` with `logger` as the current
  logger.
  |#
  (define-syntax with-logger
    (syntax-rules ()
      [(_ logger body ...)
       (parameterize ([$current-logger logger])
         body ...)]))

  )
