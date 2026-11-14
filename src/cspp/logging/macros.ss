#!chezscheme
(library (cspp logging macros)
  (export logger
          log-port-sink
          log-rich-console-sink
          log-file-sink
          log-rotating-file-sink
          log-trace
          log-debug
          log-info
          log-warn
          log-error
          log-critical
          log-tracef
          log-debugf
          log-infof
          log-warnf
          log-errorf
          log-criticalf
          logger-trace
          logger-debug
          logger-info
          logger-warn
          logger-error
          logger-critical
          logger-tracef
          logger-debugf
          logger-infof
          logger-warnf
          logger-errorf
          logger-criticalf)
  (import (cspp chez)
          (cspp logging sink)
          (cspp logging logger))

  (define-syntax logger
    (lambda (stx)
      (define keyword?
        (lambda (x)
          (and (identifier? x)
               (memq (syntax->datum x) '(:name :level :sinks :filter :error-policy)))))
      (define parse
        (lambda (items name level sinks filter error-policy)
          (cond [(null? items)
                 (values name level (reverse sinks) filter error-policy)]
                [else
                 (let ([key (car items)])
                   (case (syntax->datum key)
                     [(:name)
                      (when (or (null? (cdr items)) (keyword? (cadr items)))
                        (syntax-error stx "logger :name requires one value"))
                      (parse (cddr items) (cadr items) level sinks filter error-policy)]
                     [(:level)
                      (when (or (null? (cdr items)) (keyword? (cadr items)))
                        (syntax-error stx "logger :level requires one value"))
                      (parse (cddr items) name (cadr items) sinks filter error-policy)]
                     [(:filter)
                      (when (or (null? (cdr items)) (keyword? (cadr items)))
                        (syntax-error stx "logger :filter requires one value"))
                      (parse (cddr items) name level sinks (cadr items) error-policy)]
                     [(:error-policy)
                      (when (or (null? (cdr items)) (keyword? (cadr items)))
                        (syntax-error stx "logger :error-policy requires one value"))
                      (parse (cddr items) name level sinks filter (cadr items))]
                     [(:sinks)
                      (let collect ([rest (cdr items)] [sink* sinks])
                        (cond [(null? rest)
                               (parse rest name level sink* filter error-policy)]
                              [(keyword? (car rest))
                               (parse rest name level sink* filter error-policy)]
                              [else (collect (cdr rest) (cons (car rest) sink*))]))]
                     [else (syntax-error key "invalid logger field")]))])))
      (syntax-case stx ()
        [(_ item ...)
         (let-values ([(name level sinks filter error-policy)
                       (parse #'(item ...) #'#f #'#f '() #'#f #'#f)])
           (unless name (syntax-error stx "logger requires :name"))
           (with-syntax ([logger-name name]
                         [logger-level level]
                         [(sink ...) sinks]
                         [logger-filter filter]
                         [logger-error-policy error-policy]
                         [tmp (car (generate-temporaries #'(logger)))])
             #'(let ([tmp (make-logger logger-name)])
                 (when logger-level (logger-level-set! tmp logger-level))
                 (logger-sinks-set! tmp (list sink ...))
                 (when logger-filter (logger-filter-set! tmp logger-filter))
                 (when logger-error-policy (logger-error-policy-set! tmp logger-error-policy))
                 tmp)))]
        [_ (syntax-error stx "invalid logger form")])))

  #|macro:log-port-sink
  The `log-port-sink` macro constructs a configured port sink from `:name`,
  `:port`, and optional `:level`, `:filter`, and `:formatter` fields.
  |#
  (define-syntax log-port-sink
    (lambda (stx)
      (define parse
        (lambda (items name port level filter formatter)
          (cond [(null? items) (values name port level filter formatter)]
                [(null? (cdr items)) (syntax-error stx "sink field requires one value")]
                [else
                 (case (syntax->datum (car items))
                   [(:name) (parse (cddr items) (cadr items) port level filter formatter)]
                   [(:port) (parse (cddr items) name (cadr items) level filter formatter)]
                   [(:level) (parse (cddr items) name port (cadr items) filter formatter)]
                   [(:filter) (parse (cddr items) name port level (cadr items) formatter)]
                   [(:formatter) (parse (cddr items) name port level filter (cadr items))]
                   [else (syntax-error (car items) "invalid log sink field")])])))
      (syntax-case stx ()
        [(_ item ...)
         (let-values ([(name port level filter formatter) (parse #'(item ...) #'#f #'#f #'#f #'#f #'#f)])
           (unless name (syntax-error stx "log-port-sink requires :name"))
           (unless port (syntax-error stx "log-port-sink requires :port"))
           (with-syntax ([sink-name name]
                         [sink-port port]
                         [sink-level level]
                         [sink-filter filter]
                         [sink-formatter formatter])
             #'(let ([sink (make-log-port-sink sink-name sink-port)])
                 (when sink-level (log-sink-level-set! sink sink-level))
                 (when sink-filter (log-sink-filter-set! sink sink-filter))
                 (when sink-formatter (log-sink-formatter-set! sink sink-formatter))
                 sink)))]
        [_ (syntax-error stx "invalid log-port-sink form")])))

  #|macro:log-rich-console-sink
  The `log-rich-console-sink` macro constructs a configured rich console sink
  from `:name`, `:console`, and optional `:palette`, `:level`, `:filter`, and
  `:formatter` fields.
  |#
  (define-syntax log-rich-console-sink
    (lambda (stx)
      (define parse
        (lambda (items name console palette level filter formatter)
          (cond [(null? items) (values name console palette level filter formatter)]
                [(null? (cdr items)) (syntax-error stx "sink field requires one value")]
                [else
                 (case (syntax->datum (car items))
                   [(:name) (parse (cddr items) (cadr items) console palette level filter formatter)]
                   [(:console) (parse (cddr items) name (cadr items) palette level filter formatter)]
                   [(:palette) (parse (cddr items) name console (cadr items) level filter formatter)]
                   [(:level) (parse (cddr items) name console palette (cadr items) filter formatter)]
                   [(:filter) (parse (cddr items) name console palette level (cadr items) formatter)]
                   [(:formatter) (parse (cddr items) name console palette level filter (cadr items))]
                   [else (syntax-error (car items) "invalid log sink field")])])))
      (syntax-case stx ()
        [(_ item ...)
         (let-values ([(name console palette level filter formatter)
                       (parse #'(item ...) #'#f #'#f #'#f #'#f #'#f #'#f)])
           (unless name (syntax-error stx "log-rich-console-sink requires :name"))
           (unless console (syntax-error stx "log-rich-console-sink requires :console"))
           (with-syntax ([sink-name name]
                         [sink-console console]
                         [sink-palette palette]
                         [sink-level level]
                         [sink-filter filter]
                         [sink-formatter formatter])
             #'(let ([sink (make-log-rich-console-sink sink-name sink-console sink-palette)])
                 (when sink-level (log-sink-level-set! sink sink-level))
                 (when sink-filter (log-sink-filter-set! sink sink-filter))
                 (when sink-formatter (log-sink-formatter-set! sink sink-formatter))
                 sink)))]
        [_ (syntax-error stx "invalid log-rich-console-sink form")])))

  #|macro:log-file-sink
  The `log-file-sink` macro constructs a configured file sink from `:name`,
  `:path`, and optional configuration fields.
  |#
  (define-syntax log-file-sink
    (lambda (stx)
      (define parse
        (lambda (items name path level filter formatter)
          (cond [(null? items) (values name path level filter formatter)]
                [(null? (cdr items)) (syntax-error stx "sink field requires one value")]
                [else
                 (case (syntax->datum (car items))
                   [(:name) (parse (cddr items) (cadr items) path level filter formatter)]
                   [(:path) (parse (cddr items) name (cadr items) level filter formatter)]
                   [(:level) (parse (cddr items) name path (cadr items) filter formatter)]
                   [(:filter) (parse (cddr items) name path level (cadr items) formatter)]
                   [(:formatter) (parse (cddr items) name path level filter (cadr items))]
                   [else (syntax-error (car items) "invalid log sink field")])])))
      (syntax-case stx ()
        [(_ item ...)
         (let-values ([(name path level filter formatter) (parse #'(item ...) #'#f #'#f #'#f #'#f #'#f)])
           (unless name (syntax-error stx "log-file-sink requires :name"))
           (unless path (syntax-error stx "log-file-sink requires :path"))
           (with-syntax ([sink-name name]
                         [sink-path path]
                         [sink-level level]
                         [sink-filter filter]
                         [sink-formatter formatter])
             #'(let ([sink (make-log-file-sink sink-name sink-path)])
                 (when sink-level (log-sink-level-set! sink sink-level))
                 (when sink-filter (log-sink-filter-set! sink sink-filter))
                 (when sink-formatter (log-sink-formatter-set! sink sink-formatter))
                 sink)))]
        [_ (syntax-error stx "invalid log-file-sink form")])))

  #|macro:log-rotating-file-sink
  The `log-rotating-file-sink` macro constructs a configured rotating file sink
  from `:name`, `:path`, `:max-bytes`, `:backup-count`, and optional
  configuration fields.
  |#
  (define-syntax log-rotating-file-sink
    (lambda (stx)
      (define parse
        (lambda (items name path max-bytes backup-count level filter formatter)
          (cond [(null? items) (values name path max-bytes backup-count level filter formatter)]
                [(null? (cdr items)) (syntax-error stx "sink field requires one value")]
                [else
                 (case (syntax->datum (car items))
                   [(:name) (parse (cddr items) (cadr items) path max-bytes backup-count level filter formatter)]
                   [(:path) (parse (cddr items) name (cadr items) max-bytes backup-count level filter formatter)]
                   [(:max-bytes) (parse (cddr items) name path (cadr items) backup-count level filter formatter)]
                   [(:backup-count) (parse (cddr items) name path max-bytes (cadr items) level filter formatter)]
                   [(:level) (parse (cddr items) name path max-bytes backup-count (cadr items) filter formatter)]
                   [(:filter) (parse (cddr items) name path max-bytes backup-count level (cadr items) formatter)]
                   [(:formatter) (parse (cddr items) name path max-bytes backup-count level filter (cadr items))]
                   [else (syntax-error (car items) "invalid log sink field")])])))
      (syntax-case stx ()
        [(_ item ...)
         (let-values ([(name path max-bytes backup-count level filter formatter)
                       (parse #'(item ...) #'#f #'#f #'#f #'#f #'#f #'#f #'#f)])
           (unless name (syntax-error stx "log-rotating-file-sink requires :name"))
           (unless path (syntax-error stx "log-rotating-file-sink requires :path"))
           (unless max-bytes (syntax-error stx "log-rotating-file-sink requires :max-bytes"))
           (unless backup-count (syntax-error stx "log-rotating-file-sink requires :backup-count"))
           (with-syntax ([sink-name name]
                         [sink-path path]
                         [sink-max-bytes max-bytes]
                         [sink-backup-count backup-count]
                         [sink-level level]
                         [sink-filter filter]
                         [sink-formatter formatter])
             #'(let ([sink (make-log-rotating-file-sink sink-name sink-path sink-max-bytes sink-backup-count)])
                 (when sink-level (log-sink-level-set! sink sink-level))
                 (when sink-filter (log-sink-filter-set! sink sink-filter))
                 (when sink-formatter (log-sink-formatter-set! sink sink-formatter))
                 sink)))]
        [_ (syntax-error stx "invalid log-rotating-file-sink form")])))

  (define-syntax $define-log-macro
    (syntax-rules ()
      [(_ macro-name proc-name level)
       (define-syntax macro-name
         (syntax-rules ()
           [(_ message)
            (let ([log (current-logger)])
              (when (logger-enabled? log 'level)
                (proc-name log 'level message)))]))]))

  (define-syntax $define-logger-log-macro
    (syntax-rules ()
      [(_ macro-name proc-name level)
       (define-syntax macro-name
         (syntax-rules ()
           [(_ logger-expr message)
            (let ([log logger-expr])
              (when (logger-enabled? log 'level)
                (proc-name log 'level message)))]))]))

  (define-syntax $define-logf-macro
    (syntax-rules ()
      [(_ macro-name proc-name level)
       (define-syntax macro-name
         (syntax-rules ()
           [(_ format-string arg (... ...))
            (let ([log (current-logger)])
              (when (logger-enabled? log 'level)
                (proc-name log 'level format-string arg (... ...))))]))]))

  (define-syntax $define-logger-logf-macro
    (syntax-rules ()
      [(_ macro-name proc-name level)
       (define-syntax macro-name
         (syntax-rules ()
           [(_ logger-expr format-string arg (... ...))
            (let ([log logger-expr])
              (when (logger-enabled? log 'level)
                (proc-name log 'level format-string arg (... ...))))]))]))

  ($define-log-macro log-trace logger-log trace)
  ($define-log-macro log-debug logger-log debug)
  ($define-log-macro log-info logger-log info)
  ($define-log-macro log-warn logger-log warn)
  ($define-log-macro log-error logger-log error)
  ($define-log-macro log-critical logger-log critical)

  ($define-logf-macro log-tracef logger-logf trace)
  ($define-logf-macro log-debugf logger-logf debug)
  ($define-logf-macro log-infof logger-logf info)
  ($define-logf-macro log-warnf logger-logf warn)
  ($define-logf-macro log-errorf logger-logf error)
  ($define-logf-macro log-criticalf logger-logf critical)

  ($define-logger-log-macro logger-trace logger-log trace)
  ($define-logger-log-macro logger-debug logger-log debug)
  ($define-logger-log-macro logger-info logger-log info)
  ($define-logger-log-macro logger-warn logger-log warn)
  ($define-logger-log-macro logger-error logger-log error)
  ($define-logger-log-macro logger-critical logger-log critical)

  ($define-logger-logf-macro logger-tracef logger-logf trace)
  ($define-logger-logf-macro logger-debugf logger-logf debug)
  ($define-logger-logf-macro logger-infof logger-logf info)
  ($define-logger-logf-macro logger-warnf logger-logf warn)
  ($define-logger-logf-macro logger-errorf logger-logf error)
  ($define-logger-logf-macro logger-criticalf logger-logf critical)

  )
