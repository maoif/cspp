#!chezscheme
(import (cspp) (cspp path) (cspp rich) (cspp logging))

(define logging-string-contains?
  (lambda (s . needles)
    (andmap
     (lambda (needle)
       (let ([n (string-length s)] [m (string-length needle)])
         (let loop ([i 0])
           (cond [(> (+ i m) n) #f]
                 [(string=? needle (substring s i (+ i m))) #t]
                 [else (loop (+ i 1))]))))
     needles)))

(define logging-read-file
  (lambda (path)
    (call-with-input-file path
      (lambda (port)
        (let loop ([chars '()])
          (let ([ch (read-char port)])
            (if (eof-object? ch)
                (list->string (reverse chars))
                (loop (cons ch chars)))))))))

(define logging-delete-if-exists
  (lambda (path)
    (when (file-exists? path) (delete-file path))))

(mat logging-levels

     (and (log-level? 'trace)
          (log-level? 'debug)
          (log-level? 'info)
          (log-level? 'warn)
          (log-level? 'error)
          (log-level? 'critical)
          (log-level? 'off))

     (and (log-message-level? 'trace)
          (not (log-message-level? 'off))
          (not (log-message-level? 'bad)))

     (and (= 0 (log-level->integer 'trace))
          (= 1 (log-level->integer 'debug))
          (= 2 (log-level->integer 'info))
          (= 3 (log-level->integer 'warn))
          (= 4 (log-level->integer 'error))
          (= 5 (log-level->integer 'critical))
          (= 6 (log-level->integer 'off)))

     (and (eq? 'trace (integer->log-level 0))
          (eq? 'debug (integer->log-level 1))
          (eq? 'info (integer->log-level 2))
          (eq? 'warn (integer->log-level 3))
          (eq? 'error (integer->log-level 4))
          (eq? 'critical (integer->log-level 5))
          (eq? 'off (integer->log-level 6)))

     (and (log-level<? 'debug 'info)
          (not (log-level<? 'error 'warn))
          (log-level>=? 'error 'warn)
          (log-level>=? 'info 'info))

     (and (string=? "trace" (log-level->string 'trace))
          (string=? "debug" (log-level->string 'debug))
          (string=? "info" (log-level->string 'info))
          (string=? "warn" (log-level->string 'warn))
          (string=? "error" (log-level->string 'error))
          (string=? "critical" (log-level->string 'critical))
          (string=? "off" (log-level->string 'off)))

     (and (eq? 'trace (string->log-level "trace"))
          (eq? 'debug (string->log-level "DEBUG"))
          (eq? 'info (string->log-level "Info"))
          (eq? 'warn (string->log-level "warning"))
          (eq? 'error (string->log-level "error"))
          (eq? 'critical (string->log-level "CRITICAL"))
          (eq? 'off (string->log-level "off")))

     ;; Error case: invalid levels and level ordinals must be rejected.
     (error? (log-level->integer 'bad))
     (error? (integer->log-level 7))
     (error? (string->log-level "verbose"))
     )


(mat logging-formatters

     (log-formatter? (log-default-formatter))

     (let* ([fmt (make-log-pattern-formatter "[%L] %n %m")]
            [text (log-formatter-format fmt 'app 'info 'time #f #f 'message "started" '())])
       (string=? "[info] app started" text))

     (let* ([fmt (make-log-pattern-formatter "%l %T %t %s %% %m")]
            [text (log-formatter-format fmt "api" 'critical "T0" 'th "src" 'format "~a=~a" '(status 500))])
       (string=? "C T0 th src % status=500" text))

     (let* ([fmt (make-log-pattern-formatter "%t")]
            [out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (log-sink-formatter-set! sink fmt)
       (logger-add-sink! log sink)
       (logger-log log 'info "threaded")
       (string=? (format "~a\n" (get-thread-id)) (get-output-string out)))

     (let* ([fmt (make-log-pattern-formatter "%m")]
            [text (log-formatter-format fmt 'app 'debug #f #f #f 'values 'a '(b 3))])
       (string=? "a b 3" text))

     (let* ([fmt (make-log-json-line-formatter)]
            [text (log-formatter-format fmt 'app 'warn "T" #f #f 'message "quote \" slash \\" '())])
       (and (string? text)
            (let ([n (string-length text)])
              (and (> n 20)
                   (char=? (string-ref text 0) #\{)
                   (char=? (string-ref text (- n 1)) #\})
                   (logging-string-contains? text "\"level\":\"warn\"")
                   (logging-string-contains? text "\\\"")))))

     ;; Error case: formatter constructors and formatter calls reject bad inputs.
     (error? (make-log-pattern-formatter 'bad))
     (error? (log-formatter-format 'bad 'app 'info #f #f #f 'message "x" '()))
     )


(mat logging-sinks

     (let ([sink (make-log-null-sink 'null)])
       (and (log-sink? sink)
            (eq? 'null (log-sink-name sink))
            (not (log-sink-level sink))
            (not (log-sink-closed? sink))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-write! sink 'app 'info "T" #f #f 'message "hello" '())
       (log-sink-flush! sink)
       (string=? "[info] app hello\n" (get-output-string out)))

     (let ([seen '()])
       (let ([sink (make-log-procedure-sink
                    'proc
                    (lambda (logger-name level timestamp thread source kind payload args)
                      (set! seen (list logger-name level timestamp thread source kind payload args))))])
         (log-sink-write! sink 'app 'warn 'ts 'th 'src 'format "~a" '(x))
         (equal? seen '(app warn ts th src format "~a" (x)))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-level-set! sink 'error)
       (log-sink-write! sink 'app 'warn #f #f #f 'message "drop" '())
       (log-sink-write! sink 'app 'error #f #f #f 'message "keep" '())
       (string=? "[error] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-filter-set! sink
                             (lambda (logger-name level timestamp thread source kind payload args)
                               (eq? level 'critical)))
       (log-sink-write! sink 'app 'error #f #f #f 'message "drop" '())
       (log-sink-write! sink 'app 'critical #f #f #f 'message "keep" '())
       (string=? "[critical] app keep\n" (get-output-string out)))

     (let* ([out1 (open-output-string)]
            [out2 (open-output-string)]
            [sink1 (make-log-port-sink 'one out1)]
            [sink2 (make-log-port-sink 'two out2)]
            [tee (make-log-tee-sink 'tee (list sink1 sink2))])
       (log-sink-write! tee 'app 'info #f #f #f 'message "both" '())
       (and (string=? "[info] app both\n" (get-output-string out1))
            (string=? "[info] app both\n" (get-output-string out2))))

     ;; Error case: closed sinks reject writes.
     (let ([sink (make-log-null-sink 'closed)])
       (log-sink-close! sink)
       (log-sink-closed? sink))

     ;; Error case: writing to a closed sink is rejected.
     (error? (let ([sink (make-log-null-sink 'closed)])
               (log-sink-close! sink)
               (log-sink-write! sink 'app 'info #f #f #f 'message "x" '())))
     )


(mat logging-loggers

     (let ([log (make-logger 'app)])
       (and (logger? log)
            (eq? 'app (logger-name log))
            (eq? 'info (logger-level log))
            (null? (logger-sinks log))
            (eq? 'stderr (logger-error-policy log))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-log log 'info "hello")
       (string=? "[info] app hello\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'error)
       (logger-log log 'warn "drop")
       (logger-log log 'error "keep")
       (string=? "[error] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-filter-set! log
                           (lambda (logger-name level timestamp thread source kind payload args)
                             (eq? level 'critical)))
       (logger-log log 'error "drop")
       (logger-log log 'critical "keep")
       (string=? "[critical] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-logf log 'info "~a=~a" 'status 200)
       (string=? "[info] app status=200\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-log/source log 'info 'src "hello")
       (logger-logf/source log 'warn 'src "~a" "warn")
       (string=? "[info] app hello\n[warn] app warn\n" (get-output-string out)))

     (let ([old (current-logger)]
           [log (make-logger 'scoped)])
       (with-logger log
         (and (eq? log (current-logger))
              (begin (current-logger old) (eq? old (current-logger))))))

     ;; Error case: logger level setter rejects invalid levels.
     (error? (logger-level-set! (make-logger 'app) 'bad))

     ;; Error case: logger sink setter rejects non-sink values.
     (error? (logger-sinks-set! (make-logger 'app) (list 'bad)))

     ;; Error case: logger error policy setter rejects invalid policies.
     (error? (logger-error-policy-set! (make-logger 'app) 'bad))
     )


(mat logging-macros

     (let* ([out (open-output-string)]
            [sink (log-port-sink :name 'port :port out :level 'debug)]
            [log (logger :name 'app :level 'debug :sinks sink)])
       (with-logger log
         (log-debug "debug")
         (log-infof "~a" "info"))
       (string=? "[debug] app debug\n[info] app info\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'error)
       (logger-warn log (error 'disabled "message expression evaluated"))
       (logger-error log "enabled")
       (string=? "[error] app enabled\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'critical)
       (logger-errorf log "~a" (error 'disabled "format argument evaluated"))
       (logger-criticalf log "~a" "enabled")
       (string=? "[critical] app enabled\n" (get-output-string out)))

     ;; Error case: constructor macros reject unknown fields at expansion time.
     (error? (eval '(logger :bogus 1)))
     )


(mat logging-files-and-errors

     (let ([path "logging-file-test.out"])
       (logging-delete-if-exists path)
       (let ([sink (make-log-file-sink 'file path)])
         (log-sink-write! sink 'app 'info #f #f #f 'message "file" '())
         (log-sink-close! sink)
         (let ([text (logging-read-file path)])
           (logging-delete-if-exists path)
           (string=? "[info] app file\n" text))))

     (let ([path "logging-rotate-test.out"])
       (logging-delete-if-exists path)
       (logging-delete-if-exists "logging-rotate-test.out.1")
       (logging-delete-if-exists "logging-rotate-test.out.2")
       (let ([sink (make-log-rotating-file-sink 'rot path 35 2)])
         (log-sink-write! sink 'app 'info #f #f #f 'message "first-long-line" '())
         (log-sink-write! sink 'app 'info #f #f #f 'message "second-long-line" '())
         (log-sink-close! sink)
         (let ([ok (and (file-exists? path)
                        (file-exists? "logging-rotate-test.out.1"))])
           (logging-delete-if-exists path)
           (logging-delete-if-exists "logging-rotate-test.out.1")
           (logging-delete-if-exists "logging-rotate-test.out.2")
           ok)))

     (let ([path "logging-rotate-zero-test.out"])
       (logging-delete-if-exists path)
       (logging-delete-if-exists "logging-rotate-zero-test.out.1")
       (let ([sink (make-log-rotating-file-sink 'rot path 25 0)])
         (log-sink-write! sink 'app 'info #f #f #f 'message "first-long-line" '())
         (log-sink-write! sink 'app 'info #f #f #f 'message "second-long-line" '())
         (log-sink-close! sink)
         (let ([text (logging-read-file path)]
               [backup? (file-exists? "logging-rotate-zero-test.out.1")])
           (logging-delete-if-exists path)
           (logging-delete-if-exists "logging-rotate-zero-test.out.1")
           (and (not backup?)
                (string=? "[info] app second-long-line\n" text)))))

     (let ([path "logging-rotate-utf8-test.out"])
       (logging-delete-if-exists path)
       (logging-delete-if-exists "logging-rotate-utf8-test.out.1")
       (let ([sink (make-log-rotating-file-sink 'rot path 26 1)]
             [message (string (integer->char #x00e9))])
         (log-sink-write! sink 'app 'info #f #f #f 'message "x" '())
         (log-sink-write! sink 'app 'info #f #f #f 'message message '())
         (log-sink-close! sink)
         (let ([ok (and (file-exists? path)
                        (file-exists? "logging-rotate-utf8-test.out.1"))])
           (logging-delete-if-exists path)
           (logging-delete-if-exists "logging-rotate-utf8-test.out.1")
           ok)))

     (let* ([out (open-output-string)]
            [console (rich-console :output-port out :force-terminal? #f :color-system 'none)]
            [sink (make-log-rich-console-sink 'rich console)])
       (log-sink-write! sink 'app 'warn #f #f #f 'message "plain" '())
       (string=? "[warn] app plain\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [console (rich-console :output-port out :color-system 'standard)]
            [sink (make-log-rich-console-sink 'rich console)])
       (log-sink-write! sink 'app 'warn #f #f #f 'message "colored" '())
       (string=? "\033[33m[warn] app colored\033[0m\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [console (rich-console :output-port out :color-system 'standard)]
            [palette (make-log-rich-palette
                      (rich-style 'cyan)
                      (rich-style 'blue)
                      (rich-style 'magenta)
                      (rich-style 'yellow)
                      (rich-style 'red)
                      (rich-style 'bold 'red))]
            [sink (log-rich-console-sink
                   :name 'rich
                   :console console
                   :palette palette)])
       (and (log-rich-palette? palette)
            (string=? "\033[35m" (rich-style->ansi 'standard (log-rich-palette-ref palette 'info)))
            (begin
              (log-sink-write! sink 'app 'info #f #f #f 'message "custom" '())
              (string=? "\033[35m[info] app custom\033[0m\n" (get-output-string out)))))

     ;; Error case: rich palettes require rich styles for every message level.
     (error? (make-log-rich-palette
              (rich-style 'cyan)
              (rich-style 'blue)
              'bad
              (rich-style 'yellow)
              (rich-style 'red)
              (rich-style 'bold 'red)))

     (let* ([err (open-output-string)]
            [bad-sink (make-log-procedure-sink
                       'bad
                       (lambda (logger-name level timestamp thread source kind payload args)
                         (error 'sink "boom")))]
            [log (make-logger 'app)])
       (logger-add-sink! log bad-sink)
       (parameterize ([current-error-port err])
         (logger-log log 'info "x")
         (logging-string-contains? (get-output-string err) "logging sink error")))

     (let* ([bad-sink (make-log-procedure-sink
                       'bad
                       (lambda (logger-name level timestamp thread source kind payload args)
                         (error 'sink "boom")))]
            [log (make-logger 'app)])
       (logger-add-sink! log bad-sink)
       (logger-error-policy-set! log 'ignore)
       (logger-log log 'info "x")
       #t)

     ;; Error case: logger filter exceptions obey the logger error policy.
     (let ([log (make-logger 'app)])
       (logger-filter-set! log
                           (lambda (logger-name level timestamp thread source kind payload args)
                             (error 'filter "boom")))
       (logger-error-policy-set! log 'ignore)
       (logger-log log 'info "x")
       #t)

     ;; Error case: raise policy propagates sink exceptions.
     (error? (let* ([bad-sink (make-log-procedure-sink
                               'bad
                               (lambda (logger-name level timestamp thread source kind payload args)
                                 (error 'sink "boom")))]
                     [log (make-logger 'app)])
               (logger-add-sink! log bad-sink)
               (logger-error-policy-set! log 'raise)
               (logger-log log 'info "x")))
     )


(mat logging-concurrency

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'shared out)]
            [log1 (make-logger 'one)]
            [log2 (make-logger 'two)])
       (logger-add-sink! log1 sink)
       (logger-add-sink! log2 sink)
       (let ([threads
              (map (lambda (i)
                     (fork-thread
                      (lambda ()
                        (do ([j 0 (+ j 1)])
                            ((= j 20))
                          (if (even? i)
                              (logger-logf log1 'info "~a-~a" i j)
                              (logger-logf log2 'info "~a-~a" i j))))))
                   (iota 8))])
         (for-each thread-join threads)
         (let ([text (get-output-string out)])
           (and (logging-string-contains? text "[info] one 0-0\n")
                (logging-string-contains? text "[info] two 1-0\n")))))

     (let* ([out (open-output-string)]
            [sink1 (make-log-port-sink 'one out)]
            [sink2 (make-log-null-sink 'two)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink1)
       (let ([writer (fork-thread
                      (lambda ()
                        (do ([i 0 (+ i 1)])
                            ((= i 50))
                          (logger-logf log 'info "~a" i))))]
             [mutator (fork-thread
                       (lambda ()
                         (do ([i 0 (+ i 1)])
                             ((= i 50))
                           (logger-add-sink! log sink2)
                           (logger-remove-sink! log sink2))))])
         (thread-join writer)
         (thread-join mutator)
         #t))
     )
