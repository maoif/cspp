#!chezscheme
(import (cspp)
        (cspp path)
        (cspp rich)
        (cspp logging))

(define demo-log-file "cspp-logging-example.log")

(define delete-if-exists
  (lambda (path)
    (when (file-exists? path)
      (delete-file path))))

(define make-demo-palette
  (lambda ()
    (make-log-rich-palette
     (rich-style 'dim)
     (rich-style 'blue)
     (rich-style 'green)
     (rich-style 'yellow)
     (rich-style 'red)
     (rich-style 'bold 'red))))

(define demo-rich-logger
  (lambda ()
    (logger
      :name 'demo
      :level 'debug
      :sinks (log-rich-console-sink
               :name 'stderr
               :console (rich-console
                           :output-port (current-error-port)
                           :color-system 'standard)
               :palette (make-demo-palette)))))

(define demo-file-logger
  (lambda ()
    (logger
      :name 'file-demo
      :level 'info
      :sinks (log-file-sink
               :name 'file
               :path demo-log-file))))

(define run-default-logger-demo
  (lambda ()
    (let ([log (current-logger)])
      (logger-level-set! log 'info)
      (logger-add-sink! log (make-log-port-sink 'stdout (current-output-port)))
      (log-info "default logger writes to stdout")
      (logger-flush! log))))

(define run-rich-logger-demo
  (lambda ()
    (with-logger (demo-rich-logger)
      (log-debug "debug details are blue")
      (log-info "rich logging is green")
      (log-warnf "custom palette warning: ~a" 'yellow)
      (log-error "errors are red")
      (logger-flush! (current-logger)))))

(define run-file-logger-demo
  (lambda ()
    (delete-if-exists demo-log-file)
    (let ([log (demo-file-logger)])
      (logger-log log 'info "file sink writes this line")
      (logger-logf log 'warn "formatted value: ~a" 42)
      (logger-close! log)
      (printf "wrote ~a\n" demo-log-file))))

(run-default-logger-demo)
(run-rich-logger-demo)
(run-file-logger-demo)
