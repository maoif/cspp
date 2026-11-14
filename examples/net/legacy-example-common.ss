;; Compatibility helpers keep the established positional command forms.
(import (cspp cli)
        (cspp logging))
(load (if (file-exists? "examples/common.ss") "examples/common.ss" "../examples/common.ss"))

(define net-example-help!
  (lambda (name overview)
    (let ([arguments (command-line-arguments)])
      (when (or (member "--help" arguments) (member "-h" arguments))
        ;; Render standard help with cspp cli before opening network resources.
        (example-run! name overview '() (lambda (ref) 0) '("--help"))
        (exit 0)))))

(define call-with-legacy-example-log
  (lambda (name proc)
    (let ([log (make-logger name)])
      (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
      (when (equal? (getenv "CSPP_EXAMPLE_LOG_LEVEL") "debug")
        (logger-level-set! log 'debug))
      (dynamic-wind
        void
        (lambda ()
          (with-logger log
            (log-debug "starting network example")
            (proc)))
        (lambda ()
          (with-logger log (log-debug "closing network example"))
          (logger-close! log))))))
