#!chezscheme
(library (cspp logging private common)
  (export log-symbol-or-string?
          log-false-or-procedure?
          log-false-or-level?
          log-error-policy?
          log-display->string
          log-message->string
          log-ensure-newline
          log-current-timestamp
          log-current-thread-id)
  (import (cspp chez)
          (cspp logging level))

  (define log-symbol-or-string?
    (lambda (x) (or (symbol? x) (string? x))))

  (define log-false-or-procedure?
    (lambda (x) (or (not x) (procedure? x))))

  (define log-false-or-level?
    (lambda (x) (or (not x) (log-level? x))))

  (define log-error-policy?
    (lambda (x) (and (memq x '(raise stderr ignore)) #t)))

  (define log-display->string
    (lambda (x)
      (call-with-string-output-port
        (lambda (port) (display x port)))))

  (define log-message->string
    (lambda (kind payload args)
      (case kind
        [(message) (log-display->string payload)]
        [(format) (apply format payload args)]
        [(values)
         (let ([port (open-output-string)])
           (display payload port)
           (for-each (lambda (arg)
                       (display #\space port)
                       (display arg port))
                     args)
           (get-output-string port))]
        [else (errorf 'log-message->string "unknown log message kind: ~a" kind)])))

  (define log-ensure-newline
    (lambda (text)
      (let ([n (string-length text)])
        (if (or (= n 0) (not (char=? (string-ref text (- n 1)) #\newline)))
            (string-append text "\n")
            text))))

  (define log-current-timestamp
    (lambda () (current-time)))

  (define log-current-thread-id
    (lambda () (get-thread-id)))

  )
