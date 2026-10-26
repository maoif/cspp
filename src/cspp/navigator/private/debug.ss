(library (cspp navigator private debug)
  (export nav->datum nav-write nav-display nav-debug-name nav-explain)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core))

  #|proc:nav->datum
  The `nav->datum` procedure returns a stable diagnostic datum for navigator,
  path, recursive group, or reference value `x`.
  |#
  (define nav->datum
    (lambda (x)
      (let recur ([x x])
        (nav->datum-core x recur))))

  #|proc:nav-write
  The `nav-write` procedure writes the diagnostic datum for `x` to `port`, or to
  the current output port when `port` is omitted. The caller owns `port`.
  |#
  (define nav-write
    (case-lambda
      [(x) (write (nav->datum x))]
      [(x port)
       (pcheck ([output-port? port])
               (write (nav->datum x) port))]))

  (define display-indent
    (lambda (n port)
      (let loop ([i 0])
        (when (< i n)
          (display #\space port)
          (loop (+ i 1))))))

  (define pretty-datum
    (lambda (datum port indent)
      (cond [(pair? datum)
             (display "(" port)
             (write (car datum) port)
             (let loop ([xs (cdr datum)])
               (cond [(null? xs) (void)]
                     [(pair? xs)
                      (newline port)
                      (display-indent (+ indent 2) port)
                      (pretty-datum (car xs) port (+ indent 2))
                      (loop (cdr xs))]
                     [else
                      (newline port)
                      (display-indent (+ indent 2) port)
                      (display ". " port)
                      (pretty-datum xs port (+ indent 2))]))
             (display ")" port)]
            [else (write datum port)])))

  #|proc:nav-display
  The `nav-display` procedure pretty-prints the diagnostic datum for `x` to
  `port`, or to the current output port when `port` is omitted. The caller owns
  `port`.
  |#
  (define nav-display
    (case-lambda
      [(x) (pretty-datum (nav->datum x) (current-output-port) 0)]
      [(x port)
       (pcheck ([output-port? port])
               (pretty-datum (nav->datum x) port 0))]))

  #|proc:nav-debug-name
  The `nav-debug-name` procedure returns the symbolic debug name for `x` when
  `x` is a navigator, path, recursive group, or recursive reference.
  |#
  (define nav-debug-name
    (lambda (x)
      (nav-debug-name-core x)))

  #|proc:nav-explain
  The `nav-explain` procedure compiles `pathish` and returns a normalized
  diagnostic datum containing the path steps.
  |#
  (define nav-explain
    (lambda (pathish)
      (let ([path (nav-compile-path pathish)])
        `(nav-explain (steps ,(map nav-debug-name-core (nav-path-steps path))))))))
