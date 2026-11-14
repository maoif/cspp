#!chezscheme
(library (cspp rich pretty)
  (export rich-pretty
          rich-pretty-render)
  (import (cspp chez)
          (cspp rich private common)
          (cspp rich segment))

  #|proc:rich-pretty-render
  The `rich-pretty-render` procedure writes `value` with Scheme datum syntax
  and returns it as one plain rendered segment line.
  |#
  (define rich-pretty-render
    (lambda (value)
      (list
       (list
        (rich-segment
         (rich-string-output
          (lambda (port)
            (parameterize ([print-graph #t])
              (write value port)))))))))

  #|proc:rich-pretty
  The `rich-pretty` procedure renders `value` using the default rich pretty
  fallback.
  |#
  (define rich-pretty
    (lambda (value)
      (rich-pretty-render value))))
