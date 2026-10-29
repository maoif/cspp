#!chezscheme
(library (cspp rich private render)
  (export rich-check-rendered-value rich-string->lines rich-value->lines)
  (import (cspp chez)
          (cspp utils)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich pretty)
          (cspp rich private common)
          (cspp rich private lines))

  #|proc:rich-check-rendered-value
  The private `rich-check-rendered-value` returns renderer result `value` if it is a
  string or a list of segment lines. Otherwise it raises an error named by symbol `who`.
  |#
  (define rich-check-rendered-value
    (lambda (who value)
      (pcheck ([symbol? who])
        (if (or (string? value)
                (and (list? value)
                     (andmap (lambda (line) (and (list? line) (andmap rich-segment? line))) value)))
            value
            (errorf who "renderer returned invalid value: ~a" value)))))

  #|proc:rich-string->lines
  The private `rich-string->lines` returns string `text` as normalized segment lines,
  splitting line endings and expanding tabs at eight-cell stops.
  |#
  (define rich-string->lines
    (lambda (text)
      (pcheck ([string? text])
        (rich-lines-normalize (list (list (rich-segment text)))))))

  #|proc:rich-value->lines
  The private `rich-value->lines` renders `value` as normalized segment lines for a
  container. Registered renderers take precedence over primitive display and datum
  fallback. Invalid renderer results raise an error named by symbol `who`.
  |#
  (define rich-value->lines
    (lambda (who value)
      (pcheck ([symbol? who])
        (cond [(rich-renderer-for value) =>
               (lambda (renderer)
                 (let ([rendered (rich-check-rendered-value who (renderer value))])
                   (if (string? rendered)
                       (rich-string->lines rendered)
                       (rich-lines-normalize rendered))))]
              [(string? value) (rich-string->lines value)]
              [(or (char? value) (number? value) (symbol? value))
               (rich-string->lines
                (rich-string-output
                 (lambda (port)
                   (if (char? value) (write-char value port) (display value port)))))]
              [else (rich-pretty-render value)])))))
