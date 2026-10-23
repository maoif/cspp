#!chezscheme
(library (cspp rich private common)
  (export rich-render-width
          rich-render-ascii-only?
          rich-optional-width?
          rich-nonnegative-integer?
          rich-positive-integer?
          rich-rgb-integer?
          rich-output-target?
          rich-string-output
          rich-list-every?
          rich-list-any?
          rich-reverse-append)
  (import (cspp chez)
          (cspp utils))

  #|proc:rich-render-width
  The private `rich-render-width` parameter holds the positive available content width.
  It defaults to 80; console rendering binds it and containers narrow it for children.
  |#
  (define rich-render-width (make-parameter 80))

  #|proc:rich-render-ascii-only?
  The private `rich-render-ascii-only?` parameter returns whether automatic renderers use
  ASCII decoration. Optional boolean `ascii-only?` sets the policy and returns unspecified.
  It defaults to `#f`; console rendering binds it for all nested containers.
  |#
  (define rich-render-ascii-only?
    (make-parameter #f (lambda (ascii-only?) (pcheck ([boolean? ascii-only?]) ascii-only?))))

  #|proc:rich-optional-width?
  The private `rich-optional-width?` procedure returns whether `width` is `#f` (automatic)
  or a positive exact integer.
  |#
  (define rich-optional-width?
    (lambda (width)
      (or (not width) (rich-positive-integer? width))))

  #|proc:rich-nonnegative-integer?
  The `rich-nonnegative-integer?` procedure returns `#t` when its argument is
  an exact integer greater than or equal to zero, and `#f` otherwise.
  |#
  (define rich-nonnegative-integer?
    (lambda (x)
      (and (integer? x) (exact? x) (not (negative? x)))))

  #|proc:rich-positive-integer?
  The `rich-positive-integer?` procedure returns `#t` when its argument is an
  exact integer greater than zero, and `#f` otherwise.
  |#
  (define rich-positive-integer?
    (lambda (x)
      (and (integer? x) (exact? x) (positive? x))))

  #|proc:rich-rgb-integer?
  The `rich-rgb-integer?` procedure returns `#t` when its argument is an exact
  integer in the RGB truecolor range `#x000000` through `#xffffff`.
  |#
  (define rich-rgb-integer?
    (lambda (x)
      (and (rich-nonnegative-integer? x) (<= x #xffffff))))

  #|proc:rich-output-target?
  The `rich-output-target?` procedure returns `#t` when its argument can be used
  directly as a plain rich output target.
  |#
  (define rich-output-target?
    (lambda (x)
      (or (output-port? x) (record? x))))

  #|proc:rich-string-output
  The `rich-string-output` procedure calls `proc` with a fresh string output
  port and returns the resulting string.
  |#
  (define rich-string-output
    (lambda (proc)
      (pcheck ([procedure? proc])
              (call-with-string-output-port proc))))

  #|proc:rich-list-every?
  The `rich-list-every?` procedure returns `#t` when `pred` returns true for
  every element of `ls`.
  |#
  (define rich-list-every?
    (lambda (pred ls)
      (pcheck ([procedure? pred] [list? ls])
              (let loop ([ls ls])
                (or (null? ls)
                    (and (pred (car ls)) (loop (cdr ls))))))))

  #|proc:rich-list-any?
  The `rich-list-any?` procedure returns the first true value returned by `pred`
  for an element of `ls`, or `#f` when no element matches.
  |#
  (define rich-list-any?
    (lambda (pred ls)
      (pcheck ([procedure? pred] [list? ls])
              (let loop ([ls ls])
                (and (not (null? ls))
                     (or (pred (car ls)) (loop (cdr ls))))))))

  #|proc:rich-reverse-append
  The `rich-reverse-append` procedure reverses `head` and appends `tail`.
  |#
  (define rich-reverse-append
    (lambda (head tail)
      (pcheck ([list? head tail])
              (let loop ([head head] [tail tail])
                (if (null? head)
                    tail
                    (loop (cdr head) (cons (car head) tail))))))))
