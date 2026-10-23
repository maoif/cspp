#!chezscheme
(library (cspp rich panel)
  (export make-rich-panel
          rich-panel
          rich-panel?
          rich-panel-body
          rich-panel-body-set!
          rich-panel-title
          rich-panel-title-set!
          rich-panel-subtitle
          rich-panel-subtitle-set!
          rich-panel-box
          rich-panel-box-set!
          rich-panel-padding
          rich-panel-padding-set!
          rich-panel-width
          rich-panel-width-set!
          rich-panel-height
          rich-panel-height-set!
          rich-panel-render)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich private lines)
          (cspp rich box)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich pretty))

  (define-record-type (rich-panel-record $make-rich-panel-record $rich-panel?)
    (fields (mutable body $rich-panel-body $rich-panel-body-set!)
            (mutable title $rich-panel-title $rich-panel-title-set!)
            (mutable subtitle $rich-panel-subtitle $rich-panel-subtitle-set!)
            (mutable box $rich-panel-box $rich-panel-box-set!)
            (mutable padding $rich-panel-padding $rich-panel-padding-set!)
            (mutable width $rich-panel-width $rich-panel-width-set!)
            (mutable height $rich-panel-height $rich-panel-height-set!)))

  (define $rich-panel-title?
    (lambda (x)
      (or (not x) (string? x))))

  (define $rich-panel-size?
    (lambda (x)
      (or (not x) (rich-positive-integer? x))))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $rich-segment-line-list?
    (lambda (x)
      (and (list? x) (rich-list-every? $rich-segment-list? x))))

  (define $string-split-lines
    (lambda (text)
      (let ([len (string-length text)])
        (let loop ([start 0] [i 0] [out '()])
          (cond [(= i len)
                 (reverse (cons (substring text start i) out))]
                [(char=? (string-ref text i) #\newline)
                 (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
                [else
                 (loop start (+ i 1) out)])))))

  (define $string->segment-lines
    (lambda (text)
      (map (lambda (line) (list (rich-segment line)))
           ($string-split-lines text))))

  (define $segment-copy
    (lambda (segment text)
      (rich-segment text
                    (rich-segment-style segment)
                    (rich-segment-control? segment))))

  (define $normalize-segment-line
    (lambda (line)
      (let loop-segments ([segments line] [line '()] [lines '()])
        (if (null? segments)
            (reverse (cons (reverse line) lines))
            (let loop-pieces ([pieces ($string-split-lines
                                       (rich-segment-text (car segments)))]
                              [line line]
                              [lines lines])
              (cond [(null? pieces)
                     (loop-segments (cdr segments) line lines)]
                    [(null? (cdr pieces))
                     (loop-segments (cdr segments)
                                    (cons ($segment-copy (car segments)
                                                         (car pieces))
                                          line)
                                    lines)]
                    [else
                     (loop-pieces (cdr pieces)
                                  '()
                                  (cons (reverse
                                         (cons ($segment-copy (car segments)
                                                              (car pieces))
                                               line))
                                        lines))]))))))

  (define $normalize-segment-lines
    (lambda (lines)
      (apply append (map $normalize-segment-line lines))))

  (define $rendered->segment-lines
    (lambda (who value)
      (cond [(string? value) ($string->segment-lines value)]
            [($rich-segment-line-list? value) ($normalize-segment-lines value)]
            [else (errorf who "renderer returned invalid value: ~a" value)])))

  (define $value->segment-lines
    (lambda (value)
      (cond [(rich-renderer-for value) =>
             (lambda (renderer)
               ($rendered->segment-lines 'rich-panel-render (renderer value)))]
            [(string? value)
             ($string->segment-lines value)]
            [(or (char? value) (number? value) (symbol? value))
             ($string->segment-lines
              (rich-string-output
               (lambda (port)
                 (if (char? value)
                     (write-char value port)
                     (display value port)))))]
            [else
             (rich-pretty-render value)])))

  (define $string-repeat
    (lambda (text count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            (apply string-append (reverse out))
            (loop (- count 1) (cons text out))))))

  (define $space-segment
    (lambda (width)
      (rich-segment (make-string width #\space))))

  (define $segment-lines-width rich-lines-width)

  (define $line-pad-right-to
    (lambda (line width)
      (let ([current (rich-segments-width line)])
        (if (>= current width)
            line
            (append line (list ($space-segment (- width current))))))))

  (define $blank-lines
    (lambda (count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            out
            (loop (- count 1) (cons '() out))))))

  (define $border-line
    (lambda (chars left right inner-width)
      (list (rich-segment
             (string-append left
                            ($string-repeat (vector-ref chars 4) inner-width)
                            right)))))

  (define $content-line
    (lambda (chars line content-width padding)
      (let ([v (vector-ref chars 5)]
            [side (if (zero? padding)
                      '()
                      (list ($space-segment padding)))])
        (append (list (rich-segment v))
                side
                ($line-pad-right-to line content-width)
                side
                (list (rich-segment v))))))

  (define $panel-body-lines
    (lambda (panel)
      (append (if (rich-panel-title panel)
                  ($string->segment-lines (rich-panel-title panel))
                  '())
              (if (rich-panel-subtitle panel)
                  ($string->segment-lines (rich-panel-subtitle panel))
                  '())
              ($value->segment-lines (rich-panel-body panel)))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Panel
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-panel?
  The `rich-panel?` procedure returns `#t` when its argument is a rich panel
  object, and `#f` otherwise.
  |#
  (define rich-panel?
    (lambda (x)
      ($rich-panel? x)))

  #|proc:make-rich-panel
  The `make-rich-panel` procedure constructs a mutable panel around `body` with
  ASCII borders and one space of horizontal padding. With `width` set to `#f` (default),
  content fits naturally and wraps at the available parent or terminal width. Positive
  `width` retains its minimum-width behavior. Optional positive `height` is a minimum.
  The full form accepts optional string `title` and `subtitle`, box style `box`, and
  nonnegative horizontal `padding`. Returns the panel.
  |#
  (define make-rich-panel
    (case-lambda
      [(body)
       ($make-rich-panel-record body #f #f 'ascii 1 #f #f)]
      [(body title subtitle box padding width height)
       (pcheck ([$rich-panel-title? title subtitle]
                [rich-box-style? box]
                [rich-nonnegative-integer? padding]
                [$rich-panel-size? width height])
               ($make-rich-panel-record body title subtitle box padding width height))]))

  #|proc:rich-panel-body
  The `rich-panel-body` procedure returns the body renderable for `panel`.
  |#
  (define rich-panel-body
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-body panel))))

  #|proc:rich-panel-body-set!
  The `rich-panel-body-set!` procedure sets the body renderable for `panel`.
  |#
  (define rich-panel-body-set!
    (lambda (panel body)
      (pcheck ([rich-panel? panel])
              ($rich-panel-body-set! panel body))))

  #|proc:rich-panel-title
  The `rich-panel-title` procedure returns the optional title for `panel`.
  |#
  (define rich-panel-title
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-title panel))))

  #|proc:rich-panel-title-set!
  The `rich-panel-title-set!` procedure sets the optional title for `panel`.
  |#
  (define rich-panel-title-set!
    (lambda (panel title)
      (pcheck ([rich-panel? panel] [$rich-panel-title? title])
              ($rich-panel-title-set! panel title))))

  #|proc:rich-panel-subtitle
  The `rich-panel-subtitle` procedure returns the optional subtitle for
  `panel`.
  |#
  (define rich-panel-subtitle
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-subtitle panel))))

  #|proc:rich-panel-subtitle-set!
  The `rich-panel-subtitle-set!` procedure sets the optional subtitle for
  `panel`.
  |#
  (define rich-panel-subtitle-set!
    (lambda (panel subtitle)
      (pcheck ([rich-panel? panel] [$rich-panel-title? subtitle])
              ($rich-panel-subtitle-set! panel subtitle))))

  #|proc:rich-panel-box
  The `rich-panel-box` procedure returns the box style for `panel`.
  |#
  (define rich-panel-box
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-box panel))))

  #|proc:rich-panel-box-set!
  The `rich-panel-box-set!` procedure sets the box style for `panel`.
  |#
  (define rich-panel-box-set!
    (lambda (panel box)
      (pcheck ([rich-panel? panel] [rich-box-style? box])
              ($rich-panel-box-set! panel box))))

  #|proc:rich-panel-padding
  The `rich-panel-padding` procedure returns the horizontal padding for
  `panel`.
  |#
  (define rich-panel-padding
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-padding panel))))

  #|proc:rich-panel-padding-set!
  The `rich-panel-padding-set!` procedure sets the non-negative horizontal
  padding for `panel`.
  |#
  (define rich-panel-padding-set!
    (lambda (panel padding)
      (pcheck ([rich-panel? panel] [rich-nonnegative-integer? padding])
              ($rich-panel-padding-set! panel padding))))

  #|proc:rich-panel-width
  The `rich-panel-width` procedure returns the optional total width for
  `panel`.
  |#
  (define rich-panel-width
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-width panel))))

  #|proc:rich-panel-width-set!
  The `rich-panel-width-set!` procedure sets the optional positive total width
  for `panel`.
  |#
  (define rich-panel-width-set!
    (lambda (panel width)
      (pcheck ([rich-panel? panel] [$rich-panel-size? width])
              ($rich-panel-width-set! panel width))))

  #|proc:rich-panel-height
  The `rich-panel-height` procedure returns the optional total height for
  `panel`.
  |#
  (define rich-panel-height
    (lambda (panel)
      (pcheck ([rich-panel? panel])
              ($rich-panel-height panel))))

  #|proc:rich-panel-height-set!
  The `rich-panel-height-set!` procedure sets the optional positive total height
  for `panel`.
  |#
  (define rich-panel-height-set!
    (lambda (panel height)
      (pcheck ([rich-panel? panel] [$rich-panel-size? height])
              ($rich-panel-height-set! panel height))))

  #|proc:rich-panel-render
  The `rich-panel-render` procedure returns bordered segment lines for `panel`. Default
  sizing wraps long content and reduces padding to fit the available width. If borders
  and complete content glyphs cannot fit together, content wraps without borders.
  |#
  (define rich-panel-render
    (lambda (panel)
      (pcheck ([rich-panel? panel])
        (parameterize ([rich-layout-render-cache
                        (or (rich-layout-render-cache) (make-eq-hashtable))])
          (let* ([fixed (rich-panel-width panel)]
                 [available (or fixed (rich-render-width))]
                 [initial-padding (if fixed (rich-panel-padding panel)
                                      (min (rich-panel-padding panel)
                                           (max 0 (quotient (- available 3) 2))))]
                 [initial-budget (max 1 (- available (* 2 initial-padding) 2))]
                 [measure-width (max initial-budget (if fixed 2 (min 2 available)))]
                 [raw (parameterize
                       ([rich-render-width measure-width])
                        ($panel-body-lines panel))]
                 [minimum (rich-lines-min-width raw)]
                 [padding (if fixed initial-padding
                              (min initial-padding (max 0 (quotient (- available 2 minimum) 2))))]
                 [budget (max 1 (- available (* 2 padding) 2))]
                 [unboxed? (and (not fixed) (< available (+ 2 minimum)))]
                 [child-width (cond [fixed (max budget minimum)]
                                    [unboxed? available]
                                    [else budget])]
                 [raw (if (= child-width measure-width) raw
                          (parameterize ([rich-render-width child-width])
                            ($panel-body-lines panel)))]
                 [lines (if fixed raw (rich-lines-wrap raw budget))]
                 [natural-total-width (+ ($segment-lines-width lines) (* padding 2) 2)]
                 [total-width (if fixed (max fixed natural-total-width)
                                  (min available natural-total-width))]
                 [content-width (max 0 (- total-width (* padding 2) 2))]
                 [natural-total-height (+ (length lines) 2)]
                 [total-height (if (rich-panel-height panel)
                                   (max (rich-panel-height panel) natural-total-height)
                                   natural-total-height)]
                 [chars (rich-box-chars (rich-panel-box panel))])
            (if unboxed?
                (rich-lines-wrap raw available)
                (append
                 (list ($border-line chars (vector-ref chars 0) (vector-ref chars 1)
                                     (- total-width 2)))
                 (map (lambda (line) ($content-line chars line content-width padding))
                      (append lines ($blank-lines (- total-height natural-total-height))))
                 (list ($border-line chars (vector-ref chars 2) (vector-ref chars 3)
                                     (- total-width 2))))))))))

  #|macro:rich-panel
  The `rich-panel` macro constructs and returns a panel. The `:body` field is
  required.
  |#
  (define-syntax rich-panel
    (lambda (stx)
      (define setter-action
        (lambda (name setter value)
          (with-syntax ([panel-name name]
                        [set-panel-field! setter]
                        [field-value value])
            #'(set-panel-field! panel-name field-value))))
      (define field-action
        (lambda (name field value)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:body) '()]
              [(:title) (list (setter-action name #'rich-panel-title-set! value))]
              [(:subtitle) (list (setter-action name #'rich-panel-subtitle-set! value))]
              [(:box) (list (setter-action name #'rich-panel-box-set! value))]
              [(:padding) (list (setter-action name #'rich-panel-padding-set! value))]
              [(:width) (list (setter-action name #'rich-panel-width-set! value))]
              [(:height) (list (setter-action name #'rich-panel-height-set! value))]
              [else (syntax-error field "invalid rich-panel field")]))))
      (define body-value
        (lambda (clause*)
          (let loop ([clause* clause*])
            (cond [(null? clause*) #f]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-panel form")]
                  [(eq? (syntax->datum (car clause*)) ':body)
                   (cadr clause*)]
                  [else (loop (cddr clause*))]))))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [action* '()])
            (cond [(null? clause*) (reverse action*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-panel form")]
                  [else
                   (let ([actions (field-action name (car clause*) (cadr clause*))])
                     (loop (cddr clause*) (rich-reverse-append actions action*)))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (let ([body (body-value #'(clause ...))])
           (unless body
             (syntax-error stx "rich-panel requires :body"))
           (with-syntax ([tmp (car (generate-temporaries #'(rich-panel)))]
                         [body-value body])
             (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
               #'(let ([tmp (make-rich-panel body-value)])
                   action ...
                   tmp))))]
        [_ (syntax-error stx "invalid rich-panel form")])))

  (rich-register-container-renderer! rich-panel? rich-panel-render))
