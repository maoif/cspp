#!chezscheme
(library (cspp rich basic)
  (export make-rich-rule
          rich-rule
          rich-rule?
          rich-rule-title
          rich-rule-title-set!
          rich-rule-width
          rich-rule-width-set!
          rich-rule-style
          rich-rule-style-set!
          rich-rule-render
          make-rich-padding
          rich-padding
          rich-padding?
          rich-padding-body
          rich-padding-body-set!
          rich-padding-top
          rich-padding-top-set!
          rich-padding-right
          rich-padding-right-set!
          rich-padding-bottom
          rich-padding-bottom-set!
          rich-padding-left
          rich-padding-left-set!
          rich-padding-render
          make-rich-align
          rich-align
          rich-align?
          rich-align-body
          rich-align-body-set!
          rich-align-width
          rich-align-width-set!
          rich-align-align
          rich-align-align-set!
          rich-align-render
          make-rich-columns
          rich-columns
          rich-columns?
          rich-columns-items
          rich-columns-items-set!
          rich-columns-width
          rich-columns-width-set!
          rich-columns-gap
          rich-columns-gap-set!
          rich-columns-render
          make-rich-layout
          rich-layout
          rich-layout?
          rich-layout-direction
          rich-layout-direction-set!
          rich-layout-items
          rich-layout-items-set!
          rich-layout-render)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich private lines)
          (cspp rich box)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich pretty))

  (define-record-type (rich-rule-record $make-rich-rule-record $rich-rule?)
    (fields (mutable title $rich-rule-title $rich-rule-title-set!)
            (mutable width $rich-rule-width $rich-rule-width-set!)
            (mutable style $rich-rule-style $rich-rule-style-set!)))

  (define-record-type (rich-padding-record $make-rich-padding-record $rich-padding?)
    (fields (mutable body $rich-padding-body $rich-padding-body-set!)
            (mutable top $rich-padding-top $rich-padding-top-set!)
            (mutable right $rich-padding-right $rich-padding-right-set!)
            (mutable bottom $rich-padding-bottom $rich-padding-bottom-set!)
            (mutable left $rich-padding-left $rich-padding-left-set!)))

  (define-record-type (rich-align-record $make-rich-align-record $rich-align?)
    (fields (mutable body $rich-align-body $rich-align-body-set!)
            (mutable width $rich-align-width $rich-align-width-set!)
            (mutable align $rich-align-align $rich-align-align-set!)))

  (define-record-type (rich-columns-record $make-rich-columns-record $rich-columns?)
    (fields (mutable items $rich-columns-items $rich-columns-items-set!)
            (mutable width $rich-columns-width $rich-columns-width-set!)
            (mutable gap $rich-columns-gap $rich-columns-gap-set!)))

  (define-record-type (rich-layout-record $make-rich-layout-record $rich-layout?)
    (fields (mutable direction $rich-layout-direction $rich-layout-direction-set!)
            (mutable items $rich-layout-items $rich-layout-items-set!)))

  (define $rich-rule-title?
    (lambda (x)
      (or (not x) (string? x))))

  (define $rich-align-value?
    (lambda (x)
      (memq x '(left center right))))

  (define $rich-layout-direction?
    (lambda (x)
      (memq x '(row column))))

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

  (define $rendered->segment-lines
    (lambda (who value)
      (cond [(string? value) ($string->segment-lines value)]
            [($rich-segment-line-list? value) value]
            [else (errorf who "renderer returned invalid value: ~a" value)])))

  (define $value->segment-lines
    (lambda (value)
      (cond [(rich-renderer-for value) =>
             (lambda (renderer)
               ($rendered->segment-lines 'rich-basic-render (renderer value)))]
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

  (define $make-space-segment
    (lambda (width)
      (rich-segment (make-string width #\space))))

  (define $pad-line
    (lambda (line left right)
      (append (if (zero? left) '() (list ($make-space-segment left)))
              line
              (if (zero? right) '() (list ($make-space-segment right))))))

  (define $blank-lines
    (lambda (count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            out
            (loop (- count 1) (cons '() out))))))

  (define $string-repeat
    (lambda (text count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            (apply string-append (reverse out))
            (loop (- count 1) (cons text out))))))

  (define $segment-lines-width rich-lines-width)

  (define $line-pad-right-to
    (lambda (line width)
      (let ([current (rich-segments-width line)])
        (if (>= current width)
            line
            (append line (list ($make-space-segment (- width current))))))))

  (define $columns-render-row
    (lambda (blocks gap)
      (let ([height (let loop ([blocks blocks] [height 0])
                      (if (null? blocks)
                          height
                          (loop (cdr blocks) (max height (length (caar blocks))))))]
            [gap-text (make-string gap #\space)])
        (let loop-lines ([i 0] [out '()])
          (if (= i height)
              (reverse out)
              (let loop-blocks ([blocks blocks] [first? #t] [pieces '()])
                (if (null? blocks)
                    (loop-lines (+ i 1)
                                (cons (apply append (reverse pieces)) out))
                    (let* ([lines (caar blocks)]
                           [width (cdar blocks)]
                           [line (if (< i (length lines)) (list-ref lines i) '())]
                           [last? (null? (cdr blocks))]
                           [padded (if last? line ($line-pad-right-to line width))])
                      (loop-blocks (cdr blocks)
                                   #f
                                   (cons padded
                                         (if first?
                                             pieces
                                             (cons (list (rich-segment gap-text))
                                                   pieces))))))))))))

  (define $column-block
    (lambda (item)
      (let ([lines (rich-lines-wrap ($value->segment-lines item) (rich-render-width))])
        (cons lines ($segment-lines-width lines)))))

  (define $pack-column-blocks
    (lambda (blocks width gap)
      (let loop ([blocks blocks]
                 [row '()]
                 [row-width 0]
                 [rows '()])
        (cond [(null? blocks)
               (reverse (if (null? row) rows (cons (reverse row) rows)))]
              [else
               (let* ([block (car blocks)]
                      [block-width (cdr block)]
                      [next-width (if (null? row)
                                      block-width
                                      (+ row-width gap block-width))])
                 (if (and (not (null? row)) (> next-width width))
                     (loop blocks '() 0 (cons (reverse row) rows))
                     (loop (cdr blocks)
                           (cons block row)
                           next-width
                           rows)))]))))

  (define $render-horizontal-blocks
    (lambda (blocks gap)
      (apply append
             (map (lambda (row) ($columns-render-row row gap))
                  ($pack-column-blocks blocks (most-positive-fixnum) gap)))))

  (define $items->blocks
    (lambda (items)
      (map $column-block items)))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Rule
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-rule?
  The `rich-rule?` procedure returns `#t` when its argument is a rich rule
  object, and `#f` otherwise.
  |#
  (define rich-rule?
    (lambda (x)
      ($rich-rule? x)))

  #|proc:make-rich-rule
  The `make-rich-rule` procedure constructs a horizontal rule with an optional
  string `title` (or `#f`), optional `width`, and box `style`, and returns the rule.
  The default `width` is `#f`, which fills the available content width at render time.
  A positive exact integer keeps a fixed width, including when nested in a container.
  |#
  (define make-rich-rule
    (case-lambda
      [()
       (make-rich-rule #f #f 'ascii)]
      [(title)
       (make-rich-rule title #f 'ascii)]
      [(title width)
       (make-rich-rule title width 'ascii)]
      [(title width style)
       (pcheck ([$rich-rule-title? title]
                [rich-optional-width? width]
                [rich-box-style? style])
               ($make-rich-rule-record title width style))]))

  #|proc:rich-rule-title
  The `rich-rule-title` procedure returns the optional title for `rule`.
  |#
  (define rich-rule-title
    (lambda (rule)
      (pcheck ([rich-rule? rule])
              ($rich-rule-title rule))))

  #|proc:rich-rule-title-set!
  The `rich-rule-title-set!` procedure sets the optional title for `rule`.
  |#
  (define rich-rule-title-set!
    (lambda (rule title)
      (pcheck ([rich-rule? rule] [$rich-rule-title? title])
              ($rich-rule-title-set! rule title))))

  #|proc:rich-rule-width
  The `rich-rule-width` procedure returns the configured width for `rule`: `#f` for
  automatic sizing or a positive exact integer for a fixed width.
  |#
  (define rich-rule-width
    (lambda (rule)
      (pcheck ([rich-rule? rule])
              ($rich-rule-width rule))))

  #|proc:rich-rule-width-set!
  The `rich-rule-width-set!` procedure sets `rule` to positive exact integer `width`,
  or `#f` to fill the available content width. Returns an unspecified value.
  |#
  (define rich-rule-width-set!
    (lambda (rule width)
      (pcheck ([rich-rule? rule] [rich-optional-width? width])
              ($rich-rule-width-set! rule width))))

  #|proc:rich-rule-style
  The `rich-rule-style` procedure returns the box style for `rule`.
  |#
  (define rich-rule-style
    (lambda (rule)
      (pcheck ([rich-rule? rule])
              ($rich-rule-style rule))))

  #|proc:rich-rule-style-set!
  The `rich-rule-style-set!` procedure sets the box style for `rule`.
  |#
  (define rich-rule-style-set!
    (lambda (rule style)
      (pcheck ([rich-rule? rule] [rich-box-style? style])
              ($rich-rule-style-set! rule style))))

  #|proc:rich-rule-render
  The `rich-rule-render` procedure renders `rule` as one segment line.
  |#
  (define rich-rule-render
    (lambda (rule)
      (pcheck ([rich-rule? rule])
              (let* ([chars (rich-box-chars (rich-rule-style rule))]
                     [h (vector-ref chars 4)]
                     [title (rich-rule-title rule)]
                     [width (or (rich-rule-width rule) (rich-render-width))]
                     [text (if (and title (not (string=? title "")))
                               (let* ([title-text (string-append " " title " ")]
                                      [title-width (rich-segment-width (rich-segment title-text))])
                                 (if (>= title-width width)
                                     (rich-segments->plain
                                      (rich-segment-pad-right
                                       (rich-segment-crop (list (rich-segment title-text)) width)
                                       width))
                                     (let* ([space (- width title-width)]
                                            [left (quotient space 2)]
                                            [right (- space left)])
                                       (string-append ($string-repeat h left)
                                                      title-text
                                                      ($string-repeat h right)))))
                               ($string-repeat h width))])
                (list (list (rich-segment text)))))))

  #|macro:rich-rule
  The `rich-rule` macro constructs and returns a rule.
  |#
  (define-syntax rich-rule
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:title) #'rich-rule-title-set!]
              [(:width) #'rich-rule-width-set!]
              [(:style) #'rich-rule-style-set!]
              [else (syntax-error field "invalid rich-rule field")]))))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-rule form")]
                  [else
                   (let ([setter (field->setter (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([rule-name name]
                                   [set-rule-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-rule-field! rule-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-rule)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-rule)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-rule form")])))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Padding
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-padding?
  The `rich-padding?` procedure returns `#t` when its argument is a rich padding
  object, and `#f` otherwise.
  |#
  (define rich-padding?
    (lambda (x)
      ($rich-padding? x)))

  #|proc:make-rich-padding
  The `make-rich-padding` procedure constructs a padding renderable around
  `body` with non-negative top, right, bottom, and left padding.
  |#
  (define make-rich-padding
    (case-lambda
      [()
       (make-rich-padding "" 0 0 0 0)]
      [(body)
       (make-rich-padding body 0 0 0 0)]
      [(body top right bottom left)
       (pcheck ([rich-nonnegative-integer? top right bottom left])
               ($make-rich-padding-record body top right bottom left))]))

  #|proc:rich-padding-body
  The `rich-padding-body` procedure returns the body for `padding`.
  |#
  (define rich-padding-body
    (lambda (padding)
      (pcheck ([rich-padding? padding])
              ($rich-padding-body padding))))

  #|proc:rich-padding-body-set!
  The `rich-padding-body-set!` procedure sets the body for `padding`.
  |#
  (define rich-padding-body-set!
    (lambda (padding body)
      (pcheck ([rich-padding? padding])
              ($rich-padding-body-set! padding body))))

  #|proc:rich-padding-top
  The `rich-padding-top` procedure returns the top padding for `padding`.
  |#
  (define rich-padding-top
    (lambda (padding)
      (pcheck ([rich-padding? padding])
              ($rich-padding-top padding))))

  #|proc:rich-padding-top-set!
  The `rich-padding-top-set!` procedure sets the top padding for `padding`.
  |#
  (define rich-padding-top-set!
    (lambda (padding top)
      (pcheck ([rich-padding? padding] [rich-nonnegative-integer? top])
              ($rich-padding-top-set! padding top))))

  #|proc:rich-padding-right
  The `rich-padding-right` procedure returns the right padding for `padding`.
  |#
  (define rich-padding-right
    (lambda (padding)
      (pcheck ([rich-padding? padding])
              ($rich-padding-right padding))))

  #|proc:rich-padding-right-set!
  The `rich-padding-right-set!` procedure sets the right padding for `padding`.
  |#
  (define rich-padding-right-set!
    (lambda (padding right)
      (pcheck ([rich-padding? padding] [rich-nonnegative-integer? right])
              ($rich-padding-right-set! padding right))))

  #|proc:rich-padding-bottom
  The `rich-padding-bottom` procedure returns the bottom padding for `padding`.
  |#
  (define rich-padding-bottom
    (lambda (padding)
      (pcheck ([rich-padding? padding])
              ($rich-padding-bottom padding))))

  #|proc:rich-padding-bottom-set!
  The `rich-padding-bottom-set!` procedure sets the bottom padding for
  `padding`.
  |#
  (define rich-padding-bottom-set!
    (lambda (padding bottom)
      (pcheck ([rich-padding? padding] [rich-nonnegative-integer? bottom])
              ($rich-padding-bottom-set! padding bottom))))

  #|proc:rich-padding-left
  The `rich-padding-left` procedure returns the left padding for `padding`.
  |#
  (define rich-padding-left
    (lambda (padding)
      (pcheck ([rich-padding? padding])
              ($rich-padding-left padding))))

  #|proc:rich-padding-left-set!
  The `rich-padding-left-set!` procedure sets the left padding for `padding`.
  |#
  (define rich-padding-left-set!
    (lambda (padding left)
      (pcheck ([rich-padding? padding] [rich-nonnegative-integer? left])
              ($rich-padding-left-set! padding left))))

  #|proc:rich-padding-render
  The `rich-padding-render` procedure returns segment lines for `padding`, wrapping its
  body to the available width. Horizontal padding shrinks to leave room for complete glyphs.
  |#
  (define rich-padding-render
    (lambda (padding)
      (pcheck ([rich-padding? padding])
        (parameterize ([rich-layout-render-cache
                        (or (rich-layout-render-cache) (make-eq-hashtable))])
          (let* ([available (rich-render-width)]
                 [left (min (rich-padding-left padding) (- available 1))]
                 [right (min (rich-padding-right padding) (- available left 1))]
                 [width (- available left right)]
                 [measure-width (max width (min 2 available))]
                 [raw (parameterize ([rich-render-width measure-width])
                        ($value->segment-lines (rich-padding-body padding)))]
                 [minimum (min available (rich-lines-min-width raw))]
                 [left (min left (- available minimum))]
                 [right (min right (- available left minimum))]
                 [child-width (- available left right)]
                 [raw (if (= child-width measure-width) raw
                          (parameterize ([rich-render-width child-width])
                            ($value->segment-lines (rich-padding-body padding))))]
                 [lines (rich-lines-wrap raw child-width)])
            (append ($blank-lines (rich-padding-top padding))
                    (map (lambda (line) ($pad-line line left right)) lines)
                    ($blank-lines (rich-padding-bottom padding))))))))

  #|macro:rich-padding
  The `rich-padding` macro constructs and returns a padding renderable.
  |#
  (define-syntax rich-padding
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:body) #'rich-padding-body-set!]
              [(:top) #'rich-padding-top-set!]
              [(:right) #'rich-padding-right-set!]
              [(:bottom) #'rich-padding-bottom-set!]
              [(:left) #'rich-padding-left-set!]
              [else (syntax-error field "invalid rich-padding field")]))))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-padding form")]
                  [else
                   (let ([setter (field->setter (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([padding-name name]
                                   [set-padding-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-padding-field! padding-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-padding)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-padding)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-padding form")])))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Alignment
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-align?
  The `rich-align?` procedure returns `#t` when its argument is a rich alignment
  object, and `#f` otherwise.
  |#
  (define rich-align?
    (lambda (x)
      ($rich-align? x)))

  #|proc:make-rich-align
  The `make-rich-align` procedure returns a renderable around `body`. Optional `width`
  is a positive cell count or `#f` (default) to use the available content width.
  The `align` mode is `left` (default), `center`, or `right`.
  |#
  (define make-rich-align
    (case-lambda
      [()
       (make-rich-align "" #f 'left)]
      [(body)
       (make-rich-align body #f 'left)]
      [(body width align)
       (pcheck ([rich-optional-width? width] [$rich-align-value? align])
               ($make-rich-align-record body width align))]))

  #|proc:rich-align-body
  The `rich-align-body` procedure returns the body for `align`.
  |#
  (define rich-align-body
    (lambda (align)
      (pcheck ([rich-align? align])
              ($rich-align-body align))))

  #|proc:rich-align-body-set!
  The `rich-align-body-set!` procedure sets the body for `align`.
  |#
  (define rich-align-body-set!
    (lambda (align body)
      (pcheck ([rich-align? align])
              ($rich-align-body-set! align body))))

  #|proc:rich-align-width
  The `rich-align-width` procedure returns the configured width for `align`: `#f` for
  automatic sizing or a positive cell count.
  |#
  (define rich-align-width
    (lambda (align)
      (pcheck ([rich-align? align])
              ($rich-align-width align))))

  #|proc:rich-align-width-set!
  The `rich-align-width-set!` procedure sets `align` to positive cell count `width`,
  or `#f` to use the available content width. Returns an unspecified value.
  |#
  (define rich-align-width-set!
    (lambda (align width)
      (pcheck ([rich-align? align] [rich-optional-width? width])
              ($rich-align-width-set! align width))))

  #|proc:rich-align-align
  The `rich-align-align` procedure returns the alignment mode for `align`.
  |#
  (define rich-align-align
    (lambda (align)
      (pcheck ([rich-align? align])
              ($rich-align-align align))))

  #|proc:rich-align-align-set!
  The `rich-align-align-set!` procedure sets the alignment mode for `align`.
  |#
  (define rich-align-align-set!
    (lambda (align align-value)
      (pcheck ([rich-align? align] [$rich-align-value? align-value])
              ($rich-align-align-set! align align-value))))

  #|proc:rich-align-render
  The `rich-align-render` procedure returns segment lines for `align`, wrapping long
  content within its configured or available width before applying alignment.
  |#
  (define rich-align-render
    (lambda (align)
      (pcheck ([rich-align? align])
              (let ([width (or (rich-align-width align) (rich-render-width))]
                    [align-value (rich-align-align align)])
                (map (lambda (line)
                       (let ([current (rich-segments-width line)])
                         (if (>= current width)
                             line
                             (let ([space (- width current)])
                               (case align-value
                                 [(left) ($pad-line line 0 space)]
                                 [(right) ($pad-line line space 0)]
                                 [(center)
                                  (let ([left (quotient space 2)])
                                    ($pad-line line left (- space left)))])))))
                     (parameterize ([rich-render-width width])
                       (rich-lines-wrap ($value->segment-lines (rich-align-body align)) width)))))))

  #|macro:rich-align
  The `rich-align` macro constructs and returns an alignment renderable.
  |#
  (define-syntax rich-align
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:body) #'rich-align-body-set!]
              [(:width) #'rich-align-width-set!]
              [(:align) #'rich-align-align-set!]
              [else (syntax-error field "invalid rich-align field")]))))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-align form")]
                  [else
                   (let ([setter (field->setter (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([align-name name]
                                   [set-align-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-align-field! align-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-align)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-align)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-align form")])))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Columns
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-columns?
  The `rich-columns?` procedure returns `#t` when its argument is a rich columns
  object, and `#f` otherwise.
  |#
  (define rich-columns?
    (lambda (x)
      ($rich-columns? x)))

  #|proc:make-rich-columns
  The `make-rich-columns` procedure returns a column flow renderable from list `items`.
  Optional `width` is a positive cell count or `#f` (default) for the available width.
  The nonnegative cell count `gap` separates adjacent items and defaults to two.
  |#
  (define make-rich-columns
    (case-lambda
      [()
       (make-rich-columns '() #f 2)]
      [(items)
       (make-rich-columns items #f 2)]
      [(items width gap)
       (pcheck ([list? items]
                [rich-optional-width? width]
                [rich-nonnegative-integer? gap])
               ($make-rich-columns-record items width gap))]))

  #|proc:rich-columns-items
  The `rich-columns-items` procedure returns the items for `columns`.
  |#
  (define rich-columns-items
    (lambda (columns)
      (pcheck ([rich-columns? columns])
              ($rich-columns-items columns))))

  #|proc:rich-columns-items-set!
  The `rich-columns-items-set!` procedure sets the items for `columns`.
  |#
  (define rich-columns-items-set!
    (lambda (columns items)
      (pcheck ([rich-columns? columns] [list? items])
              ($rich-columns-items-set! columns items))))

  #|proc:rich-columns-width
  The `rich-columns-width` procedure returns the configured flow width for `columns`:
  `#f` for automatic sizing or a positive cell count.
  |#
  (define rich-columns-width
    (lambda (columns)
      (pcheck ([rich-columns? columns])
              ($rich-columns-width columns))))

  #|proc:rich-columns-width-set!
  The `rich-columns-width-set!` procedure sets `columns` to positive cell count `width`,
  or `#f` to use the available content width. Returns an unspecified value.
  |#
  (define rich-columns-width-set!
    (lambda (columns width)
      (pcheck ([rich-columns? columns] [rich-optional-width? width])
              ($rich-columns-width-set! columns width))))

  #|proc:rich-columns-gap
  The `rich-columns-gap` procedure returns the gap between columns.
  |#
  (define rich-columns-gap
    (lambda (columns)
      (pcheck ([rich-columns? columns])
              ($rich-columns-gap columns))))

  #|proc:rich-columns-gap-set!
  The `rich-columns-gap-set!` procedure sets the non-negative gap between
  columns.
  |#
  (define rich-columns-gap-set!
    (lambda (columns gap)
      (pcheck ([rich-columns? columns] [rich-nonnegative-integer? gap])
              ($rich-columns-gap-set! columns gap))))

  #|proc:rich-columns-render
  The `rich-columns-render` procedure returns packed column rows for `columns`, wrapping
  oversized items within the configured or available width.
  |#
  (define rich-columns-render
    (lambda (columns)
      (pcheck ([rich-columns? columns])
        (let ([width (or (rich-columns-width columns) (rich-render-width))])
          (apply append
                 (map (lambda (row)
                        ($columns-render-row row (rich-columns-gap columns)))
                      ($pack-column-blocks
                       (parameterize ([rich-render-width (min (rich-render-width) width)])
                         ($items->blocks (rich-columns-items columns)))
                       width
                       (rich-columns-gap columns))))))))

  #|macro:rich-columns
  The `rich-columns` macro constructs and returns a columns renderable. The
  `:items` field accepts an expression or a parenthesized literal item list such as
  `("a" "b")`. A form beginning with an identifier is evaluated as an expression;
  use `(list first second)` to collect item variables. Returns the new renderable.
  |#
  (define-syntax rich-columns
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:items) #'rich-columns-items-set!]
              [(:width) #'rich-columns-width-set!]
              [(:gap) #'rich-columns-gap-set!]
              [else (syntax-error field "invalid rich-columns field")]))))
      (define field-value
        (lambda (field value)
          (if (eq? (syntax->datum field) ':items)
              (syntax-case value ()
                [() #''()]
                [(head item ...)
                 (let ([head-datum (syntax->datum #'head)])
                   (if (symbol? head-datum)
                       value
                       (with-syntax ([first-item #'head])
                         #'(list first-item item ...))))]
                [_ value])
              value)))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-columns form")]
                  [else
                   (let* ([field (car clause*)]
                          [setter (field->setter field)]
                          [value (field-value field (cadr clause*))])
                     (with-syntax ([columns-name name]
                                   [set-columns-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-columns-field! columns-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-columns)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-columns)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-columns form")])))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Layout
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-layout?
  The `rich-layout?` procedure returns `#t` when its argument is a rich layout
  object, and `#f` otherwise.
  |#
  (define rich-layout?
    (lambda (x)
      ($rich-layout? x)))

  #|proc:make-rich-layout
  The `make-rich-layout` procedure constructs a layout renderable with
  direction `row` or `column` and a list of `items`.
  |#
  (define make-rich-layout
    (case-lambda
      [()
       (make-rich-layout 'column '())]
      [(direction items)
       (pcheck ([$rich-layout-direction? direction] [list? items])
               ($make-rich-layout-record direction items))]))

  #|proc:rich-layout-direction
  The `rich-layout-direction` procedure returns the direction for `layout`.
  |#
  (define rich-layout-direction
    (lambda (layout)
      (pcheck ([rich-layout? layout])
              ($rich-layout-direction layout))))

  #|proc:rich-layout-direction-set!
  The `rich-layout-direction-set!` procedure sets the direction for `layout`.
  |#
  (define rich-layout-direction-set!
    (lambda (layout direction)
      (pcheck ([rich-layout? layout] [$rich-layout-direction? direction])
              ($rich-layout-direction-set! layout direction))))

  #|proc:rich-layout-items
  The `rich-layout-items` procedure returns the items for `layout`.
  |#
  (define rich-layout-items
    (lambda (layout)
      (pcheck ([rich-layout? layout])
              ($rich-layout-items layout))))

  #|proc:rich-layout-items-set!
  The `rich-layout-items-set!` procedure sets the items for `layout`.
  |#
  (define rich-layout-items-set!
    (lambda (layout items)
      (pcheck ([rich-layout? layout] [list? items])
              ($rich-layout-items-set! layout items))))

  #|proc:rich-layout-render
  The `rich-layout-render` procedure returns segment lines for `layout`. Row children
  share the available width; long content wraps within each share. If complete glyphs
  and gaps cannot fit within those shares, children stack vertically. Column layouts wrap children
  within the whole available width.
  |#
  (define rich-layout-render
    (lambda (layout)
      (pcheck ([rich-layout? layout])
        (parameterize ([rich-layout-render-cache
                        (or (rich-layout-render-cache) (make-eq-hashtable))])
          (let* ([items (rich-layout-items layout)] [count (length items)]
                 [available (rich-render-width)]
                 [row? (and (eq? (rich-layout-direction layout) 'row)
                            (>= available (- (* 2 count) 1)))]
                 [width (if (and row? (positive? count))
                            (max 1 (quotient (- available (- count 1)) count)) available)]
                 [measure-width (max width (min 2 available))]
                 [raw (parameterize ([rich-render-width measure-width])
                        (map $value->segment-lines items))]
                 [row? (and row? (andmap (lambda (lines) (<= (rich-lines-min-width lines) width))
                                        raw))]
                 [child-width (if row? width available)]
                 [raw (if (= child-width measure-width) raw
                          (parameterize ([rich-render-width child-width])
                            (map $value->segment-lines items)))])
            (if row?
                ($render-horizontal-blocks
                 (map (lambda (lines)
                        (let ([lines (rich-lines-wrap lines width)])
                          (cons lines ($segment-lines-width lines)))) raw) 1)
                (apply append (map (lambda (lines) (rich-lines-wrap lines available)) raw))))))))

  #|macro:rich-layout
  The `rich-layout` macro constructs and returns a layout renderable. The
  `:items` field accepts an expression or a parenthesized literal item list such as
  `("a" "b")`. A form beginning with an identifier is evaluated as an expression;
  use `(list first second)` to collect item variables. Returns the new renderable.
  |#
  (define-syntax rich-layout
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:direction) #'rich-layout-direction-set!]
              [(:items) #'rich-layout-items-set!]
              [else (syntax-error field "invalid rich-layout field")]))))
      (define field-value
        (lambda (field value)
          (if (eq? (syntax->datum field) ':items)
              (syntax-case value ()
                [() #''()]
                [(head item ...)
                 (let ([head-datum (syntax->datum #'head)])
                   (if (symbol? head-datum)
                       value
                       (with-syntax ([first-item #'head])
                         #'(list first-item item ...))))]
                [_ value])
              value)))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-layout form")]
                  [else
                   (let* ([field (car clause*)]
                          [setter (field->setter field)]
                          [value (field-value field (cadr clause*))])
                     (with-syntax ([layout-name name]
                                   [set-layout-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-layout-field! layout-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-layout)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-layout)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-layout form")])))

  (rich-register-container-renderer! rich-rule? rich-rule-render)
  (rich-register-container-renderer! rich-padding? rich-padding-render)
  (rich-register-container-renderer! rich-align? rich-align-render)
  (rich-register-container-renderer! rich-columns? rich-columns-render)
  (rich-register-container-renderer! rich-layout? rich-layout-render))
