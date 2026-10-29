#!chezscheme
(library (cspp rich style)
  (export rich-style
          rich-style?
          rich-reset?
          reset-style
          rich-style-fg
          rich-style-bg
          rich-style-bold?
          rich-style-dim?
          rich-style-italic?
          rich-style-underline?
          rich-style-blink?
          rich-style-reverse?
          rich-style-hidden?
          rich-style-strike?
          rich-color-system?
          rich-style->ansi
          rich-ansi-reset
          rich-theme
          make-rich-theme
          rich-theme?
          rich-theme-ref
          rich-theme-set!
          ;; Internal composition support, omitted from the rich facade.
          rich-style-overlay
          rich-style->ansi/context)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common))

  (define rich-ansi-reset "\033[0m")

  ;; Attribute fields are #f (unspecified), #t (enabled), or off (explicitly disabled).
  (define-record-type (rich-style-record make-rich-style-record $rich-style?)
    (opaque #t)
    (fields (immutable fg $rich-style-fg)
            (immutable bg $rich-style-bg)
            (immutable bold? $rich-style-bold?)
            (immutable dim? $rich-style-dim?)
            (immutable italic? $rich-style-italic?)
            (immutable underline? $rich-style-underline?)
            (immutable blink? $rich-style-blink?)
            (immutable reverse? $rich-style-reverse?)
            (immutable hidden? $rich-style-hidden?)
            (immutable strike? $rich-style-strike?)))

  (define-record-type (rich-reset-record make-rich-reset-record $rich-reset?)
    (opaque #t)
    (fields))

  (define $named-fg
    '((black . 30) (red . 31) (green . 32) (yellow . 33)
      (blue . 34) (magenta . 35) (cyan . 36) (white . 37)
      (default . 39)
      (bright-black . 90) (bright-red . 91) (bright-green . 92)
      (bright-yellow . 93) (bright-blue . 94) (bright-magenta . 95)
      (bright-cyan . 96) (bright-white . 97)))

  (define $named-bg
    '((on-black . 40) (on-red . 41) (on-green . 42) (on-yellow . 43)
      (on-blue . 44) (on-magenta . 45) (on-cyan . 46) (on-white . 47)
      (on-default . 49)
      (on-bright-black . 100) (on-bright-red . 101)
      (on-bright-green . 102) (on-bright-yellow . 103)
      (on-bright-blue . 104) (on-bright-magenta . 105)
      (on-bright-cyan . 106) (on-bright-white . 107)))

  (define $color-ref
    (lambda (who table name)
      (let ([a (assq name table)])
        (and a (cdr a)))))

  (define $rgb-red
    (lambda (rgb)
      (fxsrl rgb 16)))

  (define $rgb-green
    (lambda (rgb)
      (fxlogand (fxsrl rgb 8) #xff)))

  (define $rgb-blue
    (lambda (rgb)
      (fxlogand rgb #xff)))

  (define $sgr
    (lambda (code)
      (string-append "\033[" (number->string code) "m")))

  (define $rgb-sgr
    (lambda (prefix rgb)
      (string-append "\033["
                     prefix
                     ";2;"
                     (number->string ($rgb-red rgb))
                     ";"
                     (number->string ($rgb-green rgb))
                     ";"
                     (number->string ($rgb-blue rgb))
                     "m")))

  (define $apply-symbol-style
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (case opt
        [(bold) (values fg bg #t dim? italic? underline? blink? reverse? hidden? strike?)]
        [(no-bold) (values fg bg 'off dim? italic? underline? blink? reverse? hidden? strike?)]
        [(dim) (values fg bg bold? #t italic? underline? blink? reverse? hidden? strike?)]
        [(no-dim) (values fg bg bold? 'off italic? underline? blink? reverse? hidden? strike?)]
        [(italic) (values fg bg bold? dim? #t underline? blink? reverse? hidden? strike?)]
        [(no-italic) (values fg bg bold? dim? 'off underline? blink? reverse? hidden? strike?)]
        [(underline) (values fg bg bold? dim? italic? #t blink? reverse? hidden? strike?)]
        [(no-underline) (values fg bg bold? dim? italic? 'off blink? reverse? hidden? strike?)]
        [(blink) (values fg bg bold? dim? italic? underline? #t reverse? hidden? strike?)]
        [(no-blink) (values fg bg bold? dim? italic? underline? 'off reverse? hidden? strike?)]
        [(reverse) (values fg bg bold? dim? italic? underline? blink? #t hidden? strike?)]
        [(no-reverse) (values fg bg bold? dim? italic? underline? blink? 'off hidden? strike?)]
        [(hidden) (values fg bg bold? dim? italic? underline? blink? reverse? #t strike?)]
        [(no-hidden) (values fg bg bold? dim? italic? underline? blink? reverse? 'off strike?)]
        [(strike) (values fg bg bold? dim? italic? underline? blink? reverse? hidden? #t)]
        [(no-strike) (values fg bg bold? dim? italic? underline? blink? reverse? hidden? 'off)]
        [else
         (let ([fg-code ($color-ref 'rich-style $named-fg opt)]
               [bg-code ($color-ref 'rich-style $named-bg opt)])
           (cond [fg-code (values opt bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
                 [bg-code (values fg opt bold? dim? italic? underline? blink? reverse? hidden? strike?)]
                 [else (errorf 'rich-style "unknown style option: ~a" opt)]))])))

  (define $apply-color-form
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (if (and (pair? opt) (pair? (cdr opt)) (null? (cddr opt)))
          (let ([kind (car opt)] [rgb (cadr opt)])
            (unless (rich-rgb-integer? rgb)
              (errorf 'rich-style "invalid RGB color: ~a" rgb))
            (case kind
              [(fg) (values rgb bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
              [(bg) (values fg rgb bold? dim? italic? underline? blink? reverse? hidden? strike?)]
              [else (errorf 'rich-style "invalid color form: ~a" opt)]))
          (errorf 'rich-style "invalid color form: ~a" opt))))

  (define $apply-style-option
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (cond [(symbol? opt)
             ($apply-symbol-style opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
            [(integer? opt)
             (if (rich-rgb-integer? opt)
                 (values opt bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
                 (errorf 'rich-style "invalid RGB color: ~a" opt))]
            [(pair? opt)
             ($apply-color-form opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
            [else (errorf 'rich-style "invalid style option: ~a" opt)])))

  (define $resolve-style
    (lambda (opts)
      (let loop ([opts opts]
                 [fg #f] [bg #f]
                 [bold? #f] [dim? #f] [italic? #f] [underline? #f]
                 [blink? #f] [reverse? #f] [hidden? #f] [strike? #f])
        (if (null? opts)
            (make-rich-style-record fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
            (let-values ([(fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
                          ($apply-style-option (car opts) fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)])
              (loop (cdr opts) fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?))))))

  #|proc:rich-style?
  The `rich-style?` procedure returns `#t` when its argument is a rich style
  object, and `#f` otherwise.
  |#
  (define rich-style?
    (lambda (x)
      ($rich-style? x)))

  #|proc:rich-reset?
  The `rich-reset?` procedure returns `#t` when its argument is a rich style
  reset object, and `#f` otherwise.
  |#
  (define rich-reset?
    (lambda (x)
      ($rich-reset? x)))

  #|proc:rich-style-fg
  The `rich-style-fg` procedure returns the resolved foreground color of
  `style`, or `#f` when none is set.
  |#
  (define rich-style-fg
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-fg style))))

  #|proc:rich-style-bg
  The `rich-style-bg` procedure returns the resolved background color of
  `style`, or `#f` when none is set.
  |#
  (define rich-style-bg
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-bg style))))

  #|proc:rich-style-bold?
  The `rich-style-bold?` procedure returns whether `style` enables bold text.
  |#
  (define rich-style-bold?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-bold? style) #t))))

  #|proc:rich-style-dim?
  The `rich-style-dim?` procedure returns whether `style` enables dim text.
  |#
  (define rich-style-dim?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-dim? style) #t))))

  #|proc:rich-style-italic?
  The `rich-style-italic?` procedure returns whether `style` enables italic
  text.
  |#
  (define rich-style-italic?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-italic? style) #t))))

  #|proc:rich-style-underline?
  The `rich-style-underline?` procedure returns whether `style` enables
  underlined text.
  |#
  (define rich-style-underline?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-underline? style) #t))))

  #|proc:rich-style-blink?
  The `rich-style-blink?` procedure returns whether `style` enables blinking
  text.
  |#
  (define rich-style-blink?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-blink? style) #t))))

  #|proc:rich-style-reverse?
  The `rich-style-reverse?` procedure returns whether `style` enables reversed
  text.
  |#
  (define rich-style-reverse?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-reverse? style) #t))))

  #|proc:rich-style-hidden?
  The `rich-style-hidden?` procedure returns whether `style` enables hidden
  text.
  |#
  (define rich-style-hidden?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-hidden? style) #t))))

  #|proc:rich-style-strike?
  The `rich-style-strike?` procedure returns whether `style` enables
  strikethrough text.
  |#
  (define rich-style-strike?
    (lambda (style)
      (pcheck ([rich-style? style])
              (eq? ($rich-style-strike? style) #t))))

  #|proc:reset-style
  The `reset-style` procedure returns an opaque object that resets the active
  rich style when printed by rich output procedures.
  |#
  (define reset-style
    (lambda ()
      (make-rich-reset-record)))

  #|proc:rich-style
  The `rich-style` procedure constructs an opaque rich style object from style
  symbols and RGB color forms in `opts`, returning the new style. Attribute symbols
  enable attributes; no-* symbols explicitly disable them. Later options override earlier
  options. Attributes omitted from `opts` inherit the active style when printed.
  |#
  (define rich-style
    (lambda opts
      (pcheck ([list? opts])
              ($resolve-style opts))))

  #|proc:rich-color-system?
  The `rich-color-system?` procedure returns `#t` for supported rich color
  system names, and `#f` otherwise.
  |#
  (define rich-color-system?
    (lambda (x)
      (and (memq x '(auto none standard 256 truecolor)) #t)))

  (define $rich-color-system-option?
    (lambda (x)
      (or (not x) (rich-color-system? x))))

  (define $color->ansi
    (lambda (fg? color)
      (cond [(integer? color)
             ($rgb-sgr (if fg? "38" "48") color)]
            [(symbol? color)
             (let ([code ($color-ref 'rich-style (if fg? $named-fg $named-bg) color)])
               (if code ($sgr code) ""))]
            [else ""])))

  #|proc:rich-style-overlay
  The internal `rich-style-overlay` returns `incoming` overlaid on `active`, a style or
  `#f`. Unspecified attributes inherit; explicit disabled attributes override active ones.
  |#
  (define rich-style-overlay
    (lambda (active incoming)
      (pcheck ([$rich-style-or-false? active] [rich-style? incoming])
        (if (not active)
            incoming
            (make-rich-style-record
             (or ($rich-style-fg incoming) ($rich-style-fg active))
             (or ($rich-style-bg incoming) ($rich-style-bg active))
             (or ($rich-style-bold? incoming) ($rich-style-bold? active))
             (or ($rich-style-dim? incoming) ($rich-style-dim? active))
             (or ($rich-style-italic? incoming) ($rich-style-italic? active))
             (or ($rich-style-underline? incoming) ($rich-style-underline? active))
             (or ($rich-style-blink? incoming) ($rich-style-blink? active))
             (or ($rich-style-reverse? incoming) ($rich-style-reverse? active))
             (or ($rich-style-hidden? incoming) ($rich-style-hidden? active))
             (or ($rich-style-strike? incoming) ($rich-style-strike? active)))))))

  (define $rich-style-or-false?
    (lambda (value) (or (not value) (rich-style? value))))

  #|proc:rich-style->ansi/context
  The internal `rich-style->ansi/context` encodes `style` for `color-system` over the
  current style `active` (or `#f`). It preserves bold or dim when disabling the other.
  Returns an ANSI string; the none color system returns an empty string.
  |#
  (define rich-style->ansi/context
    (lambda (color-system style active)
      (pcheck ([$rich-color-system-option? color-system] [rich-style? style]
               [$rich-style-or-false? active])
        (if (eq? color-system 'none)
            ""
            (let* ([intensity-reset? (or (eq? ($rich-style-bold? style) 'off)
                                         (eq? ($rich-style-dim? style) 'off))]
                   [intensity (if intensity-reset? (rich-style-overlay active style) style)])
              (string-append
               (if intensity-reset? ($sgr 22) "")
               (if (rich-style-bold? intensity) ($sgr 1) "")
               (if (rich-style-dim? intensity) ($sgr 2) "")
               (case ($rich-style-italic? style)
                 [(#t) ($sgr 3)] [(off) ($sgr 23)] [else ""])
               (case ($rich-style-underline? style)
                 [(#t) ($sgr 4)] [(off) ($sgr 24)] [else ""])
               (case ($rich-style-blink? style)
                 [(#t) ($sgr 5)] [(off) ($sgr 25)] [else ""])
               (case ($rich-style-reverse? style)
                 [(#t) ($sgr 7)] [(off) ($sgr 27)] [else ""])
               (case ($rich-style-hidden? style)
                 [(#t) ($sgr 8)] [(off) ($sgr 28)] [else ""])
               (case ($rich-style-strike? style)
                 [(#t) ($sgr 9)] [(off) ($sgr 29)] [else ""])
               (let ([fg (rich-style-fg style)])
                 (if fg ($color->ansi #t fg) ""))
               (let ([bg (rich-style-bg style)])
                 (if bg ($color->ansi #f bg) ""))))))))

  #|proc:rich-style->ansi
  The `rich-style->ansi` procedure returns ANSI SGR sequences for `style` using
  `color-system` (or `#f`). Explicit no-* attributes emit disabling sequences.
  The none color system suppresses ANSI; other accepted systems emit SGR and truecolor
  sequences without RGB color downgrade.
  |#
  (define rich-style->ansi
    (lambda (color-system style)
      (rich-style->ansi/context color-system style #f)))

  (define-record-type (rich-theme-record make-rich-theme-record $rich-theme?)
    (opaque #t)
    (fields (immutable table rich-theme-table)))

  (define $make-theme-table
    (lambda ()
      (make-eq-hashtable)))

  #|proc:make-rich-theme
  The `make-rich-theme` procedure constructs a mutable rich theme.
  |#
  (define make-rich-theme
    (case-lambda
      [() (make-rich-theme-record ($make-theme-table))]
      [(entries)
       (pcheck ([list? entries])
               (let ([theme (make-rich-theme)])
                 (for-each
                  (lambda (entry)
                    (if (and (pair? entry) (symbol? (car entry)))
                        (rich-theme-set! theme (car entry) (cdr entry))
                        (errorf 'make-rich-theme "invalid theme entry: ~a" entry)))
                  entries)
                 theme))]))

  #|macro:rich-theme
  The `rich-theme` macro constructs and returns a mutable rich theme. Theme
  entries are written as `:key value` clauses.
  |#
  (define-syntax rich-theme
    (lambda (stx)
      (define keyword->name
        (lambda (key)
          (let* ([sym (syntax->datum key)]
                 [str (and (symbol? sym) (symbol->string sym))]
                 [len (if str (string-length str) 0)])
            (if (and str (positive? len) (char=? (string-ref str 0) #\:))
                (string->symbol (substring str 1 len))
                (syntax-error key "invalid rich theme key")))))
      (define build-entries
        (lambda (clause*)
          (let loop ([clause* clause*] [entry* '()])
            (cond [(null? clause*) (reverse entry*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-theme form")]
                  [else
                   (let ([name (keyword->name (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([key-name (datum->syntax (car clause*) name)]
                                   [key-value value])
                       (loop (cddr clause*)
                             (cons #'(cons 'key-name key-value) entry*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([(entry ...) (build-entries #'(clause ...))])
           #'(make-rich-theme (list entry ...)))]
        [_ (syntax-error stx "invalid rich-theme form")])))

  #|proc:rich-theme?
  The `rich-theme?` procedure returns `#t` when its argument is a rich theme,
  and `#f` otherwise.
  |#
  (define rich-theme?
    (lambda (x)
      ($rich-theme? x)))

  #|proc:rich-theme-ref
  The `rich-theme-ref` procedure returns the value bound to `name` in `theme`.
  |#
  (define rich-theme-ref
    (case-lambda
      [(theme name)
       (pcheck ([rich-theme? theme] [symbol? name])
               (hashtable-ref (rich-theme-table theme) name #f))]
      [(theme name default)
       (pcheck ([rich-theme? theme] [symbol? name])
               (hashtable-ref (rich-theme-table theme) name default))]))

  #|proc:rich-theme-set!
  The `rich-theme-set!` procedure binds `name` to `value` in `theme`.
  |#
  (define rich-theme-set!
    (lambda (theme name value)
      (pcheck ([rich-theme? theme] [symbol? name] [rich-style? value])
              (hashtable-set! (rich-theme-table theme) name value)))))
