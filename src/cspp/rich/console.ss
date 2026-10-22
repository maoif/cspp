#!chezscheme
(library (cspp rich console)
  (export rich-console-frame
          rich-console-output-width
          make-rich-console
          rich-console
          rich-console?
          rich-console-output-port
          rich-console-output-port-set!
          rich-console-error-port
          rich-console-error-port-set!
          rich-console-input-port
          rich-console-input-port-set!
          rich-console-width
          rich-console-width-set!
          rich-console-color-system
          rich-console-color-system-set!
          rich-console-force-terminal?
          rich-console-force-terminal?-set!
          rich-console-soft-wrap?
          rich-console-soft-wrap?-set!
          rich-console-theme
          rich-console-theme-set!
          rich-console-ascii-only?
          rich-console-ascii-only?-set!
          rich-print
          rich-println
          rich-fprint
          rich-fprintln
          rich-render
          rich-export-text
          rich-export-ansi)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich style)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich text)
          (cspp rich pretty)
          (cspp rich tree))

  (define-record-type rich-console-record
    (fields (mutable output-port $rich-console-output-port $rich-console-output-port-set!)
            (mutable error-port $rich-console-error-port $rich-console-error-port-set!)
            (mutable input-port $rich-console-input-port $rich-console-input-port-set!)
            (mutable width $rich-console-width $rich-console-width-set!)
            (mutable color-system $rich-console-color-system $rich-console-color-system-set!)
            (mutable force-terminal? $rich-console-force-terminal? $rich-console-force-terminal?-set!)
            (mutable soft-wrap? $rich-console-soft-wrap? $rich-console-soft-wrap?-set!)
            (mutable theme $rich-console-theme $rich-console-theme-set!)
            (mutable ascii-only? $rich-console-ascii-only? $rich-console-ascii-only?-set!)))

  #|proc:rich-console?
  The `rich-console?` procedure returns `#t` when its argument is a rich console
  object, and `#f` otherwise.
  |#
  (define rich-console? rich-console-record?)

  #|proc:make-rich-console
  The `make-rich-console` procedure constructs a console using current ports and
  automatic terminal detection. ANSI is suppressed on nonterminal output ports.
  |#
  (define make-rich-console
    (case-lambda
      [()
       (make-rich-console-record (current-output-port)
                                 (current-error-port)
                                 (current-input-port)
                                 #f
                                 'auto
                                 #f
                                 #f
                                 #f
                                 #f)]))

  (define $rich-console-theme?
    (lambda (x)
      (or (not x) (rich-theme? x))))

  ;; Chez may wrap its original console ports in thread-safe custom ports.
  (define $stdout-port (console-output-port))
  (define $stderr-port (console-error-port))

  (define $terminal-output?
    (foreign-procedure "cspp_rich_terminal_output_supported" (int) boolean))

  (define $terminal-width
    (foreign-procedure "cspp_rich_terminal_width" (int) int))

  (define $output-descriptor
    (lambda (port)
      (cond [(file-port? port) (port-file-descriptor port)]
            [(eq? port $stdout-port) 1]
            [(eq? port $stderr-port) 2]
            [else #f])))

  #|proc:rich-console-output-width
  The private `rich-console-output-width` procedure returns the physical destination
  width for `console`, falling back to its effective render width when size is unknown.
  |#
  (define rich-console-output-width
    (lambda (console)
      (pcheck ([rich-console? console])
              (let* ([fd ($output-descriptor (rich-console-output-port console))]
                     [width (and fd ($terminal-width fd))])
                (if (and width (positive? width)) width (rich-console-width console))))))

  (define $console-color-system
    (lambda (console)
      (let ([system (rich-console-color-system console)]
            [port (rich-console-output-port console)])
        (if (eq? system 'auto)
            (if (or (rich-console-force-terminal? console)
                    (let ([fd ($output-descriptor port)])
                      (and fd ($terminal-output? fd))))
                'standard
                'none)
            system))))

  (define $resolve-segment-style
    (lambda (console style)
      (if (symbol? style)
          (if (eq? ($console-color-system console) 'none)
              #f
              (let* ([theme (rich-console-theme console)]
                     [resolved (and theme (rich-theme-ref theme style))])
                (or resolved (errorf 'rich-print "unknown theme style: ~s" style))))
          style)))

  #|proc:rich-console-output-port
  The `rich-console-output-port` procedure returns the output port used by
  `console`.
  |#
  (define rich-console-output-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-output-port console))))

  #|proc:rich-console-output-port-set!
  The `rich-console-output-port-set!` procedure sets the output port used by
  `console`.
  |#
  (define rich-console-output-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [output-port? port])
              ($rich-console-output-port-set! console port))))

  #|proc:rich-console-error-port
  The `rich-console-error-port` procedure returns the error port used by
  `console`.
  |#
  (define rich-console-error-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-error-port console))))

  #|proc:rich-console-error-port-set!
  The `rich-console-error-port-set!` procedure sets the error port used by
  `console`.
  |#
  (define rich-console-error-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [output-port? port])
              ($rich-console-error-port-set! console port))))

  #|proc:rich-console-input-port
  The `rich-console-input-port` procedure returns the input port used by
  `console`.
  |#
  (define rich-console-input-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-input-port console))))

  #|proc:rich-console-input-port-set!
  The `rich-console-input-port-set!` procedure sets the input port used by
  `console`.
  |#
  (define rich-console-input-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [input-port? port])
              ($rich-console-input-port-set! console port))))

  #|proc:rich-console-width
  The `rich-console-width` procedure returns the effective positive render width for
  `console`: its explicit width, the destination terminal width, or 80 if unavailable.
  Automatic widths are queried on each call so terminal resizing takes effect.
  |#
  (define rich-console-width
    (lambda (console)
      (pcheck ([rich-console? console])
              (or ($rich-console-width console)
                  (let* ([fd ($output-descriptor (rich-console-output-port console))]
                         [width (and fd ($terminal-width fd))])
                    (if (and width (positive? width)) width 80))))))

  #|proc:rich-console-width-set!
  The `rich-console-width-set!` procedure sets `console` to positive integer `width`.
  Passing `#f` restores automatic terminal width detection (the default). Returns an
  unspecified value.
  |#
  (define rich-console-width-set!
    (lambda (console width)
      (pcheck ([rich-console? console] [rich-optional-width? width])
              ($rich-console-width-set! console width))))

  #|proc:rich-console-color-system
  The `rich-console-color-system` procedure returns the ANSI color system for
  `console`.
  |#
  (define rich-console-color-system
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-color-system console))))

  #|proc:rich-console-color-system-set!
  The `rich-console-color-system-set!` procedure sets `console` to `color-system` and
  returns an unspecified value. The default `'auto` detects terminal output; `'none`
  suppresses ANSI. Explicit `'standard`, `'256`, and `'truecolor` enable ANSI even on
  nonterminal ports. RGB colors currently use truecolor sequences in each enabled mode.
  |#
  (define rich-console-color-system-set!
    (lambda (console color-system)
      (pcheck ([rich-console? console] [rich-color-system? color-system])
              ($rich-console-color-system-set! console color-system))))

  #|proc:rich-console-force-terminal?
  The `rich-console-force-terminal?` procedure returns whether `console`
  forces terminal output behavior.
  |#
  (define rich-console-force-terminal?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-force-terminal? console))))

  #|proc:rich-console-force-terminal?-set!
  The `rich-console-force-terminal?-set!` procedure sets whether `console` treats its
  output port as a terminal in automatic color mode. The boolean `force-terminal?`
  enables this override when true; explicit color modes take precedence. Returns an
  unspecified value.
  |#
  (define rich-console-force-terminal?-set!
    (lambda (console force-terminal?)
      (pcheck ([rich-console? console] [boolean? force-terminal?])
              ($rich-console-force-terminal?-set! console force-terminal?))))

  #|proc:rich-console-soft-wrap?
  The `rich-console-soft-wrap?` procedure returns whether `console` soft-wraps
  output.
  |#
  (define rich-console-soft-wrap?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-soft-wrap? console))))

  #|proc:rich-console-soft-wrap?-set!
  The `rich-console-soft-wrap?-set!` procedure sets whether `console`
  soft-wraps output.
  |#
  (define rich-console-soft-wrap?-set!
    (lambda (console soft-wrap?)
      (pcheck ([rich-console? console] [boolean? soft-wrap?])
              ($rich-console-soft-wrap?-set! console soft-wrap?))))

  #|proc:rich-console-theme
  The `rich-console-theme` procedure returns the theme for `console`, or `#f`
  when no theme is configured.
  |#
  (define rich-console-theme
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-theme console))))

  #|proc:rich-console-theme-set!
  The `rich-console-theme-set!` procedure sets the theme for `console` to `theme`, a
  rich theme or `#f`, and returns an unspecified value. Symbol styles in segments and
  text spans are looked up in this theme each time they are printed. An unbound style
  raises an error when ANSI is enabled; plain output ignores styles.
  |#
  (define rich-console-theme-set!
    (lambda (console theme)
      (pcheck ([rich-console? console] [$rich-console-theme? theme])
              ($rich-console-theme-set! console theme))))

  #|proc:rich-console-ascii-only?
  The `rich-console-ascii-only?` procedure returns whether `console` restricts
  output to ASCII.
  |#
  (define rich-console-ascii-only?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-ascii-only? console))))

  #|proc:rich-console-ascii-only?-set!
  The `rich-console-ascii-only?-set!` procedure sets whether `console`
  restricts output to ASCII.
  |#
  (define rich-console-ascii-only?-set!
    (lambda (console ascii-only?)
      (pcheck ([rich-console? console] [boolean? ascii-only?])
              ($rich-console-ascii-only?-set! console ascii-only?))))

  #|macro:rich-console
  The `rich-console` macro constructs and returns a console.
  |#
  (define-syntax rich-console
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:output-port) #'rich-console-output-port-set!]
              [(:error-port) #'rich-console-error-port-set!]
              [(:input-port) #'rich-console-input-port-set!]
              [(:width) #'rich-console-width-set!]
              [(:color-system) #'rich-console-color-system-set!]
              [(:force-terminal?) #'rich-console-force-terminal?-set!]
              [(:soft-wrap?) #'rich-console-soft-wrap?-set!]
              [(:theme) #'rich-console-theme-set!]
              [(:ascii-only?) #'rich-console-ascii-only?-set!]
              [else (syntax-error field "invalid rich-console field")]))))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-console form")]
                  [else
                   (let ([setter (field->setter (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([console-name name]
                                   [set-console-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-console-field! console-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-console)))])
           (with-syntax ([(setter ...) (build-setters #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-console)])
                 setter ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-console form")])))

  (define $target+values
    (lambda (args)
      (cond [(null? args) (values (current-output-port) '())]
            [(or (output-port? (car args)) (rich-console? (car args)))
             (values (car args) (cdr args))]
            [else (values (current-output-port) args)])))

  (define $target-port
    (lambda (target)
      (if (rich-console? target)
          (rich-console-output-port target)
          target)))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $rich-segment-line-list?
    (lambda (x)
      (and (list? x) (rich-list-every? $rich-segment-list? x))))

  (define $rich-direct-value?
    (lambda (value)
      (or (string? value)
          (char? value)
          (number? value)
          (symbol? value))))

  (define $value->segment
    (lambda (value)
      (rich-segment
       (if (string? value)
           value
           (rich-string-output
            (lambda (port)
              (if (char? value)
                  (write-char value port)
                  (display value port))))))))

  (define $write-segment-line
    (lambda (port line)
      (display (rich-segments->plain line) port)))

  (define $write-segment-lines
    (lambda (port lines)
      (let loop ([lines lines] [first? #t])
        (unless (null? lines)
          (unless first? (newline port))
          ($write-segment-line port (car lines))
          (loop (cdr lines) #f)))))

  (define $write-style-ansi
    (lambda (port color-system style)
      (if (eq? color-system 'none)
          #f
          (let ([ansi (rich-style->ansi color-system style)])
            (and (not (string=? ansi ""))
                 (begin
                   (display ansi port)
                   #t))))))

  (define $valid-rendered-value?
    (lambda (value)
      (or (string? value) ($rich-segment-line-list? value))))

  (define $check-rendered-value
    (lambda (who value)
      (if ($valid-rendered-value? value)
          value
          (errorf who "renderer returned invalid value: ~a" value))))

  (define $write-rendered-value
    (lambda (port value)
      (let ([value ($check-rendered-value 'rich-print value)])
        (cond [(string? value) (display value port)]
              [else ($write-segment-lines port value)]))))

  (define $style->ansi
    (lambda (color-system style)
      (if (eq? color-system 'none)
          ""
          (rich-style->ansi color-system style))))

  (define $restore-active-style
    (lambda (port color-system active-style)
      (let ([ansi (and active-style ($style->ansi color-system active-style))])
        (unless (or (not ansi) (string=? ansi ""))
          (display ansi port)))))

  (define $write-segment-line/ansi
    (lambda (console port color-system active-style style-emitted? line)
      (let loop ([segments line] [style-emitted? style-emitted?])
        (if (null? segments)
            style-emitted?
            (let* ([segment (car segments)]
                   [style ($resolve-segment-style console (rich-segment-style segment))]
                   [segment-style-emitted?
                    (and (rich-style? style)
                         ($write-style-ansi port color-system style))]
                   [segment-reset-emitted?
                    (and (rich-reset? style)
                         style-emitted?
                         (not (eq? color-system 'none))
                         (begin
                           (display rich-ansi-reset port)
                           #t))]
                   [style-emitted?
                    (and (not segment-reset-emitted?)
                         (or segment-style-emitted? style-emitted?))])
              (display (rich-segment-text segment) port)
              (let ([style-emitted?
                     (if (and (or segment-style-emitted? segment-reset-emitted?)
                              (not (eq? color-system 'none)))
                         (begin
                           (when segment-style-emitted?
                             (display rich-ansi-reset port))
                           ($restore-active-style port color-system active-style)
                           (and active-style #t))
                         style-emitted?)])
                (loop (cdr segments) style-emitted?)))))))

  (define $write-segment-lines/ansi
    (lambda (console port color-system active-style lines style-emitted?)
      (let loop ([lines lines] [first? #t] [style-emitted? style-emitted?])
        (if (null? lines)
            style-emitted?
            (begin
              (unless first? (newline port))
              (loop (cdr lines)
                    #f
                    ($write-segment-line/ansi console port color-system active-style
                                             style-emitted? (car lines))))))))

  (define $write-rendered-value/ansi
    (lambda (console port color-system active-style value style-emitted?)
      (let ([value ($check-rendered-value 'rich-print value)])
        (cond [(string? value)
               (display value port)
              style-emitted?]
              [else
               ($write-segment-lines/ansi console port color-system active-style value
                                          style-emitted?)]))))

  (define $write-rich-value/plain
    (lambda (port value)
      (cond [(rich-style? value) (void)]
            [(rich-reset? value) (void)]
            [else
             (let ([renderer (rich-renderer-for value)])
               (cond [renderer
                      ($write-rendered-value port (renderer value))]
                     [($rich-direct-value? value)
                      ($write-segment-line port (list ($value->segment value)))]
                     [else
                      ($write-rendered-value port
                                             (rich-pretty-render value))]))])))

  (define $write-rich-values/plain
    (lambda (port values)
      (for-each (lambda (value) ($write-rich-value/plain port value)) values)))

  (define $write-rich-values/console
    (lambda (console values)
      (let ([port (rich-console-output-port console)]
            [color-system ($console-color-system console)])
        (let loop ([values values] [active-style #f] [style-emitted? #f])
          (unless (null? values)
            (let ([value (car values)])
              (cond [(rich-style? value)
                     (if (eq? color-system 'none)
                         (loop (cdr values) #f style-emitted?)
                         (loop (cdr values)
                               value
                               (or ($write-style-ansi port color-system value)
                                   style-emitted?)))]
                    [(rich-reset? value)
                     (when (and style-emitted? (not (eq? color-system 'none)))
                       (display rich-ansi-reset port))
                     (loop (cdr values) #f #f)]
                    [else
                     (let ([renderer (rich-renderer-for value)])
                       (loop (cdr values)
                             active-style
                             (cond [(and (rich-tree? value)
                                         (rich-console-ascii-only? console))
                                    ($write-rendered-value/ansi
                                     console port color-system active-style
                                     (rich-tree-render/ascii value)
                                     style-emitted?)]
                                   [renderer
                                    ($write-rendered-value/ansi
                                     console port color-system active-style (renderer value)
                                     style-emitted?)]
                                   [($rich-direct-value? value)
                                    ($write-segment-line port
                                                         (list ($value->segment value)))
                                    style-emitted?]
                                   [else
                                    ($write-rendered-value/ansi
                                     console port color-system active-style
                                     (rich-pretty-render value)
                                     style-emitted?)])))])))))))

  (define $write-rich-values
    (lambda (target port values)
      (if (rich-console? target)
          (parameterize ([rich-render-width (rich-console-width target)])
            ($write-rich-values/console target values))
          (let ([console (make-rich-console)])
            (rich-console-output-port-set! console port)
            (parameterize ([rich-render-width (rich-console-width console)])
              ($write-rich-values/plain port values))))))

  #|proc:rich-print
  The `rich-print` procedure prints values to a rich console, an output port, or
  the current output port.
  |#
  (define rich-print
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  ($write-rich-values target port values))))))

  #|proc:rich-println
  The `rich-println` procedure prints values and then writes a newline.
  |#
  (define rich-println
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  ($write-rich-values target port values)
                  (newline port))))))

  #|proc:rich-fprint
  The `rich-fprint` procedure prints values to the given output port.
  |#
  (define rich-fprint
    (lambda (port . values)
      (pcheck ([output-port? port])
              (apply rich-print port values))))

  #|proc:rich-fprintln
  The `rich-fprintln` procedure prints values to the given output port and then
  writes a newline.
  |#
  (define rich-fprintln
    (lambda (port . values)
      (pcheck ([output-port? port])
              (apply rich-println port values))))

  #|proc:rich-render
  The `rich-render` procedure renders a value for a rich console.
  |#
  (define rich-render
    (lambda (console value)
      (pcheck ([rich-console? console])
              (parameterize ([rich-render-width (rich-console-width console)])
                (let ([renderer (rich-renderer-for value)])
                  (cond [(and (rich-tree? value)
                            (rich-console-ascii-only? console))
                       ($check-rendered-value 'rich-render
                                             (rich-tree-render/ascii value))]
                      [renderer
                       ($check-rendered-value 'rich-render (renderer value))]
                      [(or (rich-style? value)
                           (rich-reset? value)
                           ($rich-direct-value? value))
                       value]
                      [else
                       ($check-rendered-value 'rich-render
                                             (rich-pretty-render value))]))))))

  #|proc:rich-export-text
  The `rich-export-text` procedure renders a value to plain text and returns the
  result as a string.
  |#
  (define rich-export-text
    (lambda (value)
      (rich-string-output
       (lambda (port)
         (let ([renderer (rich-renderer-for value)])
           (cond [renderer
                  ($write-rendered-value port (renderer value))]
                 [(or (rich-style? value) (rich-reset? value))
                  (void)]
                 [else
                  ($write-rendered-value port (rich-pretty-render value))]))))))

  #|proc:rich-export-ansi
  The `rich-export-ansi` procedure renders a value to ANSI text and returns the
  result as a string.
  |#
  (define rich-export-ansi
    (lambda (value)
      (rich-string-output
       (lambda (port)
         (let ([console (make-rich-console)])
           (rich-console-output-port-set! console port)
           (rich-console-color-system-set! console 'standard)
           (rich-print console value))))))

  #|proc:rich-console-frame
  The private `rich-console-frame` procedure renders `value` once using `console`'s
  current width, theme, ASCII, and color policy. Returns the encoded frame string.
  The destination console and its ports are left unchanged.
  |#
  (define rich-console-frame
    (lambda (console value)
      (pcheck ([rich-console? console])
              (let ([width (rich-console-width console)]
                    [colors ($console-color-system console)])
                (rich-string-output
                 (lambda (port)
                   (let ([capture (make-rich-console-record
                                   port (rich-console-error-port console)
                                   (rich-console-input-port console) width colors
                                   (rich-console-force-terminal? console)
                                   (rich-console-soft-wrap? console)
                                   (rich-console-theme console)
                                   (rich-console-ascii-only? console))])
                     (rich-print capture value))))))))

  (rich-register-renderer! rich-text? rich-text-render))
