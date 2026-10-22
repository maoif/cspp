#!chezscheme
(library (cspp rich segment)
  (export rich-segment
          make-rich-segment
          rich-segment?
          rich-segment-text
          rich-segment-style
          rich-segment-control?
          rich-segment-width
          rich-segments-width
          rich-segments->plain
          rich-strip-ansi
          rich-segment-wrap
          rich-segment-crop
          rich-segment-pad-right)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich private cells)
          (cspp rich style))

  (define-record-type (rich-segment-record $make-rich-segment $rich-segment?)
    (fields (immutable text $rich-segment-text)
            (immutable style $rich-segment-style)
            (immutable control? $rich-segment-control?)))

  (define $rich-segment-style?
    (lambda (x)
      (or (not x) (symbol? x) (rich-style? x) (rich-reset? x))))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $segment-copy
    (lambda (segment text)
      (rich-segment text
                    (rich-segment-style segment)
                    (rich-segment-control? segment))))

  ;; Measure and scan a continuous text stream so style boundaries cannot split clusters.
  (define $visible-text
    (lambda (segments)
      (apply string-append
             (map (lambda (segment)
                    (if (rich-segment-control? segment) "" (rich-segment-text segment)))
                  segments))))

  ;; Consume a character prefix while preserving the source styles and control segments.
  (define $take-segment-chars
    (lambda (segments count)
      (let loop ([segments segments] [count count] [out '()])
        (cond
          [(null? segments) (values (reverse out) '())]
          [(rich-segment-control? (car segments))
           (loop (cdr segments) count (cons (car segments) out))]
          [(zero? count) (values (reverse out) segments)]
          [else
           (let* ([segment (car segments)] [text (rich-segment-text segment)]
                  [len (string-length text)])
             (if (<= len count)
                 (loop (cdr segments) (- count len) (cons segment out))
                 (values (reverse (cons ($segment-copy segment (substring text 0 count)) out))
                         (cons ($segment-copy segment (substring text count len))
                               (cdr segments)))))]))))

  (define $oversize-placeholder
    (lambda (segments)
      (let loop ([segments segments] [written? #f] [out '()])
        (cond [(null? segments) (reverse out)]
              [(rich-segment-control? (car segments))
               (loop (cdr segments) written? (cons (car segments) out))]
              [written? (loop (cdr segments) #t out)]
              [else (loop (cdr segments) #t
                          (cons ($segment-copy (car segments) " ") out))]))))

  #|proc:rich-segment?
  The `rich-segment?` procedure returns `#t` when its argument is a rich text
  segment, and `#f` otherwise.
  |#
  (define rich-segment?
    (lambda (x)
      ($rich-segment? x)))

  #|proc:rich-segment-text
  The `rich-segment-text` procedure returns the source text stored in
  `segment`.
  |#
  (define rich-segment-text
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-text segment))))

  #|proc:rich-segment-style
  The `rich-segment-style` procedure returns the style object, theme-name symbol,
  reset object, or `#f` stored in `segment`.
  |#
  (define rich-segment-style
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-style segment))))

  #|proc:rich-segment-control?
  The `rich-segment-control?` procedure returns whether `segment` carries
  control text that should not contribute visible width.
  |#
  (define rich-segment-control?
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-control? segment))))

  #|proc:make-rich-segment
  The `make-rich-segment` procedure returns a segment containing the string `text`.
  Optional `style` is a style object, a theme-name symbol resolved by the console,
  a reset object, or `#f`. Optional boolean `control?` makes the segment zero-width.
  |#
  (define make-rich-segment
    (case-lambda
      [(text)
       (make-rich-segment text #f #f)]
      [(text style)
       (make-rich-segment text style #f)]
      [(text style control?)
       (pcheck ([string? text] [$rich-segment-style? style] [boolean? control?])
               ($make-rich-segment text style control?))]))

  #|proc:rich-segment
  The `rich-segment` procedure returns a segment from the string `text`, optional
  `style`, and optional boolean `control?`, just like `make-rich-segment`. The style
  may be a style object, theme-name symbol, reset object, or `#f`.
  |#
  (define rich-segment make-rich-segment)

  #|proc:rich-strip-ansi
  The `rich-strip-ansi` procedure removes ANSI CSI SGR escape sequences from
  `text`.
  |#
  (define rich-strip-ansi
    (lambda (text)
      (pcheck ([string? text])
        (let ([len (string-length text)])
          (let loop ([index 0] [out '()])
            (cond [(= index len) (list->string (reverse out))]
                  [(rich-ansi-sgr-end text index) => (lambda (end) (loop end out))]
                  [else (loop (+ index 1) (cons (string-ref text index) out))]))))))

  #|proc:rich-segment-width
  The `rich-segment-width` procedure returns the visible width of `segment`.
  Control segments have width zero. CJK and wide emoji use two terminal cells;
  combining marks use zero, and East Asian ambiguous characters use one.
  |#
  (define rich-segment-width
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              (if (rich-segment-control? segment)
                  0
                  (rich-cell-string-width (rich-segment-text segment))))))

  #|proc:rich-segments-width
  The `rich-segments-width` procedure returns the terminal cell width of `segments`
  as one logical line. Grapheme clusters may cross style or control segment boundaries.
  |#
  (define rich-segments-width
    (lambda (segments)
      (pcheck ([$rich-segment-list? segments])
              (rich-cell-string-width ($visible-text segments)))))

  #|proc:rich-segments->plain
  The `rich-segments->plain` procedure concatenates the source text of
  `segments`.
  |#
  (define rich-segments->plain
    (lambda (segments)
      (pcheck ([$rich-segment-list? segments])
              (apply string-append (map rich-segment-text segments)))))

  #|proc:rich-segment-wrap
  The `rich-segment-wrap` procedure wraps `segments` into a list of segment
  lines that fit within positive terminal cell `width`. Display units stay intact;
  a glyph wider than the whole line is replaced by a single space.
  |#
  (define rich-segment-wrap
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-positive-integer? width])
        (let* ([text ($visible-text segments)] [len (string-length text)])
          (let loop ([index 0] [start 0] [used 0] [segments segments] [lines '()])
            (if (= index len)
                (reverse (cons segments lines))
                (let-values ([(end cells) (rich-cell-next text index)])
                  (cond
                    [(<= (+ used cells) width)
                     (loop end start (+ used cells) segments lines)]
                    [(positive? used)
                     (let-values ([(line rest) ($take-segment-chars segments (- index start))])
                       (loop index index 0 rest (cons line lines)))]
                    [else
                     ;; A two-cell glyph cannot fit a one-cell line. Consume it safely.
                     (let-values ([(pieces rest) ($take-segment-chars segments (- end start))])
                       (let ([lines (cons ($oversize-placeholder pieces) lines)])
                         (if (= end len)
                             (reverse lines)
                             (loop end end 0 rest lines))))]))))))))

  #|proc:rich-segment-crop
  The `rich-segment-crop` procedure crops `segments` to at most `width`
  terminal cells without splitting grapheme clusters, including across style boundaries.
  |#
  (define rich-segment-crop
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-nonnegative-integer? width])
        (let-values ([(end cells) (rich-cell-prefix ($visible-text segments) 0 width)])
          (let-values ([(prefix rest) ($take-segment-chars segments end)])
            prefix)))))

  #|proc:rich-segment-pad-right
  The `rich-segment-pad-right` procedure appends spaces to `segments` until
  their visible width is at least `width`.
  |#
  (define rich-segment-pad-right
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-nonnegative-integer? width])
              (let ([current (rich-segments-width segments)])
                (if (>= current width)
                    segments
                    (append segments
                            (list (rich-segment (make-string (- width current) #\space))))))))))
