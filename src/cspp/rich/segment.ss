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

  (define $csi-open-char (integer->char 91))

  (define $semicolon-char (integer->char 59))

  (define $ansi-sgr-end
    (lambda (text start)
      (let ([len (string-length text)])
        (and (char=? (string-ref text start) #\esc)
             (< (+ start 2) len)
             (char=? (string-ref text (+ start 1)) $csi-open-char)
             (let scan ([i (+ start 2)])
               (cond [(= i len) #f]
                     [(char=? (string-ref text i) #\m) (+ i 1)]
                     [(or (char-numeric? (string-ref text i))
                          (char=? (string-ref text i) $semicolon-char))
                      (scan (+ i 1))]
                     [else #f]))))))

  (define $substring-visible
    (lambda (text start max-width)
      (let ([len (string-length text)])
        (let loop ([i start] [visible 0])
          (cond [(= i len) (values len visible)]
                [(= visible max-width) (values i visible)]
                [($ansi-sgr-end text i) =>
                 (lambda (end) (loop end visible))]
                [else (loop (+ i 1) (+ visible 1))])))))

  (define $split-text-visible
    (lambda (text width)
      (let ([len (string-length text)])
        (let loop ([start 0] [out '()])
          (if (= start len)
              (reverse out)
              (let-values ([(end visible) ($substring-visible text start width)])
                (if (= end start)
                    (loop (+ start 1) (cons (substring text start (+ start 1)) out))
                    (loop end (cons (substring text start end) out)))))))))

  (define $segment-copy
    (lambda (segment text)
      (rich-segment text
                    (rich-segment-style segment)
                    (rich-segment-control? segment))))

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
                (let loop ([i 0] [out '()])
                  (cond [(= i len)
                         (list->string (reverse out))]
                        [(and (char=? (string-ref text i) #\esc)
                              (< (+ i 2) len)
                              (char=? (string-ref text (+ i 1)) $csi-open-char))
                         (let scan ([j (+ i 2)])
                           (cond [(= j len)
                                  (loop (+ i 1) (cons (string-ref text i) out))]
                                 [(char=? (string-ref text j) #\m)
                                  (loop (+ j 1) out)]
                                 [(or (char-numeric? (string-ref text j))
                                      (char=? (string-ref text j) $semicolon-char))
                                  (scan (+ j 1))]
                                 [else
                                  (loop (+ i 1) (cons (string-ref text i) out))]))]
                        [else
                         (loop (+ i 1) (cons (string-ref text i) out))]))))))

  #|proc:rich-segment-width
  The `rich-segment-width` procedure returns the visible width of `segment`.
  Control segments have width zero.
  |#
  (define rich-segment-width
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              (if (rich-segment-control? segment)
                  0
                  (string-length (rich-strip-ansi (rich-segment-text segment)))))))

  #|proc:rich-segments-width
  The `rich-segments-width` procedure returns the sum of visible widths for
  `segments`.
  |#
  (define rich-segments-width
    (lambda (segments)
      (pcheck ([$rich-segment-list? segments])
              (let loop ([segments segments] [width 0])
                (if (null? segments)
                    width
                    (loop (cdr segments)
                          (+ width (rich-segment-width (car segments)))))))))

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
  lines that fit within positive visible `width`.
  |#
  (define rich-segment-wrap
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-positive-integer? width])
              (let loop-segments ([segments segments]
                                  [line '()]
                                  [line-width 0]
                                  [lines '()])
                (cond
                 [(null? segments)
                  (reverse (cons (reverse line) lines))]
                 [(rich-segment-control? (car segments))
                  (loop-segments (cdr segments)
                                 (cons (car segments) line)
                                 line-width
                                 lines)]
                 [else
                  (let loop-pieces ([pieces ($split-text-visible
                                             (rich-segment-text (car segments))
                                             width)]
                                    [line line]
                                    [line-width line-width]
                                    [lines lines])
                    (cond
                     [(null? pieces)
                      (loop-segments (cdr segments) line line-width lines)]
                     [else
                      (let* ([piece (car pieces)]
                             [piece-width (string-length (rich-strip-ansi piece))]
                             [next-width (+ line-width piece-width)]
                             [piece-segment ($segment-copy (car segments) piece)])
                        (cond
                         [(= piece-width 0)
                          (loop-pieces (cdr pieces)
                                       (cons piece-segment line)
                                       line-width
                                       lines)]
                         [(> next-width width)
                          (loop-pieces pieces '() 0 (cons (reverse line) lines))]
                         [else
                          (loop-pieces (cdr pieces)
                                       (cons piece-segment line)
                                       next-width
                                       lines)]))]))])))))

  #|proc:rich-segment-crop
  The `rich-segment-crop` procedure crops `segments` to at most `width`
  visible columns.
  |#
  (define rich-segment-crop
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-nonnegative-integer? width])
              (let loop ([segments segments] [left width] [out '()])
                (cond [(null? segments) (reverse out)]
                      [(rich-segment-control? (car segments))
                       (loop (cdr segments) left (cons (car segments) out))]
                      [(= left 0) (reverse out)]
                      [else
                       (let-values ([(end visible)
                                     ($substring-visible
                                      (rich-segment-text (car segments))
                                      0
                                      left)])
                         (let ([text (substring (rich-segment-text (car segments)) 0 end)])
                           (loop (cdr segments)
                                 (- left visible)
                                 (if (string=? text "")
                                     out
                                     (cons ($segment-copy (car segments) text) out)))))])))))

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
