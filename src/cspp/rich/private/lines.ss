#!chezscheme
(library (cspp rich private lines)
  (export rich-lines-wrap rich-lines-width rich-lines-min-width)
  (import (cspp chez) (cspp utils) (cspp rich private common) (cspp rich segment)
          (cspp rich private cells)
          (only (cspp list) make-list-builder))

  ;; Preserve source styles when a custom renderer embeds newlines in its segments.
  (define $split-line
    (lambda (segments)
      (let ([lines (make-list-builder)] [line (make-list-builder)])
        (for-each
         (lambda (segment)
           (if (rich-segment-control? segment)
               (line segment)
               (let* ([text (rich-segment-text segment)] [len (string-length text)]
                      [emit (lambda (start end)
                              (line (rich-segment (substring text start end)
                                                  (rich-segment-style segment))))])
                 (let scan ([start 0] [index 0])
                   (cond [(= index len) (emit start index)]
                         [(char=? (string-ref text index) #\newline)
                          (emit start index)
                          (lines (line))
                          (set! line (make-list-builder))
                          (scan (+ index 1) (+ index 1))]
                         [else (scan start (+ index 1))])))))
         segments)
        (lines (line))
        (lines))))

  #|proc:rich-lines-wrap
  The private `rich-lines-wrap` returns segment `lines` normalized at embedded newlines
  and wrapped to nonnegative cell count `width`, preserving styles and control segments.
  A zero width crops visible content; grapheme clusters wider than a line become spaces.
  |#
  (define rich-lines-wrap
    (lambda (lines width)
      (pcheck ([list? lines] [rich-nonnegative-integer? width])
        (apply append
               (map (lambda (line)
                      (if (zero? width)
                          (list (rich-segment-crop line 0))
                          (rich-segment-wrap line width)))
                    (apply append (map $split-line lines)))))))

  #|proc:rich-lines-width
  The private `rich-lines-width` returns the maximum terminal cell width of segment
  `lines`, or zero for an empty list. Embedded newlines are measured as line breaks.
  |#
  (define rich-lines-width
    (lambda (lines)
      (pcheck ([list? lines])
        (let loop ([lines (apply append (map $split-line lines))] [width 0])
          (if (null? lines)
              width
              (loop (cdr lines) (max width (rich-segments-width (car lines)))))))))

  #|proc:rich-lines-min-width
  The private `rich-lines-min-width` returns the smallest positive width that can hold
  every complete grapheme in segment `lines`, ignoring control segments and newlines.
  |#
  (define rich-lines-min-width
    (lambda (lines)
      (pcheck ([list? lines])
        (let ([text (apply string-append
                           (map (lambda (line)
                                  (string-append
                                   (apply string-append
                                          (map (lambda (segment)
                                                 (if (rich-segment-control? segment) ""
                                                     (rich-segment-text segment))) line))
                                   "\n")) lines))])
          (let loop ([index 0] [width 1])
            (if (= index (string-length text))
                width
                (let-values ([(end cells) (rich-cell-next text index)])
                  (loop end (max width cells)))))))))
)
