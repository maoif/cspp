#!chezscheme
(library (cspp rich private lines)
  (export rich-single-line-text rich-lines-normalize rich-lines-wrap rich-lines-width rich-lines-min-width)
  (import (cspp chez) (cspp utils) (cspp rich private common) (cspp rich segment)
          (cspp rich private cells)
          (only (cspp list) make-list-builder))

  ;; Normalize display text before measuring it. Explicit control segments stay opaque.
  (define $normalize-characters
    (lambda (segments)
      (define special?
        (lambda (text)
          (let loop ([index 0])
            (and (< index (string-length text))
                 (or (memv (string-ref text index) '(#\tab #\return))
                     (loop (+ index 1)))))))
      (if (not (exists (lambda (segment)
                         (and (not (rich-segment-control? segment))
                              (special? (rich-segment-text segment)))) segments))
          segments
          (let* ([text (apply string-append
                              (map (lambda (segment)
                                     (if (rich-segment-control? segment) ""
                                         (rich-segment-text segment))) segments))]
                 [len (string-length text)]
                 [replacements (make-eqv-hashtable)])
            ;; Measure whole runs so graphemes can cross style boundaries.
            (let scan ([index 0] [start 0] [column 0])
              (unless (= index len)
                (let ([character (string-ref text index)])
                  (case character
                    [(#\tab)
                     (let* ([column (+ column (rich-cell-string-width (substring text start index)))]
                            [spaces (- 8 (modulo column 8))])
                       (hashtable-set! replacements index (make-string spaces #\space))
                       (scan (+ index 1) (+ index 1) (+ column spaces)))]
                    [(#\return)
                     (hashtable-set! replacements index "\n")
                     (let ([next (+ index 1)])
                       (if (and (< next len) (char=? (string-ref text next) #\newline))
                           (begin
                             (hashtable-set! replacements next "")
                             (scan (+ next 1) (+ next 1) 0))
                           (scan next next 0)))]
                    [(#\newline) (scan (+ index 1) (+ index 1) 0)]
                    [else (scan (+ index 1) start column)]))))
            (let ([offset 0] [result (make-list-builder)])
              (for-each
               (lambda (segment)
                 (result
                  (if (rich-segment-control? segment)
                     segment
                     (let* ([source (rich-segment-text segment)] [len (string-length source)]
                            [base offset])
                       (set! offset (+ offset len))
                       (let ([normalized
                              (rich-string-output
                               (lambda (port)
                                 (let scan ([index 0] [start 0])
                                   (cond
                                     [(= index len) (display (substring source start index) port)]
                                     [(hashtable-ref replacements (+ base index) #f) =>
                                      (lambda (replacement)
                                        (display (substring source start index) port)
                                        (display replacement port)
                                        (scan (+ index 1) (+ index 1)))]
                                     [else (scan (+ index 1) start)]))))])
                         (rich-segment normalized (rich-segment-style segment)))))))
               segments)
              (result))))))

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

  #|proc:rich-lines-normalize
  The private `rich-lines-normalize` returns segment `lines` with CRLF and CR normalized
  to line breaks and tabs expanded to eight-cell tab stops within each content line.
  Styles and explicit control segments are preserved, including across style boundaries.
  |#
  (define rich-lines-normalize
    (lambda (lines)
      (pcheck ([list? lines])
        (apply append (map (lambda (line) ($split-line ($normalize-characters line))) lines)))))

  #|proc:rich-single-line-text
  The private `rich-single-line-text` returns string `text` with CRLF, CR, and LF folded
  to spaces and tabs expanded at eight-cell stops. The result fits one logical line.
  |#
  (define rich-single-line-text
    (lambda (text)
      (pcheck ([string? text])
        (if (let scan ([index 0])
              (and (< index (string-length text))
                   (or (memv (string-ref text index) '(#\tab #\return #\newline))
                       (scan (+ index 1)))))
            (let ([text (rich-string-output
                         (lambda (port)
                           (let loop ([index 0])
                             (unless (= index (string-length text))
                               (let ([character (string-ref text index)])
                                 (write-char (if (memv character '(#\return #\newline))
                                                 #\space character) port)
                                 (loop (if (and (char=? character #\return)
                                                (< (+ index 1) (string-length text))
                                                (char=? (string-ref text (+ index 1)) #\newline))
                                           (+ index 2) (+ index 1))))))))])
              (rich-segments->plain ($normalize-characters (list (rich-segment text)))))
            text))))

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
                    (rich-lines-normalize lines))))))

  #|proc:rich-lines-width
  The private `rich-lines-width` returns the maximum terminal cell width of segment
  `lines`, or zero for an empty list. Embedded newlines are measured as line breaks.
  |#
  (define rich-lines-width
    (lambda (lines)
      (pcheck ([list? lines])
        (let loop ([lines (rich-lines-normalize lines)] [width 0])
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
