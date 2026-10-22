#!chezscheme
(library (cspp rich private cells)
  (export rich-cell-next rich-cell-prefix rich-cell-string-width rich-ansi-sgr-end)
  (import (cspp chez) (cspp utils) (cspp rich private cell-data))

  (define $in-ranges?
    (lambda (point ranges)
      (let loop ([low 0] [high (fxquotient (vector-length ranges) 2)])
        (and (fx< low high)
             (let* ([mid (fxquotient (fx+ low high) 2)] [index (fx* mid 2)])
               (cond [(fx< point (vector-ref ranges index)) (loop low mid)]
                     [(fx> point (vector-ref ranges (fx+ index 1)))
                      (loop (fx+ mid 1) high)]
                     [else #t]))))))

  (define $char-width
    (lambda (character)
      (let ([point (char->integer character)])
        (cond [(fx< point #x7f) (if (fx< point #x20) 0 1)]
              [(or (memq (char-general-category character) '(Mn Mc Me Cc))
                   (and (eq? (char-general-category character) 'Cf) (not (fx= point #xad)))
                   (fx<= #x1160 point #x11ff) (fx<= #xd7b0 point #xd7ff)) 0]
              [(or (eq? (char-grapheme-break-property character) 'Regional_Indicator)
                   ($in-ranges? point rich-wide-ranges)) 2]
              [else 1]))))

  #|proc:rich-ansi-sgr-end
  The private `rich-ansi-sgr-end` returns the index after an SGR sequence in string
  `text` at valid index `start`, or `#f` if no sequence starts there.
  |#
  (define rich-ansi-sgr-end
    (lambda (text start)
      (pcheck ([string? text] [fixnum? start])
        (let ([len (string-length text)])
          (and (< (+ start 2) len)
               (char=? (string-ref text start) #\esc)
               (char=? (string-ref text (+ start 1)) #\[)
               (let scan ([index (+ start 2)])
                 (cond [(= index len) #f]
                       [(char=? (string-ref text index) #\m) (+ index 1)]
                       [(or (char<=? #\0 (string-ref text index) #\9)
                            (char=? (string-ref text index) #\;))
                        (scan (+ index 1))]
                       [else #f])))))))

  #|proc:rich-cell-next
  The private `rich-cell-next` scans string `text` from valid index `start`, returning
  the end index and terminal cell width of the next grapheme cluster. Chez supplies
  grapheme boundaries and categories; Unicode 17 tables supply East Asian widths.
  SGR escapes and controls occupy zero cells and ambiguous characters one. Emoji occupy
  two cells; standardized VS15 text presentations occupy one.
  |#
  (define rich-cell-next
    (lambda (text start)
      (pcheck ([string? text] [fixnum? start])
        (let ([len (string-length text)])
          (let loop ([index start] [state 0] [width 0] [picture? #f]
                     [joined? #f] [emoji-base? #f])
            (cond
              [(= index len) (values index width)]
              [(rich-ansi-sgr-end text index) =>
               (lambda (end) (loop end state width picture? joined? emoji-base?))]
              [else
               (let* ([character (string-ref text index)] [point (char->integer character)])
                 (let-values ([(boundary? next-state) (char-grapheme-step character state)])
                   (if (and boundary? (not (= index start)) (not (= next-state 0)))
                       (values index width)
                       (let* ([picture? (or picture? (char-extended-pictographic? character))]
                              [joined? (or joined? (and picture? (= point #x200d)))]
                              [width (cond
                                       [(eq? (char-grapheme-break-property character)
                                             'Regional_Indicator) 2]
                                       [(and emoji-base? (= point #xfe0e)) 1]
                                       [(and emoji-base? (or (= point #xfe0f) (= point #x20e3))) 2]
                                       [(and picture? (<= #x1f3fb point #x1f3ff)) (max width 2)]
                                       [(and joined? (char-extended-pictographic? character)) 2]
                                       [else (+ width ($char-width character))])]
                              [emoji-base? ($in-ranges? point rich-emoji-ranges)])
                         (if (and boundary? (= next-state 0))
                             (values (+ index 1) width)
                             (loop (+ index 1) next-state width picture? joined?
                                   (or emoji-base? (= point #xfe0f))))))))]))))))

  #|proc:rich-cell-prefix
  The private `rich-cell-prefix` scans string `text` from valid index `start` up to
  nonnegative cell budget `width`. Returns the end index and occupied cell count;
  display units are never split, and trailing zero-width marks stay with their base.
  |#
  (define rich-cell-prefix
    (lambda (text start width)
      (pcheck ([string? text] [fixnum? start] [integer? width])
        (let ([len (string-length text)])
          (let loop ([index start] [visible 0])
            (if (= index len)
                (values index visible)
                (let-values ([(end cells) (rich-cell-next text index)])
                  (if (> (+ visible cells) width)
                      (values index visible)
                      (loop end (+ visible cells))))))))))

  #|proc:rich-cell-string-width
  The private `rich-cell-string-width` returns the terminal cell width of string `text`,
  ignoring SGR escapes. This measures one logical line; tabs and controls have width zero.
  |#
  (define rich-cell-string-width
    (lambda (text)
      (pcheck ([string? text])
        (let ([len (string-length text)])
          (let loop ([index 0] [width 0])
            (if (= index len)
                width
                (let-values ([(end cells) (rich-cell-next text index)])
                  (loop end (+ width cells))))))))))
