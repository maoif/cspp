(library (cspp parser scheme)
  (export parse-scheme-datum parse-scheme parse-scheme-file)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp internal)
          (cspp list)
          (cspp io)
          (cspp vector)
          (cspp utils))

  ;; TODO how to handle cycles?
  ;; no comments
  (define-values (parser-scheme-datum parser-scheme)
    (let ()
      (define cons-back!
        (lambda (ls v)
          (assert (not (null? ls)))
          (let loop ([ls1 ls])
            (if (null? (cdr ls1))
                (begin (set-cdr! ls1 v)
                       ls)
                (loop (cdr ls1))))))
      (define-who char->num
        (lambda (c)
          (if (char<=? #\0 c #\9)
              (fx- (char->integer c) 48)
              (errorf who "not a digit: ~a" c))))

      (declare-lazy-parser <s-datum>)

      (define <delimiter>
        (</> <eof> <whitespace> (<one-of> "()[]\"'`,;")))
      (define <boolean/character-delimiter>
        (</> <delimiter> (<char> #\#)))
      (define <scalar-token>
        (case-lambda
          [(parser) (<scalar-token> parser <delimiter>)]
          [(parser delimiter-parser)
           (<token> (<followed-by> parser delimiter-parser))]))
      (define <nat>
        (<map> (lambda (val)
                 (fold-left (lambda (s n) (+ (* 10 s) (char->num n))) 0 val))
               (<some> <digit>)))

      (define <left-paren>    (<token> (<char> #\()))
      (define <right-paren>   (<token> (<char> #\))))
      (define <left-bracket>  (<token> (<char> #\[)))
      (define <right-bracket> (<token> (<char> #\])))
      (define <left-brace>    (<token> (<char> #\{)))
      (define <right-brace>   (<token> (<char> #\})))
      ;; TODO chez allows dot to precede none-digit
      (define <dot>           (<token> (<followed-by> (<char> #\.) <boolean/character-delimiter>)))
      (define <left-vector-delim> (<token> (<string> "#(")))

      (define <quote>             (<string> "'"))
      (define <quasiquote>        (<string> "`"))
      (define <unquote>           (<string> ","))
      (define <unquote-splicing>  (<string> ",@"))
      (define <syntax>            (<string> "#'"))
      (define <quasisyntax>       (<string> "#`"))
      (define <unsyntax>          (<string> "#,"))
      (define <unsyntax-splicing> (<string> "#,@"))
      (define <abbrev-prefix>
        (<token> (</> (<as> 'quote <quote>)
                      (<as> 'quasiquote <quasiquote>)
                      ;; order matters
                      (<as> 'unquote-splicing <unquote-splicing>)
                      (<as> 'unquote <unquote>)
                      (<as> 'syntax <syntax>)
                      (<as> 'quasisyntax <quasisyntax>)
                      (<as> 'unsyntax-splicing <unsyntax-splicing>)
                      (<as> 'unsyntax <unsyntax>))))
      (define <s-abbrev> (<map> (lambda (val) (cons (car val) (list (cadr val))))
                                (<~> <abbrev-prefix> <s-datum>)))

      ;; simplified identifier parser
      (define <constituent> <letter>)
      (define <special-initial> (<one-of> "!$%&*/:<=>?^_~"))
      (define <special-subsequent> (<one-of> "+-.@"))
      (define <initial> (</> <constituent> <special-initial>))
      (define <subsequent> (</> <initial> <digit> <special-subsequent>))
      (define <peculiar-identifier>
        (</> (<map> (lambda (val)
                      (string-append (car val)
                                     ;; list of chars
                                     (list->string (cadr val))))
                    (<~> (<string> "->") (<many> <subsequent>)))
             ;; order matters: -> vs. -
             (<string> "+") (<string> "-") (<string> "...")))
      (define <identifier>
        (<map> string->symbol
               (</> (<map> (lambda (val)
                             (list->string (cons (car val) (cadr val))))
                           (<~> <initial> (<many> <subsequent>)))
                    <peculiar-identifier>)))
      (define <s-symbol> (<scalar-token> <identifier>))

      (define <s-bool>
        (<scalar-token> (</> (<as> #t (<string> "#t"))
                            (<as> #t (<string> "#T"))
                            (<as> #f (<string> "#f"))
                            (<as> #f (<string> "#F")))
                        <boolean/character-delimiter>))

      ;; no flonums, cflnums, exactness
      ;; TODO deforest
      (define <sign> (</> (<as> 1 (<char> #\+)) (<as> -1 (<char> #\-)) (<result> 1)))
      (define <s-decimal>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 10)))
               (</> (<~> <sign> (<as-string> (<some> <digit>)))
                    (<~n> 1
                          (</> (<string> "#d") (<string> "#D"))
                          (<~> <sign> (<as-string> (<some> <digit>)))))))
      (define <s-octal>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 8)))
               (<~n> 1
                     (</> (<string> "#o") (<string> "#O"))
                     (<~> <sign> (<as-string> (<some> <octdigit>))))))
      (define <s-hex>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 16)))
               (<~n> 1
                     (</> (<string> "#x") (<string> "#X"))
                     (<~> <sign> (<as-string> (<some> <hexdigit>))))))
      (define <s-bin>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 2)))
               (<~n> 1
                     (</> (<string> "#b") (<string> "#B"))
                     (<~> <sign> (<as-string> (<some> <bindigit>))))))
      (define <s-num> (<scalar-token> (</> <s-decimal> <s-octal> <s-hex> <s-bin>)))

      (define <s-special-char> (</> (<as> #\nul       (<string> "nul"))
                                    (<as> #\alarm     (<string> "alarm"))
                                    (<as> #\backspace (<string> "backspace"))
                                    (<as> #\tab       (<string> "tab"))
                                    (<as> #\linefeed  (<string> "linefeed"))
                                    (<as> #\newline   (<string> "newline"))
                                    (<as> #\vtab      (<string> "vtab"))
                                    (<as> #\page      (<string> "page"))
                                    (<as> #\return    (<string> "return"))
                                    (<as> #\esc       (<string> "esc"))
                                    (<as> #\space     (<string> "space"))
                                    ;; TODO refine
                                    (<as> #\space     (<string> " "))
                                    (<as> #\delete    (<string> "delete"))))
      (define <s-ordinary-char> <item>)
      ;; no hex char
      (define <s-char>
        (<scalar-token> (~> (<string> "#\\") (</> <s-special-char> <s-ordinary-char>))
                        <boolean/character-delimiter>))

      (define <escape-seq> (~> (<char> #\\) (</> (<as> #\" (<char> #\"))
                                                 (<as> #\\ (<char> #\\))
                                                 (<as> #\alarm     (<char> #\a))
                                                 (<as> #\backspace (<char> #\b))
                                                 (<as> #\page      (<char> #\f))
                                                 (<as> #\newline   (<char> #\n))
                                                 (<as> #\return    (<char> #\r))
                                                 (<as> #\tab       (<char> #\t))
                                                 (<as> #\nul       (<char> #\0)))))
      ;; no inline hex escape
      (define <s-literal-newline>
        (<as> #\newline
              (</> (<string> "\r\n") (<string> "\r\x85;")
                   (<one-of> "\r\n\x85;\x2028;"))))
      (define <s-str-elements>
        (</> <s-literal-newline> (<none-of> "\"\\") <escape-seq>))
      (define <s-string> (<token>
                          (</> (<as> "" (<string> "\"\""))
                               (<as-string>
                                (<~n> 1 (<char> #\") (<some> <s-str-elements>) (<char> #\"))))))

      (define (<list> left-delimiter right-delimiter)
        (~> left-delimiter
            (</> (<as> '() right-delimiter)
                 (<bind> (<some> <s-datum>)
                         (lambda (elements)
                           (</> (<as> elements right-delimiter)
                                (<map> (lambda (tail) (cons-back! elements tail))
                                       (<~1> <dot> <s-datum> right-delimiter))))))))
      (define <s-list>
        (</> (<list> <left-paren> <right-paren>)
             (<list> <left-bracket> <right-bracket>)))
      (define <s-vector>
        (</> (<as> '#() (<~> <left-vector-delim> (<many> <whitespace>) <right-paren>))
             (<map> list->vector
                    (<~n> 1 <left-vector-delim> (<many> <s-datum>) <right-paren>))))

      (define <s-lexeme-datum> (</> <s-bool> <s-char> <s-string> <s-num> <s-symbol>))
      (define <s-compound-datum> (</> <s-abbrev> <s-list> <s-vector>))
      (install-lazy-parser! <s-datum> (</> <s-lexeme-datum> <s-compound-datum>))

      (values (<fully> <s-datum>) (<fully> (<many> <s-datum>)))))


  #|proc:parse-scheme-datum
  The `parse-scheme-datum` procedure parses one datum from string `text` and returns its value.
  The supported subset includes booleans, characters, strings, radix integers, simple symbols,
  lists, vectors, and quote/syntax abbreviations. Comments, datum labels, hexadecimal character
  escapes, and non-integer numbers are unsupported. Syntax failures raise `&parser-error`.
  |#
  (define parse-scheme-datum
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-scheme-datum text))))


  #|proc:parse-scheme
  The `parse-scheme` procedure parses zero or more datums from string `text` and returns a list
  of their values. It accepts the subset described by `parse-scheme-datum`; syntax failures
  raise `&parser-error`.
  |#
  (define parse-scheme
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-scheme text))))


  #|proc:parse-scheme-file
  The `parse-scheme-file` procedure reads the regular text file at string `path` and returns a
  list of parsed datums. It accepts the subset described by `parse-scheme-datum`; syntax
  failures raise `&parser-error` with `path` as the source. File and decoding exceptions propagate.
  |#
  (define parse-scheme-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-textual-file parser-scheme path))))


  )
