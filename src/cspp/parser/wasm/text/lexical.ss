(library (cspp parser wasm text lexical)
  (export <wat-trivia> <wat-token> <wat-keyword> <wat-identifier> <wat-annotation-head>
          <wat-string> <wat-name> <wat-u32> <wat-u64> <wat-i8> <wat-i16> <wat-i32> <wat-i64>
          <wat-f32> <wat-f64>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser private)
          (cspp parser wasm types)
          (cspp string)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; Character classes, trivia, and tokens
;;;;===----------------------------------------------------------------------===

  (define wat-whitespace-character?
    (lambda (character)
      (memv character '(#\space #\tab #\newline #\return))))

  (define wat-identifier-character?
    (lambda (character)
      (or (char<=? #\0 character #\9)
          (char<=? #\a character #\z)
          (char<=? #\A character #\Z)
          (and (memv character
                     '(#\! #\# #\$ #\% #\& #\' #\* #\+ #\- #\. #\/ #\: #\< #\=
                       #\> #\? #\@ #\\ #\^ #\_ #\` #\| #\~))
               #t))))

  (define <wat-whitespace>
    (<satisfy-char> wat-whitespace-character? "not WebAssembly text whitespace"))

  (define <wat-identifier-character>
    (<satisfy-char> wat-identifier-character? "not a WebAssembly identifier character"))

  (define wat-double-quote (integer->char #x22))
  (define wat-close-parenthesis (integer->char #x29))

  (define digit-sequence-parser
    (case-lambda
      [(digit-parser radix) (digit-sequence-parser digit-parser radix #f)]
      [(digit-parser radix limit)
       (let ([convert
              (if limit
                  ;; Keep scanning after overflow without allocating larger exact integers.
                  (lambda (digits)
                    (fold-left (lambda (number digit)
                                 (if (> number limit) number (+ (* number radix) digit)))
                               0 digits))
                  (if (fx= radix 10) digits->num hexdigits->num))])
         (<map> (lambda (value)
                  (let ([digits (cons (car value) (cadr value))])
                    (cons (convert digits) (length digits))))
                (<~> digit-parser
                     (<many> (</> digit-parser
                                  (~> (<char> #\_) digit-parser))))))]))

  (define <wat-hexadecimal-digits> (digit-sequence-parser <digit16> 16))

  (define <wat-line-comment>
    (<~1> (<string> ";;")
          (parser-discard-many (<satisfy-char>
                   (lambda (character)
                     (and (not (char=? character #\newline))
                          (not (char=? character #\return))))
                   "not a line-comment character"))))

  (declare-lazy-parser <wat-block-comment>)
  (declare-lazy-parser <wat-any-annotation>)
  (declare-lazy-parser <wat-annotation-group>)
  (declare-lazy-parser <wat-ignored-annotation>)

  (define <wat-block-comment-character>
    (<~1> (<not-followed-by> (<result> #t)
                             (</> (<string> "(;") (<string> ";)")))
          <item>))

  (define <wat-block-comment-element>
    (let ([original (</> <wat-block-comment> <wat-block-comment-character>)])
      ;; Only '(' and ';' can begin a nested opener or closer. Keep both alternatives
      ;; for these prefixes and EOF, including their complete failure causes.
      (define-parser block-comment-element-parser
        (let-values ([(status character next-input)
                      (parser-call <item> (save-input inp) state lvl)])
          (if (and status (not (memv character '(#\( #\;))))
              (values #t character next-input)
              (parser-call original inp state lvl))))
      block-comment-element-parser))

  #|proc:<wat-trivia>
  The `<wat-trivia>` parser consumes WebAssembly whitespace, comments, and generic annotations.
  It leaves `@custom` annotations for the module grammar and returns the empty list.
  |#
  (define <wat-trivia>
    (begin
      (install-lazy-parser!
       <wat-block-comment>
       (<~1> (<string> "(;")
             (parser-discard-many <wat-block-comment-element>)
             (<string> ";)")))
      (<as> '()
            (parser-discard-many (</> <wat-whitespace> <wat-line-comment> <wat-block-comment>
                         <wat-ignored-annotation>)))))

  (define <wat-token-boundary>
    (</> <eof>
         <wat-whitespace>
         (<string> ";;")
         (<string> "(;")
         (<string> "(@")
         (<one-of> "()")))

  #|proc:<wat-token>
  The `<wat-token>` procedure takes `parser`, whose signature is `(TextInput -> Any)`, and returns
  a parser that reads its value and consumes following WebAssembly text trivia.
  |#
  (define <wat-token>
    (lambda (parser)
      (pcheck ([parser? parser])
              (<~0> parser
                    (<followed-by> (<result> '()) <wat-token-boundary>)
                    <wat-trivia>))))

  #|proc:<wat-keyword>
  The `<wat-keyword>` procedure takes nonempty string `keyword` and returns a parser that reads it
  only at a token boundary. The parser returns a fresh copy of `keyword` and consumes trivia.
  |#
  (define-who (<wat-keyword> keyword)
    (pcheck ([string? keyword])
            (if (zero? (string-length keyword))
                (errorf who "keyword must not be empty")
                (<wat-token>
                 (<not-followed-by> (<string> keyword) <wat-identifier-character>)))))

;;;;===----------------------------------------------------------------------===
;;;; Byte strings and names
;;;;===----------------------------------------------------------------------===

  (define unicode-scalar-value?
    (lambda (value)
      (and (integer? value)
           (exact? value)
           (<= 0 value #x10ffff)
           (not (<= #xd800 value #xdfff)))))

  (define byte-fragments->bytevector
    (lambda (fragment*)
      (let* ([count (fold-left (lambda (count fragment)
                                (+ count (bytevector-length fragment)))
                              0 fragment*)]
             [bytes (make-bytevector count)])
        (let loop ([offset 0] [fragment* fragment*])
          (unless (null? fragment*)
            (let* ([fragment (car fragment*)]
                   [length (bytevector-length fragment)])
              (bytevector-copy! fragment 0 bytes offset length)
              (loop (fx+ offset length) (cdr fragment*)))))
        bytes)))

  (define byte-fragments-length
    (lambda (fragment*)
      (fold-left (lambda (count fragment)
                   (+ count (bytevector-length fragment)))
                 0 fragment*)))

  (define character->utf8
    (lambda (character)
      (string->utf8 (string character))))

  (define <wat-simple-string-escape>
    (</> (<as> #vu8(#x09) (<char> #\t))
         (<as> #vu8(#x0a) (<char> #\n))
         (<as> #vu8(#x0d) (<char> #\r))
         (<as> #vu8(#x22) (<char> wat-double-quote))
         (<as> #vu8(#x27) (<char> #\'))
         (<as> #vu8(#x5c) (<char> #\\))))

  (define <wat-hex-byte-escape>
    (<map> (lambda (digits)
             (bytevector (hexdigits->num digits)))
           (<rep> <digit16> 2)))

  (define <wat-unicode-escape>
    (<~2> (<char> #\u)
          (<char> #\{)
          (<bind> (digit-sequence-parser <digit16> 16 #x10ffff)
                  (lambda (source)
                    (let ([value (car source)])
                      (if (unicode-scalar-value? value)
                          (<result> (character->utf8 (integer->char value)))
                          (<fail-with> "invalid Unicode scalar escape")))))
          (<char> #\})))

  (define <wat-string-escape>
    (~> (<char> #\\)
        (</> <wat-unicode-escape>
             <wat-simple-string-escape>
             <wat-hex-byte-escape>)))

  (define <wat-raw-string-character>
    (<satisfy-char>
     (lambda (character)
       (let ([value (char->integer character)])
         (and (>= value #x20)
              (not (= value #x7f))
              (not (char=? character wat-double-quote))
              (not (char=? character #\\))
              (unicode-scalar-value? value))))
     "invalid unescaped string character"))

  ;; Encode a whole ordinary run once; byte escapes remain separate raw fragments.
  (define <wat-raw-string-run>
    (<map> (lambda (characters) (string->utf8 (list->string characters)))
           (<some> <wat-raw-string-character>)))

  (define <wat-annotation-string>
    (<~1> (<char> wat-double-quote)
          (<many> (</> <wat-string-escape> <wat-raw-string-run>))
          (<char> wat-double-quote)))

  (define <wat-annotation-character>
    (<satisfy-char>
     (lambda (character)
       (and (or (wat-whitespace-character? character)
                (char<=? #\space character #\x7e))
            (not (memv character '(#\( #\) #\")))))
     "invalid annotation character"))

  (define <wat-annotation-content>
    (let ([original
           (</> <wat-any-annotation> <wat-annotation-group>
                <wat-annotation-string> <wat-block-comment> <wat-line-comment>
                <wat-annotation-character>)])
      (define-parser annotation-content-parser
        (let-values ([(status character next-input)
                      (parser-call <item> (save-input inp) state lvl)])
          (if (and status
                   (not (memv character '(#\( #\) #\" #\;)))
                   (or (wat-whitespace-character? character)
                       (char<=? #\space character #\x7e)))
              (values #t character next-input)
              (parser-call original inp state lvl))))
      annotation-content-parser))

  (define <wat-annotation-id>
    (</> (<bind> <wat-annotation-string>
                 (lambda (fragments)
                   (guard (condition [else (<fail-with> "invalid UTF-8 annotation name")])
                     (<result> (strict-utf8->string (byte-fragments->bytevector fragments))))))
         (<as-string> (<many> <wat-identifier-character>))))

  (define <wat-byte-string-raw>
    (begin
      (install-lazy-parser!
       <wat-any-annotation>
       (<~1> (<string> "(@")
             <wat-annotation-id>
             (parser-discard-many <wat-annotation-content>)
             (<char> wat-close-parenthesis)))
      (install-lazy-parser!
       <wat-annotation-group>
       (<~1> (<not-followed-by> (<char> #\() (<one-of> ";@"))
             (parser-discard-many <wat-annotation-content>) (<char> wat-close-parenthesis)))
      (install-lazy-parser!
       <wat-ignored-annotation>
       (<~1> (<string> "(@")
             (<bind> <wat-annotation-id>
                     (lambda (identifier)
                       (cond [(zero? (string-length identifier))
                              (<fail-with> "annotation identifier cannot be empty")]
                             [(string=? identifier "custom")
                              (<fail-with> "custom annotation is not trivia")]
                             [else (<result> identifier)])))
             (parser-discard-many <wat-annotation-content>)
             (<char> wat-close-parenthesis)))
      (<bind> (<~1> (<char> wat-double-quote)
                    (<many> (</> <wat-string-escape> <wat-raw-string-run>))
                    (<char> wat-double-quote))
              (lambda (fragment*)
                (if (< (byte-fragments-length fragment*) (ash 1 32))
                    (<result> (byte-fragments->bytevector fragment*))
                    (<fail-with> "WebAssembly string is too long"))))))

  (define strict-utf8->string
    (lambda (bytes) (parser-decode-bytes bytes 'utf-8)))

  (define <wat-name-raw>
    (<bind> <wat-byte-string-raw>
            (lambda (bytes)
              (guard (condition [else (<fail-with> "invalid UTF-8 WebAssembly name")])
                (<result> (strict-utf8->string bytes))))))

  #|proc:<wat-string>
  The `<wat-string>` parser reads a quoted WebAssembly byte string. It returns a newly allocated
  bytevector containing raw UTF-8 bytes and decoded simple, byte, or Unicode escapes.
  |#
  (define <wat-string> (<wat-token> <wat-byte-string-raw>))

  #|proc:<wat-name>
  The `<wat-name>` parser reads a quoted byte string containing strict UTF-8. It returns the
  decoded Scheme string and consumes following trivia.
  |#
  (define <wat-name>
    (<wat-token> <wat-name-raw>))

  #|proc:<wat-annotation-head>
  The `<wat-annotation-head>` procedure takes nonempty string `name`. It returns a parser for
  the opening parenthesis and annotation ID denoting `name`, allowing bare and quoted IDs.
  The parser returns the decoded ID and consumes following trivia, leaving the annotation body.
  |#
  (define (<wat-annotation-head> name)
    (pcheck ([(lambda (value) (and (string? value) (positive? (string-length value)))) name])
            (<~1> (<string> "(@")
                  (<satisfy> <wat-annotation-id> (lambda (id) (string=? name id))
                             "unexpected annotation identifier")
                  <wat-trivia>)))

  #|proc:<wat-identifier>
  The `<wat-identifier>` parser reads a dollar sign and a bare identifier or nonempty quoted
  UTF-8 name. It returns the decoded identifier prefixed by a dollar sign and consumes trivia.
  |#
  (define <wat-identifier>
    (<wat-token>
     (~> (<char> #\$)
         (<map> (lambda (name) (string-append "$" name))
                (</> (<as-string> (<some> <wat-identifier-character>))
                     (<bind> <wat-name-raw>
                             (lambda (name)
                               (if (positive? (string-length name))
                                   (<result> name)
                                   (<fail-with> "quoted identifier is empty")))))))))

;;;;===----------------------------------------------------------------------===
;;;; Integer spellings
;;;;===----------------------------------------------------------------------===

  (define <wat-decimal-digits> (digit-sequence-parser <digit10> 10))

  (define bounded-natural-source
    (lambda (width)
      (let ([limit (- (ash 1 width) 1)])
        (</> (~> (<string> "0x") (digit-sequence-parser <digit16> 16 limit))
             (digit-sequence-parser <digit10> 10 limit)))))

  (define <wat-sign>
    (<optional> (<one-of> "+-")))

  (define signed-magnitude-parser
    (lambda (magnitude-parser)
      (<map> (lambda (value) (cons (car value) (cadr value)))
             (<~> <wat-sign> magnitude-parser))))

  (define bounded-natural-parser
    (lambda (width)
      (<bind> (bounded-natural-source width)
              (lambda (source)
                (let ([value (car source)])
                  (if (< value (ash 1 width))
                      (<result> value)
                      (<fail-with> "unsigned integer literal is out of range")))))))

  (define modular-integer-parser
    (lambda (width)
      (<bind> (signed-magnitude-parser (bounded-natural-source width))
              (lambda (source)
                (let* ([sign (car source)]
                       [magnitude (cadr source)]
                       [negative? (and (char? sign) (char=? sign #\-))]
                       [limit (ash 1 (if negative? (fx1- width) width))])
                  (if (<= magnitude (if negative? limit (- limit 1)))
                      (<result> (modulo (if negative? (- magnitude) magnitude)
                                        (ash 1 width)))
                      (<fail-with> "integer literal is out of range")))))))

  (define numeric-token-parser
    (lambda (parser)
      (<wat-token> (<not-followed-by> parser <wat-identifier-character>))))

  #|proc:<wat-u32>
  The `<wat-u32>` parser reads a decimal or hexadecimal natural with separators. It returns an
  unsigned 32-bit integer and consumes following trivia.
  |#
  (define <wat-u32> (numeric-token-parser (bounded-natural-parser 32)))

  #|proc:<wat-u64>
  The `<wat-u64>` parser reads a decimal or hexadecimal natural with separators. It returns an
  unsigned 64-bit integer and consumes following trivia.
  |#
  (define <wat-u64> (numeric-token-parser (bounded-natural-parser 64)))

  #|proc:<wat-i8>
  The `<wat-i8>` parser reads an eight-bit integer spelling for a SIMD lane. Signed negative
  spellings must fit the signed range; nonnegative spellings may use all eight bits.
  It returns the value modulo 256 and consumes following trivia.
  |#
  (define <wat-i8> (numeric-token-parser (modular-integer-parser 8)))

  #|proc:<wat-i16>
  The `<wat-i16>` parser reads a sixteen-bit integer spelling for a SIMD lane. Signed negative
  spellings must fit the signed range; nonnegative spellings may use all sixteen bits.
  It returns the value modulo 65536 and consumes following trivia.
  |#
  (define <wat-i16> (numeric-token-parser (modular-integer-parser 16)))

  #|proc:<wat-i32>
  The `<wat-i32>` parser reads a signed decimal or hexadecimal 32-bit integer. It returns the
  integer modulo two to the power 32 and consumes following trivia.
  |#
  (define <wat-i32> (numeric-token-parser (modular-integer-parser 32)))

  #|proc:<wat-i64>
  The `<wat-i64>` parser reads a signed decimal or hexadecimal 64-bit integer. It returns the
  integer modulo two to the power 64 and consumes following trivia.
  |#
  (define <wat-i64> (numeric-token-parser (modular-integer-parser 64)))

;;;;===----------------------------------------------------------------------===
;;;; Floating-point spellings and exact IEEE encoding
;;;;===----------------------------------------------------------------------===

  (define signed-value
    (lambda (sign value)
      (if (and (char? sign) (char=? sign #\-)) (- value) value)))

  (define power-scale
    (lambda (value base exponent)
      (if (negative? exponent)
          (/ value (expt base (- exponent)))
          (* value (expt base exponent)))))

  ;; For source length N <= most-positive-fixnum, significand bits and fractional
  ;; adjustment are each at most 4*N. Beyond this cap neither can cancel an exponent
  ;; back into the f64 rounding range. Continue scanning after overflow as usual.
  (define <wat-exponent-digits>
    (digit-sequence-parser <digit10> 10 (+ (* 8 (most-positive-fixnum)) 2048)))

  (define exponent-parser
    (lambda (marker)
      (~> (<one-of> marker)
          (<map> (lambda (source)
                   (signed-value (car source) (car (cadr source))))
                 (<~> <wat-sign> <wat-exponent-digits>)))))

  (define <wat-decimal-exponent> (exponent-parser "eE"))
  (define <wat-binary-exponent> (exponent-parser "pP"))

  ;; Keep powers symbolic until the target format's bounds have been checked.
  ;; Combining mantissa digits costs at most the size of the source literal.
  (define float-source
    (lambda (integral fractional digit-radix exponent-radix exponent)
      (let* ([fractional-value (car fractional)]
             [fractional-count (cdr fractional)]
             [significand (+ (* integral (expt digit-radix fractional-count))
                             fractional-value)]
             [fractional-bits (if (= digit-radix 16) (* 4 fractional-count)
                                  fractional-count)])
        (vector significand exponent-radix (- exponent fractional-bits)))))

  (define float-body-parser
    (lambda (digits digit-radix exponent-radix exponent-parser)
      (let ([suffix-parser
             (</> (<map> (lambda (value)
                           (cons (if (pair? (car value)) (car value) '(0 . 0))
                                 (if (integer? (cadr value)) (cadr value) 0)))
                         (~> (<char> #\.)
                             (<~> (<optional> digits) (<optional> exponent-parser))))
                  (<map> (lambda (exponent) (cons '(0 . 0) exponent)) exponent-parser)
                  (<result> '((0 . 0) . 0)))]
            ;; Only used when the initial digits fail; retain all three original causes.
            [missing-prefix-parser (</> digits digits digits)])
        (define-parser body-parser
          (let ([start (save-input inp)])
            (let-values ([(status integral next) (parser-call digits inp state lvl)])
              (if status
                  (let-values ([(status suffix next)
                                (parser-call suffix-parser next state lvl)])
                    (if status
                        (values #t (float-source (car integral) (car suffix)
                                                digit-radix exponent-radix (cdr suffix)) next)
                        (values #f #f next)))
                  (parser-call missing-prefix-parser start state lvl)))))
        body-parser)))

  (define decimal-float-body-parser
    (lambda () (float-body-parser <wat-decimal-digits> 10 10 <wat-decimal-exponent>)))

  (define hexadecimal-float-body-parser
    (lambda ()
      (~> (<string> "0x")
          (float-body-parser <wat-hexadecimal-digits> 16 2 <wat-binary-exponent>))))

  (define round-rational-to-even
    (lambda (value)
      (let-values ([(integer remainder) (div-and-mod (numerator value)
                                                    (denominator value))])
        (let ([comparison (- (* 2 remainder) (denominator value))])
          (cond [(positive? comparison) (+ integer 1)]
                [(negative? comparison) integer]
                [(odd? integer) (+ integer 1)]
                [else integer])))))

  (define floor-log2-rational
    (lambda (value)
      (let* ([numerator (numerator value)]
             [denominator (denominator value)]
             [candidate (- (integer-length numerator)
                           (integer-length denominator))])
        (if (< value (power-scale 1 2 candidate))
            (- candidate 1)
            candidate))))

  (define finite-float-bits
    (lambda (width value negative-zero?)
      (let* ([fraction-width (if (fx= width 32) 23 52)]
             [exponent-width (if (fx= width 32) 8 11)]
             [bias (fx1- (ash 1 (fx1- exponent-width)))]
             [minimum-exponent (- 1 bias)]
             [maximum-exponent bias]
             [sign-bit (if (or (negative? value) negative-zero?)
                           (ash 1 (fx1- width))
                           0)]
             [absolute-value (abs value)])
        (if (zero? absolute-value)
            sign-bit
            (let ([exponent (floor-log2-rational absolute-value)])
              (if (< exponent minimum-exponent)
                  (let ([fraction
                         (round-rational-to-even
                          (power-scale absolute-value 2
                                       (- fraction-width minimum-exponent)))])
                    (if (= fraction (ash 1 fraction-width))
                        (logor sign-bit (ash 1 fraction-width))
                        (logor sign-bit fraction)))
                  (let* ([significand
                          (round-rational-to-even
                           (power-scale absolute-value 2 (- fraction-width exponent)))]
                         [carry? (= significand (ash 1 (fx1+ fraction-width)))]
                         [exponent (if carry? (fx1+ exponent) exponent)]
                         [significand (if carry? (ash significand -1) significand)])
                    (if (> exponent maximum-exponent)
                        #f
                        (logor sign-bit
                               (ash (+ exponent bias) fraction-width)
                               (- significand (ash 1 fraction-width)))))))))))

  (define special-float-parser
    (lambda (width)
      (let* ([fraction-width (if (fx= width 32) 23 52)]
             [exponent-width (if (fx= width 32) 8 11)]
             [exponent-bits (ash (fx1- (ash 1 exponent-width)) fraction-width)]
             [canonical-payload (ash 1 (fx1- fraction-width))]
             [nan-payload
              (~> (<string> "nan:0x")
                  (<bind> (digit-sequence-parser
                           <digit16> 16 (- (ash 1 fraction-width) 1))
                          (lambda (source)
                            (let ([payload (car source)])
                              (if (and (positive? payload)
                                       (< payload (ash 1 fraction-width)))
                                  (<result> payload)
                                  (<fail-with> "NaN payload is out of range"))))))]
             [body
              (</> (<as> exponent-bits (<string> "inf"))
                   (<map> (lambda (payload) (logor exponent-bits payload)) nan-payload)
                   (<as> (logor exponent-bits canonical-payload) (<string> "nan")))])
        (<map> (lambda (value)
                 (let ([sign (car value)] [bits (cadr value)])
                   (if (and (char? sign) (char=? sign #\-))
                       (logor (ash 1 (fx1- width)) bits)
                       bits)))
               (<~> <wat-sign> body)))))

  (define bounded-float-value
    (lambda (width source)
      (let* ([significand (vector-ref source 0)]
             [radix (vector-ref source 1)]
             [exponent (vector-ref source 2)]
             [bits (integer-length significand)]
             [maximum-exponent (if (= width 32) 127 1023)]
             [half-subnormal-exponent (if (= width 32) -150 -1075)]
             ;; 2^(bits-1) <= significand < 2^bits, and 2^3 < 10 < 2^4.
             ;; These conservative bounds leave every rounding boundary to exact arithmetic.
             [lower-exponent (+ (- bits 1)
                                (* exponent (if (= radix 2) 1
                                                (if (negative? exponent) 4 3))))]
             [upper-exponent (+ bits
                                (* exponent (if (= radix 2) 1
                                                (if (negative? exponent) 3 4))))])
        (cond [(zero? significand) 0]
              [(> lower-exponent (+ maximum-exponent 1)) #f]
              [(<= upper-exponent half-subnormal-exponent) 0]
              [else (power-scale significand radix exponent)]))))

  (define finite-float-parser
    (lambda (width)
      (<bind> (<~> <wat-sign>
                   (</> (hexadecimal-float-body-parser)
                        (decimal-float-body-parser)))
              (lambda (source)
                (let* ([sign (car source)]
                       [negative? (and (char? sign) (char=? sign #\-))]
                       [magnitude (bounded-float-value width (cadr source))]
                       [bits (and magnitude
                                  (finite-float-bits
                                   width (if negative? (- magnitude) magnitude)
                                   negative?))])
                  (if bits
                      (<result> bits)
                      (<fail-with> "finite float literal is out of range")))))))

  (define float-parser
    (lambda (width)
      (numeric-token-parser
       (<map> (lambda (bits) (make-wasm-float width bits))
              (</> (special-float-parser width)
                   (finite-float-parser width))))))

  #|proc:<wat-f32>
  The `<wat-f32>` parser reads a WebAssembly decimal, hexadecimal, infinity, or NaN literal. It
  returns a `wasm-float` preserving the exact IEEE 754 binary32 bits and consumes trivia.
  |#
  (define <wat-f32> (float-parser 32))

  #|proc:<wat-f64>
  The `<wat-f64>` parser reads a WebAssembly decimal, hexadecimal, infinity, or NaN literal. It
  returns a `wasm-float` preserving the exact IEEE 754 binary64 bits and consumes trivia.
  |#
  (define <wat-f64> (float-parser 64))

  )
