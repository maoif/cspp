(library (cspp parser json5)
  (export json5-document? json5-document-value
          json5-object? json5-object-members
          json5-member? json5-member-name json5-member-value
          json5-array? json5-array-elements
          json5-null?
          parse-json5 parse-json5-file)
  (import (cspp chez)
          (cspp parser private)
          (cspp parser combinator)
          (cspp file)
          (cspp list)
          (cspp utils))

  (define-record-type json5-document
    (fields (immutable value)))

  (define-record-type json5-object
    (fields (immutable members)))

  (define-record-type json5-member
    (fields (immutable name)
            (immutable value)))

  (define-record-type json5-array
    (fields (immutable elements)))

  (define-record-type json5-null
    (fields))

  (define-parser-record-writer json5-document json5-document
    ([value json5-document-value]))
  (define-parser-record-writer json5-object json5-object
    ([members json5-object-members]))
  (define-parser-record-writer json5-member json5-member
    ([name json5-member-name]
     [value json5-member-value]))
  (define-parser-record-writer json5-array json5-array
    ([elements json5-array-elements]))
  (define-parser-record-writer json5-null json5-null ())

  (define json5-null-value (make-json5-null))

;;;;===----------------------------------------------------------------------===
;;;; Character classes and token boundaries
;;;;===----------------------------------------------------------------------===

  (define json5-line-terminator?
    (lambda (character)
      (or (char=? character #\newline)
          (char=? character #\return)
          (char=? character (integer->char #x2028))
          (char=? character (integer->char #x2029)))))

  (define json5-whitespace?
    (lambda (character)
      (or (memv character
                (list #\tab #\vtab #\page #\space
                      (integer->char #xa0)
                      (integer->char #xfeff)))
          (json5-line-terminator? character)
          (eq? 'Zs (char-general-category character)))))

  (define identifier-start-character?
    (lambda (character)
      (or (char=? character #\$)
          (char=? character #\_)
          (memq (char-general-category character)
                '(Lu Ll Lt Lm Lo Nl)))))

  (define identifier-part-character?
    (lambda (character)
      (or (identifier-start-character? character)
          (memq (char-general-category character)
                '(Mn Mc Nd Pc))
          (char=? character (integer->char #x200c))
          (char=? character (integer->char #x200d)))))

  (define unicode-scalar-value?
    (lambda (value)
      (and (integer? value)
           (exact? value)
           (<= 0 value #x10ffff)
           (not (<= #xd800 value #xdfff)))))

  (define <hex-code-unit>
    (<map> hexdigits->num (<rep> <digit16> 4)))

  (define (<escaped-identifier-character> predicate)
    (~> (<string> "\\u")
        (<bind> <hex-code-unit>
                (lambda (value)
                  (if (unicode-scalar-value? value)
                      (let ([character (integer->char value)])
                        (if (predicate character)
                            (<result> character)
                            (<fail-with> "invalid escaped identifier character")))
                      (<fail-with> "invalid escaped identifier character"))))))

  (define (<identifier-character> predicate message)
    (</> (<escaped-identifier-character> predicate)
         (<satisfy-char> predicate message)))

  (define <identifier-start>
    (<identifier-character> identifier-start-character?
                            "invalid identifier start"))

  (define <identifier-part>
    (<identifier-character> identifier-part-character?
                            "invalid identifier part"))

  (define <identifier-name-raw>
    (<map> (lambda (value)
             (apply string (cons (car value) (cadr value))))
           (<~> <identifier-start> (<many> <identifier-part>))))

;;;;===----------------------------------------------------------------------===
;;;; Whitespace and comments
;;;;===----------------------------------------------------------------------===

  (define <json5-whitespace>
    (<satisfy-char> json5-whitespace? "not JSON5 whitespace"))

  (define <line-comment>
    (<~1> (<string> "//")
          (<many> (<satisfy-char>
                   (lambda (character)
                     (not (json5-line-terminator? character)))))))

  (define <block-comment>
    (<~1> (<string> "/*")
          (<many> (<~1> (<not-followed-by> (<result> #t) (<string> "*/"))
                        <item>))
          (<string> "*/")))

  (define <junk>
    (<many> (</> <json5-whitespace> <line-comment> <block-comment>)))

  (define (<json5-token> parser)
    (<~ parser <junk>))

  (define <identifier-name>
    (<json5-token> <identifier-name-raw>))

  (define (<literal-token> text value)
    (<json5-token>
     (<as> value
           (<not-followed-by> (<string> text) <identifier-part>))))

;;;;===----------------------------------------------------------------------===
;;;; Strings
;;;;===----------------------------------------------------------------------===

  (define combine-surrogates
    (lambda (high low)
      (+ #x10000
         (* (- high #xd800) #x400)
         (- low #xdc00))))

  (define <unicode-string-escape>
    (~> (<char> #\u)
        (<bind> <hex-code-unit>
                (lambda (first)
                  (cond [(<= #xd800 first #xdbff)
                         (<bind> (<~2> (<char> #\\)
                                         (<char> #\u)
                                         <hex-code-unit>)
                                 (lambda (second)
                                   (if (<= #xdc00 second #xdfff)
                                       (<result>
                                        (integer->char
                                         (combine-surrogates first second)))
                                       (<fail-with>
                                        "high surrogate must be followed by a low surrogate"))))]
                        [(<= #xdc00 first #xdfff)
                         (<fail-with> "unexpected low surrogate")]
                        [else (<result> (integer->char first))])))))

  (define <hex-string-escape>
    (~> (<char> #\x)
        (<map> (lambda (digits)
                 (integer->char (hexdigits->num digits)))
               (<rep> <digit16> 2))))

  (define <line-continuation>
    (<as> #f
          (</> (<string> "\r\n")
               (<char> #\newline)
               (<char> #\return)
               (<char> (integer->char #x2028))
               (<char> (integer->char #x2029)))))

  (define <single-character-escape>
    (</> (<as> #\backspace (<char> #\b))
         (<as> #\page (<char> #\f))
         (<as> #\newline (<char> #\n))
         (<as> #\return (<char> #\r))
         (<as> #\tab (<char> #\t))
         (<as> #\vtab (<char> #\v))
         (<not-followed-by> (<as> #\nul (<char> #\0)) <digit>)))

  (define <non-escape-character>
    (<satisfy-char>
     (lambda (character)
       (and (not (json5-line-terminator? character))
            (not (char<=? #\0 character #\9))
            (not (char=? character #\x))
            (not (char=? character #\u))))
     "invalid string escape"))

  (define <escaped-string-character>
    (~> (<char> #\\)
        (</> <line-continuation>
             <unicode-string-escape>
             <hex-string-escape>
             <single-character-escape>
             <non-escape-character>)))

  (define json5-string-parser
    (lambda (quote)
      (<json5-token>
       (<map> (lambda (characters)
                (apply string (filter char? characters)))
              (<~1> (<char> quote)
                    (<many>
                     (</> <escaped-string-character>
                          (<satisfy-char>
                           (lambda (character)
                             (and (not (char=? character quote))
                                  (not (char=? character #\\))
                                  (not (json5-line-terminator? character))))
                           "invalid unescaped string character")))
                    (<char> quote))))))

  (define <json5-string>
    (</> (json5-string-parser #\")
         (json5-string-parser #\')))

;;;;===----------------------------------------------------------------------===
;;;; Numbers
;;;;===----------------------------------------------------------------------===

  (define optional-sign->string
    (lambda (sign)
      (if (char? sign) (string sign) "")))

  (define <decimal-digits>
    (<as-string> (<some> <digit>)))

  (define <decimal-integer>
    (</> (<string> "0")
         (<map> (lambda (value) (apply string (cons (car value) (cadr value))))
                (<~> (<one-of> "123456789") (<many> <digit>)))))

  (define <decimal-base>
    (</> (<map> (lambda (value)
                  (string-append (car value) "." (caddr value)))
                (<~> <decimal-integer> (<char> #\.)
                     (<as-string> (<many> <digit>))))
         (<map> (lambda (value)
                  (string-append "." (cadr value)))
                (<~> (<char> #\.) <decimal-digits>))
         <decimal-integer>))

  (define <decimal-exponent>
    (<map> (lambda (value)
             (string-append (string (car value))
                            (optional-sign->string (cadr value))
                            (caddr value)))
           (<~> (<one-of> "eE")
                (<optional> (<one-of> "+-"))
                <decimal-digits>)))

  (define <decimal-source>
    (<map> (lambda (value)
             (string-append (optional-sign->string (car value))
                            (cadr value)
                            (if (string? (caddr value)) (caddr value) "")))
           (<~> (<optional> (<one-of> "+-"))
                <decimal-base>
                (<optional> <decimal-exponent>))))

  (define <json5-decimal>
    (<json5-token>
     (<map> string->number
            (<not-followed-by> <decimal-source> <identifier-part>))))

  (define <json5-hexadecimal>
    (<json5-token>
     (<map> (lambda (value)
              (let ([number (hexdigits->num (caddr value))])
                (if (char=? (car value) #\-)
                    (- number)
                    number)))
            (<not-followed-by>
             (<~> (</> (<one-of> "+-") (<result> #\+))
                  (</> (<string> "0x") (<string> "0X"))
                  (<some> <digit16>))
             <identifier-part>))))

  (define (<signed-special-number> text positive negative)
    (<json5-token>
     (<map> (lambda (value)
              (if (and (char? (car value)) (char=? (car value) #\-))
                  negative
                  positive))
            (<~> (<optional> (<one-of> "+-"))
                 (<not-followed-by> (<string> text) <identifier-part>)))))

  (define <json5-number>
    (</> (<signed-special-number> "Infinity" +inf.0 -inf.0)
         (<signed-special-number> "NaN" +nan.0 +nan.0)
         <json5-hexadecimal>
         <json5-decimal>))

;;;;===----------------------------------------------------------------------===
;;;; Recursive values
;;;;===----------------------------------------------------------------------===

  (declare-lazy-parser <json5-value>)

  (define <left-brace> (<json5-token> (<char> #\{)))
  (define <right-brace> (<json5-token> (<char> #\})))
  (define <left-bracket> (<json5-token> (<char> #\[)))
  (define <right-bracket> (<json5-token> (<char> #\])))
  (define <colon> (<json5-token> (<char> #\:)))
  (define <comma> (<json5-token> (<char> #\,)))

  (define <json5-member-name>
    (</> <json5-string> <identifier-name>))

  (define <json5-member>
    (<map> (lambda (value)
             (make-json5-member (car value) (caddr value)))
           (<~> <json5-member-name> <colon> <json5-value>)))

  (define <json5-object>
    (<~1> <left-brace>
          (</> (<as> (make-json5-object '#()) <right-brace>)
               (<map> (lambda (value)
                        (make-json5-object (list->vector (car value))))
                      (<~> (<sep-by1> <json5-member> <comma>)
                           (</> <right-brace>
                                (~> <comma> <right-brace>)))))))

  (define <json5-array>
    (<~1> <left-bracket>
          (</> (<as> (make-json5-array '#()) <right-bracket>)
               (<map> (lambda (value)
                        (make-json5-array (list->vector (car value))))
                      (<~> (<sep-by1> <json5-value> <comma>)
                           (</> <right-bracket>
                                (~> <comma> <right-bracket>)))))))

  (define <json5-null>
    (<literal-token> "null" json5-null-value))

  (define <json5-boolean>
    (</> (<literal-token> "true" #t)
         (<literal-token> "false" #f)))

  (define parser-json5
    (begin
      (install-lazy-parser!
       <json5-value>
       (</> <json5-object>
            <json5-array>
            <json5-null>
            <json5-boolean>
            <json5-string>
            <json5-number>))
      (<map> make-json5-document
             (<~1> <junk> <json5-value> <eof>))))

;;;;===----------------------------------------------------------------------===
;;;; Public API
;;;;===----------------------------------------------------------------------===

  #|proc:parse-json5
  The `parse-json5` procedure parses JSON5 string `text` and returns a `json5-document`.
  |#
  (define parse-json5
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-json5 text))))

  #|proc:parse-json5-file
  The `parse-json5-file` procedure parses the regular file at string `path` as JSON5 and returns
  a `json5-document`.
  |#
  (define parse-json5-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-textual-file parser-json5 path))))

  )
