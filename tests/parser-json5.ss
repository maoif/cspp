(import (cspp)
        (cspp parser json5))

(include "parser-external-tools.ss")
(include "parser-test-support.ss")

(define json5-value
  (lambda (text)
    (json5-document-value (parse-json5 text))))

(define json5-object-ref
  (lambda (object name)
    (let loop ([index 0] [members (json5-object-members object)])
      (if (fx= index (vector-length members))
          #f
          (let ([member (vector-ref members index)])
            (if (string=? name (json5-member-name member))
                (json5-member-value member)
                (loop (fx1+ index) members)))))))

(mat parse-json5-records

     (string=? "#[json5-member name: \"answer\" value: 42]"
               (format "~s"
                       (vector-ref
                        (json5-object-members
                         (json5-document-value (parse-json5 "{answer: 42}")))
                        0)))

     (json5-null? (json5-value "null"))

     (let ([value (json5-value "{answer: 42}")])
       (and (json5-object? value)
            (= 1 (vector-length (json5-object-members value)))
            (let ([member (vector-ref (json5-object-members value) 0)])
              (and (json5-member? member)
                   (string=? "answer" (json5-member-name member))
                   (= 42 (json5-member-value member))))))

     (let ([value (json5-value "[1, true, 'three']")])
       (and (json5-array? value)
            (equal? '(1 #t "three")
                    (vector->list (json5-array-elements value)))))

     )

(mat parse-json5-lexical

     (= 1 (json5-value "\t\v\f\r\n\xA0;\x2028;\x2029;\xFEFF;1"))

     (= 1 (json5-value "/* before */ 1 // after"))

     ;; error: a block comment must terminate.
     (error? (parse-json5 "/* unterminated"))

     (json5-object?
      (json5-value "{$value: 1, _x: 2, cafe\x301;: 3, null: 4}"))

     (string=? "escaped"
               (json5-member-name
                (vector-ref
                 (json5-object-members (json5-value "{\\u0065scaped: 1}"))
                 0)))

     ;; error: an identifier name cannot start with an ASCII digit.
     (error? (parse-json5 "{1name: true}"))

     ;; error: a literal token cannot be followed by an identifier character.
     (error? (parse-json5 "trueValue"))

     ;; error: Infinity must be a complete numeric token.
     (error? (parse-json5 "Infinityx"))

     )

(mat parse-json5-strings

     (string=? "\x27;\x22;\\\x8;\xC;\n\r\t\xB;\x0;"
               (json5-value "'\\\'\\\"\\\\\\b\\f\\n\\r\\t\\v\\0'"))

     (string=? "AC/DC" (json5-value "'\\A\\C\\/\\D\\C'"))

     (string=? "line continuation"
               (json5-value "'line \\\r\ncontinuation'"))

     (and (string=? "ab" (json5-value "'a\\\nb'"))
          (string=? "ab" (json5-value "'a\\\rb'"))
          (string=? "ab" (json5-value "'a\\\r\nb'"))
          (string=? "ab" (json5-value "'a\\\x2028;b'"))
          (string=? "ab" (json5-value "'a\\\x2029;b'")))

     (string=? "\x1F3BC;" (json5-value "'\\uD83C\\uDFBC'"))

     ;; error: a zero escape cannot be followed by a decimal digit.
     (error? (parse-json5 "'\\01'"))

     ;; error: a high surrogate escape must be followed by a low surrogate escape.
     (error? (parse-json5 "'\\uD83C'"))

     )

(mat parse-json5-numbers

     (and (= 0 (json5-value "0"))
          (= 125.0 (json5-value "125."))
          (= 0.125 (json5-value ".125"))
          (= 0.01 (json5-value "0.1e-1"))
          (= 12500 (json5-value "125e2"))
          (= #xdecaf (json5-value "+0xdecaf"))
          (= #x-123 (json5-value "-0X123"))
          (infinite? (json5-value "-Infinity"))
          (nan? (json5-value "-NaN")))

     ;; error: hexadecimal syntax requires at least one digit.
     (error? (parse-json5 "0x"))

     ;; error: an exponent requires at least one digit.
     (error? (parse-json5 "1e+"))

     )

(mat parse-json5-structures

     (let ([object (json5-value "{}")]
           [array (json5-value "[]")])
       (and (json5-object? object)
            (= 0 (vector-length (json5-object-members object)))
            (json5-array? array)
            (= 0 (vector-length (json5-array-elements array)))))

     (let ([value
            (json5-value
             "{/*a*/key/*b*/:/*c*/[/*d*/1/*e*/,/*f*/2/*g*/,/*h*/]/*i*/,/*j*/}")])
       (and (json5-object? value)
            (= 1 (vector-length (json5-object-members value)))
            (let ([array (json5-member-value
                          (vector-ref (json5-object-members value) 0))])
              (and (json5-array? array)
                   (equal? '(1 2) (vector->list (json5-array-elements array)))))))

     (let ([value (json5-value "{a: 1, a: 2, nested: {items: [true, null,],},}")])
       (let ([members (json5-object-members value)])
         (and (= 3 (vector-length members))
              (string=? "a" (json5-member-name (vector-ref members 0)))
              (= 1 (json5-member-value (vector-ref members 0)))
              (string=? "a" (json5-member-name (vector-ref members 1)))
              (= 2 (json5-member-value (vector-ref members 1)))
              (json5-object? (json5-member-value (vector-ref members 2))))))

     ;; error: arrays permit at most one trailing comma.
     (error? (parse-json5 "[1,,]"))

     ;; error: objects permit at most one trailing comma.
     (error? (parse-json5 "{a: 1,,}"))

     )

(mat parse-json5-files

     (let* ([object (json5-document-value
                     (parse-json5-file "data/json5-example.json5"))]
            [nested (json5-object-ref object "nested")]
            [values (json5-array-elements (json5-object-ref nested "values"))])
       (and (string=? "parser suite" (json5-object-ref object "title"))
            (= #xdecaf (json5-object-ref object "color"))
            (string=? "line continuation" (json5-object-ref object "message"))
            (= 3 (vector-length values))
            (= 1 (vector-ref values 0))
            (eq? #t (vector-ref values 1))
            (json5-null? (vector-ref values 2))))

     (let* ([object (json5-document-value
                     (parse-json5-file "data/uiua-primitives.json"))]
            [apply (json5-object-ref object "&ap")]
            [arguments (json5-object-ref object "&args")]
            [breakpoint (json5-object-ref object "&b")])
       (and (= 208 (vector-length (json5-object-members object)))
            (= 1 (json5-object-ref apply "args"))
            (= 0 (json5-object-ref apply "outputs"))
            (string=? "Env" (json5-object-ref arguments "class"))
            (eq? #t (json5-object-ref breakpoint "experimental"))))

     (or (not (external-tool-available? "jq"))
         (let* ([path "data/uiua-primitives.json"]
                [object (json5-document-value (parse-json5-file path))]
                [apply (json5-object-ref object "&ap")]
                [arguments (json5-object-ref object "&args")]
                [breakpoint (json5-object-ref object "&b")]
                [summary
                 (format "[~a,~a,~a,~s,~a]\n"
                         (vector-length (json5-object-members object))
                         (json5-object-ref apply "args")
                         (json5-object-ref apply "outputs")
                         (json5-object-ref arguments "class")
                         (if (json5-object-ref breakpoint "experimental") "true" "false"))]
                [result
                 (process-capture
                  "jq" "-c"
                  (string-append
                   "[length, .[\"&ap\"].args, .[\"&ap\"].outputs, "
                   ".[\"&args\"].class, .[\"&b\"].experimental]")
                  (begin path)
                  :stdout capture
                  :stderr capture
                  :timeout 10000)])
           (equal? summary (successful-process-output result))))

     )

(include "parser-json5-regressions.ss")
