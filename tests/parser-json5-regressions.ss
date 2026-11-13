;; Included by the registered json5 parser suite; format-specific regressions.

(mat parser-json5-whitespace-cost

     (let*-values ([(text) (string-append (make-string 100000 #\space) "42")]
                   [(document allocated)
                    (parser-test-allocation (lambda () (parse-json5 text)))])
       (and (= 42 (json5-document-value document)) (< allocated 30000000))))

(mat parser-json5-ordinary-strings-and-identifiers

     (let*-values ([(body) (make-string 100000 #\a)]
                   [(source) (string-append "\"" body "\"")]
                   [(value allocated)
                    (parser-test-allocation (lambda () (json5-document-value (parse-json5 source))))])
       (and (string=? body value) (< allocated 28000000)))

     (let*-values ([(name) (make-string 100000 #\a)]
                   [(source) (string-append "{" name ":0}")]
                   [(document allocated)
                    (parser-test-allocation (lambda () (parse-json5 source)))]
                   [(member) (vector-ref (json5-object-members (json5-document-value document)) 0)])
       (and (string=? name (json5-member-name member))
            (= 0 (json5-member-value member)) (< allocated 25000000)))

     (let* ([document (parse-json5 "{ab\\u0063:'x\\u0020y\\n'}")]
            [member (vector-ref (json5-object-members (json5-document-value document)) 0)])
       (and (string=? "abc" (json5-member-name member))
            (string=? "x y\n" (json5-member-value member))))

     (string=? (string #\x2028 #\x2029)
               (json5-document-value (parse-json5 "'\x2028;\x2029;'")))

     ;; Error cases: ordinary runs must retain invalid escapes, surrogate and identifier checks.
     (for-all (lambda (source)
                (raises-parser-error? (lambda () (parse-json5 source))))
              '("\"abc\\xQ0\"" "\"abc\\uD800\"" "{\\u0030x:1}" "\"abc")))
