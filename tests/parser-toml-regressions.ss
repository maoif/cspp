;; Included by the registered toml parser suite; format-specific regressions.

(mat parser-toml-regressions

     (= 1 (toml-table-ref (toml-document-root (parse-toml "  x = 1\n")) "x"))

     (let* ([root (toml-document-root (parse-toml "\t[a]\n  x = 1\n \t"))]
            [table (toml-table-ref root "a")])
       (= 1 (toml-table-ref table "x")))

     (= 0 (vector-length (toml-table-entries (toml-document-root (parse-toml "  \t")))))

     (let* ([root (toml-document-root (parse-toml "[[a.b]]\nx=1\n[[a.b]]\nx=2"))]
            [array (toml-table-ref (toml-table-ref root "a") "b")])
       (equal? '(1 2)
               (map (lambda (table) (toml-table-ref table "x"))
                    (vector->list (toml-array-elements array)))))

     ;; Error case: an implicit ordinary parent cannot later become an array of tables.
     (raises-parser-error?
      (lambda () (parse-toml "[[a.b]]\nx=1\n[[a]]")))

     ;; Error case: bare TOML keys are restricted to ASCII.
     (raises-parser-error? (lambda () (parse-toml "é = 1")))

     (= 1 (toml-table-ref (toml-document-root (parse-toml "'é' = 1")) "é"))

     ;; Error case: preserve the location of a missing value after an indented key.
     (let ([err (parser-error-from (lambda () (parse-toml "  a = ")))])
       (and (parser-error? err) (= 6 (parser-error-offset err))))

     )

(mat parser-toml-implicit-dotted-review

     ;; Error case: traversing an implicit parent with a dotted key defines that parent.
     (raises-parser-error?
      (lambda () (parse-toml "[a.b.c]\nx=1\n[a]\nb.d=2\n[a.b]\ny=3")))

     (let* ([root (toml-document-root (parse-toml "[a.b.c]\nx=1\n[a]\nb.d=2"))]
            [a (toml-table-ref root "a")]
            [b (toml-table-ref a "b")])
       (= 2 (toml-table-ref b "d")))

     )

(mat parser-toml-ordinary-string-characters

     (let ([body (make-string 100000 #\a)])
       (for-all
        (lambda (delimiter budget)
          (let* ([source (string-append "v=" delimiter body delimiter)]
                 [before (statistics)] [document (parse-toml source)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (string=? body (toml-table-ref (toml-document-root document) "v"))
                 (< allocated budget))))
        '("\"" "\"\"\"" "'''") '(25000000 30000000 30000000)))

     (let ([root (toml-document-root
                  (parse-toml "a=\"x\\u0041\\n\"\nb=\"\"\"\r\nx\\\r\n  y\r\nz\"\"\""))])
       (and (string=? "xA\n" (toml-table-ref root "a"))
            (string=? "xy\nz" (toml-table-ref root "b"))))

     (and (string=? "abc\"\""
                    (toml-table-ref (toml-document-root (parse-toml "v=\"\"\"abc\"\"\"\"\"")) "v"))
          (string=? "abc''"
                    (toml-table-ref (toml-document-root (parse-toml "v='''abc'''''")) "v")))

     ;; Error cases: guards retain invalid escapes, forbidden controls and missing terminators.
     (for-all (lambda (source)
                (raises-parser-error? (lambda () (parse-toml source))))
              '("v=\"abc\\q\"" "v=\"\"\"abc\\q\"\"\"" "v='''abc\x7f;'''" "v=\"\"\"abc")))
