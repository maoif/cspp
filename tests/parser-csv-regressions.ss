;; Included by the registered csv parser suite; format-specific regressions.

(mat parser-csv-ordinary-quoted-fields

     (let*-values ([(body) (make-string 100000 #\a)]
                   [(source) (string-append "\"" body "\"")]
                   [(document allocated)
                    (parser-test-allocation (lambda () (parse-csv source)))])
       (and (string=? body (vector-ref
                           (csv-record-fields (vector-ref (csv-document-records document) 0)) 0))
            (< allocated 25000000)))

     (let ([records (csv-document-records (parse-csv "\"a\"\"b\",c\r\n\"d\r\ne\",f"))])
       (equal? '(#("a\"b" "c") #("d\r\ne" "f"))
               (map csv-record-fields (vector->list records))))

     (let* ([document (parse-csv " \"abc\" ,d")]
            [warning (vector-ref (csv-document-warnings document) 0)])
       (and (string=? "abc" (vector-ref
                            (csv-record-fields (vector-ref (csv-document-records document) 0)) 0))
            (= 0 (csv-warning-record-index warning))
            (= 0 (csv-warning-field-index warning))))

     ;; Error cases: ordinary runs must not hide missing quotes or characters after a quote.
     (for-all (lambda (source)
                (raises-parser-error? (lambda () (parse-csv source))))
              '("\"abc" "\"a\"x" "a\"b")))

(mat parser-csv-unquoted-field-dispatch

     (let*-values ([(count) 10000]
                   [(text) (apply string-append (make-list count "a,b,c\n"))]
                   [(document allocated)
                    (parser-test-allocation (lambda () (parse-csv text)))])
       (and (= count (vector-length (csv-document-records document)))
            (equal? '#("a" "b" "c")
                    (csv-record-fields (vector-ref (csv-document-records document) 0)))
            (< allocated 40000000)))

     (for-all
      (lambda (delimiter)
        (let* ([text (string-append "a" (string delimiter) "" (string delimiter) "c")]
               [first (parse-csv text delimiter)] [second (parse-csv text delimiter)]
               [field (vector-ref
                       (csv-record-fields (vector-ref (csv-document-records first) 0)) 0)])
          (string-set! field 0 #\z)
          (equal? '#("a" "" "c")
                  (csv-record-fields (vector-ref (csv-document-records second) 0)))))
      '(#\, #\; #\space #\tab #\|))

     (let* ([document (parse-csv "a, \"b\" ,c")]
            [warning (vector-ref (csv-document-warnings document) 0)])
       (and (= 0 (csv-warning-record-index warning))
            (= 1 (csv-warning-field-index warning))))

     ;; Error cases: dispatch must retain illegal quotes and inconsistent record widths.
     (for-all (lambda (text) (raises-parser-error? (lambda () (parse-csv text))))
              '("a\"b,c" "a,\"b\"x" "a,b\nc")))

(mat parser-csv-warning-order

     (let* ([document (parse-csv "a, \"b\",c\n \"d\",e, \"f\"")]
            [warnings (csv-document-warnings document)])
       (and (equal? '(#("a" "b" "c") #("d" "e" "f"))
                    (map csv-record-fields
                         (vector->list (csv-document-records document))))
            (= 3 (vector-length warnings))
            (equal? '((0 1) (1 0) (1 2))
                    (map (lambda (warning)
                           (list (csv-warning-record-index warning)
                                 (csv-warning-field-index warning)))
                         (vector->list warnings))))))
