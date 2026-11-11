;; Included by the registered csv parser suite; format-specific regressions.

(mat parser-csv-ordinary-quoted-fields

     (let* ([body (make-string 100000 #\a)] [source (string-append "\"" body "\"")]
            [before (statistics)] [document (parse-csv source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
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
                (and (parser-error-from (lambda () (parse-csv source))) #t))
              '("\"abc" "\"a\"x" "a\"b")))

(mat parser-csv-unquoted-field-dispatch

     (let* ([count 10000] [text (apply string-append (make-list count "a,b,c\n"))]
            [before (statistics)] [document (parse-csv text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
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
     (for-all (lambda (text) (and (parser-error-from (lambda () (parse-csv text))) #t))
              '("a\"b,c" "a,\"b\"x" "a,b\nc")))
