(import (cspp)
        (cspp parser csv))

(include "parser-external-tools.ss")
(include "parser-test-support.ss")

(define csv-fields
  (lambda (document)
    (map (lambda (record) (vector->list (csv-record-fields record)))
         (vector->list (csv-document-records document)))))

(define csv-summary-field
  (lambda (field)
    (format "~a:~a" (string-length field) field)))

(define csv-summary
  (lambda (records)
    (let ([first (csv-record-fields (vector-ref records 0))]
          [last (csv-record-fields (vector-ref records (fx1- (vector-length records))))])
      (apply string-append
             (number->string (vector-length records))
             (map (lambda (field)
                    (string-append "|" (csv-summary-field field)))
                  (append (vector->list first) (vector->list last)))))))

(mat parse-csv-records

     (string=? "#[csv-record fields: #(\"a\" \"b\")]"
               (format "~s"
                       (vector-ref (csv-document-records (parse-csv "a,b")) 0)))

     (let ([document (parse-csv "")])
       (and (csv-document? document)
            (char=? #\, (csv-document-delimiter document))
            (equal? '() (csv-fields document))))

     (equal? '(("a" "b") ("1" "2"))
             (csv-fields (parse-csv "a,b\r\n1,2")))

     (equal? '(("aaa" "bbb" "ccc") ("xxx" "yyy" "zzz"))
             (csv-fields (parse-csv "aaa,bbb,ccc\nxxx,yyy,zzz\n")))

     (equal? '(("aaa " "  bbb " " ccc") (" xxx" " yyy  " "zzz "))
             (csv-fields (parse-csv "aaa ,  bbb , ccc\r xxx, yyy  ,zzz \r")))

     (equal? '(("aaa" "b\r\nbb" "ccc") ("xxx" "y, yy" "zzz"))
             (csv-fields (parse-csv "aaa,\"b\r\nbb\",ccc\r\nxxx,\"y, yy\",zzz")))

     (equal? '(("aaa" "b\"bb" "ccc"))
             (csv-fields (parse-csv "aaa,\"b\"\"bb\",ccc")))

     (equal? '(("" "" "" ""))
             (csv-fields (parse-csv ",,,")))

     (equal? '(("a" "b") ("1" "2"))
             (csv-fields (parse-csv "a;b\n1;2" #\;)))

     ;; error: a quoted field must end with a double quote.
     (error? (parse-csv "a,\"unterminated"))

     ;; error: a double quote cannot occur inside an unquoted field.
     (error? (parse-csv "a,b\"c"))

     ;; error: non-space data cannot follow a closing quote before the delimiter.
     (error? (parse-csv "a,\"b\"x,c"))

     ;; error: a line break cannot be used as the field delimiter.
     (error? (parse-csv "a\nb" #\newline))

     ;; error: every record must contain the same number of fields.
     (error? (parse-csv "a,b\n1,2,3"))

     (let ([document (parse-csv "a,  \"b,c\"  ,d")])
       (and (equal? '(("a" "b,c" "d")) (csv-fields document))
            (= 1 (vector-length (csv-document-warnings document)))
            (csv-warning? (vector-ref (csv-document-warnings document) 0))))

     (equal? '(("name" "value") ("alpha" "1"))
             (csv-fields (parse-csv-file "data/csv-basic.csv")))

     (let* ([document
             (parse-csv-file "data/New-Zealand-period-life-tables-2017-2019-CSV.csv")]
            [records (csv-document-records document)]
            [first (csv-record-fields (vector-ref records 0))]
            [last (csv-record-fields (vector-ref records (fx1- (vector-length records))))])
       (and (= 25663 (vector-length records))
            (equal? '#("measure" "quantile" "time" "sex" "age" "ethnic" "value") first)
            (equal? '#("ex" "97.50%" "2017-19" "Male" "100 years"
                       "European or Other" "2.37")
                    last)))

     (or (not (external-tool-available? "python3"))
         (let* ([path "data/New-Zealand-period-life-tables-2017-2019-CSV.csv"]
                [records (csv-document-records (parse-csv-file path))]
                [result
                 (process-capture
                  "python3" "-c"
                  (string-append
                   "import csv,sys\n"
                   "with open(sys.argv[1], newline='', encoding='utf-8') as csv_file:\n"
                   " rows=list(csv.reader(csv_file))\n"
                   "fields=[str(len(rows)),*rows[0],*rows[-1]]\n"
                   "print(fields[0]+'|'+'|'.join(f'{len(x)}:{x}' for x in fields[1:]), end='')")
                  (begin path)
                  :stdout capture
                  :stderr capture
                  :timeout 10000)])
           (equal? (csv-summary records) (successful-process-output result))))

     )

(include "parser-csv-regressions.ss")
