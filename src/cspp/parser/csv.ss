(library (cspp parser csv)
  (export csv-document? csv-document-delimiter csv-document-records csv-document-warnings
          csv-record? csv-record-fields
          csv-warning? csv-warning-record-index csv-warning-field-index csv-warning-message
          parse-csv parse-csv-file)
  (import (cspp chez)
          (cspp parser private)
          (cspp parser combinator)
          (cspp internal)
          (cspp string)
          (cspp list)
          (cspp vector)
          (cspp file)
          (cspp io)
          (cspp utils))

  (define-record-type csv-document
    (fields (immutable delimiter)
            (immutable records)
            (immutable warnings)))

  (define-record-type csv-record
    (fields (immutable fields)))

  (define-record-type csv-warning
    (fields (immutable record-index)
            (immutable field-index)
            (immutable message)))

  (define-record-type parsed-csv-field
    (fields (immutable value)
            (immutable warning?)))

  (define-record-type parsed-csv-record
    (fields (immutable record)
            (immutable offset)
            (immutable warning-field-indices)))

  (define-parser-record-writer csv-document csv-document
    ([delimiter csv-document-delimiter]
     [records csv-document-records]
     [warnings csv-document-warnings]))
  (define-parser-record-writer csv-record csv-record
    ([fields csv-record-fields]))
  (define-parser-record-writer csv-warning csv-warning
    ([record-index csv-warning-record-index]
     [field-index csv-warning-field-index]
     [message csv-warning-message]))

;;;;===----------------------------------------------------------------------===
;;;; CSV grammar
;;;;===----------------------------------------------------------------------===

  (define <line-break>
    (</> (<as> #\newline (<string> "\r\n"))
         (<char> #\newline)
         (<char> #\return)))

  (define <escaped-field-character>
    (let ([original (</> (<as> #\" (<string> "\"\""))
                         (<satisfy-char>
                          (lambda (character) (not (char=? character #\")))))])
      ;; Only a quote can begin the doubled-quote alternative. Retain its EOF diagnostics.
      (define-parser escaped-field-character-parser
        (let-values ([(status character next-input)
                      (parser-call <item> (save-input inp) state lvl)])
          (if (and status (not (char=? character #\")))
              (values #t character next-input)
              (parser-call original inp state lvl))))
      escaped-field-character-parser))

  (define (<escaped-field> delimiter)
    (<~1> (<char> #\")
          (<as-string> (<many> <escaped-field-character>))
          (<char> #\")))

  (define (<unescaped-field> delimiter)
    (<map> (lambda (value)
             (make-parsed-csv-field value #f))
           (<as-string>
            (<many> (<satisfy-char>
                     (lambda (character)
                       (and (not (char=? character delimiter))
                            (not (char=? character #\"))
                            (not (char=? character #\newline))
                            (not (char=? character #\return)))))))))

  (define (<quoted-field> delimiter)
    (<map> (lambda (value)
             (make-parsed-csv-field
              (cadr value)
              (or (pair? (car value)) (pair? (caddr value)))))
           (let ([padding (if (char=? delimiter #\space)
                              (<result> '())
                              (<many> (<char> #\space)))])
             (<~> padding (<escaped-field> delimiter) padding))))

  (define (<field> delimiter)
    (</> (<quoted-field> delimiter)
         (<unescaped-field> delimiter)))

  (define (<record> delimiter)
    (<map> (lambda (value)
             (let ([offset (car value)] [fields (cadr value)])
               (let loop ([fields fields] [field-index 0] [values '()] [warnings '()])
                 (if (null? fields)
                     (make-parsed-csv-record
                      (make-csv-record (list->vector (reverse values)))
                      offset
                      (reverse warnings))
                     (let ([field (car fields)])
                       (loop (cdr fields)
                             (fx1+ field-index)
                             (cons (parsed-csv-field-value field) values)
                             (if (parsed-csv-field-warning? field)
                                 (cons field-index warnings)
                                 warnings)))))))
           (<~> <pos>
                (<sep-by1> (<field> delimiter) (<char> delimiter)))))

  (define (<document-records> delimiter)
    (</> (<as> '() <eof>)
         (<map> (lambda (value)
                  (cons (car value)
                        (filter parsed-csv-record? (cadr value))))
                (<~> (<record> delimiter)
                     (<many>
                      (~> <line-break>
                          (</> (<as> #f <eof>)
                               (<record> delimiter))))))))

  (define find-inconsistent-record
    (lambda (records)
      (if (null? records)
          #f
          (let ([width (vector-length
                        (csv-record-fields (parsed-csv-record-record (car records))))])
            (let loop ([records (cdr records)])
              (cond [(null? records) #f]
                    [(= width
                        (vector-length
                         (csv-record-fields
                          (parsed-csv-record-record (car records)))))
                     (loop (cdr records))]
                    [else (car records)]))))))

  (define csv-document-from-parsed-records
    (lambda (delimiter records)
      (let record-loop ([records records]
                        [record-index 0]
                        [public-records '()]
                        [warnings '()])
        (if (null? records)
            (make-csv-document delimiter
                               (list->vector (reverse public-records))
                               (list->vector (reverse warnings)))
            (let ([record (car records)])
              (let warning-loop
                  ([field-indices (parsed-csv-record-warning-field-indices record)]
                   [warnings warnings])
                (if (null? field-indices)
                    (record-loop (cdr records)
                                 (fx1+ record-index)
                                 (cons (parsed-csv-record-record record) public-records)
                                 warnings)
                    (warning-loop
                     (cdr field-indices)
                     (cons (make-csv-warning record-index
                                             (car field-indices)
                                             "spaces around quoted field")
                           warnings)))))))))

  (define (parser-csv delimiter)
    (<bind> (<~ (<document-records> delimiter) <eof>)
            (lambda (records)
              (let ([inconsistent-record (find-inconsistent-record records)])
                (if inconsistent-record
                    (<pos-at> (parsed-csv-record-offset inconsistent-record)
                              (<fail-with> "CSV records have inconsistent field counts"))
                    (<result> (csv-document-from-parsed-records delimiter records)))))))

  (define-who validate-csv-delimiter
    (lambda (delimiter)
      (when (or (char=? delimiter #\")
                (char=? delimiter #\newline)
                (char=? delimiter #\return))
        (errorf who "invalid CSV delimiter: ~a" delimiter))))

;;;;===----------------------------------------------------------------------===
;;;; Public API
;;;;===----------------------------------------------------------------------===

  #|proc:parse-csv
  The `parse-csv` procedure parses string `text` as CSV using optional character `delimiter`.
  The delimiter defaults to comma and cannot be double quote, carriage return, or line feed.
  |#
  (define parse-csv
    (case-lambda
      [(text) (parse-csv text #\,)]
      [(text delimiter)
       (pcheck ([string? text] [char? delimiter])
               (validate-csv-delimiter delimiter)
               (run-textual-parser (parser-csv delimiter) text))]))


  #|proc:parse-csv-file
  The `parse-csv-file` procedure parses the regular file at string `path` as CSV using optional
  character `delimiter`. The delimiter defaults to comma and cannot be double quote, carriage
  return, or line feed.
  |#
  (define parse-csv-file
    (case-lambda
      [(path) (parse-csv-file path #\,)]
      [(path delimiter)
       (pcheck ([file-regular? path] [char? delimiter])
               (validate-csv-delimiter delimiter)
               (parse-textual-file (parser-csv delimiter) path))]))

  )
