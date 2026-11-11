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

  ;; Record fields are immutable. Accessors return stored strings/vectors without copying;
  ;; these objects may be mutable, so a record wrapper does not imply deep immutability.

  #|proc:csv-document?
  Returns #t when object is a parsed CSV document, and #f otherwise. The object parameter may be
  any Scheme value.
  |#
  #|proc:csv-document-delimiter
  Returns the delimiter character used for parsing from record, a csv-document record. The
  stored value is returned without copying.
  |#
  #|proc:csv-document-records
  Returns a vector of csv-record records in source order from record, a csv-document record. The
  stored value is returned without copying.
  |#
  #|proc:csv-document-warnings
  Returns a vector of csv-warning records in record/field order from record, a csv-document
  record. The stored value is returned without copying.
  |#
  (define-record-type csv-document
    (fields (immutable delimiter)
            (immutable records)
            (immutable warnings)))

  #|proc:csv-record?
  Returns #t when object is a CSV record, and #f otherwise. The object parameter may be any
  Scheme value.
  |#
  #|proc:csv-record-fields
  Returns a vector of field strings in source order, including empty fields from record, a
  csv-record record. The stored value is returned without copying.
  |#
  (define-record-type csv-record
    (fields (immutable fields)))

  #|proc:csv-warning?
  Returns #t when object is a CSV warning, and #f otherwise. The object parameter may be any
  Scheme value.
  |#
  #|proc:csv-warning-record-index
  Returns the zero-based record index from record, a csv-warning record. The stored value is
  returned without copying.
  |#
  #|proc:csv-warning-field-index
  Returns the zero-based field index within that record from record, a csv-warning record. The
  stored value is returned without copying.
  |#
  #|proc:csv-warning-message
  Returns the warning string describing removed spaces around a quoted field from record, a
  csv-warning record. The stored value is returned without copying.
  |#
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
    (let* ([unescaped (<unescaped-field> delimiter)]
           [original (</> (<quoted-field> delimiter) unescaped)])
      (define-parser field-parser
        (let-values ([(status character next-input)
                      (parser-call <item> (save-input inp) state lvl)])
          (parser-call
           (if (and status (not (memv character '(#\space #\")))) unescaped original)
           inp state lvl)))
      field-parser))

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
  Returns a `csv-document` with ordered record and warning vectors; empty input has no records.
  Every record must have the same field count. Line endings may be CR, LF, or CRLF.
  Spaces around quoted fields are removed with warnings; unquoted spaces remain field data.
  The delimiter defaults to comma and cannot be double quote, carriage return, or line feed.
  Syntax and inconsistent-width failures raise `&parser-error`.
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
  character `delimiter`, and returns a `csv-document` as described by `parse-csv`.
  The delimiter defaults to comma and cannot be double quote, carriage return, or line feed.
  Input is strict UTF-8, ignoring one initial UTF-8 signature and preserving newline characters.
  Syntax failures raise `&parser-error` with `path` as the source. File and decoding conditions
  propagate without being converted to parser errors.
  |#
  (define parse-csv-file
    (case-lambda
      [(path) (parse-csv-file path #\,)]
      [(path delimiter)
       (pcheck ([file-regular? path] [char? delimiter])
               (validate-csv-delimiter delimiter)
               (parse-textual-file (parser-csv delimiter) path))]))

  )
