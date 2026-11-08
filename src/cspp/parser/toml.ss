(library (cspp parser toml)
  (export toml-document? toml-document-root
          toml-table? toml-table-entries toml-table-inline? toml-table-ref
          toml-entry? toml-entry-key toml-entry-value
          toml-array? toml-array-elements
          toml-offset-date-time? toml-offset-date-time-year toml-offset-date-time-month
          toml-offset-date-time-day toml-offset-date-time-hour toml-offset-date-time-minute
          toml-offset-date-time-second toml-offset-date-time-nanosecond
          toml-offset-date-time-offset-seconds
          toml-local-date-time? toml-local-date-time-year toml-local-date-time-month
          toml-local-date-time-day toml-local-date-time-hour toml-local-date-time-minute
          toml-local-date-time-second toml-local-date-time-nanosecond
          toml-local-date? toml-local-date-year toml-local-date-month toml-local-date-day
          toml-local-time? toml-local-time-hour toml-local-time-minute toml-local-time-second
          toml-local-time-nanosecond
          parse-toml parse-toml-file)
  (import (cspp chez)
          (cspp parser private)
          (cspp parser combinator)
          (cspp file)
          (cspp list)
          (cspp utils))

  (define-record-type toml-document
    (fields (immutable root)))

  (define-record-type toml-table
    (fields (immutable entries)
            (immutable inline?)))

  (define-record-type toml-entry
    (fields (immutable key)
            (immutable value)))

  (define-record-type toml-array
    (fields (immutable elements)))

  (define-record-type toml-offset-date-time
    (fields (immutable year) (immutable month) (immutable day)
            (immutable hour) (immutable minute) (immutable second)
            (immutable nanosecond) (immutable offset-seconds)))

  (define-record-type toml-local-date-time
    (fields (immutable year) (immutable month) (immutable day)
            (immutable hour) (immutable minute) (immutable second)
            (immutable nanosecond)))

  (define-record-type toml-local-date
    (fields (immutable year) (immutable month) (immutable day)))

  (define-record-type toml-local-time
    (fields (immutable hour) (immutable minute) (immutable second)
            (immutable nanosecond)))

  (define-parser-record-writer toml-document toml-document
    ([root toml-document-root]))
  (define-parser-record-writer toml-table toml-table
    ([entries toml-table-entries]
     [inline? toml-table-inline?]))
  (define-parser-record-writer toml-entry toml-entry
    ([key toml-entry-key]
     [value toml-entry-value]))
  (define-parser-record-writer toml-array toml-array
    ([elements toml-array-elements]))
  (define-parser-record-writer toml-offset-date-time toml-offset-date-time
    ([year toml-offset-date-time-year]
     [month toml-offset-date-time-month]
     [day toml-offset-date-time-day]
     [hour toml-offset-date-time-hour]
     [minute toml-offset-date-time-minute]
     [second toml-offset-date-time-second]
     [nanosecond toml-offset-date-time-nanosecond]
     [offset-seconds toml-offset-date-time-offset-seconds]))
  (define-parser-record-writer toml-local-date-time toml-local-date-time
    ([year toml-local-date-time-year]
     [month toml-local-date-time-month]
     [day toml-local-date-time-day]
     [hour toml-local-date-time-hour]
     [minute toml-local-date-time-minute]
     [second toml-local-date-time-second]
     [nanosecond toml-local-date-time-nanosecond]))
  (define-parser-record-writer toml-local-date toml-local-date
    ([year toml-local-date-year]
     [month toml-local-date-month]
     [day toml-local-date-day]))
  (define-parser-record-writer toml-local-time toml-local-time
    ([hour toml-local-time-hour]
     [minute toml-local-time-minute]
     [second toml-local-time-second]
     [nanosecond toml-local-time-nanosecond]))

  #|proc:toml-table-ref
  The `toml-table-ref` procedure looks up string `key` in `table`. It returns optional `default`
  when no entry has that key; `default` is `#f` when omitted.
  |#
  (define toml-table-ref
    (case-lambda
      [(table key) (toml-table-ref table key #f)]
      [(table key default)
       (pcheck ([toml-table? table] [string? key])
               (let loop ([i 0])
                 (if (fx= i (vector-length (toml-table-entries table)))
                     default
                     (let ([entry (vector-ref (toml-table-entries table) i)])
                       (if (string=? key (toml-entry-key entry))
                           (toml-entry-value entry)
                           (loop (fx1+ i)))))))]))

;;;;===----------------------------------------------------------------------===
;;;; Private events and builder nodes
;;;;===----------------------------------------------------------------------===

  (define-record-type key-value-event
    (fields (immutable offset) (immutable key-path) (immutable value)))

  (define-record-type table-event
    (fields (immutable offset) (immutable key-path)))

  (define-record-type array-table-event
    (fields (immutable offset) (immutable key-path)))

  (define-record-type validation-issue
    (fields (immutable offset) (immutable message)))

  (define-record-type mutable-table
    (fields (mutable kind)
            (immutable entries)
            (mutable order)))

  (define-record-type mutable-table-array
    (fields (mutable tables)))

  (define make-empty-mutable-table
    (lambda (kind)
      (make-mutable-table kind (make-hashtable string-hash string=?) '())))

  (define mutable-table-ref
    (lambda (table key default)
      (hashtable-ref (mutable-table-entries table) key default)))

  (define mutable-table-contains?
    (lambda (table key)
      (hashtable-contains? (mutable-table-entries table) key)))

  (define mutable-table-set-new!
    (lambda (table key value)
      (hashtable-set! (mutable-table-entries table) key value)
      (mutable-table-order-set! table (cons key (mutable-table-order table)))))

  (define issue
    (lambda (offset message)
      (make-validation-issue offset message)))

  (define latest-array-table
    (lambda (table-array)
      (and (pair? (mutable-table-array-tables table-array))
           (car (mutable-table-array-tables table-array)))))

  (define navigate-table
    (lambda (table path missing-kind offset)
      (if (null? path)
          (values table #f)
          (let* ([key (car path)]
                 [missing (list 'missing)]
                 [value (mutable-table-ref table key missing)])
            (cond [(eq? value missing)
                   (let ([child (make-empty-mutable-table missing-kind)])
                     (mutable-table-set-new! table key child)
                     (navigate-table child (cdr path) missing-kind offset))]
                  [(mutable-table? value)
                   (if (and (eq? missing-kind 'dotted)
                            (eq? (mutable-table-kind value) 'explicit))
                       (values #f (issue offset "dotted key cannot extend an explicit table"))
                       (begin
                         (when (and (eq? missing-kind 'dotted)
                                    (eq? (mutable-table-kind value) 'implicit))
                           (mutable-table-kind-set! value 'dotted))
                         (navigate-table value (cdr path) missing-kind offset)))]
                  [(mutable-table-array? value)
                   (let ([latest (latest-array-table value)])
                     (if (eq? missing-kind 'dotted)
                         (values #f (issue offset "dotted key cannot extend an array of tables"))
                         (if latest
                             (navigate-table latest (cdr path) missing-kind offset)
                             (values #f (issue offset "array of tables is empty")))))]
                  [else
                   (values #f (issue offset "value cannot be extended as a table"))])))))

  (define insert-key-value!
    (lambda (table event)
      (let* ([path (key-value-event-key-path event)]
             [parent-path (list-head path (fx1- (length path)))]
             [key (list-last path)]
             [offset (key-value-event-offset event)])
        (let-values ([(parent problem)
                      (navigate-table table parent-path 'dotted offset)])
          (cond [problem problem]
                [(mutable-table-contains? parent key)
                 (issue offset "TOML key is already defined")]
                [else
                 (mutable-table-set-new! parent key (key-value-event-value event))
                 #f])))))

  (define open-table!
    (lambda (root event)
      (let* ([path (table-event-key-path event)]
             [parent-path (list-head path (fx1- (length path)))]
             [key (list-last path)]
             [offset (table-event-offset event)])
        (let-values ([(parent problem)
                      (navigate-table root parent-path 'implicit offset)])
          (if problem
              (values #f problem)
              (let* ([missing (list 'missing)]
                     [value (mutable-table-ref parent key missing)])
                (cond [(eq? value missing)
                       (let ([table (make-empty-mutable-table 'explicit)])
                         (mutable-table-set-new! parent key table)
                         (values table #f))]
                      [(and (mutable-table? value)
                            (eq? 'implicit (mutable-table-kind value)))
                       (mutable-table-kind-set! value 'explicit)
                       (values value #f)]
                      [(mutable-table? value)
                       (values #f (issue offset "TOML table is already defined"))]
                      [else
                       (values #f
                               (issue offset
                                      "value conflicts with a table definition"))])))))))

  (define open-array-table!
    (lambda (root event)
      (let* ([path (array-table-event-key-path event)]
             [parent-path (list-head path (fx1- (length path)))]
             [key (list-last path)]
             [offset (array-table-event-offset event)])
        (let-values ([(parent problem)
                      (navigate-table root parent-path 'implicit offset)])
          (if problem
              (values #f problem)
              (let* ([missing (list 'missing)]
                     [value (mutable-table-ref parent key missing)]
                     [table (make-empty-mutable-table 'explicit)])
                (cond [(eq? value missing)
                       (mutable-table-set-new!
                        parent key (make-mutable-table-array (list table)))
                       (values table #f)]
                      [(mutable-table-array? value)
                       (mutable-table-array-tables-set!
                        value (cons table (mutable-table-array-tables value)))
                       (values table #f)]
                      [else
                       (values #f
                               (issue offset
                                      "value conflicts with an array of tables"))])))))))

  (define freeze-value
    (lambda (value inline-context?)
      (cond [(mutable-table? value) (freeze-table value inline-context?)]
            [(mutable-table-array? value)
             (make-toml-array
              (list->vector
               (map (lambda (table) (freeze-table table #f))
                    (reverse (mutable-table-array-tables value)))))]
            [else value])))

  (define freeze-table
    (lambda (table inline-context?)
      (let ([inline? (or inline-context? (eq? 'inline (mutable-table-kind table)))])
        (make-toml-table
         (list->vector
          (map (lambda (key)
                 (make-toml-entry
                  key
                  (freeze-value (mutable-table-ref table key #f) inline?)))
               (reverse (mutable-table-order table))))
         inline?))))

  (define build-document
    (lambda (events)
      (let ([root (make-empty-mutable-table 'explicit)])
        (let loop ([events events] [current root])
          (if (null? events)
              (values (make-toml-document (freeze-table root #f)) #f)
              (let ([event (car events)])
                (cond [(key-value-event? event)
                       (let ([problem (insert-key-value! current event)])
                         (if problem
                             (values #f problem)
                             (loop (cdr events) current)))]
                      [(table-event? event)
                       (let-values ([(table problem) (open-table! root event)])
                         (if problem
                             (values #f problem)
                             (loop (cdr events) table)))]
                      [(array-table-event? event)
                       (let-values ([(table problem) (open-array-table! root event)])
                         (if problem
                             (values #f problem)
                             (loop (cdr events) table)))]
                      [else (assert-unreachable)])))))))

  (define build-inline-table
    (lambda (events)
      (let ([table (make-empty-mutable-table 'inline)])
        (let loop ([events events])
          (if (null? events)
              (values (freeze-table table #t) #f)
              (let ([problem (insert-key-value! table (car events))])
                (if problem
                    (values #f problem)
                    (loop (cdr events)))))))))

;;;;===----------------------------------------------------------------------===
;;;; TOML 1.1 lexical grammar
;;;;===----------------------------------------------------------------------===

  ;; https://toml.io/en/v1.1.0

  (define <ws-character> (<one-of> " \t"))
  (define <ws> (<many> <ws-character>))
  (define <newline>
    (</> (<as> #\newline (<string> "\r\n"))
         (<char> #\newline)))

  (define valid-comment-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (or (= value #x9)
            (and (>= value #x20) (not (= value #x7f)))))))

  (define <comment>
    (<~1> (<char> #\#)
          (<many> (<satisfy-char> valid-comment-character?
                                   "invalid TOML comment character"))))

  (define <blank-line>
    (<as> #f (<~> <ws> (<optional> <comment>) <newline>)))

  (define <final-comment-line>
    (<as> #f (<~> <ws> <comment> <eof>)))

  (define unicode-scalar-value?
    (lambda (value)
      (and (integer? value)
           (exact? value)
           (<= 0 value #x10ffff)
           (not (<= #xd800 value #xdfff)))))

  (define (<unicode-escape> marker count)
    (~> (<char> marker)
        (<bind> (<rep> <digit16> count)
                (lambda (digits)
                  (let ([value (hexdigits->num digits)])
                    (if (unicode-scalar-value? value)
                        (<result> (integer->char value))
                        (<fail-with> "escape is not a Unicode scalar value")))))))

  (define <basic-escape>
    (~> (<char> #\\)
        (</> (<as> #\" (<char> #\"))
             (<as> #\\ (<char> #\\))
             (<as> #\backspace (<char> #\b))
             (<as> #\tab (<char> #\t))
             (<as> #\newline (<char> #\n))
             (<as> #\page (<char> #\f))
             (<as> #\return (<char> #\r))
             (<as> (integer->char #x1b) (<char> #\e))
             (<unicode-escape> #\x 2)
             (<unicode-escape> #\u 4)
             (<unicode-escape> #\U 8))))

  (define basic-unescaped-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (and (or (= value #x9) (>= value #x20))
             (not (= value #x7f))
             (not (char=? character #\"))
             (not (char=? character #\\))
             (not (char=? character #\newline))
             (not (char=? character #\return))))))

  (define literal-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (and (or (= value #x9) (>= value #x20))
             (not (= value #x7f))
             (not (char=? character #\'))
             (not (char=? character #\newline))
             (not (char=? character #\return))))))

  (define <basic-string>
    (<map> (lambda (characters) (apply string characters))
           (<~1> (<char> #\")
                 (<many>
                  (</> <basic-escape>
                       (<satisfy-char> basic-unescaped-character?
                                        "invalid basic string character")))
                 (<char> #\"))))

  (define <literal-string>
    (<map> (lambda (characters) (apply string characters))
           (<~1> (<char> #\')
                 (<many> (<satisfy-char> literal-character?
                                          "invalid literal string character"))
                 (<char> #\'))))

  (define <multiline-continuation>
    (<as> #f
          (<~> (<char> #\\)
               <ws>
               <newline>
               (<many> (</> <ws-character> <newline>)))))

  (define multiline-basic-unescaped-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (and (or (= value #x9) (>= value #x20))
             (not (= value #x7f))
             (not (char=? character #\\))
             (not (char=? character #\newline))
             (not (char=? character #\return))))))

  (define <multiline-basic-character>
    (<~1> (<not-followed-by> (<result> #t) (<string> "\"\"\""))
          (</> <multiline-continuation>
               <basic-escape>
               (<as> #\newline <newline>)
               (<satisfy-char> multiline-basic-unescaped-character?
                                "invalid multiline basic string character"))))

  (define <multiline-basic-close>
    (</> (<as> '(#\" #\") (<string> "\"\"\"\"\""))
         (<as> '(#\") (<string> "\"\"\"\""))
         (<as> '() (<string> "\"\"\""))))

  (define <multiline-basic-string>
    (<map> (lambda (value)
             (apply string
                    (append (filter char? (car value)) (cadr value))))
           (<~2> (<string> "\"\"\"")
                 (<optional> <newline>)
                 (<~> (<many> <multiline-basic-character>)
                      <multiline-basic-close>))))

  (define multiline-literal-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (and (or (= value #x9) (>= value #x20))
             (not (= value #x7f))
             (not (char=? character #\newline))
             (not (char=? character #\return))))))

  (define <multiline-literal-character>
    (<~1> (<not-followed-by> (<result> #t) (<string> "'''"))
          (</> (<as> #\newline <newline>)
               (<satisfy-char> multiline-literal-character?
                                "invalid multiline literal string character"))))

  (define <multiline-literal-close>
    (</> (<as> '(#\' #\') (<string> "'''''"))
         (<as> '(#\') (<string> "''''"))
         (<as> '() (<string> "'''"))))

  (define <multiline-literal-string>
    (<map> (lambda (value)
             (apply string (append (car value) (cadr value))))
           (<~2> (<string> "'''")
                 (<optional> <newline>)
                 (<~> (<many> <multiline-literal-character>)
                      <multiline-literal-close>))))

  (define <string-value>
    (</> <multiline-basic-string>
         <multiline-literal-string>
         <basic-string>
         <literal-string>))

  (define <bare-key>
    (<as-string>
     (<some> (<satisfy-char>
               (lambda (character)
                 (or (char<=? #\a character #\z)
                     (char<=? #\A character #\Z)
                     (char<=? #\0 character #\9)
                     (memv character '(#\_ #\-))))))))

  (define <simple-key>
    (</> <basic-string> <literal-string> <bare-key>))

  (define <dot-separator>
    (<~1> <ws> (<char> #\.) <ws>))

  (define <key-path>
    (<sep-by1> <simple-key> <dot-separator>))

;;;;===----------------------------------------------------------------------===
;;;; Numbers and temporal values
;;;;===----------------------------------------------------------------------===

  (define (<digit-sequence> digit-parser)
    (<map> (lambda (value)
             (apply string (cons (car value) (cadr value))))
           (<~> digit-parser
                (<many> (~> (<optional> (<char> #\_)) digit-parser)))))

  (define <decimal-sequence> (<digit-sequence> <digit>))
  (define <hex-sequence> (<digit-sequence> <hexdigit>))
  (define <octal-sequence> (<digit-sequence> <octdigit>))
  (define <binary-sequence> (<digit-sequence> <bindigit>))

  (define <unsigned-decimal-integer-source>
    (</> (<as> "0" (<char> #\0))
         (<map> (lambda (value)
                  (apply string (cons (car value) (cadr value))))
                (<~> (<one-of> "123456789")
                     (<many> (~> (<optional> (<char> #\_)) <digit>))))))

  (define <signed-decimal-integer-source>
    (<map> (lambda (value)
             (string-append (if (char? (car value))
                                (string (car value))
                                "")
                            (cadr value)))
           (<~> (<optional> (<one-of> "+-"))
                <unsigned-decimal-integer-source>)))

  (define <integer-value>
    (</> (<map> (lambda (digits) (string->number digits 16))
                (~> (<string> "0x") <hex-sequence>))
         (<map> (lambda (digits) (string->number digits 8))
                (~> (<string> "0o") <octal-sequence>))
         (<map> (lambda (digits) (string->number digits 2))
                (~> (<string> "0b") <binary-sequence>))
         (<map> string->number <signed-decimal-integer-source>)))

  (define <exponent-source>
    (<map> (lambda (value)
             (string-append (string (car value))
                            (if (char? (cadr value))
                                (string (cadr value))
                                "")
                            (caddr value)))
           (<~> (<one-of> "eE")
                (<optional> (<one-of> "+-"))
                <decimal-sequence>)))

  (define <regular-float-source>
    (</> (<map> (lambda (value)
                  (string-append (car value) "." (caddr value)
                                 (if (string? (list-ref value 3))
                                     (list-ref value 3)
                                     "")))
                (<~> <signed-decimal-integer-source>
                     (<char> #\.)
                     <decimal-sequence>
                     (<optional> <exponent-source>)))
         (<map> (lambda (value)
                  (string-append (car value) (cadr value)))
                (<~> <signed-decimal-integer-source> <exponent-source>))))

  (define <float-value>
    (</> (<map> string->number <regular-float-source>)
         (<map> (lambda (value)
                  (if (and (char? (car value)) (char=? #\- (car value)))
                      -inf.0
                      +inf.0))
                (<~> (<optional> (<one-of> "+-")) (<string> "inf")))
         (<as> +nan.0
               (<~> (<optional> (<one-of> "+-")) (<string> "nan")))))

  (define fixed-digits
    (lambda (count)
      (<map> (lambda (digits) (string->number (apply string digits)))
             (<rep> <digit> count))))

  (define leap-year?
    (lambda (year)
      (or (= 0 (mod year 400))
          (and (= 0 (mod year 4))
               (not (= 0 (mod year 100)))))))

  (define days-in-month
    (lambda (year month)
      (case month
        [(1 3 5 7 8 10 12) 31]
        [(4 6 9 11) 30]
        [(2) (if (leap-year? year) 29 28)]
        [else 0])))

  (define valid-date?
    (lambda (year month day)
      (and (<= 1 month 12)
           (<= 1 day (days-in-month year month)))))

  (define valid-time?
    (lambda (hour minute second)
      (and (<= 0 hour 23)
           (<= 0 minute 59)
           (<= 0 second 59))))

  (define fraction->nanoseconds
    (lambda (digits)
      (let* ([first-nine (if (> (string-length digits) 9)
                             (substring digits 0 9)
                             digits)]
             [padded (string-append
                      first-nine
                      (make-string (- 9 (string-length first-nine)) #\0))])
        (string->number padded))))

  (define <date-components>
    (<bind> (<~> (fixed-digits 4) (<char> #\-)
                 (fixed-digits 2) (<char> #\-)
                 (fixed-digits 2))
            (lambda (value)
              (let ([year (car value)]
                    [month (caddr value)]
                    [day (list-ref value 4)])
                (if (valid-date? year month day)
                    (<result> (list year month day))
                    (<fail-with> "invalid TOML date"))))))

  (define <fractional-seconds>
    (~> (<char> #\.)
        (<map> fraction->nanoseconds (<as-string> (<some> <digit>)))))

  (define <time-components>
    (<bind> (<~> (fixed-digits 2) (<char> #\:)
                 (fixed-digits 2)
                 (<optional>
                  (<~> (<char> #\:)
                       (fixed-digits 2)
                       (<optional> <fractional-seconds>))))
            (lambda (value)
              (let* ([hour (car value)]
                     [minute (caddr value)]
                     [seconds-part (list-ref value 3)]
                     [second (if (pair? seconds-part)
                                 (cadr seconds-part)
                                 0)]
                     [fraction (if (and (pair? seconds-part)
                                        (number? (caddr seconds-part)))
                                   (caddr seconds-part)
                                   0)])
                (if (valid-time? hour minute second)
                    (<result> (list hour minute second fraction))
                    (<fail-with> "invalid TOML time"))))))

  (define <offset-seconds>
    (</> (<as> 0 (<one-of> "Zz"))
         (<bind> (<~> (<one-of> "+-")
                      (fixed-digits 2) (<char> #\:)
                      (fixed-digits 2))
                 (lambda (value)
                   (let ([hour (cadr value)] [minute (list-ref value 3)])
                     (if (and (<= 0 hour 23) (<= 0 minute 59))
                         (<result>
                          (* (if (char=? #\- (car value)) -1 1)
                             (+ (* hour 3600) (* minute 60))))
                         (<fail-with> "invalid TOML offset")))))))

  (define <offset-date-time>
    (<map> (lambda (value)
             (apply make-toml-offset-date-time
                    (append (car value) (caddr value) (list (list-ref value 3)))))
           (<~> <date-components>
                (<one-of> "Tt ")
                <time-components>
                <offset-seconds>)))

  (define <local-date-time>
    (<map> (lambda (value)
             (apply make-toml-local-date-time
                    (append (car value) (caddr value))))
           (<~> <date-components>
                (<one-of> "Tt ")
                <time-components>)))

  (define <local-date>
    (<map> (lambda (value) (apply make-toml-local-date value))
           <date-components>))

  (define <local-time>
    (<map> (lambda (value) (apply make-toml-local-time value))
           <time-components>))

  (define <boolean-value>
    (</> (<as> #t (<string> "true"))
         (<as> #f (<string> "false"))))

;;;;===----------------------------------------------------------------------===
;;;; Recursive collections and events
;;;;===----------------------------------------------------------------------===

  (declare-lazy-parser <toml-value>)

  (define <collection-junk>
    (<many> (</> <ws-character> <newline> <comment>)))

  (define <collection-separator>
    (<~1> <collection-junk> (<char> #\,) <collection-junk>))

  (define <array-value>
    (<~2> (<char> #\[)
          <collection-junk>
          (</> (<as> (make-toml-array '#()) (<char> #\]))
               (<map> (lambda (value)
                        (make-toml-array (list->vector (car value))))
                      (<~> (<sep-by1> <toml-value> <collection-separator>)
                           (</> (~> <collection-junk> (<char> #\]))
                                (~> <collection-separator> (<char> #\]))))))))

  (define <inline-key-value>
    (<map> (lambda (value)
             (make-key-value-event (car value) (cadr value) (list-ref value 4)))
           (<~> <pos> <key-path> <ws> (<char> #\=)
                (~> <ws> <toml-value>))))

  (define <inline-table-value>
    (<~2> (<char> #\{)
          <collection-junk>
          (</> (<as> (make-toml-table '#() #t) (<char> #\}))
               (<bind> (<~> (<sep-by1> <inline-key-value> <collection-separator>)
                            (</> (~> <collection-junk> (<char> #\}))
                                 (~> <collection-separator> (<char> #\}))))
                       (lambda (value)
                         (let-values ([(table problem)
                                       (build-inline-table (car value))])
                           (if problem
                               (<fail-with> (validation-issue-message problem))
                               (<result> table))))))))

  (define <toml-value-parser>
    (</> <offset-date-time>
         <local-date-time>
         <local-date>
         <local-time>
         <string-value>
         <boolean-value>
         <array-value>
         <inline-table-value>
         <float-value>
         <integer-value>))

  (define <key-value-statement>
    (<map> (lambda (value)
             (make-key-value-event (car value) (cadr value) (list-ref value 4)))
           (<~> <pos> <key-path> <ws> (<char> #\=)
                (~> <ws> <toml-value>))))

  (define <table-statement>
    (<map> (lambda (value) (make-table-event (car value) (list-ref value 3)))
           (<~> <pos> (<char> #\[) <ws> <key-path> <ws> (<char> #\]))))

  (define <array-table-statement>
    (<map> (lambda (value)
             (make-array-table-event (car value) (list-ref value 3)))
           (<~> <pos> (<string> "[[") <ws> <key-path> <ws> (<string> "]]"))))

  (define <statement>
    (</> <array-table-statement>
         <table-statement>
         <key-value-statement>))

  (define <statement-line>
    (<~ <statement>
        (<~> <ws> (<optional> <comment>) (</> <newline> <eof>))))

  (define <document-events>
    (<map> (lambda (values) (filter (lambda (value) value) values))
           (<~0> (<many-until>
                  (</> <blank-line> <final-comment-line> (~> <ws> <statement-line>))
                  (<~> <ws> <eof>))
                 <ws> <eof>)))

  (define parser-toml
    (begin
      (install-lazy-parser! <toml-value> <toml-value-parser>)
      (<bind> <document-events>
              (lambda (events)
                (let-values ([(document problem) (build-document events)])
                  (if problem
                      (<pos-at> (validation-issue-offset problem)
                                (<fail-with> (validation-issue-message problem)))
                      (<result> document)))))))

;;;;===----------------------------------------------------------------------===
;;;; Public API
;;;;===----------------------------------------------------------------------===

  #|proc:parse-toml
  The `parse-toml` procedure parses TOML 1.1 string `text` and returns a `toml-document`.
  |#
  (define parse-toml
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-toml text))))

  #|proc:parse-toml-file
  The `parse-toml-file` procedure parses the regular UTF-8 TOML file at string `path` and returns
  a `toml-document`. Syntax failures raise `&parser-error` with `path` as the source. File and
  UTF-8 decoding exceptions propagate as non-parser exceptions.
  |#
  (define parse-toml-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (run-textual-parser/source
               parser-toml (parser-read-text-file path) path))))

  )
