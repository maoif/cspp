(import (cspp) (cspp parser combinator))

(define parser-error-from
  (lambda (thunk)
    (guard (err [(parser-error? err) err])
      (thunk)
      #f)))

(define with-parser-file
  (lambda (bytes procedure)
    (let ([path (format "parser-regression-~a.tmp" (random 999999999))])
      (dynamic-wind
        (lambda () (write-u8vec path bytes))
        (lambda () (procedure path))
        (lambda () (when (file-exists? path) (delete-file path)))))))

(mat parser-scheme-regressions

     (equal? '(a . #\b) (parse-scheme-datum "(a . #\\b)"))

     (equal? '(a b . #\)) (parse-scheme-datum "[a b . #\\)]"))

     (equal? '((a . #f) (a . ()) (a b) #(a #\b))
             (parse-scheme "(a . #f) [a . ()] (a b) #(a #\\b)"))

     (string=? (string #\page) (parse-scheme-datum "\"\\f\""))

     ;; Error case: a dotted list must have at least one element before the dot.
     (parser-error? (parser-error-from (lambda () (parse-scheme-datum "(. a)"))))

     ;; Error case: a boolean cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#tfoo"))))

     ;; Error case: a character name cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#\\spacefoo"))))

     ;; Error case: an integer cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "123abc"))))

     ;; Error case: a radix integer must end before another radix prefix.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#x1#x2"))))

     (equal? '(#t (x) #\space (quote a) -12 + #f)
             (parse-scheme "#t(x)#\\space'a -12 + #f"))

     (equal? '(#t #f #\a #\b #\space #t)
             (parse-scheme "#t#f #\\a#\\b #\\ #t"))

     )

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
     (parser-error?
      (parser-error-from (lambda () (parse-toml "[[a.b]]\nx=1\n[[a]]"))))

     ;; Error case: bare TOML keys are restricted to ASCII.
     (parser-error? (parser-error-from (lambda () (parse-toml "é = 1"))))

     (= 1 (toml-table-ref (toml-document-root (parse-toml "'é' = 1")) "é"))

     ;; Error case: preserve the location of a missing value after an indented key.
     (let ([err (parser-error-from (lambda () (parse-toml "  a = ")))])
       (and (parser-error? err) (= 6 (parser-error-offset err))))

     )

(mat parser-json5-csv-regressions

     ;; Error case: JSON5 decimal integer parts cannot have leading zeros.
     (for-all (lambda (text)
                (parser-error? (parser-error-from (lambda () (parse-json5 text)))))
              '("00" "012" "-01" "+01.2" "01e2"))

     (equal? '(0 0.5 0.5 0.01 100.0)
             (map (lambda (text) (json5-document-value (parse-json5 text)))
                  '("0" "0.5" ".5" "1e-2" "1e02")))

     (let* ([document (parse-csv "a \"b\" c\n1 \"2\" 3" #\space)]
            [records (csv-document-records document)])
       (and (equal? '#("a" "b" "c") (csv-record-fields (vector-ref records 0)))
            (equal? '#("1" "2" "3") (csv-record-fields (vector-ref records 1)))
            (= 0 (vector-length (csv-document-warnings document)))))

     (equal? '#("" "b" "" "c" "")
             (csv-record-fields
              (vector-ref (csv-document-records (parse-csv " \"b\"  c " #\space)) 0)))

     ;; Error case: a CSV width error after CR must point to the second record.
     (let ([err (parser-error-from (lambda () (parse-csv "a,b\r1,2,3")))])
       (and (parser-error? err)
            (= 4 (parser-error-offset err))
            (= 1 (parser-error-line err))
            (= 0 (parser-error-column err))))

     )

(mat parser-xml-regressions

     (let* ([document (parse-xml "<a x='a\tb\r\nc&#9;d&#10;e&#13;f'/>")]
            [attribute (vector-ref (xml-element-attributes (xml-document-root document)) 0)])
       (string=? "a b c\td\ne\rf" (xml-attribute-value attribute)))

     (let* ([document (parse-xml "<a>one&amp;two&#65;<![CDATA[raw]]>three&amp;<!--c-->end</a>")]
            [children (xml-element-children (xml-document-root document))])
       (and (= 5 (vector-length children))
            (string=? "one&twoA" (xml-text-value (vector-ref children 0)))
            (xml-cdata? (vector-ref children 1))
            (string=? "three&" (xml-text-value (vector-ref children 2)))
            (xml-comment? (vector-ref children 3))
            (string=? "end" (xml-text-value (vector-ref children 4)))))

     (let* ([count 16000]
            [text (string-append "<a>" (apply string-append (make-list count "&amp;")) "</a>")]
            [before (statistics)]
            [root (xml-document-root (parse-xml text))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? (make-string count #\&)
                      (xml-text-value (vector-ref (xml-element-children root) 0)))
            ;; A generous allocation bound catches repeated copies of accumulated text.
            (< allocated 100000000)))

     )

(mat parser-file-regressions

     (with-parser-file #vu8()
       (lambda (path) (equal? #vu8() (read-u8vec path))))

     (with-parser-file #vu8()
       (lambda (path)
         (= 0 (vector-length
               (toml-table-entries (toml-document-root (parse-toml-file path)))))))

     ;; Error case: empty non-TOML documents fail as parser errors with their source path.
     (with-parser-file #vu8()
       (lambda (path)
         (for-all
          (lambda (parse-file)
            (let ([err (parser-error-from (lambda () (parse-file path)))])
              (and (parser-error? err)
                   (string=? path (parser-error-source err))
                   (= 0 (parser-error-offset err)))))
          (list parse-xml-file parse-elf-file parse-wasm-binary-module-file))))

     ;; Error case: decoded XML file errors preserve the path and normalized text position.
     (with-parser-file (string->utf8 "<a>\r\n</b>")
       (lambda (path)
         (let ([err (parser-error-from (lambda () (parse-xml-file path)))])
           (and (parser-error? err)
                (string=? path (parser-error-source err))
                (= 8 (parser-error-offset err))
                (= 1 (parser-error-line err))))))

     ;; Error case: decoded TOML file errors preserve the file path and failure position.
     (with-parser-file (string->utf8 "a = ")
       (lambda (path)
         (let ([err (parser-error-from (lambda () (parse-toml-file path)))])
           (and (parser-error? err)
                (string=? path (parser-error-source err))
                (= 4 (parser-error-offset err))))))

     )

(mat parser-line-regressions

     ;; Error case: all input operations must agree on the position following a line break.
     (for-all
      (lambda (newline)
        (let* ([prefix (string-append "a" newline)]
               [length (string-length prefix)]
               [text (string-append prefix "!")])
          (for-all
           (lambda (parser)
             (let ([err (parser-error-from (lambda () (run-textual-parser parser text)))])
               (and (parser-error? err)
                    (= length (parser-error-offset err))
                    (= 1 (parser-error-line err))
                    (= 0 (parser-error-column err)))))
           (list (<~> (<string> prefix) (<char> #\x))
                 (<~> (<rep> <item> length) (<char> #\x))
                 (<~> (<char> #\a) (<string> newline) (<char> #\x))
                 (<pos-at> length (<char> #\x))
                 (<~> (<string> text) (<pos-at> length (<char> #\x)))))))
      '("\n" "\r" "\r\n" "\x2028;" "\x2029;"))

     ;; Error case: CRLF is one line break even when read by separate primitive parsers.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-textual-parser (<~> (<char> #\return) (<char> #\newline)
                                            (<char> #\x))
                                       "\r\n!")))])
       (and (parser-error? err) (= 1 (parser-error-line err))
            (= 0 (parser-error-column err))))

     )

(mat parser-thread-regressions

     (let* ([workers 8] [mutex (make-mutex)] [ready (make-condition)]
            [done (make-condition)]
            [deadline (add-duration (current-time) (make-time 'time-duration 0 30))]
            [arrived 0] [finished 0] [errors '()]
            [text-parser
             (<~ (<~> (<some> <letter>)
                      (<map-st>
                       (lambda (digit state)
                         ;; Every worker is inside this same callback before parsing resumes.
                         (with-mutex mutex
                           (set! arrived (fx1+ arrived))
                           (condition-broadcast ready)
                           (let loop ()
                             (unless (= arrived workers)
                               (unless (condition-wait ready mutex deadline)
                                 (error 'parser-thread "callback barrier timed out"))
                               (loop))))
                         (unless (= digit state) (error 'parser-thread "wrong state"))
                         ;; A nested parse must not affect the outer input cursor.
                         (run-textual-parser <digit10> (number->string digit)))
                       <digit10>)
                      (<some> <letter>))
                  <eof>)]
            [binary-parser (<~ (<bounded> 2 (<~> <u8> <u8>)) <eof>)])
       (do ([worker 0 (fx1+ worker)]) [(= worker workers)]
         (fork-thread
          (lambda ()
            (guard (err [else (with-mutex mutex (set! errors (cons err errors)))])
              (unless (equal? (list '(#\a #\b) worker '(#\x #\y))
                              (run-textual-parser text-parser (format "ab~axy" worker) worker))
                (error 'parser-thread "wrong textual result"))
              (parse-elf-file "../libcspp.so")
              (do ([round 0 (fx1+ round)]) [(= round 25)]
                (unless (equal? (list worker round)
                                (run-binary-parser binary-parser (bytevector worker round)))
                  (error 'parser-thread "wrong binary result"))
                (unless (= worker (json5-document-value (parse-json5 (number->string worker))))
                  (error 'parser-thread "wrong JSON5 result"))
                (unless (= worker (toml-table-ref
                                   (toml-document-root (parse-toml (format "x=~a" worker))) "x"))
                  (error 'parser-thread "wrong TOML result"))
                (parse-csv (format "a,b\n~a,~a" worker round))
                (parse-xml (format "<a x='~a'><b/>~a</a>" worker round))
                (parse-scheme-datum (format "(a . #\\~a)" (integer->char (+ 97 worker))))
                (parse-wasm-text-module "(module (func (result i32) i32.const 42))")
                (parse-wasm-binary-module #vu8(0 97 115 109 1 0 0 0))
                ;; Error case: another worker's error must not overwrite this source name.
                (let ([err (parser-error-from
                            (lambda () (run-textual-parser/source
                                        (<char> #\x) "!" (number->string worker))))])
                  (unless (and (parser-error? err)
                               (string=? (number->string worker) (parser-error-source err)))
                    (error 'parser-thread "wrong error source")))))
            (with-mutex mutex
              (set! finished (fx1+ finished))
              (condition-broadcast done)))))
       (with-mutex mutex
         (let loop ()
           (unless (= finished workers)
             (unless (condition-wait done mutex deadline)
               (error 'parser-thread "workers timed out"))
             (loop))))
       (null? errors))

     )
