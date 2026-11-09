(import (cspp) (cspp parser combinator))

(define parser-error-from
  (lambda (thunk)
    (guard (err [(parser-error? err) err])
      (thunk)
      #f)))

(define parser-review-bytes
  (lambda bytevectors
    (u8-list->bytevector (apply append (map bytevector->u8-list bytevectors)))))

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

(mat parser-combinator-review

     ;; Error case: random access cannot escape a bounded byte range, including nested bounds.
     (for-all
      (lambda (parser)
        (parser-error?
         (parser-error-from (lambda () (run-binary-parser parser #vu8(42 7))))))
      (list (<~1> <u8> (<bounded> 1 (<~ (<pos-at> 0 <u8>) <u8>)))
            (<~1> <u8> (<bounded> 1 (<bounded> 1 (<pos-at> 0 <u8>))))))

     ;; Error case: a predicate failure identifies the rejected character before consumption.
     (let ([err (parser-error-from
                 (lambda () (run-textual-parser (<one-of> "x") "\n")))])
       (and (= 0 (parser-error-offset err))
            (= 0 (parser-error-line err)) (= 0 (parser-error-column err))))

     ;; Error case: custom failures survive equal-offset choice merging.
     (let ([err (parser-error-from
                 (lambda () (run-textual-parser
                             (</> (<fail-with> "first error")
                                  (<fail-with> "second error") (<char> #\x)) "")))])
       (and (string-contains? (parser-error-message err) "first error")
            (string-contains? (parser-error-message err) "second error")
            (= 3 (length (parser-error-causes err)))))

     ;; Error case: radix digits must be exact nonnegative integers below the base.
     (error? (digits->num '(-1 2)))

     ;; Error case: binary digits cannot be fractional.
     (error? (bindigits->num '(1/2 1)))

     ;; Error case: hexadecimal digits must be exact.
     (error? (hexdigits->num '(1.0 2)))

     )

(mat parser-text-conformance-review

     ;; Error case: dotted keys cannot extend explicit tables or arrays of tables.
     (for-all
      (lambda (text) (parser-error? (parser-error-from (lambda () (parse-toml text)))))
      '("[a.b]\nx=1\n[a]\nb.y=2" "[[a.b]]\n[a]\nb.y=2"))

     (let ([value (toml-table-ref (toml-document-root
                                  (parse-toml "t=1979-05-27t07:32:00z")) "t")])
       (and (toml-offset-date-time? value)
            (= 0 (toml-offset-date-time-offset-seconds value))))

     (for-all
      (lambda (text) (eqv? -0.0 (json5-document-value (parse-json5 text))))
      '("-0" "-0x0" "-0X00"))

     (for-all
      (lambda (character)
        (let ([value (string #\a character #\b)])
          (and (string=? value (json5-document-value
                               (parse-json5 (string-append "'" value "'"))))
               (string=? value (json5-document-value
                               (parse-json5 (string-append "\"" value "\"")))))))
      '(#\x2028 #\x2029))

     (for-all
      (lambda (source) (string=? "a\nb" (parse-scheme-datum source)))
      '("\"a\rb\"" "\"a\r\nb\"" "\"a\x85;b\"" "\"a\r\x85;b\"" "\"a\x2028;b\""))

     (equal? '(a . #t) (parse-scheme-datum "(a .#t)"))

     (equal? '(a b) (parse-scheme-datum "(a .(b))"))

     (equal? '(a quote b) (parse-scheme-datum "(a .'b)"))

     (equal? '(a . "x") (parse-scheme-datum "(a .\"x\")"))

     ;; Error case: dot must still have a lexical delimiter.
     (parser-error? (parser-error-from (lambda () (parse-scheme-datum "(a .b)"))))

     ;; Error case: XML EncName is ASCII even when the document is already decoded.
     (for-all
      (lambda (encoding)
        (parser-error?
         (parser-error-from
          (lambda () (parse-xml
                      (format "<?xml version='1.0' encoding='~a'?><a/>" encoding))))))
      '("é" "UTF-é"))

     (let* ([text (string-append
                   "<a" (apply string-append
                               (map (lambda (i) (format " x~a='v'" i)) (iota 1000))) "/>")]
            [root (xml-document-root (parse-xml text))])
       (= 1000 (vector-length (xml-element-attributes root))))

     )

(mat parser-file-decoding-review

     ;; Error case: malformed UTF-8 cannot silently become replacement characters.
     (for-all
      (lambda (parser)
        (with-parser-file #vu8(34 255 34)
          (lambda (path) (guard (err [else #t]) (parser path) #f))))
      (list parse-csv-file parse-json5-file parse-scheme-file))

     ;; Error case: WAT strings cannot hide malformed UTF-8 in file input.
     (with-parser-file
      (parser-review-bytes (string->utf8 "(module (import \"") #vu8(255)
                         (string->utf8 "\" \"f\" (func)))"))
      (lambda (path) (guard (err [else #t]) (parse-wasm-text-module-file path) #f)))

     ;; Error case: only the first BOM is a stream signature in XML/TOML files.
     (for-all
      (lambda (case)
        (with-parser-file (parser-review-bytes #vu8(239 187 191 239 187 191)
                                             (string->utf8 (cdr case)))
          (lambda (path)
            (parser-error? (parser-error-from (lambda () ((car case) path)))))))
      (list (cons parse-xml-file "<a/>") (cons parse-toml-file "a=1")))

     (with-parser-file (parser-review-bytes #vu8(239 187 191) (string->utf8 "a=1"))
       (lambda (path)
         (= 1 (toml-table-ref (toml-document-root (parse-toml-file path)) "a"))))

     ;; BOM bytes are name content in WASM, including when there are two in a row.
     (let* ([module (parse-wasm-text-module
                     "(module (func) (export \"\\u{feff}A\" (func 0)))")]
            [export (vector-ref (wasm-module-exports module) 0)])
       (string=? "\xfeff;A" (wasm-export-name export)))

     )

(mat parser-toml-implicit-dotted-review

     ;; Error case: traversing an implicit parent with a dotted key defines that parent.
     (parser-error?
      (parser-error-from
       (lambda () (parse-toml "[a.b.c]\nx=1\n[a]\nb.d=2\n[a.b]\ny=3"))))

     (let* ([root (toml-document-root (parse-toml "[a.b.c]\nx=1\n[a]\nb.d=2"))]
            [a (toml-table-ref root "a")]
            [b (toml-table-ref a "b")])
       (= 2 (toml-table-ref b "d")))

     )

(mat parser-signed-leb128-width

     (for-all
      (lambda (case)
        (= (caddr case) (run-binary-parser (<sleb128> (car case)) (cadr case))))
      '((1 #vu8(0) 0) (1 #vu8(127) -1)
        (7 #vu8(63) 63) (7 #vu8(64) -64)
        (8 #vu8(255 0) 127) (8 #vu8(128 127) -128)
        (8 #vu8(255 127) -1) (8 #vu8(128 0) 0)))

     ;; Error cases: padding cannot extend a signed LEB128 beyond its byte width.
     (for-all
      (lambda (bytes)
        (parser-error?
         (parser-error-from (lambda () (run-binary-parser (<sleb128> 8) bytes)))))
      '(#vu8(255 255 127) #vu8(128 128 0) #vu8(128 128)))

     ;; Error cases: unused terminal bits must all match the signed value's sign bit.
     (for-all
      (lambda (case)
        (parser-error?
         (parser-error-from
          (lambda () (run-binary-parser (<sleb128> (car case)) (cadr case))))))
      '((1 #vu8(1)) (1 #vu8(126)) (7 #vu8(128 0))
        (8 #vu8(128 1)) (8 #vu8(255 126))))

     ;; Error case: a continuation byte cannot end the input before a terminal byte.
     (parser-error?
      (parser-error-from (lambda () (run-binary-parser (<sleb128> 8) #vu8(128))))))

(mat parser-failure-message-output

     (string=? "review: message (0/0)\n"
               (with-output-to-string
                 (lambda ()
                   (parser-error-from
                    (lambda () (run-textual-parser (<msg-f> 'review "message") ""))))))

     ;; Error case: printing a failure message must still fail the parse.
     (let ([failure #f])
       (with-output-to-string
         (lambda ()
           (set! failure
                 (parser-error-from
                  (lambda () (run-textual-parser (<msg-f> 'review "message") ""))))))
       (parser-error? failure)))

(mat parser-choice-diagnostic-cost

     ;; Error case: preserve first occurrence order and punctuation while merging duplicates.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-textual-parser
                    (</> (<string> "a") (<string> "b") (<string> "a") (<string> "c"))
                    "x")))])
       (and (equal? '("a" "b" "c") (parser-error-expected err))
            (string=? "expected \"a\", \"b\", or \"c\", got #\\x"
                      (parser-error-message err))
            (= 4 (length (parser-error-causes err)))))

     ;; Error case: useful custom messages remain ordered and distinct.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-textual-parser
                    (</> (<fail-with> "first") (<fail-with> "second")
                         (<fail-with> "first")) "")))])
       (string=? "<fail-with>: first; <fail-with>: second" (parser-error-message err)))

     ;; Error case: a wide choice should not copy every prior expectation/message prefix.
     (let* ([count 1000]
            [parser (apply </> (map (lambda (i) (<string> (format "keyword~a" i)))
                                   (iota count)))]
            [before (statistics)]
            [err (parser-error-from (lambda () (run-textual-parser parser "x")))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= count (length (parser-error-expected err))) (< allocated 10000000))))

(mat parser-character-list-conversion

     (let* ([characters (make-list 262144 #\a)]
            [parser (<as-string> (<result> characters))]
            [before (statistics)]
            [text (run-textual-parser parser "")]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? text (make-string 262144 #\a)) (< allocated 2000000)))

     (eq? 'abc (run-textual-parser (<as-symbol> (<result> '(#\a #\b #\c))) ""))

     (let* ([parser (<as-string> (<result> '(#\a #\b)))]
            [first (run-textual-parser parser "")]
            [second (run-textual-parser parser "")])
       (string-set! first 0 #\x)
       (string=? "ab" second)))

(mat parser-integer-sign-only

     ;; Error cases: a sign must be followed by at least one digit.
     (for-all
      (lambda (text)
        (let ([err (parser-error-from
                    (lambda ()
                      (run-textual-parser (<as-integer> (<some> (<one-of> "+-0123456789")))
                                          text)))])
          (and (parser-error? err) (= 1 (parser-error-offset err)))))
      '("+" "-"))

     (equal? '(0 -1 12)
             (map (lambda (text)
                    (run-textual-parser (<as-integer> (<some> (<one-of> "+-0123456789"))) text))
                  '("+0" "-1" "12")))

     (not (run-textual-parser (<as-integer> (<result> '())) "")))

(mat parser-nested-choice-diagnostics

     ;; Error case: nested choices retain ordered expectations without copying every suffix.
     (let* ([count 500]
            [alternatives (map (lambda (i) (<string> (format "keyword~a" i))) (iota count))]
            [reversed (reverse alternatives)]
            [parser (fold-right (lambda (parser rest) (</> parser rest))
                                (car reversed) (reverse (cdr reversed)))]
            [before (statistics)]
            [err (parser-error-from (lambda () (run-textual-parser parser "x")))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= count (length (parser-error-expected err)))
            (equal? '("keyword0" "keyword1") (list-head (parser-error-expected err) 2))
            (string=? "keyword499" (list-last (parser-error-expected err)))
            (< allocated 5000000)))

     ;; Error case: a single surviving nested choice keeps its expectations and custom message.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-textual-parser
                    (</> (<char> #\z)
                         (</> (~> (<char> #\a) (<string> "x"))
                              (~> (<char> #\a) (<fail-with> "inside")))) "ab")))])
       (and (= 1 (parser-error-offset err))
            (equal? '("x") (parser-error-expected err))
            (string=? "<fail-with>: inside" (parser-error-message err))
            (= 2 (length (parser-error-causes err))))))

(mat parser-nested-choice-message-order

     ;; Error case: nested custom messages are deduplicated in first-occurrence order.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-textual-parser
                    (</> (<fail-with> "first")
                         (</> (<fail-with> "second") (<fail-with> "first"))
                         (<fail-with> "third")) "")))])
       (and (string=? "<fail-with>: first; <fail-with>: second; <fail-with>: third"
                      (parser-error-message err))
            (= 3 (length (parser-error-causes err))))))

(mat parser-character-predicate-cost

     (let* ([count 100000] [text (make-string count #\7)]
            [parser (~> (<skip> <digit> count) <pos>)]
            [before (statistics)] [position (run-textual-parser parser text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= count position) (< allocated 2000000)))

     ;; Error case: a predicate failure uses the position before the rejected line break.
     (let ([err (parser-error-from
                 (lambda () (run-textual-parser (~> (<string> "a\r") <digit>) "a\r\n")))])
       (and (= 2 (parser-error-offset err)) (= 1 (parser-error-line err))
            (= 0 (parser-error-column err)) (char=? #\newline (parser-error-found err)))))

(mat parser-json5-whitespace-cost

     (let* ([text (string-append (make-string 100000 #\space) "42")]
            [before (statistics)] [document (parse-json5 text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= 42 (json5-document-value document)) (< allocated 30000000))))

(mat parser-exact-count-contracts

     ;; Error cases: constructors reject inexact, fractional, and negative counts/indices.
     (for-all
      (lambda (number)
        (for-all (lambda (construct)
                   (guard (condition [(error? condition) #t])
                     (construct number)
                     #f))
                 (list (lambda (n) (<rep> <item> n))
                       (lambda (n) (<skip> <item> n))
                       (lambda (n) (<~n> n <item>))
                       (lambda (n) (<u8vec> n))
                       (lambda (n) (<bounded> n <u8>))
                       (lambda (n) (<pos-at> n <item>)))))
      '(0.0 1.0 1/2 -1))

     (and (equal? '() (run-textual-parser (<rep> <item> 0) "abc"))
          (equal? '() (run-textual-parser (<skip> <item> 0) "abc"))
          (equal? '(#\a) (run-textual-parser (<rep> <item> 1) "abc"))
          (char=? #\b (run-textual-parser (<~n> 1 <item> <item>) "ab")))

     ;; Error cases: large exact counts fail through the parser at EOF, not fixnum contracts.
     (for-all
      (lambda (construct)
        (let ([err (parser-error-from
                    (lambda ()
                      (run-textual-parser (construct <item> (+ 1 (most-positive-fixnum)))
                                          "abc")))])
          (and (parser-error? err) (= 3 (parser-error-offset err)))))
      (list <rep> <skip>))

     ;; Error cases: large exact byte ranges fail before allocating or installing a cursor.
     (for-all
      (lambda (parser)
        (parser-error? (parser-error-from (lambda () (run-binary-parser parser #vu8(1))))))
      (let ([large (+ 1 (most-positive-fixnum))])
        (list (<u8vec> large) (<bounded> large <u8>) (<pos-at> large <u8>)))))
