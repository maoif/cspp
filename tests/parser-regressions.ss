(import (cspp) (cspp parser combinator)
        (prefix (cspp parser elf validate) elf:)
        (prefix (cspp parser wasm validate) wasm:)
        (only (cspp parser wasm opcodes) wasm-opcode-by-binary wasm-opcode-mnemonic)
        (only (cspp parser wasm binary instructions) <wasm-expression>))

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

(mat parser-xml-comment-copy

     (let* ([body (string-append (make-string 250000 #\a) "-b")]
            [text (string-append "<r><!--" body "--></r>")]
            [before (statistics)] [document (parse-xml text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body (xml-comment-value
                           (vector-ref (xml-element-children
                                        (xml-document-root document)) 0)))
            (< allocated 68000000)))

     ;; Error case: a double hyphen inside comment text is still forbidden.
     (and (parser-error-from (lambda () (parse-xml "<r><!--a--b--></r>"))) #t)

     ;; Error case: a trailing hyphen is not part of a valid comment terminator.
     (and (parser-error-from (lambda () (parse-xml "<r><!--a---></r>"))) #t))

(mat parser-xml-newline-copy

     ;; Newline normalization must keep literal and referenced whitespace distinct.
     (for-all
      (lambda (newline)
        (let* ([document (parse-xml
                          (string-append "<r a='x" newline "y&#13;'><!--a" newline
                                         "b--><![CDATA[c" newline "d]]>e" newline "f</r>"))]
               [root (xml-document-root document)] [children (xml-element-children root)])
          (and (string=? "x y\r" (xml-attribute-value
                                  (vector-ref (xml-element-attributes root) 0)))
               (string=? "a\nb" (xml-comment-value (vector-ref children 0)))
               (string=? "c\nd" (xml-cdata-value (vector-ref children 1)))
               (string=? "e\nf" (xml-text-value (vector-ref children 2))))))
      '("\n" "\r" "\r\n")))

(mat parser-xml-bounded-references

     ;; Error cases: rejecting oversized references must not construct oversized integers.
     (for-all
      (lambda (prefix digit)
        (let* ([text (string-append "<r>" prefix (make-string 16000 digit) ";</r>")]
               [before (statistics)]
               [failure (parser-error-from (lambda () (parse-xml text)))]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and failure (< allocated 10000000))))
      '("&#" "&#x") '(#\9 #\f))

     (let* ([zeros (make-string 16000 #\0)]
            [document (parse-xml (string-append "<r a='&#x" zeros "10ffff;'>&#"
                                               zeros "65;</r>"))]
            [root (xml-document-root document)])
       (and (string=? (string #\x10ffff)
                      (xml-attribute-value (vector-ref (xml-element-attributes root) 0)))
            (string=? "A" (xml-text-value (vector-ref (xml-element-children root) 0))))))

(mat parser-balanced-exact-digits

     (for-all
      (lambda (radix convert)
        (for-all (lambda (count) (= (- (expt radix count) 1)
                                    (convert (make-list count (- radix 1)))))
                 '(0 1 14 15 29 1000)))
      '(2 8 10 16) (list bindigits->num octdigits->num digits->num hexdigits->num))

     ;; Long significant integers must not allocate a growing bignum for every digit.
     (let* ([source (string-append "0x" (make-string 16000 #\f))]
            [before (statistics)] [document (parse-json5 source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= (- (expt 16 16000) 1) (json5-document-value document))
            (< allocated 10000000)))

     ;; Error case: validation reports the first invalid digit across a split boundary.
     (let ([digits (make-list 100 1)])
       (set-car! (list-tail digits 15) 'first-invalid)
       (set-car! (list-tail digits 80) 'second-invalid)
       (guard (err [(message-condition? err)
                    (string=? "bad digit first-invalid for base 16"
                              (apply format (condition-message err) (condition-irritants err)))])
         (hexdigits->num digits) #f))

     ;; Error case: a zero leading half must not hide an invalid digit in the right half.
     (let ([digits (make-list 100 0)])
       (set-car! (list-tail digits 80) 16)
       (guard (err [(message-condition? err)
                    (string=? "bad digit 16 for base 16"
                              (apply format (condition-message err) (condition-irritants err)))])
         (hexdigits->num digits) #f))

     (= 0 (hexdigits->num (make-list 32000 0))))

(define parser-raw-failure-wrappers
  (list values
        (lambda (parser) (</> parser))
        (lambda (parser) (</> parser <fail>))
        (lambda (parser) (<many-until> parser <eof>))
        (lambda (parser) (<satisfy> parser (lambda (value) #t)))
        (lambda (parser) (<followed-by> parser (<result> #t)))
        (lambda (parser) (<not-followed-by> parser (<result> #t)))
        (lambda (parser) (<bind> (<result> #t) (lambda (value) parser)))
        (lambda (parser) (<bind-st> (<result> #t) (lambda (value state) parser)))))

(mat parser-raw-failure-source

     ;; Error cases: legacy string/#f/cursor failures retain the available source and cursor.
     (for-all
      (lambda (prefix runner input line column)
        (for-all
         (lambda (kind)
           (let ()
             (define-parser raw-failure
               (let-values ([(status value next) (parser-call prefix inp state lvl)])
                 (if status
                     (values #f #f (case kind [(string) "domain failure"]
                                               [(false) #f] [else next]))
                     (values #f #f next))))
             (for-all
              (lambda (wrap)
                (let ([err (parser-error-from
                            (lambda () (runner (wrap raw-failure) input "custom.input")))])
                  (and err (string=? "custom.input" (parser-error-source err))
                       (= 3 (parser-error-offset err))
                       (eqv? line (parser-error-line err))
                       (eqv? column (parser-error-column err))
                       (or (not (eq? kind 'string))
                           (string=? "domain failure" (parser-error-message err))))))
              parser-raw-failure-wrappers)))
         '(string false cursor)))
      (list (<string> "\r\nx") (<u8vec> 3))
      (list run-textual-parser/source run-binary-parser/source)
      (list "\r\nx" #vu8(1 2 3)) '(1 #f) '(1 #f))

     ;; Error case: normalize a bounded raw failure before discarding its private cursor.
     (let ()
       (define-parser raw-failure
         (let-values ([(status value next) (parser-call (<u8vec> 3) inp state lvl)])
           (if status (values #f #f "domain failure") (values #f #f next))))
       (let ([err (parser-error-from
                   (lambda () (run-binary-parser/source
                               (~> <u8> (<bounded> 3 raw-failure))
                               #vu8(0 1 2 3) "bounded.input")))])
         (and err (= 4 (parser-error-offset err))
              (string=? "bounded.input" (parser-error-source err))
              (string=? "domain failure" (parser-error-message err))))))

(mat parser-xml-ordinary-characters

     (for-all
      (lambda (prefix suffix)
        (let* ([text (string-append prefix (make-string 100000 #\a) suffix)]
               [before (statistics)] [document (parse-xml text)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (xml-document? document) (< allocated 22000000))))
      '("<r>" "<r><![CDATA[" "<?p ") '("</r>" "]]></r>" "?><r/>"))

     ;; Error case: ]]> remains forbidden in ordinary character data.
     (and (parser-error-from (lambda () (parse-xml "<r>a]]>b</r>"))) #t)

     ;; Error case: forbidden XML scalars cannot pass the ordinary-character shortcut.
     (and (parser-error-from (lambda () (parse-xml "<r><![CDATA[a\x0;b]]></r>"))) #t)

     (let* ([root (xml-document-root (parse-xml "<r>a]b<![CDATA[c]d]]><?p e?f?></r>"))]
            [children (xml-element-children root)])
       (and (string=? "a]b" (xml-text-value (vector-ref children 0)))
            (string=? "c]d" (xml-cdata-value (vector-ref children 1)))
            (string=? "e?f" (xml-processing-instruction-data (vector-ref children 2))))))

(mat parser-remaining-exact-integers

     ;; Exact conversion must avoid repeatedly copying a growing integer in every format.
     (let* ([count 16000] [digits (make-string count #\9)]
            [expected (- (expt 10 count) 1)]
            [parser (<as-integer> (<some> <digit>))])
       (for-all
        (lambda (parse budget)
          (let* ([before (statistics)] [actual (parse digits)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (= expected actual) (< allocated budget))))
        (list (lambda (text) (run-textual-parser parser text))
              parse-scheme-datum
              (lambda (text) (json5-document-value (parse-json5 text)))
              (lambda (text)
                (toml-table-ref (toml-document-root (parse-toml (string-append "n=" text)))
                                "n")))
        '(16000000 20000000 20000000 40000000)))

     (equal? '(-255 63 5 0)
             (map parse-scheme-datum '("#x-ff" "#o+77" "#b101" "#D-0")))

     (let ([root (toml-document-root (parse-toml "a=-1_234\nb=0xff\nc=0o77\nd=0b101"))])
       (equal? '(-1234 255 63 5)
               (map (lambda (key) (toml-table-ref root key)) '("a" "b" "c" "d"))))

     (and (eqv? -0.0 (json5-document-value (parse-json5 "-0")))
          (eqv? -0.0 (json5-document-value (parse-json5 "-0.0")))
          (eqv? -0.0 (json5-document-value (parse-json5 "-0e0")))
          (eqv? 0.1 (json5-document-value (parse-json5 "1e-1"))))

     ;; Error cases: sign-only and malformed numeric suffixes remain parser failures.
     (for-all (lambda (source) (and (parser-error-from (lambda () (parse-json5 source))) #t))
              '("+" "-" "12x" "1e+"))

     ;; Error cases: multiple invalid characters must report the first in source order.
     (for-all
      (lambda (characters)
        (guard (condition
                [(message-condition? condition)
                 (string=? "not a digit: a"
                           (apply format (condition-message condition)
                                  (condition-irritants condition)))])
          (run-textual-parser (<as-integer> (<result> characters)) "") #f))
      (list (string->list "1a2b")
            (let ([characters (make-list 100 #\1)])
              (set-car! (list-tail characters 15) #\a)
              (set-car! (list-tail characters 80) #\b)
              characters))))

(mat parser-validation-exact-arguments

     ;; Error cases: validation helpers reject inexact IDs, alignments, counts and positions.
     (for-all
      (lambda (value)
        (for-all
         (lambda (call)
           (guard (condition
                   [(and (who-condition? condition) (eq? 'pcheck (condition-who condition))) #t])
             (call value) #f))
         (list wasm:wasm-standard-section? wasm:wasm-section-rank
               (lambda (value) (wasm:wasm-section-order-valid? 0 value))
               (lambda (value) (wasm:wasm-section-order-valid? value 1))
               (lambda (value) (wasm:wasm-data-count-valid? value '#()))
               (lambda (value) (wasm:make-wasm-issue value "position"))
               elf:elf-power-of-two?
               (lambda (value) (elf:elf-range-valid? value 0 1))
               (lambda (value) (elf:elf-range-valid? 0 value 1))
               (lambda (value) (elf:elf-range-valid? 0 0 value))
               (lambda (value) (elf:make-elf-issue value "position")))))
      '(0.0 1.0 1/2 -1))

     (and (wasm:wasm-standard-section? 13) (= 6 (wasm:wasm-section-rank 13))
          (wasm:wasm-section-order-valid? 5 13)
          (not (wasm:wasm-section-order-valid? 6 13))
          (wasm:wasm-data-count-valid? #f '#())
          (wasm:wasm-data-count-valid? 0 '#())
          (elf:elf-power-of-two? 0) (elf:elf-power-of-two? (expt 2 100))
          (not (elf:elf-power-of-two? (+ 1 (expt 2 100))))
          (elf:elf-range-valid? 1 2 3)))

(mat parser-live-character-membership

     ;; Retain the caller's live set string, including changes after parser construction.
     (let* ([characters (string #\a #\x1f600)]
            [yes (<one-of> characters)] [no (<none-of> characters)])
       (and (char=? #\x1f600 (run-textual-parser yes (string #\x1f600)))
            (begin (string-set! characters 0 #\z)
                   (and (char=? #\z (run-textual-parser yes "z"))
                        (char=? #\a (run-textual-parser no "a"))))))

     (for-all
      (lambda (parser character)
        (let* ([source (make-string 100000 character)] [before (statistics)]
               [result (run-textual-parser parser source)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (= 100000 (length result)) (< allocated 11000000))))
      (list (<some> (<one-of> " \t")) (<some> (<none-of> "\"\\")))
      '(#\space #\a)))

(mat parser-scheme-ordinary-string-cost

     (let* ([body (make-string 100000 #\a)] [source (string-append "\"" body "\"")]
            [before (statistics)] [value (parse-scheme-datum source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body value) (< allocated 30000000)))

     (for-all
      (lambda (newline)
        (string=? "a\nb" (parse-scheme-datum (string-append "\"a" newline "b\""))))
      '("\r" "\n" "\r\n" "\r\x85;" "\x85;" "\x2028;"))

     (string=? (string #\return #\newline #\x2029)
               (parse-scheme-datum "\"\\r\\n\x2029;\""))

     ;; Error cases: ordinary characters must not hide invalid escapes or a missing quote.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-scheme-datum source))) #t))
              '("\"abc\\q\"" "\"abc")))

(mat parser-wasm-sequence-lookahead-cost

     (let ([bytes (make-bytevector 100001 1)])
       (bytevector-u8-set! bytes 100000 11)
       (let* ([before (statistics)] [body (run-binary-parser <wasm-expression> bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= 100000 (vector-length body))
              (eq? 'nop (wasm-instruction-mnemonic (vector-ref body 99999)))
              (< allocated 28000000)))))

(mat parser-lookup-exact-codes

     ;; Error cases: admitted numeric codes must be exact, including unknown codes and zero.
     (for-all
      (lambda (lookup)
        (for-all
         (lambda (value)
           (guard (condition
                   [(and (who-condition? condition) (eq? 'pcheck (condition-who condition))) #t])
             (lookup value) #f))
         '(0.0 1.0 62.0 999.0 1/2)))
      (list elf-section-type-name elf-program-type-name elf-machine-name elf-dynamic-tag-name
            (lambda (value) (wasm-opcode-by-binary #f value))
            (lambda (value) (wasm-opcode-by-binary 253 value))))

     (and (eq? 'progbits (elf-section-type-name 1))
          (eq? 'load (elf-program-type-name 1))
          (eq? 'x86-64 (elf-machine-name 62))
          (eq? 'needed (elf-dynamic-tag-name 1))
          (eq? 'unknown (elf-dynamic-tag-name -1))
          (eq? 'unknown (elf-section-type-name (expt 2 100)))
          (not (wasm-opcode-by-binary #f 255))
          (eq? 'nop (wasm-opcode-mnemonic (wasm-opcode-by-binary #f 1)))
          (eq? 'v128.load (wasm-opcode-mnemonic (wasm-opcode-by-binary 253 0)))))

(mat parser-long-generic-leb128

     ;; Long exact values and permitted sign padding must avoid quadratic temporary integers.
     (let* ([count 4000] [bytes (make-bytevector count 255)]
            [unsigned-expected (- (ash 1 (* count 7)) 1)]
            [signed-parser (<sleb128> (* count 7))])
       (bytevector-u8-set! bytes (- count 1) 127)
       (for-all
        (lambda (parser expected)
          (let* ([before (statistics)] [value (run-binary-parser parser bytes)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (= expected value) (< allocated 2000000))))
        (list <uleb128> signed-parser) (list unsigned-expected -1)))

     ;; Cross the small-integer path boundary and include bytes whose signed bit is set.
     (for-all
      (lambda (count)
        (let ([bytes (make-bytevector count 255)])
          (bytevector-u8-set! bytes (- count 1) 127)
          (and (= (- (ash 1 (* count 7)) 1) (run-binary-parser <uleb128> bytes))
               (= -1 (run-binary-parser (<sleb128> (* count 7)) bytes)))))
      '(1 7 8 9 15 16 17 128))

     ;; Error cases: long unterminated inputs keep source and exact failure byte position.
     (let ([bytes (make-bytevector 33 255)])
       (for-all
        (lambda (parser)
          (let ([err (parser-error-from
                      (lambda () (run-binary-parser/source parser bytes "long-leb")))])
            (and err (= 33 (parser-error-offset err))
                 (string=? "long-leb" (parser-error-source err)))))
        (list <uleb128> (<sleb128> 1000))))

     ;; Error cases: the ninth byte must obey the declared signed width's unused-bit rule.
     (let ([bytes (make-bytevector 9 128)])
       (bytevector-u8-set! bytes 8 1)
       (and (= (ash 1 56) (run-binary-parser (<sleb128> 58) bytes))
            (parser-error? (parser-error-from
                            (lambda () (run-binary-parser (<sleb128> 57) bytes)))))))

(mat parser-discarded-token-whitespace

     (let* ([source (make-string 100000 #\space)] [parser (<fully> (<result> #t))]
            [before (statistics)] [result (run-textual-parser parser source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and result (< allocated 7500000)))

     ;; Ignored whitespace must preserve callback count, caller state and result identity.
     (let* ([state (vector 0)] [result (vector 'owned)]
            [parser (<fully>
                     (<token> (<map-st> (lambda (value state)
                                          (vector-set! state 0 (+ 1 (vector-ref state 0)))
                                          result)
                                        (<string> "abc"))))])
       (and (eq? result (run-textual-parser parser " \r\nabc\t\x2028; " state))
            (= 1 (vector-ref state 0))))

     ;; Error case: ignored whitespace must not consume the next non-whitespace character.
     (let ([err (parser-error-from
                 (lambda () (run-textual-parser (<fully> (<string> "abc"))
                                                 "\r\nabc \r\n!")))])
       (and err (= 8 (parser-error-offset err))
            (= 2 (parser-error-line err)) (= 0 (parser-error-column err))
            (char=? #\! (parser-error-found err)))))

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

(mat parser-json5-ordinary-strings-and-identifiers

     (let* ([body (make-string 100000 #\a)] [source (string-append "\"" body "\"")]
            [before (statistics)] [value (json5-document-value (parse-json5 source))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body value) (< allocated 28000000)))

     (let* ([name (make-string 100000 #\a)] [source (string-append "{" name ":0}")]
            [before (statistics)] [document (parse-json5 source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))]
            [member (vector-ref (json5-object-members (json5-document-value document)) 0)])
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
                (and (parser-error-from (lambda () (parse-json5 source))) #t))
              '("\"abc\\xQ0\"" "\"abc\\uD800\"" "{\\u0030x:1}" "\"abc")))

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
                (and (parser-error-from (lambda () (parse-toml source))) #t))
              '("v=\"abc\\q\"" "v=\"\"\"abc\\q\"\"\"" "v='''abc\x7f;'''" "v=\"\"\"abc")))

(mat parser-xml-ordinary-attribute-characters

     (let* ([body (make-string 100000 #\a)] [source (string-append "<r a='" body "'/>")]
            [before (statistics)] [document (parse-xml source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body (xml-attribute-value
                           (vector-ref (xml-element-attributes (xml-document-root document)) 0)))
            (< allocated 25000000)))

     ;; Literal whitespace normalizes to spaces; a referenced carriage return stays content.
     (let* ([document (parse-xml "<r a='x\t\r\ny&#xD;&amp;&quot;'/>")]
            [attribute (vector-ref (xml-element-attributes (xml-document-root document)) 0)])
       (string=? "x  y\r&\"" (xml-attribute-value attribute)))

     ;; Error cases: direct success cannot bypass forbidden characters or reference checking.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-xml source))) #t))
              '("<r a='a<b'/>" "<r a='a&no;'/>" "<r a='x&#0;'/>" "<r a='x\x0;'/>")))

(mat parser-ordinary-block-comment-characters

     (let ([body (make-string 100000 #\a)])
       (for-all
        (lambda (parse prefix suffix)
          (let* ([source (string-append prefix body suffix)]
                 [before (statistics)] [value (parse source)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (if (wasm-module? value)
                     (= 0 (vector-length (wasm-module-functions value)))
                     (= 0 (json5-document-value value)))
                 (< allocated 25000000))))
        (list parse-wasm-text-module parse-json5) '("(;" "/*") '(";)(module)" "*/0")))

     (= 1 (vector-length
           (wasm-module-functions
            (parse-wasm-text-module "(;a(;b;)c;)(module (func nop))"))))

     (= 0 (json5-document-value (parse-json5 "/***/0")))

     ;; Error cases: nested or partial WAT comment delimiters must retain the original grammar.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-wasm-text-module source))) #t))
              '("(;a(;b;)" "(;abc;" "(;abc("))

     ;; Error cases: JSON5 comments cannot end at an isolated '*' or missing terminator.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-json5 source))) #t))
              '("/*abc" "/*abc*" "/*abc/0")))
