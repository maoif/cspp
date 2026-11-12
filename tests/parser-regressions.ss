(import (cspp) (cspp parser combinator)
        (prefix (cspp parser elf validate) elf:)
        (prefix (cspp parser wasm validate) wasm:)
        (only (cspp parser wasm opcodes) wasm-opcode-by-binary wasm-opcode-mnemonic))

(include "parser-test-support.ss")

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
          (lambda (path) (raises-decoding-error? (lambda () (parser path))))))
      (list parse-csv-file parse-json5-file parse-scheme-file))

     ;; Error case: WAT strings cannot hide malformed UTF-8 in file input.
     (with-parser-file
      (parser-test-bytes (string->utf8 "(module (import \"") #vu8(255)
                         (string->utf8 "\" \"f\" (func)))"))
      (lambda (path)
        (raises-decoding-error? (lambda () (parse-wasm-text-module-file path)))))

     ;; Error case: only the first BOM is a stream signature in XML/TOML files.
     (for-all
      (lambda (case)
        (with-parser-file (parser-test-bytes #vu8(239 187 191 239 187 191)
                                             (string->utf8 (cdr case)))
          (lambda (path)
            (parser-error? (parser-error-from (lambda () ((car case) path)))))))
      (list (cons parse-xml-file "<a/>") (cons parse-toml-file "a=1")))

     (with-parser-file (parser-test-bytes #vu8(239 187 191) (string->utf8 "a=1"))
       (lambda (path)
         (= 1 (toml-table-ref (toml-document-root (parse-toml-file path)) "a"))))

     ;; BOM bytes are name content in WASM, including when there are two in a row.
     (let* ([module (parse-wasm-text-module
                     "(module (func) (export \"\\u{feff}A\" (func 0)))")]
            [export (vector-ref (wasm-module-exports module) 0)])
       (string=? "\xfeff;A" (wasm-export-name export)))

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

     ;; Error cases: diagnostics must describe the same live set used by the predicate.
     (let* ([characters (string #\a)]
            [yes (<one-of> characters)] [no (<none-of> characters)])
       (string-set! characters 0 #\b)
       (for-all
        (lambda (parser text message)
          (let ([err (parser-error-from
                      (lambda () (run-textual-parser (<~> <item> parser) text)))])
            (and (parser-error? err)
                 (eq? 'predicate (parser-error-kind err))
                 (string=? message (parser-error-message err))
                 (= 1 (parser-error-offset err))
                 (= 1 (parser-error-column err))
                 (char=? (string-ref text 1) (parser-error-found err)))))
        (list yes no) '("xa" "xb")
        '("not one of \"b\"" "should not be one of \"b\"")))

     ;; Error case: an empty input keeps the primitive EOF failure for either live set.
     (for-all
      (lambda (parser)
        (let ([err (parser-error-from (lambda () (run-textual-parser parser "")))])
          (and (parser-error? err) (eq? 'unexpected-eof (parser-error-kind err))
               (= 0 (parser-error-offset err)))))
      (list (<one-of> "a") (<none-of> "a")))

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

(mat parser-lookup-exact-codes

     (for-all
      (lambda (code name) (eq? name (elf-section-type-name code)))
      '(9 10 11) '(relocation shared-library dynamic-symbol-table))

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

(mat parser-shared-decimal-prefixes

     (let* ([count 32000] [source (make-string count #\1)]
            [expected (div (- (expt 10 count) 1) 9)]
            [before (statistics)] [value (json5-document-value (parse-json5 source))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= expected value) (< allocated 12500000)))

     (equal? '(100.0 0.5 0.5 -0.0)
             (map (lambda (text) (json5-document-value (parse-json5 text)))
                  '("1.e2" ".5" "0.5" "-0")))

     (eqv? 1.25e-20 (toml-table-ref (toml-document-root (parse-toml "v=1.25e-20")) "v"))

     ;; Error cases: sharing must not accept incomplete suffixes or leading-zero integers.
     (for-all (lambda (text) (and (parser-error-from (lambda () (parse-json5 text))) #t))
              '("00" "1e+" "1..0" "-."))

     ;; Error cases: TOML requires fractional digits and complete decimal exponents.
     (for-all (lambda (text) (and (parser-error-from (lambda () (parse-toml text))) #t))
              '("v=1." "v=1.e2" "v=1e+" "v=01")))

(mat parser-format-discarded-trivia

     (let ([spaces (make-string 100000 #\space)])
       (for-all
        (lambda (parse text)
          (let* ([before (statistics)] [value (parse text)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (= 0 value) (< allocated 14500000))))
        (list (lambda (text) (vector-length
                             (toml-table-entries (toml-document-root (parse-toml text)))))
              (lambda (text) (vector-length
                             (xml-element-attributes (xml-document-root (parse-xml text))))))
        (list spaces (string-append "<r" spaces "/>"))))

     (let ([body (make-string 100000 #\a)])
       (for-all
        (lambda (parse prefix suffix)
          (let* ([text (string-append prefix body suffix)]
                 [before (statistics)] [value (parse text)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (= 0 value) (< allocated 7500000))))
        (list (lambda (text) (json5-document-value (parse-json5 text)))
              (lambda (text) (toml-table-ref (toml-document-root (parse-toml text)) "v"))
              (lambda (text) (vector-length
                             (wasm-module-functions (parse-wasm-text-module text)))))
        '("//" "#" ";;") '("\n0" "\nv=0" "\n(module)")))

     ;; Error cases: discard repetition must leave the first invalid/control character unread.
     (for-all
      (lambda (parse text)
        (let ([err (parser-error-from (lambda () (parse text)))])
          (and err (string=? "<string>" (parser-error-source err)))))
      (list parse-json5 parse-toml parse-xml parse-wasm-text-module)
      '("/*abc" "#abc\x0;" "<r \x0;/>" "(@foo abc")))

(mat parser-balanced-decimal-floats

     ;; Compensated long significands must not build a growing bignum once per digit.
     (let ([source (string-append (make-string 4000 #\1) "e-4000")])
       (for-all
        (lambda (parse text)
          (let* ([before (statistics)] [value (parse text)]
                 [allocated (sstats-bytes (sstats-difference (statistics) before))])
            (and (eqv? 0.1111111111111111 value) (< allocated 4000000))))
        (list (lambda (text) (json5-document-value (parse-json5 text)))
              (lambda (text) (toml-table-ref (toml-document-root (parse-toml text)) "v")))
        (list source (string-append "v=" source))))

     ;; Exact decimal halfway cases, normal/subnormal boundaries, overflow and signed zero.
     (for-all
      (lambda (source expected)
        (and (eqv? expected (json5-document-value (parse-json5 source)))
             (eqv? expected (toml-table-ref
                             (toml-document-root (parse-toml (string-append "v=" source)))
                             "v"))))
      '("-0.0" "-1e-9999" "1.7976931348623159e308" "2.4703282292062327e-324"
        "2.4703282292062328e-324" "2.2250738585072014e-308" "9007199254740993.0"
        "1.00000000000000011102230246251565404236316680908203125"
        "1.00000000000000011102230246251565404236316680908203126")
      (list -0.0 -0.0 +inf.0 0.0 5e-324 2.2250738585072014e-308 9007199254740992.0
            1.0 1.0000000000000002))

     ;; The total decimal order decides overflow/underflow, including opposite exponent signs.
     (for-all
      (lambda (source expected)
        (and (eqv? expected (json5-document-value (parse-json5 source)))
             (eqv? expected (toml-table-ref
                             (toml-document-root (parse-toml (string-append "v=" source)))
                             "v"))))
      (let* ([large (string-append (make-string 4000 #\1) "e-500")]
             [small (string-append "0." (make-string 4000 #\0) "1e500")])
        (list large (string-append "-" large) small (string-append "-" small)))
      (list +inf.0 -inf.0 0.0 -0.0))

     ;; Zero and exponent overflow retain their signs without an unbounded exponent integer.
     (for-all
      (lambda (prefix expected)
        (let* ([source (string-append prefix (make-string 4000 #\9))]
               [before (statistics)] [value (json5-document-value (parse-json5 source))]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (eqv? expected value) (< allocated 4000000))))
      '("0e" "-0e" "1e" "-1e-" "1e-") (list 0.0 -0.0 +inf.0 -0.0 0.0))

     (eqv? 0.5 (json5-document-value (parse-json5 ".5")))

     (eqv? 12.5 (toml-table-ref (toml-document-root (parse-toml "v=1_2.5_0e+0")) "v"))

     ;; Error cases: the converter cannot make malformed float spellings valid.
     (for-all (lambda (source) (and (parser-error-from (lambda () (parse-json5 source))) #t))
              '("1e" "1e+-2" "1.0e!" ".e0"))

     ;; Error cases: TOML's normalized source still comes from its strict number grammar.
     (for-all (lambda (source) (and (parser-error-from (lambda () (parse-toml source))) #t))
              '("v=1_.0" "v=1.0e_2" "v=1e+" "v=.5")))

(mat parser-counted-vectors

     ;; Compare the preallocated and short-input paths against ordinary counted repetition.
     (let ()
       (define-parser legacy-failure
         (let-values ([(status value next) (parser-call <u8> inp state lvl)])
           (values #f #f "legacy element failure")))
       (define outcome
         (lambda (parser bytes)
           (guard (condition
                   [(parser-error? condition)
                    (list (parser-error-kind condition) (parser-error-offset condition)
                          (parser-error-message condition) (parser-error-found condition)
                          (parser-error->string condition))])
             (run-binary-parser (<~> parser <pos>) bytes))))
       (for-all
        (lambda (count)
          (for-all
           (lambda (bytes)
             (for-all
              (lambda (element)
                (equal? (outcome (parser-counted-vector element count) bytes)
                        (outcome (<map> list->vector (<rep> element count)) bytes)))
              (list <u8> <u16le> (<result> 'empty) (<uimm8> 1) legacy-failure)))
           '(#vu8() #vu8(0) #vu8(1) #vu8(1 2) #vu8(1 2 3) #vu8(1 2 3 4))))
        '(0 1 2 3 4 7)))

     ;; Error cases: counts are exact naturals and elements must be parsers.
     (for-all
      (lambda (arguments)
        (guard (condition [(and (who-condition? condition)
                           (eq? 'pcheck (condition-who condition))) #t])
          (apply parser-counted-vector arguments) #f))
      (list (list <u8> -1) (list <u8> 1.0) (list <u8> 1/2) (list #f 1))))
