(import (cspp)
        (cspp parser combinator)
        (cspp parser wasm binary instructions)
        (cspp parser wasm binary values)
        (cspp parser wasm binary types)
        (cspp parser wasm text lexical)
        (cspp parser wasm text instructions)
        (only (cspp parser wasm text types)
              wat-index-reference-value wat-instruction-syntax-mnemonic
              wat-instruction-syntax-immediates))
(load "benchmarks/common.ss")

;; Run from the repository root: ./cspp --script=benchmarks/parser.ss
;; Construct inputs and parsers before measuring. Check the results after measuring.
(define measure-parser
  (lambda (label size thunk validate)
    (collect)
    (let* ([before (statistics)] [started (real-time)]
           [result (thunk)] [elapsed (- (real-time) started)]
           [allocated (sstats-bytes (sstats-difference (statistics) before))])
      (unless (validate result) (error 'parser-benchmark "incorrect parse result" label))
      (map (lambda (value) (format "~a" value)) (list label size allocated elapsed)))))

(define measure-xml-attributes
  (lambda (count)
    (let ([text (string-append
                 "<a" (apply string-append
                            (map (lambda (index) (format " x~a='v'" index)) (iota count)))
                 "/>")])
      (measure-parser
       "XML attributes" count (lambda () (parse-xml text))
       (lambda (document)
         (= count (vector-length (xml-element-attributes (xml-document-root document)))))))))

(define measure-zero-float
  (lambda (exponent)
    (let ([text (format "(module (func (result f32) f32.const 0e~a))" exponent)])
      (measure-parser
       "WAT zero exponent" exponent (lambda () (parse-wasm-text-module text))
       (lambda (module)
         (let* ([function (vector-ref (wasm-module-functions module) 0)]
                [instruction (vector-ref (wasm-function-body function) 0)]
                [value (vector-ref (wasm-instruction-immediates instruction) 0)])
           (= 0 (wasm-float-bits value))))))))

;; Retain the original composition here to compare the allocation cost in one process.
(define byte-vector-via-list
  (<bind> <wasm-u32>
          (lambda (count) (<map> u8-list->bytevector (<rep> <u8> count)))))

(define measure-byte-vectors
  (lambda ()
    (let* ([count 1048576] [prefix #vu8(128 128 64)]
           [input (make-bytevector (+ count 3) 42)])
      (bytevector-copy! prefix 0 input 0 3)
      (map (lambda (case)
             (measure-parser
              (car case) count (lambda () (run-binary-parser (cdr case) input))
              (lambda (bytes)
                (and (= count (bytevector-length bytes))
                     (= 42 (bytevector-u8-ref bytes (fx1- count)))))))
           (list (cons "WASM bytes via list" byte-vector-via-list)
                 (cons "WASM bytes direct" <wasm-byte-vector>))))))

(define measure-choice-failure
  (lambda (count)
    (let ([parser (apply </> (map (lambda (index) (<string> (format "keyword~a" index)))
                                 (iota count)))])
      (measure-parser
       "Choice failure" count
       (lambda ()
         (guard (condition [(parser-error? condition) condition])
           (run-textual-parser parser "x")))
       (lambda (condition)
         (and (parser-error? condition)
              (= count (length (parser-error-expected condition)))))))))

(define measure-nested-choice-failure
  (lambda (count)
    (let* ([alternatives (map (lambda (index) (<string> (format "keyword~a" index)))
                              (iota count))]
           [reversed (reverse alternatives)]
           [parser (fold-right (lambda (parser rest) (</> parser rest))
                               (car reversed) (reverse (cdr reversed)))])
      (measure-parser
       "Nested choice failure" count
       (lambda ()
         (guard (condition [(parser-error? condition) condition])
           (run-textual-parser parser "x")))
       (lambda (condition)
         (and (parser-error? condition)
              (= count (length (parser-error-expected condition)))))))))

(define measure-bounded-integer-overflow
  (lambda (count)
    (let ([source (make-string count #\9)])
      (measure-parser
       "WAT oversized u32" count
       (lambda ()
         (guard (condition [(parser-error? condition) condition])
           (run-textual-parser <wat-u32> source)))
       (lambda (condition)
         (and (parser-error? condition) (= count (parser-error-offset condition))))))))

(define measure-signed-integers
  (lambda (count)
    (let ([bytes (make-bytevector (* count 5) 255)] [parser (<rep> <wasm-s32> count)])
      (do ([index 4 (fx+ index 5)]) ((fx>= index (bytevector-length bytes)))
        (bytevector-u8-set! bytes index 127))
      (measure-parser
       "WASM signed integers" count (lambda () (run-binary-parser parser bytes))
       (lambda (values)
         (and (= count (length values)) (for-all (lambda (value) (= value -1)) values)))))))

(define measure-elf-completion
  (lambda (size)
    ;; A table-free ELF isolates cursor completion from section copying/interpretation.
    (let ([bytes (make-bytevector size 0)])
      (bytevector-copy! #vu8(127 69 76 70 2 1 1) 0 bytes 0 7)
      (bytevector-u32-set! bytes 20 1 (endianness little))
      (bytevector-u16-set! bytes 52 64 (endianness little))
      (measure-parser
       "ELF file bytes" size (lambda () (parse-elf bytes))
       (lambda (file)
         (and (elf-file? file) (= 0 (vector-length (elf-file-sections file)))))))))

(define measure-wat-string
  (lambda (count)
    (let ([text (string-append "(module (data \"" (make-string count #\a) "\"))")])
      (measure-parser
       "WAT string characters" count (lambda () (parse-wasm-text-module text))
       (lambda (module)
         (bytevector=? (make-bytevector count 97)
                       (wasm-data-bytes (vector-ref (wasm-module-data module) 0))))))))

(define measure-character-conversion
  (lambda (count)
    (let ([parser (<as-string> (<result> (make-list count #\a)))])
      (measure-parser
       "Character list to string" count (lambda () (run-textual-parser parser ""))
       (lambda (text) (string=? (make-string count #\a) text))))))

(define measure-character-predicates
  (lambda (count)
    (let ([text (make-string count #\7)] [parser (~> (<skip> <digit> count) <pos>)])
      (measure-parser
       "Character predicates" count (lambda () (run-textual-parser parser text))
       (lambda (position) (= count position))))))

(define measure-unsigned-integers
  (lambda (count)
    (let ([bytes (make-bytevector (* count 5) #x80)]
          [parser (~> (<skip> <wasm-u32> count) <pos>)])
      (do ([index 4 (fx+ index 5)]) ((fx>= index (bytevector-length bytes)))
        (bytevector-u8-set! bytes index 1))
      (measure-parser
       "WASM unsigned integers" count (lambda () (run-binary-parser parser bytes))
       (lambda (position) (= (* count 5) position))))))

(define measure-json5-whitespace
  (lambda (count)
    (let ([text (string-append (make-string count #\space) "42")])
      (measure-parser
       "JSON5 whitespace" count (lambda () (parse-json5 text))
       (lambda (document) (= 42 (json5-document-value document)))))))

(define measure-bounded-payloads
  (lambda (count)
    ;; Error cases: oversized Unicode scalars and NaN payloads must reject after lexical scanning.
    (let ([digits (make-string count #\f)])
      (map
       (lambda (entry)
         (let ([parser (cadr entry)] [source (caddr entry)])
           (measure-parser
            (car entry) count
            (lambda ()
              (guard (condition [(parser-error? condition) condition])
                (run-textual-parser parser source)))
            (lambda (condition)
              (and (parser-error? condition)
                   (= (if (eq? parser <wat-string>) 1 3)
                      (parser-error-offset condition)))))))
       (list (list "WAT oversized scalar" <wat-string>
                   (string-append "\"\\u{" digits "}\""))
             (list "WAT oversized NaN f32" <wat-f32> (string-append "nan:0x" digits))
             (list "WAT oversized NaN f64" <wat-f64> (string-append "nan:0x" digits)))))))

(define measure-long-exponent
  (lambda (count)
    (let ([text (string-append "-0e-" (make-string count #\9))])
      (measure-parser
       "WAT exponent digits" count (lambda () (run-textual-parser <wat-f64> text))
       (lambda (value) (= #x8000000000000000 (wasm-float-bits value)))))))

(define measure-xml-comment
  (lambda (count)
    (let* ([body (make-string count #\a)]
           [text (string-append "<r><!--" body "--></r>")])
      (measure-parser
       "XML comment characters" count (lambda () (parse-xml text))
       (lambda (document)
         (string=? body (xml-comment-value
                        (vector-ref (xml-element-children (xml-document-root document)) 0))))))))

(define measure-xml-no-cr
  (lambda (count)
    (let* ([body (make-string count #\a)] [text (string-append "<r>" body "</r>")])
      (measure-parser
       "XML text without CR" count (lambda () (parse-xml text))
       (lambda (document)
         (string=? body (xml-text-value
                        (vector-ref (xml-element-children (xml-document-root document)) 0))))))))

(define measure-wat-annotation
  (lambda (count)
    (let ([text (string-append "(@ignored " (make-string count #\a) ")")])
      (measure-parser
       "WAT annotation characters" count
       (lambda () (run-textual-parser (~> <wat-trivia> <pos>) text))
       (lambda (position) (= position (string-length text)))))))

(define measure-finite-significand
  (lambda (count)
    (let ([text (string-append (make-string count #\1) "e-" (number->string count))])
      (measure-parser
       "WAT finite significand" count (lambda () (run-textual-parser <wat-f64> text))
       (lambda (value) (= 4592670820000712476 (wasm-float-bits value)))))))

(define measure-xml-reference
  (lambda (count)
    ;; Error case: scanning an oversized reference must not build an oversized integer.
    (let ([text (string-append "<r>&#x" (make-string count #\f) ";</r>")])
      (measure-parser
       "XML oversized reference" count
       (lambda () (guard (condition [(parser-error? condition) condition]) (parse-xml text)))
       parser-error?))))

(define measure-binary-instructions
  (lambda (count)
    (let ([bytes (make-bytevector (fx1+ count) 1)])
      (bytevector-u8-set! bytes count 11)
      (measure-parser
       "WASM nop instructions" count (lambda () (run-binary-parser <wasm-expression> bytes))
       (lambda (body)
         (and (= count (vector-length body))
              (eq? 'nop (wasm-instruction-mnemonic (vector-ref body (fx1- count))))))))))

(define measure-binary-floats
  (lambda (count)
    (let ([bytes (make-bytevector (* count 8) 0)] [parser (<rep> <wasm-f64> count)])
      (measure-parser
       "WASM f64 values" count (lambda () (run-binary-parser parser bytes))
       (lambda (values)
         (and (= count (length values))
              (for-all (lambda (value) (= 0 (wasm-float-bits value))) values)))))))

(define measure-counted-vector
  (lambda (count)
    (let* ([prefix (u8-list->bytevector
                    (let loop ([value count])
                      (if (< value 128) (list value)
                          (cons (logor 128 (logand value 127))
                                (loop (ash value -7))))))]
           [bytes (make-bytevector (+ (bytevector-length prefix) count) 0)]
           [parser (<wasm-vector> <wasm-u32>)])
      (bytevector-copy! prefix 0 bytes 0 (bytevector-length prefix))
      (measure-parser
       "WASM counted u32 vector" count (lambda () (run-binary-parser parser bytes))
       (lambda (result) (and (= count (vector-length result)) (vfor-all zero? result)))))))

(define measure-text-memory-instructions
  (lambda (count)
    (let ([source (apply string-append
                        (make-list count "i32.load offset=8 align=4 "))]
          [parser (<~ <wat-expression> <eof>)])
      (measure-parser
       "WAT memory instructions" count (lambda () (run-textual-parser parser source))
       (lambda (body)
         (and (= count (vector-length body))
              (eq? 'i32.load (wat-instruction-syntax-mnemonic (vector-ref body 0)))))))))

(define measure-text-branch-table
  (lambda (count)
    (let ([source (string-append "br_table "
                                (apply string-append (make-list count "0 ")) "0")]
          [parser (<~ <wat-instruction> <eof>)])
      (measure-parser
       "WAT br_table targets" count (lambda () (run-textual-parser parser source))
       (lambda (instruction)
         (let ([immediates (wat-instruction-syntax-immediates instruction)])
           (and (= count (vector-length (vector-ref immediates 0)))
                (= 0 (wat-index-reference-value (vector-ref immediates 1))))))))))

(define measure-elf-hash-validation
  (lambda (count)
    ;; One SysV hash linked to a valid symbol table: null symbol, then unnamed globals.
    (let* ([symbols-offset 328] [hash-offset (+ symbols-offset (* count 24))]
           [hash-size (+ 12 (* count 4))]
           [bytes (make-bytevector (+ hash-offset hash-size) 0)]
           [endian (endianness little)])
      (bytevector-copy! #vu8(127 69 76 70 2 1 1) 0 bytes 0 7)
      (bytevector-u16-set! bytes 16 1 endian)
      (bytevector-u16-set! bytes 18 62 endian)
      (bytevector-u32-set! bytes 20 1 endian)
      (bytevector-u64-set! bytes 40 64 endian)
      (bytevector-u16-set! bytes 52 64 endian)
      (bytevector-u16-set! bytes 58 64 endian)
      (bytevector-u16-set! bytes 60 4 endian)
      (for-each
       (lambda (index type offset size link info alignment entry-size)
         (let ([at (+ 64 (* 64 index))])
           (bytevector-u32-set! bytes (+ at 4) type endian)
           (bytevector-u64-set! bytes (+ at 24) offset endian)
           (bytevector-u64-set! bytes (+ at 32) size endian)
           (bytevector-u32-set! bytes (+ at 40) link endian)
           (bytevector-u32-set! bytes (+ at 44) info endian)
           (bytevector-u64-set! bytes (+ at 48) alignment endian)
           (bytevector-u64-set! bytes (+ at 56) entry-size endian)))
       '(1 2 3) '(3 2 5) (list 320 symbols-offset hash-offset)
       (list 1 (* count 24) hash-size) '(0 1 2) '(0 1 0) '(1 8 4) '(0 24 4))
      (do ([index 1 (fx1+ index)]) ((fx= index count))
        (bytevector-u8-set! bytes (+ symbols-offset (* index 24) 4) 16))
      (bytevector-u32-set! bytes hash-offset 1 endian)
      (bytevector-u32-set! bytes (+ hash-offset 4) count endian)
      (measure-parser
       "ELF hash entries" count (lambda () (parse-elf bytes))
       (lambda (file)
         (let ([hash (elf-section-content (vector-ref (elf-file-sections file) 3))])
           (and (= count (vector-length (elf-hash-table-chains hash)))
                (vfor-all zero? (elf-hash-table-chains hash)))))))))

(define measure-json5-exact-hex
  (lambda (count)
    (let ([source (string-append "0x" (make-string count #\f))])
      (measure-parser
       "JSON5 exact hex digits" count (lambda () (parse-json5 source))
       (lambda (document) (= (- (expt 16 count) 1) (json5-document-value document)))))))

(define measure-short-value-types
  (lambda (byte name)
    (let ([bytes (make-bytevector 100000 byte)]
          [parser (<rep> <wasm-value-type> 100000)])
      (measure-parser
       (format "WASM ~a value types" name) 100000
       (lambda () (run-binary-parser parser bytes))
       (lambda (types)
         (and (= 100000 (length types))
              (for-all (lambda (type)
                         (if (eq? name 'funcref)
                             (and (wasm-reference-type? type)
                                  (wasm-reference-type-nullable? type)
                                  (eq? 'func (wasm-reference-type-heap-type type)))
                             (eq? name type)))
                       types)))))))

(define measure-wat-punctuation
  (lambda (count)
    (let ([source (string-append "$" (make-string count #\!))])
      (measure-parser
       "WAT identifier punctuation" count
       (lambda () (run-textual-parser <wat-identifier> source))
       (lambda (identifier) (string=? source identifier))))))

(define measure-xml-delimited-content
  (lambda (count prefix suffix name extract)
    (let* ([body (make-string count #\a)] [source (string-append prefix body suffix)])
      (measure-parser
       (format "XML ~a characters" name) count (lambda () (parse-xml source))
       (lambda (document) (string=? body (extract document)))))))

(define measure-decimal-integers
  (lambda (count)
    (let ([source (make-string count #\9)] [parser (<as-integer> (<some> <digit>))])
      (map
       (lambda (name parse input)
         (measure-parser
          name count (lambda () (parse input))
          (lambda (value) (= (- (expt 10 count) 1) value))))
       '("Combinator exact decimal" "Scheme exact decimal"
         "JSON5 exact decimal" "TOML exact decimal")
       (list (lambda (text) (run-textual-parser parser text)) parse-scheme-datum
             (lambda (text) (json5-document-value (parse-json5 text)))
             (lambda (text) (toml-table-ref (toml-document-root (parse-toml text)) "n")))
       (list source source source (string-append "n=" source))))))

(define measure-character-membership
  (lambda (count)
    (map
     (lambda (name character parser)
       (let ([source (make-string count character)])
         (measure-parser name count (lambda () (run-textual-parser parser source))
                         (lambda (characters) (= count (length characters))))))
     '("Character one-of" "Character none-of") '(#\space #\a)
     (list (<some> (<one-of> " \t")) (<some> (<none-of> "\"\\"))))))

(define measure-scheme-string
  (lambda (count)
    (let* ([body (make-string count #\a)] [source (string-append "\"" body "\"")])
      (measure-parser "Scheme ordinary string" count (lambda () (parse-scheme-datum source))
                      (lambda (value) (string=? body value))))))

(define measure-toml-whitespace
  (lambda (count)
    (let ([source (string-append (make-string count #\space) "n=1")])
      (measure-parser "TOML leading spaces" count (lambda () (parse-toml source))
                      (lambda (document) (= 1 (toml-table-ref (toml-document-root document) "n")))))))

(define measure-elf-word-table
  (lambda (count)
    ;; A two-section executable with one fixed-width RELR table, without symbol-name costs.
    (let ([bytes (make-bytevector (+ 192 (* count 8)) 0)] [endian (endianness little)])
      (bytevector-copy! #vu8(127 69 76 70 2 1 1) 0 bytes 0 7)
      (bytevector-u16-set! bytes 16 3 endian)
      (bytevector-u16-set! bytes 18 62 endian)
      (bytevector-u32-set! bytes 20 1 endian)
      (bytevector-u64-set! bytes 40 64 endian)
      (bytevector-u16-set! bytes 52 64 endian)
      (bytevector-u16-set! bytes 58 64 endian)
      (bytevector-u16-set! bytes 60 2 endian)
      (bytevector-u32-set! bytes (+ 128 4) 19 endian)
      (bytevector-u64-set! bytes (+ 128 24) 192 endian)
      (bytevector-u64-set! bytes (+ 128 32) (* count 8) endian)
      (bytevector-u64-set! bytes (+ 128 48) 8 endian)
      (bytevector-u64-set! bytes (+ 128 56) 8 endian)
      (bytevector-u64-set! bytes 192 #x1000 endian)
      (measure-parser
       "ELF RELR words" count (lambda () (parse-elf bytes))
       (lambda (file)
         (let ([entries (elf-relr-table-entries
                         (elf-section-content (vector-ref (elf-file-sections file) 1)))])
           (and (= count (vector-length entries)) (= #x1000 (vector-ref entries 0)))))))))

(printf "~a ~a\n" (scheme-version) (machine-type))
(define measure-generic-leb128
  (lambda (count signed?)
    (let ([bytes (make-bytevector count 255)]
          [parser (if signed? (<sleb128> (* count 7)) <uleb128>)]
          [expected (if signed? -1 (- (ash 1 (* count 7)) 1))])
      (bytevector-u8-set! bytes (fx1- count) 127)
      (measure-parser
       (if signed? "Generic signed LEB" "Generic unsigned LEB") count
       (lambda () (run-binary-parser parser bytes))
       (lambda (number) (= expected number))))))

(define measure-ignored-whitespace
  (lambda (count)
    (let ([text (make-string count #\space)] [parser (<fully> (<result> #t))])
      (measure-parser "Ignored token whitespace" count
                      (lambda () (run-textual-parser parser text))
                      (lambda (value) (eq? #t value))))))

(define measure-csv-quoted-field
  (lambda (count)
    (let* ([body (make-string count #\a)] [text (string-append "\"" body "\"")])
      (measure-parser
       "CSV quoted characters" count (lambda () (parse-csv text))
       (lambda (document)
         (string=? body (vector-ref
                        (csv-record-fields (vector-ref (csv-document-records document) 0)) 0)))))))

(define measure-json5-characters
  (lambda (count identifier?)
    (let* ([body (make-string count #\a)]
           [text (if identifier? (string-append "{" body ":0}")
                     (string-append "\"" body "\""))])
      (measure-parser
       (if identifier? "JSON5 identifier characters" "JSON5 string characters") count
       (lambda () (parse-json5 text))
       (lambda (document)
         (string=? body
                   (if identifier?
                       (json5-member-name
                        (vector-ref (json5-object-members (json5-document-value document)) 0))
                       (json5-document-value document))))))))

(define measure-toml-string-characters
  (lambda (count delimiter label)
    (let* ([body (make-string count #\a)] [text (string-append "v=" delimiter body delimiter)])
      (measure-parser
       label count (lambda () (parse-toml text))
       (lambda (document) (string=? body (toml-table-ref (toml-document-root document) "v")))))))

(define measure-xml-attribute-characters
  (lambda (count)
    (let* ([body (make-string count #\a)] [text (string-append "<r a='" body "'/>")])
      (measure-parser
       "XML attribute characters" count (lambda () (parse-xml text))
       (lambda (document)
         (string=? body (xml-attribute-value
                         (vector-ref (xml-element-attributes (xml-document-root document)) 0))))))))

(define measure-block-comment-characters
  (lambda (count wat?)
    (let* ([body (make-string count #\a)]
           [text (if wat? (string-append "(;" body ";)(module)")
                     (string-append "/*" body "*/0"))])
      (measure-parser
       (if wat? "WAT block comment characters" "JSON5 block comment characters") count
       (lambda () (if wat? (parse-wasm-text-module text) (parse-json5 text)))
       (lambda (value)
         (if wat? (= 0 (vector-length (wasm-module-functions value)))
             (= 0 (json5-document-value value))))))))

(define measure-wasm-declarations
  (lambda (count tables?)
    (define encode-count
      (lambda (value)
        (if (< value 128) (list value)
            (cons (+ 128 (bitwise-and value 127)) (encode-count (ash value -7))))))
    (let* ([entry-size (if tables? 4 2)]
           [count-bytes (encode-count count)]
           [payload-size (+ (length count-bytes) (* count entry-size))]
           [header (u8-list->bytevector
                    (append '(0 97 115 109 1 0 0 0) (list (if tables? 4 5))
                            (encode-count payload-size) count-bytes))]
           [start (bytevector-length header)]
           [bytes (make-bytevector (+ start (* count entry-size)) 0)])
      (bytevector-copy! header 0 bytes 0 start)
      (when tables?
        (do ([index start (+ index 4)]) ((= index (bytevector-length bytes)))
          (bytevector-u8-set! bytes index 112)
          (bytevector-u8-set! bytes (+ index 1) 1)))
      (measure-parser
       (if tables? "WASM table declarations" "WASM memory declarations") count
       (lambda () (parse-wasm-binary-module bytes))
       (lambda (module)
         (= count (vector-length ((if tables? wasm-module-tables wasm-module-memories)
                                  module))))))))

(define measure-csv-unquoted-records
  (lambda (count)
    (let ([text (apply string-append (make-list count "a,b,c\n"))])
      (measure-parser
       "CSV unquoted records" count (lambda () (parse-csv text))
       (lambda (document) (= count (vector-length (csv-document-records document))))))))

(benchmark-print-table!
 "Parser review workloads"
 '(("workload" . left) ("count/exponent" . right)
   ("allocated bytes" . right) ("elapsed ms" . right))
 (append (map (lambda (count) (measure-wasm-declarations count #f)) '(10000 100000))
         (map (lambda (count) (measure-wasm-declarations count #t)) '(10000 100000))
         (map measure-xml-attributes '(4000 8000 16000 32000))
         (map measure-zero-float '(10000 100000 1000000))
         (measure-byte-vectors)
         (map measure-choice-failure '(1000 2000 4000 8000))
         (map (lambda (count) (measure-block-comment-characters count #t)) '(100000 1000000))
         (map (lambda (count) (measure-block-comment-characters count #f)) '(100000 1000000))
         (map measure-xml-attribute-characters '(100000 1000000))
         (map (lambda (count) (measure-toml-string-characters count "\"" "TOML basic characters"))
              '(100000 1000000))
         (map (lambda (count)
                (measure-toml-string-characters count "\"\"\"" "TOML multiline basic characters"))
              '(100000 1000000))
         (map (lambda (count)
                (measure-toml-string-characters count "'''" "TOML multiline literal characters"))
              '(100000 1000000))
         (map (lambda (count) (measure-json5-characters count #f)) '(100000 1000000))
         (map (lambda (count) (measure-json5-characters count #t)) '(100000 1000000))
         (map measure-csv-quoted-field '(100000 1000000))
         (map measure-csv-unquoted-records '(10000 100000))
         (map measure-ignored-whitespace '(100000 1000000))
         (map (lambda (count) (measure-generic-leb128 count #f)) '(1000 4000 16000 32000))
         (map (lambda (count) (measure-generic-leb128 count #t)) '(1000 4000 16000 32000))
         (map measure-nested-choice-failure '(1000 2000 4000 8000))
         (map measure-bounded-integer-overflow '(1000 2000 4000 8000 16000))
         (list (measure-signed-integers 100000))
         (map measure-elf-completion '(1048576 8388608 67108864))
         (map measure-wat-string '(8192 32768 131072 262144))
         (list (measure-character-conversion 262144)
               (measure-character-predicates 1000000)
               (measure-unsigned-integers 100000)
               (measure-json5-whitespace 1000000))
         (measure-bounded-payloads 16000)
         (map measure-long-exponent '(1000 4000 16000))
         (list (measure-xml-comment 250000) (measure-xml-no-cr 1000000)
               (measure-wat-annotation 250000) (measure-wat-annotation 1000000))
         (map measure-finite-significand '(1000 4000 16000))
         (map measure-xml-reference '(1000 4000 16000 32000))
         (map measure-binary-instructions '(10000 100000 500000))
         (list (measure-binary-floats 1000000))
         (map measure-counted-vector '(1000 100000 1000000))
         (map measure-text-memory-instructions '(1000 10000 50000))
         (map measure-text-branch-table '(1000 10000 50000))
         (map measure-elf-hash-validation '(1000 10000 100000))
         (map measure-json5-exact-hex '(1000 4000 16000 32000))
         (map measure-short-value-types '(127 124 123 112) '(i32 f64 v128 funcref))
         (apply append (map measure-decimal-integers '(1000 4000 16000 32000)))
         (measure-character-membership 1000000)
         (list (measure-scheme-string 1000000) (measure-toml-whitespace 1000000))
         (map measure-elf-word-table '(1000 10000 100000))
         (list
          (measure-wat-punctuation 100000)
          (measure-xml-delimited-content
           1000000 "<r><![CDATA[" "]]></r>" 'cdata
           (lambda (document)
             (xml-cdata-value
              (vector-ref (xml-element-children (xml-document-root document)) 0))))
          (measure-xml-delimited-content
           1000000 "<?p " "?><r/>" 'processing-instruction
           (lambda (document)
             (xml-processing-instruction-data
              (vector-ref (xml-document-before-root document) 0)))))))
