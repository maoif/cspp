(import (cspp)
        (cspp parser combinator)
        (cspp parser wasm binary values)
        (cspp parser wasm text lexical))
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

(printf "~a ~a\n" (scheme-version) (machine-type))
(benchmark-print-table!
 "Parser review workloads"
 '(("workload" . left) ("count/exponent" . right)
   ("allocated bytes" . right) ("elapsed ms" . right))
 (append (map measure-xml-attributes '(4000 8000 16000 32000))
         (map measure-zero-float '(10000 100000 1000000))
         (measure-byte-vectors)
         (map measure-choice-failure '(1000 2000 4000 8000))
         (map measure-nested-choice-failure '(1000 2000 4000 8000))
         (map measure-bounded-integer-overflow '(1000 2000 4000 8000 16000))
         (list (measure-signed-integers 100000))
         (map measure-elf-completion '(1048576 8388608 67108864))
         (map measure-wat-string '(8192 32768 131072 262144))
         (list (measure-character-conversion 262144)
               (measure-character-predicates 1000000)
               (measure-unsigned-integers 100000)
               (measure-json5-whitespace 1000000))
         (measure-bounded-payloads 16000)))
