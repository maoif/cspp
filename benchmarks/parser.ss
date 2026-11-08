(import (cspp)
        (cspp parser combinator)
        (cspp parser wasm binary values))
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

(printf "~a ~a\n" (scheme-version) (machine-type))
(benchmark-print-table!
 "Parser review workloads"
 '(("workload" . left) ("count/exponent" . right)
   ("allocated bytes" . right) ("elapsed ms" . right))
 (append (map measure-xml-attributes '(4000 8000 16000 32000))
         (map measure-zero-float '(10000 100000 1000000))
         (measure-byte-vectors)))
