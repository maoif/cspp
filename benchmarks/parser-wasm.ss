(import (cspp) (cspp parser wasm text) (cspp parser wasm normalize))
(load "benchmarks/common.ss")

;; Run from the repository root: ./cspp --script=benchmarks/parser-wasm.ss
;; Source construction and collection precede measurement; bytes are cumulative allocation.
(define measure-wat
  (lambda (label count source validate)
    (collect)
    (let* ([before (statistics)] [started (real-time)]
           [module (parse-wasm-text-module source)]
           [elapsed (- (real-time) started)]
           [allocated (sstats-bytes (sstats-difference (statistics) before))])
      (unless (validate module)
        (error 'parser-wasm-benchmark "incorrect parse result" label count))
      (map (lambda (value) (format "~a" value))
           (list label count allocated elapsed)))))

(define measure-folded
  (lambda (count)
    (measure-wat
     "Folded operators" count
     (string-append "(module (func (result i32) "
                    (apply string-append (make-list count "(i32.eqz "))
                    "(i32.const 0)" (make-string count #\)) "))")
     (lambda (module)
       (let ([body (wasm-function-body (vector-ref (wasm-module-functions module) 0))])
         (and (= (fx1+ count) (vector-length body))
              (eq? 'i32.const (wasm-instruction-mnemonic (vector-ref body 0)))
              (eq? 'i32.eqz (wasm-instruction-mnemonic (vector-ref body count)))))))))

(define measure-implicit-types
  (lambda (count)
    (measure-wat
     "Implicit types" count
     (string-append
      "(module " (apply string-append (make-list count "(type (struct))"))
      (apply string-append
             (map (lambda (index) (format "(func (param (ref ~a)))" index)) (iota count)))
      ")")
     (lambda (module)
       (and (= (fx* count 2) (vector-length (wasm-module-types module)))
            (= count (vector-length (wasm-module-functions module)))
            (= count (wasm-function-type-index (vector-ref (wasm-module-functions module) 0)))
            (= (fx1- (fx* count 2))
               (wasm-function-type-index
                (vector-ref (wasm-module-functions module) (fx1- count)))))))))

(define measure-long-implicit-types
  (lambda (count)
    (let* ([fixed (apply string-append (make-list 64 " i32"))]
           [source (string-append
                    "(module " (apply string-append (make-list count "(type (struct))"))
                    (apply string-append
                           (map (lambda (index)
                                  (format "(func (param~a (ref ~a)))" fixed index))
                                (iota count))) ")")]
           ;; Isolate signature interning from textual parsing for this workload.
           [syntax (run-textual-parser parser-wat-module-syntax source)])
      (collect)
      (let* ([before (statistics)] [started (real-time)]
             [module (normalize-wat-module syntax)]
             [elapsed (- (real-time) started)]
             [allocated (sstats-bytes (sstats-difference (statistics) before))])
        (unless (and (wasm-module? module)
                     (= (* 2 count) (vector-length (wasm-module-types module)))
                     (= count (vector-length (wasm-module-functions module)))
                     (= (- (* 2 count) 1)
                        (wasm-function-type-index
                         (vector-ref (wasm-module-functions module) (- count 1)))))
          (error 'parser-wasm-benchmark "incorrect long-signature result" count))
        (map (lambda (value) (format "~a" value))
             (list "Long signatures (normalize)" count allocated elapsed))))))

(define measure-named-labels
  (lambda (count)
    (let* ([source
            (call-with-port (open-output-string)
              (lambda (port)
                (display "(module (func block $outer " port)
                (do ([index 0 (fx1+ index)]) ((fx= index count))
                  (display "block br $outer " port))
                (do ([index 0 (fx1+ index)]) ((fx> index count))
                  (display "end " port))
                (display "))" port)
                (get-output-string port)))]
           [syntax (run-textual-parser parser-wat-module-syntax source)])
      ;; Isolate label resolution from text parsing and source construction.
      (collect)
      (let* ([before (statistics)] [started (real-time)]
             [module (normalize-wat-module syntax)]
             [elapsed (- (real-time) started)]
             [allocated (sstats-bytes (sstats-difference (statistics) before))])
        (let loop ([body (wasm-instruction-body
                         (vector-ref (wasm-function-body
                                      (vector-ref (wasm-module-functions module) 0)) 0))]
                   [depth 1])
          (let* ([block (vector-ref body (if (fx= depth 1) 0 1))]
                 [nested (wasm-instruction-body block)])
            (unless (and (eq? 'block (wasm-instruction-mnemonic block))
                         (eq? 'br (wasm-instruction-mnemonic (vector-ref nested 0)))
                         (fx= depth (vector-ref
                                     (wasm-instruction-immediates (vector-ref nested 0)) 0)))
              (error 'parser-wasm-benchmark "incorrect named branch" depth))
            (if (fx= depth count)
                (unless (fx= 1 (vector-length nested))
                  (error 'parser-wasm-benchmark "incorrect nested leaf"))
                (loop nested (fx1+ depth)))))
        (map (lambda (value) (format "~a" value))
             (list "Named labels (normalize)" count allocated elapsed))))))

(benchmark-print-table!
 "WAT allocation scaling"
 '(("workload" . left) ("count" . right) ("allocated bytes" . right) ("elapsed ms" . right))
 (append (map measure-folded '(1000 2000 4000 8000))
         (map measure-implicit-types '(1000 2000 4000 8000))
         (map measure-long-implicit-types '(250 500 1000 2000))
         (map measure-named-labels '(1000 2000 4000 8000 16000))))
