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

(benchmark-print-table!
 "WAT allocation scaling"
 '(("workload" . left) ("count" . right) ("allocated bytes" . right) ("elapsed ms" . right))
 (append (map measure-folded '(1000 2000 4000 8000))
         (map measure-implicit-types '(1000 2000 4000 8000))
         (map measure-long-implicit-types '(250 500 1000 2000))))
