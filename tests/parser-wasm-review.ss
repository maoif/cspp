;; Included by parser-wasm.ss; these tests cover the second parser review.
(define wasm-review-binary
  (lambda (parser bytes)
    (run-binary-parser (<~ parser <eof>) bytes)))

(define wasm-review-error?
  (lambda (thunk)
    (and (capture-parser-error thunk) #t)))

(mat wasm-review-binary-types

     (equal? '(exn none noextern nofunc noexn)
             (map (lambda (byte)
                    (wasm-review-binary <wasm-heap-type> (bytevector byte)))
                  '(#x69 #x71 #x72 #x73 #x74)))

     ;; Error case: obsolete heap-type bytes are unassigned in final Core 3.0.
     (for-all (lambda (byte)
                (wasm-review-error?
                 (lambda () (wasm-review-binary <wasm-heap-type> (bytevector byte)))))
              '(#x65 #x66 #x67 #x68))

     ;; Error case: abstract heap types have one-byte encodings, unlike concrete indices.
     (wasm-review-error?
      (lambda () (wasm-review-binary <wasm-heap-type> #vu8(#xf0 #x7f))))

     (= 0 (wasm-review-binary <wasm-heap-type> #vu8(#x80 #x00)))

     (let* ([module (parse-wasm-text-module
                     "(module (func i32.const -1 i32.const 2147483648
                                    i64.const 18446744073709551615))")]
            [body (wasm-function-body (vector-ref (wasm-module-functions module) 0))])
       (equal? '(-1 -2147483648 -1)
               (map (lambda (instruction)
                      (vector-ref (wasm-instruction-immediates instruction) 0))
                    (vector->list body))))

     (let* ([module (parse-wasm-text-module
                     "(module (func block $outer
                        try_table (catch_all $outer) nop end end))")]
            [block (vector-ref (wasm-function-body
                                (vector-ref (wasm-module-functions module) 0)) 0)]
            [try (vector-ref (wasm-instruction-body block) 0)]
            [catch (vector-ref (wasm-instruction-alternate try) 0)])
       (= 0 (wasm-catch-label-index catch)))

     ;; Error case: a catch target cannot name the try_table's own body label.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  "(module (func try_table $self (catch_all $self) nop end))")))

     )

(mat wasm-review-byte-vectors

     (let ([first (wasm-review-binary <wasm-byte-vector> #vu8(3 1 2 3))]
           [second (wasm-review-binary <wasm-byte-vector> #vu8(3 1 2 3))])
       (bytevector-u8-set! first 0 99)
       (equal? #vu8(1 2 3) second))

     (equal? #vu8() (wasm-review-binary <wasm-byte-vector> #vu8(0)))

     ;; Error case: a byte-vector length cannot exceed its enclosing binary range.
     (wasm-review-error?
      (lambda () (wasm-review-binary (<bounded> 3 <wasm-byte-vector>) #vu8(3 1 2 3))))

     ;; Error case: a huge length with no payload must fail before any large allocation.
     (wasm-review-error?
      (lambda () (wasm-review-binary <wasm-byte-vector>
                                     #vu8(255 255 255 255 15))))

     )

(mat wasm-review-local-count

     ;; Error case: compressed declaration counts must sum to less than 2^32 before allocation.
     (wasm-review-error?
      (lambda ()
        (parse-wasm-binary-module
         #vu8(0 97 115 109 1 0 0 0 1 4 1 96 0 0 3 2 1 0
               10 12 1 10 2 255 255 255 255 15 127 1 127 11))))

     (let* ([module (parse-wasm-binary-module
                     #vu8(0 97 115 109 1 0 0 0 1 4 1 96 0 0 3 2 1 0
                           10 8 1 6 2 2 127 1 126 11))]
            [function (vector-ref (wasm-module-functions module) 0)])
       (equal? '#(i32 i32 i64) (wasm-function-locals function)))

     )

(define wasm-review-float-bits
  (lambda (parser text)
    (wasm-float-bits (run-textual-parser (<~ parser <eof>) text))))

(mat wasm-review-float-spellings

     (= #x3fc00000 (wasm-review-float-bits <wat-f32> "0x1.8"))

     (= #x3f800000 (wasm-review-float-bits <wat-f32> "0x1."))

     (= #x3ff8000000000000 (wasm-review-float-bits <wat-f64> "0x1.8"))

     (= #x7f7fffff (wasm-review-float-bits <wat-f32> "0x1.fffffep127"))

     ;; Error case: rounding at the upper finite limit overflows to infinity.
     (wasm-review-error?
      (lambda () (wasm-review-float-bits <wat-f32> "0x1.ffffffp127")))

     (= 0 (wasm-review-float-bits <wat-f32> "0x1p-150"))

     (= 1 (wasm-review-float-bits <wat-f32> "0x1.000002p-150"))

     (= 0 (wasm-review-float-bits <wat-f64> "0x1p-1075"))

     (= 1 (wasm-review-float-bits <wat-f64> "0x1.0000000000001p-1075"))

     ;; Huge exponents describe bounded values without constructing enormous exact powers.
     (let* ([before (statistics)]
            [values (map (lambda (text) (wasm-review-float-bits <wat-f32> text))
                         '("0e1000000" "-0e-1000000" "1e-1000000"))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (equal? '(0 2147483648 0) values) (< allocated 1000000)))

     ;; Error case: enormous positive exponents report overflow without allocating huge powers.
     (wasm-review-error?
      (lambda () (wasm-review-float-bits <wat-f64> "1e1000000")))

     ;; A long mantissa may compensate for a large negative exponent and stay finite.
     (= #x3f800000
        (wasm-review-float-bits
         <wat-f32> (string-append "1" (make-string 1000 #\0) "e-1000")))

     )

(mat wasm-review-text-expressions

     (let* ([module (parse-wasm-text-module
                     "(module (memory 1)
                        (data (i32.add (i32.const 1) (i32.const 2)) \"x\"))")]
            [offset (wasm-data-offset (vector-ref (wasm-module-data module) 0))])
       (equal? '(i32.const i32.const i32.add)
               (map wasm-instruction-mnemonic (vector->list offset))))

     (let* ([module (parse-wasm-text-module
                     "(module (memory 1)
                        (data (offset (i32.add (i32.const 1) (i32.const 2))) \"x\"))")]
            [offset (wasm-data-offset (vector-ref (wasm-module-data module) 0))])
       (equal? '(i32.const i32.const i32.add)
               (map wasm-instruction-mnemonic (vector->list offset))))

     (let* ([module (parse-wasm-text-module
                     "(module (func $f)
                        (elem funcref (item ref.func $f) (item (ref.null func))))")]
            [items (wasm-element-initializers (vector-ref (wasm-module-elements module) 0))])
       (and (= 2 (vector-length items))
            (eq? 'ref.func (wasm-instruction-mnemonic (vector-ref (vector-ref items 0) 0)))
            (eq? 'ref.null (wasm-instruction-mnemonic (vector-ref (vector-ref items 1) 0)))))

     (let* ([module (parse-wasm-text-module
                     "(module (table 1 (ref func) (ref.func 0)) (func))")]
            [table (vector-ref (wasm-module-tables module) 0)]
            [initializer (wasm-table-initializer table)])
       (and (= 1 (vector-length initializer))
            (eq? 'ref.func (wasm-instruction-mnemonic (vector-ref initializer 0)))))

     (let* ([module (parse-wasm-text-module "(module (rec))")]
            [groups (wasm-module-types module)])
       (and (= 1 (vector-length groups))
            (= 0 (vector-length (wasm-recursive-type-subtypes (vector-ref groups 0))))))

     ;; Error case: an imported table cannot have an initializer expression.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  "(module (table (import \"m\" \"t\") 1 funcref (ref.null func)))")))

     )

(mat wasm-review-annotations

     (wasm-module? (parse-wasm-text-module "(module (@\"quoted tag\"))"))

     (wasm-module? (parse-wasm-text-module "(module (@a (x) () (y (z))))"))

     (wasm-module? (parse-wasm-text-module "(module (@a (@(@(@(@))))))"))

     ;; Error case: ignoring an annotation must not accept raw forbidden control characters.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  (string-append "(module (@a " (string #\nul) "))"))))

     ;; Error case: ordinary parentheses inside annotations must also balance.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module "(module (@a (x))")))

     ;; Error case: a malformed nested comment must not be accepted as an ordinary group.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module "(module (@a (; unterminated)))")))

     )

(mat wasm-review-shared-memory

     (not (wasm-limits-shared? (make-wasm-limits 'i32 1 2)))

     (wasm-limits-shared? (make-wasm-limits 'i64 1 2 #t))

     (let* ([text (parse-wasm-text-module "(module (memory 1 2 shared))")]
            [binary (parse-wasm-binary-module
                     #vu8(0 97 115 109 1 0 0 0 5 4 1 3 1 2))]
            [text-limits (wasm-memory-type-limits
                          (wasm-memory-type (vector-ref (wasm-module-memories text) 0)))]
            [binary-limits (wasm-memory-type-limits
                            (wasm-memory-type (vector-ref (wasm-module-memories binary) 0)))])
       (and (wasm-limits-shared? text-limits)
            (wasm-limits-shared? binary-limits)
            (= 1 (wasm-limits-minimum binary-limits))
            (= 2 (wasm-limits-maximum text-limits))))

     (let* ([binary (parse-wasm-binary-module
                     #vu8(0 97 115 109 1 0 0 0 5 4 1 7 1 2))]
            [limits (wasm-memory-type-limits
                     (wasm-memory-type (vector-ref (wasm-module-memories binary) 0)))])
       (and (wasm-limits-shared? limits) (eq? 'i64 (wasm-limits-address-type limits))))

     ;; Error case: binary shared-memory flags require a maximum bound.
     (wasm-review-error?
      (lambda () (wasm-review-binary <wasm-memory-type> #vu8(2 1))))

     ;; Error case: shared limits are not valid table types.
     (wasm-review-error?
      (lambda () (wasm-review-binary <wasm-table-type> #vu8(112 3 1 2))))

     ;; Error case: constructor validation rejects shared limits without a maximum.
     (error? (make-wasm-limits 'i32 1 #f #t))

     ;; Error case: the public table constructor also rejects shared-memory limits.
     (error? (make-wasm-table-type (make-wasm-reference-type #t 'func)
                                  (make-wasm-limits 'i32 1 2 #t)))

     )
