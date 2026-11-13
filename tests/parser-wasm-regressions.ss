;; WebAssembly format and normalization regressions; included by parser-wasm.ss.
(define wasm-review-error?
  (lambda (thunk)
    (and (parser-error-from thunk) #t)))

(mat wasm-review-binary-types

     (equal? '(exn none noextern nofunc noexn)
             (map (lambda (byte)
                    (parse-binary <wasm-heap-type> (bytevector byte)))
                  '(#x69 #x71 #x72 #x73 #x74)))

     ;; Error case: obsolete heap-type bytes are unassigned in final Core 3.0.
     (for-all (lambda (byte)
                (wasm-review-error?
                 (lambda () (parse-binary <wasm-heap-type> (bytevector byte)))))
              '(#x65 #x66 #x67 #x68))

     ;; Error case: abstract heap types have one-byte encodings, unlike concrete indices.
     (wasm-review-error?
      (lambda () (parse-binary <wasm-heap-type> #vu8(#xf0 #x7f))))

     (= 0 (parse-binary <wasm-heap-type> #vu8(#x80 #x00)))

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

     (let ([first (parse-binary <wasm-byte-vector> #vu8(3 1 2 3))]
           [second (parse-binary <wasm-byte-vector> #vu8(3 1 2 3))])
       (bytevector-u8-set! first 0 99)
       (equal? #vu8(1 2 3) second))

     ;; Error case: a byte-vector length cannot exceed its enclosing binary range.
     (wasm-review-error?
      (lambda () (parse-binary (<bounded> 3 <wasm-byte-vector>) #vu8(3 1 2 3))))

     ;; Error case: a huge length with no payload must fail before any large allocation.
     (wasm-review-error?
      (lambda () (parse-binary <wasm-byte-vector>
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
      (lambda () (parse-binary <wasm-memory-type> #vu8(2 1))))

     ;; Error case: shared limits are not valid table types.
     (wasm-review-error?
      (lambda () (parse-binary <wasm-table-type> #vu8(112 3 1 2))))

     ;; Error case: constructor validation rejects shared limits without a maximum.
     (error? (make-wasm-limits 'i32 1 #f #t))

     ;; Error case: the public table constructor also rejects shared-memory limits.
     (error? (make-wasm-table-type (make-wasm-reference-type #t 'func)
                                  (make-wasm-limits 'i32 1 2 #t)))

     )

(mat wasm-review-annotation-lexical-boundaries

     ;; Error case: a top-level annotation requires a nonempty identifier.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (@))" "(module (@ ))" "(module (@\"\"))"))

     ;; Error case: unquoted annotation tokens are ASCII, even in otherwise ignored contents.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (@a Heiße))" "(module (@a 😀))"))

     (wasm-module? (parse-wasm-text-module "(module (@a \"Heiße 😀\" (@)))"))

     )

(mat wasm-review-mixed-and-folded-instructions

     (for-all
      (lambda (text)
        (let* ([module (parse-wasm-text-module text)]
               [body (wasm-function-body (vector-ref (wasm-module-functions module) 0))]
               [control (vector-ref body (fx1- (vector-length body)))])
          (eq? 'nop (wasm-instruction-mnemonic
                     (vector-ref (wasm-instruction-body control) 0)))))
      '("(module (func block (nop) end))"
        "(module (func loop (nop) end))"
        "(module (func i32.const 0 if (nop) else (nop) end))"
        "(module (func try_table (nop) end))"))

     (let* ([module (parse-wasm-text-module
                     "(module (func (i32.add (i32.const 1) (i32.eqz (i32.const 0)))))")]
            [body (wasm-function-body (vector-ref (wasm-module-functions module) 0))])
       (equal? '(i32.const i32.const i32.eqz i32.add)
               (map wasm-instruction-mnemonic (vector->list body))))

     ;; Error cases: operands of a folded instruction must themselves be folded.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func (drop i32.const 0)))"
        "(module (func (if i32.const 0 (then))))"))

     ;; Error cases: end/else delimit control syntax and cannot be folded instructions.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func (end)))" "(module (func (else)))"))

     ;; Error cases: canonical hyphens are not WAT's underscore mnemonic spellings.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func block i32.const 0 br-if 0 end))"
        "(module (memory 1) (func (i32.load8-s (i32.const 0))))")))

(mat wasm-review-source-abbreviations

     (= 0 (vector-length
           (wasm-struct-type-fields
            (wasm-subtype-composite-type
             (vector-ref (wasm-recursive-type-subtypes
                          (vector-ref (wasm-module-types
                                       (parse-wasm-text-module
                                        "(module (type (struct (field))))")) 0)) 0)))))

     (= 1 (vector-length (wasm-module-functions (parse-wasm-text-module "(func)"))))

     (= 0 (vector-length (wasm-module-functions (parse-wasm-text-module ";; empty"))))

     ;; Error case: omitting the wrapper does not admit WebAssembly script commands.
     (wasm-review-error? (lambda () (parse-wasm-text-module "(func) (invoke \"f\")"))))

(mat wasm-review-bare-module-file

     (with-parser-file
      (string->utf8 "(func) (func)")
      (lambda (path)
        (= 2 (vector-length (wasm-module-functions (parse-wasm-text-module-file path)))))))

(mat wasm-review-anonymous-type-uses

     ;; Error cases: block and indirect-call signatures do not bind local parameter names.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func block (param $x i32) drop end))"
        "(module (func loop (param $x i32) drop end))"
        "(module (func if (param $x i32) drop end))"
        "(module (table 0 funcref) (func call_indirect (param $x i32)))"
        "(module (table 0 funcref) (func return_call_indirect (param $x i32)))")))

(mat wasm-review-import-order

     (wasm-module?
      (parse-wasm-text-module
       "(module (data \"\") (type (func)) (import \"m\" \"f\" (func)))"))

     ;; Error cases: explicit and inline imports must precede entity definitions.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func) (import \"m\" \"f\" (func)))"
        "(module (func) (func (import \"m\" \"f\")))"
        "(module (table 0 funcref) (import \"m\" \"f\" (func)))"
        "(module (memory 0) (import \"m\" \"f\" (func)))"
        "(module (global i32 i32.const 0) (import \"m\" \"f\" (func)))"
        "(module (tag) (import \"m\" \"f\" (func)))")))

(mat wasm-review-inline-type-targets

     ;; Error cases: an inline signature needs an existing function target after expansion.
     (for-all
      (lambda (text) (wasm-review-error? (lambda () (parse-wasm-text-module text))))
      '("(module (func (type 0) (param i32)))"
        "(module (type (struct)) (func (type 0) (param i32)))"
        "(module (func (type 1) (param i32)) (func (param f32)))"
        "(module (func (type 0) (param i32)) (func (param f32)))"))

     (let* ([module (parse-wasm-text-module
                     "(module (func (type 0) (param i32)) (func (param i32)))")]
            [functions (wasm-module-functions module)])
       (and (= 1 (vector-length (wasm-module-types module)))
            (equal? '(0 0) (map wasm-function-type-index (vector->list functions)))))

     ;; The indexed signature can name a type introduced by a later instruction signature.
     (let ([module (parse-wasm-text-module
                    "(module (func (type 1) (param i32)) (func block (param i32) end))")])
       (and (= 2 (vector-length (wasm-module-types module)))
            (= 1 (wasm-function-type-index (vector-ref (wasm-module-functions module) 0)))))

     ;; Structurally equal reference signatures reuse the earliest explicit function type.
     (let ([module
            (parse-wasm-text-module
             "(module (type (struct))
                (type (func (param (ref null 0))))
                (type (func (param (ref null 0))))
                (func (param (ref null 0))) (func (param (ref null 0))))")])
       (and (= 3 (vector-length (wasm-module-types module)))
            (equal? '(1 1)
                    (map wasm-function-type-index
                         (vector->list (wasm-module-functions module)))))))

(define wasm-review-data-module
  (lambda (instructions count?)
    (let ([body (parser-test-bytes #vu8(0) instructions #vu8(#x0b))])
      (make-wasm-module-bytes
       ;; One function type and one mutable i32 array type.
       (make-wasm-section-bytes 1 #vu8(2 #x60 0 0 #x5e #x7f 1))
       (make-wasm-section-bytes 3 #vu8(1 0))
       (make-wasm-section-bytes 5 #vu8(1 0 0))
       (if count? (make-wasm-section-bytes 12 #vu8(1)) #vu8())
       (make-wasm-section-bytes
        10 (parser-test-bytes #vu8(1) (encode-wasm-u32 (bytevector-length body)) body))
       (make-wasm-section-bytes 11 #vu8(1 1 0))))))

(define wasm-review-data-instructions
  (list
   #vu8(#x41 0 #x41 0 #x41 0 #xfc 8 0 0) ; memory.init
   #vu8(#xfc 9 0) ; data.drop
   #vu8(#x41 0 #x41 0 #xfb 9 1 0 #x1a) ; array.new_data, drop
   #vu8(#xd0 1 #x41 0 #x41 0 #x41 0 #xfb 18 1 0) ; array.init_data
   #vu8(2 #x40 #xfc 9 0 #x0b) ; block containing data.drop
   #vu8(3 #x40 #xfc 9 0 #x0b) ; loop containing data.drop
   #vu8(#x41 0 4 #x40 5 #xfc 9 0 #x0b) ; if alternate containing data.drop
   #vu8(#x1f #x40 0 #xfc 9 0 #x0b))) ; try_table containing data.drop

(mat wasm-review-required-data-count

     (for-all
      (lambda (instructions)
        (wasm-module? (parse-wasm-binary-module (wasm-review-data-module instructions #t))))
      wasm-review-data-instructions)

     ;; Error cases: data-index instructions need data-count, including nested bodies/alternates.
     (for-all
      (lambda (instructions)
        (wasm-review-error?
         (lambda () (parse-wasm-binary-module (wasm-review-data-module instructions #f)))))
      wasm-review-data-instructions)

     ;; Ordinary code and a data section do not themselves require a data-count declaration.
     (wasm-module? (parse-wasm-binary-module (wasm-review-data-module #vu8(1) #f))))

(mat wasm-review-implicit-type-eligibility

     (for-all
      (lambda (case)
        (let ([module (parse-wasm-text-module (car case))])
          (and (= (cadr case)
                  (wasm-function-type-index (vector-ref (wasm-module-functions module) 0)))
               (= (caddr case) (vector-length (wasm-module-types module))))))
      '(("(module (type (func)) (func))" 0 1)
        ("(module (rec (type (func))) (func))" 0 1)
        ("(module (type (sub final (func))) (func))" 0 1)
        ("(module (type (sub (func))) (func))" 1 2)
        ("(module (rec (type (func)) (type (func))) (func))" 2 2)
        ("(module (type $b (sub (func))) (type (sub final $b (func))) (func))" 2 3))))

(mat wasm-review-implicit-type-source-order

     (let* ([module
             (parse-wasm-text-module
              "(module (func i32.const 0 block (param i32) drop end) (func (param f32)))")]
            [functions (wasm-module-functions module)]
            [block (vector-ref (wasm-function-body (vector-ref functions 0)) 1)])
       (and (equal? '(0 2) (map wasm-function-type-index (vector->list functions)))
            (= 1 (wasm-block-type-value (vector-ref (wasm-instruction-immediates block) 0)))))

     (let* ([module
             (parse-wasm-text-module
              "(module (func i32.const 0 block (param i32) drop
                 f32.const 0 block (param f32) drop end end))")]
            [outer (vector-ref (wasm-function-body
                                (vector-ref (wasm-module-functions module) 0)) 1)]
            [inner (vector-ref (wasm-instruction-body outer) 2)])
       (and (= 1 (wasm-block-type-value (vector-ref (wasm-instruction-immediates outer) 0)))
            (= 2 (wasm-block-type-value (vector-ref (wasm-instruction-immediates inner) 0)))))

     ;; Folded headers precede their operands in source, although execution order is reversed.
     (let* ([module
             (parse-wasm-text-module
              "(module (table 0 funcref) (func f32.const 0
                 (call_indirect (param i32)
                   (block (param f32) (result i32) drop i32.const 0) (i32.const 0))))")]
            [body (wasm-function-body (vector-ref (wasm-module-functions module) 0))]
            [block (vector-ref body 1)]
            [call (vector-ref body 3)])
       (and (= 1 (vector-ref (wasm-instruction-immediates call) 0))
            (= 2 (wasm-block-type-value (vector-ref (wasm-instruction-immediates block) 0))))))

(define wasm-review-element-type
  (lambda (module)
    (wasm-element-reference-type (vector-ref (wasm-module-elements module) 0))))

(mat wasm-review-indexed-element-types

     (for-all
      (lambda (source)
        (let ([type (wasm-review-element-type (parse-wasm-text-module source))])
          (and (not (wasm-reference-type-nullable? type))
               (eq? 'func (wasm-reference-type-heap-type type)))))
      '("(module (elem func))"
        "(module (elem declare func))"
        "(module (elem (i32.const 0)))"
        "(module (elem (table 0) (i32.const 0) func))"))

     (for-all
      (lambda (payload)
        (not (wasm-reference-type-nullable?
              (wasm-review-element-type
               (parse-wasm-binary-module
                (make-wasm-module-bytes (make-wasm-section-bytes 9 payload)))))))
      '(#vu8(1 0 65 0 11 0) #vu8(1 1 0 0)
        #vu8(1 2 0 65 0 11 0 0) #vu8(1 3 0 0)))

     ;; Expression-list flag 4 has a nullable default, unlike indexed lists.
     (wasm-reference-type-nullable?
      (wasm-review-element-type
       (parse-wasm-binary-module
        (make-wasm-module-bytes (make-wasm-section-bytes 9 #vu8(1 4 65 0 11 0)))))))

(mat wasm-review-element-keyword

     ;; Error case: a passive indexed list requires func, even when empty.
     (wasm-review-error? (lambda () (parse-wasm-text-module "(module (elem))")))

     ;; Error case: a declarative indexed list requires func.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module "(module (elem declare 0))")))

     ;; Error case: an explicit table use disables the legacy omission of func.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  "(module (elem (table 0) (i32.const 0) 0))")))

     (wasm-module? (parse-wasm-text-module "(module (elem (i32.const 0) 0))"))

     (wasm-module? (parse-wasm-text-module "(module (elem (offset i32.const 0)))")))

(define wasm-review-lanes
  (lambda (shape lane count)
    (format "(module (func v128.const ~a ~a~a drop))"
            shape lane (apply string-append (make-list (fx1- count) " 0")))))

(mat wasm-review-narrow-lanes

     ;; Error cases: wide positive decimal/hex literals cannot masquerade as negative lanes.
     (for-all
      (lambda (lane)
        (wasm-review-error?
         (lambda () (parse-wasm-text-module (wasm-review-lanes "i8x16" lane 16)))))
      '("4294967295" "+4294967295" "0xffffff80" "256" "-129"))

     ;; Error cases: the same source-sign loss affects sixteen-bit lane validation.
     (for-all
      (lambda (lane)
        (wasm-review-error?
         (lambda () (parse-wasm-text-module (wasm-review-lanes "i16x8" lane 8)))))
      '("4294967295" "0xffff8000" "65536" "-32769"))

     (for-all
      (lambda (case)
        (let* ([module (parse-wasm-text-module
                        (wasm-review-lanes (car case) (cadr case) (caddr case)))]
               [body (wasm-function-body (vector-ref (wasm-module-functions module) 0))]
               [bytes (vector-ref (wasm-instruction-immediates (vector-ref body 0)) 0)])
          (= (cadddr case)
             (if (= 16 (caddr case)) (bytevector-u8-ref bytes 0)
                 (bytevector-u16-ref bytes 0 (endianness little))))))
      '(("i8x16" "-128" 16 128) ("i8x16" "255" 16 255)
        ("i16x8" "-32768" 8 32768) ("i16x8" "65535" 8 65535))))

(define wasm-review-custom
  (lambda (source)
    (vector-ref (wasm-module-custom-sections (parse-wasm-text-module source)) 0)))

(mat wasm-review-custom-annotation-names

     (for-all
      (lambda (source)
        (let ([custom (wasm-review-custom source)])
          (and (string=? "x" (wasm-custom-section-name custom))
               (equal? #vu8(121) (wasm-custom-section-bytes custom)))))
      '("(module (@custom \"x\" \"y\"))"
        "(module (@\"custom\" \"x\" \"y\"))"
        "(module (@\"cu\\73tom\" \"x\" \"y\"))"
        "(module (@custom\"x\" \"y\"))"))

     ;; Error case: an annotation head has no space between its opening parenthesis and @.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module "(module ( @custom \"x\"))"))))

(mat wasm-review-custom-anchors

     (for-all
      (lambda (case)
        (eq? (cadr case)
             (wasm-custom-section-after-section (wasm-review-custom (car case)))))
      '(("(module (func) (@custom \"x\" (after func)))" function)
        ("(module (func data.drop 0) (data \"x\")
            (@custom \"x\" (after datacount)))" data-count)
        ("(module (func) (@custom \"x\" (before first)))" #f)
        ("(module (func) (@custom \"x\" (after first)))" type)
        ("(module (data \"x\") (@custom \"x\" (after last)))" data)
        ("(module (data \"x\") (@custom \"x\" (before last)))" code)
        ("(module (func data.drop 0) (data \"x\")
            (@custom \"x\" (before code)))" data-count)
        ("(module (func data.drop 0) (data \"x\")
            (@custom \"x\" (before datacount)))" element)))

     ;; Error case: function is an internal section name, not the WAT anchor spelling func.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  "(module (func) (@custom \"x\" (after function)))"))))

(mat wasm-review-custom-default

     (for-all
      (lambda (source)
        (eq? 'data (wasm-custom-section-after-section (wasm-review-custom source))))
      '("(module (@custom \"x\" \"y\") (data \"z\"))"
        "(module (func) (@custom \"x\" \"y\") (data \"z\"))"
        "(module (@\"custom\" \"x\" \"y\"))"))

     (not (wasm-custom-section-after-section
           (wasm-review-custom "(module (@custom \"x\" (before first)))"))))

(mat wasm-review-signed-decoder-reuse

     (let* ([count 10000] [bytes (make-bytevector (* count 5) 255)]
            [parser (<rep> <wasm-s32> count)])
       (do ([i 4 (fx+ i 5)]) ((fx>= i (bytevector-length bytes)))
         (bytevector-u8-set! bytes i 127))
       (let* ([before (statistics)]
              [values (run-binary-parser parser bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= count (length values))
              (for-all (lambda (value) (= value -1)) values)
              (< allocated 2000000)))))

(mat wasm-review-string-runs

     (equal? (parser-test-bytes
              (string->utf8 "aλ😀") #vu8(255 0 34 92 10)
              (string->utf8 "z\xfeff;"))
             (parse-wat-lexeme <wat-string> "\"aλ😀\\ff\\00\\\"\\\\\\nz\\u{feff}\""))

     ;; Error case: byte strings can contain invalid UTF-8, but names cannot.
     (wasm-review-error? (lambda () (parse-wat-lexeme <wat-name> "\"abc\\ffdef\"")))

     ;; Error case: batching must still reject literal control characters.
     (wasm-review-error?
      (lambda () (parse-wat-lexeme <wat-string> "\"abc\tdef\"")))

     (let* ([count 16384] [source (string-append "\"" (make-string count #\a) "\"")]
            [before (statistics)]
            [bytes (parse-wat-lexeme <wat-string> source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (bytevector=? bytes (make-bytevector count 97)) (< allocated 6000000))))

(mat wasm-review-bounded-integer-cost

     ;; Error case: an overflowing decimal token is scanned without growing an unbounded bignum.
     (let* ([source (make-string 16000 #\9)]
            [before (statistics)]
            [err (parser-error-from (lambda () (run-textual-parser <wat-u32> source)))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (parser-error? err) (= 16000 (parser-error-offset err)) (< allocated 12000000)))

     (= 1 (run-textual-parser <wat-u32> (string-append (make-string 16000 #\0) "1")))

     (equal? '(4294967295 18446744073709551615 2147483648 9223372036854775808)
             (list (run-textual-parser <wat-u32> "4_294_967_295")
                   (run-textual-parser <wat-u64> "0xffff_ffff_ffff_ffff")
                   (run-textual-parser <wat-i32> "-2_147_483_648")
                   (run-textual-parser <wat-i64> "-0x8000_0000_0000_0000")))

     ;; Error cases: signed and unsigned limits still reject overflow at their own boundaries.
     (for-all
      (lambda (case)
        (parser-error?
         (parser-error-from (lambda () (run-textual-parser (car case) (cdr case))))))
      (list (cons <wat-u32> "4294967296") (cons <wat-u64> "0x10000000000000000")
            (cons <wat-i32> "-2147483649") (cons <wat-i64> "-9223372036854775809"))))

(mat wasm-review-long-type-signatures

     (let* ([fixed (apply string-append (make-list 64 " i32"))]
            [module (parse-wasm-text-module
                     (string-append
                      "(module (type (struct)) (type (struct))"
                      "(func (param" fixed " (ref 0)))"
                      "(func (param" fixed " (ref 1)))"
                      "(func (param" fixed " (ref 0))))"))]
            [functions (wasm-module-functions module)])
       (and (= 4 (vector-length (wasm-module-types module)))
            (equal? '(2 3 2) (map wasm-function-type-index (vector->list functions))))))

(mat wasm-review-unsigned-decoder-cost

     (let* ([count 10000] [bytes (make-bytevector (* count 5) #x80)]
            [parser (<rep> <wasm-u32> count)])
       (do ([i 4 (fx+ i 5)]) ((fx>= i (bytevector-length bytes)))
         (bytevector-u8-set! bytes i 1))
       (let* ([before (statistics)] [values (run-binary-parser parser bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= count (length values))
              (for-all (lambda (value) (= value #x10000000)) values)
              (< allocated 2000000)))))

(mat wasm-review-limit-widths

     ;; Grammar controls: wide i32-address bounds are parsed without full size validation.
     (for-all
      (lambda (kind)
        (for-all
         (lambda (address)
           (let* ([table? (string=? kind "table")]
                  [module (parse-wasm-text-module
                           (format "(module (~a ~a 0 18446744073709551615~a))"
                                   kind address (if table? " funcref" "")))]
                  [entity (vector-ref (if table? (wasm-module-tables module)
                                          (wasm-module-memories module)) 0)]
                  [limits (if table?
                              (wasm-table-type-limits (wasm-table-type entity))
                              (wasm-memory-type-limits (wasm-memory-type entity)))])
             (and (= 0 (wasm-limits-minimum limits))
                  (= #xffffffffffffffff (wasm-limits-maximum limits))
                  (eq? (if (string=? address "i64") 'i64 'i32)
                       (wasm-limits-address-type limits)))))
         '("" "i32" "i64")))
      '("table" "memory"))

     (let* ([module (parse-wasm-text-module
                     "(module (import \"m\" \"t\" (table i32 4294967296 funcref))
                              (import \"m\" \"m\" (memory 0 4294967296)))")]
            [imports (wasm-module-imports module)]
            [table (wasm-import-external-type (vector-ref imports 0))]
            [memory (wasm-import-external-type (vector-ref imports 1))])
       (and (= 4294967296 (wasm-limits-minimum
                          (wasm-table-type-limits (wasm-external-type-type table))))
            (= 4294967296 (wasm-limits-maximum
                          (wasm-memory-type-limits (wasm-external-type-type memory))))))

     ;; Error case: both address types still reject values beyond u64.
     (for-all
      (lambda (address)
        (wasm-review-error?
         (lambda () (parse-wasm-text-module
                     (format "(module (memory ~a 18446744073709551616))" address)))))
      '("i32" "i64")))

(mat wasm-review-bounded-payload-cost

     ;; Error cases: Unicode scalars and NaN payloads stop growing after their fixed limits.
     (for-all
      (lambda (entry)
        (let* ([source (cdr entry)] [before (statistics)]
               [err (parser-error-from
                     (lambda () (run-textual-parser (car entry) source)))]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (parser-error? err) (< allocated 12000000))))
      (let ([digits (make-string 16000 #\f)])
        (list (cons <wat-string> (string-append "\"\\u{" digits "}\""))
              (cons <wat-f32> (string-append "nan:0x" digits))
              (cons <wat-f64> (string-append "nan:0x" digits)))))

     (let ([zeros (make-string 16000 #\0)])
       (and (bytevector=? (string->utf8 (string #\x10ffff))
                         (parse-wat-lexeme <wat-string>
                                          (string-append "\"\\u{" zeros "_10ffff}\"")))
            (= #x7fffffff (wasm-float-bits
                           (parse-wat-lexeme <wat-f32>
                                            (string-append "nan:0x" zeros "_7fffff"))))
            (= #xffffffffffffffff
               (wasm-float-bits (parse-wat-lexeme <wat-f64>
                                                (string-append "-nan:0x" zeros
                                                               "_fffffffffffff")))))))

(mat wasm-review-long-exponent-digits

     ;; The full exponent must be scanned, but a zero significand needs no giant bignum.
     (let* ([text (string-append "-0e-" (make-string 16000 #\9))]
            [before (statistics)]
            [bits (wasm-review-float-bits <wat-f64> text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= #x8000000000000000 bits) (< allocated 20000000)))

     ;; Error case: a nonzero significand with a huge positive exponent still overflows.
     (wasm-review-error?
      (lambda () (wasm-review-float-bits
                  <wat-f64> (string-append "1e" (make-string 1000 #\9)))))

     ;; Exponent zero prefixes, separators and mantissa cancellation remain exact.
     (for-all
      (lambda (parser)
        (let ([one (wasm-review-float-bits parser "1")])
          (and (= one (wasm-review-float-bits
                       parser (string-append "1e+" (make-string 1000 #\0))))
               (= one (wasm-review-float-bits
                       parser (string-append "0." (make-string 1000 #\0) "1e1_001")))
               (= one (wasm-review-float-bits
                       parser (string-append "0x1" (make-string 1000 #\0) "p-4_000"))))))
      (list <wat-f32> <wat-f64>)))

(mat wasm-review-label-scopes

     ;; Named and numeric labels must produce identical canonical modules, including catches.
     (for-all
      (lambda (sources)
        (string=? (format "~s" (parse-wasm-text-module (car sources)))
                  (format "~s" (parse-wasm-text-module (cadr sources)))))
      '(("(module (func block $x block $x br $x end br $x end))"
         "(module (func block block br 0 end br 0 end))")
        ("(module (func block $x block block br $x end end br $x end))"
         "(module (func block block block br 2 end end br 0 end))")
        ("(module (func block $x if $y block $x br $x end br $x
                        else br $y br $x end br $x end))"
         "(module (func block if block br 0 end br 1 else br 0 br 1 end br 0 end))")
        ("(module (func block $x try_table $x (catch_all $x) br $x end br $x end))"
         "(module (func block try_table (catch_all 0) br 0 end br 0 end))")
        ("(module (func block $x block $y br_table $x $y $x end end))"
         "(module (func block block br_table 1 0 1 end end))")
        ("(module (func block $x block $y br_on_cast $x (ref null any) (ref any) end end))"
         "(module (func block block br_on_cast 1 (ref null any) (ref any) end end))")
        ("(module (func (block $x (br $x)) (block $x (br $x))))"
         "(module (func (block (br 0)) (block (br 0))))")))

     ;; Error case: a label must disappear after its block ends.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module "(module (func block $gone end br $gone))")))

     ;; Error case: a prior function's labels must not leak into another function.
     (wasm-review-error?
      (lambda () (parse-wasm-text-module
                  "(module (func block $gone end) (func br $gone))"))))

(mat wasm-review-shared-float-prefix

     ;; The finite value uses exact cancellation; repeated significand conversion wastes memory.
     (for-all
      (lambda (text expected)
        (let* ([before (statistics)] [bits (wasm-review-float-bits <wat-f64> text)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (= expected bits) (< allocated 160000000))))
      (list (string-append (make-string 16000 #\1) "e-16000")
            (string-append "0x" (make-string 16000 #\1) "p-64000"))
      '(4592670820000712476 4589468260265693457)))

(mat wasm-review-binary-float-allocation

     (for-all
      (lambda (parser width)
        (let* ([count 100000] [bytes (make-bytevector (* count width) 0)]
               [repeated (<rep> parser count)] [before (statistics)]
               [result (run-binary-parser repeated bytes)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (= count (length result))
               (for-all (lambda (value) (= 0 (wasm-float-bits value))) result)
               (< allocated 9000000))))
      (list <wasm-f32> <wasm-f64>) '(4 8)))

(mat wasm-review-fixed-instruction-parsers

     (let* ([count 100000] [bytes (make-bytevector (fx1+ count) 1)])
       (bytevector-u8-set! bytes count 11)
       (let* ([before (statistics)] [body (run-binary-parser <wasm-expression> bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= count (vector-length body))
              (eq? 'nop (wasm-instruction-mnemonic (vector-ref body (fx1- count))))
              (not (eq? (vector-ref body 0) (vector-ref body 1)))
              (< allocated 38000000))))

     ;; Exact binary variants have different immediates despite sharing their WAT mnemonic.
     (for-all
      (lambda (bytes)
        (vector? (run-binary-parser <wasm-expression> bytes)))
      '(#vu8(27 11) #vu8(251 21 110 11) #vu8(251 23 110 11))))

(mat wasm-review-inline-element-lists

     ;; Normative element lists have a type distinct from the enclosing table's type.
     (for-all
      (lambda (sources)
        (string=? (format "~s" (parse-wasm-text-module (car sources)))
                  (format "~s" (parse-wasm-text-module (cadr sources)))))
      '(("(module (func $f) (table funcref (elem func $f)))"
         "(module (func $f) (table 1 1 funcref)
                  (elem (table 0) (i32.const 0) func $f))")
        ("(module (table externref (elem externref (ref.null extern))))"
         "(module (table 1 1 externref)
                  (elem (table 0) (i32.const 0) externref (ref.null extern)))")
        ("(module (table i64 funcref (elem func)))"
         "(module (table i64 0 0 funcref) (elem (table 0) (i64.const 0) func))")
        ("(module (table externref (elem externref)))"
         "(module (table 0 0 externref) (elem (table 0) (i32.const 0) externref))")
        ("(module (import \"m\" \"t\" (table 0 funcref)) (func $f)
                  (table $t (export \"t\") funcref (elem (ref func) (item ref.func $f))))"
         "(module (import \"m\" \"t\" (table 0 funcref)) (func $f)
                  (table $t (export \"t\") 1 1 funcref)
                  (elem (table $t) (i32.const 0) (ref func) (item ref.func $f)))")))

     ;; Preserve the existing bare-index, bare-expression and empty abbreviations.
     (for-all
      (lambda (text)
        (wasm-module? (parse-wasm-text-module text)))
      '("(module (func $f) (table funcref (elem $f)))"
        "(module (table externref (elem (ref.null extern))))"
        "(module (table funcref (elem)))")))

(mat wasm-review-counted-vector-allocation

     ;; Known counts can fill the vector without retaining one temporary pair per element.
     (let* ([count 100000] [bytes (make-bytevector (+ count 3) 0)]
            [parser (<wasm-vector> <wasm-u32>)])
       (bytevector-u8-set! bytes 0 #xa0)
       (bytevector-u8-set! bytes 1 #x8d)
       (bytevector-u8-set! bytes 2 #x06)
       (let* ([before (statistics)] [result (run-binary-parser parser bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= count (vector-length result)) (vfor-all zero? result)
              (< allocated 1800000))))

     ;; Public element parsers may consume zero bytes, even when count exceeds remaining input.
     (equal? '#(7 7 7) (run-binary-parser (<wasm-vector> (<result> 7)) #vu8(3)))

     (equal? '(#(7 7 7) 1)
             (run-binary-parser (<~> (<wasm-vector> (<result> 7)) <pos>) #vu8(3 0 0 0)))

     ;; Error case: a huge count on truncated input must fail before preallocating its result.
     (wasm-review-error?
      (lambda () (run-binary-parser (<wasm-vector> <u8>) #vu8(255 255 255 255 15))))

     ;; Error case: a truncated multibyte element keeps the actual failure offset.
     (let ([failure (parser-error-from
                     (lambda () (run-binary-parser (<wasm-vector> <wasm-u32>)
                                                  #vu8(2 1 128))))])
       (and failure (= 3 (parser-error-offset failure))))

     (equal? '(9 #(0 1 2) 8)
             (run-binary-parser (<~> <u8> (<bounded> 4 (<wasm-vector> <wasm-u32>)) <u8>)
                                #vu8(9 3 0 1 2 8)))

     ;; Vectors and mutable element values belong to the invocation.
     (let* ([parser (<wasm-vector> (<u8vec> 1))]
            [first (run-binary-parser parser #vu8(2 8 9))]
            [second (run-binary-parser parser #vu8(2 8 9))])
       (bytevector-u8-set! (vector-ref first 0) 0 42)
       (vector-set! first 1 #vu8(0))
       (and (= 8 (bytevector-u8-ref (vector-ref second 0) 0))
            (= 9 (bytevector-u8-ref (vector-ref second 1) 0)))))

(mat wasm-review-text-immediate-results

     ;; br_table preserves target order, final default, source positions and immutable vectors.
     (for-all
      (lambda (source targets default)
        (let* ([instruction (parse-wat-lexeme <wat-instruction> source)]
               [immediates (wat-instruction-syntax-immediates instruction)]
               [actual (vector-ref immediates 0)])
          (and (equal? targets
                       (map wat-index-reference-value (vector->list actual)))
               (equal? default (wat-index-reference-value (vector-ref immediates 1)))
               (= 9 (wat-index-reference-pos
                     (if (zero? (vector-length actual)) (vector-ref immediates 1)
                         (vector-ref actual 0))))
               (or (zero? (vector-length actual)) (immutable-vector? actual)))))
      '("br_table 2" "br_table 2 0 1" "br_table $a $b $a")
      '(() (2 0) ("$a" "$b")) '(2 1 "$a"))

     ;; Cached parser compositions must not share mutable v128 results.
     (let* ([source "v128.const i32x4 1 2 3 4"]
            [first (parse-wat-lexeme <wat-instruction> source)]
            [second (parse-wat-lexeme <wat-instruction> source)]
            [bytes (vector-ref (wat-instruction-syntax-immediates first) 0)])
       (bytevector-u8-set! bytes 0 42)
       (= 1 (bytevector-u8-ref
             (vector-ref (wat-instruction-syntax-immediates second) 0) 0))))

(mat wasm-review-short-value-type-cost

     (for-all
      (lambda (byte)
        (let* ([bytes (make-bytevector 100000 byte)]
               [parser (<rep> <wasm-value-type> 100000)]
               [before (statistics)] [types (run-binary-parser parser bytes)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (= 100000 (length types)) (< allocated 6000000))))
      '(#x7f #x7c #x7b #x70))

     (let ([first (run-binary-parser <wasm-value-type> #vu8(#x70))]
           [second (run-binary-parser <wasm-value-type> #vu8(#x70))])
       (and (not (eq? first second))
            (wasm-reference-type-nullable? first)
            (eq? 'func (wasm-reference-type-heap-type first))))

     ;; Error case: unknown value-type tags retain structured diagnostics.
     (parser-rejects? <wasm-value-type> #vu8(#x65))

     ;; Error case: an empty bounded range cannot read the surrounding known type byte.
     (parser-rejects? (<bounded> 0 <wasm-value-type>) #vu8(#x7f)))

(mat wasm-review-balanced-significands

     (for-all
      (lambda (prefix radix)
        (let* ([source (string-append prefix (make-string 16000 #\1)
                                     (if (= radix 10) "e-16000" "p-64000"))]
               [before (statistics)] [number (parse-wat-lexeme <wat-f64> source)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (= (if (= radix 10) 4592670820000712476 4589468260265693457)
                  (wasm-float-bits number))
               (< allocated 14000000))))
      '("" "0x") '(10 16)))

(mat wasm-review-identifier-punctuation-cost

     (let* ([source (string-append "$" (make-string 100000 #\!))]
            [before (statistics)] [identifier (parse-wat-lexeme <wat-identifier> source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? source identifier) (< allocated 11000000))))

;;;; Allocation and value ownership regressions

(mat parser-wasm-sequence-lookahead-cost

     (let ([bytes (make-bytevector 100001 1)])
       (bytevector-u8-set! bytes 100000 11)
       (let* ([before (statistics)] [body (run-binary-parser <wasm-expression> bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= 100000 (vector-length body))
              (eq? 'nop (wasm-instruction-mnemonic (vector-ref body 99999)))
              (< allocated 28000000)))))

(mat parser-wasm-reused-limits

     ;; Repeated declarations must reuse parser compositions while keeping each value fresh.
     (let* ([count 10000] [bytes (make-bytevector (+ 14 (* count 2)) 0)])
       (bytevector-copy! #vu8(0 97 115 109 1 0 0 0 5 162 156 1 144 78) 0 bytes 0 14)
       (let* ([before (statistics)] [module (parse-wasm-binary-module bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))]
              [memories (wasm-module-memories module)])
         (and (= count (vector-length memories))
              (not (eq? (vector-ref memories 0) (vector-ref memories 1)))
              (< allocated 1700000))))

     ;; Error case: a cached parser must still reject a maximum smaller than its minimum.
     (parser-error? (parser-error-from
                    (lambda () (parse-wasm-binary-module
                                #vu8(0 97 115 109 1 0 0 0 5 4 1 1 2 1)))))

     (let* ([module (parse-wasm-binary-module
                     #vu8(0 97 115 109 1 0 0 0 5 7 2 3 1 2 7 3 4))]
            [memories (wasm-module-memories module)])
       (and (= 2 (vector-length memories))
            (wasm-limits-shared? (wasm-memory-type-limits (wasm-memory-type (vector-ref memories 0))))
            (eq? 'i64 (wasm-limits-address-type
                       (wasm-memory-type-limits (wasm-memory-type (vector-ref memories 1))))))))

(mat parser-wasm-standalone-reference-dispatch

     (let* ([count 10000] [bytes (make-bytevector (+ 14 (* count 4)) 0)])
       (bytevector-copy! #vu8(0 97 115 109 1 0 0 0 4 194 184 2 144 78) 0 bytes 0 14)
       (do ([index 14 (+ index 4)]) ((= index (bytevector-length bytes)))
         (bytevector-u8-set! bytes index 112)
         (bytevector-u8-set! bytes (+ index 1) 1))
       (let* ([before (statistics)] [module (parse-wasm-binary-module bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= count (vector-length (wasm-module-tables module)))
              (< allocated 11000000))))

     (for-all
      (lambda (byte name)
        (let* ([input (bytevector byte)]
               [first (run-binary-parser <wasm-reference-type> input)]
               [second (run-binary-parser <wasm-reference-type> input)])
          (and (wasm-reference-type-nullable? first)
               (eq? name (wasm-reference-type-heap-type first))
               (not (eq? first second)))))
      '(105 106 107 108 109 110 111 112 113 114 115 116)
      '(exn array struct i31 eq any extern func none noextern nofunc noexn))

     (let ([type (run-binary-parser <wasm-reference-type> #vu8(100 0))])
       (and (not (wasm-reference-type-nullable? type))
            (= 0 (wasm-reference-type-heap-type type))))

     ;; Error case: an out-of-range shorthand byte must not be read through a bounded cursor.
     (let ([err (parser-error-from
                 (lambda ()
                   (run-binary-parser/source (<bounded> 0 <wasm-reference-type>)
                                             #vu8(112) "reference-range")))])
       (and err (= 0 (parser-error-offset err))
            (string=? "reference-range" (parser-error-source err)))))

(mat parser-wat-ordinary-annotation-content

     (let* ([body (make-string 100000 #\a)]
            [source (string-append "(@foo " body ")(module)")]
            [before (statistics)] [module (parse-wasm-text-module source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= 0 (vector-length (wasm-module-functions module)))
            (< allocated 25000000)))

     (= 1 (vector-length
           (wasm-module-functions
            (parse-wasm-text-module
             "(@\"foo\" a (b \"c\") (;d(;e;)f;))(module (func nop))"))))

     ;; Error cases: ordinary content cannot hide missing parentheses, strings or comments.
     (for-all
      (lambda (text) (and (parser-error-from (lambda () (parse-wasm-text-module text))) #t))
      '("(@foo abc" "(@foo abc (def)" "(@foo abc \"def)" "(@foo abc (;def)")))

(mat parser-wasm-immediate-vector-cost

     (let* ([count 100000] [bytes (make-bytevector (* 2 count) 0)]
            [parser (<rep> <wasm-instruction> count)])
       (do ([index 0 (+ index 2)]) ((= index (bytevector-length bytes)))
         (bytevector-u8-set! bytes index #x41))
       (let* ([before (statistics)] [instructions (run-binary-parser parser bytes)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))]
              [first (car instructions)] [last (list-ref instructions (- count 1))])
         (and (= count (length instructions))
              (eq? 'i32.const (wasm-instruction-mnemonic first))
              (equal? '#(0) (wasm-instruction-immediates first))
              (immutable-vector? (wasm-instruction-immediates first))
              (not (eq? (wasm-instruction-immediates first)
                        (wasm-instruction-immediates last)))
              (< allocated 12500000)))))

(mat wasm-review-zero-integral-fraction-cost

     (let* ([source (string-append "0." (make-string 64000 #\0))]
            [before (statistics)] [value (parse-wat-lexeme <wat-f64> source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= 0 (wasm-float-bits value)) (< allocated 16000000)))

     (equal? '(0 0 #x8000000000000000 #x8000000000000000 #x3fe0000000000000)
             (map (lambda (source) (wasm-review-float-bits <wat-f64> source))
                  '("0.0" "0x0.000p0" "-0.00" "-0x0.0p0" "0x0.8p0"))))

(mat wasm-review-early-section-order

     ;; Error cases: an earlier duplicate wins over malformed duplicate or later payloads.
     (for-all
      (lambda (bytes)
        (let ([failure (parser-error-from (lambda () (parse-wasm-binary-module bytes)))])
          (and failure (= 11 (parser-error-offset failure))
               (string=? "<fail-with>: duplicate or out-of-order WebAssembly section"
                         (parser-error-message failure)))))
      '(#vu8(0 97 115 109 1 0 0 0 1 1 0 1)
        #vu8(0 97 115 109 1 0 0 0 1 1 0 1 1 0 0 2 0)))

     ;; Error case: an out-of-order type ID wins over its missing size field.
     (let ([failure
            (parser-error-from
             (lambda ()
               (parse-wasm-binary-module #vu8(0 97 115 109 1 0 0 0 2 1 0 1))))])
       (and failure (= 11 (parser-error-offset failure))
            (string=? "<fail-with>: duplicate or out-of-order WebAssembly section"
                      (parser-error-message failure))))

     ;; Error case: a duplicate must stop before copying an unrelated 1 MiB custom payload.
     (let* ([bytes (make-bytevector (+ 19 1048576) 0)]
            [prefix #vu8(0 97 115 109 1 0 0 0 1 1 0 1 1 0 0 129 128 64 0)])
       (bytevector-copy! prefix 0 bytes 0 (bytevector-length prefix))
       (let* ([before (statistics)]
              [failure (parser-error-from (lambda () (parse-wasm-binary-module bytes)))]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and failure (= 11 (parser-error-offset failure)) (< allocated 100000))))

     ;; Custom sections may repeat and do not reset the previous standard-section rank.
     (wasm-module?
      (parse-wasm-binary-module
       #vu8(0 97 115 109 1 0 0 0 0 1 0 1 1 0 0 1 0 0 1 0 2 1 0))))

(mat wasm-review-delimited-expression-diagnostics

     ;; Error cases: missing immediates in offsets, items, and branches retain the inner error.
     (for-all
      (lambda (test)
        (let ([failure
               (parser-error-from (lambda () (parse-wasm-text-module (car test))))])
          (and failure (= (cadr test) (parser-error-offset failure))
               (string=? "not a digit" (parser-error-message failure)))))
      '(("(module (memory 1) (data (offset i32.const) \"x\"))" 42)
        ("(module (memory 1) (data (offset i32.const 0 i32.const) \"x\"))" 54)
        ("(module (func (if (then i32.const))))" 33)
        ("(module (func (if (then (i32.const)))))" 34)
        ("(module (func (if (then) (else i32.const))))" 40)
        ("(module (func (if (then) (else (i32.const)))))" 41)
        ("(module (memory 1) (data (memory 0) (offset i32.const) \"x\"))" 53)
        ("(module (memory 1) (data (i32.const) \"x\"))" 35)
        ("(module (table 1 funcref) (elem funcref (item i32.const)))" 55)
        ("(module (table 1 funcref) (elem (offset i32.const) func))" 49)
        ("(module (table 1 funcref) (elem funcref (item ref.null func) (item i32.const)))" 76)
        ("(module (table funcref (elem (item ref.null func) (item i32.const))))" 65)
        ("(module (table funcref (elem funcref (item i32.const))))" 52)
        ("(module (table 1 funcref) (elem (table 0) (offset i32.const) func))" 59)
        ("(module (table 1 funcref) (elem (i32.const) func))" 42)))

     ;; Standalone expressions still accept a successful prefix before an unrelated token.
     (= 1 (vector-length (run-textual-parser <wat-expression> "nop ???")))

     (wasm-module?
      (parse-wasm-text-module "(module (func (if (then) (else))))")))

(mat wasm-review-parenthesized-passive-element-type

     (wasm-module?
      (parse-wasm-text-module
       "(module (elem (ref null func) (item ref.null func)))")))
(mat wasm-review-type-use-parameter-identifiers

     ;; Error cases: imports, tags, and functions require unique parameter bindings.
     (for-all
      (lambda (source)
        (let ([failure (parser-error-from (lambda () (parse-wasm-text-module source)))])
          (and failure
               (string-contains? (parser-error-message failure)
                                 "duplicate WebAssembly local identifier: $x")
               (= (parser-error-offset failure)
                  (string-search source "(param $x i64)")))))
      '("(module (import \"m\" \"f\" (func (param $x i32) (param $x i64))))"
        "(module (func (import \"m\" \"f\") (param $x i32) (param $x i64)))"
        "(module (type $t (func (param i32 i64))) (func (import \"m\" \"f\") (type $t) (param $x i32) (param $x i64)))"
        "(module (tag (param $x i32) (param $x i64)))"
        "(module (import \"m\" \"t\" (tag (param $x i32) (param $x i64))))"
        "(module (tag (import \"m\" \"t\") (param $x i32) (param $x i64)))"
        "(module (func (param $x i32) (param $x i64)))"))

     ;; Type definitions' parameter names are documentation; separate type uses have local scope.
     (for-all
      (lambda (source) (wasm-module? (parse-wasm-text-module source)))
      '("(module (type (func (param $x i32) (param $x i64))))"
        "(module (import \"m\" \"f\" (func (param $x i32) (param $y i64))))"
        "(module (tag (param $x i32) (param $y i64)))"
        "(module (func (param $x i32)) (func (param $x i64)))")))
(mat wasm-review-custom-placement-order

     (let* ([module
             (parse-wasm-text-module
              "(module (table 0 funcref) (memory 0)
                 (@custom \"before-memory\" (before memory) \"A\")
                 (@custom \"after-table\" (after table) \"B\"))")]
            [custom* (wasm-module-custom-sections module)])
       (and (equal? '#("after-table" "before-memory")
                    (vector-map wasm-custom-section-name custom*))
            (equal? '#(#vu8(66) #vu8(65))
                    (vector-map wasm-custom-section-bytes custom*))
            (equal? '#(table table)
                    (vector-map wasm-custom-section-after-section custom*))))

     (let ([module
            (parse-wasm-text-module
             "(module (func)
                (@custom \"late\" (after code))
                (@custom \"early\" (before type)))")])
       (equal? '#("early" "late")
               (vector-map wasm-custom-section-name (wasm-module-custom-sections module))))

     ;; Identical placements retain source order, including default and explicit after-last.
     (let ([module
            (parse-wasm-text-module
             "(module (table 0 funcref)
                (@custom \"last-a\")
                (@custom \"table-a\" (after table))
                (@custom \"last-b\" (after last))
                (@custom \"table-b\" (after table)))")])
       (equal? '#("table-a" "table-b" "last-a" "last-b")
               (vector-map wasm-custom-section-name (wasm-module-custom-sections module)))))

(mat wasm-review-custom-anchor-existence

     ;; Error cases: neither placement direction may target an absent ordinary section.
     (for-all
      (lambda (section)
        (for-all
         (lambda (direction)
           (let* ([source (format "(module (@custom \"x\" (~a ~a)))" direction section)]
                  [failure (parser-error-from (lambda () (parse-wasm-text-module source)))])
             (and failure (= 8 (parser-error-offset failure))
                  (string-contains? (parser-error-message failure)
                                    "custom placement requires a nonempty section"))))
         '(before after)))
      '(type import func table memory tag global export start elem datacount code data))

     ;; Error cases: imports alone do not emit function/code/table/memory/tag definition sections.
     (for-all
      (lambda (test)
        (wasm-review-error?
         (lambda ()
           (parse-wasm-text-module
            (format "(module ~a (@custom \"x\" (after ~a)))" (car test) (cadr test))))))
      '(("(import \"m\" \"f\" (func))" func)
        ("(import \"m\" \"f\" (func))" code)
        ("(import \"m\" \"t\" (table 0 funcref))" table)
        ("(import \"m\" \"m\" (memory 0))" memory)
        ("(import \"m\" \"t\" (tag))" tag)
        ("(data \"x\")" datacount)))

     ;; Every ordinary anchor is accepted in both directions when its section is emitted.
     (for-all
      (lambda (test)
        (for-all
         (lambda (direction)
           (wasm-module?
            (parse-wasm-text-module
             (format "(module ~a (@custom \"x\" (~a ~a)))"
                     (car test) direction (cadr test)))))
         '(before after)))
      '(("(type (func))" type)
        ("(import \"m\" \"f\" (func))" import)
        ("(func)" func)
        ("(table 0 funcref)" table)
        ("(memory 0)" memory)
        ("(tag)" tag)
        ("(global i32 (i32.const 0))" global)
        ("(func (export \"f\"))" export)
        ("(func) (start 0)" start)
        ("(elem func)" elem)
        ("(func data.drop 0)" datacount)
        ("(func)" code)
        ("(data \"\")" data)))

     ;; Inline abbreviations and implicit signatures also create anchorable sections.
     (for-all
      (lambda (source) (wasm-module? (parse-wasm-text-module source)))
      '("(module (func) (@custom \"x\" (after type)))"
        "(module (memory (data \"x\")) (@custom \"x\" (after data)))"
        "(module (table funcref (elem)) (@custom \"x\" (after elem)))"
        "(module (func (if (then data.drop 0))) (@custom \"x\" (after datacount)))"
        "(module (func (if (then) (else data.drop 0))) (@custom \"x\" (after datacount)))"))

     ;; Virtual anchors and defaults remain valid even in an empty module, including aliases.
     (for-all
      (lambda (source) (wasm-module? (parse-wasm-text-module source)))
      '("(module (@custom \"x\"))"
        "(module (@custom \"x\" (before first)))"
        "(module (@custom \"x\" (after first)))"
        "(module (@custom \"x\" (before last)))"
        "(module (@custom \"x\" (after last)))")))
