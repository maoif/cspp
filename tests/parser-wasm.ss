(import (cspp)
        (cspp parser wasm)
        (cspp parser wasm opcodes)
        (cspp parser wasm binary values)
        (cspp parser wasm binary types)
        (cspp parser wasm binary instructions)
        (cspp parser wasm text lexical)
        (cspp parser wasm text types)
        (cspp parser wasm text instructions)
        (cspp parser wasm text))

(include "parser-external-tools.ss")

(mat parser-wasm-record-writers

     (string=? "#[wasm-limits address-type: i64 minimum: 2 maximum: 9]"
               (format "~s" (make-wasm-limits 'i64 2 9))))

(define parse-binary
  (lambda (parser bytes)
    (run-binary-parser (<~0> parser <eof>) bytes)))

(define capture-parser-error
  (lambda (thunk)
    (guard (err [(parser-error? err) err]
                [else #f])
      (thunk)
      #f)))

(define parser-rejects?
  (lambda (parser bytes)
    (and (capture-parser-error (lambda () (parse-binary parser bytes))) #t)))

(define parse-wat-lexeme
  (lambda (parser text)
    (run-textual-parser (<~0> parser <eof>) text)))

(define with-temporary-wat
  (lambda (text procedure)
    (let ([path (format "parser-wasm-~a.wat" (random 999999))]
          [port #f])
      (dynamic-wind
        (lambda ()
          (set! port
                (open-file-output-port path
                                       (file-options no-fail replace)
                                       (buffer-mode block)
                                       #f))
          (put-bytevector port (string->utf8 text))
          (flush-output-port port))
        (lambda () (procedure path))
        (lambda ()
          (when port (close-port port))
          (when (file-exists? path) (delete-file path)))))))

(define immutable-vector?
  (lambda (value)
    ;; An immutable vector rejects even a no-op mutation.
    (guard (err [else #t])
      (vector-set! value 0 (vector-ref value 0))
      #f)))

(define wasm-mnemonic->text
  (lambda (mnemonic)
    (list->string
     (map (lambda (character)
            (if (char=? character #\-) #\_ character))
          (string->list (symbol->string mnemonic))))))

(define repeated-text
  (lambda (text count)
    (apply string-append
           (map (lambda (unused) (string-append " " text)) (iota count)))))

(define descriptor-immediate-text
  (lambda (descriptor)
    (case (wasm-opcode-immediate-shape descriptor)
      [(none block-type memory-argument try-table) ""]
      [(label-index function-index type-index table-index memory-index global-index
                    local-index tag-index data-index element-index)
       " 0"]
      [(label-vector) " 0"]
      [(heap-type) " func"]
      [(heap-type-non-null) " (ref func)"]
      [(heap-type-nullable) " (ref null func)"]
      [(reference-type) " funcref"]
      [(value-type-vector select-types) " (result i32)"]
      [(call-indirect) " (type 0)"]
      [(i32 i64 f32 f64 lane-index) " 0"]
      [(table-pair memory-pair memory-data table-element struct-field array-new-fixed
                   array-copy type-data type-element)
       " 0 0"]
      [(br-on-cast) " 0 funcref funcref"]
      [(vector-bytes) (string-append " i8x16" (repeated-text "0" 16))]
      [(shuffle-bytes) (repeated-text "0" 16)]
      [(memory-argument-lane) " offset=0 0"]
      [else (error 'descriptor-immediate-text "uncovered immediate shape")])))

(define descriptor-flat-text
  (lambda (descriptor)
    (let ([mnemonic (wasm-opcode-mnemonic descriptor)])
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop)
         (format "~a nop end" (wasm-mnemonic->text mnemonic))]
        [(if)
         (format "~a nop else nop end" (wasm-mnemonic->text mnemonic))]
        [(try-table)
         "try_table (catch_all 0) nop end"]
        [else
         (string-append (wasm-mnemonic->text mnemonic)
                        (descriptor-immediate-text descriptor))]))))

(define descriptor-folded-text
  (lambda (descriptor)
    (let ([mnemonic (wasm-opcode-mnemonic descriptor)])
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop)
         (format "(~a (nop))" (wasm-mnemonic->text mnemonic))]
        [(if)
         "(if (then (nop)) (else (nop)))"]
        [(try-table)
         "(try_table (nop))"]
        [else
         (format "(~a~a)" (wasm-mnemonic->text mnemonic)
                 (descriptor-immediate-text descriptor))]))))

(mat wasm-text-instructions

     (let ([flat (parse-wat-lexeme <wat-expression>
                                   "i32.const 1 i32.const 2 i32.add")]
           [folded (parse-wat-lexeme <wat-expression>
                                     "(i32.add (i32.const 1) (i32.const 2))")])
       (and (= 3 (vector-length flat))
            (= 3 (vector-length folded))
            (eq? 'i32.add
                 (wat-instruction-syntax-mnemonic (vector-ref folded 2)))))

     (andmap
      (lambda (descriptor)
        (let ([mnemonic (wasm-opcode-mnemonic descriptor)])
          (if (memq mnemonic '(else end))
              #t
              (let* ([flat
                        (guard (error
                                [else
                                 (errorf 'wasm-text-instructions
                                         "flat descriptor ~a failed: ~a"
                                         mnemonic error)])
                          (parse-wat-lexeme
                           <wat-expression> (descriptor-flat-text descriptor)))]
                       [folded
                        (guard (error
                                [else
                                 (errorf 'wasm-text-instructions
                                         "folded descriptor ~a failed: ~a"
                                         mnemonic (condition-message error))])
                          (parse-wat-lexeme
                           <wat-expression> (descriptor-folded-text descriptor)))])
                  (and (positive? (vector-length flat))
                       (positive? (vector-length folded))
                       (eq? mnemonic
                            (wat-instruction-syntax-mnemonic
                             (vector-ref folded (fx1- (vector-length folded))))))))))
      (vector->list wasm-core-3-opcodes))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "local.get $value i32.load $memory offset=4 align=2")]
            [local (vector-ref expression 0)]
            [load (vector-ref expression 1)]
            [local-index (vector-ref (wat-instruction-syntax-immediates local) 0)]
            [memory-argument
             (vector-ref (wat-instruction-syntax-immediates load) 0)])
       (and (= 10 (wat-index-reference-pos local-index))
            (string=? "$value" (wat-index-reference-value local-index))
            (= 1 (vector-ref memory-argument 0))
            (= 4 (vector-ref memory-argument 1))
            (string=? "$memory"
                      (wat-index-reference-value (vector-ref memory-argument 2)))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "block $outer i32.const 1 loop $inner nop end $inner end $outer")]
            [block (vector-ref expression 0)]
            [loop (vector-ref (wat-instruction-syntax-body block) 1)])
       (and (eq? 'block (wat-instruction-syntax-mnemonic block))
            (eq? 'loop (wat-instruction-syntax-mnemonic loop))))

     (let* ([flat
             (vector-ref
              (parse-wat-lexeme
               <wat-expression>
               "try_table (catch $tag $label) (catch_all 2) nop end")
              0)]
            [folded
             (vector-ref
              (parse-wat-lexeme
               <wat-expression>
               "(try_table (catch_ref 3 4) (catch_all_ref $label) (nop))")
              0)]
            [flat-catches (wat-instruction-syntax-alternate flat)]
            [folded-catches (wat-instruction-syntax-alternate folded)])
       (and (= 2 (vector-length flat-catches))
            (= 2 (vector-length folded-catches))
            (eq? 'catch (wat-catch-syntax-kind (vector-ref flat-catches 0)))
            (eq? 'catch-all (wat-catch-syntax-kind (vector-ref flat-catches 1)))
            (eq? 'catch-ref (wat-catch-syntax-kind (vector-ref folded-catches 0)))
            (eq? 'catch-all-ref
                 (wat-catch-syntax-kind (vector-ref folded-catches 1)))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "table.get table.copy table.init 7 memory.size memory.copy memory.init 8")]
            [table-get (vector-ref expression 0)]
            [table-copy (vector-ref expression 1)]
            [table-init (vector-ref expression 2)]
            [memory-size (vector-ref expression 3)]
            [memory-copy (vector-ref expression 4)]
            [memory-init (vector-ref expression 5)])
       (and (= 0 (wat-index-reference-value
                  (vector-ref (wat-instruction-syntax-immediates table-get) 0)))
            (equal? '(0 0)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates table-copy))))
            (equal? '(0 7)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates table-init))))
            (= 0 (wat-index-reference-value
                  (vector-ref (wat-instruction-syntax-immediates memory-size) 0)))
            (equal? '(0 0)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates memory-copy))))
            (equal? '(0 8)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates memory-init))))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "memory.init 3 4 table.init 5 6 array.new_fixed $array 2")]
            [memory-init (vector-ref expression 0)]
            [table-init (vector-ref expression 1)]
            [array-new-fixed (vector-ref expression 2)])
       (and (equal? '(3 4)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates memory-init))))
            (equal? '(5 6)
                    (map wat-index-reference-value
                         (vector->list
                          (wat-instruction-syntax-immediates table-init))))
            (= 2 (vector-ref (wat-instruction-syntax-immediates array-new-fixed) 1))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "ref.test (ref null func) ref.cast (ref func)")]
            [test-type
             (vector-ref
              (wat-instruction-syntax-immediates (vector-ref expression 0)) 0)]
            [cast-type
             (vector-ref
              (wat-instruction-syntax-immediates (vector-ref expression 1)) 0)])
       (and (wasm-reference-type? test-type)
            (wasm-reference-type-nullable? test-type)
            (wasm-reference-type? cast-type)
            (not (wasm-reference-type-nullable? cast-type))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "v128.const i8x16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 255")]
            [bytes
             (vector-ref
              (wat-instruction-syntax-immediates (vector-ref expression 0)) 0)])
       (and (bytevector? bytes)
            (= 16 (bytevector-length bytes))
            (= 255 (bytevector-u8-ref bytes 15))))

     (let* ([expression
             (parse-wat-lexeme <wat-expression> "i32.load i64.load align=8")]
            [default-argument
             (vector-ref
              (wat-instruction-syntax-immediates (vector-ref expression 0)) 0)]
            [explicit-argument
             (vector-ref
              (wat-instruction-syntax-immediates (vector-ref expression 1)) 0)])
       (and (= 2 (vector-ref default-argument 0))
            (= 3 (vector-ref explicit-argument 0))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "select select (result) select (result i32) (result funcref)")]
            [untyped (wat-instruction-syntax-immediates (vector-ref expression 0))]
            [empty-typed (wat-instruction-syntax-immediates (vector-ref expression 1))]
            [typed (wat-instruction-syntax-immediates (vector-ref expression 2))]
            [folded
             (wat-instruction-syntax-immediates
              (vector-ref
               (parse-wat-lexeme
                <wat-expression> "(select (result i32) (result i64))")
               0))])
       (and (zero? (vector-length untyped))
            (zero? (vector-length (vector-ref empty-typed 0)))
            (= 2 (vector-length (vector-ref typed 0)))
            (= 2 (vector-length (vector-ref folded 0)))))

     (let* ([flat
             (vector-ref
              (parse-wat-lexeme <wat-expression> "v128.load8_lane 3") 0)]
            [folded
             (vector-ref
              (parse-wat-lexeme <wat-expression> "(v128.store16_lane 7)") 0)]
            [flat-immediates (wat-instruction-syntax-immediates flat)]
            [folded-immediates (wat-instruction-syntax-immediates folded)])
       (and (= 3 (vector-ref flat-immediates 1))
            (= 0
               (wat-index-reference-value
                (vector-ref (vector-ref flat-immediates 0) 2)))
            (= 7 (vector-ref folded-immediates 1))
            (= 0
               (wat-index-reference-value
                (vector-ref (vector-ref folded-immediates 0) 2)))))

     (let* ([expression
             (parse-wat-lexeme
              <wat-expression>
              "(block nop (nop)) (i32.add i32.const 1 (i32.const 2))")]
            [block (vector-ref expression 0)])
       (and (= 4 (vector-length expression))
            (= 2 (vector-length (wat-instruction-syntax-body block)))
            (eq? 'i32.add
                 (wat-instruction-syntax-mnemonic (vector-ref expression 3)))))

     ;; error: unknown instruction mnemonics are rejected.
     (error? (parse-wat-lexeme <wat-expression> "not.an.opcode"))

     ;; error: required instruction immediates cannot be omitted.
     (error? (parse-wat-lexeme <wat-expression> "local.get"))

     ;; error: opening and closing structured labels must match.
     (error? (parse-wat-lexeme <wat-expression> "block $a nop end $b"))

     ;; error: a SIMD lane must be in range for its lane shape.
     (error? (parse-wat-lexeme <wat-expression> "i8x16.extract_lane_s 16"))

     ;; error: shuffle lanes are limited to the two input vectors.
     (error? (parse-wat-lexeme
              <wat-expression>
              (string-append "i8x16.shuffle" (repeated-text "32" 16))))

     ;; error: each memory attribute may occur at most once.
     (error? (parse-wat-lexeme <wat-expression> "i32.load offset=1 offset=2"))

     ;; error: explicit memory alignment must be a positive power of two.
     (error? (parse-wat-lexeme <wat-expression> "i32.load align=3"))

     ;; error: a memory offset must precede its alignment attribute.
     (error? (parse-wat-lexeme <wat-expression> "i32.load align=2 offset=1"))

     ;; error: array.new_fixed requires a numeric element count.
     (error? (parse-wat-lexeme <wat-expression> "array.new_fixed 0 $count"))

     ;; error: ref.test requires a reference type rather than a bare heap type.
     (error? (parse-wat-lexeme <wat-expression> "ref.test func"))

     ;; error: a v128 constant requires exactly the lane count selected by its shape.
     (error? (parse-wat-lexeme <wat-expression> "v128.const i8x16 0 1"))

     ;; error: integer vector lanes must fit their selected lane width.
     (error? (parse-wat-lexeme
              <wat-expression>
              "v128.const i8x16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 256"))

     ;; error: catch syntax rejects kinds outside the Core 3.0 catch grammar.
     (error? (make-wat-catch-syntax
              0 'wrong #f (make-wat-index-reference 0 0)))

     ;; error: folded instructions must have a closing parenthesis.
     (error? (parse-wat-lexeme <wat-expression> "(i32.const 0"))

     ;; error: else and end are reserved structured terminators.
     (error? (parse-wat-lexeme <wat-expression> "else"))

     ;; error: end cannot appear outside a structured instruction.
     (error? (parse-wat-lexeme <wat-expression> "end"))

     )

(define core-fields-wat
  "(module $m
     (@custom \"before\" \"A\")
     (rec
       (type $node (sub (struct (field $next (mut (ref null $node))))))
       (type $items (array (mut i16))))
     (type $sig (func (param $value i32) (result i32)))
     (import \"env\" \"f\" (func $imported (type $sig)))
     (func $f (export \"f\") (type $sig) (param $value i32)
       (result i32) local.get $value)
     (table $tab (export \"tab\") 1 4 funcref)
     (table $inline-table funcref (elem $f))
     (memory $mem (export \"memory\") i64 1 4)
     (memory $bytes (data \"abc\" \"def\"))
     (global $g (mut i32) (i32.const 0))
     (tag $tag (type $sig))
     (export \"g\" (global $g))
     (start $f)
     (elem $active (table $tab) (offset (i32.const 0)) func $f)
     (elem $passive funcref (ref.func $f))
     (data $active-data (memory $mem) (offset (i64.const 0)) \"x\")
     (data $passive-data \"y\")
     (@custom \"after\" \"B\"))")

(mat wasm-text-types-and-fields

     (let* ([module (run-textual-parser parser-wat-module-syntax core-fields-wat)]
            [field* (wat-module-fields module)]
            [first-field (vector-ref field* 0)]
            [recursive-field (vector-ref field* 1)])
       (and (wat-module? module)
            (= 0 (wat-module-pos module))
            (string=? "$m" (wat-module-id module))
            (= 18 (vector-length field*))
            (wat-module-field? first-field)
            (eq? 'custom (wat-module-field-kind first-field))
            (= 16 (wat-module-field-pos first-field))
            (not
             (wat-custom-placement-after
              (vector-ref (wat-module-field-data first-field) 2)))
            (eq? 'recursive-type (wat-module-field-kind recursive-field))
            (string=? "$node"
                      (wat-subtype-syntax-id
                       (vector-ref
                        (wat-recursive-type-syntax-subtypes
                         (wat-module-field-data recursive-field))
                        0)))
            (string=? "$imported" (wat-module-field-id (vector-ref field* 3)))
            (string=? "$f" (wat-module-field-id (vector-ref field* 4)))
            (eq? 'table-element
                 (wat-inline-abbreviation-kind
                  (wat-module-field-abbreviation (vector-ref field* 6))))
            (wat-element-segment-syntax?
             (wat-inline-abbreviation-data
              (wat-module-field-abbreviation (vector-ref field* 6))))
            (eq? 'memory-data
                 (wat-inline-abbreviation-kind
                  (wat-module-field-abbreviation (vector-ref field* 8))))
            (eq? 'active
                 (wat-element-segment-syntax-mode
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 13)))))
            (string=? "$tab"
                      (wat-index-reference-value
                       (wat-element-segment-syntax-table
                        (wat-inline-abbreviation-data
                         (wat-module-field-abbreviation
                          (vector-ref field* 13))))))
            (eq? 'active
                 (wat-data-segment-syntax-mode
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 15)))))
            (string=? "$mem"
                      (wat-index-reference-value
                       (wat-data-segment-syntax-memory
                        (wat-inline-abbreviation-data
                         (wat-module-field-abbreviation
                          (vector-ref field* 15))))))
            (eq? 'data
                 (wat-custom-placement-after
                  (vector-ref
                   (wat-module-field-data (vector-ref field* 17)) 2)))))

     (let ([type (parse-wat-lexeme <wat-value-type> "(ref null $node)")])
       (and (wat-reference-type-syntax? type)
            (= 0 (wat-reference-type-syntax-pos type))
            (string=? "$node"
                      (wat-index-reference-value
                       (wat-reference-type-syntax-heap-type type)))))

     (let ([type (parse-wat-lexeme <wat-function-type>
                                   "(func (param $x i32) (param i64 f32) (result i32))")])
       (and (wat-function-type-syntax? type)
            (= 3 (vector-length (wat-function-type-syntax-parameters type)))
            (string=? "$x"
                      (wat-binding-id
                       (vector-ref (wat-function-type-syntax-parameters type) 0)))
            (= 1 (vector-length (wat-function-type-syntax-results type)))))

     (let ([limits (parse-wat-lexeme <wat-limits> "i64 1 4")])
       (and (wasm-limits? limits)
            (eq? 'i64 (wasm-limits-address-type limits))
            (= 1 (wasm-limits-minimum limits))
            (= 4 (wasm-limits-maximum limits))))

     (let ([type (parse-wat-lexeme <wat-table-type> "1 2 funcref")])
       (and (wasm-table-type? type)
            (= 1 (wasm-limits-minimum (wasm-table-type-limits type)))
            (eq? 'func
                 (wasm-reference-type-heap-type
                  (wasm-table-type-reference-type type)))))

     (let ([type (parse-wat-lexeme <wat-memory-type> "i64 2")])
       (and (wasm-memory-type? type)
            (eq? 'i64
                 (wasm-limits-address-type (wasm-memory-type-limits type)))))

     (let ([type (parse-wat-lexeme <wat-global-type> "(mut externref)")])
       (and (wasm-global-type? type)
            (wasm-global-type-mutable? type)))

     (let ([type (parse-wat-lexeme <wat-table-type> "1 (ref null $node)")])
       (and (wat-table-type-syntax? type)
            (= 0 (wat-table-type-syntax-pos type))))

     (let* ([recursive-type
             (parse-wat-lexeme <wat-recursive-type>
                               "(rec (type $item (sub (func))))")]
            [subtype
             (vector-ref (wat-recursive-type-syntax-subtypes recursive-type) 0)])
       (= 5 (wat-subtype-syntax-pos subtype)))

     (let* ([module
             (run-textual-parser
              parser-wat-module-syntax
              "(module
                 (func $f (export \"f\") (import \"m\" \"f\") (type 0))
                 (table $t (export \"t\") (import \"m\" \"t\") 1 funcref)
                 (memory $mem (export \"mem\") (import \"m\" \"mem\") 1)
                 (global $g (export \"g\") (import \"m\" \"g\") i32)
                 (tag $e (export \"e\") (import \"m\" \"e\") (type 0)))")]
            [field* (wat-module-fields module)]
            [function-field (vector-ref field* 0)]
            [table-field (vector-ref field* 1)]
            [memory-field (vector-ref field* 2)]
            [global-field (vector-ref field* 3)]
            [tag-field (vector-ref field* 4)])
       (and (= 5 (vector-length field*))
            (string=? "f" (vector-ref (wat-module-field-import function-field) 1))
            (string=? "f" (vector-ref (wat-module-field-exports function-field) 0))
            (string=? "t" (vector-ref (wat-module-field-import table-field) 1))
            (string=? "mem" (vector-ref (wat-module-field-import memory-field) 1))
            (string=? "g" (vector-ref (wat-module-field-import global-field) 1))
            (string=? "e" (vector-ref (wat-module-field-import tag-field) 1))
            (immutable-vector? (wat-module-field-import function-field))
            (immutable-vector? (wat-module-field-exports function-field))))

     (let* ([module
             (run-textual-parser
              parser-wat-module-syntax
              "(module
                 (func (export \"x\") (import \"m\" \"f\"))
                 (@custom \"after-import\" \"payload\"))")]
            [custom (vector-ref (wat-module-fields module) 1)]
            [placement (vector-ref (wat-module-field-data custom) 2)])
       (eq? 'import (wat-custom-placement-after placement)))

     (let* ([module
             (run-textual-parser
              parser-wat-module-syntax
              "(module
                 (import \"m\" \"f\" (func))
                 (func (export \"defined\"))
                 (global i32 (i32.const 0))
                 (export \"defined\" (func 1))
                 (@custom \"metadata\" \"payload\"))")]
            [field* (wat-module-fields module)]
            [import-field (vector-ref field* 0)]
            [function-field (vector-ref field* 1)]
            [global-field (vector-ref field* 2)]
            [export-field (vector-ref field* 3)]
            [custom-field (vector-ref field* 4)])
       (and (immutable-vector? field*)
            (immutable-vector? (wat-module-field-data import-field))
            (immutable-vector? (wat-module-field-data function-field))
            (immutable-vector? (wat-module-field-exports function-field))
            (immutable-vector? (wat-module-field-data global-field))
            (immutable-vector? (wat-module-field-data export-field))
            (immutable-vector? (wat-module-field-data custom-field))))

     (let* ([module
            (run-textual-parser
             parser-wat-module-syntax
             "(module
                (memory i32 1)
                (table i32 1 funcref)
                (memory 1 2 shared)
                (memory (data)))")]
            [field* (wat-module-fields module)]
            [shared-type (wat-module-field-data (vector-ref field* 2))]
            [inline-data
             (wat-inline-abbreviation-data
              (wat-module-field-abbreviation (vector-ref field* 3)))])
       (and (= 4 (vector-length field*))
            (wasm-memory-type? (wat-module-field-data (vector-ref field* 0)))
            (wasm-table-type? (wat-module-field-data (vector-ref field* 1)))
            (wat-memory-type-syntax? shared-type)
            (wat-limits-syntax-shared?
             (wat-memory-type-syntax-limits shared-type))
            (wat-data-segment-syntax? inline-data)
            (zero? (vector-length (wat-data-segment-syntax-strings inline-data)))))

     (let* ([module
            (run-textual-parser
             parser-wat-module-syntax
             "(module
                (func (param) (result) (local))
                (@custom \"a\" (after type) \"x\")
                (@custom \"b\" (before import) \"y\"))")]
            [field* (wat-module-fields module)]
            [after
             (vector-ref (wat-module-field-data (vector-ref field* 1)) 2)]
            [before
             (vector-ref (wat-module-field-data (vector-ref field* 2)) 2)])
       (and (= 3 (vector-length field*))
            (eq? 'type (wat-custom-placement-after after))
            (eq? 'import (wat-custom-placement-before before))))

     (let* ([module
             (run-textual-parser
              parser-wat-module-syntax
              "(module
                 (table i64 funcref (elem))
                 (table i32 funcref (elem))
                 (memory i64 (data))
                 (memory i32 (data)))")]
            [field* (wat-module-fields module)])
       (and (= 4 (vector-length field*))
            (eq? 'i64
                 (wat-element-segment-syntax-address-type
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 0)))))
            (eq? 'i32
                 (wat-element-segment-syntax-address-type
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 1)))))
            (eq? 'i64
                 (wat-data-segment-syntax-address-type
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 2)))))
            (eq? 'i32
                 (wat-data-segment-syntax-address-type
                  (wat-inline-abbreviation-data
                   (wat-module-field-abbreviation (vector-ref field* 3)))))))

     (let* ([module
             (run-textual-parser
              parser-wat-module-syntax
              (string-append
               "(module\n"
               "  (table funcref (elem 0 1))\n"
               "  (table funcref (elem (ref.func 0) (ref.func 1))))"))]
            [field* (wat-module-fields module)]
            [index-items
             (wat-inline-abbreviation-data
              (wat-module-field-abbreviation (vector-ref field* 0)))]
            [expression-items
             (wat-inline-abbreviation-data
              (wat-module-field-abbreviation (vector-ref field* 1)))])
       (and (eq? 'indexes (wat-element-segment-syntax-item-kind index-items))
            (= 2 (vector-length (wat-element-segment-syntax-items index-items)))
            (= 0 (wat-index-reference-value
                  (vector-ref (wat-element-segment-syntax-items index-items) 0)))
            (eq? 'expressions
                 (wat-element-segment-syntax-item-kind expression-items))
            (= 2 (vector-length (wat-element-segment-syntax-items expression-items)))
            (wat-instruction-syntax?
             (vector-ref (wat-element-segment-syntax-items expression-items) 0))))

     (let ([module
            (run-textual-parser parser-wat-module-syntax
                                "(module (elem) (data))")])
       (= 2 (vector-length (wat-module-fields module))))

     ;; error: an element segment cannot contain an untyped reserved token.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (elem nonsense))"))

     ;; error: a data segment contains byte strings, not numeric tokens.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (data 123))"))

     ;; error: a table element abbreviation requires indexes or element expressions.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (table funcref (elem nonsense)))"))

     ;; error: a table element abbreviation cannot mix indexes and expressions.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (table funcref (elem 0 (ref.func 0))))"))

     ;; error: a table element abbreviation cannot mix expressions and indexes.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (table funcref (elem (ref.func 0) 0)))"))

     ;; error: shared memory limits require an explicit maximum.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (memory 1 shared))"))

     ;; error: tables cannot use shared limits.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (table 1 2 shared funcref))"))

     ;; error: duplicate custom after anchors are not allowed.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (@custom \"x\" (after type) (after import) \"a\"))"))

     ;; error: duplicate custom before anchors are not allowed.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (@custom \"x\" (before type) (before import) \"a\"))"))

     ;; error: a custom annotation permits only one placement anchor in total.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (@custom \"x\" (after type) (before import) \"a\"))"))

     ;; error: data-count is not a supported custom annotation placement anchor.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (@custom \"x\" (after data-count) \"a\"))"))

     ;; error: custom placement anchors must name a standard section.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (@custom \"x\" (before unknown) \"a\"))"))

     ;; error: an inline-imported function cannot declare locals.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (func (import \"m\" \"f\") (local i32)))"))

     ;; error: an inline import cannot precede an inline export.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (func (import \"m\" \"f\") (export \"x\")))"))

     ;; error: a table inline import cannot precede an inline export.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (table (import \"m\" \"t\") (export \"t\") 1 funcref))"))

     ;; error: a global inline import cannot precede an inline export.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (global (import \"m\" \"g\") (export \"g\") i32))"))

     ;; error: the module keyword must end at a token boundary.
     (error? (run-textual-parser parser-wat-module-syntax "(modulex)"))

     ;; error: a module must have a closing delimiter.
     (error? (run-textual-parser parser-wat-module-syntax "(module (memory 1)"))

     ;; error: a function may contain at most one inline import clause.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (func (import \"a\" \"b\") (import \"c\" \"d\")))"))

     ;; error: a function may contain at most one explicit type clause.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (func (type 0) (type 1)))"))

     ;; error: unknown module field keywords are rejected.
     (error? (run-textual-parser parser-wat-module-syntax "(module (wrong 0))"))

     ;; error: export descriptions require an index reference.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (export \"f\" (func)))"))

     ;; error: limits reject a maximum below the minimum.
     (error? (run-textual-parser parser-wat-module-syntax "(module (memory 4 1))"))

     ;; error: an inline import must precede a function type use.
     (error? (run-textual-parser
              parser-wat-module-syntax
              "(module (func (param i32) (import \"a\" \"b\")))"))

     ;; error: custom annotations must have a closing delimiter.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module (@custom \"name\" \"value\")"))

     ;; error: WAST commands after the single module are rejected.
     (error? (run-textual-parser parser-wat-module-syntax
                                 "(module) (invoke \"f\")"))

     )

(mat wasm-text-lexical

     (equal? '()
             (parse-wat-lexeme
              <wat-trivia>
              " \t;; line\r\n(; outer (; middle (; inner ;) ;) ;)"))

     (equal? '()
             (parse-wat-lexeme <wat-trivia>
                               "(@metadata value (@nested \"text)\"))"))

     ;; error: custom annotations remain available to the module-field grammar.
     (error? (parse-wat-lexeme <wat-trivia> "(@custom \"name\" \"payload\")"))

     (string=? "$type-0!" (parse-wat-lexeme <wat-identifier> "$type-0!"))

     (string=? "$hello"
               (parse-wat-lexeme <wat-identifier> "$\"hello\""))

     (string=? "$lambda: \x03bb;"
               (parse-wat-lexeme <wat-identifier> "$\"lambda: λ\""))

     (string=? "module" (parse-wat-lexeme (<wat-keyword> "module") "module (; ok ;)"))

     ;; error: keywords must end at a token boundary.
     (error? (parse-wat-lexeme (<wat-keyword> "module") "modulex"))

     (equal? #vu8(#x41 #x0a #xff)
             (parse-wat-lexeme <wat-string> "\"A\\n\\ff\""))

     (equal? #vu8(#x09 #x0a #x0d #x22 #x27 #x5c)
             (parse-wat-lexeme <wat-string> "\"\\t\\n\\r\\\"\\'\\\\\""))

     (equal? #vu8(#xf0 #x9f #x98 #x80)
             (parse-wat-lexeme <wat-string> "\"\\u{1f600}\""))

     (equal? #vu8(#xf0 #x9f #x98 #x80)
             (parse-wat-lexeme <wat-string> "\"\\u{1_f600}\""))

     (equal? #vu8(#x00)
             (parse-wat-lexeme <wat-string> "\"\\u{0000000}\""))

     (string=? "lambda: \x03bb;"
               (parse-wat-lexeme <wat-name> "\"lambda: λ\""))

     (equal? #vu8(#xc0 #xaf)
             (parse-wat-lexeme <wat-string> "\"\\c0\\af\""))

     (= 4294967295 (parse-wat-lexeme <wat-u32> "4_294_967_295"))

     (= #xffffffffffffffff
        (parse-wat-lexeme <wat-u64> "0xffff_ffff_ffff_ffff"))

     (= #xffffffff (parse-wat-lexeme <wat-i32> "-1"))

     (= #xffffffffffffffff (parse-wat-lexeme <wat-i64> "0xffff_ffff_ffff_ffff"))

     (= #x80000000 (parse-wat-lexeme <wat-i32> "-2_147_483_648"))

     (= #x3fc00000
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "1.5")))

     (= #x4000000000000000
        (wasm-float-bits (parse-wat-lexeme <wat-f64> "0x1p+1")))

     (= #x40400000
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "0x1.8p+1")))

     (= #x3ff0000000000000
        (wasm-float-bits (parse-wat-lexeme <wat-f64> "1e0")))

     (= #x00000001
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "0x1p-149")))

     (= #x0000000000000001
        (wasm-float-bits (parse-wat-lexeme <wat-f64> "0x1p-1074")))

     (= #x4b800000
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "16_777_217")))

     (= #xff800000
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "-inf")))

     (= #x7fc00000
        (wasm-float-bits (parse-wat-lexeme <wat-f32> "nan")))

     (= #xfff0000000000001
        (wasm-float-bits (parse-wat-lexeme <wat-f64> "-nan:0x1")))

     ;; error: block comments must close at their original nesting depth.
     (error? (parse-wat-lexeme <wat-trivia> "(; outer (; inner ;)"))

     ;; error: byte strings reject unknown escapes.
     (error? (parse-wat-lexeme <wat-string> "\"\\q\""))

     ;; error: byte strings reject unescaped control characters.
     (error? (parse-wat-lexeme <wat-string> "\"line\nfeed\""))

     ;; error: Unicode escapes must denote scalar values.
     (error? (parse-wat-lexeme <wat-string> "\"\\u{d800}\""))

     ;; error: Unicode escapes cannot exceed the maximum scalar value.
     (error? (parse-wat-lexeme <wat-string> "\"\\u{110000}\""))

     ;; error: Unicode escapes require at least one hexadecimal digit.
     (error? (parse-wat-lexeme <wat-string> "\"\\u{}\""))

     ;; error: Unicode escapes require a closing brace.
     (error? (parse-wat-lexeme <wat-string> "\"\\u{41\""))

     ;; error: names must contain valid UTF-8 after byte escapes are decoded.
     (error? (parse-wat-lexeme <wat-name> "\"\\c0\\af\""))

     ;; error: numeric separators cannot occur alone or consecutively.
     (error? (parse-wat-lexeme <wat-i32> "1__0"))

     ;; error: an underscore alone is not an integer literal.
     (error? (parse-wat-lexeme <wat-i32> "_"))

     ;; error: identifiers require a character after the dollar sign.
     (error? (parse-wat-lexeme <wat-identifier> "$"))

     ;; error: commas are not legal identifier characters.
     (error? (parse-wat-lexeme <wat-identifier> "$bad,"))

     ;; error: hexadecimal floats require an exponent.
     (error? (parse-wat-lexeme <wat-f64> "0x1.5"))

     ;; error: decimal exponents require digits.
     (error? (parse-wat-lexeme <wat-f64> "1e+"))

     ;; error: numeric tokens reject trailing identifier characters.
     (error? (parse-wat-lexeme <wat-f32> "1.0oops"))

     ;; error: adjacent keyword and string tokens require a separator.
     (error? (parse-wat-lexeme (<~> (<wat-keyword> "module") <wat-string>)
                               "module\"x\""))

     ;; error: adjacent identifier and string tokens require a separator.
     (error? (parse-wat-lexeme (<~> <wat-identifier> <wat-string>)
                               "$x\"y\""))

     ;; error: adjacent string tokens require a separator.
     (error? (parse-wat-lexeme (<~> <wat-string> <wat-string>)
                               "\"x\"\"y\""))

     ;; error: adjacent float and string tokens require a separator.
     (error? (parse-wat-lexeme (<~> <wat-f32> <wat-string>)
                               "1e0\"y\""))

     ;; error: finite float literals cannot round to infinity.
     (error? (parse-wat-lexeme <wat-f32> "1e1000"))

     ;; error: unescaped DEL is not a legal string character.
     (error? (parse-wat-lexeme <wat-string>
                               (string (integer->char #x22)
                                       (integer->char #x7f)
                                       (integer->char #x22))))

     ;; error: integer tokens reject trailing identifier characters.
     (error? (parse-wat-lexeme <wat-u32> "10things"))

     ;; error: unsigned 32-bit integer literals cannot overflow.
     (error? (parse-wat-lexeme <wat-u32> "4294967296"))

     ;; error: negative 32-bit integer spellings cannot exceed the signed range.
     (error? (parse-wat-lexeme <wat-i32> "-2147483649"))

     ;; error: NaN payloads must fit the target significand.
     (error? (parse-wat-lexeme <wat-f32> "nan:0x800000"))

     ;; error: NaN payloads must be nonzero.
     (error? (parse-wat-lexeme <wat-f64> "nan:0x0"))

     ;; error: the token wrapper requires a parser argument.
     (error? (<wat-token> 'not-a-parser))

     ;; error: the keyword parser requires a string argument.
     (error? (<wat-keyword> 'module))

     ;; error: an empty string is not a keyword.
     (error? (<wat-keyword> ""))

     )

(mat wasm-binary-values-and-types

     (= #xffffffff
        (run-binary-parser <wasm-u32>
                           (bytevector #xff #xff #xff #xff #x0f)))

     (= -2147483648
        (run-binary-parser <wasm-s32>
                           (bytevector #x80 #x80 #x80 #x80 #x78)))

     ;; A non-minimal encoding is legal when it stays within ceil(N / 7) bytes.
     (= 0 (run-binary-parser <wasm-u32> (bytevector #x80 #x00)))

     ;; error: u32 has nonzero unused bits in its fifth byte.
     (error? (run-binary-parser <wasm-u32>
                                (bytevector #xff #xff #xff #xff #x10)))

     ;; error: u32 cannot occupy six bytes.
     (error? (run-binary-parser <wasm-u32>
                                (bytevector #x80 #x80 #x80 #x80 #x80 #x00)))

     (let ([type (run-binary-parser <wasm-reference-type> (bytevector #x64 #x70))])
       (and (wasm-reference-type? type)
            (not (wasm-reference-type-nullable? type))
            (eq? 'func (wasm-reference-type-heap-type type))))

     (let ([limits (run-binary-parser <wasm-limits>
                                      (bytevector #x05 #x02 #x09))])
       (and (eq? 'i64 (wasm-limits-address-type limits))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))))

     (equal? '#(1 2 3)
             (parse-binary (<wasm-vector> <wasm-u32>) #vu8(3 1 2 3)))

     (let ([instruction
            (parse-binary <wasm-instruction>
                          #vu8(#xfd #x0d 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))])
       (let ([immediates (wasm-instruction-immediates instruction)])
         (and (immutable-vector? immediates)
              (equal? #vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                      (vector-ref immediates 0)))))

     )

(define instruction-has-fields?
  (lambda (instruction mnemonic immediates body alternate)
    (and (wasm-instruction? instruction)
         (eq? mnemonic (wasm-instruction-mnemonic instruction))
         (equal? immediates (wasm-instruction-immediates instruction))
         (equal? body (wasm-instruction-body instruction))
         (equal? alternate (wasm-instruction-alternate instruction)))))

(define integer-range
  (lambda (start end)
    (let loop ([value start] [values '()])
      (if (= value end)
          (reverse values)
          (loop (+ value 1) (cons value values))))))

(define reserved-one-byte-opcodes
  ;; Final Core 3.0 unassigned bytes, excluding the 0xfb, 0xfc, and 0xfd prefixes.
  (append '(#x06 #x07 #x09 #x16 #x17 #x18 #x19 #x1d #x1e #x27)
          (integer-range #xc5 #xd0)
          (integer-range #xd7 #xfb)
          '(#xfe #xff)))

(define make-nop-expression-bytes
  (lambda (count)
    (let ([bytes (make-bytevector (+ count 1) #x01)])
      (bytevector-u8-set! bytes count #x0b)
      bytes)))

(define make-shuffle-instruction-bytes
  (lambda (invalid-position)
    (let ([bytes (make-bytevector 18)])
      (bytevector-u8-set! bytes 0 #xfd)
      (bytevector-u8-set! bytes 1 #x0d)
      (let loop ([position 0])
        (when (fx< position 16)
          (bytevector-u8-set! bytes (fx+ position 2)
                              (if (fx= position invalid-position) 32 position))
          (loop (fx1+ position))))
      bytes)))

(mat wasm-binary-scalar-and-control-instructions

     (instruction-has-fields?
      (parse-binary <wasm-instruction> #vu8(#x01))
      'nop '#() '#() '#())

     (instruction-has-fields?
      (parse-binary <wasm-instruction> #vu8(#x1b))
      'select '#() '#() '#())

     (andmap
      (lambda (test)
        (let ([instruction (parse-binary <wasm-instruction> (car test))])
          (instruction-has-fields? instruction (cadr test) (caddr test) '#() '#())))
      (list (list #vu8(#x0c #x03) 'br '#(3))
            (list #vu8(#x10 #x04) 'call '#(4))
            (list #vu8(#x14 #x05) 'call-ref '#(5))
            (list #vu8(#x25 #x06) 'table.get '#(6))
            (list #vu8(#x3f #x07) 'memory.size '#(7))
            (list #vu8(#x23 #x08) 'global.get '#(8))
            (list #vu8(#x20 #x09) 'local.get '#(9))
            (list #vu8(#x08 #x0a) 'throw '#(10))
            (list #vu8(#xfc #x09 #x0b) 'data.drop '#(11))
            (list #vu8(#xfc #x0d #x0c) 'elem.drop '#(12))))

     (let* ([instruction (parse-binary <wasm-instruction>
                                       #vu8(#x0e #x02 #x01 #x02 #x03))]
            [immediates (wasm-instruction-immediates instruction)])
       (and (eq? 'br-table (wasm-instruction-mnemonic instruction))
            (= 2 (vector-length immediates))
            (equal? '#(1 2) (vector-ref immediates 0))
            (= 3 (vector-ref immediates 1))))

     (and (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#x11 #x02 #x07))
           'call-indirect '#(2 7) '#() '#())
          (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#x13 #x03 #x09))
           'return-call-indirect '#(3 9) '#() '#()))

     (let* ([instruction (parse-binary <wasm-instruction> #vu8(#x28 #x02 #x10))]
            [argument (vector-ref (wasm-instruction-immediates instruction) 0)])
       (and (eq? 'i32.load (wasm-instruction-mnemonic instruction))
            (wasm-memory-argument? argument)
            (= 2 (wasm-memory-argument-alignment argument))
            (= 0 (wasm-memory-argument-memory-index argument))
            (= 16 (wasm-memory-argument-offset argument))))

     (let* ([instruction
             (parse-binary
              <wasm-instruction>
              #vu8(#x29 #x43 #x02 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01))]
            [argument (vector-ref (wasm-instruction-immediates instruction) 0)])
       (and (= 3 (wasm-memory-argument-alignment argument))
            (= 2 (wasm-memory-argument-memory-index argument))
            (= #x8000000000000000 (wasm-memory-argument-offset argument))))

     (let* ([instruction
             (parse-binary
              <wasm-instruction>
              #vu8(#x28 #x3f
                    #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01))]
            [argument (vector-ref (wasm-instruction-immediates instruction) 0)])
       (and (= 63 (wasm-memory-argument-alignment argument))
            (= 0 (wasm-memory-argument-memory-index argument))
            (= #xffffffffffffffff (wasm-memory-argument-offset argument))))

     (let* ([instruction
             (parse-binary <wasm-instruction> #vu8(#x28 #x40 #x03 #x00))]
            [argument (vector-ref (wasm-instruction-immediates instruction) 0)])
       (and (= 0 (wasm-memory-argument-alignment argument))
            (= 3 (wasm-memory-argument-memory-index argument))
            (= 0 (wasm-memory-argument-offset argument))))

     (and (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#x41 #x7f))
           'i32.const '#(-1) '#() '#())
          (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#x42 #x7e))
           'i64.const '#(-2) '#() '#()))

     (let* ([f32-instruction
             (parse-binary <wasm-instruction> #vu8(#x43 #x01 #x23 #x45 #x67))]
            [f64-instruction
             (parse-binary
              <wasm-instruction>
              #vu8(#x44 #xef #xcd #xab #x89 #x67 #x45 #x23 #x01))]
            [f32-value (vector-ref (wasm-instruction-immediates f32-instruction) 0)]
            [f64-value (vector-ref (wasm-instruction-immediates f64-instruction) 0)])
       (and (wasm-float? f32-value)
            (= 32 (wasm-float-width f32-value))
            (= #x67452301 (wasm-float-bits f32-value))
            (wasm-float? f64-value)
            (= 64 (wasm-float-width f64-value))
            (= #x0123456789abcdef (wasm-float-bits f64-value))))

     (let* ([instruction
             (parse-binary <wasm-instruction> #vu8(#x1c #x02 #x7f #x63 #x70))]
            [immediates (wasm-instruction-immediates instruction)]
            [types (vector-ref immediates 0)]
            [reference-type (vector-ref types 1)])
       (and (eq? 'select (wasm-instruction-mnemonic instruction))
            (= 1 (vector-length immediates))
            (= 2 (vector-length types))
            (eq? 'i32 (vector-ref types 0))
            (wasm-reference-type? reference-type)
            (wasm-reference-type-nullable? reference-type)
            (eq? 'func (wasm-reference-type-heap-type reference-type))))

     (and (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#xd0 #x03))
           'ref.null '#(3) '#() '#())
          (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#xd0 #x70))
           'ref.null '#(func) '#() '#()))

     (andmap
      (lambda (test)
        (let ([instruction (parse-binary <wasm-instruction> (car test))])
          (instruction-has-fields? instruction (cadr test) (caddr test) '#() '#())))
      (list (list #vu8(#xfc #x08 #x05 #x02) 'memory.init '#(2 5))
            (list #vu8(#xfc #x0a #x03 #x04) 'memory.copy '#(3 4))
            (list #vu8(#xfc #x0c #x06 #x07) 'table.init '#(7 6))
            (list #vu8(#xfc #x0e #x08 #x09) 'table.copy '#(8 9))))

     (let ([instruction (parse-binary <wasm-instruction> #vu8(#x02 #x40 #x01 #x0b))])
       (and (eq? 'block (wasm-instruction-mnemonic instruction))
            (= 1 (vector-length (wasm-instruction-immediates instruction)))
            (eq? 'empty
                 (wasm-block-type-kind
                  (vector-ref (wasm-instruction-immediates instruction) 0)))
            (= 1 (vector-length (wasm-instruction-body instruction)))
            (eq? 'nop
                 (wasm-instruction-mnemonic
                  (vector-ref (wasm-instruction-body instruction) 0)))
            (equal? '#() (wasm-instruction-alternate instruction))))

     (let ([instruction
            (parse-binary <wasm-instruction> #vu8(#x03 #x7f #x41 #x01 #x0b))])
       (and (eq? 'loop (wasm-instruction-mnemonic instruction))
            (eq? 'value-type
                 (wasm-block-type-kind
                  (vector-ref (wasm-instruction-immediates instruction) 0)))
            (instruction-has-fields?
             (vector-ref (wasm-instruction-body instruction) 0)
             'i32.const '#(1) '#() '#())))

     (let ([instruction (parse-binary <wasm-instruction> #vu8(#x04 #x40 #x01 #x0b))])
       (and (eq? 'if (wasm-instruction-mnemonic instruction))
            (= 1 (vector-length (wasm-instruction-body instruction)))
            (equal? '#() (wasm-instruction-alternate instruction))))

     (let ([instruction
            (parse-binary
             <wasm-instruction>
             #vu8(#x04 #x40 #x41 #x01 #x05 #x41 #x02 #x0b))])
       (and (eq? 'if (wasm-instruction-mnemonic instruction))
            (= 1 (vector-length (wasm-instruction-body instruction)))
            (= 1 (vector-length (wasm-instruction-alternate instruction)))
            (= 1
               (vector-ref
                (wasm-instruction-immediates
                 (vector-ref (wasm-instruction-body instruction) 0))
                0))
            (= 2
               (vector-ref
                (wasm-instruction-immediates
                 (vector-ref (wasm-instruction-alternate instruction) 0))
                0))))

     (let ([instruction
            (parse-binary
             <wasm-instruction>
             #vu8(#x02 #x40 #x04 #x40 #x03 #x40 #x01 #x0b #x05 #x00 #x0b #x0b))])
       (and (eq? 'block (wasm-instruction-mnemonic instruction))
            (eq? 'if
                 (wasm-instruction-mnemonic
                  (vector-ref (wasm-instruction-body instruction) 0)))
            (eq? 'loop
                 (wasm-instruction-mnemonic
                  (vector-ref
                   (wasm-instruction-body
                    (vector-ref (wasm-instruction-body instruction) 0))
                   0)))
            (eq? 'unreachable
                 (wasm-instruction-mnemonic
                  (vector-ref
                   (wasm-instruction-alternate
                    (vector-ref (wasm-instruction-body instruction) 0))
                   0)))))

     (let* ([instruction
             (parse-binary
              <wasm-instruction>
              #vu8(#x1f #x40 #x04
                    #x00 #x01 #x02
                    #x01 #x03 #x04
                    #x02 #x05
                    #x03 #x06
                    #x01 #x0b))]
            [catches (wasm-instruction-alternate instruction)])
       (and (eq? 'try-table (wasm-instruction-mnemonic instruction))
            (= 4 (vector-length catches))
            (eq? 'catch (wasm-catch-kind (vector-ref catches 0)))
            (= 1 (wasm-catch-tag-index (vector-ref catches 0)))
            (= 2 (wasm-catch-label-index (vector-ref catches 0)))
            (eq? 'catch-ref (wasm-catch-kind (vector-ref catches 1)))
            (= 3 (wasm-catch-tag-index (vector-ref catches 1)))
            (= 4 (wasm-catch-label-index (vector-ref catches 1)))
            (eq? 'catch-all (wasm-catch-kind (vector-ref catches 2)))
            (not (wasm-catch-tag-index (vector-ref catches 2)))
            (= 5 (wasm-catch-label-index (vector-ref catches 2)))
            (eq? 'catch-all-ref (wasm-catch-kind (vector-ref catches 3)))
            (not (wasm-catch-tag-index (vector-ref catches 3)))
            (= 6 (wasm-catch-label-index (vector-ref catches 3)))
            (eq? 'nop
                 (wasm-instruction-mnemonic
                  (vector-ref (wasm-instruction-body instruction) 0)))))

     (let ([expression (parse-binary <wasm-expression> #vu8(#x41 #x7f #x0b))])
       (and (= 1 (vector-length expression))
            (instruction-has-fields?
             (vector-ref expression 0) 'i32.const '#(-1) '#() '#())))

     (let* ([count 10000]
            [expression
             (parse-binary <wasm-expression> (make-nop-expression-bytes count))])
       (and (= count (vector-length expression))
            (eq? 'nop
                 (wasm-instruction-mnemonic (vector-ref expression 0)))
            (eq? 'nop
                 (wasm-instruction-mnemonic
                  (vector-ref expression (fx1- count))))))

     ;; error: memory flags are truncated before the offset.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-instruction> #vu8(#x28 #x02))))])
       (and (parser-error? err)
            (eq? 'unexpected-eof (parser-error-kind err))
            (= 2 (parser-error-offset err))
            (string-contains? (parser-error->string err) "unexpected EOF")))

     ;; error: a memory argument is truncated before its alignment flags.
     (error? (parse-binary <wasm-instruction> #vu8(#x28)))

     ;; error: a continued memory alignment encoding is truncated.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-instruction> #vu8(#x28 #x80))))])
       (and (parser-error? err)
            (eq? 'unexpected-eof (parser-error-kind err))
            (= 2 (parser-error-offset err))
            (string-contains? (parser-error->string err) "unexpected EOF")))

     ;; error: explicit-memory flags are truncated before the memory index.
     (error? (parse-binary <wasm-instruction> #vu8(#x28 #x40)))

     ;; error: explicit-memory flags are truncated before the offset.
     (error? (parse-binary <wasm-instruction> #vu8(#x28 #x40 #x01)))

     ;; error: memory argument flags 128 and above are reserved.
     (error? (parse-binary <wasm-instruction> #vu8(#x28 #x80 #x01)))

     ;; error: every unassigned final Core 3.0 one-byte opcode is rejected.
     (for-all
      (lambda (opcode)
        (let ([err
               (capture-parser-error
                (lambda ()
                  (parse-binary <wasm-instruction> (bytevector opcode))))])
          (and (parser-error? err)
               (eq? 'custom (parser-error-kind err))
               (= 1 (parser-error-offset err))
               (string=? "<fail-with>: unknown WebAssembly opcode"
                         (parser-error-message err)))))
      reserved-one-byte-opcodes)

     ;; error: an unknown one-byte opcode reports its consumed offset and category.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-instruction> #vu8(#x06))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 1 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "unknown WebAssembly opcode")))

     ;; error: an unknown opcode in an expression preserves its instruction failure.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-expression> #vu8(#x06 #x0b))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 1 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "unknown WebAssembly opcode")))

     ;; error: an unknown opcode in a block preserves its instruction failure.
     (let ([err
            (capture-parser-error
             (lambda ()
               (parse-binary <wasm-instruction> #vu8(#x02 #x40 #x06 #x0b))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 3 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "unknown WebAssembly opcode")))

     ;; error: an unknown opcode in an if alternate preserves its instruction failure.
     (let ([err
            (capture-parser-error
             (lambda ()
               (parse-binary <wasm-instruction>
                             #vu8(#x04 #x40 #x05 #x06 #x0b))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 4 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "unknown WebAssembly opcode")))

     (let* ([block
             (parse-binary <wasm-instruction>
                           #vu8(#x02 #x40 #xfb #x02 #x01 #x02 #x0b))]
            [instruction (vector-ref (wasm-instruction-body block) 0)])
       (instruction-has-fields? instruction 'struct.get '#(1 2) '#() '#()))

     ;; error: an unknown aggregate subopcode is rejected.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x7f)))

     ;; error: an unknown miscellaneous subopcode is rejected.
     (error? (parse-binary <wasm-instruction> #vu8(#xfc #x7f)))

     ;; error: an unknown multi-byte vector subopcode is rejected.
     (error? (parse-binary <wasm-instruction> #vu8(#xfd #x80 #x04)))

     ;; error: an aggregate prefix is truncated before its subopcode.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb)))

     ;; error: a miscellaneous prefix has a truncated bounded u32 subopcode.
     (error? (parse-binary <wasm-instruction> #vu8(#xfc #x80)))

     ;; error: else is a terminator, not a standalone instruction.
     (error? (parse-binary <wasm-instruction> #vu8(#x05)))

     ;; error: end is a terminator, not a standalone instruction.
     (error? (parse-binary <wasm-instruction> #vu8(#x0b)))

     ;; error: try-table accepts only catch kind bytes zero through three.
     (error? (parse-binary <wasm-instruction>
                           #vu8(#x1f #x40 #x01 #x04 #x00 #x0b)))

     ;; error: a block must end with an end terminator.
     (error? (parse-binary <wasm-instruction> #vu8(#x02 #x40 #x01)))

     ;; error: an expression must end with an end terminator.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-expression> #vu8(#x01))))])
       (and (parser-error? err)
            (eq? 'unexpected-eof (parser-error-kind err))
            (= 1 (parser-error-offset err))
            (string-contains? (parser-error->string err) "unexpected EOF")))

     ;; error: else is illegal in a block body.
     (error? (parse-binary <wasm-instruction> #vu8(#x02 #x40 #x05 #x0b)))

     ;; error: a second else is illegal in an if alternate.
     (error? (parse-binary <wasm-instruction> #vu8(#x04 #x40 #x05 #x05 #x0b)))

     ;; error: branch table omits its default label.
     (error? (parse-binary <wasm-instruction> #vu8(#x0e #x01 #x00)))

     ;; error: indirect call omits its table index.
     (error? (parse-binary <wasm-instruction> #vu8(#x11 #x00)))

     ;; error: typed select omits its declared value type.
     (error? (parse-binary <wasm-instruction> #vu8(#x1c #x01)))

     (andmap
      (lambda (test)
        (instruction-has-fields?
         (parse-binary <wasm-instruction> (car test))
         (cadr test) (caddr test) '#() '#()))
      (list (list #vu8(#xfb #x02 #x07 #x09) 'struct.get '#(7 9))
            (list #vu8(#xfb #x05 #x0a #x0b) 'struct.set '#(10 11))
            (list #vu8(#xfb #x08 #x03 #x04) 'array.new-fixed '#(3 4))
            (list #vu8(#xfb #x11 #x05 #x06) 'array.copy '#(5 6))
            (list #vu8(#xfb #x09 #x07 #x08) 'array.new-data '#(7 8))
            (list #vu8(#xfb #x12 #x09 #x0a) 'array.init-data '#(9 10))
            (list #vu8(#xfb #x0a #x0b #x0c) 'array.new-elem '#(11 12))
            (list #vu8(#xfb #x13 #x0d #x0e) 'array.init-elem '#(13 14))))

     (andmap
      (lambda (test)
        (let* ([instruction (parse-binary <wasm-instruction> (car test))]
               [reference-type (vector-ref (wasm-instruction-immediates instruction) 0)])
          (and (eq? (cadr test) (wasm-instruction-mnemonic instruction))
               (wasm-reference-type? reference-type)
               (eq? (caddr test) (wasm-reference-type-nullable? reference-type))
               (equal? (cadddr test) (wasm-reference-type-heap-type reference-type)))))
      (list (list #vu8(#xfb #x14 #x03) 'ref.test #f 3)
            (list #vu8(#xfb #x15 #x70) 'ref.test #t 'func)
            (list #vu8(#xfb #x16 #x04) 'ref.cast #f 4)
            (list #vu8(#xfb #x17 #x6d) 'ref.cast #t 'eq)))

     (let* ([instruction
             (parse-binary <wasm-instruction>
                           #vu8(#xfb #x18 #x03 #x05 #x70 #x03))]
            [immediates (wasm-instruction-immediates instruction)]
            [source-type (vector-ref immediates 1)]
            [target-type (vector-ref immediates 2)])
       (and (eq? 'br-on-cast (wasm-instruction-mnemonic instruction))
            (= 5 (vector-ref immediates 0))
            (wasm-reference-type-nullable? source-type)
            (eq? 'func (wasm-reference-type-heap-type source-type))
            (wasm-reference-type-nullable? target-type)
            (= 3 (wasm-reference-type-heap-type target-type))))

     (let* ([instruction
             (parse-binary <wasm-instruction>
                           #vu8(#xfb #x19 #x00 #x06 #x6c #x04))]
            [immediates (wasm-instruction-immediates instruction)])
       (and (eq? 'br-on-cast-fail (wasm-instruction-mnemonic instruction))
            (= 6 (vector-ref immediates 0))
            (not (wasm-reference-type-nullable? (vector-ref immediates 1)))
            (eq? 'i31
                 (wasm-reference-type-heap-type (vector-ref immediates 1)))
            (not (wasm-reference-type-nullable? (vector-ref immediates 2)))
            (= 4 (wasm-reference-type-heap-type (vector-ref immediates 2)))))

     (let* ([instruction
             (parse-binary <wasm-instruction>
                           #vu8(#xfb #x18 #x01 #x07 #x70 #x03))]
            [immediates (wasm-instruction-immediates instruction)]
            [source-type (vector-ref immediates 1)]
            [target-type (vector-ref immediates 2)])
       (and (eq? 'br-on-cast (wasm-instruction-mnemonic instruction))
            (= 7 (vector-ref immediates 0))
            (wasm-reference-type-nullable? source-type)
            (eq? 'func (wasm-reference-type-heap-type source-type))
            (not (wasm-reference-type-nullable? target-type))
            (= 3 (wasm-reference-type-heap-type target-type))))

     (let* ([instruction
             (parse-binary <wasm-instruction>
                           #vu8(#xfb #x19 #x02 #x08 #x04 #x6d))]
            [immediates (wasm-instruction-immediates instruction)]
            [source-type (vector-ref immediates 1)]
            [target-type (vector-ref immediates 2)])
       (and (eq? 'br-on-cast-fail (wasm-instruction-mnemonic instruction))
            (= 8 (vector-ref immediates 0))
            (not (wasm-reference-type-nullable? source-type))
            (= 4 (wasm-reference-type-heap-type source-type))
            (wasm-reference-type-nullable? target-type)
            (eq? 'eq (wasm-reference-type-heap-type target-type))))

     (let ([instruction
            (parse-binary <wasm-instruction>
                          #vu8(#xfd #x0c 0 1 2 3 4 5 6 7
                                8 9 10 11 12 13 14 15))])
       (and (eq? 'v128.const (wasm-instruction-mnemonic instruction))
            (equal? (bytevector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                    (vector-ref (wasm-instruction-immediates instruction) 0))))

     (let ([instruction
            (parse-binary <wasm-instruction>
                          #vu8(#xfd #x0d 31 30 29 28 27 26 25 24
                                23 22 21 20 19 18 17 16))])
       (and (eq? 'i8x16.shuffle (wasm-instruction-mnemonic instruction))
            (equal? (bytevector 31 30 29 28 27 26 25 24
                                23 22 21 20 19 18 17 16)
                    (vector-ref (wasm-instruction-immediates instruction) 0))))

     (andmap
      (lambda (test)
        (instruction-has-fields?
         (parse-binary <wasm-instruction> (car test))
         (cadr test) (vector (caddr test)) '#() '#()))
      (list (list #vu8(#xfd #x15 #x0f) 'i8x16.extract-lane-s 15)
            (list #vu8(#xfd #x18 #x07) 'i16x8.extract-lane-s 7)
            (list #vu8(#xfd #x1b #x03) 'i32x4.extract-lane 3)
            (list #vu8(#xfd #x1f #x03) 'f32x4.extract-lane 3)
            (list #vu8(#xfd #x1d #x01) 'i64x2.extract-lane 1)
            (list #vu8(#xfd #x21 #x01) 'f64x2.extract-lane 1)))

     (andmap
      (lambda (test)
        (let* ([instruction (parse-binary <wasm-instruction> (car test))]
               [immediates (wasm-instruction-immediates instruction)]
               [argument (vector-ref immediates 0)])
          (and (eq? (cadr test) (wasm-instruction-mnemonic instruction))
               (wasm-memory-argument? argument)
               (= (caddr test) (wasm-memory-argument-alignment argument))
               (= (cadddr test) (wasm-memory-argument-offset argument))
               (= (car (cddddr test)) (wasm-memory-argument-memory-index argument))
               (= (cadr (cddddr test)) (vector-ref immediates 1)))))
      (list (list #vu8(#xfd #x54 #x02 #x10 #x0f) 'v128.load8-lane 2 16 0 15)
            (list #vu8(#xfd #x55 #x01 #x11 #x07) 'v128.load16-lane 1 17 0 7)
            (list #vu8(#xfd #x56 #x40 #x03 #x12 #x03)
                  'v128.load32-lane 0 18 3 3)
            (list #vu8(#xfd #x5b #x03 #x13 #x01) 'v128.store64-lane 3 19 0 1)))

     (and (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#xfd #x80 #x02))
           'i8x16.relaxed-swizzle '#() '#() '#())
          (instruction-has-fields?
           (parse-binary <wasm-instruction> #vu8(#xfd #x93 #x02))
           'i32x4.relaxed-dot-i8x16-i7x16-add-s '#() '#() '#()))

     ;; error: a vector constant is truncated before its sixteenth byte.
     (error? (parse-binary <wasm-instruction>
                           #vu8(#xfd #x0c 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))

     ;; error: a shuffle constant is truncated before its sixteenth byte.
     (error? (parse-binary <wasm-instruction>
                           #vu8(#xfd #x0d 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))

     ;; error: every shuffle position rejects the first invalid lane value 32.
     (for-all
      (lambda (position)
        (parser-rejects? <wasm-instruction>
                         (make-shuffle-instruction-bytes position)))
      (integer-range 0 16))

     ;; error: shuffle lane 32 reports its exact consumed offset and category.
     (let ([err (capture-parser-error
                 (lambda ()
                   (parse-binary <wasm-instruction>
                                 (make-shuffle-instruction-bytes 0))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 3 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "invalid WebAssembly shuffle lane")))

     ;; error: each direct lane family rejects its first invalid lane.
     (andmap
      (lambda (bytes) (parser-rejects? <wasm-instruction> bytes))
      (list #vu8(#xfd #x15 16) #vu8(#xfd #x18 8)
            #vu8(#xfd #x1b 4) #vu8(#xfd #x1f 4)
            #vu8(#xfd #x1d 2) #vu8(#xfd #x21 2)))

     ;; error: each memory lane family rejects its first invalid lane.
     (andmap
      (lambda (bytes) (parser-rejects? <wasm-instruction> bytes))
      (list #vu8(#xfd #x54 0 0 16) #vu8(#xfd #x55 0 0 8)
            #vu8(#xfd #x56 0 0 4) #vu8(#xfd #x57 0 0 2)))

     ;; error: an invalid direct lane reports its exact consumed offset and category.
     (let ([err (capture-parser-error
                 (lambda ()
                   (parse-binary <wasm-instruction> #vu8(#xfd #x18 8))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 3 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "invalid WebAssembly lane index")))

     ;; error: structure fields are truncated before the field index.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x02 #x01)))

     ;; error: a truncated aggregate reports the exact missing-field offset.
     (let ([err (capture-parser-error
                 (lambda ()
                   (parse-binary <wasm-instruction> #vu8(#xfb #x02 #x01))))])
       (and (parser-error? err)
            (eq? 'unexpected-eof (parser-error-kind err))
            (= 3 (parser-error-offset err))
            (string-contains? (parser-error->string err) "unexpected EOF")))

     ;; error: a malformed aggregate type index retains a stable custom failure.
     (let ([err
            (capture-parser-error
             (lambda ()
               (parse-binary <wasm-instruction>
                             #vu8(#xfb #x02 #xff #xff #xff #xff #x10 #x00))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 7 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "integer has nonzero unused bits")))

     ;; error: fixed arrays are truncated before the fixed count.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x08 #x01)))

     ;; error: array copies are truncated before the source type index.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x11 #x01)))

     ;; error: type/data operands are truncated before the data index.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x12 #x01)))

     ;; error: type/element operands are truncated before the element index.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x13 #x01)))

     ;; error: reserved br-on-cast flag bits are rejected before the label.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x18 #x04 #x00 #x70 #x70)))

     ;; error: br-on-cast fields cannot omit the second heap type.
     (error? (parse-binary <wasm-instruction> #vu8(#xfb #x18 #x00 #x01 #x70)))

     ;; error: malformed abstract source heap types retain their location and diagnostic.
     (let ([err (capture-parser-error
                 (lambda ()
                   (parse-binary <wasm-instruction> #vu8(#xfb #x18 #x00 #x00 #x64 #x70))))])
       (and (parser-error? err)
            (= 5 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "undefined WebAssembly heap type")))

     ;; error: a malformed concrete heap type exceeds the signed 33-bit encoding.
     (error? (parse-binary <wasm-instruction>
                           #vu8(#xfb #x14 #x80 #x80 #x80 #x80 #x10)))

     ;; error: a representative bounded LEB failure preserves its exact location and message.
     (let ([err (capture-parser-error
                 (lambda ()
                   (parse-binary <wasm-u32> #vu8(#xff #xff #xff #xff #x10))))])
       (and (parser-error? err)
            (= 5 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "integer has nonzero unused bits")))

     ;; error: a representative malformed UTF-8 name preserves its location and message.
     (let ([err (capture-parser-error
                 (lambda () (parse-binary <wasm-name> #vu8(#x02 #xc3 #x28))))])
       (and (parser-error? err)
            (eq? 'custom (parser-error-kind err))
            (= 3 (parser-error-offset err))
            (string-contains? (parser-error-message err)
                              "invalid UTF-8 WebAssembly name")))

     )

(define expected-core-3-mnemonics
  '#(
   ;; parametric
   unreachable nop drop select
   ;; control
   block loop if else end br br-if br-table return call call-indirect br-on-null br-on-non-null
   br-on-cast br-on-cast-fail
   ;; exception/tail-call; final Core 3.0 has no continuation instructions
   throw throw-ref return-call return-call-indirect call-ref return-call-ref try-table
   ;; variable
   local.get local.set local.tee global.get global.set
   ;; table
   table.get table.set table.init elem.drop table.copy table.grow table.size table.fill
   ;; memory
   i32.load i64.load f32.load f64.load i32.load8-s i32.load8-u i32.load16-s i32.load16-u
   i64.load8-s i64.load8-u i64.load16-s i64.load16-u i64.load32-s i64.load32-u i32.store
   i64.store f32.store f64.store i32.store8 i32.store16 i64.store8 i64.store16 i64.store32
   memory.size memory.grow memory.init data.drop memory.copy memory.fill v128.load
   v128.load8x8-s v128.load8x8-u v128.load16x4-s v128.load16x4-u v128.load32x2-s
   v128.load32x2-u v128.load8-splat v128.load16-splat v128.load32-splat v128.load64-splat
   v128.store v128.load8-lane v128.load16-lane v128.load32-lane v128.load64-lane
   v128.store8-lane v128.store16-lane v128.store32-lane v128.store64-lane v128.load32-zero
   v128.load64-zero
   ;; numeric
   i32.const i64.const f32.const f64.const i32.eqz i32.eq i32.ne i32.lt-s i32.lt-u i32.gt-s
   i32.gt-u i32.le-s i32.le-u i32.ge-s i32.ge-u i64.eqz i64.eq i64.ne i64.lt-s i64.lt-u
   i64.gt-s i64.gt-u i64.le-s i64.le-u i64.ge-s i64.ge-u f32.eq f32.ne f32.lt f32.gt f32.le
   f32.ge f64.eq f64.ne f64.lt f64.gt f64.le f64.ge i32.clz i32.ctz i32.popcnt i32.add i32.sub
   i32.mul i32.div-s i32.div-u i32.rem-s i32.rem-u i32.and i32.or i32.xor i32.shl i32.shr-s
   i32.shr-u i32.rotl i32.rotr i64.clz i64.ctz i64.popcnt i64.add i64.sub i64.mul i64.div-s
   i64.div-u i64.rem-s i64.rem-u i64.and i64.or i64.xor i64.shl i64.shr-s i64.shr-u i64.rotl
   i64.rotr f32.abs f32.neg f32.ceil f32.floor f32.trunc f32.nearest f32.sqrt f32.add f32.sub
   f32.mul f32.div f32.min f32.max f32.copysign f64.abs f64.neg f64.ceil f64.floor f64.trunc
   f64.nearest f64.sqrt f64.add f64.sub f64.mul f64.div f64.min f64.max f64.copysign
   i32.wrap-i64 i32.trunc-f32-s i32.trunc-f32-u i32.trunc-f64-s i32.trunc-f64-u
   i64.extend-i32-s i64.extend-i32-u i64.trunc-f32-s i64.trunc-f32-u i64.trunc-f64-s
   i64.trunc-f64-u f32.convert-i32-s f32.convert-i32-u f32.convert-i64-s f32.convert-i64-u
   f32.demote-f64 f64.convert-i32-s f64.convert-i32-u f64.convert-i64-s f64.convert-i64-u
   f64.promote-f32 i32.reinterpret-f32 i64.reinterpret-f64 f32.reinterpret-i32
   f64.reinterpret-i64 i32.extend8-s i32.extend16-s i64.extend8-s i64.extend16-s i64.extend32-s
   i32.trunc-sat-f32-s i32.trunc-sat-f32-u i32.trunc-sat-f64-s i32.trunc-sat-f64-u
   i64.trunc-sat-f32-s i64.trunc-sat-f32-u i64.trunc-sat-f64-s i64.trunc-sat-f64-u
   ;; reference
   ref.null ref.is-null ref.func ref.eq ref.as-non-null ref.test ref.cast any.convert-extern
   extern.convert-any ref.i31
   ;; aggregate/gc
   struct.new struct.new-default struct.get struct.get-s struct.get-u struct.set array.new
   array.new-default array.new-fixed array.new-data array.new-elem array.get array.get-s
   array.get-u array.set array.len array.fill array.copy array.init-data array.init-elem
   i31.get-s i31.get-u
   ;; vector
   v128.const i8x16.shuffle i8x16.swizzle i8x16.splat i16x8.splat i32x4.splat i64x2.splat
   f32x4.splat f64x2.splat i8x16.extract-lane-s i8x16.extract-lane-u i8x16.replace-lane
   i16x8.extract-lane-s i16x8.extract-lane-u i16x8.replace-lane i32x4.extract-lane
   i32x4.replace-lane i64x2.extract-lane i64x2.replace-lane f32x4.extract-lane
   f32x4.replace-lane f64x2.extract-lane f64x2.replace-lane i8x16.eq i8x16.ne i8x16.lt-s
   i8x16.lt-u i8x16.gt-s i8x16.gt-u i8x16.le-s i8x16.le-u i8x16.ge-s i8x16.ge-u i16x8.eq
   i16x8.ne i16x8.lt-s i16x8.lt-u i16x8.gt-s i16x8.gt-u i16x8.le-s i16x8.le-u i16x8.ge-s
   i16x8.ge-u i32x4.eq i32x4.ne i32x4.lt-s i32x4.lt-u i32x4.gt-s i32x4.gt-u i32x4.le-s
   i32x4.le-u i32x4.ge-s i32x4.ge-u f32x4.eq f32x4.ne f32x4.lt f32x4.gt f32x4.le f32x4.ge
   f64x2.eq f64x2.ne f64x2.lt f64x2.gt f64x2.le f64x2.ge v128.not v128.and v128.andnot v128.or
   v128.xor v128.bitselect v128.any-true f32x4.demote-f64x2-zero f64x2.promote-low-f32x4
   i8x16.abs i8x16.neg i8x16.popcnt i8x16.all-true i8x16.bitmask i8x16.narrow-i16x8-s
   i8x16.narrow-i16x8-u f32x4.ceil f32x4.floor f32x4.trunc f32x4.nearest i8x16.shl i8x16.shr-s
   i8x16.shr-u i8x16.add i8x16.add-sat-s i8x16.add-sat-u i8x16.sub i8x16.sub-sat-s
   i8x16.sub-sat-u f64x2.ceil f64x2.floor i8x16.min-s i8x16.min-u i8x16.max-s i8x16.max-u
   f64x2.trunc i8x16.avgr-u i16x8.extadd-pairwise-i8x16-s i16x8.extadd-pairwise-i8x16-u
   i32x4.extadd-pairwise-i16x8-s i32x4.extadd-pairwise-i16x8-u i16x8.abs i16x8.neg
   i16x8.q15mulr-sat-s i16x8.all-true i16x8.bitmask i16x8.narrow-i32x4-s i16x8.narrow-i32x4-u
   i16x8.extend-low-i8x16-s i16x8.extend-high-i8x16-s i16x8.extend-low-i8x16-u
   i16x8.extend-high-i8x16-u i16x8.shl i16x8.shr-s i16x8.shr-u i16x8.add i16x8.add-sat-s
   i16x8.add-sat-u i16x8.sub i16x8.sub-sat-s i16x8.sub-sat-u f64x2.nearest i16x8.mul
   i16x8.min-s i16x8.min-u i16x8.max-s i16x8.max-u i16x8.avgr-u i16x8.extmul-low-i8x16-s
   i16x8.extmul-high-i8x16-s i16x8.extmul-low-i8x16-u i16x8.extmul-high-i8x16-u i32x4.abs
   i32x4.neg i32x4.all-true i32x4.bitmask i32x4.extend-low-i16x8-s i32x4.extend-high-i16x8-s
   i32x4.extend-low-i16x8-u i32x4.extend-high-i16x8-u i32x4.shl i32x4.shr-s i32x4.shr-u
   i32x4.add i32x4.sub i32x4.mul i32x4.min-s i32x4.min-u i32x4.max-s i32x4.max-u
   i32x4.dot-i16x8-s i32x4.extmul-low-i16x8-s i32x4.extmul-high-i16x8-s
   i32x4.extmul-low-i16x8-u i32x4.extmul-high-i16x8-u i64x2.abs i64x2.neg i64x2.all-true
   i64x2.bitmask i64x2.extend-low-i32x4-s i64x2.extend-high-i32x4-s i64x2.extend-low-i32x4-u
   i64x2.extend-high-i32x4-u i64x2.shl i64x2.shr-s i64x2.shr-u i64x2.add i64x2.sub i64x2.mul
   i64x2.eq i64x2.ne i64x2.lt-s i64x2.gt-s i64x2.le-s i64x2.ge-s i64x2.extmul-low-i32x4-s
   i64x2.extmul-high-i32x4-s i64x2.extmul-low-i32x4-u i64x2.extmul-high-i32x4-u f32x4.abs
   f32x4.neg f32x4.sqrt f32x4.add f32x4.sub f32x4.mul f32x4.div f32x4.min f32x4.max f32x4.pmin
   f32x4.pmax f64x2.abs f64x2.neg f64x2.sqrt f64x2.add f64x2.sub f64x2.mul f64x2.div f64x2.min
   f64x2.max f64x2.pmin f64x2.pmax i32x4.trunc-sat-f32x4-s i32x4.trunc-sat-f32x4-u
   f32x4.convert-i32x4-s f32x4.convert-i32x4-u i32x4.trunc-sat-f64x2-s-zero
   i32x4.trunc-sat-f64x2-u-zero f64x2.convert-low-i32x4-s f64x2.convert-low-i32x4-u
   ;; relaxed-simd
   i8x16.relaxed-swizzle i32x4.relaxed-trunc-f32x4-s i32x4.relaxed-trunc-f32x4-u
   i32x4.relaxed-trunc-f64x2-s-zero i32x4.relaxed-trunc-f64x2-u-zero f32x4.relaxed-madd
   f32x4.relaxed-nmadd f64x2.relaxed-madd f64x2.relaxed-nmadd i8x16.relaxed-laneselect
   i16x8.relaxed-laneselect i32x4.relaxed-laneselect i64x2.relaxed-laneselect f32x4.relaxed-min
   f32x4.relaxed-max f64x2.relaxed-min f64x2.relaxed-max i16x8.relaxed-q15mulr-s
   i16x8.relaxed-dot-i8x16-i7x16-s i32x4.relaxed-dot-i8x16-i7x16-add-s
   ))

;; Source: WebAssembly/spec `wg-3.0` commit `9d36019973201a19f9c9ebb0f10828b2fe2374aa`.
(define expected-core-3-binary-assignments
  (vector
   ;; one-byte instructions
   (vector #f #x0 'unreachable)
   (vector #f #x1 'nop)
   (vector #f #x2 'block)
   (vector #f #x3 'loop)
   (vector #f #x4 'if)
   (vector #f #x5 'else)
   (vector #f #x8 'throw)
   (vector #f #xa 'throw-ref)
   (vector #f #xb 'end)
   (vector #f #xc 'br)
   (vector #f #xd 'br-if)
   (vector #f #xe 'br-table)
   (vector #f #xf 'return)
   (vector #f #x10 'call)
   (vector #f #x11 'call-indirect)
   (vector #f #x12 'return-call)
   (vector #f #x13 'return-call-indirect)
   (vector #f #x14 'call-ref)
   (vector #f #x15 'return-call-ref)
   (vector #f #x1a 'drop)
   (vector #f #x1b 'select)
   (vector #f #x1c 'select)
   (vector #f #x1f 'try-table)
   (vector #f #x20 'local.get)
   (vector #f #x21 'local.set)
   (vector #f #x22 'local.tee)
   (vector #f #x23 'global.get)
   (vector #f #x24 'global.set)
   (vector #f #x25 'table.get)
   (vector #f #x26 'table.set)
   (vector #f #x28 'i32.load)
   (vector #f #x29 'i64.load)
   (vector #f #x2a 'f32.load)
   (vector #f #x2b 'f64.load)
   (vector #f #x2c 'i32.load8-s)
   (vector #f #x2d 'i32.load8-u)
   (vector #f #x2e 'i32.load16-s)
   (vector #f #x2f 'i32.load16-u)
   (vector #f #x30 'i64.load8-s)
   (vector #f #x31 'i64.load8-u)
   (vector #f #x32 'i64.load16-s)
   (vector #f #x33 'i64.load16-u)
   (vector #f #x34 'i64.load32-s)
   (vector #f #x35 'i64.load32-u)
   (vector #f #x36 'i32.store)
   (vector #f #x37 'i64.store)
   (vector #f #x38 'f32.store)
   (vector #f #x39 'f64.store)
   (vector #f #x3a 'i32.store8)
   (vector #f #x3b 'i32.store16)
   (vector #f #x3c 'i64.store8)
   (vector #f #x3d 'i64.store16)
   (vector #f #x3e 'i64.store32)
   (vector #f #x3f 'memory.size)
   (vector #f #x40 'memory.grow)
   (vector #f #x41 'i32.const)
   (vector #f #x42 'i64.const)
   (vector #f #x43 'f32.const)
   (vector #f #x44 'f64.const)
   (vector #f #x45 'i32.eqz)
   (vector #f #x46 'i32.eq)
   (vector #f #x47 'i32.ne)
   (vector #f #x48 'i32.lt-s)
   (vector #f #x49 'i32.lt-u)
   (vector #f #x4a 'i32.gt-s)
   (vector #f #x4b 'i32.gt-u)
   (vector #f #x4c 'i32.le-s)
   (vector #f #x4d 'i32.le-u)
   (vector #f #x4e 'i32.ge-s)
   (vector #f #x4f 'i32.ge-u)
   (vector #f #x50 'i64.eqz)
   (vector #f #x51 'i64.eq)
   (vector #f #x52 'i64.ne)
   (vector #f #x53 'i64.lt-s)
   (vector #f #x54 'i64.lt-u)
   (vector #f #x55 'i64.gt-s)
   (vector #f #x56 'i64.gt-u)
   (vector #f #x57 'i64.le-s)
   (vector #f #x58 'i64.le-u)
   (vector #f #x59 'i64.ge-s)
   (vector #f #x5a 'i64.ge-u)
   (vector #f #x5b 'f32.eq)
   (vector #f #x5c 'f32.ne)
   (vector #f #x5d 'f32.lt)
   (vector #f #x5e 'f32.gt)
   (vector #f #x5f 'f32.le)
   (vector #f #x60 'f32.ge)
   (vector #f #x61 'f64.eq)
   (vector #f #x62 'f64.ne)
   (vector #f #x63 'f64.lt)
   (vector #f #x64 'f64.gt)
   (vector #f #x65 'f64.le)
   (vector #f #x66 'f64.ge)
   (vector #f #x67 'i32.clz)
   (vector #f #x68 'i32.ctz)
   (vector #f #x69 'i32.popcnt)
   (vector #f #x6a 'i32.add)
   (vector #f #x6b 'i32.sub)
   (vector #f #x6c 'i32.mul)
   (vector #f #x6d 'i32.div-s)
   (vector #f #x6e 'i32.div-u)
   (vector #f #x6f 'i32.rem-s)
   (vector #f #x70 'i32.rem-u)
   (vector #f #x71 'i32.and)
   (vector #f #x72 'i32.or)
   (vector #f #x73 'i32.xor)
   (vector #f #x74 'i32.shl)
   (vector #f #x75 'i32.shr-s)
   (vector #f #x76 'i32.shr-u)
   (vector #f #x77 'i32.rotl)
   (vector #f #x78 'i32.rotr)
   (vector #f #x79 'i64.clz)
   (vector #f #x7a 'i64.ctz)
   (vector #f #x7b 'i64.popcnt)
   (vector #f #x7c 'i64.add)
   (vector #f #x7d 'i64.sub)
   (vector #f #x7e 'i64.mul)
   (vector #f #x7f 'i64.div-s)
   (vector #f #x80 'i64.div-u)
   (vector #f #x81 'i64.rem-s)
   (vector #f #x82 'i64.rem-u)
   (vector #f #x83 'i64.and)
   (vector #f #x84 'i64.or)
   (vector #f #x85 'i64.xor)
   (vector #f #x86 'i64.shl)
   (vector #f #x87 'i64.shr-s)
   (vector #f #x88 'i64.shr-u)
   (vector #f #x89 'i64.rotl)
   (vector #f #x8a 'i64.rotr)
   (vector #f #x8b 'f32.abs)
   (vector #f #x8c 'f32.neg)
   (vector #f #x8d 'f32.ceil)
   (vector #f #x8e 'f32.floor)
   (vector #f #x8f 'f32.trunc)
   (vector #f #x90 'f32.nearest)
   (vector #f #x91 'f32.sqrt)
   (vector #f #x92 'f32.add)
   (vector #f #x93 'f32.sub)
   (vector #f #x94 'f32.mul)
   (vector #f #x95 'f32.div)
   (vector #f #x96 'f32.min)
   (vector #f #x97 'f32.max)
   (vector #f #x98 'f32.copysign)
   (vector #f #x99 'f64.abs)
   (vector #f #x9a 'f64.neg)
   (vector #f #x9b 'f64.ceil)
   (vector #f #x9c 'f64.floor)
   (vector #f #x9d 'f64.trunc)
   (vector #f #x9e 'f64.nearest)
   (vector #f #x9f 'f64.sqrt)
   (vector #f #xa0 'f64.add)
   (vector #f #xa1 'f64.sub)
   (vector #f #xa2 'f64.mul)
   (vector #f #xa3 'f64.div)
   (vector #f #xa4 'f64.min)
   (vector #f #xa5 'f64.max)
   (vector #f #xa6 'f64.copysign)
   (vector #f #xa7 'i32.wrap-i64)
   (vector #f #xa8 'i32.trunc-f32-s)
   (vector #f #xa9 'i32.trunc-f32-u)
   (vector #f #xaa 'i32.trunc-f64-s)
   (vector #f #xab 'i32.trunc-f64-u)
   (vector #f #xac 'i64.extend-i32-s)
   (vector #f #xad 'i64.extend-i32-u)
   (vector #f #xae 'i64.trunc-f32-s)
   (vector #f #xaf 'i64.trunc-f32-u)
   (vector #f #xb0 'i64.trunc-f64-s)
   (vector #f #xb1 'i64.trunc-f64-u)
   (vector #f #xb2 'f32.convert-i32-s)
   (vector #f #xb3 'f32.convert-i32-u)
   (vector #f #xb4 'f32.convert-i64-s)
   (vector #f #xb5 'f32.convert-i64-u)
   (vector #f #xb6 'f32.demote-f64)
   (vector #f #xb7 'f64.convert-i32-s)
   (vector #f #xb8 'f64.convert-i32-u)
   (vector #f #xb9 'f64.convert-i64-s)
   (vector #f #xba 'f64.convert-i64-u)
   (vector #f #xbb 'f64.promote-f32)
   (vector #f #xbc 'i32.reinterpret-f32)
   (vector #f #xbd 'i64.reinterpret-f64)
   (vector #f #xbe 'f32.reinterpret-i32)
   (vector #f #xbf 'f64.reinterpret-i64)
   (vector #f #xc0 'i32.extend8-s)
   (vector #f #xc1 'i32.extend16-s)
   (vector #f #xc2 'i64.extend8-s)
   (vector #f #xc3 'i64.extend16-s)
   (vector #f #xc4 'i64.extend32-s)
   (vector #f #xd0 'ref.null)
   (vector #f #xd1 'ref.is-null)
   (vector #f #xd2 'ref.func)
   (vector #f #xd3 'ref.eq)
   (vector #f #xd4 'ref.as-non-null)
   (vector #f #xd5 'br-on-null)
   (vector #f #xd6 'br-on-non-null)
   ;; #xfb aggregate and GC instructions
   (vector #xfb #x0 'struct.new)
   (vector #xfb #x1 'struct.new-default)
   (vector #xfb #x2 'struct.get)
   (vector #xfb #x3 'struct.get-s)
   (vector #xfb #x4 'struct.get-u)
   (vector #xfb #x5 'struct.set)
   (vector #xfb #x6 'array.new)
   (vector #xfb #x7 'array.new-default)
   (vector #xfb #x8 'array.new-fixed)
   (vector #xfb #x9 'array.new-data)
   (vector #xfb #xa 'array.new-elem)
   (vector #xfb #xb 'array.get)
   (vector #xfb #xc 'array.get-s)
   (vector #xfb #xd 'array.get-u)
   (vector #xfb #xe 'array.set)
   (vector #xfb #xf 'array.len)
   (vector #xfb #x10 'array.fill)
   (vector #xfb #x11 'array.copy)
   (vector #xfb #x12 'array.init-data)
   (vector #xfb #x13 'array.init-elem)
   (vector #xfb #x14 'ref.test)
   (vector #xfb #x15 'ref.test)
   (vector #xfb #x16 'ref.cast)
   (vector #xfb #x17 'ref.cast)
   (vector #xfb #x18 'br-on-cast)
   (vector #xfb #x19 'br-on-cast-fail)
   (vector #xfb #x1a 'any.convert-extern)
   (vector #xfb #x1b 'extern.convert-any)
   (vector #xfb #x1c 'ref.i31)
   (vector #xfb #x1d 'i31.get-s)
   (vector #xfb #x1e 'i31.get-u)
   ;; #xfc saturating conversion and bulk instructions
   (vector #xfc #x0 'i32.trunc-sat-f32-s)
   (vector #xfc #x1 'i32.trunc-sat-f32-u)
   (vector #xfc #x2 'i32.trunc-sat-f64-s)
   (vector #xfc #x3 'i32.trunc-sat-f64-u)
   (vector #xfc #x4 'i64.trunc-sat-f32-s)
   (vector #xfc #x5 'i64.trunc-sat-f32-u)
   (vector #xfc #x6 'i64.trunc-sat-f64-s)
   (vector #xfc #x7 'i64.trunc-sat-f64-u)
   (vector #xfc #x8 'memory.init)
   (vector #xfc #x9 'data.drop)
   (vector #xfc #xa 'memory.copy)
   (vector #xfc #xb 'memory.fill)
   (vector #xfc #xc 'table.init)
   (vector #xfc #xd 'elem.drop)
   (vector #xfc #xe 'table.copy)
   (vector #xfc #xf 'table.grow)
   (vector #xfc #x10 'table.size)
   (vector #xfc #x11 'table.fill)
   ;; #xfd vector and relaxed SIMD instructions
   (vector #xfd #x0 'v128.load)
   (vector #xfd #x1 'v128.load8x8-s)
   (vector #xfd #x2 'v128.load8x8-u)
   (vector #xfd #x3 'v128.load16x4-s)
   (vector #xfd #x4 'v128.load16x4-u)
   (vector #xfd #x5 'v128.load32x2-s)
   (vector #xfd #x6 'v128.load32x2-u)
   (vector #xfd #x7 'v128.load8-splat)
   (vector #xfd #x8 'v128.load16-splat)
   (vector #xfd #x9 'v128.load32-splat)
   (vector #xfd #xa 'v128.load64-splat)
   (vector #xfd #xb 'v128.store)
   (vector #xfd #xc 'v128.const)
   (vector #xfd #xd 'i8x16.shuffle)
   (vector #xfd #xe 'i8x16.swizzle)
   (vector #xfd #xf 'i8x16.splat)
   (vector #xfd #x10 'i16x8.splat)
   (vector #xfd #x11 'i32x4.splat)
   (vector #xfd #x12 'i64x2.splat)
   (vector #xfd #x13 'f32x4.splat)
   (vector #xfd #x14 'f64x2.splat)
   (vector #xfd #x15 'i8x16.extract-lane-s)
   (vector #xfd #x16 'i8x16.extract-lane-u)
   (vector #xfd #x17 'i8x16.replace-lane)
   (vector #xfd #x18 'i16x8.extract-lane-s)
   (vector #xfd #x19 'i16x8.extract-lane-u)
   (vector #xfd #x1a 'i16x8.replace-lane)
   (vector #xfd #x1b 'i32x4.extract-lane)
   (vector #xfd #x1c 'i32x4.replace-lane)
   (vector #xfd #x1d 'i64x2.extract-lane)
   (vector #xfd #x1e 'i64x2.replace-lane)
   (vector #xfd #x1f 'f32x4.extract-lane)
   (vector #xfd #x20 'f32x4.replace-lane)
   (vector #xfd #x21 'f64x2.extract-lane)
   (vector #xfd #x22 'f64x2.replace-lane)
   (vector #xfd #x23 'i8x16.eq)
   (vector #xfd #x24 'i8x16.ne)
   (vector #xfd #x25 'i8x16.lt-s)
   (vector #xfd #x26 'i8x16.lt-u)
   (vector #xfd #x27 'i8x16.gt-s)
   (vector #xfd #x28 'i8x16.gt-u)
   (vector #xfd #x29 'i8x16.le-s)
   (vector #xfd #x2a 'i8x16.le-u)
   (vector #xfd #x2b 'i8x16.ge-s)
   (vector #xfd #x2c 'i8x16.ge-u)
   (vector #xfd #x2d 'i16x8.eq)
   (vector #xfd #x2e 'i16x8.ne)
   (vector #xfd #x2f 'i16x8.lt-s)
   (vector #xfd #x30 'i16x8.lt-u)
   (vector #xfd #x31 'i16x8.gt-s)
   (vector #xfd #x32 'i16x8.gt-u)
   (vector #xfd #x33 'i16x8.le-s)
   (vector #xfd #x34 'i16x8.le-u)
   (vector #xfd #x35 'i16x8.ge-s)
   (vector #xfd #x36 'i16x8.ge-u)
   (vector #xfd #x37 'i32x4.eq)
   (vector #xfd #x38 'i32x4.ne)
   (vector #xfd #x39 'i32x4.lt-s)
   (vector #xfd #x3a 'i32x4.lt-u)
   (vector #xfd #x3b 'i32x4.gt-s)
   (vector #xfd #x3c 'i32x4.gt-u)
   (vector #xfd #x3d 'i32x4.le-s)
   (vector #xfd #x3e 'i32x4.le-u)
   (vector #xfd #x3f 'i32x4.ge-s)
   (vector #xfd #x40 'i32x4.ge-u)
   (vector #xfd #x41 'f32x4.eq)
   (vector #xfd #x42 'f32x4.ne)
   (vector #xfd #x43 'f32x4.lt)
   (vector #xfd #x44 'f32x4.gt)
   (vector #xfd #x45 'f32x4.le)
   (vector #xfd #x46 'f32x4.ge)
   (vector #xfd #x47 'f64x2.eq)
   (vector #xfd #x48 'f64x2.ne)
   (vector #xfd #x49 'f64x2.lt)
   (vector #xfd #x4a 'f64x2.gt)
   (vector #xfd #x4b 'f64x2.le)
   (vector #xfd #x4c 'f64x2.ge)
   (vector #xfd #x4d 'v128.not)
   (vector #xfd #x4e 'v128.and)
   (vector #xfd #x4f 'v128.andnot)
   (vector #xfd #x50 'v128.or)
   (vector #xfd #x51 'v128.xor)
   (vector #xfd #x52 'v128.bitselect)
   (vector #xfd #x53 'v128.any-true)
   (vector #xfd #x54 'v128.load8-lane)
   (vector #xfd #x55 'v128.load16-lane)
   (vector #xfd #x56 'v128.load32-lane)
   (vector #xfd #x57 'v128.load64-lane)
   (vector #xfd #x58 'v128.store8-lane)
   (vector #xfd #x59 'v128.store16-lane)
   (vector #xfd #x5a 'v128.store32-lane)
   (vector #xfd #x5b 'v128.store64-lane)
   (vector #xfd #x5c 'v128.load32-zero)
   (vector #xfd #x5d 'v128.load64-zero)
   (vector #xfd #x5e 'f32x4.demote-f64x2-zero)
   (vector #xfd #x5f 'f64x2.promote-low-f32x4)
   (vector #xfd #x60 'i8x16.abs)
   (vector #xfd #x61 'i8x16.neg)
   (vector #xfd #x62 'i8x16.popcnt)
   (vector #xfd #x63 'i8x16.all-true)
   (vector #xfd #x64 'i8x16.bitmask)
   (vector #xfd #x65 'i8x16.narrow-i16x8-s)
   (vector #xfd #x66 'i8x16.narrow-i16x8-u)
   (vector #xfd #x67 'f32x4.ceil)
   (vector #xfd #x68 'f32x4.floor)
   (vector #xfd #x69 'f32x4.trunc)
   (vector #xfd #x6a 'f32x4.nearest)
   (vector #xfd #x6b 'i8x16.shl)
   (vector #xfd #x6c 'i8x16.shr-s)
   (vector #xfd #x6d 'i8x16.shr-u)
   (vector #xfd #x6e 'i8x16.add)
   (vector #xfd #x6f 'i8x16.add-sat-s)
   (vector #xfd #x70 'i8x16.add-sat-u)
   (vector #xfd #x71 'i8x16.sub)
   (vector #xfd #x72 'i8x16.sub-sat-s)
   (vector #xfd #x73 'i8x16.sub-sat-u)
   (vector #xfd #x74 'f64x2.ceil)
   (vector #xfd #x75 'f64x2.floor)
   (vector #xfd #x76 'i8x16.min-s)
   (vector #xfd #x77 'i8x16.min-u)
   (vector #xfd #x78 'i8x16.max-s)
   (vector #xfd #x79 'i8x16.max-u)
   (vector #xfd #x7a 'f64x2.trunc)
   (vector #xfd #x7b 'i8x16.avgr-u)
   (vector #xfd #x7c 'i16x8.extadd-pairwise-i8x16-s)
   (vector #xfd #x7d 'i16x8.extadd-pairwise-i8x16-u)
   (vector #xfd #x7e 'i32x4.extadd-pairwise-i16x8-s)
   (vector #xfd #x7f 'i32x4.extadd-pairwise-i16x8-u)
   (vector #xfd #x80 'i16x8.abs)
   (vector #xfd #x81 'i16x8.neg)
   (vector #xfd #x82 'i16x8.q15mulr-sat-s)
   (vector #xfd #x83 'i16x8.all-true)
   (vector #xfd #x84 'i16x8.bitmask)
   (vector #xfd #x85 'i16x8.narrow-i32x4-s)
   (vector #xfd #x86 'i16x8.narrow-i32x4-u)
   (vector #xfd #x87 'i16x8.extend-low-i8x16-s)
   (vector #xfd #x88 'i16x8.extend-high-i8x16-s)
   (vector #xfd #x89 'i16x8.extend-low-i8x16-u)
   (vector #xfd #x8a 'i16x8.extend-high-i8x16-u)
   (vector #xfd #x8b 'i16x8.shl)
   (vector #xfd #x8c 'i16x8.shr-s)
   (vector #xfd #x8d 'i16x8.shr-u)
   (vector #xfd #x8e 'i16x8.add)
   (vector #xfd #x8f 'i16x8.add-sat-s)
   (vector #xfd #x90 'i16x8.add-sat-u)
   (vector #xfd #x91 'i16x8.sub)
   (vector #xfd #x92 'i16x8.sub-sat-s)
   (vector #xfd #x93 'i16x8.sub-sat-u)
   (vector #xfd #x94 'f64x2.nearest)
   (vector #xfd #x95 'i16x8.mul)
   (vector #xfd #x96 'i16x8.min-s)
   (vector #xfd #x97 'i16x8.min-u)
   (vector #xfd #x98 'i16x8.max-s)
   (vector #xfd #x99 'i16x8.max-u)
   (vector #xfd #x9b 'i16x8.avgr-u)
   (vector #xfd #x9c 'i16x8.extmul-low-i8x16-s)
   (vector #xfd #x9d 'i16x8.extmul-high-i8x16-s)
   (vector #xfd #x9e 'i16x8.extmul-low-i8x16-u)
   (vector #xfd #x9f 'i16x8.extmul-high-i8x16-u)
   (vector #xfd #xa0 'i32x4.abs)
   (vector #xfd #xa1 'i32x4.neg)
   (vector #xfd #xa3 'i32x4.all-true)
   (vector #xfd #xa4 'i32x4.bitmask)
   (vector #xfd #xa7 'i32x4.extend-low-i16x8-s)
   (vector #xfd #xa8 'i32x4.extend-high-i16x8-s)
   (vector #xfd #xa9 'i32x4.extend-low-i16x8-u)
   (vector #xfd #xaa 'i32x4.extend-high-i16x8-u)
   (vector #xfd #xab 'i32x4.shl)
   (vector #xfd #xac 'i32x4.shr-s)
   (vector #xfd #xad 'i32x4.shr-u)
   (vector #xfd #xae 'i32x4.add)
   (vector #xfd #xb1 'i32x4.sub)
   (vector #xfd #xb5 'i32x4.mul)
   (vector #xfd #xb6 'i32x4.min-s)
   (vector #xfd #xb7 'i32x4.min-u)
   (vector #xfd #xb8 'i32x4.max-s)
   (vector #xfd #xb9 'i32x4.max-u)
   (vector #xfd #xba 'i32x4.dot-i16x8-s)
   (vector #xfd #xbc 'i32x4.extmul-low-i16x8-s)
   (vector #xfd #xbd 'i32x4.extmul-high-i16x8-s)
   (vector #xfd #xbe 'i32x4.extmul-low-i16x8-u)
   (vector #xfd #xbf 'i32x4.extmul-high-i16x8-u)
   (vector #xfd #xc0 'i64x2.abs)
   (vector #xfd #xc1 'i64x2.neg)
   (vector #xfd #xc3 'i64x2.all-true)
   (vector #xfd #xc4 'i64x2.bitmask)
   (vector #xfd #xc7 'i64x2.extend-low-i32x4-s)
   (vector #xfd #xc8 'i64x2.extend-high-i32x4-s)
   (vector #xfd #xc9 'i64x2.extend-low-i32x4-u)
   (vector #xfd #xca 'i64x2.extend-high-i32x4-u)
   (vector #xfd #xcb 'i64x2.shl)
   (vector #xfd #xcc 'i64x2.shr-s)
   (vector #xfd #xcd 'i64x2.shr-u)
   (vector #xfd #xce 'i64x2.add)
   (vector #xfd #xd1 'i64x2.sub)
   (vector #xfd #xd5 'i64x2.mul)
   (vector #xfd #xd6 'i64x2.eq)
   (vector #xfd #xd7 'i64x2.ne)
   (vector #xfd #xd8 'i64x2.lt-s)
   (vector #xfd #xd9 'i64x2.gt-s)
   (vector #xfd #xda 'i64x2.le-s)
   (vector #xfd #xdb 'i64x2.ge-s)
   (vector #xfd #xdc 'i64x2.extmul-low-i32x4-s)
   (vector #xfd #xdd 'i64x2.extmul-high-i32x4-s)
   (vector #xfd #xde 'i64x2.extmul-low-i32x4-u)
   (vector #xfd #xdf 'i64x2.extmul-high-i32x4-u)
   (vector #xfd #xe0 'f32x4.abs)
   (vector #xfd #xe1 'f32x4.neg)
   (vector #xfd #xe3 'f32x4.sqrt)
   (vector #xfd #xe4 'f32x4.add)
   (vector #xfd #xe5 'f32x4.sub)
   (vector #xfd #xe6 'f32x4.mul)
   (vector #xfd #xe7 'f32x4.div)
   (vector #xfd #xe8 'f32x4.min)
   (vector #xfd #xe9 'f32x4.max)
   (vector #xfd #xea 'f32x4.pmin)
   (vector #xfd #xeb 'f32x4.pmax)
   (vector #xfd #xec 'f64x2.abs)
   (vector #xfd #xed 'f64x2.neg)
   (vector #xfd #xef 'f64x2.sqrt)
   (vector #xfd #xf0 'f64x2.add)
   (vector #xfd #xf1 'f64x2.sub)
   (vector #xfd #xf2 'f64x2.mul)
   (vector #xfd #xf3 'f64x2.div)
   (vector #xfd #xf4 'f64x2.min)
   (vector #xfd #xf5 'f64x2.max)
   (vector #xfd #xf6 'f64x2.pmin)
   (vector #xfd #xf7 'f64x2.pmax)
   (vector #xfd #xf8 'i32x4.trunc-sat-f32x4-s)
   (vector #xfd #xf9 'i32x4.trunc-sat-f32x4-u)
   (vector #xfd #xfa 'f32x4.convert-i32x4-s)
   (vector #xfd #xfb 'f32x4.convert-i32x4-u)
   (vector #xfd #xfc 'i32x4.trunc-sat-f64x2-s-zero)
   (vector #xfd #xfd 'i32x4.trunc-sat-f64x2-u-zero)
   (vector #xfd #xfe 'f64x2.convert-low-i32x4-s)
   (vector #xfd #xff 'f64x2.convert-low-i32x4-u)
   (vector #xfd #x100 'i8x16.relaxed-swizzle)
   (vector #xfd #x101 'i32x4.relaxed-trunc-f32x4-s)
   (vector #xfd #x102 'i32x4.relaxed-trunc-f32x4-u)
   (vector #xfd #x103 'i32x4.relaxed-trunc-f64x2-s-zero)
   (vector #xfd #x104 'i32x4.relaxed-trunc-f64x2-u-zero)
   (vector #xfd #x105 'f32x4.relaxed-madd)
   (vector #xfd #x106 'f32x4.relaxed-nmadd)
   (vector #xfd #x107 'f64x2.relaxed-madd)
   (vector #xfd #x108 'f64x2.relaxed-nmadd)
   (vector #xfd #x109 'i8x16.relaxed-laneselect)
   (vector #xfd #x10a 'i16x8.relaxed-laneselect)
   (vector #xfd #x10b 'i32x4.relaxed-laneselect)
   (vector #xfd #x10c 'i64x2.relaxed-laneselect)
   (vector #xfd #x10d 'f32x4.relaxed-min)
   (vector #xfd #x10e 'f32x4.relaxed-max)
   (vector #xfd #x10f 'f64x2.relaxed-min)
   (vector #xfd #x110 'f64x2.relaxed-max)
   (vector #xfd #x111 'i16x8.relaxed-q15mulr-s)
   (vector #xfd #x112 'i16x8.relaxed-dot-i8x16-i7x16-s)
   (vector #xfd #x113 'i32x4.relaxed-dot-i8x16-i7x16-add-s)
   ))

;; Shapes follow the final Core 3.0 binary grammar and the normalized order below.
(define expected-core-3-special-opcodes
  (vector
   ;; one-byte special descriptors
   (vector #f #x2 'block 'block-type 'block)
   (vector #f #x3 'loop 'block-type 'loop)
   (vector #f #x4 'if 'block-type 'if)
   (vector #f #x8 'throw 'tag-index #f)
   (vector #f #xc 'br 'label-index #f)
   (vector #f #xd 'br-if 'label-index #f)
   (vector #f #xe 'br-table 'label-vector #f)
   (vector #f #x10 'call 'function-index #f)
   (vector #f #x11 'call-indirect 'call-indirect #f)
   (vector #f #x12 'return-call 'function-index #f)
   (vector #f #x13 'return-call-indirect 'call-indirect #f)
   (vector #f #x14 'call-ref 'type-index #f)
   (vector #f #x15 'return-call-ref 'type-index #f)
   (vector #f #x1c 'select 'select-types #f)
   (vector #f #x1f 'try-table 'try-table 'try-table)
   (vector #f #x20 'local.get 'local-index #f)
   (vector #f #x21 'local.set 'local-index #f)
   (vector #f #x22 'local.tee 'local-index #f)
   (vector #f #x23 'global.get 'global-index #f)
   (vector #f #x24 'global.set 'global-index #f)
   (vector #f #x25 'table.get 'table-index #f)
   (vector #f #x26 'table.set 'table-index #f)
   (vector #f #x28 'i32.load 'memory-argument #f)
   (vector #f #x29 'i64.load 'memory-argument #f)
   (vector #f #x2a 'f32.load 'memory-argument #f)
   (vector #f #x2b 'f64.load 'memory-argument #f)
   (vector #f #x2c 'i32.load8-s 'memory-argument #f)
   (vector #f #x2d 'i32.load8-u 'memory-argument #f)
   (vector #f #x2e 'i32.load16-s 'memory-argument #f)
   (vector #f #x2f 'i32.load16-u 'memory-argument #f)
   (vector #f #x30 'i64.load8-s 'memory-argument #f)
   (vector #f #x31 'i64.load8-u 'memory-argument #f)
   (vector #f #x32 'i64.load16-s 'memory-argument #f)
   (vector #f #x33 'i64.load16-u 'memory-argument #f)
   (vector #f #x34 'i64.load32-s 'memory-argument #f)
   (vector #f #x35 'i64.load32-u 'memory-argument #f)
   (vector #f #x36 'i32.store 'memory-argument #f)
   (vector #f #x37 'i64.store 'memory-argument #f)
   (vector #f #x38 'f32.store 'memory-argument #f)
   (vector #f #x39 'f64.store 'memory-argument #f)
   (vector #f #x3a 'i32.store8 'memory-argument #f)
   (vector #f #x3b 'i32.store16 'memory-argument #f)
   (vector #f #x3c 'i64.store8 'memory-argument #f)
   (vector #f #x3d 'i64.store16 'memory-argument #f)
   (vector #f #x3e 'i64.store32 'memory-argument #f)
   (vector #f #x3f 'memory.size 'memory-index #f)
   (vector #f #x40 'memory.grow 'memory-index #f)
   (vector #f #x41 'i32.const 'i32 #f)
   (vector #f #x42 'i64.const 'i64 #f)
   (vector #f #x43 'f32.const 'f32 #f)
   (vector #f #x44 'f64.const 'f64 #f)
   (vector #f #xd0 'ref.null 'heap-type #f)
   (vector #f #xd2 'ref.func 'function-index #f)
   (vector #f #xd5 'br-on-null 'label-index #f)
   (vector #f #xd6 'br-on-non-null 'label-index #f)
   ;; #xfb aggregate, cast, and branch descriptors
   (vector #xfb #x0 'struct.new 'type-index #f)
   (vector #xfb #x1 'struct.new-default 'type-index #f)
   (vector #xfb #x2 'struct.get 'struct-field #f)
   (vector #xfb #x3 'struct.get-s 'struct-field #f)
   (vector #xfb #x4 'struct.get-u 'struct-field #f)
   (vector #xfb #x5 'struct.set 'struct-field #f)
   (vector #xfb #x6 'array.new 'type-index #f)
   (vector #xfb #x7 'array.new-default 'type-index #f)
   (vector #xfb #x8 'array.new-fixed 'array-new-fixed #f)
   (vector #xfb #x9 'array.new-data 'type-data #f)
   (vector #xfb #xa 'array.new-elem 'type-element #f)
   (vector #xfb #xb 'array.get 'type-index #f)
   (vector #xfb #xc 'array.get-s 'type-index #f)
   (vector #xfb #xd 'array.get-u 'type-index #f)
   (vector #xfb #xe 'array.set 'type-index #f)
   (vector #xfb #x10 'array.fill 'type-index #f)
   (vector #xfb #x11 'array.copy 'array-copy #f)
   (vector #xfb #x12 'array.init-data 'type-data #f)
   (vector #xfb #x13 'array.init-elem 'type-element #f)
   (vector #xfb #x14 'ref.test 'heap-type-non-null #f)
   (vector #xfb #x15 'ref.test 'heap-type-nullable #f)
   (vector #xfb #x16 'ref.cast 'heap-type-non-null #f)
   (vector #xfb #x17 'ref.cast 'heap-type-nullable #f)
   (vector #xfb #x18 'br-on-cast 'br-on-cast #f)
   (vector #xfb #x19 'br-on-cast-fail 'br-on-cast #f)
   ;; #xfc bulk descriptors
   (vector #xfc #x8 'memory.init 'memory-data #f)
   (vector #xfc #x9 'data.drop 'data-index #f)
   (vector #xfc #xa 'memory.copy 'memory-pair #f)
   (vector #xfc #xb 'memory.fill 'memory-index #f)
   (vector #xfc #xc 'table.init 'table-element #f)
   (vector #xfc #xd 'elem.drop 'element-index #f)
   (vector #xfc #xe 'table.copy 'table-pair #f)
   (vector #xfc #xf 'table.grow 'table-index #f)
   (vector #xfc #x10 'table.size 'table-index #f)
   (vector #xfc #x11 'table.fill 'table-index #f)
   ;; #xfd vector descriptors
   (vector #xfd #x0 'v128.load 'memory-argument #f)
   (vector #xfd #x1 'v128.load8x8-s 'memory-argument #f)
   (vector #xfd #x2 'v128.load8x8-u 'memory-argument #f)
   (vector #xfd #x3 'v128.load16x4-s 'memory-argument #f)
   (vector #xfd #x4 'v128.load16x4-u 'memory-argument #f)
   (vector #xfd #x5 'v128.load32x2-s 'memory-argument #f)
   (vector #xfd #x6 'v128.load32x2-u 'memory-argument #f)
   (vector #xfd #x7 'v128.load8-splat 'memory-argument #f)
   (vector #xfd #x8 'v128.load16-splat 'memory-argument #f)
   (vector #xfd #x9 'v128.load32-splat 'memory-argument #f)
   (vector #xfd #xa 'v128.load64-splat 'memory-argument #f)
   (vector #xfd #xb 'v128.store 'memory-argument #f)
   (vector #xfd #xc 'v128.const 'vector-bytes #f)
   (vector #xfd #xd 'i8x16.shuffle 'shuffle-bytes #f)
   (vector #xfd #x15 'i8x16.extract-lane-s 'lane-index #f)
   (vector #xfd #x16 'i8x16.extract-lane-u 'lane-index #f)
   (vector #xfd #x17 'i8x16.replace-lane 'lane-index #f)
   (vector #xfd #x18 'i16x8.extract-lane-s 'lane-index #f)
   (vector #xfd #x19 'i16x8.extract-lane-u 'lane-index #f)
   (vector #xfd #x1a 'i16x8.replace-lane 'lane-index #f)
   (vector #xfd #x1b 'i32x4.extract-lane 'lane-index #f)
   (vector #xfd #x1c 'i32x4.replace-lane 'lane-index #f)
   (vector #xfd #x1d 'i64x2.extract-lane 'lane-index #f)
   (vector #xfd #x1e 'i64x2.replace-lane 'lane-index #f)
   (vector #xfd #x1f 'f32x4.extract-lane 'lane-index #f)
   (vector #xfd #x20 'f32x4.replace-lane 'lane-index #f)
   (vector #xfd #x21 'f64x2.extract-lane 'lane-index #f)
   (vector #xfd #x22 'f64x2.replace-lane 'lane-index #f)
   (vector #xfd #x54 'v128.load8-lane 'memory-argument-lane #f)
   (vector #xfd #x55 'v128.load16-lane 'memory-argument-lane #f)
   (vector #xfd #x56 'v128.load32-lane 'memory-argument-lane #f)
   (vector #xfd #x57 'v128.load64-lane 'memory-argument-lane #f)
   (vector #xfd #x58 'v128.store8-lane 'memory-argument-lane #f)
   (vector #xfd #x59 'v128.store16-lane 'memory-argument-lane #f)
   (vector #xfd #x5a 'v128.store32-lane 'memory-argument-lane #f)
   (vector #xfd #x5b 'v128.store64-lane 'memory-argument-lane #f)
   (vector #xfd #x5c 'v128.load32-zero 'memory-argument #f)
   (vector #xfd #x5d 'v128.load64-zero 'memory-argument #f)
   ))

(define concatenate-bytevectors
  (lambda bytevectors
    (let* ([length (apply + (map bytevector-length bytevectors))]
           [result (make-bytevector length)])
      (let loop ([bytevectors bytevectors] [offset 0])
        (if (null? bytevectors)
            result
            (let* ([bytes (car bytevectors)]
                   [next-offset (fx+ offset (bytevector-length bytes))])
              (bytevector-copy! bytes 0 result offset (bytevector-length bytes))
              (loop (cdr bytevectors) next-offset)))))))

(define byte-list->bytevector
  (lambda (byte*)
    (let* ([length (length byte*)] [bytes (make-bytevector length)])
      (let loop ([index 0] [byte* byte*])
        (unless (null? byte*)
          (bytevector-u8-set! bytes index (car byte*))
          (loop (fx1+ index) (cdr byte*))))
      bytes)))

(define encode-minimal-u32
  (lambda (value)
    (let loop ([value value] [byte* '()])
      (let ([next (fxsra value 7)] [payload (fxand value #x7f)])
        (if (fxzero? next)
            (byte-list->bytevector (reverse (cons payload byte*)))
            (loop next (cons (fxior payload #x80) byte*)))))))

(define expected-special-row
  (lambda (assignment)
    (let ([prefix (vector-ref assignment 0)] [code (vector-ref assignment 1)])
      (let loop ([row* (vector->list expected-core-3-special-opcodes)])
        (and (not (null? row*))
             (let ([row (car row*)])
               (if (and (equal? prefix (vector-ref row 0))
                        (= code (vector-ref row 1)))
                   row
                   (loop (cdr row*)))))))))

(define minimal-binary-immediate
  (lambda (shape structured-kind)
    (case shape
      [(none) #vu8()]
      [(block-type)
       (case structured-kind
         [(block loop if) #vu8(#x40 #x0b)]
         [else (error 'minimal-binary-immediate "unhandled structured block type")])]
      [(try-table) #vu8(#x40 #x00 #x0b)]
      [(label-index function-index type-index table-index memory-index global-index
                    local-index tag-index field-index data-index element-index)
       #vu8(0)]
      [(label-vector) #vu8(0 0)]
      [(heap-type heap-type-non-null heap-type-nullable) #vu8(#x70)]
      [(reference-type) #vu8(#x70)]
      [(value-type-vector select-types) #vu8(1 #x7f)]
      [(call-indirect table-pair memory-pair array-new-fixed array-copy struct-field
                      type-data type-element memory-data table-element)
       #vu8(0 0)]
      [(br-on-cast) #vu8(0 0 #x70 #x70)]
      [(memory-argument) #vu8(0 0)]
      [(memory-argument-lane) #vu8(0 0 0)]
      [(lane-index) #vu8(0)]
      [(shuffle-bytes vector-bytes)
       #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]
      [(i32 i64) #vu8(0)]
      [(f32) #vu8(0 0 0 0)]
      [(f64) #vu8(0 0 0 0 0 0 0 0)]
      [else (error 'minimal-binary-immediate "unhandled expected shape" shape)])))

(define minimal-binary-instruction
  (lambda (assignment)
    (let* ([prefix (vector-ref assignment 0)]
           [code (vector-ref assignment 1)]
           [special (expected-special-row assignment)]
           [shape (if special (vector-ref special 3) 'none)]
           [structured-kind (and special (vector-ref special 4))]
           [opcode (if prefix
                       (concatenate-bytevectors (bytevector prefix)
                                                (encode-minimal-u32 code))
                       (bytevector code))])
      (concatenate-bytevectors opcode
                               (minimal-binary-immediate shape structured-kind)))))

(define expected-lane-instructions
  (vector
   (vector #xfd #x15 'i8x16.extract-lane-s 'lane-index 16)
   (vector #xfd #x16 'i8x16.extract-lane-u 'lane-index 16)
   (vector #xfd #x17 'i8x16.replace-lane 'lane-index 16)
   (vector #xfd #x18 'i16x8.extract-lane-s 'lane-index 8)
   (vector #xfd #x19 'i16x8.extract-lane-u 'lane-index 8)
   (vector #xfd #x1a 'i16x8.replace-lane 'lane-index 8)
   (vector #xfd #x1b 'i32x4.extract-lane 'lane-index 4)
   (vector #xfd #x1c 'i32x4.replace-lane 'lane-index 4)
   (vector #xfd #x1d 'i64x2.extract-lane 'lane-index 2)
   (vector #xfd #x1e 'i64x2.replace-lane 'lane-index 2)
   (vector #xfd #x1f 'f32x4.extract-lane 'lane-index 4)
   (vector #xfd #x20 'f32x4.replace-lane 'lane-index 4)
   (vector #xfd #x21 'f64x2.extract-lane 'lane-index 2)
   (vector #xfd #x22 'f64x2.replace-lane 'lane-index 2)
   (vector #xfd #x54 'v128.load8-lane 'memory-argument-lane 16)
   (vector #xfd #x55 'v128.load16-lane 'memory-argument-lane 8)
   (vector #xfd #x56 'v128.load32-lane 'memory-argument-lane 4)
   (vector #xfd #x57 'v128.load64-lane 'memory-argument-lane 2)
   (vector #xfd #x58 'v128.store8-lane 'memory-argument-lane 16)
   (vector #xfd #x59 'v128.store16-lane 'memory-argument-lane 8)
   (vector #xfd #x5a 'v128.store32-lane 'memory-argument-lane 4)
   (vector #xfd #x5b 'v128.store64-lane 'memory-argument-lane 2)))

(define lane-instruction-test-bytes
  (lambda (expected)
    (let* ([opcode (concatenate-bytevectors
                    (bytevector (vector-ref expected 0))
                    (encode-minimal-u32 (vector-ref expected 1)))]
           [lane (fx1- (vector-ref expected 4))]
           [immediate (if (eq? 'lane-index (vector-ref expected 3))
                          (bytevector lane)
                          (bytevector 0 0 lane))])
      (concatenate-bytevectors opcode immediate))))

(define private-binary-variant?
  (lambda (assignment)
    (let ([prefix (vector-ref assignment 0)] [code (vector-ref assignment 1)])
      (or (and (not prefix) (= code #x1b))
          (and (eqv? prefix #xfb) (memv code '(21 23)))))))

(define wasm-opcode-immediate-shapes
  '(none block-type label-index label-vector function-index type-index table-index
    memory-index global-index local-index tag-index field-index data-index element-index
    heap-type reference-type value-type-vector select-types call-indirect br-on-cast
    memory-argument memory-argument-lane lane-index shuffle-bytes vector-bytes i32 i64
    f32 f64 table-pair memory-pair array-new-fixed array-copy struct-field try-table
    resume-table type-data type-element memory-data table-element heap-type-non-null
    heap-type-nullable))

(define wasm-opcode-structured-kinds '(#f block loop if try-table))

(define expected-core-3-binary-variants
  (vector (vector #f #x1b 'select 'none #f)
          (vector #xfb 21 'ref.test 'heap-type-nullable #f)
          (vector #xfb 23 'ref.cast 'heap-type-nullable #f)))

(define unique-values?
  (lambda (values)
    (let ([seen (make-hashtable equal-hash equal?)])
      (andmap (lambda (value)
                (and (not (hashtable-ref seen value #f))
                     (begin (hashtable-set! seen value #t) #t)))
              values))))

(define binary-assignment-pair
  (lambda (assignment)
    (cons (vector-ref assignment 0) (vector-ref assignment 1))))

(define actual-core-3-binary-pairs
  (lambda ()
    (append
     (map (lambda (descriptor)
            (cons (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)))
          (vector->list wasm-core-3-opcodes))
     (map binary-assignment-pair
          (vector->list expected-core-3-binary-variants)))))

(define descriptor-fields=?
  (lambda (descriptor prefix code mnemonic immediate-shape structured-kind)
    (and (wasm-opcode-descriptor? descriptor)
         (equal? prefix (wasm-opcode-prefix descriptor))
         (= code (wasm-opcode-code descriptor))
         (eq? mnemonic (wasm-opcode-mnemonic descriptor))
         (eq? immediate-shape (wasm-opcode-immediate-shape descriptor))
         (eq? structured-kind (wasm-opcode-structured-kind descriptor)))))

(define special-opcode-fields=?
  (lambda (expected)
    (descriptor-fields=?
     (wasm-opcode-by-binary (vector-ref expected 0) (vector-ref expected 1))
     (vector-ref expected 0) (vector-ref expected 1) (vector-ref expected 2)
     (vector-ref expected 3) (vector-ref expected 4))))

(define descriptor-special?
  (lambda (descriptor)
    (or (not (eq? 'none (wasm-opcode-immediate-shape descriptor)))
        (wasm-opcode-structured-kind descriptor))))

(define descriptor-special-fields
  (lambda (descriptor)
    (vector (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)
            (wasm-opcode-mnemonic descriptor) (wasm-opcode-immediate-shape descriptor)
            (wasm-opcode-structured-kind descriptor))))

(mat wasm-core-3-opcode-table

     (andmap (lambda (mnemonic)
               (wasm-opcode-descriptor? (wasm-opcode-by-mnemonic mnemonic)))
             (vector->list expected-core-3-mnemonics))

     (= (vector-length expected-core-3-mnemonics)
        (vector-length wasm-core-3-opcodes))

     (unique-values? (vector->list expected-core-3-mnemonics))

     (unique-values?
      (map wasm-opcode-mnemonic (vector->list wasm-core-3-opcodes)))

     (andmap (lambda (descriptor)
               (and (memq (wasm-opcode-mnemonic descriptor)
                          (vector->list expected-core-3-mnemonics))
                    #t))
             (vector->list wasm-core-3-opcodes))

     (unique-values?
      (append
       (map (lambda (descriptor)
              (cons (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)))
            (vector->list wasm-core-3-opcodes))
       (map (lambda (variant)
              (cons (vector-ref variant 0) (vector-ref variant 1)))
            (vector->list expected-core-3-binary-variants))))

     (= 499 (+ (vector-length wasm-core-3-opcodes)
               (vector-length expected-core-3-binary-variants)))

     (= 499 (vector-length expected-core-3-binary-assignments))

     (= 128 (vector-length expected-core-3-special-opcodes))

     (= 22 (vector-length expected-lane-instructions))

     (unique-values?
      (map binary-assignment-pair (vector->list expected-lane-instructions)))

     (let ([lane-specials
            (filter (lambda (special)
                      (memq (vector-ref special 3)
                            '(lane-index memory-argument-lane)))
                    (vector->list expected-core-3-special-opcodes))])
       (and (= 22 (length lane-specials))
            (andmap
             (lambda (special)
               (= 1
                  (length
                   (filter
                    (lambda (expected)
                      (and (equal? (binary-assignment-pair special)
                                   (binary-assignment-pair expected))
                           (eq? (vector-ref special 2) (vector-ref expected 2))
                           (eq? (vector-ref special 3) (vector-ref expected 3))))
                    (vector->list expected-lane-instructions)))))
             lane-specials)))

     (andmap
      (lambda (expected)
        (let* ([instruction
                (parse-binary <wasm-instruction>
                              (lane-instruction-test-bytes expected))]
               [immediates (wasm-instruction-immediates instruction)]
               [lane-position
                (if (eq? 'lane-index (vector-ref expected 3)) 0 1)])
          (and (eq? (vector-ref expected 2)
                    (wasm-instruction-mnemonic instruction))
               (= (fx1- (vector-ref expected 4))
                  (vector-ref immediates lane-position))
               (or (fxzero? lane-position)
                   (let ([argument (vector-ref immediates 0)])
                     (and (wasm-memory-argument? argument)
                          (zero? (wasm-memory-argument-alignment argument))
                          (zero? (wasm-memory-argument-offset argument))
                          (zero? (wasm-memory-argument-memory-index argument))))))))
      (vector->list expected-lane-instructions))

     (let loop ([assignment* (vector->list expected-core-3-binary-assignments)]
                [successes 0]
                [terminator-failures 0]
                [private-successes 0])
       (if (null? assignment*)
           (and (= successes 497)
                (= terminator-failures 2)
                (= private-successes 3))
           (let* ([assignment (car assignment*)]
                  [mnemonic (vector-ref assignment 2)]
                  [bytes (minimal-binary-instruction assignment)])
             (if (memq mnemonic '(else end))
                 (and (parser-rejects? <wasm-instruction> bytes)
                      (loop (cdr assignment*) successes
                            (fx1+ terminator-failures) private-successes))
                 (let ([instruction (parse-binary <wasm-instruction> bytes)])
                   (and (eq? mnemonic (wasm-instruction-mnemonic instruction))
                        (loop (cdr assignment*) (fx1+ successes)
                              terminator-failures
                              (if (private-binary-variant? assignment)
                                  (fx1+ private-successes)
                                  private-successes))))))))

     (unique-values?
      (map binary-assignment-pair
           (vector->list expected-core-3-binary-assignments)))

     (unique-values?
      (map binary-assignment-pair
           (vector->list expected-core-3-special-opcodes)))

     (let ([official-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-binary-assignments))])
       (andmap (lambda (special)
                 (and (member (binary-assignment-pair special) official-pairs) #t))
               (vector->list expected-core-3-special-opcodes)))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 0))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 1))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 2))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 3))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 4))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 5))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 6))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 7))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 8))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 9))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 10))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 11))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 12))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 13))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 14))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 15))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 16))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 17))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 18))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 19))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 20))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 21))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 22))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 23))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 24))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 25))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 26))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 27))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 28))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 29))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 30))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 31))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 32))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 33))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 34))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 35))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 36))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 37))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 38))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 39))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 40))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 41))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 42))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 43))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 44))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 45))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 46))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 47))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 48))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 49))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 50))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 51))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 52))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 53))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 54))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 55))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 56))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 57))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 58))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 59))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 60))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 61))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 62))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 63))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 64))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 65))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 66))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 67))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 68))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 69))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 70))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 71))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 72))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 73))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 74))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 75))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 76))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 77))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 78))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 79))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 80))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 81))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 82))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 83))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 84))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 85))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 86))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 87))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 88))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 89))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 90))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 91))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 92))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 93))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 94))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 95))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 96))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 97))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 98))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 99))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 100))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 101))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 102))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 103))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 104))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 105))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 106))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 107))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 108))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 109))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 110))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 111))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 112))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 113))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 114))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 115))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 116))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 117))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 118))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 119))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 120))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 121))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 122))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 123))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 124))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 125))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 126))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 127))

     (let ([special-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-special-opcodes))])
       (andmap
        (lambda (assignment)
          (if (member (binary-assignment-pair assignment) special-pairs)
              #t
              (let ([descriptor
                     (wasm-opcode-by-binary (vector-ref assignment 0)
                                            (vector-ref assignment 1))])
                (and (eq? 'none (wasm-opcode-immediate-shape descriptor))
                     (not (wasm-opcode-structured-kind descriptor))))))
        (vector->list expected-core-3-binary-assignments)))

     (let ([expected-special (vector->list expected-core-3-special-opcodes)])
       (andmap
        (lambda (assignment)
          (let ([descriptor
                 (wasm-opcode-by-binary (vector-ref assignment 0)
                                        (vector-ref assignment 1))])
            (or (not (descriptor-special? descriptor))
                (and (member (descriptor-special-fields descriptor) expected-special)
                     #t))))
        (vector->list expected-core-3-binary-assignments)))

     (= 128
        (length
         (filter descriptor-special?
                 (map (lambda (assignment)
                        (wasm-opcode-by-binary (vector-ref assignment 0)
                                               (vector-ref assignment 1)))
                      (vector->list expected-core-3-binary-assignments)))))

     (andmap
      (lambda (assignment)
        (let ([descriptor
               (wasm-opcode-by-binary (vector-ref assignment 0)
                                      (vector-ref assignment 1))])
          (and (wasm-opcode-descriptor? descriptor)
               (eq? (vector-ref assignment 2)
                    (wasm-opcode-mnemonic descriptor)))))
      (vector->list expected-core-3-binary-assignments))

     (let ([expected-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-binary-assignments))])
       (andmap (lambda (actual-pair)
                 (and (member actual-pair expected-pairs) #t))
               (actual-core-3-binary-pairs)))

     (let ([actual-pairs (actual-core-3-binary-pairs)])
       (andmap (lambda (expected-pair)
                 (and (member expected-pair actual-pairs) #t))
               (map binary-assignment-pair
                    (vector->list expected-core-3-binary-assignments))))

     (andmap
      (lambda (descriptor)
        (eq? descriptor
             (wasm-opcode-by-binary (wasm-opcode-prefix descriptor)
                                    (wasm-opcode-code descriptor))))
      (vector->list wasm-core-3-opcodes))

     (andmap (lambda (descriptor)
               (and (memq (wasm-opcode-immediate-shape descriptor)
                          wasm-opcode-immediate-shapes)
                    (memq (wasm-opcode-structured-kind descriptor)
                          wasm-opcode-structured-kinds)
                    #t))
             (vector->list wasm-core-3-opcodes))

     (andmap
      (lambda (variant)
        (let ([descriptor
               (wasm-opcode-by-binary (vector-ref variant 0) (vector-ref variant 1))])
          (descriptor-fields=? descriptor
                               (vector-ref variant 0) (vector-ref variant 1)
                               (vector-ref variant 2) (vector-ref variant 3)
                               (vector-ref variant 4))))
      (vector->list expected-core-3-binary-variants))

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'unreachable)
                          #f #x00 'unreachable 'none #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'block)
                          #f #x02 'block 'block-type 'block)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'call-indirect)
                          #f #x11 'call-indirect 'call-indirect #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'struct.get)
                          #xfb 2 'struct.get 'struct-field #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'memory.init)
                          #xfc 8 'memory.init 'memory-data #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'v128.load)
                          #xfd 0 'v128.load 'memory-argument #f)

     ;; error case: an unassigned binary pair has no descriptor.
     (not (wasm-opcode-by-binary #xfd #xffff))

     ;; error case: the largest u32 subopcode is valid input but unassigned.
     (not (wasm-opcode-by-binary #xfd #xffffffff))

     ;; error case: a subopcode above the u32 range violates the public contract.
     (error? (wasm-opcode-by-binary #xfd #x100000000))

     ;; error case: an unknown textual mnemonic has no descriptor.
     (not (wasm-opcode-by-mnemonic 'not-a-wasm-opcode))

     )


(mat wasm-binary-integer-boundaries

     (= 0 (parse-binary <wasm-u32> #vu8(0)))

     (= #xffffffffffffffff
        (parse-binary <wasm-u64>
                      (bytevector #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01)))

     (= 0 (parse-binary <wasm-u64> #vu8(128 0)))

     (= 2147483647
        (parse-binary <wasm-s32> #vu8(255 255 255 255 7)))

     (= -2147483648
        (parse-binary <wasm-s32> #vu8(128 128 128 128 120)))

     (= 1 (parse-binary <wasm-s32> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s32> #vu8(255 127)))

     (= #xffffffff
        (parse-binary <wasm-s33> #vu8(255 255 255 255 15)))

     (= -4294967296
        (parse-binary <wasm-s33> #vu8(128 128 128 128 112)))

     (= 1 (parse-binary <wasm-s33> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s33> #vu8(255 127)))

     (= #x7fffffffffffffff
        (parse-binary <wasm-s64>
                      #vu8(255 255 255 255 255 255 255 255 255 0)))

     (= -9223372036854775808
        (parse-binary <wasm-s64>
                      #vu8(128 128 128 128 128 128 128 128 128 127)))

     (= 1 (parse-binary <wasm-s64> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s64> #vu8(255 127)))

     ;; error: a u32 input cannot end while the continuation bit is set.
     (error? (parse-binary <wasm-u32> #vu8(128)))

     ;; error: a u64 tenth byte may only carry its one remaining value bit.
     (error? (parse-binary <wasm-u64>
                           #vu8(128 128 128 128 128 128 128 128 128 2)))

     ;; error: a u64 encoding cannot continue beyond its tenth byte.
     (error? (parse-binary <wasm-u64>
                           #vu8(128 128 128 128 128 128 128 128 128 128 0)))

     ;; error: an s32 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s32> #vu8(128 128 128 128 8)))

     ;; error: an s32 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s32> #vu8(255 255 255 255 119)))

     ;; error: an s33 encoding cannot continue beyond its fifth byte.
     (error? (parse-binary <wasm-s33> #vu8(128 128 128 128 128 0)))

     ;; error: an s33 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s33> #vu8(128 128 128 128 16)))

     ;; error: an s33 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s33> #vu8(255 255 255 255 111)))

     ;; error: an s64 input cannot end while the continuation bit is set.
     (error? (parse-binary <wasm-s64> #vu8(128)))

     ;; error: an s64 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 2)))

     ;; error: an s64 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 125)))

     ;; error: an s64 encoding cannot continue beyond its tenth byte.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 128 0)))

     )

(mat wasm-binary-scalars-and-vectors

     (let ([value (parse-binary <wasm-f32> #vu8(1 0 192 127))])
       (and (= 32 (wasm-float-width value))
            (= #x7fc00001 (wasm-float-bits value))))

     (let ([value (parse-binary <wasm-f64> #vu8(1 0 0 0 0 0 248 127))])
       (and (= 64 (wasm-float-width value))
            (= #x7ff8000000000001 (wasm-float-bits value))))

     (equal? #vu8() (parse-binary <wasm-byte-vector> #vu8(0)))

     (equal? #vu8(1 2 255)
             (parse-binary <wasm-byte-vector> #vu8(3 1 2 255)))

     (string=? "wasm" (parse-binary <wasm-name> #vu8(4 119 97 115 109)))

     (string=? (string (integer->char #x4e2d))
               (parse-binary <wasm-name> #vu8(3 228 184 173)))

     (equal? '#() (parse-binary (<wasm-vector> <wasm-u32>) #vu8(0)))

     (equal? '#(1 300)
             (parse-binary (<wasm-vector> <wasm-u32>) #vu8(2 1 172 2)))

     ;; error: the vector parser constructor requires an element parser.
     (error? (<wasm-vector> 'not-a-parser))

     ;; error: a byte vector must contain the declared number of bytes.
     (error? (parse-binary <wasm-byte-vector> #vu8(2 1)))

     ;; error: overlong UTF-8 is not a valid WebAssembly name.
     (error? (parse-binary <wasm-name> #vu8(2 192 175)))

     )

(mat wasm-binary-basic-types

     (equal? '(i32 i64 f32 f64)
             (map (lambda (byte)
                    (parse-binary <wasm-number-type> (bytevector byte)))
                  '(#x7f #x7e #x7d #x7c)))

     (eq? 'v128 (parse-binary <wasm-vector-type> #vu8(123)))

     (equal? '(func extern any eq i31 struct array none nofunc noextern exn noexn)
             (map (lambda (byte)
                    (parse-binary <wasm-heap-type> (bytevector byte)))
                  '(#x70 #x6f #x6e #x6d #x6c #x6b #x6a #x71 #x73 #x72 #x69 #x74)))

     (= 3 (parse-binary <wasm-heap-type> #vu8(3)))

     (let ([type (parse-binary <wasm-reference-type> #vu8(99 113))])
       (and (wasm-reference-type-nullable? type)
            (eq? 'none (wasm-reference-type-heap-type type))))

     (let ([type (parse-binary <wasm-reference-type> #vu8(100 3))])
       (and (not (wasm-reference-type-nullable? type))
            (= 3 (wasm-reference-type-heap-type type))))

     (equal? '(func extern any eq i31 struct array none nofunc noextern exn noexn)
             (map (lambda (byte)
                    (wasm-reference-type-heap-type
                     (parse-binary <wasm-value-type> (bytevector byte))))
                  '(#x70 #x6f #x6e #x6d #x6c #x6b #x6a #x71 #x73 #x72 #x69 #x74)))

     (equal? '(i8 i16 i32 v128)
             (map (lambda (byte)
                    (parse-binary <wasm-storage-type> (bytevector byte)))
                  '(#x78 #x77 #x7f #x7b)))

     ;; error: an unassigned negative s33 value is not a heap type.
     (error? (parse-binary <wasm-heap-type> #vu8(100)))

     ;; error: an unknown leading byte is not a value type.
     (error? (parse-binary <wasm-value-type> #vu8(97)))

     )

(mat wasm-binary-composite-types

     (let ([type (parse-binary <wasm-function-type> #vu8(96 2 127 112 1 126))])
       (and (equal? '#(i64) (wasm-function-type-results type))
            (= 2 (vector-length (wasm-function-type-parameters type)))
            (eq? 'i32 (vector-ref (wasm-function-type-parameters type) 0))
            (wasm-reference-type?
             (vector-ref (wasm-function-type-parameters type) 1))))

     (let* ([type (parse-binary <wasm-struct-type> #vu8(95 2 120 1 126 0))]
            [fields (wasm-struct-type-fields type)])
       (and (= 2 (vector-length fields))
            (eq? 'i8 (wasm-field-type-storage-type (vector-ref fields 0)))
            (wasm-field-type-mutable? (vector-ref fields 0))
            (eq? 'i64 (wasm-field-type-storage-type (vector-ref fields 1)))
            (not (wasm-field-type-mutable? (vector-ref fields 1)))))

     (let* ([type (parse-binary <wasm-array-type> #vu8(94 119 0))]
            [field (wasm-array-type-field type)])
       (and (eq? 'i16 (wasm-field-type-storage-type field))
            (not (wasm-field-type-mutable? field))))

     (let ([type (parse-binary <wasm-subtype> #vu8(79 1 3 96 0 0))])
       (and (wasm-subtype-final? type)
            (equal? '#(3) (wasm-subtype-supertypes type))
            (wasm-function-type? (wasm-subtype-composite-type type))))

     (let ([type (parse-binary <wasm-subtype> #vu8(80 0 94 127 1))])
       (and (not (wasm-subtype-final? type))
            (equal? '#() (wasm-subtype-supertypes type))
            (wasm-array-type? (wasm-subtype-composite-type type))))

     (let ([type (parse-binary <wasm-subtype> #vu8(96 0 0))])
       (and (wasm-subtype-final? type)
            (equal? '#() (wasm-subtype-supertypes type))))

     (let* ([type (parse-binary <wasm-recursive-type>
                                #vu8(78 2 96 0 0 94 120 1))]
            [subtypes (wasm-recursive-type-subtypes type)])
       (and (= 2 (vector-length subtypes))
            (wasm-function-type?
             (wasm-subtype-composite-type (vector-ref subtypes 0)))
            (wasm-array-type?
             (wasm-subtype-composite-type (vector-ref subtypes 1)))))

     (= 1
        (vector-length
         (wasm-recursive-type-subtypes
          (parse-binary <wasm-recursive-type> #vu8(96 0 0)))))

     ;; error: mutability is encoded by exactly zero or one.
     (error? (parse-binary <wasm-field-type> #vu8(127 2)))

     ;; error: an explicit recursive group must contain every declared subtype.
     (error? (parse-binary <wasm-recursive-type> #vu8(78 2 96 0 0)))

     ;; error: a recursive group cannot end in a truncated composite type.
     (error? (parse-binary <wasm-recursive-type> #vu8(78 1 96 0)))

     )

(mat wasm-binary-administrative-types

     (eq? 'empty
          (wasm-block-type-kind (parse-binary <wasm-block-type> #vu8(64))))

     (eq? 'value-type
          (wasm-block-type-kind (parse-binary <wasm-block-type> #vu8(127))))

     (= 3 (wasm-block-type-value (parse-binary <wasm-block-type> #vu8(3))))

     (let ([type (parse-binary <wasm-global-type> #vu8(126 1))])
       (and (eq? 'i64 (wasm-global-type-value-type type))
            (wasm-global-type-mutable? type)))

     (let ([type (parse-binary <wasm-table-type> #vu8(112 1 2 9))])
       (and (eq? 'func
                 (wasm-reference-type-heap-type
                  (wasm-table-type-reference-type type)))
            (= 2 (wasm-limits-minimum (wasm-table-type-limits type)))
            (= 9 (wasm-limits-maximum (wasm-table-type-limits type)))))

     (let ([type (parse-binary <wasm-memory-type> #vu8(4 7))])
       (and (eq? 'i64
                 (wasm-limits-address-type (wasm-memory-type-limits type)))
            (= 7 (wasm-limits-minimum (wasm-memory-type-limits type)))
            (not (wasm-limits-maximum (wasm-memory-type-limits type)))))

     (= 4 (wasm-tag-type-type-index
           (parse-binary <wasm-tag-type> #vu8(0 4))))

     (equal? '(function table memory global tag)
             (map (lambda (bytes)
                    (wasm-external-type-kind
                     (parse-binary <wasm-external-type> bytes)))
                  (list #vu8(0 2) #vu8(1 112 0 1) #vu8(2 0 1)
                        #vu8(3 127 0) #vu8(4 0 2))))

     (let ([function (parse-binary <wasm-external-type> #vu8(0 2))]
           [table (parse-binary <wasm-external-type> #vu8(1 112 0 1))]
           [memory (parse-binary <wasm-external-type> #vu8(2 0 1))]
           [global (parse-binary <wasm-external-type> #vu8(3 127 0))]
           [tag (parse-binary <wasm-external-type> #vu8(4 0 2))])
       (and (= 2 (wasm-external-type-type function))
            (wasm-table-type? (wasm-external-type-type table))
            (wasm-memory-type? (wasm-external-type-type memory))
            (wasm-global-type? (wasm-external-type-type global))
            (wasm-tag-type? (wasm-external-type-type tag))
            (= 2
               (wasm-tag-type-type-index
                (wasm-external-type-type tag)))))

     ;; error: a negative s33 value cannot be a block type index.
     (error? (parse-binary <wasm-block-type> #vu8(100)))

     ;; error: global mutability is encoded by exactly zero or one.
     (error? (parse-binary <wasm-global-type> #vu8(127 2)))

     ;; error: the tag attribute must be zero in Core 3.0.
     (error? (parse-binary <wasm-tag-type> #vu8(1 0)))

     ;; error: limits flags outside 0, 1, 4, and 5 are invalid.
     (error? (parse-binary <wasm-limits> #vu8(2 0)))

     ;; error: a limits maximum cannot be lower than its minimum.
     (error? (parse-binary <wasm-limits> #vu8(1 9 2)))

     ;; error: external type kind five is undefined.
     (error? (parse-binary <wasm-external-type> #vu8(5)))

     )

(mat wasm-records

     (let* ([limits (make-wasm-limits 'i64 2 9)]
            [memory-type (make-wasm-memory-type limits)]
            [memory (make-wasm-memory memory-type)]
            [module (make-wasm-module '#() '#() '#() '#() (vector memory) '#() '#()
                                      '#() #f '#() '#() '#())])
       (and (wasm-module? module)
            (= 1 (vector-length (wasm-module-memories module)))
            (eq? 'i64 (wasm-limits-address-type
                       (wasm-memory-type-limits
                        (wasm-memory-type
                         (vector-ref (wasm-module-memories module) 0)))))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))))

     (let* ([reference-type (make-wasm-reference-type #t 'func)]
            [field-type (make-wasm-field-type 'i16 #t)]
            [parameters (vector 'i32 reference-type)]
            [results (vector 'i64)]
            [function-type (make-wasm-function-type parameters results)]
            [struct-fields (vector field-type)]
            [struct-type (make-wasm-struct-type struct-fields)]
            [array-type (make-wasm-array-type field-type)]
            [supertypes (vector 3)]
            [subtype (make-wasm-subtype #f supertypes function-type)]
            [subtypes (vector subtype)]
            [recursive-type (make-wasm-recursive-type subtypes)]
            [limits (make-wasm-limits 'i32 2 9)]
            [table-type (make-wasm-table-type reference-type limits)]
            [memory-type (make-wasm-memory-type limits)]
            [global-type (make-wasm-global-type 'i32 #t)]
            [tag-type (make-wasm-tag-type 4)]
            [external-type (make-wasm-external-type 'function 7)]
            [table-external (make-wasm-external-type 'table table-type)]
            [memory-external (make-wasm-external-type 'memory memory-type)]
            [global-external (make-wasm-external-type 'global global-type)]
            [tag-external (make-wasm-external-type 'tag tag-type)]
            [import (make-wasm-import "env" "callback" external-type)]
            [immediates (vector 7)]
            [leaf (make-wasm-instruction 'i32.const immediates '#() '#())]
            [catch (make-wasm-catch 'catch 4 2)]
            [instruction-body (vector leaf)]
            [instruction-alternate (vector leaf)]
            [instruction (make-wasm-instruction
                          'if '#() instruction-body instruction-alternate)]
            [catches (vector catch)]
            [try-table (make-wasm-instruction 'try-table '#() instruction-body catches)]
            [locals (vector 'i64)]
            [function-body (vector instruction try-table)]
            [function (make-wasm-function 0 locals function-body)]
            [initializer (vector leaf)]
            [table (make-wasm-table table-type initializer)]
            [memory (make-wasm-memory memory-type)]
            [global (make-wasm-global global-type initializer)]
            [tag (make-wasm-tag tag-type)]
            [export (make-wasm-export "run" 'function 0)]
            [element-initializers (vector initializer)]
            [active-element
             (make-wasm-element 'active reference-type 1 initializer element-initializers)]
            [passive-element
             (make-wasm-element 'passive reference-type #f #f element-initializers)]
            [declarative-element
             (make-wasm-element 'declarative reference-type #f #f element-initializers)]
            [data-bytes #vu8(1 2 3)]
            [active-data (make-wasm-data 'active 2 initializer data-bytes)]
            [passive-data (make-wasm-data 'passive #f #f data-bytes)]
            [custom-bytes #vu8(9 8)]
            [custom-section (make-wasm-custom-section "meta" custom-bytes 'type)]
            [memory-argument (make-wasm-memory-argument 2 16 1)]
            [block-type (make-wasm-block-type 'value-type reference-type)]
            [empty-block-type (make-wasm-block-type 'empty #f)]
            [indexed-block-type (make-wasm-block-type 'type-index 3)]
            [catch-ref (make-wasm-catch 'catch-ref 5 3)]
            [catch-all (make-wasm-catch 'catch-all #f 4)]
            [catch-all-ref (make-wasm-catch 'catch-all-ref #f 5)]
            [float32 (make-wasm-float 32 #xffffffff)]
            [float (make-wasm-float 64 #xffffffffffffffff)]
            [types (vector recursive-type)]
            [imports (vector import)]
            [functions (vector function)]
            [tables (vector table)]
            [memories (vector memory)]
            [globals (vector global)]
            [tags (vector tag)]
            [exports (vector export)]
            [elements (vector active-element passive-element declarative-element)]
            [data (vector active-data passive-data)]
            [custom-sections (vector custom-section)]
            [module (make-wasm-module types imports functions tables memories globals tags
                                      exports 0 elements data custom-sections)])
       (and (eq? types (wasm-module-types module))
            (eq? imports (wasm-module-imports module))
            (eq? functions (wasm-module-functions module))
            (eq? tables (wasm-module-tables module))
            (eq? memories (wasm-module-memories module))
            (eq? globals (wasm-module-globals module))
            (eq? tags (wasm-module-tags module))
            (eq? exports (wasm-module-exports module))
            (= 0 (wasm-module-start module))
            (eq? elements (wasm-module-elements module))
            (eq? data (wasm-module-data module))
            (eq? custom-sections (wasm-module-custom-sections module))
            (string=? "meta" (wasm-custom-section-name custom-section))
            (equal? custom-bytes (wasm-custom-section-bytes custom-section))
            (eq? 'type (wasm-custom-section-after-section custom-section))
            (eq? subtypes (wasm-recursive-type-subtypes recursive-type))
            (not (wasm-subtype-final? subtype))
            (eq? supertypes (wasm-subtype-supertypes subtype))
            (eq? function-type (wasm-subtype-composite-type subtype))
            (eq? parameters (wasm-function-type-parameters function-type))
            (eq? results (wasm-function-type-results function-type))
            (eq? struct-fields (wasm-struct-type-fields struct-type))
            (eq? field-type (wasm-array-type-field array-type))
            (eq? 'i16 (wasm-field-type-storage-type field-type))
            (wasm-field-type-mutable? field-type)
            (wasm-reference-type-nullable? reference-type)
            (eq? 'func (wasm-reference-type-heap-type reference-type))
            (eq? 'i32 (wasm-limits-address-type limits))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))
            (eq? reference-type (wasm-table-type-reference-type table-type))
            (eq? limits (wasm-table-type-limits table-type))
            (eq? limits (wasm-memory-type-limits memory-type))
            (eq? 'i32 (wasm-global-type-value-type global-type))
            (wasm-global-type-mutable? global-type)
            (= 4 (wasm-tag-type-type-index tag-type))
            (eq? 'function (wasm-external-type-kind external-type))
            (= 7 (wasm-external-type-type external-type))
            (eq? table-type (wasm-external-type-type table-external))
            (eq? memory-type (wasm-external-type-type memory-external))
            (eq? global-type (wasm-external-type-type global-external))
            (eq? tag-type (wasm-external-type-type tag-external))
            (string=? "env" (wasm-import-module import))
            (string=? "callback" (wasm-import-name import))
            (eq? external-type (wasm-import-external-type import))
            (= 0 (wasm-function-type-index function))
            (eq? locals (wasm-function-locals function))
            (eq? function-body (wasm-function-body function))
            (eq? table-type (wasm-table-type table))
            (eq? initializer (wasm-table-initializer table))
            (eq? memory-type (wasm-memory-type memory))
            (eq? global-type (wasm-global-type global))
            (eq? initializer (wasm-global-initializer global))
            (eq? tag-type (wasm-tag-type tag))
            (string=? "run" (wasm-export-name export))
            (eq? 'function (wasm-export-kind export))
            (= 0 (wasm-export-index export))
            (eq? 'active (wasm-element-mode active-element))
            (eq? reference-type (wasm-element-reference-type active-element))
            (= 1 (wasm-element-table-index active-element))
            (eq? initializer (wasm-element-offset active-element))
            (eq? element-initializers (wasm-element-initializers active-element))
            (eq? 'passive (wasm-element-mode passive-element))
            (not (wasm-element-table-index passive-element))
            (not (wasm-element-offset passive-element))
            (eq? 'declarative (wasm-element-mode declarative-element))
            (not (wasm-element-table-index declarative-element))
            (not (wasm-element-offset declarative-element))
            (eq? 'active (wasm-data-mode active-data))
            (= 2 (wasm-data-memory-index active-data))
            (eq? initializer (wasm-data-offset active-data))
            (equal? data-bytes (wasm-data-bytes active-data))
            (eq? 'passive (wasm-data-mode passive-data))
            (not (wasm-data-memory-index passive-data))
            (not (wasm-data-offset passive-data))
            (equal? data-bytes (wasm-data-bytes passive-data))
            (eq? 'if (wasm-instruction-mnemonic instruction))
            (equal? '#() (wasm-instruction-immediates instruction))
            (eq? instruction-body (wasm-instruction-body instruction))
            (eq? instruction-alternate (wasm-instruction-alternate instruction))
            (eq? catches (wasm-instruction-alternate try-table))
            (= 2 (wasm-memory-argument-alignment memory-argument))
            (= 16 (wasm-memory-argument-offset memory-argument))
            (= 1 (wasm-memory-argument-memory-index memory-argument))
            (eq? 'value-type (wasm-block-type-kind block-type))
            (eq? reference-type (wasm-block-type-value block-type))
            (not (wasm-block-type-value empty-block-type))
            (= 3 (wasm-block-type-value indexed-block-type))
            (eq? 'catch (wasm-catch-kind catch))
            (= 4 (wasm-catch-tag-index catch))
            (= 2 (wasm-catch-label-index catch))
            (= 5 (wasm-catch-tag-index catch-ref))
            (not (wasm-catch-tag-index catch-all))
            (not (wasm-catch-tag-index catch-all-ref))
            (= #xffffffff (wasm-float-bits float32))
            (= 64 (wasm-float-width float))
            (= #xffffffffffffffff (wasm-float-bits float))))

     ;; error: record accessors reject values of the wrong record type.
     (error? (wasm-module-types 'not-a-module))

     ;; error: limits require an i32 or i64 address type.
     (error? (make-wasm-limits 'f32 0 #f))

     ;; error: a memory external type requires a memory type record.
     (error? (make-wasm-external-type 'memory 0))

     ;; error: only a function external type can use a natural type index directly.
     (error? (make-wasm-external-type 'table 0))

     ;; error: a global external type requires a global type record.
     (error? (make-wasm-external-type 'global 0))

     ;; error: a tag external type requires a tag type record.
     (error? (make-wasm-external-type 'tag 0))

     ;; error: an empty block type cannot carry a value.
     (error? (make-wasm-block-type 'empty 'i32))

     ;; error: a value block type requires a WebAssembly value type.
     (error? (make-wasm-block-type 'value-type 0))

     ;; error: an indexed block type requires a natural type index.
     (error? (make-wasm-block-type 'type-index 'i32))

     ;; error: a tagged catch requires a natural tag index.
     (error? (make-wasm-catch 'catch #f 0))

     ;; error: a tagged reference catch requires a natural tag index.
     (error? (make-wasm-catch 'catch-ref #f 0))

     ;; error: a catch-all clause cannot carry a tag index.
     (error? (make-wasm-catch 'catch-all 0 0))

     ;; error: a catch-all reference clause cannot carry a tag index.
     (error? (make-wasm-catch 'catch-all-ref 0 0))

     ;; error: a 32-bit float bit pattern cannot exceed 32 bits.
     (error? (make-wasm-float 32 #x100000000))

     ;; error: a 64-bit float bit pattern cannot exceed 64 bits.
     (error? (make-wasm-float 64 #x10000000000000000))

     ;; error: a floating-point bit pattern cannot be negative.
     (error? (make-wasm-float 32 -1))

     ;; error: an active element segment requires a table index and offset expression.
     (error? (make-wasm-element 'active (make-wasm-reference-type #t 'func)
                                #f #f '#()))

     ;; error: a passive element segment cannot carry active placement fields.
     (error? (make-wasm-element 'passive (make-wasm-reference-type #t 'func)
                                0 '#() '#()))

     ;; error: an active data segment requires a memory index and offset expression.
     (error? (make-wasm-data 'active #f #f #vu8()))

     ;; error: a passive data segment cannot carry active placement fields.
     (error? (make-wasm-data 'passive 0 '#() #vu8()))

     ;; error: an instruction alternate cannot mix instructions and catch clauses.
     (error? (make-wasm-instruction
              'try-table '#() '#()
              (vector (make-wasm-instruction 'nop '#() '#() '#())
                      (make-wasm-catch 'catch 0 0))))

     ;; error: a present limits maximum cannot be less than its minimum.
     (error? (make-wasm-limits 'i32 9 2))

     )

(mat wasm-public-contracts

     ;; error: a module requires a vector of recursive type groups.
     (error? (make-wasm-module #f '#() '#() '#() '#() '#() '#() '#() #f '#() '#() '#()))

     ;; error: a custom section name must be a string.
     (error? (make-wasm-custom-section #f #vu8() #f))

     ;; error: a recursive type requires a vector of subtypes.
     (error? (make-wasm-recursive-type #f))

     ;; error: a subtype finality flag must be a boolean.
     (error? (make-wasm-subtype 'not-a-boolean '#()
                                (make-wasm-function-type '#() '#())))

     ;; error: function parameters must be a vector of value types.
     (error? (make-wasm-function-type #f '#()))

     ;; error: struct fields must be a vector of field types.
     (error? (make-wasm-struct-type #f))

     ;; error: an array element must be a field type.
     (error? (make-wasm-array-type #f))

     ;; error: a field storage type must be a WebAssembly storage type.
     (error? (make-wasm-field-type #f #f))

     ;; error: reference nullability must be a boolean.
     (error? (make-wasm-reference-type 'not-a-boolean 'func))

     ;; error: a limits address type must be i32 or i64.
     (error? (make-wasm-limits #f 0 #f))

     ;; error: a table requires a reference element type.
     (error? (make-wasm-table-type #f (make-wasm-limits 'i32 0 #f)))

     ;; error: a memory requires a limits record.
     (error? (make-wasm-memory-type #f))

     ;; error: a global requires a WebAssembly value type.
     (error? (make-wasm-global-type #f #f))

     ;; error: a tag type index must be natural.
     (error? (make-wasm-tag-type #f))

     ;; error: an external type kind must name a Core external namespace.
     (error? (make-wasm-external-type #f 0))

     ;; error: an import module name must be a string.
     (error? (make-wasm-import #f "name" (make-wasm-external-type 'function 0)))

     ;; error: a function type index must be natural.
     (error? (make-wasm-function #f '#() '#()))

     ;; error: a table entity requires a table type.
     (error? (make-wasm-table #f #f))

     ;; error: a memory entity requires a memory type.
     (error? (make-wasm-memory #f))

     ;; error: a global entity requires a global type.
     (error? (make-wasm-global #f '#()))

     ;; error: a tag entity requires a tag type.
     (error? (make-wasm-tag #f))

     ;; error: an export name must be a string.
     (error? (make-wasm-export #f 'function 0))

     ;; error: an element mode must name a Core element mode.
     (error? (make-wasm-element #f (make-wasm-reference-type #t 'func) #f #f '#()))

     ;; error: a data mode must name a Core data mode.
     (error? (make-wasm-data #f #f #f #vu8()))

     ;; error: an instruction mnemonic must be a symbol.
     (error? (make-wasm-instruction #f '#() '#() '#()))

     ;; error: a memory alignment exponent must be natural.
     (error? (make-wasm-memory-argument #f 0 0))

     ;; error: a block type kind must name a Core block type form.
     (error? (make-wasm-block-type #f #f))

     ;; error: a catch kind must name a Core catch form.
     (error? (make-wasm-catch #f #f 0))

     ;; error: a float width must be 32 or 64.
     (error? (make-wasm-float #f 0))

     ;; error: binary module input must be a bytevector or string path.
     (error? (parse-wasm-binary-module 'not-input))

     ;; error: a binary file path must be a string naming a regular file.
     (error? (parse-wasm-binary-module-file #vu8()))

     ;; error: a missing binary file path is rejected by the explicit file API.
     (error? (parse-wasm-binary-module-file "data/not-a-wasm-module.wasm"))

     ;; error: a directory is not accepted as a binary module file.
     (error? (parse-wasm-binary-module-file "."))

     ;; error: textual module input must be a source string.
     (error? (parse-wasm-text-module #vu8()))

     ;; error: the source-string API parses a path spelling as source text.
     (let ([error
            (capture-parser-error
             (lambda () (parse-wasm-text-module "data/wasm-core3.wat")))])
       (and error (string=? "<string>" (parser-error-source error))))

     ;; error: a text file path must be a string naming a regular file.
     (error? (parse-wasm-text-module-file #vu8()))

     ;; error: a missing text file path is rejected by the explicit file API.
     (error? (parse-wasm-text-module-file "data/not-a-wasm-module.wat"))

     ;; error: a directory is not accepted as a text module file.
     (error? (parse-wasm-text-module-file "."))

     )

(mat wasm-positioned-errors

     ;; error: a type truncated inside a bounded section reports its exact byte position.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-binary-module
                #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00 #x01 #x01 #x60))))])
       (and error
            (eq? 'expected (parser-error-kind error))
            (string=? "<bytevector>" (parser-error-source error))
            (= 11 (parser-error-offset error))
            (not (parser-error-line error))
            (not (parser-error-column error))
            (equal? '(#x4e #x4f #x50 #x60 #x5f #x5e)
                    (parser-error-expected error))
            (eq? 'eof (parser-error-found error))
            (string=?
             (string-append
              "<bytevector>:byte 11: unexpected EOF, expected #x4e, #x4f, #x50, "
              "#x60, #x5f, or #x5e")
             (parser-error->string error))))

     ;; error: an unknown nested folded instruction retains its source position.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-text-module
                "(module (func (i32.add (i32.const 1) (bad))))")))])
       (and error
            (eq? 'custom (parser-error-kind error))
            (string=? "<string>" (parser-error-source error))
            (= 41 (parser-error-offset error))
            (= 0 (parser-error-line error))
            (= 41 (parser-error-column error))
            (null? (parser-error-expected error))
            (not (parser-error-found error))
            (string=?
             "<string>:1:42: <fail-with>: unknown WebAssembly instruction: bad"
             (parser-error->string error))))

     ;; error: an unterminated nested comment retains exact expected field alternatives.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-text-module "(module (; outer (; inner ;)")))])
       (and error
            (eq? 'expected (parser-error-kind error))
            (string=? "<string>" (parser-error-source error))
            (= 9 (parser-error-offset error))
            (= 0 (parser-error-line error))
            (= 9 (parser-error-column error))
            (equal? '("@custom" "rec" "type" "import" "func" "table" "memory"
                      "global" "tag" "export" "start" "elem" "data")
                    (parser-error-expected error))
            (char=? (integer->char #x3b) (parser-error-found error))
            (string=?
             (string-append
              "<string>:1:10: expected \"@custom\", \"rec\", \"type\", \"import\", "
              "\"func\", \"table\", \"memory\", \"global\", \"tag\", \"export\", "
              "\"start\", \"elem\", or \"data\", got #\\;")
             (parser-error->string error))))

     ;; error: post-parse identifier resolution reports the original symbolic reference.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-text-module "(module\n  (func call $missing))")))])
       (and error
            (eq? 'custom (parser-error-kind error))
            (string=? "<string>" (parser-error-source error))
            (= 21 (parser-error-offset error))
            (= 1 (parser-error-line error))
            (= 13 (parser-error-column error))
            (null? (parser-error-expected error))
            (not (parser-error-found error))
            (string=?
             (string-append
              "<string>:2:14: <fail-with>: unknown WebAssembly function identifier: "
              "$missing")
             (parser-error->string error))))

     )

(define append-bytevectors
  (lambda bytevector*
    (let ([result
           (make-bytevector
            (fold-left (lambda (length bytes)
                         (+ length (bytevector-length bytes)))
                       0 bytevector*))])
      (let loop ([bytevector* bytevector*] [offset 0])
        (if (null? bytevector*)
            result
            (let* ([bytes (car bytevector*)]
                   [length (bytevector-length bytes)])
              (bytevector-copy! bytes 0 result offset length)
              (loop (cdr bytevector*) (+ offset length))))))))

(define encode-wasm-u32
  (lambda (value)
    (let loop ([value value] [byte* '()])
      (let ([byte (logand value #x7f)]
            [remaining (ash value -7)])
        (if (zero? remaining)
            (apply bytevector (reverse (cons byte byte*)))
            (loop remaining (cons (logor byte #x80) byte*)))))))

(define make-wasm-section-bytes
  (lambda (id payload)
    (append-bytevectors (bytevector id)
                        (encode-wasm-u32 (bytevector-length payload))
                        payload)))

(define make-wasm-module-bytes
  (lambda section*
    (apply append-bytevectors
           (cons #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00) section*))))

(define standard-section-order
  '#(1 2 3 4 5 13 6 7 8 9 12 10 11))

(define reversed-standard-section-pairs
  (let ([pair* '()])
    (let outer ([later 1])
      (when (< later (vector-length standard-section-order))
        (let inner ([earlier 0])
          (if (= earlier later)
              (outer (fx1+ later))
              (begin
                (set! pair*
                      (cons (cons (vector-ref standard-section-order later)
                                  (vector-ref standard-section-order earlier))
                            pair*))
                (inner (fx1+ earlier)))))))
    pair*))

(mat wasm-binary-modules

     (let ([module
            (parse-wasm-binary-module
             #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))])
       (and (wasm-module? module)
            (zero? (vector-length (wasm-module-types module)))
            (zero? (vector-length (wasm-module-functions module)))
            (zero? (vector-length (wasm-module-custom-sections module)))))

     (let* ([module
             (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x01 #x04 #x01 #x60 #x00 #x00
                    #x03 #x02 #x01 #x00
                    #x07 #x07 #x01 #x03 #x61 #x64 #x64 #x00 #x00
                    #x0a #x04 #x01 #x02 #x00 #x0b))]
            [function (vector-ref (wasm-module-functions module) 0)]
            [export (vector-ref (wasm-module-exports module) 0)])
       (and (= 1 (vector-length (wasm-module-types module)))
            (= 0 (wasm-function-type-index function))
            (zero? (vector-length (wasm-function-locals function)))
            (zero? (vector-length (wasm-function-body function)))
            (string=? "add" (wasm-export-name export))
            (eq? 'function (wasm-export-kind export))
            (= 0 (wasm-export-index export))))

     (let* ([module
             (parse-wasm-binary-module
              (make-wasm-module-bytes
               (make-wasm-section-bytes 1 #vu8(#x01 #x60 #x00 #x00))
               (make-wasm-section-bytes
                2 #vu8(#x01 #x03 #x65 #x6e #x76 #x01 #x66 #x00 #x00))
               (make-wasm-section-bytes 3 #vu8(#x01 #x00))
               (make-wasm-section-bytes 5 #vu8(#x01 #x00 #x01))
               (make-wasm-section-bytes 13 #vu8(#x01 #x00 #x00))
               (make-wasm-section-bytes
                6 #vu8(#x01 #x7f #x00 #x41 #x00 #x0b))
               (make-wasm-section-bytes
                7 #vu8(#x01 #x03 #x72 #x75 #x6e #x00 #x01))
               (make-wasm-section-bytes 8 #vu8(#x01))
               (make-wasm-section-bytes 12 #vu8(#x01))
               (make-wasm-section-bytes 10 #vu8(#x01 #x02 #x00 #x0b))
               (make-wasm-section-bytes
                11 #vu8(#x01 #x00 #x41 #x00 #x0b #x01 #xaa))))]
            [import (vector-ref (wasm-module-imports module) 0)]
            [memory (vector-ref (wasm-module-memories module) 0)]
            [global (vector-ref (wasm-module-globals module) 0)]
            [tag (vector-ref (wasm-module-tags module) 0)]
            [data (vector-ref (wasm-module-data module) 0)])
       (and (string=? "env" (wasm-import-module import))
            (eq? 'function
                 (wasm-external-type-kind (wasm-import-external-type import)))
            (= 1 (vector-length (wasm-module-functions module)))
            (= 1 (wasm-limits-minimum
                  (wasm-memory-type-limits (wasm-memory-type memory))))
            (= 1 (vector-length (wasm-global-initializer global)))
            (= 0 (wasm-tag-type-type-index (wasm-tag-type tag)))
            (= 1 (wasm-module-start module))
            (equal? #vu8(#xaa) (wasm-data-bytes data))))

     (let* ([module
             (parse-wasm-binary-module
              (make-wasm-module-bytes
               (make-wasm-section-bytes
                9
                #vu8(#x08
                      #x00 #x41 #x00 #x0b #x01 #x00
                      #x01 #x00 #x01 #x01
                      #x02 #x01 #x41 #x00 #x0b #x00 #x01 #x02
                      #x03 #x00 #x01 #x03
                      #x04 #x41 #x00 #x0b #x01 #xd2 #x04 #x0b
                      #x05 #x70 #x01 #xd2 #x05 #x0b
                      #x06 #x01 #x41 #x00 #x0b #x70 #x01 #xd2 #x06 #x0b
                      #x07 #x70 #x01 #xd2 #x07 #x0b))))]
            [elements (wasm-module-elements module)])
       (and (= 8 (vector-length elements))
            (equal? '#(active passive active declarative
                       active passive active declarative)
                    (vector-map wasm-element-mode elements))
            (andmap (lambda (element)
                      (= 1 (vector-length (wasm-element-initializers element))))
                    (vector->list elements))))

     (let* ([module
             (parse-wasm-binary-module
              (make-wasm-module-bytes
               (make-wasm-section-bytes
                11
                #vu8(#x03
                      #x00 #x41 #x00 #x0b #x01 #xaa
                      #x01 #x01 #xbb
                      #x02 #x02 #x41 #x01 #x0b #x01 #xcc))))]
            [data (wasm-module-data module)])
       (and (= 3 (vector-length data))
            (eq? 'active (wasm-data-mode (vector-ref data 0)))
            (= 0 (wasm-data-memory-index (vector-ref data 0)))
            (eq? 'passive (wasm-data-mode (vector-ref data 1)))
            (not (wasm-data-memory-index (vector-ref data 1)))
            (= 2 (wasm-data-memory-index (vector-ref data 2)))
            (equal? #vu8(#xcc) (wasm-data-bytes (vector-ref data 2)))))

     (let* ([module
             (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x04 #x09 #x01 #x40 #x00 #x70 #x00 #x01
                    #xd0 #x70 #x0b))]
            [table (vector-ref (wasm-module-tables module) 0)]
            [initializer (wasm-table-initializer table)])
       (and (= 1 (vector-length initializer))
            (eq? 'ref.null
                 (wasm-instruction-mnemonic (vector-ref initializer 0)))))

     (let* ([module
             (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x09 #x05 #x01 #x01 #x00 #x01 #x02))]
            [element (vector-ref (wasm-module-elements module) 0)]
            [initializer (vector-ref (wasm-element-initializers element) 0)])
       (and (eq? 'passive (wasm-element-mode element))
            (= 2
               (vector-ref
                (wasm-instruction-immediates (vector-ref initializer 0))
                0))))

     (let* ([module
             (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x09 #x05 #x01 #x03 #x00 #x01 #x03))]
            [element (vector-ref (wasm-module-elements module) 0)]
            [initializer (vector-ref (wasm-element-initializers element) 0)])
       (and (eq? 'declarative (wasm-element-mode element))
            (= 3
               (vector-ref
                (wasm-instruction-immediates (vector-ref initializer 0))
                0))))

     (let* ([module
             (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x01 #x01 #x00
                    #x00 #x04 #x01 #x78 #xaa #xbb))]
            [custom (vector-ref (wasm-module-custom-sections module) 0)])
       (and (string=? "x" (wasm-custom-section-name custom))
            (equal? #vu8(#xaa #xbb) (wasm-custom-section-bytes custom))
            (eq? 'type (wasm-custom-section-after-section custom))))

     ;; error: duplicate standard sections are invalid.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-binary-module
                #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                      #x01 #x01 #x00 #x01 #x01 #x00))))])
       (and error (= 11 (parser-error-offset error))))

     (andmap
      (lambda (section-pair)
        (let ([error
               (capture-parser-error
                (lambda ()
                  (parse-wasm-binary-module
                   (make-wasm-module-bytes
                    (make-wasm-section-bytes (car section-pair) #vu8(#x00))
                    (make-wasm-section-bytes (cdr section-pair) #vu8(#x00))))))])
          (and error (= 11 (parser-error-offset error)))))
      reversed-standard-section-pairs)

     ;; error: a function section requires a code body for each function.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x03 #x02 #x01 #x00)))

     ;; error: a section parser must consume the complete declared payload.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x01 #x02 #x00 #x00)))

     ;; error: data-count must equal the number of data segments.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-binary-module
                #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                      #x0c #x01 #x01))))])
       (and error (= 8 (parser-error-offset error))))

     ;; error: standard sections must occur in Core 3.0 order.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-binary-module
                #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                      #x07 #x01 #x00 #x01 #x01 #x00))))])
       (and error (= 11 (parser-error-offset error))))

     ;; error: element segment flags are limited to zero through seven.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x09 #x02 #x01 #x08)))

     ;; error: a tag type attribute must be zero.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x0d #x03 #x01 #x01 #x00)))

     ;; error: the binary magic must be exactly zero, a, s, m.
     (error? (parse-wasm-binary-module
              #vu8(#x01 #x61 #x73 #x6d #x01 #x00 #x00 #x00)))

     ;; error: only WebAssembly binary version one is accepted.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x02 #x00 #x00 #x00)))

     ;; error: unknown standard section IDs are rejected.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00 #x0e #x00)))

     ;; error: custom section names must contain valid UTF-8.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x00 #x03 #x02 #xc0 #xaf)))

     ;; error: data segment flags are limited to zero through two.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00
                    #x0b #x02 #x01 #x03)))

     ;; error: a code body must contain its complete terminating expression.
     (error? (parse-wasm-binary-module
              (make-wasm-module-bytes
               (make-wasm-section-bytes 3 #vu8(#x01 #x00))
               (make-wasm-section-bytes 10 #vu8(#x01 #x02 #x00)))))

     ;; error: bytes after the last complete section are not permitted.
     (error? (parse-wasm-binary-module
              #vu8(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00 #xff)))

     )

(mat wasm-text-normalization

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func $id (export \"id\") (param $x i32) (result i32)
                   local.get $x)
                 (start $id))")]
            [function (vector-ref (wasm-module-functions module) 0)]
            [export (vector-ref (wasm-module-exports module) 0)]
            [instruction (vector-ref (wasm-function-body function) 0)])
       (and (= 1 (vector-length (wasm-module-types module)))
            (= 0 (wasm-function-type-index function))
            (eq? 'local.get (wasm-instruction-mnemonic instruction))
            (= 0 (vector-ref (wasm-instruction-immediates instruction) 0))
            (eq? 'function (wasm-export-kind export))
            (= 0 (wasm-export-index export))
            (= 0 (wasm-module-start module))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func $later (param i32) (result i32) local.get 0)
                 (import \"env\" \"first\" (func $first (param i32) (result i32)))
                 (func $again (param i32) (result i32)
                   (i32.add (call $later (local.get 0)) (call $first (local.get 0))))
                 (export \"later\" (func $later)))")]
            [function* (wasm-module-functions module)]
            [body (wasm-function-body (vector-ref function* 1))])
       (and (= 1 (vector-length (wasm-module-types module)))
            (= 0 (wasm-function-type-index (vector-ref function* 0)))
            (= 0 (wasm-function-type-index (vector-ref function* 1)))
            (equal? '(local.get call local.get call i32.add)
                    (map wasm-instruction-mnemonic (vector->list body)))
            (= 1 (vector-ref
                  (wasm-instruction-immediates (vector-ref body 1)) 0))
            (= 0 (vector-ref
                  (wasm-instruction-immediates (vector-ref body 3)) 0))
            (= 1 (wasm-export-index
                  (vector-ref (wasm-module-exports module) 0)))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (type $sig (func (param i32 i64)))
                 (func (type $sig) (local $temporary i32)
                   local.get $temporary))")]
            [instruction
             (vector-ref
              (wasm-function-body
               (vector-ref (wasm-module-functions module) 0)) 0)])
       (= 2 (vector-ref (wasm-instruction-immediates instruction) 0)))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func $f)
                 (table funcref (elem $f))
                 (elem $later declare func $f)
                 (func elem.drop $later))")]
            [body (wasm-function-body
                   (vector-ref (wasm-module-functions module) 1))])
       (and (= 2 (vector-length (wasm-module-elements module)))
            (= 1 (vector-ref
                  (wasm-instruction-immediates (vector-ref body 0)) 0))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func (result i32) i32.const 0)
                 (import \"env\" \"later\" (func (param i64))))")]
            [type* (wasm-module-types module)]
            [first
             (wasm-subtype-composite-type
              (vector-ref (wasm-recursive-type-subtypes (vector-ref type* 0)) 0))]
            [second
             (wasm-subtype-composite-type
              (vector-ref (wasm-recursive-type-subtypes (vector-ref type* 1)) 0))])
       (and (equal? '#(i32) (wasm-function-type-results first))
            (equal? '#(i64) (wasm-function-type-parameters second))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (type $pair (struct (field $left i32)))
                 (type $sig (func))
                 (import \"env\" \"f\" (func $imported (type $sig)))
                 (table $table 1 funcref)
                 (memory $memory 1)
                 (global $global i32 (i32.const 0))
                 (tag $tag (type $sig))
                 (elem $element declare func $imported)
                 (data $data \"x\")
                 (func
                   block $exit br $exit end $exit
                   global.get $global memory.size $memory table.size $table
                   throw $tag elem.drop $element data.drop $data
                   struct.get $pair $left))")]
            [body (wasm-function-body
                   (vector-ref (wasm-module-functions module) 0))]
            [immediate
             (lambda (index offset)
               (vector-ref
                (wasm-instruction-immediates (vector-ref body index)) offset))])
       (and (= 0
               (vector-ref
                (wasm-instruction-immediates
                 (vector-ref (wasm-instruction-body (vector-ref body 0)) 0)) 0))
            (= 0 (immediate 1 0))
            (= 0 (immediate 2 0))
            (= 0 (immediate 3 0))
            (= 0 (immediate 4 0))
            (= 0 (immediate 5 0))
            (= 0 (immediate 6 0))
            (= 0 (immediate 7 0))
            (= 0 (immediate 7 1))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (type $sig (func))
                 (tag $tag (type $sig))
                 (func
                   try_table $outer (type $sig) (catch $tag 0)
                   end $outer))")]
            [instruction
             (vector-ref
              (wasm-function-body
               (vector-ref (wasm-module-functions module) 0)) 0)]
            [catch (vector-ref (wasm-instruction-alternate instruction) 0)])
       (and (= 0 (wasm-catch-tag-index catch))
            (= 0 (wasm-catch-label-index catch))))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func $f)
                 (elem declare funcref (ref.func $f)))")]
            [initializer
             (vector-ref
              (wasm-element-initializers
               (vector-ref (wasm-module-elements module) 0)) 0)])
       (= 0
          (vector-ref
           (wasm-instruction-immediates (vector-ref initializer 0)) 0)))

     (let* ([module
             (parse-wasm-text-module
              "(module
                 (func $f (export \"f\") (import \"env\" \"f\"))
                 (table $t (export \"t\") (import \"env\" \"t\") 1 funcref)
                 (memory $m (export \"m\") (import \"env\" \"m\") 1)
                 (global $g (export \"g\") (import \"env\" \"g\") i32)
                 (tag $e (export \"e\") (import \"env\" \"e\")))")]
            [import* (wasm-module-imports module)]
            [export* (wasm-module-exports module)])
       (and (= 5 (vector-length import*))
            (= 5 (vector-length export*))
            (equal? '(function table memory global tag)
                    (map (lambda (import)
                           (wasm-external-type-kind
                            (wasm-import-external-type import)))
                         (vector->list import*)))
            (equal? '(function table memory global tag)
                    (map wasm-export-kind (vector->list export*)))
            (andmap zero? (map wasm-export-index (vector->list export*)))))

     (let ([module (parse-wasm-text-module "(module (func (type 99)))")])
       (= 99
          (wasm-function-type-index
           (vector-ref (wasm-module-functions module) 0))))

     ;; error: symbolic references must name a declaration in the matching namespace.
     (let ([error
            (capture-parser-error
             (lambda ()
               (parse-wasm-text-module
                "(module (func call $missing))")))])
       (and error (> (parser-error-offset error) 0)))

     ;; error: duplicate textual identifiers in one namespace are rejected.
     (error? (parse-wasm-text-module
              "(module (memory $m 1) (memory $m 1))"))

     ;; error: an inline signature must agree with its explicit function type.
     (error? (parse-wasm-text-module
              "(module
                 (type $sig (func (param i32)))
                 (func (type $sig) (param i64)))"))

     ;; error: named parameters and locals share one function-local namespace.
     (error? (parse-wasm-text-module
              "(module (func (param $x i32) (local $x i64)))"))

     ;; error: a symbolic branch label must name an enclosing structured instruction.
     (error? (parse-wasm-text-module
              "(module (func block br $missing end))"))

     ;; error: the source-string API requires a string.
     (error? (parse-wasm-text-module #vu8()))

     (with-temporary-wat
      "(module)"
      (lambda (path) (wasm-module? (parse-wasm-text-module-file path))))

     ;; error: the file API requires a regular-file path.
     (error? (parse-wasm-text-module-file "data/not-a-wasm-module.wat"))

     )

(define wasm-vector=?
  (lambda (left right value=?)
    (and (vector? right)
         (= (vector-length left) (vector-length right))
         (let loop ([index 0])
           (or (= index (vector-length left))
               (and (value=? (vector-ref left index) (vector-ref right index))
                    (loop (+ index 1))))))))

(define wasm-vector-any?
  (lambda (predicate vector)
    (let loop ([index 0])
      (and (< index (vector-length vector))
           (or (predicate (vector-ref vector index)) (loop (+ index 1)))))))

(define wasm-value=?
  (lambda (left right)
    (cond
      [(wasm-recursive-type? left) (wasm-recursive-type=? left right)]
      [(wasm-subtype? left) (wasm-subtype=? left right)]
      [(wasm-function-type? left) (wasm-function-type=? left right)]
      [(wasm-struct-type? left) (wasm-struct-type=? left right)]
      [(wasm-array-type? left) (wasm-array-type=? left right)]
      [(wasm-field-type? left) (wasm-field-type=? left right)]
      [(wasm-reference-type? left) (wasm-reference-type=? left right)]
      [(wasm-limits? left) (wasm-limits=? left right)]
      [(wasm-table-type? left) (wasm-table-type=? left right)]
      [(wasm-memory-type? left) (wasm-memory-type=? left right)]
      [(wasm-global-type? left) (wasm-global-type=? left right)]
      [(wasm-tag-type? left) (wasm-tag-type=? left right)]
      [(wasm-external-type? left) (wasm-external-type=? left right)]
      [(wasm-import? left) (wasm-import=? left right)]
      [(wasm-function? left) (wasm-function=? left right)]
      [(wasm-table? left) (wasm-table=? left right)]
      [(wasm-memory? left) (wasm-memory=? left right)]
      [(wasm-global? left) (wasm-global=? left right)]
      [(wasm-tag? left) (wasm-tag=? left right)]
      [(wasm-export? left) (wasm-export=? left right)]
      [(wasm-element? left) (wasm-element=? left right)]
      [(wasm-data? left) (wasm-data=? left right)]
      [(wasm-custom-section? left) (wasm-custom-section=? left right)]
      [(wasm-instruction? left) (wasm-instruction=? left right)]
      [(wasm-memory-argument? left) (wasm-memory-argument=? left right)]
      [(wasm-block-type? left) (wasm-block-type=? left right)]
      [(wasm-catch? left) (wasm-catch=? left right)]
      [(wasm-float? left) (wasm-float=? left right)]
      [(vector? left) (wasm-vector=? left right wasm-value=?)]
      [else (equal? left right)])))

(define wasm-recursive-type=?
  (lambda (left right)
    (and (wasm-recursive-type? right)
         (wasm-vector=? (wasm-recursive-type-subtypes left)
                        (wasm-recursive-type-subtypes right)
                        wasm-value=?))))

(define wasm-subtype=?
  (lambda (left right)
    (and (wasm-subtype? right)
         (eq? (wasm-subtype-final? left) (wasm-subtype-final? right))
         (wasm-vector=? (wasm-subtype-supertypes left)
                        (wasm-subtype-supertypes right)
                        wasm-value=?)
         (wasm-value=? (wasm-subtype-composite-type left)
                       (wasm-subtype-composite-type right)))))

(define wasm-function-type=?
  (lambda (left right)
    (and (wasm-function-type? right)
         (wasm-vector=? (wasm-function-type-parameters left)
                        (wasm-function-type-parameters right)
                        wasm-value=?)
         (wasm-vector=? (wasm-function-type-results left)
                        (wasm-function-type-results right)
                        wasm-value=?))))

(define wasm-struct-type=?
  (lambda (left right)
    (and (wasm-struct-type? right)
         (wasm-vector=? (wasm-struct-type-fields left)
                        (wasm-struct-type-fields right)
                        wasm-value=?))))

(define wasm-array-type=?
  (lambda (left right)
    (and (wasm-array-type? right)
         (wasm-value=? (wasm-array-type-field left) (wasm-array-type-field right)))))

(define wasm-field-type=?
  (lambda (left right)
    (and (wasm-field-type? right)
         (wasm-value=? (wasm-field-type-storage-type left)
                       (wasm-field-type-storage-type right))
         (eq? (wasm-field-type-mutable? left) (wasm-field-type-mutable? right)))))

(define wasm-reference-type=?
  (lambda (left right)
    (and (wasm-reference-type? right)
         (eq? (wasm-reference-type-nullable? left)
              (wasm-reference-type-nullable? right))
         (wasm-value=? (wasm-reference-type-heap-type left)
                       (wasm-reference-type-heap-type right)))))

(define wasm-limits=?
  (lambda (left right)
    (and (wasm-limits? right)
         (eq? (wasm-limits-address-type left) (wasm-limits-address-type right))
         (= (wasm-limits-minimum left) (wasm-limits-minimum right))
         (equal? (wasm-limits-maximum left) (wasm-limits-maximum right)))))

(define wasm-table-type=?
  (lambda (left right)
    (and (wasm-table-type? right)
         (wasm-value=? (wasm-table-type-reference-type left)
                       (wasm-table-type-reference-type right))
         (wasm-limits=? (wasm-table-type-limits left) (wasm-table-type-limits right)))))

(define wasm-memory-type=?
  (lambda (left right)
    (and (wasm-memory-type? right)
         (wasm-limits=? (wasm-memory-type-limits left)
                        (wasm-memory-type-limits right)))))

(define wasm-global-type=?
  (lambda (left right)
    (and (wasm-global-type? right)
         (wasm-value=? (wasm-global-type-value-type left)
                       (wasm-global-type-value-type right))
         (eq? (wasm-global-type-mutable? left) (wasm-global-type-mutable? right)))))

(define wasm-tag-type=?
  (lambda (left right)
    (and (wasm-tag-type? right)
         (= (wasm-tag-type-type-index left) (wasm-tag-type-type-index right)))))

(define wasm-external-type=?
  (lambda (left right)
    (and (wasm-external-type? right)
         (eq? (wasm-external-type-kind left) (wasm-external-type-kind right))
         (wasm-value=? (wasm-external-type-type left)
                       (wasm-external-type-type right)))))

(define wasm-import=?
  (lambda (left right)
    (and (wasm-import? right)
         (string=? (wasm-import-module left) (wasm-import-module right))
         (string=? (wasm-import-name left) (wasm-import-name right))
         (wasm-external-type=? (wasm-import-external-type left)
                               (wasm-import-external-type right)))))

(define wasm-function=?
  (lambda (left right)
    (and (wasm-function? right)
         (= (wasm-function-type-index left) (wasm-function-type-index right))
         (wasm-vector=? (wasm-function-locals left) (wasm-function-locals right) wasm-value=?)
         (wasm-vector=? (wasm-function-body left) (wasm-function-body right) wasm-value=?))))

(define wasm-table=?
  (lambda (left right)
    (and (wasm-table? right)
         (wasm-table-type=? (wasm-table-type left) (wasm-table-type right))
         (wasm-value=? (wasm-table-initializer left) (wasm-table-initializer right)))))

(define wasm-memory=?
  (lambda (left right)
    (and (wasm-memory? right)
         (wasm-memory-type=? (wasm-memory-type left) (wasm-memory-type right)))))

(define wasm-global=?
  (lambda (left right)
    (and (wasm-global? right)
         (wasm-global-type=? (wasm-global-type left) (wasm-global-type right))
         (wasm-value=? (wasm-global-initializer left) (wasm-global-initializer right)))))

(define wasm-tag=?
  (lambda (left right)
    (and (wasm-tag? right) (wasm-tag-type=? (wasm-tag-type left) (wasm-tag-type right)))))

(define wasm-export=?
  (lambda (left right)
    (and (wasm-export? right)
         (string=? (wasm-export-name left) (wasm-export-name right))
         (eq? (wasm-export-kind left) (wasm-export-kind right))
         (= (wasm-export-index left) (wasm-export-index right)))))

(define wasm-element=?
  (lambda (left right)
    (and (wasm-element? right)
         (eq? (wasm-element-mode left) (wasm-element-mode right))
         (wasm-value=? (wasm-element-reference-type left)
                       (wasm-element-reference-type right))
         (wasm-value=? (wasm-element-table-index left) (wasm-element-table-index right))
         (wasm-value=? (wasm-element-offset left) (wasm-element-offset right))
         (wasm-vector=? (wasm-element-initializers left)
                        (wasm-element-initializers right)
                        wasm-value=?))))

(define wasm-data=?
  (lambda (left right)
    (and (wasm-data? right)
         (eq? (wasm-data-mode left) (wasm-data-mode right))
         (wasm-value=? (wasm-data-memory-index left) (wasm-data-memory-index right))
         (wasm-value=? (wasm-data-offset left) (wasm-data-offset right))
         (equal? (wasm-data-bytes left) (wasm-data-bytes right)))))

(define wasm-custom-section=?
  (lambda (left right)
    (and (wasm-custom-section? right)
         (string=? (wasm-custom-section-name left) (wasm-custom-section-name right))
         (equal? (wasm-custom-section-bytes left) (wasm-custom-section-bytes right))
         (or (not (wasm-custom-section-after-section left))
             (not (wasm-custom-section-after-section right))
             (eq? (wasm-custom-section-after-section left)
                  (wasm-custom-section-after-section right))))))

(define wasm-instruction=?
  (lambda (left right)
    (and (wasm-instruction? right)
         (eq? (wasm-instruction-mnemonic left) (wasm-instruction-mnemonic right))
         (wasm-vector=? (wasm-instruction-immediates left)
                        (wasm-instruction-immediates right)
                        wasm-value=?)
         (wasm-vector=? (wasm-instruction-body left) (wasm-instruction-body right) wasm-value=?)
         (wasm-vector=? (wasm-instruction-alternate left)
                        (wasm-instruction-alternate right)
                        wasm-value=?))))

(define wasm-memory-argument=?
  (lambda (left right)
    (and (wasm-memory-argument? right)
         (= (wasm-memory-argument-alignment left) (wasm-memory-argument-alignment right))
         (= (wasm-memory-argument-offset left) (wasm-memory-argument-offset right))
         (= (wasm-memory-argument-memory-index left)
            (wasm-memory-argument-memory-index right)))))

(define wasm-block-type=?
  (lambda (left right)
    (and (wasm-block-type? right)
         (eq? (wasm-block-type-kind left) (wasm-block-type-kind right))
         (wasm-value=? (wasm-block-type-value left) (wasm-block-type-value right)))))

(define wasm-catch=?
  (lambda (left right)
    (and (wasm-catch? right)
         (eq? (wasm-catch-kind left) (wasm-catch-kind right))
         (wasm-value=? (wasm-catch-tag-index left) (wasm-catch-tag-index right))
         (= (wasm-catch-label-index left) (wasm-catch-label-index right)))))

(define wasm-float=?
  (lambda (left right)
    (and (wasm-float? right)
         (= (wasm-float-width left) (wasm-float-width right))
         (= (wasm-float-bits left) (wasm-float-bits right)))))

(define wasm-module=?
  (lambda (left right)
    (and (wasm-module? right)
         (wasm-vector=? (wasm-module-types left) (wasm-module-types right) wasm-value=?)
         (wasm-vector=? (wasm-module-imports left) (wasm-module-imports right) wasm-value=?)
         (wasm-vector=? (wasm-module-functions left) (wasm-module-functions right) wasm-value=?)
         (wasm-vector=? (wasm-module-tables left) (wasm-module-tables right) wasm-value=?)
         (wasm-vector=? (wasm-module-memories left) (wasm-module-memories right) wasm-value=?)
         (wasm-vector=? (wasm-module-globals left) (wasm-module-globals right) wasm-value=?)
         (wasm-vector=? (wasm-module-tags left) (wasm-module-tags right) wasm-value=?)
         (wasm-vector=? (wasm-module-exports left) (wasm-module-exports right) wasm-value=?)
         (wasm-value=? (wasm-module-start left) (wasm-module-start right))
         (wasm-vector=? (wasm-module-elements left) (wasm-module-elements right) wasm-value=?)
         (wasm-vector=? (wasm-module-data left) (wasm-module-data right) wasm-value=?)
         (wasm-vector=? (wasm-module-custom-sections left)
                        (wasm-module-custom-sections right)
                        wasm-value=?))))

(define wasm-section-count-matches?
  (lambda (output label values)
    (or (fxzero? (vector-length values))
        (string-contains? output
                          (format "~a[~a]:" label (vector-length values))))))

(define wasm-objdump-counts-match?
  (lambda (module output)
    (and (wasm-section-count-matches? output "Type" (wasm-module-types module))
         (wasm-section-count-matches? output "Function" (wasm-module-functions module))
         (wasm-section-count-matches? output "Table" (wasm-module-tables module))
         (wasm-section-count-matches? output "Memory" (wasm-module-memories module))
         (wasm-section-count-matches? output "Global" (wasm-module-globals module))
         (wasm-section-count-matches? output "Export" (wasm-module-exports module))
         (wasm-section-count-matches? output "Elem" (wasm-module-elements module))
         (wasm-section-count-matches? output "Data" (wasm-module-data module))
         (wasm-section-count-matches? output "Code" (wasm-module-functions module)))))

(define wasm-file-counts-match?
  (lambda (path)
    (let* ([module (parse-wasm-binary-module-file path)]
           [result
            (process-capture "wasm-objdump" "-x" (begin path)
              :stdout capture
              :stderr capture
              :timeout 10000)]
           [output (successful-process-output result)])
      (and output (wasm-objdump-counts-match? module output)))))

(mat wasm-fixtures

     (let* ([module (parse-wasm-binary-module-file "data/example.wasm")]
            [function (vector-ref (wasm-module-functions module) 0)]
            [export (vector-ref (wasm-module-exports module) 0)])
       (and (= 1 (vector-length (wasm-module-types module)))
            (= 1 (vector-length (wasm-module-functions module)))
            (fxzero? (vector-length (wasm-module-tables module)))
            (fxzero? (vector-length (wasm-module-memories module)))
            (fxzero? (vector-length (wasm-module-globals module)))
            (= 1 (vector-length (wasm-module-exports module)))
            (fxzero? (vector-length (wasm-module-elements module)))
            (fxzero? (vector-length (wasm-module-data module)))
            (equal? '#(i32 i32)
                    (wasm-function-type-parameters
                     (wasm-subtype-composite-type
                      (vector-ref
                       (wasm-recursive-type-subtypes (vector-ref (wasm-module-types module) 0))
                       0))))
            (equal? '#(i32)
                    (wasm-function-type-results
                     (wasm-subtype-composite-type
                      (vector-ref
                       (wasm-recursive-type-subtypes (vector-ref (wasm-module-types module) 0))
                       0))))
            (= 0 (wasm-function-type-index function))
            (string=? "add" (wasm-export-name export))
            (eq? 'function (wasm-export-kind export))
            (= 0 (wasm-export-index export))
            (equal? '(local.get local.get i32.add)
                    (map wasm-instruction-mnemonic
                         (vector->list (wasm-function-body function))))))

     (let* ([module (parse-wasm-binary-module-file "data/fibonacci.wasm")]
            [table (vector-ref (wasm-module-tables module) 0)]
            [memory (vector-ref (wasm-module-memories module) 0)]
            [element (vector-ref (wasm-module-elements module) 0)]
            [data (vector-ref (wasm-module-data module) 0)]
            [export-name* (vector-map wasm-export-name (wasm-module-exports module))]
            [custom-name* (vector-map wasm-custom-section-name
                                      (wasm-module-custom-sections module))])
       (and (= 11 (vector-length (wasm-module-types module)))
            (= 57 (vector-length (wasm-module-functions module)))
            (= 1 (vector-length (wasm-module-tables module)))
            (= 18 (wasm-limits-minimum (wasm-table-type-limits (wasm-table-type table))))
            (= 18 (wasm-limits-maximum (wasm-table-type-limits (wasm-table-type table))))
            (= 1 (vector-length (wasm-module-memories module)))
            (= 17 (wasm-limits-minimum (wasm-memory-type-limits (wasm-memory-type memory))))
            (= 3 (vector-length (wasm-module-globals module)))
            (= 4 (vector-length (wasm-module-exports module)))
            (equal? '#("memory" "fibonacci" "__data_end" "__heap_base") export-name*)
            (= 1 (vector-length (wasm-module-elements module)))
            (eq? 'active (wasm-element-mode element))
            (= 17 (vector-length (wasm-element-initializers element)))
            (= 1 (vector-length (wasm-module-data module)))
            (eq? 'active (wasm-data-mode data))
            (= 916 (bytevector-length (wasm-data-bytes data)))
            (wasm-vector-any? (lambda (name) (string=? "name" name)) custom-name*)))

     (or (not (external-tool-available? "wasm-objdump"))
         (wasm-file-counts-match? "data/example.wasm"))

     (or (not (external-tool-available? "wasm-objdump"))
         (wasm-file-counts-match? "data/fibonacci.wasm"))

     (let ([text-module (parse-wasm-text-module-file "data/wasm-core3.wat")]
           [binary-module (parse-wasm-binary-module-file "data/wasm-core3.wasm")])
       (wasm-module=? text-module binary-module))

     )

(include "parser-wasm-review.ss")
