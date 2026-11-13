(library (cspp parser wasm binary instructions)
  (export <wasm-memory-argument> <wasm-instruction> <wasm-expression>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm binary types)
          (cspp parser wasm binary values)
          (cspp parser wasm opcodes)
          (cspp parser wasm private)
          (cspp parser wasm types))

  (define empty-vector '#())

  (define one-immediate
    (lambda (parser)
      (<map> immutable-vector parser)))

  (define pair-immediates
    (lambda (first-parser second-parser)
      (<map> list->immutable-vector (<~> first-parser second-parser))))

  (define reversed-pair-immediates
    (lambda (first-parser second-parser)
      (<map> (lambda (values)
               (immutable-vector (cadr values) (car values)))
             (<~> first-parser second-parser))))

  (define instruction-record
    (lambda (descriptor immediates)
      (make-wasm-instruction
       (wasm-opcode-mnemonic descriptor) immediates empty-vector empty-vector)))

  (define opcode-descriptor-parser
    (<bind>
     <u8>
     (lambda (opcode)
       (if (memv opcode '(#xfb #xfc #xfd))
           (<bind> <wasm-u32>
                   (lambda (subopcode)
                     (let ([descriptor (wasm-opcode-by-binary opcode subopcode)])
                       (if descriptor
                           (<result> descriptor)
                           (<fail-with> "unknown WebAssembly prefixed opcode")))))
           (let ([descriptor (wasm-opcode-by-binary #f opcode)])
             (if descriptor
                 (<result> descriptor)
                 (<fail-with> "unknown WebAssembly opcode")))))))

  #|proc:<wasm-memory-argument>
  The `<wasm-memory-argument>` parser reads a Core 3.0 alignment, memory index, and offset.
  |#
  (define <wasm-memory-argument>
    (<bind>
     <wasm-u32>
     (lambda (flags)
       (cond [(< flags 64)
              (<map> (lambda (offset)
                       (make-wasm-memory-argument flags offset 0))
                     <wasm-u64>)]
             [(< flags 128)
              (<map> (lambda (values)
                       (make-wasm-memory-argument
                        (- flags 64) (cadr values) (car values)))
                     (<~> <wasm-u32> <wasm-u64>))]
             [else
              (<fail-with> "invalid WebAssembly memory argument flags")]))))

  (define catch-parser
    (<bind>
     <u8>
     (lambda (kind)
       (case kind
         [(0)
          (<map> (lambda (values)
                   (make-wasm-catch 'catch (car values) (cadr values)))
                 (<~> <wasm-u32> <wasm-u32>))]
         [(1)
          (<map> (lambda (values)
                   (make-wasm-catch 'catch-ref (car values) (cadr values)))
                 (<~> <wasm-u32> <wasm-u32>))]
         [(2)
          (<map> (lambda (label-index)
                   (make-wasm-catch 'catch-all #f label-index))
                 <wasm-u32>)]
         [(3)
          (<map> (lambda (label-index)
                   (make-wasm-catch 'catch-all-ref #f label-index))
                 <wasm-u32>)]
         [else (<fail-with> "invalid WebAssembly catch kind")]))))

  (define two-u32-immediates
    (pair-immediates <wasm-u32> <wasm-u32>))

  (define reference-type-immediates
    (one-immediate <wasm-reference-type>))

  (define value-type-vector-immediates
    (<map> (lambda (types)
             (immutable-vector (vector->immutable-vector types)))
           (<wasm-vector> <wasm-value-type>)))

  (define non-null-heap-type-immediates
    (<map> (lambda (heap-type)
             (immutable-vector (make-wasm-reference-type #f heap-type)))
           <wasm-heap-type>))

  (define nullable-heap-type-immediates
    (<map> (lambda (heap-type)
             (immutable-vector (make-wasm-reference-type #t heap-type)))
           <wasm-heap-type>))

  (define vector-bytes-immediates
    (one-immediate (<u8vec> 16)))

  (define shuffle-lane-parser
    (<bind>
     <u8>
     (lambda (lane)
       (if (fx< lane 32)
           (<result> lane)
           (<fail-with> "invalid WebAssembly shuffle lane")))))

  (define shuffle-bytes-immediates
    (<map> (lambda (lane*)
             (immutable-vector (u8-list->bytevector lane*)))
           (<rep> shuffle-lane-parser 16)))

  ;; Core 3.0 binary grammar, `Bcastop` and `Binstr/cast`, uses flags before all fields.
  (define br-on-cast-flags-parser
    (<bind>
     <u8>
     (lambda (flags)
       (if (fx< flags 4)
           (<result> flags)
           (<fail-with> "invalid WebAssembly br-on-cast flags")))))

  (define br-on-cast-immediates
    (<bind>
     br-on-cast-flags-parser
     (lambda (flags)
       (<map>
        (lambda (values)
          (immutable-vector
           (car values)
           (make-wasm-reference-type (not (fxzero? (fxand flags 1)))
                                     (cadr values))
           (make-wasm-reference-type (not (fxzero? (fxand flags 2)))
                                     (caddr values))))
        (<~> <wasm-u32> <wasm-heap-type> <wasm-heap-type>)))))

  (define lane-index-parser
    (lambda (descriptor)
      (let ([lane-count (wasm-instruction-lane-count (wasm-opcode-mnemonic descriptor))])
        (if lane-count
            (<bind>
             <u8>
             (lambda (lane)
               (if (fx< lane lane-count)
                   (<result> lane)
                   (<fail-with> "invalid WebAssembly lane index"))))
            (<fail-with> "unclassified WebAssembly lane instruction")))))

  (define immediate-parser
    (lambda (descriptor)
      (case (wasm-opcode-immediate-shape descriptor)
        [(none) (<result> empty-vector)]
        [(block-type) (one-immediate <wasm-block-type>)]
        [(label-index function-index type-index table-index memory-index global-index
                      local-index tag-index data-index element-index)
         (one-immediate <wasm-u32>)]
        [(label-vector)
         (<map> (lambda (values)
                  (immutable-vector
                   (vector->immutable-vector (car values)) (cadr values)))
                (<~> (<wasm-vector> <wasm-u32>) <wasm-u32>))]
        [(heap-type) (one-immediate <wasm-heap-type>)]
        [(reference-type) reference-type-immediates]
        [(value-type-vector select-types) value-type-vector-immediates]
        [(call-indirect) two-u32-immediates]
        [(memory-argument) (one-immediate <wasm-memory-argument>)]
        [(i32) (one-immediate <wasm-s32>)]
        [(i64) (one-immediate <wasm-s64>)]
        [(f32) (one-immediate <wasm-f32>)]
        [(f64) (one-immediate <wasm-f64>)]
        [(table-pair memory-pair) two-u32-immediates]
        [(memory-data table-element)
         (reversed-pair-immediates <wasm-u32> <wasm-u32>)]
        [(struct-field array-new-fixed array-copy type-data type-element)
         two-u32-immediates]
        [(heap-type-non-null) non-null-heap-type-immediates]
        [(heap-type-nullable) nullable-heap-type-immediates]
        [(br-on-cast) br-on-cast-immediates]
        [(vector-bytes) vector-bytes-immediates]
        [(shuffle-bytes) shuffle-bytes-immediates]
        [(lane-index) (one-immediate (lane-index-parser descriptor))]
        [(memory-argument-lane)
         (pair-immediates <wasm-memory-argument> (lane-index-parser descriptor))]
        [(try-table)
         (<fail-with> "try-table immediate must be parsed structurally")]
        [else
         (<fail-with>
          (format "unsupported immediate shape: ~a"
                  (wasm-opcode-immediate-shape descriptor)))])))

  #|proc:<wasm-instruction>
  The `<wasm-instruction>` parser reads one scalar or structured Core 3.0 instruction.
  |#
  (declare-lazy-parser <wasm-instruction>)

  (define sequence-before
    (lambda (terminator-parser)
      ;; Non-terminator bytes need no mismatch diagnostic: many-until discards it.
      ;; Leave terminator candidates and EOF on the original diagnostic path.
      (define-parser guarded-terminator
        (let ([position (input-pos inp)])
          (if (and (fx< position (input-len inp))
                   (not (memv (bytevector-u8-ref (binary-input-data inp) position)
                              '(#x05 #x0b))))
              (values #f #f inp)
              (parser-call terminator-parser inp state lvl))))
      (<map> list->immutable-vector
             (<many-until> <wasm-instruction> guarded-terminator))))

  (define sequence-ending-with-end
    (<~ (sequence-before (<uimm8> #x0b)) (<uimm8> #x0b)))

  (define block-instruction-parser
    (lambda (descriptor)
      (<map>
       (lambda (values)
         (make-wasm-instruction
          (wasm-opcode-mnemonic descriptor)
          (immutable-vector (car values))
          (cadr values)
          empty-vector))
       (<~> <wasm-block-type> sequence-ending-with-end))))

  (define if-instruction-parser
    (lambda (descriptor)
      (<bind>
       <wasm-block-type>
       (lambda (block-type)
         (<bind>
          (sequence-before (</> (<uimm8> #x05) (<uimm8> #x0b)))
          (lambda (body)
            (</>
             (<map> (lambda (alternate)
                      (make-wasm-instruction
                       (wasm-opcode-mnemonic descriptor)
                       (immutable-vector block-type)
                       body
                       alternate))
                    (<~1> (<uimm8> #x05) sequence-ending-with-end))
             (<as> (make-wasm-instruction
                    (wasm-opcode-mnemonic descriptor)
                    (immutable-vector block-type)
                    body
                    empty-vector)
                   (<uimm8> #x0b)))))))))

  (define try-table-instruction-parser
    (lambda (descriptor)
      (<bind>
       <wasm-block-type>
       (lambda (block-type)
         (<bind>
          (<wasm-vector> catch-parser)
          (lambda (catches)
            (<map> (lambda (body)
                     (make-wasm-instruction
                      (wasm-opcode-mnemonic descriptor)
                      (immutable-vector block-type)
                      body
                      (vector->immutable-vector catches)))
                   sequence-ending-with-end)))))))

  (define structured-instruction-parser
    (lambda (descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (block-instruction-parser descriptor)]
        [(if) (if-instruction-parser descriptor)]
        [(try-table) (try-table-instruction-parser descriptor)]
        [else (<fail-with> "unsupported structured WebAssembly instruction")])))

  (define ordinary-instruction-parser
    (lambda (descriptor)
      (let ([mnemonic (wasm-opcode-mnemonic descriptor)])
        (cond [(memq mnemonic '(else end))
               (<fail-with> "unexpected WebAssembly instruction terminator")]
              [(wasm-opcode-structured-kind descriptor)
               (structured-instruction-parser descriptor)]
              [else
               (<map> (lambda (immediates)
                        (instruction-record descriptor immediates))
                      (immediate-parser descriptor))]))))

  #|proc:<wasm-expression>
  The `<wasm-expression>` parser reads an instruction vector terminated by one end byte.
  |#
  (define <wasm-expression> sequence-ending-with-end)

  ;; Prepare fixed compositions before sharing parsers. Each invocation still creates its
  ;; own instruction records and nonempty immediate vectors, including in structured bodies.
  (define instruction-parsers
    (let ([table (make-eq-hashtable)])
      (vector-for-each
       (lambda (descriptor)
         (hashtable-set! table descriptor (ordinary-instruction-parser descriptor)))
       wasm-core-3-opcodes)
      ;; Untyped select and nullable ref.test/ref.cast have encoding-specific shapes.
      (for-each
       (lambda (encoding)
         (let ([descriptor (wasm-opcode-by-binary (car encoding) (cdr encoding))])
           (hashtable-set! table descriptor (ordinary-instruction-parser descriptor))))
       '((#f . #x1b) (#xfb . 21) (#xfb . 23)))
      (hashtable-copy table #f)))

  (install-lazy-parser!
   <wasm-instruction>
   (<bind> opcode-descriptor-parser
           (lambda (descriptor) (hashtable-ref instruction-parsers descriptor #f))))

  )
