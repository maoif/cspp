(library (cspp parser wasm text instructions)
  (export make-wat-catch-syntax wat-catch-syntax? wat-catch-syntax-pos
          wat-catch-syntax-kind wat-catch-syntax-tag wat-catch-syntax-label
          <wat-instruction> <wat-folded-instruction> <wat-expression>)
  (import (cspp chez)
          (only (cspp list) make-list-builder)
          (cspp parser combinator)
          (cspp parser wasm opcodes)
          (cspp parser wasm types)
          (cspp parser wasm text lexical)
          (cspp parser wasm text types)
          (cspp parser wasm private)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; Instruction tokens and immediate parsers
;;;;===----------------------------------------------------------------------===

  (define empty-vector '#())

  (define flatten-lists
    (lambda (value**)
      (let ([builder (make-list-builder)])
        (for-each (lambda (value*) (for-each builder value*)) value**)
        (builder))))

  (define optional-value
    (lambda (value default)
      (if (null? value) default value)))

  (define-record-type ($wat-catch-syntax $make-wat-catch-syntax $wat-catch-syntax?)
    (fields (immutable pos $wat-catch-syntax-pos)
            (immutable kind $wat-catch-syntax-kind)
            (immutable tag $wat-catch-syntax-tag)
            (immutable label $wat-catch-syntax-label)))

  (define optional-index-reference?
    (lambda (value)
      (or (not value) (wat-index-reference? value))))

  #|proc:make-wat-catch-syntax
  Creates private catch syntax. `pos` is the source offset, `kind` is the catch kind, `tag` is
  the optional tag reference, and `label` is the required label reference. Returns catch syntax.
  |#
  (define make-wat-catch-syntax
    (lambda (pos kind tag label)
      (pcheck ([natural? pos] [symbol? kind] [optional-index-reference? tag]
               [wat-index-reference? label])
              (unless (and (memq kind '(catch catch-ref catch-all catch-all-ref))
                           (if (memq kind '(catch catch-ref)) tag (not tag)))
                (errorf 'make-wat-catch-syntax "invalid catch syntax fields"))
              ($make-wat-catch-syntax pos kind tag label))))

  #|proc:wat-catch-syntax?
  Returns whether `object` is private catch syntax. `object` is tested.
  |#
  (define wat-catch-syntax?
    (lambda (object)
      (pcheck () ($wat-catch-syntax? object))))

  #|proc:wat-catch-syntax-pos
  Returns the source offset of catch syntax `record`.
  |#
  (define wat-catch-syntax-pos
    (lambda (record)
      (pcheck ([$wat-catch-syntax? record]) ($wat-catch-syntax-pos record))))

  #|proc:wat-catch-syntax-kind
  Returns the catch kind of catch syntax `record`.
  |#
  (define wat-catch-syntax-kind
    (lambda (record)
      (pcheck ([$wat-catch-syntax? record]) ($wat-catch-syntax-kind record))))

  #|proc:wat-catch-syntax-tag
  Returns the optional tag reference of catch syntax `record`.
  |#
  (define wat-catch-syntax-tag
    (lambda (record)
      (pcheck ([$wat-catch-syntax? record]) ($wat-catch-syntax-tag record))))

  #|proc:wat-catch-syntax-label
  Returns the label reference of catch syntax `record`.
  |#
  (define wat-catch-syntax-label
    (lambda (record)
      (pcheck ([$wat-catch-syntax? record]) ($wat-catch-syntax-label record))))

  (define instruction-character?
    (lambda (character)
      (or (char<=? #\0 character #\9)
          (char<=? #\a character #\z)
          (char<=? #\A character #\Z)
          (memv character '(#\. #\_)))))

  (define <wat-instruction-token>
    (<wat-token>
     (<as-string>
      (<some> (<satisfy-char> instruction-character?
                              "not a WebAssembly instruction character")))))

  (define text-mnemonic->symbol
    (lambda (text)
      (string->symbol
       (list->string
        (map (lambda (character)
               (if (char=? character #\_) #\- character))
             (string->list text))))))

  (define descriptor-parser
    (<bind>
     <wat-instruction-token>
     (lambda (text)
       (let ([descriptor (wasm-opcode-by-mnemonic (text-mnemonic->symbol text))])
         (if descriptor
             (<result> descriptor)
             (<fail-with> (format "unknown WebAssembly instruction: ~a" text)))))))

  (define one-immediate
    (lambda (parser)
      (<map> (lambda (value) (immutable-vector value)) parser)))

  (define two-immediates
    (lambda (first second)
      (<map> list->immutable-vector (<~> first second))))

  (define index-immediate (one-immediate <wat-index-reference>))

  (define indexed-pair-immediate
    (two-immediates <wat-index-reference> <wat-index-reference>))

  (define default-index-reference
    (lambda ()
      (make-wat-index-reference 0 0)))

  (define optional-index-immediate
    (<map> (lambda (index*)
             (immutable-vector
              (optional-value index* (default-index-reference))))
           (<optional> <wat-index-reference>)))

  (define optional-pair-immediate
    (</> indexed-pair-immediate
         (<result> (immutable-vector
                    (default-index-reference) (default-index-reference)))))

  (define optional-leading-index-pair-immediate
    (</> indexed-pair-immediate
         (<map> (lambda (second)
                  (immutable-vector (default-index-reference) second))
                <wat-index-reference>)))

  (define select-types-immediate
    (<map>
     (lambda (type**)
       (if (null? type**)
           empty-vector
           (immutable-vector
            (list->immutable-vector (flatten-lists type**)))))
     (<many> (wat-parenthesized "result" (<many> <wat-value-type>)))))

  (define call-indirect-immediate
    (<map>
     (lambda (value)
       (immutable-vector (cadr value)
                              (optional-value (car value)
                                              (make-wat-index-reference 0 0))))
     (<~> (<optional> <wat-index-reference>) <wat-anonymous-type-use>)))

  (define named-attribute
    (lambda (name parser)
      (<~1> (<string> name) parser)))

  (define exact-log2
    (lambda (value)
      (and (positive? value)
           (let loop ([value value] [exponent 0])
             (cond [(= value 1) exponent]
                   [(odd? value) #f]
                   [else (loop (quotient value 2) (fx1+ exponent))])))))

  (define memory-natural-alignment
    (lambda (mnemonic)
      (cond
       [(memq mnemonic '(v128.load v128.store)) 4]
       [(memq mnemonic
              '(i64.load f64.load i64.store f64.store
                v128.load8x8-s v128.load8x8-u v128.load16x4-s
                v128.load16x4-u v128.load32x2-s v128.load32x2-u
                v128.load64-splat v128.load64-zero
                v128.load64-lane v128.store64-lane))
        3]
       [(memq mnemonic
              '(i32.load f32.load i64.load32-s i64.load32-u
                i32.store f32.store i64.store32
                v128.load32-splat v128.load32-zero
                v128.load32-lane v128.store32-lane))
        2]
       [(memq mnemonic
              '(i32.load16-s i32.load16-u i64.load16-s i64.load16-u
                i32.store16 i64.store16 v128.load16-splat
                v128.load16-lane v128.store16-lane))
        1]
       [else 0])))

  (define explicit-alignment
    (<bind>
     (named-attribute "align=" <wat-u64>)
     (lambda (alignment)
       (let ([exponent (exact-log2 alignment)])
         (if exponent
             (<result> exponent)
             (<fail-with> "WebAssembly alignment is not a positive power of two"))))))

  (define memory-argument-with-index
    (lambda (descriptor index-parser)
      (<map>
       (lambda (value)
         (immutable-vector
          (immutable-vector
           (optional-value (caddr value)
                           (memory-natural-alignment
                            (wasm-opcode-mnemonic descriptor)))
           (optional-value (cadr value) 0)
           (car value))))
       (<~> index-parser
            (<optional> (named-attribute "offset=" <wat-u64>))
            (<optional> explicit-alignment)))))

  (define memory-argument-immediate
    (lambda (descriptor)
      (memory-argument-with-index
       descriptor
       (<map> (lambda (index*)
                (optional-value index* (default-index-reference)))
              (<optional> <wat-index-reference>)))))

  (define memory-argument-lane-immediate
    (lambda (descriptor)
      (let ([combine
             (lambda (memory-argument-parser)
               (<map> (lambda (value)
                        (immutable-vector
                         (vector-ref (car value) 0) (cadr value)))
                      (<~> memory-argument-parser (lane-parser descriptor))))])
        (</> (combine (memory-argument-with-index descriptor <wat-index-reference>))
             (combine (memory-argument-with-index
                       descriptor (<result> (default-index-reference))))))))

  (define lane-parser
    (lambda (descriptor)
      (<bind>
       <wat-u32>
       (lambda (lane)
         (let ([count (wasm-instruction-lane-count (wasm-opcode-mnemonic descriptor))])
           (if (and count (< lane count))
               (<result> lane)
               (<fail-with> "invalid WebAssembly lane index")))))))

  (define shuffle-immediate
    (<bind>
     (<rep> <wat-u32> 16)
     (lambda (lane*)
       (if (andmap (lambda (lane) (< lane 32)) lane*)
           (<result> (immutable-vector (apply bytevector lane*)))
           (<fail-with> "invalid WebAssembly shuffle lane")))))

  (define vector-lanes-immediate
    (lambda (lane-parser lane-count lane-width lane-bits)
      (<map>
       (lambda (lane*)
         (let ([bytes (make-bytevector 16)])
           (let loop ([index 0] [lane* lane*])
             (unless (null? lane*)
               (let ([offset (* index lane-width)] [lane (car lane*)])
                 (case lane-width
                   [(1) (bytevector-u8-set! bytes offset lane)]
                   [(2) (bytevector-u16-set! bytes offset lane (endianness little))]
                   [(4) (bytevector-u32-set! bytes offset (lane-bits lane)
                                            (endianness little))]
                   [(8) (bytevector-u64-set! bytes offset (lane-bits lane)
                                            (endianness little))])
                 (loop (fx1+ index) (cdr lane*)))))
           (immutable-vector bytes)))
       (<rep> lane-parser lane-count))))

  (define vector-immediate
    (</> (<~1> (<wat-keyword> "i8x16")
                (vector-lanes-immediate
                 <wat-i8> 16 1 values))
         (<~1> (<wat-keyword> "i16x8")
                (vector-lanes-immediate
                 <wat-i16> 8 2 values))
         (<~1> (<wat-keyword> "i32x4")
                (vector-lanes-immediate <wat-i32> 4 4 values))
         (<~1> (<wat-keyword> "i64x2")
                (vector-lanes-immediate <wat-i64> 2 8 values))
         (<~1> (<wat-keyword> "f32x4")
                (vector-lanes-immediate
                 <wat-f32> 4 4 wasm-float-bits))
         (<~1> (<wat-keyword> "f64x2")
                (vector-lanes-immediate
                 <wat-f64> 2 8 wasm-float-bits))))

  (define br-on-cast-immediate
    (<map> list->immutable-vector
           (<~> <wat-index-reference> <wat-reference-type> <wat-reference-type>)))

  (define make-immediate-parser
    (lambda (descriptor)
      (case (wasm-opcode-immediate-shape descriptor)
        [(none) (<result> empty-vector)]
        [(block-type) (one-immediate <wat-block-type>)]
        [(label-index function-index type-index global-index local-index tag-index
                      data-index element-index)
         index-immediate]
        [(table-index memory-index) optional-index-immediate]
        [(label-vector)
         (<map> (lambda (index*)
                  (let* ([count (fx1- (length index*))]
                         [labels (make-vector count)])
                    (let loop ([index 0] [rest index*])
                      (if (fx= index count)
                          (immutable-vector
                           (vector->immutable-vector labels) (car rest))
                          (begin
                            (vector-set! labels index (car rest))
                            (loop (fx1+ index) (cdr rest)))))))
                (<some> <wat-index-reference>))]
        [(heap-type) (one-immediate <wat-heap-type>)]
        [(heap-type-non-null heap-type-nullable)
         (one-immediate <wat-reference-type>)]
        [(reference-type) (one-immediate <wat-reference-type>)]
        [(value-type-vector select-types) select-types-immediate]
        [(call-indirect) call-indirect-immediate]
        [(memory-argument) (memory-argument-immediate descriptor)]
        [(i32) (one-immediate <wat-i32>)]
        [(i64) (one-immediate <wat-i64>)]
        [(f32) (one-immediate <wat-f32>)]
        [(f64) (one-immediate <wat-f64>)]
        [(table-pair memory-pair) optional-pair-immediate]
        [(struct-field array-copy type-data type-element)
         indexed-pair-immediate]
        [(array-new-fixed)
         (two-immediates <wat-index-reference> <wat-u32>)]
        [(memory-data table-element) optional-leading-index-pair-immediate]
        [(br-on-cast) br-on-cast-immediate]
        [(vector-bytes) vector-immediate]
        [(shuffle-bytes) shuffle-immediate]
        [(lane-index) (one-immediate (lane-parser descriptor))]
        [(memory-argument-lane) (memory-argument-lane-immediate descriptor)]
        [(try-table) (<result> empty-vector)]
        [else
         (<fail-with>
          (format "unsupported WebAssembly text immediate shape: ~a"
                  (wasm-opcode-immediate-shape descriptor)))])))

  ;; Cache fixed compositions only; their callbacks still create invocation-local results.
  (define immediate-parsers
    (let ([table (make-eq-hashtable)])
      (vector-for-each
       (lambda (descriptor)
         (hashtable-set! table descriptor (make-immediate-parser descriptor)))
       wasm-core-3-opcodes)
      (hashtable-copy table #f)))

  (define immediate-parser
    (lambda (descriptor) (hashtable-ref immediate-parsers descriptor #f)))

;;;;===----------------------------------------------------------------------===
;;;; Flat and folded instruction grammar
;;;;===----------------------------------------------------------------------===

  (define labels-match?
    (lambda (opening closing)
      (or (not closing) (and opening (string=? opening closing)))))

  (define closing-label
    (lambda (opening)
      (<bind>
       (<optional> <wat-identifier>)
       (lambda (closing*)
         (let ([closing (optional-value closing* #f)])
           (if (labels-match? opening closing)
               (<result> closing)
               (<fail-with> "structured instruction labels do not match")))))))

  (declare-lazy-parser <wat-flat-instruction>)
  (declare-lazy-parser <wat-folded-node>)
  (declare-lazy-parser <wat-sequence-node>)

  ;; Keep only direct operand nodes while parsing, then emit execution order once per body.
  (define instruction-list
    (lambda (roots)
      (let ([builder (make-list-builder)])
        (define emit!
          (lambda (instruction)
            (vector-for-each emit! (wat-instruction-syntax-operands instruction))
            (builder instruction)))
        (for-each emit! roots)
        (builder))))

  (define instruction-vector
    (lambda (roots) (list->immutable-vector (instruction-list roots))))

  (define flat-sequence-before
    (lambda (terminator)
      (<map> instruction-list (<many-until> <wat-sequence-node> terminator))))

  (define make-instruction
    (lambda (pos descriptor immediates body alternate operands label)
      (make-wat-instruction-syntax
       pos (wasm-opcode-mnemonic descriptor) immediates body alternate operands label)))

  (define flat-simple-after
    (lambda (pos descriptor)
      (<map> (lambda (immediates)
               (make-instruction pos descriptor immediates empty-vector #f empty-vector #f))
             (immediate-parser descriptor))))

  (define flat-block-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (let ([label (optional-value (car header) #f)] [type (cadr header)])
           (<bind>
            (flat-sequence-before (<wat-keyword> "end"))
            (lambda (body)
              (<map>
               (lambda (unused)
                 (make-instruction
                  pos descriptor (immutable-vector type)
                  (list->immutable-vector body) #f empty-vector label))
               (<~0> (<wat-keyword> "end") (closing-label label))))))))))

  (define flat-if-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (let ([label (optional-value (car header) #f)] [type (cadr header)])
           (<bind>
            (flat-sequence-before
             (</> (<wat-keyword> "else") (<wat-keyword> "end")))
            (lambda (body)
              (</>
               (<bind>
                (<~0> (<wat-keyword> "else") (closing-label label))
                (lambda (unused)
                  (<bind>
                   (flat-sequence-before (<wat-keyword> "end"))
                   (lambda (alternate)
                     (<map>
                      (lambda (unused)
                        (make-instruction
                         pos descriptor (immutable-vector type)
                         (list->immutable-vector body)
                         (list->immutable-vector alternate) empty-vector label))
                      (<~0> (<wat-keyword> "end") (closing-label label)))))))
               (<map>
                (lambda (unused)
                  (make-instruction
                   pos descriptor (immutable-vector type)
                   (list->immutable-vector body) empty-vector empty-vector label))
                (<~0> (<wat-keyword> "end") (closing-label label)))))))))))

  (define try-catch-kind
    (</> (<as> 'catch-all-ref (<wat-keyword> "catch_all_ref"))
         (<as> 'catch-all (<wat-keyword> "catch_all"))
         (<as> 'catch-ref (<wat-keyword> "catch_ref"))
         (<as> 'catch (<wat-keyword> "catch"))))

  (define try-catch-clause
    (<~2>
     (<followed-by> (<result> #t) (<~0> wat-open try-catch-kind))
     wat-open
     (<bind>
      (<~> <pos> try-catch-kind)
      (lambda (head)
        (let ([pos (car head)] [kind (cadr head)])
          (<map>
           (lambda (value)
             (if (memq kind '(catch catch-ref))
                 (make-wat-catch-syntax pos kind (vector-ref value 0)
                                        (vector-ref value 1))
                 (make-wat-catch-syntax pos kind #f (vector-ref value 0))))
           (if (memq kind '(catch catch-ref))
               (two-immediates <wat-index-reference> <wat-index-reference>)
               (one-immediate <wat-index-reference>))))))
     wat-close))

  (define flat-try-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> try-catch-clause))
       (lambda (header)
         (let ([label (optional-value (car header) #f)])
           (<bind>
            (flat-sequence-before (<wat-keyword> "end"))
            (lambda (body)
              (<map>
               (lambda (unused)
                 (make-instruction
                  pos descriptor
                  (immutable-vector (cadr header))
                  (list->immutable-vector body)
                  (list->immutable-vector (caddr header)) empty-vector label))
               (<~0> (<wat-keyword> "end") (closing-label label))))))))))

  (define flat-after
    (lambda (pos descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (flat-block-after pos descriptor)]
        [(if) (flat-if-after pos descriptor)]
        [(try-table) (flat-try-after pos descriptor)]
        [else
         (if (memq (wasm-opcode-mnemonic descriptor) '(else end))
             (<fail-with> "reserved structured terminator")
             (flat-simple-after pos descriptor))])))

  (define folded-simple-after
    (lambda (pos descriptor)
      (<bind>
       (immediate-parser descriptor)
       (lambda (immediates)
         (<map>
          (lambda (operands)
            (make-instruction pos descriptor immediates empty-vector #f
                              (list->immutable-vector operands) #f))
          (<many-until> <wat-folded-node> wat-close))))))

  (define folded-block-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (<map>
          (lambda (body)
            (make-instruction
             pos descriptor (immutable-vector (cadr header))
             (instruction-vector body) #f empty-vector
             (optional-value (car header) #f)))
          (<many-until> <wat-sequence-node> wat-close))))))

  (define folded-try-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> try-catch-clause))
       (lambda (header)
         (<map>
          (lambda (body)
            (make-instruction
             pos descriptor (immutable-vector (cadr header))
             (instruction-vector body)
             (list->immutable-vector (caddr header)) empty-vector
             (optional-value (car header) #f)))
          (<many-until> <wat-sequence-node> wat-close))))))

  (define folded-branch
    (lambda (keyword)
      (wat-parenthesized keyword (<map> instruction-vector (<many> <wat-sequence-node>)))))

  (define folded-if-after
    (lambda (pos descriptor)
      (<map>
       (lambda (value)
         (make-instruction
          pos descriptor (immutable-vector (cadr value))
          (cadddr value) (optional-value (car (cddddr value)) empty-vector)
          (list->immutable-vector (caddr value))
          (optional-value (car value) #f)))
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> <wat-folded-node>)
            (folded-branch "then")
            (<optional> (folded-branch "else"))))))

  (define folded-after
    (lambda (pos descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (folded-block-after pos descriptor)]
        [(try-table) (folded-try-after pos descriptor)]
        [(if) (folded-if-after pos descriptor)]
        [else
         (if (memq (wasm-opcode-mnemonic descriptor) '(else end))
             (<fail-with> "reserved structured terminator")
             (folded-simple-after pos descriptor))])))

  (define flat-parser
    (<bind> (<~> <pos> descriptor-parser)
            (lambda (value) (flat-after (car value) (cadr value)))))

  (define folded-parser
    (<~1>
     wat-open
     (<bind> (<~> <pos> descriptor-parser)
             (lambda (value)
               (<~0> (folded-after (car value) (cadr value)) wat-close)))))

  #|proc:<wat-instruction>
  The `<wat-instruction>` parser reads one flat Core 3.0 WAT instruction and returns positioned
  instruction syntax. Structured bodies accept mixed flat and folded instructions.
  |#
  (define <wat-instruction>
    (begin
      (install-lazy-parser! <wat-flat-instruction> flat-parser)
      (install-lazy-parser! <wat-folded-node> folded-parser)
      (install-lazy-parser! <wat-sequence-node> (</> <wat-flat-instruction> <wat-folded-node>))
      flat-parser))

  #|proc:<wat-folded-instruction>
  The `<wat-folded-instruction>` parser reads one folded Core 3.0 WAT instruction and returns its
  operands followed by its containing instruction as a list. Each syntax node retains only its
  direct nested operands, avoiding copies of descendant prefixes.
  |#
  (define <wat-folded-instruction>
    (<map> (lambda (instruction) (instruction-list (list instruction))) folded-parser))

  #|proc:<wat-expression>
  The `<wat-expression>` parser reads flat and folded Core 3.0 WAT instructions and returns one
  immutable vector in execution order. Each syntax node retains its direct nested operands.
  |#
  (define <wat-expression>
    (<map> instruction-vector (<many> <wat-sequence-node>)))

  )
