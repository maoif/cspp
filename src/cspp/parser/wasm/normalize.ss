(library (cspp parser wasm normalize)
  (export normalize-wat-module)
  (import (cspp chez)
          (only (cspp list) make-list-builder)
          (only (cspp vector) vfold-left)
          (cspp parser wasm opcodes)
          (cspp parser wasm types)
          (cspp parser wasm validate)
          (cspp parser wasm text types)
          (cspp parser wasm text instructions)
          (cspp parser wasm private)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; Normalization state and namespace construction
;;;;===----------------------------------------------------------------------===

  (define-record-type normalization-state
    (fields fields namespaces field-indices field-namespaces
            type-groups (mutable type-catalog) (mutable type-count)
            function-types resolved-type-uses deferred-type-uses))

  (define vector-map*
    (lambda (procedure vector)
      (let ([result (make-vector (vector-length vector))])
        (let loop ([index 0])
          (if (fx= index (vector-length vector))
              (vector->immutable-vector result)
              (begin
                (vector-set! result index (procedure (vector-ref vector index)))
                (loop (fx1+ index))))))))

  (define identifier-key
    (lambda (identifier)
      (and identifier (string->symbol identifier))))

  (define normalization-error
    (lambda (offset message)
      (raise (make-wasm-issue offset message))))

  (define field-entity-kind
    (lambda (field)
      (if (eq? 'import (wat-module-field-kind field))
          (vector-ref (wat-module-field-data field) 2)
          (wat-module-field-kind field))))

  (define imported-field?
    (lambda (field)
      (or (eq? 'import (wat-module-field-kind field))
          (wat-module-field-import field))))

  (define field-in-namespace?
    (lambda (field kind imported?)
      (and (eq? kind (field-entity-kind field))
           (eq? imported? (and (imported-field? field) #t)))))

  (define add-namespace-id!
    (lambda (table identifier index offset kind)
      (when identifier
        (let ([key (identifier-key identifier)])
          (if (hashtable-contains? table key)
              (normalization-error
               offset (format "duplicate WebAssembly ~a identifier: ~a" kind identifier))
              (hashtable-set! table key index))))))

  (define build-entity-namespace!
    (lambda (field* kind field-indices)
      (let ([table (make-eq-hashtable)] [next 0])
        (for-each
         (lambda (imported?)
           (vector-for-each
            (lambda (field)
              (when (field-in-namespace? field kind imported?)
                (add-namespace-id! table (wat-module-field-id field) next
                                   (wat-module-field-pos field) kind)
                (hashtable-set! field-indices field next)
                (set! next (fx1+ next))))
            field*))
         '(#t #f))
        table)))

  (define build-segment-namespace!
    (lambda (field* kind field-indices)
      (let ([table (make-eq-hashtable)] [next 0])
        (vector-for-each
         (lambda (field)
           (let* ([field-kind (wat-module-field-kind field)]
                  [abbreviation (wat-module-field-abbreviation field)]
                  [implicit?
                   (and abbreviation
                        (or (and (eq? kind 'element) (eq? field-kind 'table))
                            (and (eq? kind 'data) (eq? field-kind 'memory))))])
             (when (or (eq? kind field-kind) implicit?)
               (unless implicit?
                 (add-namespace-id! table (wat-module-field-id field) next
                                    (wat-module-field-pos field) kind))
               (unless implicit? (hashtable-set! field-indices field next))
               (set! next (fx1+ next)))))
         field*)
        table)))

  (define build-type-namespace!
    (lambda (field*)
      (let ([table (make-eq-hashtable)] [next 0])
        (vector-for-each
         (lambda (field)
           (when (eq? 'recursive-type (wat-module-field-kind field))
             (vector-for-each
              (lambda (subtype)
                (add-namespace-id! table (wat-subtype-syntax-id subtype) next
                                   (wat-subtype-syntax-pos subtype) 'type)
                (set! next (fx1+ next)))
              (wat-recursive-type-syntax-subtypes
               (wat-module-field-data field)))))
         field*)
        table)))

  (define check-import-order!
    (lambda (fields)
      (let loop ([index 0] [defined? #f])
        (unless (fx= index (vector-length fields))
          (let* ([field (vector-ref fields index)] [imported? (imported-field? field)])
            (when (and imported? defined?)
              (normalization-error (wat-module-field-pos field)
                                   "WebAssembly imports must precede entity definitions"))
            (loop (fx1+ index)
                  (or defined?
                      (and (not imported?)
                           (memq (field-entity-kind field)
                                 '(function table memory global tag))))))))))

  (define build-state
    (lambda (module)
      (let* ([field* (wat-module-fields module)]
             [namespaces (make-eq-hashtable)]
             [field-indices (make-eq-hashtable)]
             [field-namespaces (make-eq-hashtable)])
        (check-import-order! field*)
        (hashtable-set! namespaces 'type (build-type-namespace! field*))
        (for-each
         (lambda (kind)
           (hashtable-set!
            namespaces kind
            (if (memq kind '(element data))
                (build-segment-namespace! field* kind field-indices)
                (build-entity-namespace! field* kind field-indices))))
         '(function table memory global tag element data))
        (make-normalization-state
         field* namespaces field-indices field-namespaces
         (make-list-builder) (make-vector 16 #f) 0
         (make-hashtable function-type-hash equal?) (make-eq-hashtable) (make-list-builder)))))

  (define resolve-reference
    (lambda (state kind reference)
      (let ([value (wat-index-reference-value reference)])
        (if (natural? value)
            value
            (let* ([table (hashtable-ref (normalization-state-namespaces state)
                                         kind #f)]
                   [index (and table
                               (hashtable-ref table (identifier-key value) #f))])
              (if index
                  index
                  (normalization-error
                   (wat-index-reference-pos reference)
                   (format "unknown WebAssembly ~a identifier: ~a" kind value))))))))

;;;;===----------------------------------------------------------------------===
;;;; Type normalization and implicit function type generation
;;;;===----------------------------------------------------------------------===

  (define normalize-reference-type
    (lambda (state type)
      (if (wasm-reference-type? type)
          type
          (make-wasm-reference-type
           (wat-reference-type-syntax-nullable? type)
           (resolve-reference state 'type
                              (wat-reference-type-syntax-heap-type type))))))

  (define normalize-value-type
    (lambda (state type)
      (if (or (symbol? type) (wasm-reference-type? type))
          type
          (normalize-reference-type state type))))

  (define normalize-field-type
    (lambda (state field)
      (if (wasm-field-type? field)
          field
          (make-wasm-field-type
           (normalize-value-type state (wat-field-type-syntax-storage-type field))
           (wat-field-type-syntax-mutable? field)))))

  (define normalize-function-type
    (lambda (state type)
      (if (wasm-function-type? type)
          type
          (make-wasm-function-type
           (vector-map*
            (lambda (binding)
              (normalize-value-type state (wat-binding-type binding)))
            (wat-function-type-syntax-parameters type))
           (vector-map* (lambda (value) (normalize-value-type state value))
                        (wat-function-type-syntax-results type))))))

  (define normalize-composite-type
    (lambda (state type type-index)
      (cond
       [(or (wasm-function-type? type) (wat-function-type-syntax? type))
        (normalize-function-type state type)]
       [(or (wasm-struct-type? type) (wat-struct-type-syntax? type))
        (let* ([binding* (and (wat-struct-type-syntax? type)
                              (wat-struct-type-syntax-fields type))]
               [field* (if binding*
                           (vector-map*
                            (lambda (binding)
                              (normalize-field-type state (wat-binding-type binding)))
                            binding*)
                           (vector-map* (lambda (field)
                                          (normalize-field-type state field))
                                        (wasm-struct-type-fields type)))]
               [namespace (make-eq-hashtable)])
          (when binding*
            (let loop ([index 0])
              (unless (fx= index (vector-length binding*))
                (let ([binding (vector-ref binding* index)])
                  (add-namespace-id! namespace (wat-binding-id binding) index
                                     (wat-binding-pos binding) 'field)
                  (loop (fx1+ index))))))
          (hashtable-set! (normalization-state-field-namespaces state)
                          type-index namespace)
          (make-wasm-struct-type field*))]
       [(or (wasm-array-type? type) (wat-array-type-syntax? type))
        (make-wasm-array-type
         (normalize-field-type
          state (if (wasm-array-type? type)
                    (wasm-array-type-field type)
                    (wat-array-type-syntax-field type))))]
       [else
        (normalization-error 0 "invalid WebAssembly composite type syntax")])))

  (define normalize-explicit-types!
    (lambda (state)
      (let ([group-builder (normalization-state-type-groups state)] [type-index 0])
        (vector-for-each
         (lambda (field)
           (when (eq? 'recursive-type (wat-module-field-kind field))
             (let ([subtype-builder (make-list-builder)])
               (vector-for-each
                (lambda (syntax)
                  (let ([subtype
                         (make-wasm-subtype
                          (wat-subtype-syntax-final? syntax)
                          (vector-map*
                           (lambda (reference)
                             (resolve-reference state 'type reference))
                           (wat-subtype-syntax-supertypes syntax))
                          (normalize-composite-type
                           state (wat-subtype-syntax-composite-type syntax)
                           type-index))])
                    (subtype-builder subtype)
                    (append-type!
                     state subtype
                     (= 1 (vector-length
                           (wat-recursive-type-syntax-subtypes
                            (wat-module-field-data field)))))
                    (set! type-index (fx1+ type-index))))
                (wat-recursive-type-syntax-subtypes
                 (wat-module-field-data field)))
               (group-builder
                (make-wasm-recursive-type
                 (list->immutable-vector (subtype-builder)))))))
         (normalization-state-fields state)))))

  (define value-type=?
    (lambda (left right)
      (cond [(and (symbol? left) (symbol? right)) (eq? left right)]
            [(and (wasm-reference-type? left) (wasm-reference-type? right))
             (and (eq? (wasm-reference-type-nullable? left)
                       (wasm-reference-type-nullable? right))
                  (equal? (wasm-reference-type-heap-type left)
                          (wasm-reference-type-heap-type right)))]
            [else #f])))

  (define type-vector=?
    (lambda (left right)
      (and (= (vector-length left) (vector-length right))
           (let loop ([index 0])
             (or (fx= index (vector-length left))
                 (and (value-type=? (vector-ref left index)
                                    (vector-ref right index))
                      (loop (fx1+ index))))))))

  (define function-type=?
    (lambda (left right)
      (and (type-vector=? (wasm-function-type-parameters left)
                          (wasm-function-type-parameters right))
           (type-vector=? (wasm-function-type-results left)
                          (wasm-function-type-results right)))))

  (define function-type-at
    (lambda (state index offset)
      (let ([catalog (normalization-state-type-catalog state)])
        (if (< index (normalization-state-type-count state))
            (let ([composite (wasm-subtype-composite-type
                              (vector-ref catalog index))])
              (if (wasm-function-type? composite)
                  composite
                  (normalization-error
                   offset "WebAssembly type use does not name a function type")))
            (normalization-error offset "WebAssembly type index is out of range")))))

  (define known-function-type-at
    (lambda (state index)
      (let ([catalog (normalization-state-type-catalog state)])
        (and (< index (normalization-state-type-count state))
             (let ([composite
                    (wasm-subtype-composite-type (vector-ref catalog index))])
               (and (wasm-function-type? composite) composite))))))

  ;; Structural keys keep reference-type signatures comparable across distinct records.
  (define function-type-key
    (lambda (type)
      (define value-key
        (lambda (value)
          (if (wasm-reference-type? value)
              (cons (wasm-reference-type-nullable? value) (wasm-reference-type-heap-type value))
              value)))
      (cons (vector-map* value-key (wasm-function-type-parameters type))
            (vector-map* value-key (wasm-function-type-results type)))))

  ;; equal-hash bounds its traversal, so hash every component of these flat signature keys.
  (define function-type-hash
    (lambda (key)
      (define mix
        (lambda (hash value)
          (fxlogand (most-positive-fixnum)
                    (fx+/wraparound (fx*/wraparound hash 31) (equal-hash value)))))
      (define hash-vector
        (lambda (hash types)
          (vfold-left mix (mix hash (vector-length types)) types)))
      (hash-vector (hash-vector 0 (car key)) (cdr key))))

  (define append-type!
    (lambda (state subtype singular?)
      (let* ([index (normalization-state-type-count state)]
             [catalog (normalization-state-type-catalog state)]
             [type (wasm-subtype-composite-type subtype)])
        (when (fx= index (vector-length catalog))
          (let ([grown (make-vector (fx* 2 (vector-length catalog)) #f)])
            (vector-copy! catalog 0 grown 0 index)
            (normalization-state-type-catalog-set! state grown)
            (set! catalog grown)))
        (vector-set! catalog index subtype)
        (normalization-state-type-count-set! state (fx1+ index))
        ;; Type-use abbreviations match only singular final function types without supertypes.
        (when (and singular? (wasm-subtype-final? subtype)
                   (zero? (vector-length (wasm-subtype-supertypes subtype)))
                   (wasm-function-type? type))
          (let ([key (function-type-key type)]
                [signatures (normalization-state-function-types state)])
            ;; Preserve the earliest explicit or implicit match, including index zero.
            (unless (hashtable-contains? signatures key)
              (hashtable-set! signatures key index))))
        index)))

  (define append-implicit-function-type!
    (lambda (state type)
      (let* ([subtype (make-wasm-subtype #t '#() type)]
             [index (append-type! state subtype #t)])
        ((normalization-state-type-groups state)
         (make-wasm-recursive-type (vector subtype)))
        index)))

  (define find-function-type
    (lambda (state type)
      (hashtable-ref (normalization-state-function-types state) (function-type-key type) #f)))

  (define type-use-signature
    (lambda (state type-use)
      (make-wasm-function-type
       (vector-map*
        (lambda (binding) (normalize-value-type state (wat-binding-type binding)))
        (wat-type-use-parameters type-use))
       (vector-map* (lambda (type) (normalize-value-type state type))
                    (wat-type-use-results type-use)))))

  (define resolve-type-use
    (lambda (state type-use)
      (let ([resolved (normalization-state-resolved-type-uses state)])
        (or (hashtable-ref resolved type-use #f)
            (let* ([signature (type-use-signature state type-use)]
                   [reference (wat-type-use-index type-use)]
                   [offset (wat-type-use-pos type-use)]
                   [inline?
                    (or (positive? (vector-length (wasm-function-type-parameters signature)))
                        (positive? (vector-length (wasm-function-type-results signature))))]
                   [index
                    (if reference
                        (let ([index (resolve-reference state 'type reference)])
                          (if inline?
                              ;; Numeric references may name types generated by later fields or bodies.
                              ((normalization-state-deferred-type-uses state)
                               (list index signature offset))
                              (unless (natural? (wat-index-reference-value reference))
                                (function-type-at state index offset)))
                          index)
                        (or (find-function-type state signature)
                            (append-implicit-function-type! state signature)))])
              (hashtable-set! resolved type-use index)
              index)))))

  (define check-deferred-type-uses!
    (lambda (state)
      (for-each
       (lambda (use)
         (unless (function-type=? (function-type-at state (car use) (caddr use)) (cadr use))
           (normalization-error
            (caddr use) "inline WebAssembly signature conflicts with its type use")))
       ((normalization-state-deferred-type-uses state)))))

  (define intern-implicit-types!
    (lambda (state)
      (let ([uses (make-list-builder)])
        (define visit
          (lambda (syntax)
            (cond
              [(wat-type-use? syntax) (uses syntax)]
              [(wat-block-type-syntax? syntax)
               (let ([use (wat-block-type-syntax-type-use syntax)])
                 (when (or (wat-type-use-index use)
                           (positive? (vector-length (wat-type-use-parameters use)))
                           (> (vector-length (wat-type-use-results use)) 1))
                   (uses use)))]
              [(wat-instruction-syntax? syntax)
               (visit (wat-instruction-syntax-immediates syntax))
               (visit (wat-instruction-syntax-body syntax))
               (visit (wat-instruction-syntax-alternate syntax))]
              [(wat-inline-abbreviation? syntax)
               (visit (wat-inline-abbreviation-data syntax))]
              [(wat-element-segment-syntax? syntax)
               (visit (wat-element-segment-syntax-offset syntax))
               (visit (wat-element-segment-syntax-items syntax))]
              [(wat-data-segment-syntax? syntax)
               (visit (wat-data-segment-syntax-offset syntax))]
              [(vector? syntax) (vector-for-each visit syntax)])))
        (vector-for-each
         (lambda (field)
           (visit (wat-module-field-data field))
           (visit (wat-module-field-abbreviation field)))
         (normalization-state-fields state))
        ;; Execution vectors contain each operand once; do not revisit retained operand trees.
        ;; Sort source offsets because folded headers precede their operands in source order.
        (for-each
         (lambda (use) (resolve-type-use state use))
         (list-sort (lambda (left right) (< (wat-type-use-pos left) (wat-type-use-pos right)))
                    (uses))))))

  (define normalize-block-type
    (lambda (state syntax)
      (let* ([type-use (wat-block-type-syntax-type-use syntax)]
             [reference (wat-type-use-index type-use)]
             [parameter* (wat-type-use-parameters type-use)]
             [result* (wat-type-use-results type-use)])
        (cond [reference
               (make-wasm-block-type 'type-index
                                     (resolve-type-use state type-use))]
              [(and (zero? (vector-length parameter*))
                    (zero? (vector-length result*)))
               (make-wasm-block-type 'empty #f)]
              [(and (zero? (vector-length parameter*))
                    (= 1 (vector-length result*)))
               (make-wasm-block-type
                'value-type (normalize-value-type state (vector-ref result* 0)))]
              [else
               (make-wasm-block-type 'type-index
                                     (resolve-type-use state type-use))]))))

;;;;===----------------------------------------------------------------------===
;;;; Instruction and expression normalization
;;;;===----------------------------------------------------------------------===

  (define resolve-label
    (lambda (reference labels depth)
      (let ([value (wat-index-reference-value reference)])
        (if (natural? value)
            value
            (let ([binding-depth (hashtable-ref labels (identifier-key value) #f)])
              (if binding-depth
                  (fx- depth binding-depth)
                  (normalization-error
                   (wat-index-reference-pos reference)
                   (format "unknown WebAssembly label identifier: ~a" value))))))))

  (define resolve-local
    (lambda (reference locals)
      (let ([value (wat-index-reference-value reference)])
        (if (natural? value)
            value
            (let ([index (hashtable-ref locals (identifier-key value) #f)])
              (if index
                  index
                  (normalization-error
                   (wat-index-reference-pos reference)
                   (format "unknown WebAssembly local identifier: ~a" value))))))))

  (define resolve-field
    (lambda (state type-index reference)
      (let ([value (wat-index-reference-value reference)])
        (if (natural? value)
            value
            (let* ([namespace
                    (hashtable-ref (normalization-state-field-namespaces state)
                                   type-index #f)]
                   [index (and namespace
                               (hashtable-ref namespace (identifier-key value) #f))])
              (if index
                  index
                  (normalization-error
                   (wat-index-reference-pos reference)
                   (format "unknown WebAssembly field identifier: ~a" value))))))))

  (define normalize-memory-argument
    (lambda (state syntax)
      (make-wasm-memory-argument
       (vector-ref syntax 0) (vector-ref syntax 1)
       (resolve-reference state 'memory (vector-ref syntax 2)))))

  (define normalize-index-pair
    (lambda (state first-kind second-kind immediate*)
      (vector (resolve-reference state first-kind (vector-ref immediate* 0))
              (resolve-reference state second-kind (vector-ref immediate* 1)))))

  (define normalize-immediates
    (lambda (state instruction locals labels depth)
      (let* ([mnemonic (wat-instruction-syntax-mnemonic instruction)]
             [descriptor (wasm-opcode-by-mnemonic mnemonic)]
             [shape (wasm-opcode-immediate-shape descriptor)]
             [immediate* (wat-instruction-syntax-immediates instruction)])
        (case shape
          [(none f32 f64 vector-bytes shuffle-bytes lane-index) immediate*]
          [(i32 i64)
           ;; WAT integer literals are unsigned bit patterns; binary constants are signed.
           (let* ([width (if (eq? shape 'i32) 32 64)]
                  [value (vector-ref immediate* 0)])
             (vector (if (logbit? (fx1- width) value)
                         (- value (ash 1 width))
                         value)))]
          [(block-type try-table)
           (vector (normalize-block-type state (vector-ref immediate* 0)))]
          [(label-index)
           (vector (resolve-label (vector-ref immediate* 0) labels depth))]
          [(function-index)
           (vector (resolve-reference state 'function (vector-ref immediate* 0)))]
          [(type-index)
           (vector (resolve-reference state 'type (vector-ref immediate* 0)))]
          [(table-index)
           (vector (resolve-reference state 'table (vector-ref immediate* 0)))]
          [(memory-index)
           (vector (resolve-reference state 'memory (vector-ref immediate* 0)))]
          [(global-index)
           (vector (resolve-reference state 'global (vector-ref immediate* 0)))]
          [(local-index)
           (vector (resolve-local (vector-ref immediate* 0) locals))]
          [(tag-index)
           (vector (resolve-reference state 'tag (vector-ref immediate* 0)))]
          [(data-index)
           (vector (resolve-reference state 'data (vector-ref immediate* 0)))]
          [(element-index)
           (vector (resolve-reference state 'element (vector-ref immediate* 0)))]
          [(heap-type)
           (let ([type (vector-ref immediate* 0)])
             (vector (if (wat-index-reference? type)
                         (resolve-reference state 'type type)
                         type)))]
          [(heap-type-non-null heap-type-nullable reference-type)
           (vector (normalize-reference-type state (vector-ref immediate* 0)))]
          [(value-type-vector select-types)
           (if (zero? (vector-length immediate*))
               immediate*
               (vector
                (vector-map* (lambda (type) (normalize-value-type state type))
                             (vector-ref immediate* 0))))]
          [(call-indirect)
           (vector (resolve-type-use state (vector-ref immediate* 0))
                   (resolve-reference state 'table (vector-ref immediate* 1)))]
          [(memory-argument)
           (vector (normalize-memory-argument state (vector-ref immediate* 0)))]
          [(memory-argument-lane)
           (vector (normalize-memory-argument state (vector-ref immediate* 0))
                   (vector-ref immediate* 1))]
          [(label-vector)
           (let ([target* (vector-ref immediate* 0)])
             (vector (vector-map* (lambda (target) (resolve-label target labels depth)) target*)
                     (resolve-label (vector-ref immediate* 1) labels depth)))]
          [(table-pair)
           (normalize-index-pair state 'table 'table immediate*)]
          [(memory-pair)
           (normalize-index-pair state 'memory 'memory immediate*)]
          [(array-copy)
           (normalize-index-pair state 'type 'type immediate*)]
          [(type-data)
           (normalize-index-pair state 'type 'data immediate*)]
          [(type-element)
           (normalize-index-pair state 'type 'element immediate*)]
          [(memory-data)
           (normalize-index-pair state 'memory 'data immediate*)]
          [(table-element)
           (normalize-index-pair state 'table 'element immediate*)]
          [(struct-field)
           (let ([type-index
                  (resolve-reference state 'type (vector-ref immediate* 0))])
             (vector type-index
                     (resolve-field state type-index (vector-ref immediate* 1))))]
          [(array-new-fixed)
           (vector (resolve-reference state 'type (vector-ref immediate* 0))
                   (vector-ref immediate* 1))]
          [(br-on-cast)
           (vector (resolve-label (vector-ref immediate* 0) labels depth)
                   (normalize-reference-type state (vector-ref immediate* 1))
                   (normalize-reference-type state (vector-ref immediate* 2)))]
          [else
           (normalization-error
            (wat-instruction-syntax-pos instruction)
            (format "unsupported WebAssembly normalization shape: ~a" shape))]))))

  (define normalize-expression
    (lambda (state instruction* locals)
      ;; Each expression owns its table. Bind absolute depths while traversing a body,
      ;; then restore the shadowed binding before processing siblings or catch targets.
      (let ([labels (make-eq-hashtable)])
        (let normalize ([instruction* instruction*] [depth 0])
          (vector-map*
           (lambda (instruction)
             (let* ([label (identifier-key (wat-instruction-syntax-label instruction))]
                    [previous (and label (hashtable-ref labels label #f))]
                    [body
                     (begin
                       (when label (hashtable-set! labels label (fx1+ depth)))
                       (normalize (wat-instruction-syntax-body instruction) (fx1+ depth)))]
                    [alternate-syntax (wat-instruction-syntax-alternate instruction)]
                    [catches? (and alternate-syntax
                                   (positive? (vector-length alternate-syntax))
                                   (wat-catch-syntax? (vector-ref alternate-syntax 0)))]
                    [alternate
                     (if (or (not alternate-syntax) catches?)
                         '#()
                         (normalize alternate-syntax (fx1+ depth)))])
               (when label
                 (if previous
                     (hashtable-set! labels label previous)
                     (hashtable-delete! labels label)))
               (make-wasm-instruction
                (wat-instruction-syntax-mnemonic instruction)
                (normalize-immediates state instruction locals labels depth)
                body
                (if catches?
                    ;; try_table catches target the enclosing scope, not its body label.
                    (vector-map*
                     (lambda (catch)
                       (make-wasm-catch
                        (wat-catch-syntax-kind catch)
                        (and (wat-catch-syntax-tag catch)
                             (resolve-reference state 'tag (wat-catch-syntax-tag catch)))
                        (resolve-label (wat-catch-syntax-label catch) labels depth)))
                     alternate-syntax)
                    alternate))))
           instruction*)))))

;;;;===----------------------------------------------------------------------===
;;;; Entity and abbreviation normalization
;;;;===----------------------------------------------------------------------===

  (define add-binding-ids!
    (lambda (namespace binding* start kind)
      (let loop ([index 0])
        (unless (fx= index (vector-length binding*))
          (let ([binding (vector-ref binding* index)])
            (add-namespace-id! namespace (wat-binding-id binding) (+ start index)
                               (wat-binding-pos binding) kind)
            (loop (fx1+ index)))))))

  (define function-locals
    (lambda (state type-use local*)
      (vector-map* (lambda (binding)
                     (normalize-value-type state (wat-binding-type binding)))
                   local*)))

  (define normalize-function
    (lambda (state field)
      (let* ([data (wat-module-field-data field)]
             [type-use (vector-ref data 0)]
             [type-index (resolve-type-use state type-use)]
             [parameter* (wat-type-use-parameters type-use)]
             [parameter-count
              (let ([type (known-function-type-at state type-index)])
                (if type
                    (vector-length (wasm-function-type-parameters type))
                    (vector-length parameter*)))]
             [local* (vector-ref data 1)]
             [locals (make-eq-hashtable)])
        (add-binding-ids! locals parameter* 0 'local)
        (add-binding-ids! locals local* parameter-count 'local)
        (make-wasm-function
         type-index (function-locals state type-use local*)
         (normalize-expression state (vector-ref data 2) locals)))))

  (define normalize-table-type
    (lambda (state type)
      (if (wasm-table-type? type)
          type
          (make-wasm-table-type
           (normalize-reference-type state
                                     (wat-table-type-syntax-reference-type type))
           (wat-table-type-syntax-limits type)))))

  (define normalize-memory-type
    (lambda (type)
      (if (wasm-memory-type? type)
          type
          (let ([limits (wat-memory-type-syntax-limits type)])
            (make-wasm-memory-type
             (make-wasm-limits
              (wat-limits-syntax-address-type limits)
              (wat-limits-syntax-minimum limits)
              (wat-limits-syntax-maximum limits)
              (wat-limits-syntax-shared? limits)))))))

  (define normalize-global-type
    (lambda (state type)
      (if (wasm-global-type? type)
          type
          (make-wasm-global-type
           (normalize-value-type state (wat-global-type-syntax-value-type type))
           (wat-global-type-syntax-mutable? type)))))

  (define zero-expression
    (lambda (address-type)
      (vector (make-wasm-instruction
               (if (eq? address-type 'i64) 'i64.const 'i32.const)
               (vector 0) '#() '#()))))

  (define ref-function-expression
    (lambda (index)
      (vector (make-wasm-instruction 'ref.func (vector index) '#() '#()))))

  (define normalize-element
    (lambda (state syntax table-index override-mode)
      (let* ([mode (or override-mode (wat-element-segment-syntax-mode syntax))]
             [reference-type
              (normalize-reference-type
               state (wat-element-segment-syntax-reference-type syntax))]
             [table
              (and (eq? mode 'active)
                   (or table-index
                       (and (wat-element-segment-syntax-table syntax)
                            (resolve-reference
                             state 'table (wat-element-segment-syntax-table syntax)))
                       0))]
             [offset
              (and (eq? mode 'active)
                   (if (wat-element-segment-syntax-offset syntax)
                       (normalize-expression
                        state (wat-element-segment-syntax-offset syntax)
                        (make-eq-hashtable))
                       (zero-expression
                        (or (wat-element-segment-syntax-address-type syntax) 'i32))))]
             [items (wat-element-segment-syntax-items syntax)]
             [initializers
              (if (eq? 'indexes (wat-element-segment-syntax-item-kind syntax))
                  (vector-map*
                   (lambda (reference)
                     (ref-function-expression
                      (resolve-reference state 'function reference)))
                  items)
                  (vector-map*
                   (lambda (expression)
                     (normalize-expression state expression (make-eq-hashtable)))
                   items))])
        (make-wasm-element mode reference-type table offset initializers))))

  (define normalize-data
    (lambda (state syntax memory-index override-mode)
      (let* ([mode (or override-mode (wat-data-segment-syntax-mode syntax))]
             [memory
              (and (eq? mode 'active)
                   (or memory-index
                       (and (wat-data-segment-syntax-memory syntax)
                            (resolve-reference
                             state 'memory (wat-data-segment-syntax-memory syntax)))
                       0))]
             [offset
              (and (eq? mode 'active)
                   (if (wat-data-segment-syntax-offset syntax)
                       (normalize-expression
                        state (wat-data-segment-syntax-offset syntax)
                        (make-eq-hashtable))
                       (zero-expression
                        (or (wat-data-segment-syntax-address-type syntax) 'i32))))])
        (make-wasm-data
         mode memory offset
         (concatenate-bytevectors
          (vector->list (wat-data-segment-syntax-strings syntax)))))))

  (define normalize-inline-table
    (lambda (state field abbreviation)
      (let* ([data (wat-inline-abbreviation-data abbreviation)]
             [syntax (vector-ref data 1)]
             [count (vector-length (wat-element-segment-syntax-items syntax))]
             [address-type
              (or (wat-element-segment-syntax-address-type syntax) 'i32)]
             [type (make-wasm-table-type
                    (normalize-reference-type state (vector-ref data 0))
                    (make-wasm-limits address-type count count))]
             [index (hashtable-ref
                     (normalization-state-field-indices state) field #f)])
        (values (make-wasm-table type #f)
                (normalize-element state syntax index 'active)))))

  (define normalize-inline-memory
    (lambda (state field abbreviation)
      (let* ([syntax (wat-inline-abbreviation-data abbreviation)]
             [bytes (concatenate-bytevectors
                     (vector->list (wat-data-segment-syntax-strings syntax)))]
             [pages (quotient (+ (bytevector-length bytes) 65535) 65536)]
             [address-type (or (wat-data-segment-syntax-address-type syntax) 'i32)]
             [index (hashtable-ref
                     (normalization-state-field-indices state) field #f)])
        (values
         (make-wasm-memory
          (make-wasm-memory-type
           (make-wasm-limits address-type pages pages)))
         (make-wasm-data 'active index (zero-expression address-type) bytes)))))

  (define normalize-import
    (lambda (state field)
      (let* ([explicit? (eq? 'import (wat-module-field-kind field))]
             [data (wat-module-field-data field)]
             [import (if explicit? data (wat-module-field-import field))]
             [kind (if explicit? (vector-ref data 2)
                       (wat-module-field-kind field))]
             [type (if explicit? (vector-ref data 3) data)]
             [external-type
              (case kind
                [(function)
                 (let ([type-use (if explicit? type (vector-ref type 0))])
                   (make-wasm-external-type
                    kind (resolve-type-use state type-use)))]
                [(table)
                 (make-wasm-external-type kind (normalize-table-type state type))]
                [(memory)
                 (make-wasm-external-type kind (normalize-memory-type type))]
                [(global)
                 (let ([global-type (if explicit? type (vector-ref type 0))])
                   (make-wasm-external-type
                    kind (normalize-global-type state global-type)))]
                [(tag)
                 (make-wasm-external-type
                  kind (make-wasm-tag-type (resolve-type-use state type)))])])
        (make-wasm-import (vector-ref import 0) (vector-ref import 1) external-type))))

  (define export-kind
    (lambda (kind)
      (if (eq? kind 'func) 'function kind)))

  (define append-inline-exports!
    (lambda (field builder state)
      (let ([kind (field-entity-kind field)]
            [index (hashtable-ref
                    (normalization-state-field-indices state) field #f)])
        (vector-for-each
         (lambda (name) (builder (make-wasm-export name kind index)))
         (wat-module-field-exports field)))))

  (define preceding-section
    (lambda (section)
      (case section
        [(type) #f] [(import) 'type] [(function) 'import] [(table) 'function]
        [(memory) 'table] [(tag) 'memory] [(global) 'tag] [(export) 'global]
        [(start) 'export] [(element) 'start] [(data-count) 'element]
        [(code) 'data-count] [(data) 'code]
        [else #f])))

  (define normalize-custom-section
    (lambda (field)
      (let* ([data (wat-module-field-data field)]
             [placement (vector-ref data 2)]
             [after (or (wat-custom-placement-after placement)
                        (and (wat-custom-placement-before placement)
                             (preceding-section
                              (wat-custom-placement-before placement))))])
        (make-wasm-custom-section
         (vector-ref data 0)
         (concatenate-bytevectors (vector->list (vector-ref data 1)))
         after))))

;;;;===----------------------------------------------------------------------===
;;;; Module assembly
;;;;===----------------------------------------------------------------------===

  (define assemble-module
    (lambda (state)
      (let ([import-builder (make-list-builder)] [function-builder (make-list-builder)]
            [table-builder (make-list-builder)] [memory-builder (make-list-builder)]
            [global-builder (make-list-builder)] [tag-builder (make-list-builder)]
            [export-builder (make-list-builder)] [element-builder (make-list-builder)]
            [data-builder (make-list-builder)] [custom-builder (make-list-builder)]
            [start #f] [start-pos #f])
        (vector-for-each
         (lambda (field)
           (when (imported-field? field)
             (import-builder (normalize-import state field))))
         (normalization-state-fields state))
        (vector-for-each
         (lambda (field)
           (let ([kind (wat-module-field-kind field)])
             (unless (or (eq? kind 'recursive-type) (imported-field? field))
               (case kind
                 [(function) (function-builder (normalize-function state field))]
                 [(table)
                  (if (wat-module-field-abbreviation field)
                      (let-values ([(table element)
                                    (normalize-inline-table
                                     state field (wat-module-field-abbreviation field))])
                        (table-builder table) (element-builder element))
                      (let* ([data (wat-module-field-data field)]
                             [initialized? (vector? data)])
                        (table-builder
                         (make-wasm-table
                          (normalize-table-type state (if initialized? (vector-ref data 0) data))
                          (and initialized?
                               (normalize-expression state (vector-ref data 1)
                                                     (make-eq-hashtable)))))))]
                 [(memory)
                  (if (wat-module-field-abbreviation field)
                      (let-values ([(memory data)
                                    (normalize-inline-memory
                                     state field (wat-module-field-abbreviation field))])
                        (memory-builder memory) (data-builder data))
                      (memory-builder
                       (make-wasm-memory
                        (normalize-memory-type (wat-module-field-data field)))))]
                 [(global)
                  (let ([data (wat-module-field-data field)])
                    (global-builder
                     (make-wasm-global
                      (normalize-global-type state (vector-ref data 0))
                      (normalize-expression state (vector-ref data 1)
                                            (make-eq-hashtable)))))]
                 [(tag)
                  (tag-builder
                   (make-wasm-tag
                    (make-wasm-tag-type
                     (resolve-type-use state (wat-module-field-data field)))))]
                 [(export)
                  (let* ([data (wat-module-field-data field)]
                         [description (vector-ref data 1)]
                         [exported-kind (export-kind (car description))])
                    (export-builder
                     (make-wasm-export
                      (vector-ref data 0) exported-kind
                      (resolve-reference state exported-kind (cdr description)))))]
                 [(start)
                  (if start-pos
                      (normalization-error (wat-module-field-pos field)
                                           "duplicate WebAssembly start declaration")
                      (begin
                        (set! start-pos (wat-module-field-pos field))
                        (set! start
                              (resolve-reference state 'function
                                                 (wat-module-field-data field)))))]
                 [(element)
                  (element-builder
                   (normalize-element
                    state
                    (wat-inline-abbreviation-data
                     (wat-module-field-abbreviation field))
                    #f #f))]
                 [(data)
                  (data-builder
                   (normalize-data
                    state
                    (wat-inline-abbreviation-data
                     (wat-module-field-abbreviation field))
                    #f #f))]
                 [(custom) (custom-builder (normalize-custom-section field))]))
             (when (and (memq (field-entity-kind field)
                              '(function table memory global tag))
                        (positive? (vector-length
                                    (wat-module-field-exports field))))
               (append-inline-exports! field export-builder state))))
         (normalization-state-fields state))
        (make-wasm-module
         (list->immutable-vector ((normalization-state-type-groups state)))
         (list->immutable-vector (import-builder))
         (list->immutable-vector (function-builder))
         (list->immutable-vector (table-builder))
         (list->immutable-vector (memory-builder))
         (list->immutable-vector (global-builder))
         (list->immutable-vector (tag-builder))
         (list->immutable-vector (export-builder)) start
         (list->immutable-vector (element-builder))
         (list->immutable-vector (data-builder))
         (list->immutable-vector (custom-builder))))))

  #|proc:normalize-wat-module
  Normalizes positioned WAT syntax `module` into a canonical module. Returns a `wasm-module` on
  success or a positioned `wasm-issue` when identifier resolution or expansion fails.
  |#
  (define normalize-wat-module
    (lambda (module)
      (pcheck ([wat-module? module])
              (guard (issue [(wasm-issue? issue) issue])
                (let ([state (build-state module)])
                  (normalize-explicit-types! state)
                  (intern-implicit-types! state)
                  (let ([module (assemble-module state)])
                    (check-deferred-type-uses! state)
                    module))))))

  )
