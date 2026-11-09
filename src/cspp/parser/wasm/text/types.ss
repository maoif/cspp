(library (cspp parser wasm text types)
  (export
    make-wat-index-reference wat-index-reference? wat-index-reference-pos
    wat-index-reference-value
    make-wat-binding wat-binding? wat-binding-pos wat-binding-id wat-binding-type
    make-wat-reference-type-syntax wat-reference-type-syntax?
    wat-reference-type-syntax-pos wat-reference-type-syntax-nullable?
    wat-reference-type-syntax-heap-type
    make-wat-field-type-syntax wat-field-type-syntax? wat-field-type-syntax-pos
    wat-field-type-syntax-storage-type wat-field-type-syntax-mutable?
    make-wat-function-type-syntax wat-function-type-syntax?
    wat-function-type-syntax-pos wat-function-type-syntax-parameters
    wat-function-type-syntax-results
    make-wat-struct-type-syntax wat-struct-type-syntax? wat-struct-type-syntax-pos
    wat-struct-type-syntax-fields
    make-wat-array-type-syntax wat-array-type-syntax? wat-array-type-syntax-pos
    wat-array-type-syntax-field
    make-wat-subtype-syntax wat-subtype-syntax? wat-subtype-syntax-pos
    wat-subtype-syntax-id wat-subtype-syntax-final? wat-subtype-syntax-supertypes
    wat-subtype-syntax-composite-type
    make-wat-recursive-type-syntax wat-recursive-type-syntax?
    wat-recursive-type-syntax-pos wat-recursive-type-syntax-subtypes
    make-wat-type-use wat-type-use? wat-type-use-pos wat-type-use-index
    wat-type-use-parameters wat-type-use-results
    make-wat-block-type-syntax wat-block-type-syntax? wat-block-type-syntax-pos
    wat-block-type-syntax-type-use
    make-wat-global-type-syntax wat-global-type-syntax? wat-global-type-syntax-pos
    wat-global-type-syntax-value-type wat-global-type-syntax-mutable?
    make-wat-table-type-syntax wat-table-type-syntax? wat-table-type-syntax-pos
    wat-table-type-syntax-limits wat-table-type-syntax-reference-type
    make-wat-limits-syntax wat-limits-syntax? wat-limits-syntax-pos
    wat-limits-syntax-address-type wat-limits-syntax-minimum
    wat-limits-syntax-maximum wat-limits-syntax-shared?
    make-wat-memory-type-syntax wat-memory-type-syntax? wat-memory-type-syntax-pos
    wat-memory-type-syntax-limits
    make-wat-element-segment-syntax wat-element-segment-syntax?
    wat-element-segment-syntax-pos wat-element-segment-syntax-mode
    wat-element-segment-syntax-table wat-element-segment-syntax-offset
    wat-element-segment-syntax-address-type
    wat-element-segment-syntax-reference-type wat-element-segment-syntax-item-kind
    wat-element-segment-syntax-items
    make-wat-data-segment-syntax wat-data-segment-syntax?
    wat-data-segment-syntax-pos wat-data-segment-syntax-mode
    wat-data-segment-syntax-memory wat-data-segment-syntax-offset
    wat-data-segment-syntax-address-type
    wat-data-segment-syntax-strings
    make-wat-custom-placement wat-custom-placement? wat-custom-placement-pos
    wat-custom-placement-before wat-custom-placement-after
    make-wat-module wat-module? wat-module-pos wat-module-id wat-module-fields
    make-wat-module-field wat-module-field? wat-module-field-pos wat-module-field-kind
    wat-module-field-id wat-module-field-data wat-module-field-import
    wat-module-field-exports wat-module-field-abbreviation
    make-wat-instruction-syntax wat-instruction-syntax? wat-instruction-syntax-pos
    wat-instruction-syntax-mnemonic wat-instruction-syntax-immediates
    wat-instruction-syntax-body wat-instruction-syntax-alternate
    wat-instruction-syntax-operands wat-instruction-syntax-label
    make-wat-inline-abbreviation wat-inline-abbreviation?
    wat-inline-abbreviation-pos wat-inline-abbreviation-kind
    wat-inline-abbreviation-data
    <wat-index-reference> <wat-heap-type> <wat-reference-type> <wat-value-type>
    <wat-storage-type> <wat-field-type> <wat-function-type> <wat-struct-type>
    <wat-array-type> <wat-composite-type> <wat-subtype> <wat-type-definition>
    <wat-recursive-type>
    <wat-type-use> <wat-anonymous-type-use> <wat-block-type> <wat-limits> <wat-global-type>
    <wat-table-type> <wat-memory-type> <wat-tag-type>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm types)
          (cspp parser wasm private)
          (cspp parser wasm text lexical))

;;;;===----------------------------------------------------------------------===
;;;; Positioned intermediate syntax
;;;;===----------------------------------------------------------------------===

  (define-record-type ($wat-index-reference make-wat-index-reference wat-index-reference?)
    (fields (immutable pos wat-index-reference-pos)
            (immutable value wat-index-reference-value)))

  (define-record-type ($wat-binding make-wat-binding wat-binding?)
    (fields (immutable pos wat-binding-pos)
            (immutable id wat-binding-id)
            (immutable type wat-binding-type)))

  (define-record-type
    ($wat-reference-type-syntax make-wat-reference-type-syntax
                                wat-reference-type-syntax?)
    (fields (immutable pos wat-reference-type-syntax-pos)
            (immutable nullable? wat-reference-type-syntax-nullable?)
            (immutable heap-type wat-reference-type-syntax-heap-type)))

  (define-record-type ($wat-field-type-syntax make-wat-field-type-syntax
                                              wat-field-type-syntax?)
    (fields (immutable pos wat-field-type-syntax-pos)
            (immutable storage-type wat-field-type-syntax-storage-type)
            (immutable mutable? wat-field-type-syntax-mutable?)))

  (define-record-type ($wat-function-type-syntax make-wat-function-type-syntax
                                                 wat-function-type-syntax?)
    (fields (immutable pos wat-function-type-syntax-pos)
            (immutable parameters wat-function-type-syntax-parameters)
            (immutable results wat-function-type-syntax-results)))

  (define-record-type ($wat-struct-type-syntax make-wat-struct-type-syntax
                                               wat-struct-type-syntax?)
    (fields (immutable pos wat-struct-type-syntax-pos)
            (immutable fields wat-struct-type-syntax-fields)))

  (define-record-type ($wat-array-type-syntax make-wat-array-type-syntax
                                              wat-array-type-syntax?)
    (fields (immutable pos wat-array-type-syntax-pos)
            (immutable field wat-array-type-syntax-field)))

  (define-record-type ($wat-subtype-syntax make-wat-subtype-syntax wat-subtype-syntax?)
    (fields (immutable pos wat-subtype-syntax-pos)
            (immutable id wat-subtype-syntax-id)
            (immutable final? wat-subtype-syntax-final?)
            (immutable supertypes wat-subtype-syntax-supertypes)
            (immutable composite-type wat-subtype-syntax-composite-type)))

  (define-record-type
    ($wat-recursive-type-syntax make-wat-recursive-type-syntax
                                wat-recursive-type-syntax?)
    (fields (immutable pos wat-recursive-type-syntax-pos)
            (immutable subtypes wat-recursive-type-syntax-subtypes)))

  (define-record-type ($wat-type-use make-wat-type-use wat-type-use?)
    (fields (immutable pos wat-type-use-pos)
            (immutable index wat-type-use-index)
            (immutable parameters wat-type-use-parameters)
            (immutable results wat-type-use-results)))

  (define-record-type ($wat-block-type-syntax make-wat-block-type-syntax
                                              wat-block-type-syntax?)
    (fields (immutable pos wat-block-type-syntax-pos)
            (immutable type-use wat-block-type-syntax-type-use)))

  (define-record-type ($wat-global-type-syntax make-wat-global-type-syntax
                                               wat-global-type-syntax?)
    (fields (immutable pos wat-global-type-syntax-pos)
            (immutable value-type wat-global-type-syntax-value-type)
            (immutable mutable? wat-global-type-syntax-mutable?)))

  (define-record-type ($wat-table-type-syntax make-wat-table-type-syntax
                                              wat-table-type-syntax?)
    (fields (immutable pos wat-table-type-syntax-pos)
            (immutable limits wat-table-type-syntax-limits)
            (immutable reference-type wat-table-type-syntax-reference-type)))

  (define-record-type ($wat-limits-syntax make-wat-limits-syntax wat-limits-syntax?)
    (fields (immutable pos wat-limits-syntax-pos)
            (immutable address-type wat-limits-syntax-address-type)
            (immutable minimum wat-limits-syntax-minimum)
            (immutable maximum wat-limits-syntax-maximum)
            (immutable shared? wat-limits-syntax-shared?)))

  (define-record-type ($wat-memory-type-syntax make-wat-memory-type-syntax
                                               wat-memory-type-syntax?)
    (fields (immutable pos wat-memory-type-syntax-pos)
            (immutable limits wat-memory-type-syntax-limits)))

  (define-record-type
    ($wat-element-segment-syntax make-wat-element-segment-syntax
                                 wat-element-segment-syntax?)
    (fields (immutable pos wat-element-segment-syntax-pos)
            (immutable mode wat-element-segment-syntax-mode)
            (immutable table wat-element-segment-syntax-table)
            (immutable offset wat-element-segment-syntax-offset)
            (immutable address-type wat-element-segment-syntax-address-type)
            (immutable reference-type wat-element-segment-syntax-reference-type)
            (immutable item-kind wat-element-segment-syntax-item-kind)
            (immutable items wat-element-segment-syntax-items)))

  (define-record-type
    ($wat-data-segment-syntax make-wat-data-segment-syntax wat-data-segment-syntax?)
    (fields (immutable pos wat-data-segment-syntax-pos)
            (immutable mode wat-data-segment-syntax-mode)
            (immutable memory wat-data-segment-syntax-memory)
            (immutable offset wat-data-segment-syntax-offset)
            (immutable address-type wat-data-segment-syntax-address-type)
            (immutable strings wat-data-segment-syntax-strings)))

  (define-record-type ($wat-custom-placement make-wat-custom-placement
                                             wat-custom-placement?)
    (fields (immutable pos wat-custom-placement-pos)
            (immutable before wat-custom-placement-before)
            (immutable after wat-custom-placement-after)))

  (define-record-type ($wat-module make-wat-module wat-module?)
    (fields (immutable pos wat-module-pos)
            (immutable id wat-module-id)
            (immutable fields wat-module-fields)))

  (define-record-type ($wat-module-field make-wat-module-field wat-module-field?)
    (fields (immutable pos wat-module-field-pos)
            (immutable kind wat-module-field-kind)
            (immutable id wat-module-field-id)
            (immutable data wat-module-field-data)
            (immutable import wat-module-field-import)
            (immutable exports wat-module-field-exports)
            (immutable abbreviation wat-module-field-abbreviation)))

  (define-record-type
    ($wat-instruction-syntax make-wat-instruction-syntax wat-instruction-syntax?)
    (fields (immutable pos wat-instruction-syntax-pos)
            (immutable mnemonic wat-instruction-syntax-mnemonic)
            (immutable immediates wat-instruction-syntax-immediates)
            (immutable body wat-instruction-syntax-body)
            (immutable alternate wat-instruction-syntax-alternate)
            (immutable operands wat-instruction-syntax-operands)
            (immutable label wat-instruction-syntax-label)))

  (define-record-type
    ($wat-inline-abbreviation make-wat-inline-abbreviation wat-inline-abbreviation?)
    (fields (immutable pos wat-inline-abbreviation-pos)
            (immutable kind wat-inline-abbreviation-kind)
            (immutable data wat-inline-abbreviation-data)))

;;;;===----------------------------------------------------------------------===
;;;; Type parser helpers
;;;;===----------------------------------------------------------------------===

  (define optional-value
    (lambda (value)
      (if (null? value) #f value)))

  (define wat-open
    (<~0> (<char> #\() <wat-trivia>))

  (define wat-close
    (<~0> (<char> #\)) <wat-trivia>))

  (define wat-parenthesized
    (lambda (keyword parser)
      (<~1> (<~0> wat-open (<wat-keyword> keyword)) parser wat-close)))

  (define wat-head
    (lambda (keyword)
      (<~0> <pos> wat-open (<wat-keyword> keyword))))

  (define canonical-value-type?
    (lambda (type)
      (or (memq type '(i32 i64 f32 f64 v128))
          (wasm-reference-type? type))))

  (define canonical-storage-type?
    (lambda (type)
      (or (memq type '(i8 i16)) (canonical-value-type? type))))

  (define all-canonical-bindings?
    (lambda (binding*)
      (let loop ([binding* binding*])
        (or (null? binding*)
            (and (not (wat-binding-id (car binding*)))
                 (canonical-value-type? (wat-binding-type (car binding*)))
                 (loop (cdr binding*)))))))

  (define binding-type-vector
    (lambda (binding*)
      (list->immutable-vector (map wat-binding-type binding*))))

  (define with-position
    (lambda (parser maker)
      (<map> (lambda (value) (maker (car value) (cadr value)))
             (<~> <pos> parser))))

;;;;===----------------------------------------------------------------------===
;;;; Core value, reference, and aggregate types
;;;;===----------------------------------------------------------------------===

  #|proc:<wat-index-reference>
  The `<wat-index-reference>` parser reads a numeric index or source identifier and retains its
  source position. The return value is an internal index-reference record.
  |#
  (define <wat-index-reference>
    (with-position (</> <wat-identifier> <wat-u32>) make-wat-index-reference))

  (define abstract-heap-type-parser
    (apply </>
           (map (lambda (name)
                  (<as> (string->symbol name) (<wat-keyword> name)))
                '("any" "eq" "i31" "struct" "array" "none" "func" "nofunc"
                  "exn" "noexn" "extern" "noextern"))))

  #|proc:<wat-heap-type>
  The `<wat-heap-type>` parser reads an abstract heap type or positioned type index.
  |#
  (define <wat-heap-type>
    (</> abstract-heap-type-parser <wat-index-reference>))

  (define explicit-reference-type
    (<map>
     (lambda (value)
       (let* ([pos (car value)]
              [nullable? (not (null? (cadr value)))]
              [heap-type (caddr value)])
         (if (symbol? heap-type)
             (make-wasm-reference-type nullable? heap-type)
             (make-wat-reference-type-syntax pos nullable? heap-type))))
     (<~0> (<~> (wat-head "ref")
                  (<optional> (<wat-keyword> "null"))
                  <wat-heap-type>)
           wat-close)))

  (define shorthand-reference-type
    (apply
     </>
     (map (lambda (entry)
            (<as> (make-wasm-reference-type #t (cdr entry))
                  (<wat-keyword> (car entry))))
          '(("anyref" . any) ("eqref" . eq) ("i31ref" . i31)
            ("structref" . struct) ("arrayref" . array) ("nullref" . none)
            ("funcref" . func) ("nullfuncref" . nofunc) ("exnref" . exn)
            ("nullexnref" . noexn) ("externref" . extern)
            ("nullexternref" . noextern)))))

  #|proc:<wat-reference-type>
  The `<wat-reference-type>` parser reads an explicit or shorthand reference type.
  |#
  (define <wat-reference-type>
    (</> explicit-reference-type shorthand-reference-type))

  #|proc:<wat-value-type>
  The `<wat-value-type>` parser reads a numeric, vector, or reference value type.
  |#
  (define <wat-value-type>
    (</> <wat-reference-type>
         (<as> 'i32 (<wat-keyword> "i32"))
         (<as> 'i64 (<wat-keyword> "i64"))
         (<as> 'f32 (<wat-keyword> "f32"))
         (<as> 'f64 (<wat-keyword> "f64"))
         (<as> 'v128 (<wat-keyword> "v128"))))

  #|proc:<wat-storage-type>
  The `<wat-storage-type>` parser reads an i8, i16, or value storage type.
  |#
  (define <wat-storage-type>
    (</> (<as> 'i8 (<wat-keyword> "i8"))
         (<as> 'i16 (<wat-keyword> "i16"))
         <wat-value-type>))

  (define mutable-field-type
    (<map> (lambda (value)
             (let ([pos (car value)] [storage-type (cadr value)])
               (if (canonical-storage-type? storage-type)
                   (make-wasm-field-type storage-type #t)
                   (make-wat-field-type-syntax pos storage-type #t))))
           (<~0> (<~> (wat-head "mut") <wat-storage-type>)
                 wat-close)))

  #|proc:<wat-field-type>
  The `<wat-field-type>` parser reads a mutable or immutable aggregate field type.
  |#
  (define <wat-field-type>
    (</> mutable-field-type
         (<map> (lambda (value)
                  (let ([pos (car value)] [storage-type (cadr value)])
                    (if (canonical-storage-type? storage-type)
                        (make-wasm-field-type storage-type #f)
                        (make-wat-field-type-syntax pos storage-type #f))))
                (<~> <pos> <wat-storage-type>))))

  (define named-parameter-clause
    (<map> (lambda (value)
             (list (make-wat-binding (car value) (cadr value) (caddr value))))
           (<~0> (<~> (wat-head "param") <wat-identifier> <wat-value-type>)
                 wat-close)))

  (define unnamed-parameter-clause
    (<map> (lambda (value)
             (let ([pos (car value)])
               (map (lambda (type) (make-wat-binding pos #f type)) (cadr value))))
           (<~0> (<~> (wat-head "param") (<many> <wat-value-type>))
                 wat-close)))

  (define parameter-clause
    (</> named-parameter-clause unnamed-parameter-clause))

  (define result-clause
    (<~1> wat-open
           (<~1> (<wat-keyword> "result") (<many> <wat-value-type>))
           wat-close))

  (define make-function-type
    (lambda (value)
      (let* ([pos (car value)]
             [parameter* (apply append (cadr value))]
             [result* (apply append (caddr value))])
        (if (and (all-canonical-bindings? parameter*)
                 (for-all canonical-value-type? result*))
            (make-wasm-function-type (binding-type-vector parameter*)
                                     (list->immutable-vector result*))
            (make-wat-function-type-syntax
             pos (list->immutable-vector parameter*)
             (list->immutable-vector result*))))))

  #|proc:<wat-function-type>
  The `<wat-function-type>` parser reads function parameter and result clauses, retaining names.
  |#
  (define <wat-function-type>
    (<map> make-function-type
           (<~0> (<~> (wat-head "func")
                      (<many> parameter-clause) (<many> result-clause))
                 wat-close)))

  (define named-struct-field
    (<map> (lambda (value)
             (list (make-wat-binding (car value) (cadr value) (caddr value))))
           (<~0> (<~> (wat-head "field") <wat-identifier> <wat-field-type>)
                 wat-close)))

  (define unnamed-struct-field
    (<map> (lambda (value)
             (let ([pos (car value)])
               (map (lambda (type) (make-wat-binding pos #f type)) (cadr value))))
           (<~0> (<~> (wat-head "field") (<many> <wat-field-type>))
                 wat-close)))

  (define struct-field (</> named-struct-field unnamed-struct-field))

  (define canonical-field-binding?
    (lambda (binding)
      (and (not (wat-binding-id binding))
           (wasm-field-type? (wat-binding-type binding)))))

  #|proc:<wat-struct-type>
  The `<wat-struct-type>` parser reads a structure type and retains optional field identifiers.
  |#
  (define <wat-struct-type>
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [field* (apply append (cadr value))])
         (if (for-all canonical-field-binding? field*)
             (make-wasm-struct-type
              (list->immutable-vector (map wat-binding-type field*)))
             (make-wat-struct-type-syntax pos (list->immutable-vector field*)))))
     (<~0> (<~> (wat-head "struct") (<many> struct-field))
           wat-close)))

  #|proc:<wat-array-type>
  The `<wat-array-type>` parser reads an array element field type.
  |#
  (define <wat-array-type>
    (<map> (lambda (value)
             (let ([pos (car value)] [field (cadr value)])
               (if (wasm-field-type? field)
                   (make-wasm-array-type field)
                   (make-wat-array-type-syntax pos field))))
           (<~0> (<~> (wat-head "array") <wat-field-type>)
                 wat-close)))

  #|proc:<wat-composite-type>
  The `<wat-composite-type>` parser reads a function, structure, or array type.
  |#
  (define <wat-composite-type>
    (</> <wat-function-type> <wat-struct-type> <wat-array-type>))

  (define explicit-subtype
    (<map>
     (lambda (value)
       (make-wat-subtype-syntax
        (car value) #f (not (null? (cadr value)))
        (list->immutable-vector (caddr value)) (cadddr value)))
     (<~0> (<~> (wat-head "sub")
                  (<optional> (<wat-keyword> "final"))
                  (<many> <wat-index-reference>) <wat-composite-type>)
           wat-close)))

  #|proc:<wat-subtype>
  The `<wat-subtype>` parser reads an explicit subtype or shorthand final composite type.
  |#
  (define <wat-subtype>
    (</> explicit-subtype
         (<map> (lambda (value)
                  (make-wat-subtype-syntax (car value) #f #t '#() (cadr value)))
                (<~> <pos> <wat-composite-type>))))

  #|proc:<wat-type-definition>
  The `<wat-type-definition>` parser reads a named or anonymous subtype definition.
  |#
  (define <wat-type-definition>
    (<map>
     (lambda (value)
       (let ([id (optional-value (cadr value))] [subtype (caddr value)])
         (make-wat-subtype-syntax
          (car value) id
          (wat-subtype-syntax-final? subtype)
          (wat-subtype-syntax-supertypes subtype)
          (wat-subtype-syntax-composite-type subtype))))
     (<~0> (<~> (wat-head "type")
                  (<optional> <wat-identifier>) <wat-subtype>)
           wat-close)))

  #|proc:<wat-recursive-type>
  The `<wat-recursive-type>` parser reads a recursive group containing named type definitions.
  |#
  (define <wat-recursive-type>
    (<map> (lambda (value)
             (make-wat-recursive-type-syntax
              (car value) (list->immutable-vector (cadr value))))
           (<~0> (<~> (wat-head "rec")
                      (<many> <wat-type-definition>))
                 wat-close)))

;;;;===----------------------------------------------------------------------===
;;;; Type uses and entity types
;;;;===----------------------------------------------------------------------===

  (define explicit-type-index
    (wat-parenthesized "type" <wat-index-reference>))

  (define type-use-parser
    (lambda (parameters)
      (<map> (lambda (value)
               (let ([parameter* (apply append (caddr value))]
                     [result* (apply append (cadddr value))])
                 (make-wat-type-use
                  (car value) (optional-value (cadr value))
                  (list->immutable-vector parameter*)
                  (list->immutable-vector result*))))
             (<~> <pos> (<optional> explicit-type-index)
                  (<many> parameters) (<many> result-clause)))))

  #|proc:<wat-type-use>
  The `<wat-type-use>` parser reads an optional type index and inline function signature,
  retaining parameter bindings for function declarations.
  |#
  (define <wat-type-use> (type-use-parser parameter-clause))

  #|proc:<wat-anonymous-type-use>
  The `<wat-anonymous-type-use>` parser reads an optional type index and an inline signature
  without parameter names, returning type-use syntax for instruction signatures.
  |#
  (define <wat-anonymous-type-use> (type-use-parser unnamed-parameter-clause))

  #|proc:<wat-block-type>
  The `<wat-block-type>` parser reads an optional indexed or inline block signature.
  |#
  (define <wat-block-type>
    (<map> (lambda (type-use)
             (make-wat-block-type-syntax (wat-type-use-pos type-use) type-use))
           <wat-anonymous-type-use>))

  (define limits-parser
    (lambda (pos address-type bound-parser)
      (<bind> (<~> bound-parser
                   (<optional> bound-parser)
                   (<optional> (<wat-keyword> "shared")))
              (lambda (bound*)
                (let ([minimum (car bound*)]
                      [maximum (optional-value (cadr bound*))]
                      [shared? (not (null? (caddr bound*)))])
                  (cond [(and shared? (not maximum))
                         (<fail-with> "shared memory limits require a maximum")]
                        [(and maximum (> minimum maximum))
                         (<fail-with> "WebAssembly limits maximum is below minimum")]
                        [shared?
                         (<result>
                          (make-wat-limits-syntax
                           pos address-type minimum maximum #t))]
                        [else
                         (<result>
                          (make-wasm-limits address-type minimum maximum))]))))))

  #|proc:<wat-limits>
  The `<wat-limits>` parser reads an optional i32/i64 address type and u64 minimum and optional
  maximum bounds. It returns limits syntax or a canonical limits record; the address defaults to i32.
  |#
  (define <wat-limits>
    (<bind> (<~> <pos>
                 (</> (<as> 'i64 (<wat-keyword> "i64"))
                      (<as> 'i32 (<wat-keyword> "i32"))
                      (<result> 'i32)))
            (lambda (value)
              (let ([pos (car value)] [address-type (cadr value)])
                (limits-parser pos address-type <wat-u64>)))))

  #|proc:<wat-global-type>
  The `<wat-global-type>` parser reads a mutable or immutable global value type.
  |#
  (define <wat-global-type>
    (<map>
     (lambda (value)
       (let ([pos (car value)] [type (cadr value)] [mutable? (caddr value)])
         (if (canonical-value-type? type)
             (make-wasm-global-type type mutable?)
             (make-wat-global-type-syntax pos type mutable?))))
     (</> (<map> (lambda (value) (list (car value) (cadr value) #t))
                 (<~> <pos> (wat-parenthesized "mut" <wat-value-type>)))
          (<map> (lambda (value) (list (car value) (cadr value) #f))
                 (<~> <pos> <wat-value-type>)))))

  #|proc:<wat-table-type>
  The `<wat-table-type>` parser reads table limits followed by a reference type.
  |#
  (define <wat-table-type>
    (<bind>
     (<~> <pos> <wat-limits> <wat-reference-type>)
     (lambda (value)
       (let ([pos (car value)]
             [limits (cadr value)]
             [reference-type (caddr value)])
         (cond [(wat-limits-syntax? limits)
                (<fail-with> "WebAssembly tables cannot be shared")]
               [(wasm-reference-type? reference-type)
                (<result> (make-wasm-table-type reference-type limits))]
               [else
                (<result>
                 (make-wat-table-type-syntax pos limits reference-type))])))))

  #|proc:<wat-memory-type>
  The `<wat-memory-type>` parser reads memory limits.
  |#
  (define <wat-memory-type>
    (<map> (lambda (value)
             (if (wasm-limits? value)
                 (make-wasm-memory-type value)
                 (make-wat-memory-type-syntax (wat-limits-syntax-pos value) value)))
           <wat-limits>))

  #|proc:<wat-tag-type>
  The `<wat-tag-type>` parser reads a tag function type use.
  |#
  (define <wat-tag-type> <wat-type-use>)

  )
