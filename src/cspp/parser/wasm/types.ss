(library (cspp parser wasm types)
  (export
    make-wasm-module wasm-module? wasm-module-types wasm-module-imports
    wasm-module-functions wasm-module-tables wasm-module-memories wasm-module-globals
    wasm-module-tags wasm-module-exports wasm-module-start wasm-module-elements
    wasm-module-data wasm-module-custom-sections
    make-wasm-custom-section wasm-custom-section? wasm-custom-section-name
    wasm-custom-section-bytes wasm-custom-section-after-section
    make-wasm-recursive-type wasm-recursive-type? wasm-recursive-type-subtypes
    make-wasm-subtype wasm-subtype? wasm-subtype-final? wasm-subtype-supertypes
    wasm-subtype-composite-type
    make-wasm-function-type wasm-function-type? wasm-function-type-parameters
    wasm-function-type-results
    make-wasm-struct-type wasm-struct-type? wasm-struct-type-fields
    make-wasm-array-type wasm-array-type? wasm-array-type-field
    make-wasm-field-type wasm-field-type? wasm-field-type-storage-type
    wasm-field-type-mutable?
    make-wasm-reference-type wasm-reference-type? wasm-reference-type-nullable?
    wasm-reference-type-heap-type
    make-wasm-limits wasm-limits? wasm-limits-address-type wasm-limits-minimum
    wasm-limits-maximum wasm-limits-shared?
    make-wasm-table-type wasm-table-type? wasm-table-type-reference-type
    wasm-table-type-limits
    make-wasm-memory-type wasm-memory-type? wasm-memory-type-limits
    make-wasm-global-type wasm-global-type? wasm-global-type-value-type
    wasm-global-type-mutable?
    make-wasm-tag-type wasm-tag-type? wasm-tag-type-type-index
    make-wasm-external-type wasm-external-type? wasm-external-type-kind
    wasm-external-type-type
    make-wasm-import wasm-import? wasm-import-module wasm-import-name
    wasm-import-external-type
    make-wasm-function wasm-function? wasm-function-type-index wasm-function-locals
    wasm-function-body
    make-wasm-table wasm-table? wasm-table-type wasm-table-initializer
    make-wasm-memory wasm-memory? wasm-memory-type
    make-wasm-global wasm-global? wasm-global-type wasm-global-initializer
    make-wasm-tag wasm-tag? wasm-tag-type
    make-wasm-export wasm-export? wasm-export-name wasm-export-kind wasm-export-index
    make-wasm-element wasm-element? wasm-element-mode wasm-element-reference-type
    wasm-element-table-index wasm-element-offset wasm-element-initializers
    make-wasm-data wasm-data? wasm-data-mode wasm-data-memory-index wasm-data-offset
    wasm-data-bytes
    make-wasm-instruction wasm-instruction? wasm-instruction-mnemonic
    wasm-instruction-immediates wasm-instruction-body wasm-instruction-alternate
    make-wasm-memory-argument wasm-memory-argument? wasm-memory-argument-alignment
    wasm-memory-argument-offset wasm-memory-argument-memory-index
    make-wasm-block-type wasm-block-type? wasm-block-type-kind wasm-block-type-value
    make-wasm-catch wasm-catch? wasm-catch-kind wasm-catch-tag-index
    wasm-catch-label-index
    make-wasm-float wasm-float? wasm-float-width wasm-float-bits)
  (import (cspp chez)
          (cspp parser private)
          (cspp utils))

  (define vector-of?
    (lambda (predicate)
      (lambda (value)
        (and (vector? value)
             (let loop ([index 0])
               (or (fx= index (vector-length value))
                   (and (predicate (vector-ref value index))
                        (loop (fx1+ index)))))))))

  (define optional-natural? (lambda (value) (or (not value) (natural? value))))
  (define instruction-vector?
    (lambda (value) ((vector-of? wasm-instruction?) value)))
  (define optional-instruction-vector?
    (lambda (value) (or (not value) (instruction-vector? value))))
  (define address-type? (lambda (value) (memq value '(i32 i64))))
  (define heap-type?
    (lambda (value)
      (or (natural? value)
          (and (symbol? value)
               (memq value '(any eq i31 struct array none func nofunc exn noexn
                                  extern noextern))))))
  (define value-type?
    (lambda (value)
      (or (and (symbol? value) (memq value '(i32 i64 f32 f64 v128)))
          (wasm-reference-type? value))))
  (define storage-type?
    (lambda (value)
      (or (and (symbol? value) (memq value '(i8 i16)))
          (value-type? value))))
  (define composite-type?
    (lambda (value)
      (or (wasm-function-type? value)
          (wasm-struct-type? value)
          (wasm-array-type? value))))
  (define external-kind?
    (lambda (value) (and (symbol? value) (memq value '(function table memory global tag)))))
  (define element-mode?
    (lambda (value) (and (symbol? value) (memq value '(active passive declarative)))))
  (define data-mode? (lambda (value) (and (symbol? value) (memq value '(active passive)))))
  (define block-kind?
    (lambda (value) (and (symbol? value) (memq value '(empty value-type type-index)))))
  (define catch-kind?
    (lambda (value)
      (and (symbol? value) (memq value '(catch catch-ref catch-all catch-all-ref)))))
  (define instruction-alternate?
    (lambda (value)
      (and (vector? value)
           (or ((vector-of? wasm-instruction?) value)
               ((vector-of? wasm-catch?) value)))))
  (define float-width? (lambda (value) (or (eqv? value 32) (eqv? value 64))))
  (define section-position?
    (lambda (value)
      (or (not value)
          (and (symbol? value)
               (memq value '(header type import function table memory tag global export start
                                    element data-count code data))))))

  (define external-type-fields?
    (lambda (field*)
      (let ([kind (vector-ref field* 0)] [type (vector-ref field* 1)])
        (case kind
          [(function) (natural? type)]
          [(table) (wasm-table-type? type)]
          [(memory) (wasm-memory-type? type)]
          [(global) (wasm-global-type? type)]
          [(tag) (wasm-tag-type? type)]
          [else #f]))))

  (define block-type-fields?
    (lambda (field*)
      (let ([kind (vector-ref field* 0)] [value (vector-ref field* 1)])
        (case kind
          [(empty) (not value)]
          [(value-type) (value-type? value)]
          [(type-index) (natural? value)]
          [else #f]))))

  (define catch-fields?
    (lambda (field*)
      (let ([kind (vector-ref field* 0)] [tag-index (vector-ref field* 1)])
        (case kind
          [(catch catch-ref) (natural? tag-index)]
          [(catch-all catch-all-ref) (not tag-index)]
          [else #f]))))

  (define float-fields?
    (lambda (field*)
      (let ([width (vector-ref field* 0)] [bits (vector-ref field* 1)])
        (and (natural? bits)
             (if (= width 32)
                 (<= bits #xffffffff)
                 (<= bits #xffffffffffffffff))))))

  (define limits-fields?
    (lambda (field*)
      (let ([minimum (vector-ref field* 1)] [maximum (vector-ref field* 2)])
        (and (or (not maximum) (>= maximum minimum))
             (or (not (vector-ref field* 3)) maximum)))))

  (define element-fields?
    (lambda (field*)
      (let ([mode (vector-ref field* 0)]
            [table-index (vector-ref field* 2)]
            [offset (vector-ref field* 3)])
        (case mode
          [(active) (and (natural? table-index) (instruction-vector? offset))]
          [(passive declarative) (and (not table-index) (not offset))]
          [else #f]))))

  (define data-fields?
    (lambda (field*)
      (let ([mode (vector-ref field* 0)]
            [memory-index (vector-ref field* 1)]
            [offset (vector-ref field* 2)])
        (case mode
          [(active) (and (natural? memory-index) (instruction-vector? offset))]
          [(passive) (and (not memory-index) (not offset))]
          [else #f]))))


;;;;===----------------------------------------------------------------------===
;;;; Canonical modules and custom sections
;;;;===----------------------------------------------------------------------===

  ;; Records have immutable fields but retain supplied mutable contents. Constructors check
  ;; field shapes and the relationships documented below; they do not perform WASM validation.
  ;; Value types are i32, i64, f32, f64, v128, or wasm-reference-type records. Unless stated
  ;; otherwise, index arguments use the existing natural? contract, without encoded-width checks.

  #|proc:make-wasm-module
  Creates and returns a `wasm-module` record. `types` is a vector of `wasm-recursive-type` groups
  in type-section order. `imports` is a vector of `wasm-import` records in import order.
  `functions` is a vector of defined `wasm-function` records, excluding imports. `tables` is a
  vector of defined `wasm-table` records, excluding imports. `memories` is a vector of defined
  `wasm-memory` records, excluding imports. `globals` is a vector of defined `wasm-global`
  records, excluding imports. `tags` is a vector of defined `wasm-tag` records, excluding imports.
  `exports` is a vector of `wasm-export` records. `start` is a natural zero-based function index,
  or `#f` when absent. `elements` is a vector of `wasm-element` segments. `data` is a vector of
  `wasm-data` segments. `custom-sections` is a vector of `wasm-custom-section` records in
  custom-section order.
  Entity indexes include imports before definitions in each namespace; type indexes flatten
  recursive groups. The constructor checks record/vector shapes, not module typing or index
  bounds.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-module?
  Returns whether `object` is a module record. `object` is the value to test.
  |#
  #|proc:wasm-module-types
  Returns the stored `types` of `wasm-module` record `record`. It is a vector of
  `wasm-recursive-type` groups in type-section order. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-imports
  Returns the stored `imports` of `wasm-module` record `record`. It is a vector of `wasm-import`
  records in import order. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-functions
  Returns the stored `functions` of `wasm-module` record `record`. It is a vector of defined
  `wasm-function` records, excluding imports. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-tables
  Returns the stored `tables` of `wasm-module` record `record`. It is a vector of defined
  `wasm-table` records, excluding imports. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-memories
  Returns the stored `memories` of `wasm-module` record `record`. It is a vector of defined
  `wasm-memory` records, excluding imports. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-globals
  Returns the stored `globals` of `wasm-module` record `record`. It is a vector of defined
  `wasm-global` records, excluding imports. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-tags
  Returns the stored `tags` of `wasm-module` record `record`. It is a vector of defined `wasm-tag`
  records, excluding imports. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-exports
  Returns the stored `exports` of `wasm-module` record `record`. It is a vector of `wasm-export`
  records. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-start
  Returns the stored `start` of `wasm-module` record `record`. It is a natural zero-based function
  index, or `#f` when absent.
  |#
  #|proc:wasm-module-elements
  Returns the stored `elements` of `wasm-module` record `record`. It is a vector of `wasm-element`
  segments. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-data
  Returns the stored `data` of `wasm-module` record `record`. It is a vector of `wasm-data`
  segments. Mutable contents are returned by reference.
  |#
  #|proc:wasm-module-custom-sections
  Returns the stored `custom-sections` of `wasm-module` record `record`. It is a vector of
  `wasm-custom-section` records in custom-section order. Mutable contents are returned by
  reference.
  |#
  (define-checked-record-type $wasm-module make-wasm-module wasm-module? wasm-module
    $make-wasm-module $wasm-module?
    ([types wasm-module-types $wasm-module-types (vector-of? wasm-recursive-type?)]
     [imports wasm-module-imports $wasm-module-imports (vector-of? wasm-import?)]
     [functions wasm-module-functions $wasm-module-functions (vector-of? wasm-function?)]
     [tables wasm-module-tables $wasm-module-tables (vector-of? wasm-table?)]
     [memories wasm-module-memories $wasm-module-memories (vector-of? wasm-memory?)]
     [globals wasm-module-globals $wasm-module-globals (vector-of? wasm-global?)]
     [tags wasm-module-tags $wasm-module-tags (vector-of? wasm-tag?)]
     [exports wasm-module-exports $wasm-module-exports (vector-of? wasm-export?)]
     [start wasm-module-start $wasm-module-start optional-natural?]
     [elements wasm-module-elements $wasm-module-elements (vector-of? wasm-element?)]
     [data wasm-module-data $wasm-module-data (vector-of? wasm-data?)]
     [custom-sections wasm-module-custom-sections $wasm-module-custom-sections
                      (vector-of? wasm-custom-section?)]))

  #|proc:make-wasm-custom-section
  Creates and returns a `wasm-custom-section` record. `name` is a decoded section-name string.
  `bytes` is a bytevector of payload bytes after the encoded section name. `after-section` is `#f`
  before all standard sections, or one of `header`, `type`, `import`, `function`, `table`,
  `memory`, `tag`, `global`, `export`, `start`, `element`, `data-count`, `code`, `data`.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-custom-section?
  Returns whether `object` is a custom section. `object` is the value to test.
  |#
  #|proc:wasm-custom-section-name
  Returns the stored `name` of `wasm-custom-section` record `record`. It is a decoded section-name
  string. Mutable contents are returned by reference.
  |#
  #|proc:wasm-custom-section-bytes
  Returns the stored `bytes` of `wasm-custom-section` record `record`. It is a bytevector of
  payload bytes after the encoded section name. Mutable contents are returned by reference.
  |#
  #|proc:wasm-custom-section-after-section
  Returns the stored `after-section` of `wasm-custom-section` record `record`. It is `#f` before
  all standard sections, or one of `header`, `type`, `import`, `function`, `table`, `memory`,
  `tag`, `global`, `export`, `start`, `element`, `data-count`, `code`, `data`.
  |#
  (define-checked-record-type $wasm-custom-section
    make-wasm-custom-section wasm-custom-section? wasm-custom-section
    $make-wasm-custom-section $wasm-custom-section?
    ([name wasm-custom-section-name $wasm-custom-section-name string?]
     [bytes wasm-custom-section-bytes $wasm-custom-section-bytes bytevector?]
     [after-section wasm-custom-section-after-section $wasm-custom-section-after-section
                    section-position?]))

;;;;===----------------------------------------------------------------------===
;;;; Canonical type records
;;;;===----------------------------------------------------------------------===

  #|proc:make-wasm-recursive-type
  Creates and returns a `wasm-recursive-type` record. `subtypes` is a vector of `wasm-subtype`
  records in group order.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-recursive-type?
  Returns whether `object` is a recursive type. `object` is the value to test.
  |#
  #|proc:wasm-recursive-type-subtypes
  Returns the stored `subtypes` of `wasm-recursive-type` record `record`. It is a vector of
  `wasm-subtype` records in group order. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-recursive-type
    make-wasm-recursive-type wasm-recursive-type? wasm-recursive-type
    $make-wasm-recursive-type $wasm-recursive-type?
    ([subtypes wasm-recursive-type-subtypes $wasm-recursive-type-subtypes
               (vector-of? wasm-subtype?)]))

  #|proc:make-wasm-subtype
  Creates and returns a `wasm-subtype` record. `final?` is a boolean forbidding further subtyping.
  `supertypes` is a vector of natural zero-based type indexes. `composite-type` is a
  `wasm-function-type`, `wasm-struct-type`, or `wasm-array-type` record.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-subtype?
  Returns whether `object` is a subtype. `object` is the value to test.
  |#
  #|proc:wasm-subtype-final?
  Returns the stored `final?` of `wasm-subtype` record `record`. It is a boolean forbidding
  further subtyping.
  |#
  #|proc:wasm-subtype-supertypes
  Returns the stored `supertypes` of `wasm-subtype` record `record`. It is a vector of natural
  zero-based type indexes. Mutable contents are returned by reference.
  |#
  #|proc:wasm-subtype-composite-type
  Returns the stored `composite-type` of `wasm-subtype` record `record`. It is a
  `wasm-function-type`, `wasm-struct-type`, or `wasm-array-type` record.
  |#
  (define-checked-record-type $wasm-subtype make-wasm-subtype wasm-subtype? wasm-subtype
    $make-wasm-subtype $wasm-subtype?
    ([final? wasm-subtype-final? $wasm-subtype-final? boolean?]
     [supertypes wasm-subtype-supertypes $wasm-subtype-supertypes (vector-of? natural?)]
     [composite-type wasm-subtype-composite-type $wasm-subtype-composite-type composite-type?]))

  #|proc:make-wasm-function-type
  Creates and returns a `wasm-function-type` record. `parameters` is a vector of parameter value
  types in argument order. `results` is a vector of result value types in result order.
  Each value type is `i32`, `i64`, `f32`, `f64`, `v128`, or a `wasm-reference-type` record.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-function-type?
  Returns whether `object` is a function type. `object` is the value to test.
  |#
  #|proc:wasm-function-type-parameters
  Returns the stored `parameters` of `wasm-function-type` record `record`. It is a vector of
  parameter value types in argument order. Mutable contents are returned by reference.
  |#
  #|proc:wasm-function-type-results
  Returns the stored `results` of `wasm-function-type` record `record`. It is a vector of result
  value types in result order. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-function-type
    make-wasm-function-type wasm-function-type? wasm-function-type
    $make-wasm-function-type $wasm-function-type?
    ([parameters wasm-function-type-parameters $wasm-function-type-parameters
                 (vector-of? value-type?)]
     [results wasm-function-type-results $wasm-function-type-results
              (vector-of? value-type?)]))

  #|proc:make-wasm-struct-type
  Creates and returns a `wasm-struct-type` record. `fields` is a vector of `wasm-field-type`
  records in field-index order.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-struct-type?
  Returns whether `object` is a struct type. `object` is the value to test.
  |#
  #|proc:wasm-struct-type-fields
  Returns the stored `fields` of `wasm-struct-type` record `record`. It is a vector of
  `wasm-field-type` records in field-index order. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-struct-type make-wasm-struct-type wasm-struct-type? wasm-struct-type
    $make-wasm-struct-type $wasm-struct-type?
    ([fields wasm-struct-type-fields $wasm-struct-type-fields (vector-of? wasm-field-type?)]))

  #|proc:make-wasm-array-type
  Creates and returns a `wasm-array-type` record. `field` is a `wasm-field-type` record describing
  each array element.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-array-type?
  Returns whether `object` is an array type. `object` is the value to test.
  |#
  #|proc:wasm-array-type-field
  Returns the stored `field` of `wasm-array-type` record `record`. It is a `wasm-field-type`
  record describing each array element.
  |#
  (define-checked-record-type $wasm-array-type make-wasm-array-type wasm-array-type? wasm-array-type
    $make-wasm-array-type $wasm-array-type?
    ([field wasm-array-type-field $wasm-array-type-field wasm-field-type?]))

  #|proc:make-wasm-field-type
  Creates and returns a `wasm-field-type` record. `storage-type` is a value type (`i32`, `i64`,
  `f32`, `f64`, `v128`, or `wasm-reference-type`), or packed-storage symbol `i8` or `i16`.
  `mutable?` is a boolean allowing field mutation.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-field-type?
  Returns whether `object` is a field type. `object` is the value to test.
  |#
  #|proc:wasm-field-type-storage-type
  Returns the stored `storage-type` of `wasm-field-type` record `record`. It is a value type
  (`i32`, `i64`, `f32`, `f64`, `v128`, or `wasm-reference-type`), or packed-storage symbol `i8` or
  `i16`.
  |#
  #|proc:wasm-field-type-mutable?
  Returns the stored `mutable?` of `wasm-field-type` record `record`. It is a boolean allowing
  field mutation.
  |#
  (define-checked-record-type $wasm-field-type make-wasm-field-type wasm-field-type? wasm-field-type
    $make-wasm-field-type $wasm-field-type?
    ([storage-type wasm-field-type-storage-type $wasm-field-type-storage-type storage-type?]
     [mutable? wasm-field-type-mutable? $wasm-field-type-mutable? boolean?]))

  #|proc:make-wasm-reference-type
  Creates and returns a `wasm-reference-type` record. `nullable?` is a boolean allowing null
  references. `heap-type` is a natural zero-based concrete type index or abstract heap symbol
  `any`, `eq`, `i31`, `struct`, `array`, `none`, `func`, `nofunc`, `exn`, `noexn`, `extern`, or
  `noextern`.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-reference-type?
  Returns whether `object` is a reference type. `object` is the value to test.
  |#
  #|proc:wasm-reference-type-nullable?
  Returns the stored `nullable?` of `wasm-reference-type` record `record`. It is a boolean
  allowing null references.
  |#
  #|proc:wasm-reference-type-heap-type
  Returns the stored `heap-type` of `wasm-reference-type` record `record`. It is a natural
  zero-based concrete type index or abstract heap symbol `any`, `eq`, `i31`, `struct`, `array`,
  `none`, `func`, `nofunc`, `exn`, `noexn`, `extern`, or `noextern`.
  |#
  (define-checked-record-type $wasm-reference-type
    make-wasm-reference-type wasm-reference-type? wasm-reference-type
    $make-wasm-reference-type $wasm-reference-type?
    ([nullable? wasm-reference-type-nullable? $wasm-reference-type-nullable? boolean?]
     [heap-type wasm-reference-type-heap-type $wasm-reference-type-heap-type heap-type?]))

  #|proc:wasm-limits?
  Returns whether `object` is limits. `object` is the value to test.
  |#
  #|proc:wasm-limits-address-type
  Returns the stored `address-type` of `wasm-limits` record `record`. It is the symbol `i32` or
  `i64`.
  |#
  #|proc:wasm-limits-minimum
  Returns the stored `minimum` of `wasm-limits` record `record`. It is a natural lower bound in
  table elements or 64 KiB memory pages.
  |#
  #|proc:wasm-limits-maximum
  Returns the stored `maximum` of `wasm-limits` record `record`. It is a natural upper bound in
  the same units, or `#f` when absent.
  |#
  #|proc:wasm-limits-shared?
  Returns the stored `shared?` of `wasm-limits` record `record`. It is a boolean selecting shared
  memory.
  |#
  (define-checked-record-type $wasm-limits make-checked-wasm-limits wasm-limits? wasm-limits
    $make-wasm-limits $wasm-limits?
    limits-fields?
    ([address-type wasm-limits-address-type $wasm-limits-address-type address-type?]
     [minimum wasm-limits-minimum $wasm-limits-minimum natural?]
     [maximum wasm-limits-maximum $wasm-limits-maximum optional-natural?]
     [shared? wasm-limits-shared? $wasm-limits-shared? boolean?]))

  #|proc:make-wasm-limits
  Creates and returns a `wasm-limits` record. `address-type` is the symbol `i32` or `i64`.
  `minimum` is a natural lower bound in table elements or 64 KiB memory pages. `maximum` is a
  natural upper bound in the same units, or `#f` when absent. `shared?` is a boolean selecting
  shared memory.
  The three-argument form defaults `shared?` to `#f`. A supplied maximum must be at least the
  minimum; shared limits require a maximum.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  (define make-wasm-limits
    (case-lambda
      [(address-type minimum maximum) (make-wasm-limits address-type minimum maximum #f)]
      [(address-type minimum maximum shared?)
       (pcheck ([address-type? address-type] [natural? minimum]
                [optional-natural? maximum] [boolean? shared?])
               (make-checked-wasm-limits address-type minimum maximum shared?))]))

  #|proc:make-wasm-table-type
  Creates and returns a `wasm-table-type` record. `reference-type` is a `wasm-reference-type`
  record for table elements. `limits` is a nonshared `wasm-limits` record, measured in table
  elements.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-table-type?
  Returns whether `object` is a table type. `object` is the value to test.
  |#
  #|proc:wasm-table-type-reference-type
  Returns the stored `reference-type` of `wasm-table-type` record `record`. It is a
  `wasm-reference-type` record for table elements.
  |#
  #|proc:wasm-table-type-limits
  Returns the stored `limits` of `wasm-table-type` record `record`. It is a nonshared
  `wasm-limits` record, measured in table elements.
  |#
  (define-checked-record-type $wasm-table-type make-wasm-table-type wasm-table-type? wasm-table-type
    $make-wasm-table-type $wasm-table-type?
    (lambda (fields) (not (wasm-limits-shared? (vector-ref fields 1))))
    ([reference-type wasm-table-type-reference-type $wasm-table-type-reference-type
                     wasm-reference-type?]
     [limits wasm-table-type-limits $wasm-table-type-limits wasm-limits?]))

  #|proc:make-wasm-memory-type
  Creates and returns a `wasm-memory-type` record. `limits` is a `wasm-limits` record measured in
  64 KiB memory pages.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-memory-type?
  Returns whether `object` is a memory type. `object` is the value to test.
  |#
  #|proc:wasm-memory-type-limits
  Returns the stored `limits` of `wasm-memory-type` record `record`. It is a `wasm-limits` record
  measured in 64 KiB memory pages.
  |#
  (define-checked-record-type $wasm-memory-type make-wasm-memory-type wasm-memory-type? wasm-memory-type
    $make-wasm-memory-type $wasm-memory-type?
    ([limits wasm-memory-type-limits $wasm-memory-type-limits wasm-limits?]))

  #|proc:make-wasm-global-type
  Creates and returns a `wasm-global-type` record. `value-type` is a value type (`i32`, `i64`,
  `f32`, `f64`, `v128`, or `wasm-reference-type`). `mutable?` is a boolean allowing global
  mutation.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-global-type?
  Returns whether `object` is a global type. `object` is the value to test.
  |#
  #|proc:wasm-global-type-value-type
  Returns the stored `value-type` of `wasm-global-type` record `record`. It is a value type
  (`i32`, `i64`, `f32`, `f64`, `v128`, or `wasm-reference-type`).
  |#
  #|proc:wasm-global-type-mutable?
  Returns the stored `mutable?` of `wasm-global-type` record `record`. It is a boolean allowing
  global mutation.
  |#
  (define-checked-record-type $wasm-global-type make-wasm-global-type wasm-global-type? wasm-global-type
    $make-wasm-global-type $wasm-global-type?
    ([value-type wasm-global-type-value-type $wasm-global-type-value-type value-type?]
     [mutable? wasm-global-type-mutable? $wasm-global-type-mutable? boolean?]))

  #|proc:make-wasm-tag-type
  Creates and returns a `wasm-tag-type` record. `type-index` is a natural zero-based function type
  index.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-tag-type?
  Returns whether `object` is a tag type. `object` is the value to test.
  |#
  #|proc:wasm-tag-type-type-index
  Returns the stored `type-index` of `wasm-tag-type` record `record`. It is a natural zero-based
  function type index.
  |#
  (define-checked-record-type $wasm-tag-type make-wasm-tag-type wasm-tag-type? wasm-tag-type
    $make-wasm-tag-type $wasm-tag-type?
    ([type-index wasm-tag-type-type-index $wasm-tag-type-type-index natural?]))

  #|proc:make-wasm-external-type
  Creates and returns a `wasm-external-type` record. `kind` is the namespace symbol `function`,
  `table`, `memory`, `global`, or `tag`. `type` is a natural function type index for `function`,
  or a matching `wasm-table-type`, `wasm-memory-type`, `wasm-global-type`, or `wasm-tag-type`
  record.
  The constructor checks that the payload matches `kind`.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-external-type?
  Returns whether `object` is an external type. `object` is the value to test.
  |#
  #|proc:wasm-external-type-kind
  Returns the stored `kind` of `wasm-external-type` record `record`. It is the namespace symbol
  `function`, `table`, `memory`, `global`, or `tag`.
  |#
  #|proc:wasm-external-type-type
  Returns the stored `type` of `wasm-external-type` record `record`. It is a natural function type
  index for `function`, or a matching `wasm-table-type`, `wasm-memory-type`, `wasm-global-type`,
  or `wasm-tag-type` record.
  |#
  (define-checked-record-type $wasm-external-type
    make-wasm-external-type wasm-external-type? wasm-external-type
    $make-wasm-external-type $wasm-external-type?
    external-type-fields?
    ([kind wasm-external-type-kind $wasm-external-type-kind external-kind?]
     [type wasm-external-type-type $wasm-external-type-type
           (lambda (value)
             (or (natural? value) (wasm-table-type? value) (wasm-memory-type? value)
                 (wasm-global-type? value) (wasm-tag-type? value)))]))

;;;;===----------------------------------------------------------------------===
;;;; Entities and segments
;;;;===----------------------------------------------------------------------===

  #|proc:make-wasm-import
  Creates and returns a `wasm-import` record. `module` is the source module-name string. `name` is
  the imported item-name string. `external-type` is a `wasm-external-type` record describing the
  imported entity.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-import?
  Returns whether `object` is an import. `object` is the value to test.
  |#
  #|proc:wasm-import-module
  Returns the stored `module` of `wasm-import` record `record`. It is the source module-name
  string. Mutable contents are returned by reference.
  |#
  #|proc:wasm-import-name
  Returns the stored `name` of `wasm-import` record `record`. It is the imported item-name string.
  Mutable contents are returned by reference.
  |#
  #|proc:wasm-import-external-type
  Returns the stored `external-type` of `wasm-import` record `record`. It is a
  `wasm-external-type` record describing the imported entity.
  |#
  (define-checked-record-type $wasm-import make-wasm-import wasm-import? wasm-import
    $make-wasm-import $wasm-import?
    ([module wasm-import-module $wasm-import-module string?]
     [name wasm-import-name $wasm-import-name string?]
     [external-type wasm-import-external-type $wasm-import-external-type wasm-external-type?]))

  #|proc:make-wasm-function
  Creates and returns a `wasm-function` record. `type-index` is a natural zero-based function type
  index. `locals` is a vector of nonparameter local value types in local-index order. `body` is a
  vector of `wasm-instruction` records in execution order.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-function?
  Returns whether `object` is a function. `object` is the value to test.
  |#
  #|proc:wasm-function-type-index
  Returns the stored `type-index` of `wasm-function` record `record`. It is a natural zero-based
  function type index.
  |#
  #|proc:wasm-function-locals
  Returns the stored `locals` of `wasm-function` record `record`. It is a vector of nonparameter
  local value types in local-index order. Mutable contents are returned by reference.
  |#
  #|proc:wasm-function-body
  Returns the stored `body` of `wasm-function` record `record`. It is a vector of
  `wasm-instruction` records in execution order. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-function make-wasm-function wasm-function? wasm-function
    $make-wasm-function $wasm-function?
    ([type-index wasm-function-type-index $wasm-function-type-index natural?]
     [locals wasm-function-locals $wasm-function-locals (vector-of? value-type?)]
     [body wasm-function-body $wasm-function-body (vector-of? wasm-instruction?)]))

  #|proc:make-wasm-table
  Creates and returns a `wasm-table` record. `type` is a `wasm-table-type` record. `initializer`
  is an instruction-expression vector, or `#f` when no explicit initializer is present.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-table?
  Returns whether `object` is a table. `object` is the value to test.
  |#
  #|proc:wasm-table-type
  Returns the stored `type` of `wasm-table` record `record`. It is a `wasm-table-type` record.
  |#
  #|proc:wasm-table-initializer
  Returns the stored `initializer` of `wasm-table` record `record`. It is an
  instruction-expression vector, or `#f` when no explicit initializer is present. Mutable contents
  are returned by reference.
  |#
  (define-checked-record-type $wasm-table make-wasm-table wasm-table? wasm-table
    $make-wasm-table $wasm-table?
    ([type wasm-table-type $wasm-table-entity-type wasm-table-type?]
     [initializer wasm-table-initializer $wasm-table-initializer
                  optional-instruction-vector?]))

  #|proc:make-wasm-memory
  Creates and returns a `wasm-memory` record. `type` is a `wasm-memory-type` record.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-memory?
  Returns whether `object` is a memory. `object` is the value to test.
  |#
  #|proc:wasm-memory-type
  Returns the stored `type` of `wasm-memory` record `record`. It is a `wasm-memory-type` record.
  |#
  (define-checked-record-type $wasm-memory make-wasm-memory wasm-memory? wasm-memory
    $make-wasm-memory $wasm-memory?
    ([type wasm-memory-type $wasm-memory-entity-type wasm-memory-type?]))

  #|proc:make-wasm-global
  Creates and returns a `wasm-global` record. `type` is a `wasm-global-type` record. `initializer`
  is a vector of `wasm-instruction` records for the initialization expression.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-global?
  Returns whether `object` is a global. `object` is the value to test.
  |#
  #|proc:wasm-global-type
  Returns the stored `type` of `wasm-global` record `record`. It is a `wasm-global-type` record.
  |#
  #|proc:wasm-global-initializer
  Returns the stored `initializer` of `wasm-global` record `record`. It is a vector of
  `wasm-instruction` records for the initialization expression. Mutable contents are returned by
  reference.
  |#
  (define-checked-record-type $wasm-global make-wasm-global wasm-global? wasm-global
    $make-wasm-global $wasm-global?
    ([type wasm-global-type $wasm-global-entity-type wasm-global-type?]
     [initializer wasm-global-initializer $wasm-global-initializer instruction-vector?]))

  #|proc:make-wasm-tag
  Creates and returns a `wasm-tag` record. `type` is a `wasm-tag-type` record.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-tag?
  Returns whether `object` is a tag. `object` is the value to test.
  |#
  #|proc:wasm-tag-type
  Returns the stored `type` of `wasm-tag` record `record`. It is a `wasm-tag-type` record.
  |#
  (define-checked-record-type $wasm-tag make-wasm-tag wasm-tag? wasm-tag
    $make-wasm-tag $wasm-tag?
    ([type wasm-tag-type $wasm-tag-entity-type wasm-tag-type?]))

  #|proc:make-wasm-export
  Creates and returns a `wasm-export` record. `name` is the external export-name string. `kind` is
  the namespace symbol `function`, `table`, `memory`, `global`, or `tag`. `index` is a natural
  zero-based entity index in that namespace.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-export?
  Returns whether `object` is an export. `object` is the value to test.
  |#
  #|proc:wasm-export-name
  Returns the stored `name` of `wasm-export` record `record`. It is the external export-name
  string. Mutable contents are returned by reference.
  |#
  #|proc:wasm-export-kind
  Returns the stored `kind` of `wasm-export` record `record`. It is the namespace symbol
  `function`, `table`, `memory`, `global`, or `tag`.
  |#
  #|proc:wasm-export-index
  Returns the stored `index` of `wasm-export` record `record`. It is a natural zero-based entity
  index in that namespace.
  |#
  (define-checked-record-type $wasm-export make-wasm-export wasm-export? wasm-export
    $make-wasm-export $wasm-export?
    ([name wasm-export-name $wasm-export-name string?]
     [kind wasm-export-kind $wasm-export-kind external-kind?]
     [index wasm-export-index $wasm-export-index natural?]))

  #|proc:make-wasm-element
  Creates and returns a `wasm-element` record. `mode` is the symbol `active`, `passive`, or
  `declarative`. `reference-type` is a `wasm-reference-type` record for the segment elements.
  `table-index` is a natural zero-based table index for active mode, or `#f` otherwise. `offset`
  is a `wasm-instruction` vector locating an active segment, or `#f` otherwise. `initializers` is
  a vector of expression vectors, each containing `wasm-instruction` records producing one
  reference.
  The constructor requires both placement fields for active mode and `#f` for both in
  passive/declarative mode.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-element?
  Returns whether `object` is an element segment. `object` is the value to test.
  |#
  #|proc:wasm-element-mode
  Returns the stored `mode` of `wasm-element` record `record`. It is the symbol `active`,
  `passive`, or `declarative`.
  |#
  #|proc:wasm-element-reference-type
  Returns the stored `reference-type` of `wasm-element` record `record`. It is a
  `wasm-reference-type` record for the segment elements.
  |#
  #|proc:wasm-element-table-index
  Returns the stored `table-index` of `wasm-element` record `record`. It is a natural zero-based
  table index for active mode, or `#f` otherwise.
  |#
  #|proc:wasm-element-offset
  Returns the stored `offset` of `wasm-element` record `record`. It is a `wasm-instruction` vector
  locating an active segment, or `#f` otherwise. Mutable contents are returned by reference.
  |#
  #|proc:wasm-element-initializers
  Returns the stored `initializers` of `wasm-element` record `record`. It is a vector of
  expression vectors, each containing `wasm-instruction` records producing one reference. Mutable
  contents are returned by reference.
  |#
  (define-checked-record-type $wasm-element make-wasm-element wasm-element? wasm-element
    $make-wasm-element $wasm-element?
    element-fields?
    ([mode wasm-element-mode $wasm-element-mode element-mode?]
     [reference-type wasm-element-reference-type $wasm-element-reference-type
                     wasm-reference-type?]
     [table-index wasm-element-table-index $wasm-element-table-index optional-natural?]
     [offset wasm-element-offset $wasm-element-offset optional-instruction-vector?]
     [initializers wasm-element-initializers $wasm-element-initializers
                   (vector-of? instruction-vector?)]))

  #|proc:make-wasm-data
  Creates and returns a `wasm-data` record. `mode` is the symbol `active` or `passive`.
  `memory-index` is a natural zero-based memory index for active mode, or `#f` for passive mode.
  `offset` is a `wasm-instruction` vector locating an active segment, or `#f` for passive mode.
  `bytes` is a bytevector of segment payload bytes.
  The constructor requires both placement fields for active mode and `#f` for both in passive
  mode.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-data?
  Returns whether `object` is a data segment. `object` is the value to test.
  |#
  #|proc:wasm-data-mode
  Returns the stored `mode` of `wasm-data` record `record`. It is the symbol `active` or
  `passive`.
  |#
  #|proc:wasm-data-memory-index
  Returns the stored `memory-index` of `wasm-data` record `record`. It is a natural zero-based
  memory index for active mode, or `#f` for passive mode.
  |#
  #|proc:wasm-data-offset
  Returns the stored `offset` of `wasm-data` record `record`. It is a `wasm-instruction` vector
  locating an active segment, or `#f` for passive mode. Mutable contents are returned by
  reference.
  |#
  #|proc:wasm-data-bytes
  Returns the stored `bytes` of `wasm-data` record `record`. It is a bytevector of segment payload
  bytes. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-data make-wasm-data wasm-data? wasm-data
    $make-wasm-data $wasm-data?
    data-fields?
    ([mode wasm-data-mode $wasm-data-mode data-mode?]
     [memory-index wasm-data-memory-index $wasm-data-memory-index optional-natural?]
     [offset wasm-data-offset $wasm-data-offset optional-instruction-vector?]
     [bytes wasm-data-bytes $wasm-data-bytes bytevector?]))

;;;;===----------------------------------------------------------------------===
;;;; Instructions and immediates
;;;;===----------------------------------------------------------------------===

  #|proc:make-wasm-instruction
  Creates and returns a `wasm-instruction` record. `mnemonic` is the instruction-name symbol.
  `immediates` is an opcode-specific vector of immediate values. `body` is a vector of nested
  `wasm-instruction` records, empty for simple instructions. `alternate` is a vector of
  `wasm-instruction` records for an else body or `wasm-catch` records for try-table handlers,
  empty when absent.
  Parsers use natural indexes, exact integer constants, `wasm-float` constants,
  `wasm-memory-argument`/`wasm-block-type`/reference-type records, abstract heap symbols, and
  vectors/bytevectors as required by the opcode. The constructor accepts any symbol and any
  immediate vector; it does not check their opcode relationship.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-instruction?
  Returns whether `object` is an instruction. `object` is the value to test.
  |#
  #|proc:wasm-instruction-mnemonic
  Returns the stored `mnemonic` of `wasm-instruction` record `record`. It is the instruction-name
  symbol.
  |#
  #|proc:wasm-instruction-immediates
  Returns the stored `immediates` of `wasm-instruction` record `record`. It is an opcode-specific
  vector of immediate values. Mutable contents are returned by reference.
  |#
  #|proc:wasm-instruction-body
  Returns the stored `body` of `wasm-instruction` record `record`. It is a vector of nested
  `wasm-instruction` records, empty for simple instructions. Mutable contents are returned by
  reference.
  |#
  #|proc:wasm-instruction-alternate
  Returns the stored `alternate` of `wasm-instruction` record `record`. It is a vector of
  `wasm-instruction` records for an else body or `wasm-catch` records for try-table handlers,
  empty when absent. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $wasm-instruction
    make-wasm-instruction wasm-instruction? wasm-instruction
    $make-wasm-instruction $wasm-instruction?
    ([mnemonic wasm-instruction-mnemonic $wasm-instruction-mnemonic symbol?]
     [immediates wasm-instruction-immediates $wasm-instruction-immediates vector?]
     [body wasm-instruction-body $wasm-instruction-body instruction-vector?]
     [alternate wasm-instruction-alternate $wasm-instruction-alternate
                instruction-alternate?]))

  #|proc:make-wasm-memory-argument
  Creates and returns a `wasm-memory-argument` record. `alignment` is a natural base-two exponent
  encoding byte alignment. `offset` is a natural static address offset in bytes. `memory-index` is
  a natural zero-based memory index.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-memory-argument?
  Returns whether `object` is a memory argument. `object` is the value to test.
  |#
  #|proc:wasm-memory-argument-alignment
  Returns the stored `alignment` of `wasm-memory-argument` record `record`. It is a natural
  base-two exponent encoding byte alignment.
  |#
  #|proc:wasm-memory-argument-offset
  Returns the stored `offset` of `wasm-memory-argument` record `record`. It is a natural static
  address offset in bytes.
  |#
  #|proc:wasm-memory-argument-memory-index
  Returns the stored `memory-index` of `wasm-memory-argument` record `record`. It is a natural
  zero-based memory index.
  |#
  (define-checked-record-type $wasm-memory-argument
    make-wasm-memory-argument wasm-memory-argument? wasm-memory-argument
    $make-wasm-memory-argument $wasm-memory-argument?
    ([alignment wasm-memory-argument-alignment $wasm-memory-argument-alignment natural?]
     [offset wasm-memory-argument-offset $wasm-memory-argument-offset natural?]
     [memory-index wasm-memory-argument-memory-index $wasm-memory-argument-memory-index natural?]))

  #|proc:make-wasm-block-type
  Creates and returns a `wasm-block-type` record. `kind` is the symbol `empty`, `value-type`, or
  `type-index`. `value` is `#f` for `empty`, a value type for `value-type`, or a natural function
  type index for `type-index`.
  The constructor checks the relationship between `kind` and `value`.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-block-type?
  Returns whether `object` is a block type. `object` is the value to test.
  |#
  #|proc:wasm-block-type-kind
  Returns the stored `kind` of `wasm-block-type` record `record`. It is the symbol `empty`,
  `value-type`, or `type-index`.
  |#
  #|proc:wasm-block-type-value
  Returns the stored `value` of `wasm-block-type` record `record`. It is `#f` for `empty`, a value
  type for `value-type`, or a natural function type index for `type-index`.
  |#
  (define-checked-record-type $wasm-block-type make-wasm-block-type wasm-block-type? wasm-block-type
    $make-wasm-block-type $wasm-block-type?
    block-type-fields?
    ([kind wasm-block-type-kind $wasm-block-type-kind block-kind?]
     [value wasm-block-type-value $wasm-block-type-value
            (lambda (value) (or (not value) (natural? value) (value-type? value)))]))

  #|proc:make-wasm-catch
  Creates and returns a `wasm-catch` record. `kind` is the symbol `catch`, `catch-ref`,
  `catch-all`, or `catch-all-ref`. `tag-index` is a natural zero-based tag index for
  `catch`/`catch-ref`, or `#f` for `catch-all`/`catch-all-ref`. `label-index` is a natural
  relative label depth, with zero selecting the innermost target.
  The constructor checks which kinds require a tag index or `#f`.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-catch?
  Returns whether `object` is a catch clause. `object` is the value to test.
  |#
  #|proc:wasm-catch-kind
  Returns the stored `kind` of `wasm-catch` record `record`. It is the symbol `catch`,
  `catch-ref`, `catch-all`, or `catch-all-ref`.
  |#
  #|proc:wasm-catch-tag-index
  Returns the stored `tag-index` of `wasm-catch` record `record`. It is a natural zero-based tag
  index for `catch`/`catch-ref`, or `#f` for `catch-all`/`catch-all-ref`.
  |#
  #|proc:wasm-catch-label-index
  Returns the stored `label-index` of `wasm-catch` record `record`. It is a natural relative label
  depth, with zero selecting the innermost target.
  |#
  (define-checked-record-type $wasm-catch make-wasm-catch wasm-catch? wasm-catch
    $make-wasm-catch $wasm-catch?
    catch-fields?
    ([kind wasm-catch-kind $wasm-catch-kind catch-kind?]
     [tag-index wasm-catch-tag-index $wasm-catch-tag-index optional-natural?]
     [label-index wasm-catch-label-index $wasm-catch-label-index natural?]))

  #|proc:make-wasm-float
  Creates and returns a `wasm-float` record. `width` is the exact format width `32` or `64` in
  bits. `bits` is a natural unsigned IEEE-754 bit pattern fitting `width` bits, including NaN
  payloads and signed zero.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:wasm-float?
  Returns whether `object` is a floating constant. `object` is the value to test.
  |#
  #|proc:wasm-float-width
  Returns the stored `width` of `wasm-float` record `record`. It is the exact format width `32` or
  `64` in bits.
  |#
  #|proc:wasm-float-bits
  Returns the stored `bits` of `wasm-float` record `record`. It is a natural unsigned IEEE-754 bit
  pattern fitting `width` bits, including NaN payloads and signed zero.
  |#
  (define-checked-record-type $wasm-float make-wasm-float wasm-float? wasm-float
    $make-wasm-float $wasm-float?
    float-fields?
    ([width wasm-float-width $wasm-float-width float-width?]
     [bits wasm-float-bits $wasm-float-bits natural?]))

  )
