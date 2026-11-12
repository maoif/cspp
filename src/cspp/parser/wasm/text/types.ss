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

  ;; These exports are internal cross-library APIs shared by the WAT grammar and normalizer.
  ;; Generated constructors retain arguments without validation or copying; immutable fields do
  ;; not make their strings/vectors/bytevectors immutable. The contracts below describe grammar
  ;; output, not guarantees for arbitrary constructor arguments. Accessors return stored values.
  ;; All pos fields are zero-based character offsets in decoded text, not UTF-8 byte offsets.
  ;; Type parsers return canonical WASM records when names/resolution are unnecessary, and syntax
  ;; records otherwise. Value/storage types may also be symbols, and indexes are positioned source
  ;; references. Normalization resolves bindings, expands abbreviations, and produces canonical
  ;; WASM records; intermediate syntax alone is neither a canonical module nor a validation result.

  #|proc:make-wat-index-reference
  Creates and returns internal `wat-index-reference` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `value` is a u32 numeric index or
  decoded identifier string including its leading `$`.
  |#
  #|proc:wat-index-reference?
  Returns `#t` if `object` is an internal `wat-index-reference` record, and `#f` otherwise.
  |#
  #|proc:wat-index-reference-pos
  Returns the stored `pos` of internal `wat-index-reference` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-index-reference-value
  Returns the stored `value` of internal `wat-index-reference` record `record`. It is a u32
  numeric index or decoded identifier string including its leading `$`. Mutable contents are
  returned by reference.
  |#
  (define-record-type ($wat-index-reference make-wat-index-reference wat-index-reference?)
    (fields (immutable pos wat-index-reference-pos)
            (immutable value wat-index-reference-value)))

  #|proc:make-wat-binding
  Creates and returns internal `wat-binding` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `id` is a decoded identifier string
  including its leading `$`, or `#f` if anonymous. `type` is a value type: `i32`, `i64`, `f32`,
  `f64`, `v128`, a `wasm-reference-type`, or `wat-reference-type-syntax` for parameters/locals; a
  `wasm-field-type` or `wat-field-type-syntax` for struct fields.
  |#
  #|proc:wat-binding?
  Returns `#t` if `object` is an internal `wat-binding` record, and `#f` otherwise.
  |#
  #|proc:wat-binding-pos
  Returns the stored `pos` of internal `wat-binding` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-binding-id
  Returns the stored `id` of internal `wat-binding` record `record`. It is a decoded identifier
  string including its leading `$`, or `#f` if anonymous. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-binding-type
  Returns the stored `type` of internal `wat-binding` record `record`. It is a value type: `i32`,
  `i64`, `f32`, `f64`, `v128`, a `wasm-reference-type`, or `wat-reference-type-syntax` for
  parameters/locals; a `wasm-field-type` or `wat-field-type-syntax` for struct fields.
  |#
  (define-record-type ($wat-binding make-wat-binding wat-binding?)
    (fields (immutable pos wat-binding-pos)
            (immutable id wat-binding-id)
            (immutable type wat-binding-type)))

;;;;===----------------------------------------------------------------------===
;;;; Intermediate value and composite types
;;;;===----------------------------------------------------------------------===

  #|proc:make-wat-reference-type-syntax
  Creates and returns internal `wat-reference-type-syntax` syntax from the supplied fields. `pos`
  is the zero-based character offset in the decoded source string. `nullable?` is a boolean
  allowing null values. `heap-type` is a positioned `wat-index-reference` to a concrete type.
  |#
  #|proc:wat-reference-type-syntax?
  Returns `#t` if `object` is an internal `wat-reference-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-reference-type-syntax-pos
  Returns the stored `pos` of internal `wat-reference-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-reference-type-syntax-nullable?
  Returns the stored `nullable?` of internal `wat-reference-type-syntax` record `record`. It is a
  boolean allowing null values.
  |#
  #|proc:wat-reference-type-syntax-heap-type
  Returns the stored `heap-type` of internal `wat-reference-type-syntax` record `record`. It is a
  positioned `wat-index-reference` to a concrete type.
  |#
  (define-record-type
    ($wat-reference-type-syntax make-wat-reference-type-syntax
                                wat-reference-type-syntax?)
    (fields (immutable pos wat-reference-type-syntax-pos)
            (immutable nullable? wat-reference-type-syntax-nullable?)
            (immutable heap-type wat-reference-type-syntax-heap-type)))

  #|proc:make-wat-field-type-syntax
  Creates and returns internal `wat-field-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `storage-type` is a value type:
  `i32`, `i64`, `f32`, `f64`, `v128`, a `wasm-reference-type`, or `wat-reference-type-syntax`, or
  packed storage symbol `i8` or `i16`. `mutable?` is a boolean allowing field mutation.
  |#
  #|proc:wat-field-type-syntax?
  Returns `#t` if `object` is an internal `wat-field-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-field-type-syntax-pos
  Returns the stored `pos` of internal `wat-field-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-field-type-syntax-storage-type
  Returns the stored `storage-type` of internal `wat-field-type-syntax` record `record`. It is a
  value type: `i32`, `i64`, `f32`, `f64`, `v128`, a `wasm-reference-type`, or
  `wat-reference-type-syntax`, or packed storage symbol `i8` or `i16`.
  |#
  #|proc:wat-field-type-syntax-mutable?
  Returns the stored `mutable?` of internal `wat-field-type-syntax` record `record`. It is a
  boolean allowing field mutation.
  |#
  (define-record-type ($wat-field-type-syntax make-wat-field-type-syntax
                                              wat-field-type-syntax?)
    (fields (immutable pos wat-field-type-syntax-pos)
            (immutable storage-type wat-field-type-syntax-storage-type)
            (immutable mutable? wat-field-type-syntax-mutable?)))

  #|proc:make-wat-function-type-syntax
  Creates and returns internal `wat-function-type-syntax` syntax from the supplied fields. `pos`
  is the zero-based character offset in the decoded source string. `parameters` is a vector of
  `wat-binding` parameter records in declaration order. `results` is a vector of value types as
  described for `wat-binding-type`.
  |#
  #|proc:wat-function-type-syntax?
  Returns `#t` if `object` is an internal `wat-function-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-function-type-syntax-pos
  Returns the stored `pos` of internal `wat-function-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-function-type-syntax-parameters
  Returns the stored `parameters` of internal `wat-function-type-syntax` record `record`. It is a
  vector of `wat-binding` parameter records in declaration order. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-function-type-syntax-results
  Returns the stored `results` of internal `wat-function-type-syntax` record `record`. It is a
  vector of value types as described for `wat-binding-type`. Mutable contents are returned by
  reference.
  |#
  (define-record-type ($wat-function-type-syntax make-wat-function-type-syntax
                                                 wat-function-type-syntax?)
    (fields (immutable pos wat-function-type-syntax-pos)
            (immutable parameters wat-function-type-syntax-parameters)
            (immutable results wat-function-type-syntax-results)))

  #|proc:make-wat-struct-type-syntax
  Creates and returns internal `wat-struct-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `fields` is a vector of
  `wat-binding` field records in declaration order.
  |#
  #|proc:wat-struct-type-syntax?
  Returns `#t` if `object` is an internal `wat-struct-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-struct-type-syntax-pos
  Returns the stored `pos` of internal `wat-struct-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-struct-type-syntax-fields
  Returns the stored `fields` of internal `wat-struct-type-syntax` record `record`. It is a vector
  of `wat-binding` field records in declaration order. Mutable contents are returned by reference.
  |#
  (define-record-type ($wat-struct-type-syntax make-wat-struct-type-syntax
                                               wat-struct-type-syntax?)
    (fields (immutable pos wat-struct-type-syntax-pos)
            (immutable fields wat-struct-type-syntax-fields)))

  #|proc:make-wat-array-type-syntax
  Creates and returns internal `wat-array-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `field` is a `wasm-field-type` or
  `wat-field-type-syntax`.
  |#
  #|proc:wat-array-type-syntax?
  Returns `#t` if `object` is an internal `wat-array-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-array-type-syntax-pos
  Returns the stored `pos` of internal `wat-array-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-array-type-syntax-field
  Returns the stored `field` of internal `wat-array-type-syntax` record `record`. It is a
  `wasm-field-type` or `wat-field-type-syntax`.
  |#
  (define-record-type ($wat-array-type-syntax make-wat-array-type-syntax
                                              wat-array-type-syntax?)
    (fields (immutable pos wat-array-type-syntax-pos)
            (immutable field wat-array-type-syntax-field)))

  #|proc:make-wat-subtype-syntax
  Creates and returns internal `wat-subtype-syntax` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `id` is a decoded identifier string
  including its leading `$`, or `#f` if anonymous. `final?` is a boolean forbidding subtyping.
  `supertypes` is a vector of `wat-index-reference` type references. `composite-type` is a
  canonical WASM function/struct/array type or its corresponding WAT syntax record.
  |#
  #|proc:wat-subtype-syntax?
  Returns `#t` if `object` is an internal `wat-subtype-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-subtype-syntax-pos
  Returns the stored `pos` of internal `wat-subtype-syntax` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-subtype-syntax-id
  Returns the stored `id` of internal `wat-subtype-syntax` record `record`. It is a decoded
  identifier string including its leading `$`, or `#f` if anonymous. Mutable contents are returned
  by reference.
  |#
  #|proc:wat-subtype-syntax-final?
  Returns the stored `final?` of internal `wat-subtype-syntax` record `record`. It is a boolean
  forbidding subtyping.
  |#
  #|proc:wat-subtype-syntax-supertypes
  Returns the stored `supertypes` of internal `wat-subtype-syntax` record `record`. It is a vector
  of `wat-index-reference` type references. Mutable contents are returned by reference.
  |#
  #|proc:wat-subtype-syntax-composite-type
  Returns the stored `composite-type` of internal `wat-subtype-syntax` record `record`. It is a
  canonical WASM function/struct/array type or its corresponding WAT syntax record.
  |#
  (define-record-type ($wat-subtype-syntax make-wat-subtype-syntax wat-subtype-syntax?)
    (fields (immutable pos wat-subtype-syntax-pos)
            (immutable id wat-subtype-syntax-id)
            (immutable final? wat-subtype-syntax-final?)
            (immutable supertypes wat-subtype-syntax-supertypes)
            (immutable composite-type wat-subtype-syntax-composite-type)))

  #|proc:make-wat-recursive-type-syntax
  Creates and returns internal `wat-recursive-type-syntax` syntax from the supplied fields. `pos`
  is the zero-based character offset in the decoded source string. `subtypes` is a vector of
  `wat-subtype-syntax` records in declaration order.
  |#
  #|proc:wat-recursive-type-syntax?
  Returns `#t` if `object` is an internal `wat-recursive-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-recursive-type-syntax-pos
  Returns the stored `pos` of internal `wat-recursive-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-recursive-type-syntax-subtypes
  Returns the stored `subtypes` of internal `wat-recursive-type-syntax` record `record`. It is a
  vector of `wat-subtype-syntax` records in declaration order. Mutable contents are returned by
  reference.
  |#
  (define-record-type
    ($wat-recursive-type-syntax make-wat-recursive-type-syntax
                                wat-recursive-type-syntax?)
    (fields (immutable pos wat-recursive-type-syntax-pos)
            (immutable subtypes wat-recursive-type-syntax-subtypes)))

;;;;===----------------------------------------------------------------------===
;;;; Intermediate signatures and entity types
;;;;===----------------------------------------------------------------------===

  #|proc:make-wat-type-use
  Creates and returns internal `wat-type-use` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `index` is a `wat-index-reference` to
  an explicit type, or `#f` if omitted. `parameters` is a vector of `wat-binding` parameter
  records. `results` is a vector of value types as described for `wat-binding-type`.
  |#
  #|proc:wat-type-use?
  Returns `#t` if `object` is an internal `wat-type-use` record, and `#f` otherwise.
  |#
  #|proc:wat-type-use-pos
  Returns the stored `pos` of internal `wat-type-use` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-type-use-index
  Returns the stored `index` of internal `wat-type-use` record `record`. It is a
  `wat-index-reference` to an explicit type, or `#f` if omitted.
  |#
  #|proc:wat-type-use-parameters
  Returns the stored `parameters` of internal `wat-type-use` record `record`. It is a vector of
  `wat-binding` parameter records. Mutable contents are returned by reference.
  |#
  #|proc:wat-type-use-results
  Returns the stored `results` of internal `wat-type-use` record `record`. It is a vector of value
  types as described for `wat-binding-type`. Mutable contents are returned by reference.
  |#
  (define-record-type ($wat-type-use make-wat-type-use wat-type-use?)
    (fields (immutable pos wat-type-use-pos)
            (immutable index wat-type-use-index)
            (immutable parameters wat-type-use-parameters)
            (immutable results wat-type-use-results)))

  #|proc:make-wat-block-type-syntax
  Creates and returns internal `wat-block-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `type-use` is a `wat-type-use`
  record for the block signature.
  |#
  #|proc:wat-block-type-syntax?
  Returns `#t` if `object` is an internal `wat-block-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-block-type-syntax-pos
  Returns the stored `pos` of internal `wat-block-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-block-type-syntax-type-use
  Returns the stored `type-use` of internal `wat-block-type-syntax` record `record`. It is a
  `wat-type-use` record for the block signature.
  |#
  (define-record-type ($wat-block-type-syntax make-wat-block-type-syntax
                                              wat-block-type-syntax?)
    (fields (immutable pos wat-block-type-syntax-pos)
            (immutable type-use wat-block-type-syntax-type-use)))

  #|proc:make-wat-global-type-syntax
  Creates and returns internal `wat-global-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `value-type` is a value type:
  `i32`, `i64`, `f32`, `f64`, `v128`, a `wasm-reference-type`, or `wat-reference-type-syntax`.
  `mutable?` is a boolean allowing global mutation.
  |#
  #|proc:wat-global-type-syntax?
  Returns `#t` if `object` is an internal `wat-global-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-global-type-syntax-pos
  Returns the stored `pos` of internal `wat-global-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-global-type-syntax-value-type
  Returns the stored `value-type` of internal `wat-global-type-syntax` record `record`. It is a
  value type: `i32`, `i64`, `f32`, `f64`, `v128`, a `wasm-reference-type`, or
  `wat-reference-type-syntax`.
  |#
  #|proc:wat-global-type-syntax-mutable?
  Returns the stored `mutable?` of internal `wat-global-type-syntax` record `record`. It is a
  boolean allowing global mutation.
  |#
  (define-record-type ($wat-global-type-syntax make-wat-global-type-syntax
                                               wat-global-type-syntax?)
    (fields (immutable pos wat-global-type-syntax-pos)
            (immutable value-type wat-global-type-syntax-value-type)
            (immutable mutable? wat-global-type-syntax-mutable?)))

  #|proc:make-wat-table-type-syntax
  Creates and returns internal `wat-table-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `limits` is a nonshared canonical
  `wasm-limits` record. `reference-type` is a `wasm-reference-type` or
  `wat-reference-type-syntax`.
  |#
  #|proc:wat-table-type-syntax?
  Returns `#t` if `object` is an internal `wat-table-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-table-type-syntax-pos
  Returns the stored `pos` of internal `wat-table-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-table-type-syntax-limits
  Returns the stored `limits` of internal `wat-table-type-syntax` record `record`. It is a
  nonshared canonical `wasm-limits` record.
  |#
  #|proc:wat-table-type-syntax-reference-type
  Returns the stored `reference-type` of internal `wat-table-type-syntax` record `record`. It is a
  `wasm-reference-type` or `wat-reference-type-syntax`.
  |#
  (define-record-type ($wat-table-type-syntax make-wat-table-type-syntax
                                              wat-table-type-syntax?)
    (fields (immutable pos wat-table-type-syntax-pos)
            (immutable limits wat-table-type-syntax-limits)
            (immutable reference-type wat-table-type-syntax-reference-type)))

  #|proc:make-wat-limits-syntax
  Creates and returns internal `wat-limits-syntax` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `address-type` is the symbol `i32` or
  `i64`. `minimum` is a u64 lower bound in table elements or 64 KiB memory pages. `maximum` is a
  u64 upper bound in the same units, or `#f` if absent. `shared?` is a boolean; the grammar
  produces this syntax form only for shared limits.
  The grammar checks that a supplied maximum is at least the minimum and shared limits have a
  maximum. Canonical nonshared limits may be returned directly by the type parser.
  |#
  #|proc:wat-limits-syntax?
  Returns `#t` if `object` is an internal `wat-limits-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-limits-syntax-pos
  Returns the stored `pos` of internal `wat-limits-syntax` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-limits-syntax-address-type
  Returns the stored `address-type` of internal `wat-limits-syntax` record `record`. It is the
  symbol `i32` or `i64`.
  |#
  #|proc:wat-limits-syntax-minimum
  Returns the stored `minimum` of internal `wat-limits-syntax` record `record`. It is a u64 lower
  bound in table elements or 64 KiB memory pages.
  |#
  #|proc:wat-limits-syntax-maximum
  Returns the stored `maximum` of internal `wat-limits-syntax` record `record`. It is a u64 upper
  bound in the same units, or `#f` if absent.
  |#
  #|proc:wat-limits-syntax-shared?
  Returns the stored `shared?` of internal `wat-limits-syntax` record `record`. It is a boolean;
  the grammar produces this syntax form only for shared limits.
  |#
  (define-record-type ($wat-limits-syntax make-wat-limits-syntax wat-limits-syntax?)
    (fields (immutable pos wat-limits-syntax-pos)
            (immutable address-type wat-limits-syntax-address-type)
            (immutable minimum wat-limits-syntax-minimum)
            (immutable maximum wat-limits-syntax-maximum)
            (immutable shared? wat-limits-syntax-shared?)))

  #|proc:make-wat-memory-type-syntax
  Creates and returns internal `wat-memory-type-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `limits` is a `wat-limits-syntax`
  record for shared memory bounds.
  |#
  #|proc:wat-memory-type-syntax?
  Returns `#t` if `object` is an internal `wat-memory-type-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-memory-type-syntax-pos
  Returns the stored `pos` of internal `wat-memory-type-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-memory-type-syntax-limits
  Returns the stored `limits` of internal `wat-memory-type-syntax` record `record`. It is a
  `wat-limits-syntax` record for shared memory bounds.
  |#
  (define-record-type ($wat-memory-type-syntax make-wat-memory-type-syntax
                                               wat-memory-type-syntax?)
    (fields (immutable pos wat-memory-type-syntax-pos)
            (immutable limits wat-memory-type-syntax-limits)))

;;;;===----------------------------------------------------------------------===
;;;; Intermediate segments and custom placement
;;;;===----------------------------------------------------------------------===

  #|proc:make-wat-element-segment-syntax
  Creates and returns internal `wat-element-segment-syntax` syntax from the supplied fields. `pos`
  is the zero-based character offset in the decoded source string. `mode` is `active`, `passive`,
  `declarative`, or internal `abbreviation`. `table` is a `wat-index-reference`, or `#f` for the
  default active table or nonactive forms. `offset` is an instruction-syntax vector for active
  placement, or `#f` for other modes. `address-type` is `i32` or `i64` for a table abbreviation,
  or `#f` to infer/default it. `reference-type` is a `wasm-reference-type` or
  `wat-reference-type-syntax`. `item-kind` is the symbol `indexes` or `expressions`. `items` is a
  vector of `wat-index-reference` function references for `indexes`, or a vector of
  instruction-syntax expression vectors for `expressions`.
  |#
  #|proc:wat-element-segment-syntax?
  Returns `#t` if `object` is an internal `wat-element-segment-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-element-segment-syntax-pos
  Returns the stored `pos` of internal `wat-element-segment-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-element-segment-syntax-mode
  Returns the stored `mode` of internal `wat-element-segment-syntax` record `record`. It is
  `active`, `passive`, `declarative`, or internal `abbreviation`.
  |#
  #|proc:wat-element-segment-syntax-table
  Returns the stored `table` of internal `wat-element-segment-syntax` record `record`. It is a
  `wat-index-reference`, or `#f` for the default active table or nonactive forms.
  |#
  #|proc:wat-element-segment-syntax-offset
  Returns the stored `offset` of internal `wat-element-segment-syntax` record `record`. It is an
  instruction-syntax vector for active placement, or `#f` for other modes. Mutable contents are
  returned by reference.
  |#
  #|proc:wat-element-segment-syntax-address-type
  Returns the stored `address-type` of internal `wat-element-segment-syntax` record `record`. It
  is `i32` or `i64` for a table abbreviation, or `#f` to infer/default it.
  |#
  #|proc:wat-element-segment-syntax-reference-type
  Returns the stored `reference-type` of internal `wat-element-segment-syntax` record `record`. It
  is a `wasm-reference-type` or `wat-reference-type-syntax`.
  |#
  #|proc:wat-element-segment-syntax-item-kind
  Returns the stored `item-kind` of internal `wat-element-segment-syntax` record `record`. It is
  the symbol `indexes` or `expressions`.
  |#
  #|proc:wat-element-segment-syntax-items
  Returns the stored `items` of internal `wat-element-segment-syntax` record `record`. It is a
  vector of `wat-index-reference` function references for `indexes`, or a vector of
  instruction-syntax expression vectors for `expressions`. Mutable contents are returned by
  reference.
  |#
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

  #|proc:make-wat-data-segment-syntax
  Creates and returns internal `wat-data-segment-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `mode` is `active`, `passive`, or
  internal `abbreviation`. `memory` is a `wat-index-reference`, or `#f` for the default active
  memory or other modes. `offset` is an instruction-syntax vector for active placement, or `#f`
  for other modes. `address-type` is `i32` or `i64` for a memory abbreviation, or `#f` to
  infer/default it. `strings` is a vector of bytevectors decoded from WAT string literals,
  concatenated during normalization.
  |#
  #|proc:wat-data-segment-syntax?
  Returns `#t` if `object` is an internal `wat-data-segment-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-data-segment-syntax-pos
  Returns the stored `pos` of internal `wat-data-segment-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-data-segment-syntax-mode
  Returns the stored `mode` of internal `wat-data-segment-syntax` record `record`. It is `active`,
  `passive`, or internal `abbreviation`.
  |#
  #|proc:wat-data-segment-syntax-memory
  Returns the stored `memory` of internal `wat-data-segment-syntax` record `record`. It is a
  `wat-index-reference`, or `#f` for the default active memory or other modes.
  |#
  #|proc:wat-data-segment-syntax-offset
  Returns the stored `offset` of internal `wat-data-segment-syntax` record `record`. It is an
  instruction-syntax vector for active placement, or `#f` for other modes. Mutable contents are
  returned by reference.
  |#
  #|proc:wat-data-segment-syntax-address-type
  Returns the stored `address-type` of internal `wat-data-segment-syntax` record `record`. It is
  `i32` or `i64` for a memory abbreviation, or `#f` to infer/default it.
  |#
  #|proc:wat-data-segment-syntax-strings
  Returns the stored `strings` of internal `wat-data-segment-syntax` record `record`. It is a
  vector of bytevectors decoded from WAT string literals, concatenated during normalization.
  Mutable contents are returned by reference.
  |#
  (define-record-type
    ($wat-data-segment-syntax make-wat-data-segment-syntax wat-data-segment-syntax?)
    (fields (immutable pos wat-data-segment-syntax-pos)
            (immutable mode wat-data-segment-syntax-mode)
            (immutable memory wat-data-segment-syntax-memory)
            (immutable offset wat-data-segment-syntax-offset)
            (immutable address-type wat-data-segment-syntax-address-type)
            (immutable strings wat-data-segment-syntax-strings)))

  #|proc:make-wat-custom-placement
  Creates and returns internal `wat-custom-placement` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `before` is a standard-section
  symbol to precede, or `#f`. `after` is a standard-section symbol to follow, or `#f`; the grammar
  defaults to `data`.
  Section symbols are `type`, `import`, `function`, `table`, `memory`, `tag`, `global`, `export`,
  `start`, `element`, `data-count`, `code`, and `data`. The grammar sets exactly one anchor;
  normalization maps a before-anchor to canonical preceding-section placement.
  |#
  #|proc:wat-custom-placement?
  Returns `#t` if `object` is an internal `wat-custom-placement` record, and `#f` otherwise.
  |#
  #|proc:wat-custom-placement-pos
  Returns the stored `pos` of internal `wat-custom-placement` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-custom-placement-before
  Returns the stored `before` of internal `wat-custom-placement` record `record`. It is a
  standard-section symbol to precede, or `#f`.
  |#
  #|proc:wat-custom-placement-after
  Returns the stored `after` of internal `wat-custom-placement` record `record`. It is a
  standard-section symbol to follow, or `#f`; the grammar defaults to `data`.
  |#
  (define-record-type ($wat-custom-placement make-wat-custom-placement
                                             wat-custom-placement?)
    (fields (immutable pos wat-custom-placement-pos)
            (immutable before wat-custom-placement-before)
            (immutable after wat-custom-placement-after)))

;;;;===----------------------------------------------------------------------===
;;;; Intermediate modules and fields
;;;;===----------------------------------------------------------------------===

  #|proc:make-wat-module
  Creates and returns internal `wat-module` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `id` is a decoded identifier string
  including its leading `$`, or `#f` if anonymous. `fields` is a vector of `wat-module-field`
  records in source order.
  |#
  #|proc:wat-module?
  Returns `#t` if `object` is an internal `wat-module` record, and `#f` otherwise.
  |#
  #|proc:wat-module-pos
  Returns the stored `pos` of internal `wat-module` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-module-id
  Returns the stored `id` of internal `wat-module` record `record`. It is a decoded identifier
  string including its leading `$`, or `#f` if anonymous. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-module-fields
  Returns the stored `fields` of internal `wat-module` record `record`. It is a vector of
  `wat-module-field` records in source order. Mutable contents are returned by reference.
  |#
  (define-record-type ($wat-module make-wat-module wat-module?)
    (fields (immutable pos wat-module-pos)
            (immutable id wat-module-id)
            (immutable fields wat-module-fields)))

  #|proc:make-wat-module-field
  Creates and returns internal `wat-module-field` syntax from the supplied fields. `pos` is the
  zero-based character offset in the decoded source string. `kind` is `recursive-type`, `import`,
  `function`, `table`, `memory`, `global`, `tag`, `export`, `start`, `element`, `data`, or
  `custom`. `id` is a decoded identifier string including its leading `$`, or `#f` if anonymous.
  `data` is the kind-specific payload described below, or `#f` when held in `abbreviation`.
  `import` is a two-string vector of inline import module/item names, or `#f`. `exports` is a
  vector of inline export name strings in source order. `abbreviation` is a
  `wat-inline-abbreviation` for table/element/memory/data sugar, or `#f`.
  For `recursive-type`, the payload is `wat-recursive-type-syntax`. For `import`, it is a vector
  of module name, item name, external kind, and type/type-use. For `function`, it is a vector of
  type-use, local-binding vector, and instruction-syntax body vector. For `table`, it is a table
  type or a vector of table type and initializer expression; `memory` holds a memory type. For
  `global`, it is a vector of global type and initializer expression; `tag` holds a type-use. For
  `export`, it is a vector of name and a pair of namespace symbol (`func`, `table`, `memory`,
  `global`, or `tag`) and index reference. `start` holds an index reference. `custom` holds a
  vector of name string, payload-bytevector vector, and placement syntax.
  |#
  #|proc:wat-module-field?
  Returns `#t` if `object` is an internal `wat-module-field` record, and `#f` otherwise.
  |#
  #|proc:wat-module-field-pos
  Returns the stored `pos` of internal `wat-module-field` record `record`. It is the zero-based
  character offset in the decoded source string. Mutable contents are returned by reference.
  |#
  #|proc:wat-module-field-kind
  Returns the stored `kind` of internal `wat-module-field` record `record`. It is
  `recursive-type`, `import`, `function`, `table`, `memory`, `global`, `tag`, `export`, `start`,
  `element`, `data`, or `custom`.
  |#
  #|proc:wat-module-field-id
  Returns the stored `id` of internal `wat-module-field` record `record`. It is a decoded
  identifier string including its leading `$`, or `#f` if anonymous. Mutable contents are returned
  by reference.
  |#
  #|proc:wat-module-field-data
  Returns the stored `data` of internal `wat-module-field` record `record`. It is the
  kind-specific payload described below, or `#f` when held in `abbreviation`.
  |#
  #|proc:wat-module-field-import
  Returns the stored `import` of internal `wat-module-field` record `record`. It is a two-string
  vector of inline import module/item names, or `#f`. Mutable contents are returned by reference.
  |#
  #|proc:wat-module-field-exports
  Returns the stored `exports` of internal `wat-module-field` record `record`. It is a vector of
  inline export name strings in source order. Mutable contents are returned by reference.
  |#
  #|proc:wat-module-field-abbreviation
  Returns the stored `abbreviation` of internal `wat-module-field` record `record`. It is a
  `wat-inline-abbreviation` for table/element/memory/data sugar, or `#f`.
  |#
  (define-record-type ($wat-module-field make-wat-module-field wat-module-field?)
    (fields (immutable pos wat-module-field-pos)
            (immutable kind wat-module-field-kind)
            (immutable id wat-module-field-id)
            (immutable data wat-module-field-data)
            (immutable import wat-module-field-import)
            (immutable exports wat-module-field-exports)
            (immutable abbreviation wat-module-field-abbreviation)))

;;;;===----------------------------------------------------------------------===
;;;; Intermediate instructions and abbreviations
;;;;===----------------------------------------------------------------------===

  #|proc:make-wat-instruction-syntax
  Creates and returns internal `wat-instruction-syntax` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `mnemonic` is the canonical
  instruction-name symbol. `immediates` is an opcode-specific vector of literals, type syntax,
  source references, and nested immediate vectors. `body` is a vector of nested instruction-syntax
  records, empty for simple instructions. `alternate` is `#f` if absent, an instruction-syntax
  vector for `else`, or a `wat-catch-syntax` vector for `try_table`. `operands` is a vector of
  direct folded operand instruction nodes, empty for flat instructions. `label` is a decoded
  opening label string including `$`, or `#f`.
  Expression parsers flatten folded operands into execution order; `operands` retains direct
  operand nodes for that traversal. Normalization resolves immediate references and structured
  labels, and maps absent alternates to empty canonical vectors.
  |#
  #|proc:wat-instruction-syntax?
  Returns `#t` if `object` is an internal `wat-instruction-syntax` record, and `#f` otherwise.
  |#
  #|proc:wat-instruction-syntax-pos
  Returns the stored `pos` of internal `wat-instruction-syntax` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-instruction-syntax-mnemonic
  Returns the stored `mnemonic` of internal `wat-instruction-syntax` record `record`. It is the
  canonical instruction-name symbol.
  |#
  #|proc:wat-instruction-syntax-immediates
  Returns the stored `immediates` of internal `wat-instruction-syntax` record `record`. It is an
  opcode-specific vector of literals, type syntax, source references, and nested immediate
  vectors. Mutable contents are returned by reference.
  |#
  #|proc:wat-instruction-syntax-body
  Returns the stored `body` of internal `wat-instruction-syntax` record `record`. It is a vector
  of nested instruction-syntax records, empty for simple instructions. Mutable contents are
  returned by reference.
  |#
  #|proc:wat-instruction-syntax-alternate
  Returns the stored `alternate` of internal `wat-instruction-syntax` record `record`. It is `#f`
  if absent, an instruction-syntax vector for `else`, or a `wat-catch-syntax` vector for
  `try_table`. Mutable contents are returned by reference.
  |#
  #|proc:wat-instruction-syntax-operands
  Returns the stored `operands` of internal `wat-instruction-syntax` record `record`. It is a
  vector of direct folded operand instruction nodes, empty for flat instructions. Mutable contents
  are returned by reference.
  |#
  #|proc:wat-instruction-syntax-label
  Returns the stored `label` of internal `wat-instruction-syntax` record `record`. It is a decoded
  opening label string including `$`, or `#f`. Mutable contents are returned by reference.
  |#
  (define-record-type
    ($wat-instruction-syntax make-wat-instruction-syntax wat-instruction-syntax?)
    (fields (immutable pos wat-instruction-syntax-pos)
            (immutable mnemonic wat-instruction-syntax-mnemonic)
            (immutable immediates wat-instruction-syntax-immediates)
            (immutable body wat-instruction-syntax-body)
            (immutable alternate wat-instruction-syntax-alternate)
            (immutable operands wat-instruction-syntax-operands)
            (immutable label wat-instruction-syntax-label)))

  #|proc:make-wat-inline-abbreviation
  Creates and returns internal `wat-inline-abbreviation` syntax from the supplied fields. `pos` is
  the zero-based character offset in the decoded source string. `kind` is `table-element`,
  `memory-data`, `element`, or `data`. `data` is a two-element vector of table reference type and
  element-segment syntax for `table-element`; an element-segment syntax record for `element`; a
  data-segment syntax record for `memory-data` or `data`.
  |#
  #|proc:wat-inline-abbreviation?
  Returns `#t` if `object` is an internal `wat-inline-abbreviation` record, and `#f` otherwise.
  |#
  #|proc:wat-inline-abbreviation-pos
  Returns the stored `pos` of internal `wat-inline-abbreviation` record `record`. It is the
  zero-based character offset in the decoded source string. Mutable contents are returned by
  reference.
  |#
  #|proc:wat-inline-abbreviation-kind
  Returns the stored `kind` of internal `wat-inline-abbreviation` record `record`. It is
  `table-element`, `memory-data`, `element`, or `data`.
  |#
  #|proc:wat-inline-abbreviation-data
  Returns the stored `data` of internal `wat-inline-abbreviation` record `record`. It is a
  two-element vector of table reference type and element-segment syntax for `table-element`; an
  element-segment syntax record for `element`; a data-segment syntax record for `memory-data` or
  `data`. Mutable contents are returned by reference.
  |#
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

  (define canonical-value-type?
    (lambda (type)
      (or (memq type '(i32 i64 f32 f64 v128))
          (wasm-reference-type? type))))

  (define canonical-storage-type?
    (lambda (type)
      (or (memq type '(i8 i16)) (canonical-value-type? type))))

  (define all-canonical-bindings?
    (lambda (binding*)
      (for-all (lambda (binding)
                 (and (not (wat-binding-id binding))
                      (canonical-value-type? (wat-binding-type binding))))
               binding*)))

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
  Reads a u32 index or `$` identifier and returns positioned internal `wat-index-reference`
  syntax.
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
  Reads an abstract heap-type symbol or a concrete type reference. Returns the symbol or a
  positioned `wat-index-reference`; normalization resolves concrete references.
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
  Reads an explicit or shorthand reference type. Returns a canonical `wasm-reference-type` for
  abstract heap types, or `wat-reference-type-syntax` for a concrete type reference.
  |#
  (define <wat-reference-type>
    (</> explicit-reference-type shorthand-reference-type))

  #|proc:<wat-value-type>
  Reads a value type. Returns `i32`, `i64`, `f32`, `f64`, `v128`, a canonical
  `wasm-reference-type`, or internal `wat-reference-type-syntax` requiring normalization.
  |#
  (define <wat-value-type>
    (</> <wat-reference-type>
         (<as> 'i32 (<wat-keyword> "i32"))
         (<as> 'i64 (<wat-keyword> "i64"))
         (<as> 'f32 (<wat-keyword> "f32"))
         (<as> 'f64 (<wat-keyword> "f64"))
         (<as> 'v128 (<wat-keyword> "v128"))))

  #|proc:<wat-storage-type>
  Reads a storage type. Returns packed-storage symbol `i8` or `i16`, or any value returned by
  `<wat-value-type>`.
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
  Reads a mutable or immutable aggregate field. Returns `wasm-field-type` if its storage type is
  canonical, or internal `wat-field-type-syntax` if reference resolution is needed.
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
  Reads parameter and result clauses. Returns `wasm-function-type` for unnamed parameters and
  canonical value types; otherwise returns `wat-function-type-syntax` retaining bindings.
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
  Reads struct field clauses. Returns `wasm-struct-type` for unnamed fields with canonical field
  types; otherwise returns `wat-struct-type-syntax` retaining bindings.
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
  Reads an array element field. Returns `wasm-array-type` for a canonical field type, or
  `wat-array-type-syntax` when reference resolution is needed.
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
  Reads a function, struct, or array composite type. Returns the canonical WASM type record or
  corresponding WAT syntax record, as described by its component parser.
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
  Reads an explicit subtype or a shorthand final composite type. Returns internal
  `wat-subtype-syntax`, with a canonical or syntax composite type and positioned supertype
  references.
  |#
  (define <wat-subtype>
    (</> explicit-subtype
         (<map> (lambda (value)
                  (make-wat-subtype-syntax (car value) #f #t '#() (cadr value)))
                (<~> <pos> <wat-composite-type>))))

  #|proc:<wat-type-definition>
  Reads a named or anonymous subtype definition and returns internal `wat-subtype-syntax`; its
  `id` is the decoded `$` identifier or `#f`.
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
  Reads a recursive group of named or anonymous type definitions and returns internal
  `wat-recursive-type-syntax` with a vector of subtype syntax records.
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
  Reads an optional explicit type reference and inline function signature. Returns internal
  `wat-type-use` with parameter bindings, preserving names for function declarations.
  |#
  (define <wat-type-use> (type-use-parser parameter-clause))

  #|proc:<wat-anonymous-type-use>
  Reads an optional explicit type reference and an inline signature without parameter names.
  Returns internal `wat-type-use` for instruction signatures.
  |#
  (define <wat-anonymous-type-use> (type-use-parser unnamed-parameter-clause))

  #|proc:<wat-block-type>
  Reads an optional indexed or inline block signature and returns internal `wat-block-type-syntax`
  containing a `wat-type-use`, even for an empty signature.
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
  Reads an optional `i32`/`i64` address type (default `i32`), a u64 minimum, optional u64 maximum,
  and optional `shared`. Returns canonical `wasm-limits` when nonshared, or internal
  `wat-limits-syntax` when shared. Bounds use table-element or 64 KiB memory-page units.
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
  Reads a mutable or immutable global type. Returns canonical `wasm-global-type` if its value type
  is canonical, or internal `wat-global-type-syntax` requiring reference resolution.
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
  Reads nonshared table limits and a reference type. Returns canonical `wasm-table-type` for a
  canonical reference type, or internal `wat-table-type-syntax` for an unresolved reference.
  Shared table limits are rejected.
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
  Reads memory limits. Returns canonical `wasm-memory-type` for nonshared limits, or internal
  `wat-memory-type-syntax` for shared limits, normalized later to a canonical memory type.
  |#
  (define <wat-memory-type>
    (<map> (lambda (value)
             (if (wasm-limits? value)
                 (make-wasm-memory-type value)
                 (make-wat-memory-type-syntax (wat-limits-syntax-pos value) value)))
           <wat-limits>))

  #|proc:<wat-tag-type>
  Reads a tag function type use and returns internal `wat-type-use`, to be resolved to a canonical
  `wasm-tag-type` during normalization.
  |#
  (define <wat-tag-type> <wat-type-use>)

  )
