(library (cspp parser wasm binary types)
  (export <wasm-number-type> <wasm-vector-type> <wasm-heap-type>
          <wasm-reference-type> <wasm-value-type> <wasm-storage-type>
          <wasm-field-type> <wasm-composite-type> <wasm-subtype>
          <wasm-recursive-type> <wasm-function-type> <wasm-struct-type>
          <wasm-array-type> <wasm-block-type> <wasm-global-type>
          <wasm-table-type> <wasm-memory-type> <wasm-limits>
          <wasm-tag-type> <wasm-external-type>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm binary values)
          (cspp parser wasm types))

  ;; Abstract heap types are single-byte terminals in the final Core 3.0 grammar.
  (define abstract-heap-type-from-byte
    (lambda (byte)
      (case byte
        [(#x69) 'exn] [(#x6a) 'array] [(#x6b) 'struct]
        [(#x6c) 'i31] [(#x6d) 'eq] [(#x6e) 'any]
        [(#x6f) 'extern] [(#x70) 'func] [(#x71) 'none]
        [(#x72) 'noextern] [(#x73) 'nofunc] [(#x74) 'noexn]
        [else #f])))

  (define abstract-heap-type-parser
    (<bind> <u8>
            (lambda (byte)
              (let ([type (abstract-heap-type-from-byte byte)])
                (if type (<result> type)
                    (<fail-with> "undefined WebAssembly abstract heap type"))))))

  (define heap-type-parser
    (</> abstract-heap-type-parser
         (<bind> <wasm-s33>
                 (lambda (value)
                   (if (>= value 0) (<result> value)
                       (<fail-with> "undefined WebAssembly heap type"))))))

  (define nullable-reference
    (lambda (heap-type)
      (make-wasm-reference-type #t heap-type)))

  (define non-null-reference
    (lambda (heap-type)
      (make-wasm-reference-type #f heap-type)))

  (define shorthand-heap-type-parser abstract-heap-type-parser)

  (define mutability-parser
    (</> (<as> #f (<uimm8> 0))
         (<as> #t (<uimm8> 1))))

  (define limits-with-maximum
    (lambda (address-type shared?)
      (<bind> (<~> <wasm-u64> <wasm-u64>)
              (lambda (bound*)
                (let ([minimum (car bound*)] [maximum (cadr bound*)])
                  (if (<= minimum maximum)
                      (<result> (make-wasm-limits address-type minimum maximum shared?))
                      (<fail-with> "WebAssembly limits maximum is below minimum")))))))

  (define limits-without-maximum
    (lambda (address-type)
      (<map> (lambda (minimum) (make-wasm-limits address-type minimum #f))
             <wasm-u64>)))

  #|proc:<wasm-number-type>
  The `<wasm-number-type>` parser reads an i32, i64, f32, or f64 type.
  |#
  (define <wasm-number-type>
    (</> (<as> 'i32 (<uimm8> #x7f))
         (<as> 'i64 (<uimm8> #x7e))
         (<as> 'f32 (<uimm8> #x7d))
         (<as> 'f64 (<uimm8> #x7c))))

  #|proc:<wasm-vector-type>
  The `<wasm-vector-type>` parser reads the v128 vector type.
  |#
  (define <wasm-vector-type> (<as> 'v128 (<uimm8> #x7b)))

  #|proc:<wasm-heap-type>
  The `<wasm-heap-type>` parser reads an abstract heap type or concrete type index.
  |#
  (define <wasm-heap-type> heap-type-parser)

  #|proc:<wasm-reference-type>
  The `<wasm-reference-type>` parser reads an explicit or shorthand reference type.
  |#
  (define <wasm-reference-type>
    (</> (<map> nullable-reference
                (<~1> (<uimm8> #x63) <wasm-heap-type>))
         (<map> non-null-reference
                (<~1> (<uimm8> #x64) <wasm-heap-type>))
         (<map> nullable-reference shorthand-heap-type-parser)))

  #|proc:<wasm-value-type>
  The `<wasm-value-type>` parser reads a numeric, vector, or reference value type.
  |#
  (define <wasm-value-type>
    (let ([fallback (</> <wasm-number-type> <wasm-vector-type> <wasm-reference-type>)])
      (define-parser value-type-parser
        (let* ([position (input-pos inp)]
               [byte (and (fx< position (input-len inp))
                          (bytevector-u8-ref (binary-input-data inp) position))]
               [number-type (case byte
                              [(#x7f) 'i32] [(#x7e) 'i64] [(#x7d) 'f32]
                              [(#x7c) 'f64] [(#x7b) 'v128] [else #f])]
               [heap-type (and byte (not number-type) (abstract-heap-type-from-byte byte))])
          ;; Only known successful short forms bypass choice. Other forms keep full causes.
          (if (or number-type heap-type)
              (begin (input-pos-set! inp (fx1+ position))
                     (values #t (or number-type (nullable-reference heap-type)) inp))
              (parser-call fallback inp state lvl))))
      value-type-parser))

  #|proc:<wasm-storage-type>
  The `<wasm-storage-type>` parser reads i8, i16, or a value type.
  |#
  (define <wasm-storage-type>
    (</> (<as> 'i8 (<uimm8> #x78))
         (<as> 'i16 (<uimm8> #x77))
         <wasm-value-type>))

  #|proc:<wasm-field-type>
  The `<wasm-field-type>` parser reads a storage type and its mutability flag.
  |#
  (define <wasm-field-type>
    (<map> (lambda (field*)
             (make-wasm-field-type (car field*) (cadr field*)))
           (<~> <wasm-storage-type> mutability-parser)))

  #|proc:<wasm-function-type>
  The `<wasm-function-type>` parser reads function parameter and result vectors.
  |#
  (define <wasm-function-type>
    (<map> (lambda (field*)
             (make-wasm-function-type (car field*) (cadr field*)))
           (<~1> (<uimm8> #x60)
                  (<~> (<wasm-vector> <wasm-value-type>)
                       (<wasm-vector> <wasm-value-type>)))))

  #|proc:<wasm-struct-type>
  The `<wasm-struct-type>` parser reads a vector of structure fields.
  |#
  (define <wasm-struct-type>
    (<map> make-wasm-struct-type
           (<~1> (<uimm8> #x5f) (<wasm-vector> <wasm-field-type>))))

  #|proc:<wasm-array-type>
  The `<wasm-array-type>` parser reads an array element field.
  |#
  (define <wasm-array-type>
    (<map> make-wasm-array-type
           (<~1> (<uimm8> #x5e) <wasm-field-type>)))

  #|proc:<wasm-composite-type>
  The `<wasm-composite-type>` parser reads a function, structure, or array type.
  |#
  (define <wasm-composite-type>
    (</> <wasm-function-type> <wasm-struct-type> <wasm-array-type>))

  #|proc:<wasm-subtype>
  The `<wasm-subtype>` parser reads an explicit subtype or shorthand composite subtype.
  |#
  (define <wasm-subtype>
    (</> (<map> (lambda (field*)
                  (make-wasm-subtype #t (car field*) (cadr field*)))
                (<~1> (<uimm8> #x4f)
                       (<~> (<wasm-vector> <wasm-u32>) <wasm-composite-type>)))
         (<map> (lambda (field*)
                  (make-wasm-subtype #f (car field*) (cadr field*)))
                (<~1> (<uimm8> #x50)
                       (<~> (<wasm-vector> <wasm-u32>) <wasm-composite-type>)))
         (<map> (lambda (composite-type)
                  (make-wasm-subtype #t '#() composite-type))
                <wasm-composite-type>)))

  #|proc:<wasm-recursive-type>
  The `<wasm-recursive-type>` parser reads an explicit group or shorthand singleton group.
  |#
  (define <wasm-recursive-type>
    (</> (<map> make-wasm-recursive-type
                (<~1> (<uimm8> #x4e) (<wasm-vector> <wasm-subtype>)))
         (<map> (lambda (subtype) (make-wasm-recursive-type (vector subtype)))
                <wasm-subtype>)))

  #|proc:<wasm-block-type>
  The `<wasm-block-type>` parser reads an empty, value, or indexed block type.
  |#
  (define <wasm-block-type>
    (</> (<as> (make-wasm-block-type 'empty #f) (<uimm8> #x40))
         (<map> (lambda (value-type)
                  (make-wasm-block-type 'value-type value-type))
                <wasm-value-type>)
         (<bind> <wasm-s33>
                 (lambda (type-index)
                   (if (>= type-index 0)
                       (<result> (make-wasm-block-type 'type-index type-index))
                       (<fail-with> "undefined negative WebAssembly block type"))))))

  #|proc:<wasm-global-type>
  The `<wasm-global-type>` parser reads a value type and mutability flag.
  |#
  (define <wasm-global-type>
    (<map> (lambda (field*)
             (make-wasm-global-type (car field*) (cadr field*)))
           (<~> <wasm-value-type> mutability-parser)))

  ;; These six compositions are independent of the declaration being parsed.
  (define i32-minimum-limits (limits-without-maximum 'i32))
  (define i32-bounded-limits (limits-with-maximum 'i32 #f))
  (define i32-shared-limits (limits-with-maximum 'i32 #t))
  (define i64-minimum-limits (limits-without-maximum 'i64))
  (define i64-bounded-limits (limits-with-maximum 'i64 #f))
  (define i64-shared-limits (limits-with-maximum 'i64 #t))

  #|proc:<wasm-limits>
  The `<wasm-limits>` parser reads i32 or i64 minimum and optional maximum bounds. Shared-memory
  extension flags are supported and require a maximum.
  |#
  (define <wasm-limits>
    (<bind> <u8>
            (lambda (flags)
              (case flags
                [(#x00) i32-minimum-limits]
                [(#x01) i32-bounded-limits]
                [(#x03) i32-shared-limits]
                [(#x04) i64-minimum-limits]
                [(#x05) i64-bounded-limits]
                [(#x07) i64-shared-limits]
                [else (<fail-with> "invalid WebAssembly limits flags")]))))

  #|proc:<wasm-table-type>
  The `<wasm-table-type>` parser reads a reference element type and table limits.
  |#
  (define <wasm-table-type>
    (<bind> (<~> <wasm-reference-type> <wasm-limits>)
            (lambda (fields)
              (if (wasm-limits-shared? (cadr fields))
                  (<fail-with> "WebAssembly tables cannot be shared")
                  (<result> (make-wasm-table-type (car fields) (cadr fields)))))))

  #|proc:<wasm-memory-type>
  The `<wasm-memory-type>` parser reads memory limits.
  |#
  (define <wasm-memory-type> (<map> make-wasm-memory-type <wasm-limits>))

  #|proc:<wasm-tag-type>
  The `<wasm-tag-type>` parser reads the required zero attribute and function type index.
  |#
  (define <wasm-tag-type>
    (<map> make-wasm-tag-type (<~1> (<uimm8> 0) <wasm-u32>)))

  #|proc:<wasm-external-type>
  The `<wasm-external-type>` parser reads a function, table, memory, global, or tag type.
  |#
  (define <wasm-external-type>
    (<bind> <u8>
            (lambda (kind)
              (case kind
                [(0) (<map> (lambda (type-index)
                              (make-wasm-external-type 'function type-index))
                            <wasm-u32>)]
                [(1) (<map> (lambda (type)
                              (make-wasm-external-type 'table type))
                            <wasm-table-type>)]
                [(2) (<map> (lambda (type)
                              (make-wasm-external-type 'memory type))
                            <wasm-memory-type>)]
                [(3) (<map> (lambda (type)
                              (make-wasm-external-type 'global type))
                            <wasm-global-type>)]
                [(4) (<map> (lambda (type)
                              (make-wasm-external-type 'tag type))
                            <wasm-tag-type>)]
                [else (<fail-with> "invalid WebAssembly external type kind")]))))
  )
