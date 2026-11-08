(library (cspp parser wasm)
  (export
    ;; Parser API
    parse-wasm-binary-module parse-wasm-binary-module-file
    parse-wasm-text-module parse-wasm-text-module-file

    ;; Module and custom sections
    make-wasm-module wasm-module? wasm-module-types wasm-module-imports
    wasm-module-functions wasm-module-tables wasm-module-memories wasm-module-globals
    wasm-module-tags wasm-module-exports wasm-module-start wasm-module-elements
    wasm-module-data wasm-module-custom-sections
    make-wasm-custom-section wasm-custom-section? wasm-custom-section-name
    wasm-custom-section-bytes wasm-custom-section-after-section

    ;; Types
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

    ;; Entities and segments
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

    ;; Instructions and immediates
    make-wasm-instruction wasm-instruction? wasm-instruction-mnemonic
    wasm-instruction-immediates wasm-instruction-body wasm-instruction-alternate
    make-wasm-memory-argument wasm-memory-argument? wasm-memory-argument-alignment
    wasm-memory-argument-offset wasm-memory-argument-memory-index
    make-wasm-block-type wasm-block-type? wasm-block-type-kind wasm-block-type-value
    make-wasm-catch wasm-catch? wasm-catch-kind wasm-catch-tag-index
    wasm-catch-label-index
    make-wasm-float wasm-float? wasm-float-width wasm-float-bits)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm types)
          (cspp parser wasm binary)
          (cspp parser wasm text)
          (cspp file)
          (cspp utils))

  (define bytevector-or-path?
    (lambda (value) (or (bytevector? value) (string? value))))

  #|proc:parse-wasm-binary-module
  Parses a WebAssembly Core 3.0 binary module from `input`, a bytevector or regular-file path.
  The return value is a canonical `wasm-module` record. Shared-memory limits are also supported.
  |#
  (define-who parse-wasm-binary-module
    (lambda (input)
      (pcheck ([bytevector-or-path? input])
              (if (bytevector? input)
                  (run-binary-parser <wasm-binary-module> input)
                  (parse-wasm-binary-module-file input)))))

  #|proc:parse-wasm-binary-module-file
  Parses a WebAssembly Core 3.0 binary module from regular file `path`. The return value is a
  canonical `wasm-module` record. Shared-memory limits are also supported.
  |#
  (define-who parse-wasm-binary-module-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-binary-file <wasm-binary-module> path))))

  #|proc:parse-wasm-text-module
  Parses a WebAssembly Core 3.0 text module from source string `text`. The return value is a
  canonical `wasm-module` record. Shared-memory limits are also supported.
  |#
  (define-who parse-wasm-text-module
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-wat-module text))))

  #|proc:parse-wasm-text-module-file
  Parses a WebAssembly Core 3.0 text module from regular-file path `path`. The return value is a
  canonical `wasm-module` record. Shared-memory limits are also supported.
  |#
  (define-who parse-wasm-text-module-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-textual-file parser-wat-module path))))
  )
