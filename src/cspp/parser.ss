(library (cspp parser)
  (export)
  (import (cspp chez))

  (export (import (except (cspp parser combinator)
                          parser-call input-pos input-pos-set! input-len
                          save-input binary-input-data parser-discard-many
                          parser-counted-vector parser-ordinary-character
                          bindigits->num octdigits->num digits->num hexdigits->num)

                  (cspp parser csv)
                  (cspp parser json5)
                  (cspp parser xml)
                  (cspp parser scheme)
                  (cspp parser toml)
                  (cspp parser elf)
                  (cspp parser wasm)))
  )
