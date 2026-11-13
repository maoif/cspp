(library (cspp parser)
  (export)
  (import (chezscheme))

  (export (import (except (cspp parser combinator)
                          parser-call input-pos input-pos-set! input-len
                          save-input binary-input-data parser-discard-many
                          parser-counted-vector parser-ordinary-character
                          bindigits->num octdigits->num digits->num hexdigits->num))))
