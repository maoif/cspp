(library (cspp system private)
  (export $unsigned-32?)
  (import (cspp chez) (cspp utils))

  ;; Shared contracts for values passed to native system helpers.
  (define $unsigned-32?
    (lambda (value) (and (natural? value) (<= value #xffffffff))))

)
