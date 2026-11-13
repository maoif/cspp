(library (cspp private optional-library)
  (export require-optional-library)
  (import (cspp chez) (cspp utils))

  (define native-library-info
    (foreign-procedure "cspp_optional_library_info" (string) scheme-object))

  #|proc:require-optional-library
  The `require-optional-library` procedure checks the native dependency named by `library`.
  `who` is the caller's symbol used in error reports, and `library` is a dependency symbol.
  It completes without a useful return value when available, or raises the native diagnostic.
  |#
  (define require-optional-library
    (lambda (who library)
      (pcheck ([symbol? who library])
              (let ([info (native-library-info (symbol->string library))])
                (unless (and (vector? info) (vector-ref info 1))
                  (error who (if (vector? info) (vector-ref info 4)
                                 "unknown optional library")))))))
  )
