(library (cspp private libcspp)
  (export libcspp-loaded?)
  (import (cspp chez))

  #|value:libcspp-loaded?
  The `libcspp-loaded?` value is `#t` after this library has loaded CSPP's native runtime.
  Initialization checks `LIBCSPP`, the current directory, and standard library directories.
  It raises the last loader error, or a missing-library error when no candidate exists.
  |#
  (define libcspp-loaded?
    (let loop ([paths `(,(or ($getenv "LIBCSPP") "")
                        ,(path-build (current-directory) "libcspp.so")
                        "/lib/libcspp.so"
                        "/usr/lib/libcspp.so"
                        "/usr/local/lib/libcspp.so")]
               [failure #f])
      (cond
       [(null? paths)
        (if failure
            (raise failure)
            (error 'libcspp "cannot find libcspp.so"))]
       [(not (file-regular? (car paths))) (loop (cdr paths) failure)]
       [else
        (guard (condition [else (loop (cdr paths) condition)])
          (load-shared-object (car paths))
          #t)]))))
