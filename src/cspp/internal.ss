(library (cspp internal)
  (export todo $construct-name flag->mask $record-type-key $datatype-variant-key)
  (import (chezscheme))


  #|doc:$record-type-key
  Private binding used by `record` and `match` to associate a public constructor with the
  defining-context identifier of its record type. It has no runtime purpose.
  |#
  (define $record-type-key #f)

  #|doc:$datatype-variant-key
  Private binding used by `datatype` and `match` to associate a constructor or singleton with
  the defining-context identifier of its variant type. It has no runtime purpose.
  |#
  (define $datatype-variant-key #f)

  ;; todo imple as a macro? and use `who`
  (define todo
    (case-lambda
      [() (errorf #f "not impl")]
      [(x) (errorf x "not impl")]))

  (define $construct-name
    (lambda (template-identifier . args)
      (datum->syntax
       template-identifier
       (string->symbol
        (apply string-append
               (map (lambda (x)
                      (if (string? x)
                          x
                          (symbol->string (syntax->datum x))))
                    args))))))

  ;; for utils: define-flags
  (define flag->mask
    (lambda (m e)
      (cond
       [(fixnum? m) m]
       [(and (symbol? m) (assq m e)) => cdr]
       [(and (list? m) (eq? (car m) 'or))
        (let f ((ls (cdr m)))
          (if (null? ls)
              0
              (fxlogor (flag->mask (car ls) e) (f (cdr ls)))))]
       ;; TODO better error report
       [else (errorf 'flag->mask "invalid mask ~s" m)])))




  (let loop ([paths `(,(or (getenv "LIBCSPP") "")
                      ,(path-build (current-directory) "libcspp.so")
                      "/lib/libcspp.so"
                      "/usr/lib/libcspp.so"
                      "/usr/local/lib/libcspp.so")])
    (if (null? paths)
        (errorf 'cspp-internal "failed to load libcspp.so")
        (if (file-regular? (car paths))
            (with-exception-handler
                (lambda (con) (loop (cdr paths)))
              (lambda () (load-shared-object (car paths))))
            (loop (cdr paths)))))
  )
