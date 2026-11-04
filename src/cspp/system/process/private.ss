(library (cspp system process private)
  (export $option-ref $cwd-option $validate-process-options)
  (import (cspp chez) (cspp utils) (cspp system private))

  ;; Options have already been validated before native argument construction.
  (define $option-ref
    (lambda (options key default)
      (let ([entry (assq key options)])
        (if entry (cdr entry) default))))

  (define $cwd-option
    (lambda (options)
      (let ([cwd ($option-ref options 'cwd #f)])
        (if cwd cwd ""))))

  (define $validate-process-options
    (lambda (options live?)
      (pcheck ([list? options] [boolean? live?])
        (let ([seen '()])
          (for-each
            (lambda (entry)
              (unless (and (pair? entry) (symbol? (car entry)))
                (errorf 'process "invalid option entry: ~s" entry))
              (let ([key (car entry)] [value (cdr entry)])
                (when (memq key seen) (errorf 'process "duplicate option: ~s" key))
                (set! seen (cons key seen))
                (unless
                  (case key
                    [(cwd) (or (not value) ($os-string? value))]
                    [(env) (or (not value)
                               (and (list? value)
                                    (andmap (lambda (pair) (and (pair? pair) ($environment-name? (car pair))
                                                               ($os-string? (cdr pair)))) value)))]
                    [(env-mode) (eq? value 'replace)]
                    [(stdin) (if live? (memq value '(inherit null pipe))
                                  (or (not value) (memq value '(inherit null))
                                 (string? value) (bytevector? value)))]
                    [(stdout) (memq value (if live? '(inherit null pipe) '(inherit null capture)))]
                    [(stderr) (memq value (if live? '(inherit null pipe stdout)
                                                    '(inherit null capture stdout)))]
                    [(encoding) (and (not live?) (memq value '(utf-8 binary)))]
                    [(timeout) (and (not live?) (or (not value) (and (integer? value) (exact? value)
                                                  (<= 0 value #x7fffffff))))]
                    [(success) (and (not live?) (or (not value) (procedure? value)
                                   (and (list? value)
                                        (andmap (lambda (code) (and (fixnum? code)
                                                                   (<= 0 code 255))) value))))]
                    [else #f])
                  (errorf 'process "invalid ~s option: ~s" key value))))
            options)))))

)
