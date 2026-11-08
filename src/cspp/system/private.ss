(library (cspp system private)
  (export $unsigned-32? $positive-pid? $os-string? $os-string-list?
          $environment-name? $process-specs?)
  (import (cspp chez) (cspp utils) (cspp string))

  ;; Shared contracts for values passed to native system helpers.
  (define $unsigned-32?
    (lambda (value) (and (natural? value) (<= value #xffffffff))))

  (define $positive-pid?
    (lambda (value) (and (natural? value) (<= 1 value #x7fffffff))))

  (define $os-string?
    (lambda (text) (and (string? text) (not (string-search text "\x0;")))))

  (define $os-string-list?
    (lambda (values) (and (list? values) (andmap $os-string? values))))

  (define $environment-name?
    (lambda (name)
      (and ($os-string? name) (not (string-empty? name)) (not (string-search name "=")))))

  (define $process-specs?
    (lambda (specs)
      (and (pair? specs) (list? specs)
           (andmap (lambda (spec) (and (pair? spec) ($os-string-list? spec))) specs))))

)
