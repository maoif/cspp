(library (cspp navigator private custom)
  (export nav/getter nav/getter-setter nav/getter-setter!)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core))

  #|proc:nav/getter
  The `nav/getter` procedure creates a read-only navigator named by `name`.
  The `get-proc` procedure receives the parent value and returns the focused
  child value.
  |#
  (define nav/getter
    (lambda (name get-proc)
      (pcheck ([symbol? name] [procedure? get-proc])
              (make-$navigator
               name 'custom `(nav-custom ,name)
               (lambda (value emit)
                 (emit (nav-call-user get-proc value)))
               (lambda (value update)
                 (nav-error name "read-only custom navigator cannot transform: ~s" value))
               (lambda (value update!)
                 (nav-error name "read-only custom navigator cannot mutate: ~s" value))
               (lambda (value clear)
                 (nav-error name "read-only custom navigator cannot clear: ~s" value))
               (lambda (value clear!)
                 (nav-error name "read-only custom navigator cannot clear!: ~s" value))))))

  #|proc:nav/getter-setter
  The `nav/getter-setter` procedure creates a pure navigator named by `name`.
  The `get-proc` procedure extracts the child value, and `set-proc` receives
  the parent value and transformed child value and returns a rebuilt parent.
  |#
  (define nav/getter-setter
    (lambda (name get-proc set-proc)
      (pcheck ([symbol? name] [procedure? get-proc set-proc])
              (make-$navigator
               name 'custom `(nav-custom ,name)
               (lambda (value emit)
                 (emit (nav-call-user get-proc value)))
               (lambda (value update)
                 (nav-call-user set-proc value (update (nav-call-user get-proc value))))
               (lambda (value update!)
                 (nav-error name "custom navigator has no mutating setter: ~s" value))
               (lambda (value clear)
                 (nav-error name "custom navigator cannot clear: ~s" value))
               (lambda (value clear!)
                 (nav-error name "custom navigator cannot clear!: ~s" value))))))

  #|proc:nav/getter-setter!
  The `nav/getter-setter!` procedure creates a navigator named by `name` that
  supports selection and both transformation modes. `name` is a symbol used for diagnostics.
  `get-proc` has signature `(parent) -> child`. `set-proc` has signature
  `(parent new-child) -> rebuilt-parent` and must leave the original parent unchanged.
  `set!-proc` has signature `(parent new-child) -> unspecified` and mutates the parent;
  navigator mutation returns that original parent. The result is a navigator.
  |#
  (define nav/getter-setter!
    (lambda (name get-proc set-proc set!-proc)
      (pcheck ([symbol? name] [procedure? get-proc set-proc set!-proc])
              (make-$navigator
               name 'custom `(nav-custom ,name)
               (lambda (value emit)
                 (emit (nav-call-user get-proc value)))
               (lambda (value update)
                 (nav-call-user set-proc value (update (nav-call-user get-proc value))))
               (lambda (value update!)
                 (nav-call-user set!-proc value (update! (nav-call-user get-proc value)))
                 value)
               (lambda (value clear)
                 (nav-error name "custom navigator cannot clear: ~s" value))
               (lambda (value clear!)
                 (nav-error name "custom navigator cannot clear!: ~s" value)))))))
