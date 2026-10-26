(library (cspp navigator private core)
  (export $navigator? $path? $recursive-nav-group? $nav-ref?
          make-$navigator make-$path make-$recursive-nav-group make-$nav-ref
          $navigator-name $navigator-kind $navigator-metadata
          $navigator-select-proc $navigator-transform-proc $navigator-transform!-proc
          $navigator-clear-proc $navigator-clear!-proc
          $path-steps $recursive-nav-group-bindings $recursive-nav-group-root
          $recursive-nav-group-env
          $nav-ref-name $nav-ref-target $nav-ref-target-set!
          nav? nav-path? nav/empty-path nav-empty-path?
          make-nav nav-name nav-coerce nav/path nav-compile-path nav/path-append
          make-nav-ref nav-ref? nav-missing nav-missing?
          current-update-count nav-update-version nav-note-update!
          current-optional-focus? nav-call-step
          nav-error nav-focus-error nav-focus-error? nav-absent-focus?
          nav-call-user nav-protect-callback nav-unsupported-error nav-required-error
          nav-path-steps nav-normalize-step nav-debug-name-core nav->datum-core
          install-nav-key-proc! install-nav-nth-proc!)
  (import (chezscheme)
          (cspp utils))

  (define-record-type ($navigator make-$navigator $navigator?)
    (fields (immutable name)
            (immutable kind)
            (immutable metadata)
            (immutable select-proc)
            (immutable transform-proc)
            (immutable transform!-proc)
            (immutable clear-proc)
            (immutable clear!-proc))
    (opaque #t)
    (sealed #t))

  (define-record-type ($path make-$path $path?)
    (fields (immutable steps))
    (opaque #t)
    (sealed #t))

  (define-record-type ($recursive-nav-group make-$recursive-nav-group $recursive-nav-group?)
    (fields (immutable bindings)
            (immutable root)
            (immutable env))
    (opaque #t)
    (sealed #t))

  (define-record-type ($nav-ref make-$nav-ref $nav-ref?)
    (fields (immutable name) (mutable target))
    (opaque #t)
    (sealed #t))

  ;; One counter belongs to each public update. Virtual default focuses use it to
  ;; distinguish a visited suffix from an actual invocation of the terminal callback.
  (define current-update-count (make-parameter #f))
  (define nav-update-version
    (lambda () (let ([counter (current-update-count)]) (if counter (car counter) 0))))
  (define nav-note-update!
    (lambda ()
      (let ([counter (current-update-count)])
        (when counter (set-car! counter (+ 1 (car counter)))))))

  (define nav-missing (list 'nav-missing))
  (define nav-missing? (lambda (x) (eq? x nav-missing)))

  #|proc:nav?
  The `nav?` procedure returns `#t` when `x` is a navigator record.
  |#
  (define nav? $navigator?)

  #|proc:nav-path?
  The `nav-path?` procedure returns `#t` when `x` is a compiled navigator path.
  |#
  (define nav-path? $path?)

  #|proc:nav/empty-path
  The `nav/empty-path` value is the compiled empty path that focuses the root value.
  |#
  (define nav/empty-path (make-$path '()))

  #|proc:nav-empty-path?
  The `nav-empty-path?` procedure returns `#t` when `path` is a compiled path
  containing no steps.
  |#
  (define nav-empty-path?
    (lambda (path)
      (pcheck ([$path? path])
              (null? ($path-steps path)))))

  (define nav-error
    (lambda (who fmt . args)
      (apply errorf who fmt args)))

  (define-condition-type &nav-focus-error &error
    make-nav-focus-error nav-focus-error?)

  (define nav-focus-error
    (lambda (who fmt . args)
      (raise (condition (make-nav-focus-error)
                        (make-who-condition who)
                        (make-message-condition (apply format fmt args))))))

  ;; Absence is handled at the failing step, allowing enclosing collection walks
  ;; to retain successful siblings. User callbacks and outer suffixes stay protected.
  (define current-optional-focus? (make-parameter #f))
  (define nav-call-step
    (lambda (proc value continue fallback)
      (if (current-optional-focus?)
          (guard (exn [(nav-absent-focus? exn) fallback])
            (proc value continue))
          (proc value continue))))

  (define-condition-type &nav-callback-error &condition
    make-nav-callback-error nav-callback-error?)

  (define nav-absent-focus?
    (lambda (exn)
      (and (nav-focus-error? exn) (not (nav-callback-error? exn)))))

  ;; Keep the original condition components and message, but mark callback-origin
  ;; navigation failures so optional path handlers cannot mistake them for absence.
  (define nav-call-user
    (lambda (proc . args)
      (guard (exn [(nav-absent-focus? exn)
                   (raise (condition exn (make-nav-callback-error)))])
        (apply proc args))))

  (define nav-protect-callback
    (lambda (proc)
      (lambda args (apply nav-call-user proc args))))

  ;; Custom navigators invoke an engine continuation as well as their own code.
  ;; Preserve continuation-origin absence for an enclosing optional path to handle.
  (define nav-protect-traversal
    (lambda (proc)
      (lambda (value continue)
        (let ([downstream-condition #f])
          (guard (exn [(and (nav-absent-focus? exn)
                            (not (eq? exn downstream-condition)))
                       (raise (condition exn (make-nav-callback-error)))])
            (proc value
                  (lambda (child)
                    (guard (exn [else (set! downstream-condition exn) (raise exn)])
                      (continue child)))))))))

  (define nav-unsupported-error
    (lambda (who nav value action)
      (nav-error who "navigator ~a does not support ~a for value: ~s"
                 ($navigator-name nav) action value)))

  (define nav-required-error
    (lambda (who nav value detail)
      (nav-error who "navigator ~a failed on required focus ~s: ~a"
                 ($navigator-name nav) value detail)))

  #|proc:make-nav
  The `make-nav` procedure creates a custom navigator named by the symbol `name`.
  `select-proc` has signature `(parent emit) -> unspecified`; it calls `(emit child)`
  once per selected child, in traversal order. `transform-proc` has signature
  `(parent update) -> rebuilt-parent`; it replaces each child with `(update child)`.
  `transform!-proc` has signature `(parent update!) -> parent`; it mutates each focus
  using `(update! child)`, which returns the replacement child. The result is a navigator.
  Custom navigators created by this procedure do not support clearing.
  |#
  (define make-nav
    (lambda (name select-proc transform-proc transform!-proc)
      (pcheck ([symbol? name] [procedure? select-proc transform-proc transform!-proc])
              (make-$navigator name 'custom `(nav-custom ,name)
                               (nav-protect-traversal select-proc)
                               (nav-protect-traversal transform-proc)
                               (nav-protect-traversal transform!-proc)
                               (lambda (value clear)
                                 (nav-error name "custom navigator cannot clear: ~s" value))
                               (lambda (value clear!)
                                 (nav-error name "custom navigator cannot clear!: ~s" value))))))

  #|proc:nav-name
  The `nav-name` procedure returns the symbolic debug name of navigator `nav`.
  |#
  (define nav-name
    (lambda (nav)
      (pcheck ([$navigator? nav])
              ($navigator-name nav))))

  #|proc:make-nav-ref
  The `make-nav-ref` procedure creates a symbolic recursive navigator reference
  named by the symbol `name`. It resolves within the defining environment of the active
  recursive group; fresh public operations start with no active bindings.
  |#
  (define make-nav-ref
    (lambda (name)
      (pcheck ([symbol? name])
              (make-$nav-ref name #f))))

  (define nav-ref? $nav-ref?)

  (define nav-path-steps
    (lambda (path)
      (pcheck ([$path? path])
              ($path-steps path))))

  (define nav-key-proc
    (let ([proc #f])
      (case-lambda
        [() (or proc (nav-error 'nav/key "nav/key is not installed"))]
        [(p) (set! proc p)])))

  (define nav-nth-proc
    (let ([proc #f])
      (case-lambda
        [() (or proc (nav-error 'nav/nth "nav/nth is not installed"))]
        [(p) (set! proc p)])))

  (define install-nav-key-proc!
    (lambda (proc)
      (pcheck ([procedure? proc])
              (nav-key-proc proc))))

  (define install-nav-nth-proc!
    (lambda (proc)
      (pcheck ([procedure? proc])
              (nav-nth-proc proc))))

  (define nav-normalize-step
    (lambda (x)
      (cond [($navigator? x) (list x)]
            [($path? x) ($path-steps x)]
            [($recursive-nav-group? x) (list x)]
            [($nav-ref? x) (list x)]
            [(symbol? x) (list ((nav-key-proc) x))]
            [(and (integer? x) (>= x 0)) (list ((nav-nth-proc) x))]
            [else (nav-error 'nav-compile-path "cannot coerce path step: ~s" x)])))

  #|proc:nav-coerce
  The `nav-coerce` procedure converts `x` into a navigator or path. Navigator
  and path values are returned in equivalent form, symbols become key
  navigators, and non-negative integer indexes become nth navigators.
  |#
  (define nav-coerce
    (lambda (x)
      (cond [($navigator? x) x]
            [($path? x) x]
            [else
             (let ([steps (nav-normalize-step x)])
               (if (and (pair? steps) (null? (cdr steps)))
                   (car steps)
                   (make-$path steps)))])))

  #|proc:nav-compile-path
  The `nav-compile-path` procedure converts path-like value `pathish` into a
  compiled path. Lists are treated as lists of path steps and nested paths are
  flattened.
  |#
  (define nav-compile-path
    (lambda (pathish)
      (cond [($path? pathish) pathish]
            [($navigator? pathish) (make-$path (list pathish))]
            [($recursive-nav-group? pathish) (make-$path (list pathish))]
            [($nav-ref? pathish) (make-$path (list pathish))]
            [(list? pathish)
             (make-$path (apply append (map nav-normalize-step pathish)))]
            [else (make-$path (nav-normalize-step pathish))])))

  #|proc:nav/path
  The `nav/path` procedure creates a compiled path from the path-like values
  `args`. Navigator values, path values, recursive values, symbols, and
  non-negative integer indexes are accepted; nested paths are flattened.
  |#
  (define nav/path
    (lambda args
      (make-$path (apply append (map nav-normalize-step args)))))

  #|proc:nav/path-append
  The `nav/path-append` procedure concatenates each path-like value in `paths`
  and returns one compiled path.
  |#
  (define nav/path-append
    (lambda paths
      (make-$path
       (apply append
              (map (lambda (path) ($path-steps (nav-compile-path path)))
                   paths)))))

  (define nav-debug-name-core
    (lambda (x)
      (cond [($navigator? x) ($navigator-name x)]
            [($path? x) 'nav/path]
            [($recursive-nav-group? x) 'nav/letrec]
            [($nav-ref? x) ($nav-ref-name x)]
            [else #f])))

  (define nav->datum-core
    (lambda (x recur)
      (cond [($navigator? x) ($navigator-metadata x)]
            [($path? x) `(nav/path ,@(map recur ($path-steps x)))]
            [($nav-ref? x) `(nav-ref ,($nav-ref-name x))]
            [($recursive-nav-group? x)
             `(nav/letrec ,(map (lambda (binding)
                                  (list (car binding) (recur (cdr binding))))
                                ($recursive-nav-group-bindings x))
                          ,(recur ($recursive-nav-group-root x)))]
            [else x]))))
