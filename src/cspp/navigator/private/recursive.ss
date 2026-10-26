(library (cspp navigator private recursive)
  (export nav/rec nav/letrec nav/children nav/leaves nav/walker nav/before nav/after)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core)
          (cspp navigator private data)
          (cspp navigator private engine))

  (define make-recursive-nav-group
    (lambda (bindings root)
      (let loop ([names (map car bindings)] [seen '()])
        (cond [(null? names) (make-$recursive-nav-group bindings root)]
              [(memq (car names) seen)
               (nav-error 'nav/letrec "duplicate recursive navigator name: ~s" (car names))]
              [else (loop (cdr names) (cons (car names) seen))]))))

  (define leaf?
    (lambda (value)
      (or (and (pair? value) (not (list? value)))
          ;; Use a one-shot continuation to stop after discovering one child.
          (call/1cc
           (lambda (return)
             (emit-children value (lambda (child) (return #f)))
             #t)))))

  (define emit-current-focus
    (lambda (value emit)
      (let ([emitted? #f])
        (emit-children value
                       (lambda (child)
                         (when (leaf? child)
                           (set! emitted? #t)
                           (emit child))))
        (unless emitted?
          (emit value)))))

  (define make-tree-nav
    (lambda (name metadata select-proc transform-proc transform!-proc clear-proc clear!-proc)
      (make-$navigator
       name 'recursive metadata
       select-proc
       transform-proc transform!-proc clear-proc clear!-proc)))

  (define tree-unsupported-transform
    (lambda (name)
      (lambda (value update)
        (nav-error 'nav-transform "recursive navigator ~s does not support transform yet" name))))

  (define tree-unsupported-transform!
    (lambda (name)
      (lambda (value update!)
        (nav-error 'nav-transform! "recursive navigator ~s does not support transform! yet" name))))

  (define tree-unsupported-clear
    (lambda (name)
      (lambda (value clear)
        (nav-error 'nav-clearval "recursive navigator ~s does not support clear yet" name))))

  (define tree-unsupported-clear!
    (lambda (name)
      (lambda (value clear!)
        (nav-error 'nav-clearval! "recursive navigator ~s does not support clear! yet" name))))

  #|proc:nav/children
  The `nav/children` navigator focuses immediate children of supported nested
  values when composed with later path steps. As a final selection step, it
  focuses collection descendants in tree order. Both modes support registered indexed
  and keyed containers as well as built-in containers.
  |#
  (define nav/children
    (make-tree-nav
     'nav/children 'nav/children
     (lambda (value emit)
       (emit-children value emit))
     (lambda (value update)
       (cond [(indexed-supported? value)
              (indexed-transform value (index-list (indexed-length value)) update)]
             [(keyed-supported? value) (keyed-update-values value update)]
             [else value]))
     (lambda (value update!)
       (cond [(indexed-supported? value)
              (indexed-transform! value (index-list (indexed-length value)) update!)]
             [(keyed-supported? value) (keyed-update-values! value update!)]
             [else value]))
     (lambda (value clear)
       (cond [(indexed-supported? value)
              (indexed-clear-values value (index-list (indexed-length value)) clear)]
             [(keyed-supported? value) (keyed-update-values value clear)]
             [else value]))
     (lambda (value clear!)
       (cond [(indexed-supported? value)
              (indexed-clear-values! value (index-list (indexed-length value)) clear!)]
             [(keyed-supported? value) (keyed-update-values! value clear!)]
             [else value]))))

  #|proc:nav/leaves
  The `nav/leaves` navigator focuses nested values with no supported children.
  |#
  (define nav/leaves
    (make-tree-nav
     'nav/leaves 'nav/leaves
     (lambda (value emit)
       (let walk ([value value])
         (if (leaf? value)
             (emit value)
             (emit-children value walk))))
     (tree-unsupported-transform 'nav/leaves)
     (tree-unsupported-transform! 'nav/leaves)
     (tree-unsupported-clear 'nav/leaves)
     (tree-unsupported-clear! 'nav/leaves)))

  #|proc:nav/walker
  The `nav/walker` procedure returns a navigator that recursively walks nested
  values and focuses every value for which `predicate` returns true.
  |#
  (define nav/walker
    (lambda (predicate)
     (pcheck ([procedure? predicate])
             (make-tree-nav
              'nav/walker '(nav/walker)
              (lambda (value emit)
                (let walk ([value value])
                  (when (nav-call-user predicate value)
                    (emit value))
                  (emit-children value walk)))
              (tree-unsupported-transform 'nav/walker)
              (tree-unsupported-transform! 'nav/walker)
              (tree-unsupported-clear 'nav/walker)
              (tree-unsupported-clear! 'nav/walker)))))

  #|proc:nav/before
  The `nav/before` procedure returns a navigator that focuses immediate leaf children
  of the current value before values selected by `path`. If no immediate child is a leaf,
  it focuses the current value itself first.
  |#
  (define nav/before
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-tree-nav
         'nav/before '(nav/before)
         (lambda (value emit)
           (emit-current-focus value emit)
           (select-path compiled value emit))
         (tree-unsupported-transform 'nav/before)
         (tree-unsupported-transform! 'nav/before)
         (tree-unsupported-clear 'nav/before)
         (tree-unsupported-clear! 'nav/before)))))

  #|proc:nav/after
  The `nav/after` procedure returns a navigator that focuses values selected by `path`
  before focusing immediate leaf children of the current value. If no immediate child is
  a leaf, it focuses the current value itself last.
  |#
  (define nav/after
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-tree-nav
         'nav/after '(nav/after)
         (lambda (value emit)
           (select-path compiled value emit)
           (emit-current-focus value emit))
         (tree-unsupported-transform 'nav/after)
         (tree-unsupported-transform! 'nav/after)
         (tree-unsupported-clear 'nav/after)
         (tree-unsupported-clear! 'nav/after)))))

  #|macro:nav/rec
  The `nav/rec` macro creates a recursive navigator named by identifier `name`
  whose body is `body`.
  |#
  (define-syntax nav/rec
    (lambda (stx)
      (syntax-case stx ()
        [(_ name body)
         (identifier? #'name)
         #'(nav/letrec ([name body]) name)]
        [_ (syntax-error stx "invalid nav/rec form:")])))

  #|macro:nav/letrec
  The `nav/letrec` macro creates a mutually recursive navigator group from
  bindings and returns `root`.
  |#
  (define-syntax nav/letrec
    (lambda (stx)
      (syntax-case stx ()
        [(_ ([name body] ...) root)
         (andmap identifier? #'(name ...))
         #'(let-syntax ([name (identifier-syntax (make-nav-ref 'name))] ...)
             (make-recursive-nav-group
              (list (cons 'name body) ...)
              root))]
        [_ (syntax-error stx "invalid nav/letrec form:")]))))
