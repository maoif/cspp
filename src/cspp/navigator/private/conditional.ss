(library (cspp navigator private conditional)
  (export nav/pred nav/not-pred nav/must nav/maybe nav/if nav/when nav/unless
          nav/multi-path nav/choice)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core)
          (cspp navigator private basic)
          (cspp navigator private engine))

  (define make-conditional-nav
    (lambda (name metadata select-proc transform-proc transform!-proc clear-proc clear!-proc)
      (make-$navigator name 'conditional metadata
                       select-proc
                       transform-proc
                       transform!-proc
                       clear-proc
                       clear!-proc)))

  (define path-selected?
    (lambda (path value)
      (guard [exn [(nav-absent-focus? exn) #f]]
        ;; Use a one-shot continuation to stop traversal after the first focus.
        (call/1cc
         (lambda (return)
           (select-path path value (lambda (child) (return #t)) #t)
           #f)))))

  ;; Only steps inside the wrapped traversal are optional. Its continuation belongs
  ;; to the caller, so protect its failures and restore the caller's optional scope.
  ;; For (nav/path (nav/maybe inner) suffix), absence inside inner is optional;
  ;; absence in suffix is governed by the caller's scope. The terminal callback
  ;; crosses this boundary even when inner delegates through another public call.
  (define optional-focus
    (lambda (run callback)
      (let ([outer-optional? (current-optional-focus?)])
        (parameterize ([current-optional-focus? #t])
          (run (lambda (value)
                 (parameterize ([current-optional-focus? outer-optional?])
                   (nav-call-user callback value))))))))

  #|proc:nav/pred
  The `nav/pred` procedure returns a navigator that focuses the current value
  only when `predicate` returns true for that value. Its signature is `(value) -> boolean`.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/pred
    (lambda (predicate)
      (pcheck ([procedure? predicate])
              (make-conditional-nav
               'nav/pred '(nav/pred)
               (lambda (value emit)
                 (when (nav-call-user predicate value)
                   (emit value)))
               (lambda (value update)
                 (if (nav-call-user predicate value) (update value) value))
               (lambda (value update!)
                 (if (nav-call-user predicate value) (update! value) value))
               (lambda (value clear)
                 (if (nav-call-user predicate value) (clear value) value))
               (lambda (value clear!)
                 (if (nav-call-user predicate value) (clear! value) value))))))

  #|proc:nav/not-pred
  The `nav/not-pred` procedure returns a navigator that focuses the current
  value only when `predicate` returns false for that value.
  `predicate` has signature `(value) -> boolean`.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/not-pred
    (lambda (predicate)
      (pcheck ([procedure? predicate])
              (nav/pred (lambda (value) (not (nav-call-user predicate value)))))))

  #|proc:nav/maybe
  The `nav/maybe` procedure wraps `path` so each absent or unsupported focus is skipped.
  Successful sibling focuses are retained in selection, transformation, and clearing.
  Errors from user callbacks or later path steps propagate normally. The result is a navigator.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/maybe
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-conditional-nav
         'nav/maybe '(nav/maybe)
         (lambda (value emit)
           (optional-focus (lambda (callback) (select-path compiled value callback))
                           emit))
         (lambda (value update)
           (optional-focus (lambda (callback) (transform-path compiled callback value))
                           update))
         (lambda (value update!)
           (optional-focus (lambda (callback) (transform-path! compiled callback value))
                           update!))
         (lambda (value clear)
           (optional-focus (lambda (callback) (clear-path compiled callback value))
                           clear))
         (lambda (value clear!)
           (optional-focus (lambda (callback) (clear-path! compiled callback value))
                           clear!))))))

  ;; Count focuses during the operation itself so stateful callbacks run only once.
  (define require-focus
    (lambda (run callback)
      (let ([selected? #f])
        (let ([result (run (lambda (value)
                             (set! selected? #t)
                             (callback value)))])
          (unless selected?
            (nav-error 'nav/must "required path selected no values"))
          result))))

  #|proc:nav/must
  The `nav/must` procedure wraps `path` so selecting no values signals an error.
  Each operation checks its own traversal without a separate selection pass.
  The result is a navigator.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/must
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-conditional-nav
         'nav/must '(nav/must)
         (lambda (value emit)
           (require-focus (lambda (callback) (select-path compiled value callback)) emit))
         (lambda (value update)
           (require-focus (lambda (callback) (transform-path compiled callback value)) update))
         (lambda (value update!)
           (require-focus (lambda (callback) (transform-path! compiled callback value)) update!))
         (lambda (value clear)
           (require-focus (lambda (callback) (clear-path compiled callback value)) clear))
         (lambda (value clear!)
           (require-focus (lambda (callback) (clear-path! compiled callback value)) clear!))))))

  #|proc:nav/if
  The `nav/if` procedure chooses `then-path` when `test-path` selects at least
  one value from the current focus, otherwise it chooses `else-path`.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/if
    (lambda (test-path then-path else-path)
      (let ([test (nav-compile-path test-path)]
            [then (nav-compile-path then-path)]
            [else-path (nav-compile-path else-path)])
        (define choose
          (lambda (value)
            (if (path-selected? test value) then else-path)))
        (make-conditional-nav
         'nav/if '(nav/if)
         (lambda (value emit)
           (select-path (choose value) value emit))
         (lambda (value update)
           (transform-path (choose value) update value))
         (lambda (value update!)
           (transform-path! (choose value) update! value))
         (lambda (value clear)
           (clear-path (choose value) clear value))
         (lambda (value clear!)
           (clear-path! (choose value) clear! value))))))

  #|proc:nav/when
  The `nav/when` procedure chooses `then-path` when `test-path` selects at
  least one value; otherwise it selects nothing.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/when
    (lambda (test-path then-path)
      (nav/if test-path then-path nav/none)))

  #|proc:nav/unless
  The `nav/unless` procedure chooses `then-path` when `test-path` selects no
  values; otherwise it selects nothing.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/unless
    (lambda (test-path then-path)
      (nav/if test-path nav/none then-path)))

  #|proc:nav/multi-path
  The `nav/multi-path` procedure returns a navigator that focuses all values
  selected by each path in `paths`, preserving path order.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/multi-path
    (lambda paths
      (let ([compiled (map nav-compile-path paths)])
        (make-conditional-nav
         'nav/multi-path '(nav/multi-path)
         (lambda (value emit)
           (for-each (lambda (path) (select-path path value emit)) compiled))
         (lambda (value update)
           (let loop ([paths compiled] [value value])
             (if (null? paths)
                 value
                 (loop (cdr paths) (transform-path (car paths) update value)))))
         (lambda (value update!)
           (let loop ([paths compiled] [value value])
             (if (null? paths)
                 value
                 (loop (cdr paths) (transform-path! (car paths) update! value)))))
         (lambda (value clear)
           (let loop ([paths compiled] [value value])
             (if (or (null? paths) (nav-missing? value))
                 value
                 (loop (cdr paths) (clear-path (car paths) clear value)))))
         (lambda (value clear!)
           (let loop ([paths compiled] [value value])
             (if (or (null? paths) (nav-missing? value))
                 value
                 (loop (cdr paths) (clear-path! (car paths) clear! value)))))))))

  ;; Try each path using the operation itself. Once a path yields a focus its
  ;; continuation is used immediately; predicates/getters are not called again.
  (define run-choice
    (lambda (paths run callback value)
      (let loop ([paths paths])
        (if (null? paths)
            value
            (let ([selected? #f])
              (let ([result (optional-focus
                             (lambda (continue) (run (car paths) continue value))
                             (lambda (child) (set! selected? #t) (callback child)))])
                (if selected? result (loop (cdr paths)))))))))

  #|proc:nav/choice
  The `nav/choice` procedure returns a navigator that uses the first path in
  `paths` that yields a focus during the current operation. Each candidate is traversed
  once. Absent focuses are skipped; successful sibling focuses of the chosen path remain.
  Kind: Reusable conditional step combinator.
  |#
  (define nav/choice
    (lambda paths
      (let ([compiled (map nav-compile-path paths)])
        (make-conditional-nav
         'nav/choice '(nav/choice)
         (lambda (value emit)
           (run-choice compiled (lambda (path emit value) (select-path path value emit))
                       emit value))
         (lambda (value update)
           (run-choice compiled transform-path update value))
         (lambda (value update!)
           (run-choice compiled transform-path! update! value))
         (lambda (value clear)
           (run-choice compiled clear-path clear value))
         (lambda (value clear!)
           (run-choice compiled clear-path! clear! value)))))))
