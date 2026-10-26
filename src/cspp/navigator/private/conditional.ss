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

  ;; A missing focus in a wrapped path is optional; a downstream failure is not.
  (define optional-focus
    (lambda (run callback default)
      (let ([downstream-condition #f])
        (guard (exn [(and (nav-absent-focus? exn)
                          (not (eq? exn downstream-condition))) default])
          (run (lambda (value)
                 (guard (exn [else (set! downstream-condition exn) (raise exn)])
                   (callback value))))))))

  #|proc:nav/pred
  The `nav/pred` procedure returns a navigator that focuses the current value
  only when `predicate` returns true for that value.
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
  |#
  (define nav/not-pred
    (lambda (predicate)
      (pcheck ([procedure? predicate])
              (nav/pred (lambda (value) (not (nav-call-user predicate value)))))))

  #|proc:nav/maybe
  The `nav/maybe` procedure wraps `path` so absent or unsupported focuses become no-ops.
  Errors from user callbacks or later path steps propagate normally. The result is a navigator.
  |#
  (define nav/maybe
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-conditional-nav
         'nav/maybe '(nav/maybe)
         (lambda (value emit)
           (optional-focus (lambda (callback) (select-path compiled value callback))
                           emit (void)))
         (lambda (value update)
           (optional-focus (lambda (callback) (transform-path compiled callback value))
                           update value))
         (lambda (value update!)
           (optional-focus (lambda (callback) (transform-path! compiled callback value))
                           update! value))
         (lambda (value clear)
           (optional-focus (lambda (callback) (clear-path compiled callback value))
                           clear value))
         (lambda (value clear!)
           (optional-focus (lambda (callback) (clear-path! compiled callback value))
                           clear! value))))))

  #|proc:nav/must
  The `nav/must` procedure wraps `path` so selecting no values signals an error.
  |#
  (define nav/must
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-conditional-nav
         'nav/must '(nav/must)
         (lambda (value emit)
           (let ([count 0])
             (select-path compiled value
                          (lambda (child)
                            (set! count (+ count 1))
                            (emit child)))
             (when (= count 0)
               (nav-error 'nav/must "required path selected no values"))))
         (lambda (value update)
           (if (path-selected? compiled value)
               (transform-path compiled update value)
               (nav-error 'nav/must "required path selected no values")))
         (lambda (value update!)
           (if (path-selected? compiled value)
               (transform-path! compiled update! value)
               (nav-error 'nav/must "required path selected no values")))
         (lambda (value clear)
           (if (path-selected? compiled value)
               (clear-path compiled clear value)
               (nav-error 'nav/must "required path selected no values")))
         (lambda (value clear!)
           (if (path-selected? compiled value)
               (clear-path! compiled clear! value)
               (nav-error 'nav/must "required path selected no values")))))))

  #|proc:nav/if
  The `nav/if` procedure chooses `then-path` when `test-path` selects at least
  one value from the current focus, otherwise it chooses `else-path`.
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
  |#
  (define nav/when
    (lambda (test-path then-path)
      (nav/if test-path then-path nav/none)))

  #|proc:nav/unless
  The `nav/unless` procedure chooses `then-path` when `test-path` selects no
  values; otherwise it selects nothing.
  |#
  (define nav/unless
    (lambda (test-path then-path)
      (nav/if test-path nav/none then-path)))

  #|proc:nav/multi-path
  The `nav/multi-path` procedure returns a navigator that focuses all values
  selected by each path in `paths`, preserving path order.
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

  #|proc:nav/choice
  The `nav/choice` procedure returns a navigator that uses the first path in
  `paths` that selects at least one value.
  |#
  (define nav/choice
    (lambda paths
      (let ([compiled (map nav-compile-path paths)])
        (define choose
          (lambda (value)
            (let loop ([paths compiled])
              (cond [(null? paths) nav/none]
                    [(path-selected? (car paths) value) (car paths)]
                    [else (loop (cdr paths))]))))
        (make-conditional-nav
         'nav/choice '(nav/choice)
         (lambda (value emit)
           (select-path (choose value) value emit))
         (lambda (value update)
           (transform-path (choose value) update value))
         (lambda (value update!)
           (transform-path! (choose value) update! value))
         (lambda (value clear)
           (clear-path (choose value) clear value))
         (lambda (value clear!)
           (clear-path! (choose value) clear! value)))))))
