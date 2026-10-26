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

  (define emit-children
    (lambda (value emit)
      (cond [(and (pair? value) (not (alist? value)))
             (when (list? value)
               (for-each emit value))]
            [(indexed-supported? value)
             (let ([len (indexed-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (indexed-ref/missing value i))
                   (loop (+ i 1)))))]
            [(keyed-values value)
             => (lambda (values) (for-each emit values))]
            [else (void)])))

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

  (define remove-missing-values
    (lambda (items)
      (let loop ([items items])
        (cond [(null? items) '()]
              [(nav-missing? (car items)) (loop (cdr items))]
              [else (cons (car items) (loop (cdr items)))]))))

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
  focuses collection descendants in tree order.
  |#
  (define nav/children
    (make-tree-nav
     'nav/children 'nav/children
     (lambda (value emit)
       (emit-children value emit))
     (lambda (value update)
       (cond [(and (pair? value) (list? value))
              (remove-missing-values (map update value))]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0] [value value])
                  (if (= i len)
                      value
                      (let* ([old (indexed-ref/missing value i)]
                             [new-value (update old)])
                        (loop (+ i 1)
                              (if (nav-missing? new-value)
                                  value
                                  (indexed-set value i new-value)))))))]
             [(keyed-entries value)
              => (lambda (entries)
                   (let loop ([entries entries] [value value])
                     (if (null? entries)
                         value
                         (let ([new-value (update (cdar entries))])
                           (loop (cdr entries)
                                 (if (nav-missing? new-value)
                                     (keyed-delete value (caar entries))
                                     (keyed-set value (caar entries) new-value)))))))]
             [else value]))
     (lambda (value update!)
       (cond [(and (pair? value) (list? value))
              (let loop ([xs value])
                (unless (null? xs)
                  (let ([new-value (update! (car xs))])
                    (unless (nav-missing? new-value)
                      (set-car! xs new-value)))
                  (loop (cdr xs))))
              value]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (let ([new-value (update! (indexed-ref/missing value i))])
                      (unless (nav-missing? new-value)
                        (indexed-set! value i new-value)))
                    (loop (+ i 1)))))
              value]
             [(keyed-entries value)
              => (lambda (entries)
                   (for-each
                    (lambda (entry)
                      (let ([new-value (update! (cdr entry))])
                        (if (nav-missing? new-value)
                            (keyed-delete! value (car entry))
                            (keyed-set! value (car entry) new-value))))
                    entries)
                   value)]
             [else value]))
     (lambda (value clear)
       (cond [(and (pair? value) (list? value))
              (remove-missing-values
               (map clear value))]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0] [result value])
                  (if (fx= i len)
                      result
                      (let ([child (clear (indexed-ref/missing value i))])
                        (when (nav-missing? child)
                          (nav-error 'nav-clearval "cannot remove indexed child from: ~s" value))
                        (loop (fx+ i 1) (indexed-set result i child))))))]
             [(keyed-entries value)
              => (lambda (entries)
                   (let loop ([entries entries] [value value])
                     (if (null? entries)
                         value
                         (let ([child (clear (cdar entries))])
                           (loop (cdr entries)
                                 (if (nav-missing? child)
                                     (keyed-delete value (caar entries))
                                     (keyed-set value (caar entries) child)))))))]
             [else value]))
     (lambda (value clear!)
       (cond [(and (pair? value) (list? value))
              (let loop ([items value])
                (unless (null? items)
                  (let ([child (clear! (car items))])
                    (when (nav-missing? child)
                      (nav-error 'nav-clearval! "cannot remove list child in place: ~s" value))
                    (set-car! items child))
                  (loop (cdr items))))
              value]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (let ([child (clear! (indexed-ref/missing value i))])
                      (when (nav-missing? child)
                        (nav-error 'nav-clearval! "cannot remove indexed child from: ~s" value))
                      (indexed-set! value i child))
                    (loop (+ i 1)))))
              value]
             [(keyed-entries value)
              => (lambda (entries)
                   (for-each
                    (lambda (entry)
                      (let ([child (clear! (cdr entry))])
                        (if (nav-missing? child)
                            (keyed-delete! value (car entry))
                            (keyed-set! value (car entry) child))))
                    entries)
                   value)]
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
  The `nav/before` procedure returns a navigator that focuses the current value
  before values selected by `path`.
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
  The `nav/after` procedure returns a navigator that focuses values selected by
  `path` before focusing the current value.
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
