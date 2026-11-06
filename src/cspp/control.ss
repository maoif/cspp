(library (cspp control)
  (export forever
          compose compose1 >>> >>>1
          sect sect+
          let/cc let/1cc)
  (import (chezscheme)
          (cspp utils))

  ;; User examples and multiple-value contracts: docs/control-utils.md.
  (define $procedures? (lambda (procedures) (andmap procedure? procedures)))

  #|macro:forever
  The `forever` macro repeats its body until the implicitly bound `(break value ...)` escapes,
  returning those values. The optional `(index identifier)` binds a zero-based iteration count.
  Body expressions execute in order and may share mutable state with their enclosing scope.
  |#
  (define-syntax forever
    (lambda (stx)
      (syntax-case stx ()
        [(k (index i) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'i))
         (with-implicit (k break)
           #'(call/1cc
              (lambda (break)
                (let loop ([i 0])
                  (begin e e* ...)
                  (loop (add1 i))))))]
        [(k e e* ...)
         (with-implicit (k break)
           #'(call/1cc
              (lambda (break)
                (let loop ()
                  (begin e e* ...)
                  (loop)))))])))


  #|proc:compose
  The `compose` procedure returns the composition of `proc` and the remaining `proc*` procedures.
  The returned procedure accepts the rightmost procedure's arguments and applies procedures
  right to left. Each procedure consumes all values returned by its right neighbor; the leftmost
  procedure determines the final return values. A single procedure is returned unchanged.
  Callback signatures must match these input and output arities. Captured state is shared;
  callers must synchronize any mutable state when invoking the composition from multiple threads.
  |#
  (define-who compose
    (lambda (proc . proc*)
      (pcheck-who who ([procedure? proc] [$procedures? proc*])
        (if (null? proc*)
            proc
            (let ([p* (cons proc proc*)])
              (lambda args
                (let loop ([p* p*])
                  (if (null? p*)
                      (apply values args)
                      ;; Recurse to the last procedure first.
                      (call-with-values (lambda () (loop (cdr p*))) (car p*))))))))))


  #|proc:compose1
  The `compose1` procedure returns the right-to-left composition of `proc` and remaining `proc*`.
  The rightmost callback accepts the returned procedure's arguments. Every callback except the
  leftmost returns one value; every callback except the rightmost accepts one value. The leftmost
  callback may return multiple values. A single callback is returned unchanged.
  Callers synchronize shared mutable callback state when using the composition across threads.
  |#
  (define-who compose1
    (lambda (proc . proc*)
      (pcheck-who who ([procedure? proc] [$procedures? proc*])
        (if (null? proc*)
            proc
            (let ([procedures (cons proc proc*)])
              (lambda args
                (let loop ([procedures procedures])
                  (if (null? (cdr procedures))
                      (apply (car procedures) args)
                      ((car procedures) (loop (cdr procedures)))))))))))


  #|proc:>>>
  The `>>>` procedure passes `v` through `proc` and remaining `proc*` callbacks left to right.
  The first callback accepts one argument. Each later callback accepts all values returned by
  its predecessor. The final callback determines the returned values.
  |#
  (define-who >>>
    (lambda (v proc . proc*)
      (pcheck-who who ([procedure? proc] [$procedures? proc*])
        (if (null? proc*)
            (proc v)
            (let loop ([p* (reverse proc*)])
              (if (null? p*)
                  (proc v)
                  (call-with-values (lambda () (loop (cdr p*))) (car p*))))))))


  #|proc:>>>1
  The `>>>1` procedure passes `v` through `proc` and remaining `proc*` callbacks left to right.
  Each callback accepts one argument; every callback except the last returns one value.
  The last callback determines the returned values.
  |#
  (define-who >>>1
    (lambda (v proc . proc*)
      (pcheck-who who ([procedure? proc] [$procedures? proc*])
        (if (null? proc*)
            (proc v)
            (let loop ([p* proc*] [res (proc v)])
              (if (null? (cdr p*))
                  ((car p*) res)
                  (loop (cdr p*) ((car p*) res))))))))


  (define-syntax $sect
    (lambda (stx)
      (define parse-arguments
        (lambda (expressions curried?)
          (let loop ([expressions expressions] [slots '()] [bindings '()] [arguments '()])
            (cond
              [(null? expressions)
               (values (reverse slots) (reverse bindings) (reverse arguments) #f)]
              [(eq? (syntax->datum (car expressions)) '...)
               (if (null? (cdr expressions))
                   (values (reverse slots) (reverse bindings) (reverse arguments) #t)
                   (syntax-error expressions (if curried? "sect+" "sect")
                                 ": ... must appear last:"))]
              [else
               (let ([name (car (generate-temporaries '(argument)))])
                 (if (eq? (syntax->datum (car expressions)) '_)
                     (loop (cdr expressions) (cons name slots) bindings (cons name arguments))
                     (loop (cdr expressions) slots
                           (cons (list name (car expressions)) bindings)
                           (cons name arguments))))]))))
      (syntax-case stx ()
        [(k ?curried proc e ...)
         (let-values ([(slots bindings arguments dots?)
                       (parse-arguments #'(e ...) (datum ?curried))])
           (with-syntax ([(slot ...) slots]
                         [([capture expression] ...) bindings]
                         [(argument ...) arguments]
                         [(p rest) (generate-temporaries '(proc rest))])
             (if (datum ?curried)
                 #`(let ([who 'sect+] [p proc] [capture expression] ...)
                     (pcheck-who who ([procedure? p])
                       #,(let gen-lambdas ([slots slots])
                           (if (null? slots)
                               (if dots?
                                   #'(lambda rest (apply p argument ... rest))
                                   #'(p argument ...))
                               #`(lambda (#,(car slots))
                                   #,(gen-lambdas (cdr slots)))))))
                 #`(let ([who 'sect] [p proc] [capture expression] ...)
                     (pcheck-who who ([procedure? p])
                       #,(if dots?
                             #'(lambda (slot ... . rest) (apply p argument ... rest))
                             #'(lambda (slot ...) (p argument ...))))))))])))

  #|macro:sect
  The `sect` macro returns a procedure with one parameter per `_` placeholder in its arguments.
  It evaluates `proc` and fixed argument expressions once when creating the procedure and captures
  their values. Calling the result fills placeholders left to right and calls `proc` with the
  completed arguments, returning its values. A final `...` accepts additional trailing arguments.
  The callback must accept the completed argument list. Captured mutable objects remain shared.
  With no arguments after `proc`, the result calls `proc` with zero arguments.
  A nonprocedure `proc` is rejected when constructing the section.
  |#
  (define-syntax sect
    (syntax-rules ()
      [(_ proc e ...) ($sect #f proc e ...)]))
  #|macro:sect+
  The `sect+` macro creates a curried section of `proc`, with one unary procedure per `_` slot.
  Fixed argument expressions and `proc` are evaluated and captured once. Supplying all slots calls
  `proc` with the completed argument list and returns its values. A final `...` produces a
  variadic procedure after all unary slots. With no slots and no `...`, `proc` runs immediately.
  The callback must accept the completed arguments. Captured mutable objects remain shared.
  A nonprocedure `proc` is rejected before the curried section is returned.
  |#
  (define-syntax sect+
    (syntax-rules ()
      [(_ proc e ...) ($sect #t proc e ...)]))


  #|macro:let/cc
  The `let/cc` macro evaluates its body with `k` bound to its current continuation.
  Calling `(k value ...)` returns those values from the form; normal completion returns the
  body's values. The continuation can be invoked repeatedly and has capture/resumption overhead.
  |#
  (define-syntax let/cc
    (lambda (stx)
      (syntax-case stx ()
        [(_ k e e* ...)
         (identifier? #'k)
         ;; call/cc intentionally provides a reusable continuation to the macro's body.
         #'(call/cc (lambda (k) e e* ...))])))


  #|macro:let/1cc
  The `let/1cc` macro evaluates its body with `k` bound to a one-shot current continuation.
  Calling `(k value ...)` returns those values from the form; normal completion returns the
  body's values. The continuation may be invoked at most once.
  |#
  (define-syntax let/1cc
    (lambda (stx)
      (syntax-case stx ()
        [(_ k e e* ...)
         (identifier? #'k)
         #'(call/1cc (lambda (k) e e* ...))])))


  )
