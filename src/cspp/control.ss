(library (cspp control)
  (export forever
          compose compose1 >>> >>>1
          sect sect+
          let/cc let/1cc)
  (import (chezscheme)
          (cspp utils)
          (cspp internal)
          (cspp list))

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
      (pcheck-proc (proc)
                   (if (null? proc*)
                       proc
                       (if (andmap procedure? proc*)
                           (let ([p* (cons proc proc*)])
                             (lambda args
                               (let loop ([p* p*])
                                 (if (null? p*)
                                     (apply values args)
                                     ;; recurse to the last proc first
                                     (call-with-values (lambda () (loop (cdr p*)))
                                       (car p*))))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|proc:compose1
  The `compose1` procedure returns the right-to-left composition of `proc` and remaining `proc*`.
  The rightmost callback accepts the returned procedure's arguments. Every callback except the
  leftmost returns one value; every callback except the rightmost accepts one value. The leftmost
  callback may return multiple values. A single callback is returned unchanged.
  Callers synchronize shared mutable callback state when using the composition across threads.
  |#
  (define-who compose1
    (lambda (proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       proc
                       (if (andmap procedure? proc*)
                           (let* ([ps (cons proc proc*)]
                                  [p* (list-head ps (sub1 (length ps)))]
                                  [p1 (list-ref ps (sub1 (length ps)))])
                             (lambda args
                               (let loop ([p* p*])
                                 (if (null? p*)
                                     (apply p1 args)
                                     ((car p*) (loop (cdr p*)))))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|proc:>>>
  The `>>>` procedure passes `v` through `proc` and remaining `proc*` callbacks left to right.
  The first callback accepts one argument. Each later callback accepts all values returned by
  its predecessor. The final callback determines the returned values.
  |#
  (define-who >>>
    (lambda (v proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       (proc v)
                       (if (andmap procedure? proc*)
                           (let loop ([p* (reverse proc*)])
                             (if (null? p*)
                                 (proc v)
                                 (call-with-values (lambda () (loop (cdr p*)))
                                   (car p*))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|proc:>>>1
  The `>>>1` procedure passes `v` through `proc` and remaining `proc*` callbacks left to right.
  Each callback accepts one argument; every callback except the last returns one value.
  The last callback determines the returned values.
  |#
  (define-who >>>1
    (lambda (v proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       (proc v)
                       (if (andmap procedure? proc*)
                           (let loop ([p* proc*] [res (proc v)])
                             (if (null? (cdr p*))
                                 ((car p*) res)
                                 (loop (cdr p*) ((car p*) res))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  (define-syntax $sect
    (lambda (stx)
      (define categorize-args
        (lambda (args ?curried)
          (let loop ([arg* (syntax->datum args)] [args args] [nslots 0] [rres '()] [dots? #f])
            (if (null? arg*)
                (values nslots (reverse rres) dots?)
                (let ([arg (car arg*)])
                  (cond
                   [(eq? arg '...)
                    (if (null? (cdr arg*))
                        (loop (cdr arg*) (cdr args) nslots (cons '(dots) rres) #t)
                        (syntax-error arg* (if ?curried "sect+" "sect") ": ... must appear last:"))]
                   [(eq? arg '_)
                    (loop (cdr arg*) (cdr args) (add1 nslots) (cons '(slot) rres) dots?)]
                   [else (loop (cdr arg*) (cdr args) nslots (cons (cons 'expr (car args)) rres) dots?)]))))))
      (syntax-case stx ()
        [(k ?curried proc e e* ...)
         ;; res*: ('expr . e) | '(slot) | '(dots)
         (let-values ([(nslots res* dots?) (categorize-args #'(e e* ...) (datum ?curried))])
           ;; x*: params for lambdas
           ;; p: bound to given procedure
           ;; varg*: bound to e*
           (with-syntax ([(x* ...) (generate-temporaries (iota nslots))]
                         [(p xdot) (generate-temporaries '(proc xdot))]
                         [(((varg*) e*) ...) (map
                                              (lambda (p) (list (generate-temporaries '(arg)) (cdr p)))
                                              (filter (lambda (x) (eq? 'expr (car x))) res*))])
             ;; generate the args that are eventually fed to the procedure
             ;; args: either given expressions bound in let (varg*) or formal parameters (x*)
             (with-syntax ([(args ...)
                            (let loop ([res* res*] [varg* #'(varg* ...)] [x* #'(x* ...)] [rres '()])
                              (if (null? res*)
                                  (reverse rres)
                                  (case (caar res*)
                                    [(slot) (loop (cdr res*) varg* (cdr x*) (cons (car x*) rres))]
                                    [(expr) (loop (cdr res*) (cdr varg*) x* (cons (car varg*) rres))]
                                    ;; don't care
                                    [(dots) (reverse rres)])))])
               (if (datum ?curried)
                   #`(let ([p proc] [varg* e*] ...)
                       (pcheck ([procedure? p])
                         #,(let gen-lambs ([x* #'(x* ...)])
                             (if (null? x*)
                                 (if dots?
                                     #`(lambda xdot
                                         (apply p args ... xdot))
                                     #'(p args ...))
                                 #`(lambda (#,(car x*))
                                     #,(gen-lambs (cdr x*)))))))
                   #`(let ([p proc] [varg* e*] ...)
                       (pcheck ([procedure? p])
                         #,(if dots?
                               #'(lambda (x* ... . xdot)
                                   (apply p args ... xdot))
                               #'(lambda (x* ...)
                                   (p args ...)))))))))])))

  #|macro:sect
  The `sect` macro returns a procedure with one parameter per `_` placeholder in its arguments.
  It evaluates `proc` and fixed argument expressions once when creating the procedure and captures
  their values. Calling the result fills placeholders left to right and calls `proc` with the
  completed arguments, returning its values. A final `...` accepts additional trailing arguments.
  The callback must accept the completed argument list. Captured mutable objects remain shared.
  A nonprocedure `proc` is rejected when constructing the section.
  |#
  (define-syntax sect
    (syntax-rules ()
      [(_ proc e e* ...) ($sect #f proc e e* ...)]))
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
      [(_ proc e e* ...) ($sect #t proc e e* ...)]))


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
