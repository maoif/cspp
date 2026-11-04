#!chezscheme
(library (cspp utils)
  (export pcase pcheck pcheck-pair pcheck-list pcheck-string pcheck-char pcheck-proc
          pcheck-hashtable pcheck-vector pcheck-port pcheck-binary-port pcheck-textual-port
          pcheck-open-port pcheck-open-binary-port pcheck-open-textual-port
          pcheck-input-binary-port  pcheck-input-textual-port
          pcheck-output-binary-port pcheck-output-textual-port
          pcheck-file pcheck-directory pcheck-symlink
          pcheck-fxvector pcheck-flvector pcheck-bytevector pcheck-natural
          unreachable!
          incr! decr! add1! sub1! fx+! fx-! fx*! fx/!
          cons! cdr!
          natural? positive-natural?
          neq? neqv? nequal?
          id bool octal hex bin
          define-who trace-define-who

          p/and p/or p/not

          procedure-arity procedure-variable-arity? procedure-name

          random-char random-string random-symbol random-datum random-list random-box
          random-bytevector random-u8vec

          define-flags flag-any-set? flag-all-set? flag-set flag-reset)
  (import (chezscheme)
          (cspp internal))

  #|proc:id
  The `id` procedure returns its arbitrary `value` argument unchanged.
  |#
  (define id (lambda (value) value))

  #|macro:pcase
  The `pcase` macro tests the object bound to `id` against clause predicates in order.
  Each predicate has signature `object -> object`; any nonfalse result selects the clause.
  A clause may list alternative predicates. The selected body's values are returned.
  An `else` clause supplies the fallback; without it, failure raises an error.
  |#
  (define-syntax pcase
    (lambda (stx)
      (syntax-case stx (else)
        [(_ id cl cl* ... [else el el* ...])
         (identifier? #'id)
         (let loop ([cl* #'(cl cl* ...)])
           (if (null? cl*)
               #'(begin el el* ...)
               (syntax-case (car cl*) ()
                 [((pred preds ...) e e* ...)
                  (with-syntax ([(p pds ...) (generate-temporaries #'(pred preds ...))])
                    #`(let ([p pred] [pds preds] ...)
                        (if (or (p id) (pds id) ...)
                            (begin e e* ...)
                            #,(loop (cdr cl*)))))]
                 [(pred e e* ...)
                  (with-syntax ([(p) (generate-temporaries '(p))])
                    #`(let ([p pred])
                        (if (p id)
                            (begin e e* ...)
                            #,(loop (cdr cl*)))))])))]
        [(k id cl cl* ...)
         (identifier? #'id)
         #'(pcase id cl cl* ...
                  [else (errorf #f "failed ~a: no predicates are satisfied for ~a" 'k 'id)])]
        [_ (syntax-error stx "invalid pcase form:")])))


  #|macro:pcheck
  The `pcheck` macro checks bound argument identifiers using clauses `[predicate id ...]`.
  Each predicate has signature `object -> object`; a false result raises an argument error.
  Checks run in clause order before the body, whose values are returned on success.
  Use this macro at API argument boundaries; each predicate expression is evaluated once per clause.
  |#
  (define-syntax pcheck
    (lambda (stx)
      (define all-ids?
        (lambda (ls*)
          (andmap
           (lambda (ls)
             (andmap identifier? ls))
           ls*)))
      (syntax-case stx ()
        [(k () e e* ...)
         #'(begin e e* ...)]
        [(k ([pred x x* ...] ...) e e* ...)
         (all-ids? #'((x x* ...) ...))
         (let loop ([preds #'(pred ...)] [id** #'((x x* ...) ...)])
           (if (null? preds)
               #`(let () e e* ...)
               (with-syntax ([pred (car preds)] [ids (car id**)]
                             [(p) (generate-temporaries '(p))])
                 #`(let ([p pred])
                     #,(let f ([id* #'ids])
                         (if (null? id*)
                             (loop (cdr preds) (cdr id**))
                             #`(if (p #,(car id*))
                                   #,(f (cdr id*))
                                   (errorf 'k "failed ~a ~a: ~a, value: ~a" 'k 'pred '#,(car id*) #,(car id*)))))))))]
        [_ (syntax-error stx "invalid pcheck form:")])))

  (define-syntax gen-pcheck
    (lambda (stx)
      (syntax-case stx ()
        [(k name pred)
         (with-syntax ([pcheck-name ($construct-name #'k "pcheck-" #'name)])
           #`(define-syntax pcheck-name
               (lambda (s)
                 (syntax-case s ()
                   [(_ (id id* (... ...)) e e* (... ...))
                    #'(pcheck ([pred id id* (... ...)])
                              e e* (... ...))]))))])))

  (define open-port? (lambda (p) (and (port? p) (not (port-closed? p)))))
  (define open-textual-port? (lambda (p) (and (textual-port? p) (not (port-closed? p)))))
  (define open-binary-port?  (lambda (p) (and (binary-port? p) (not (port-closed? p)))))

  ;; just by default test for openness
  (define input-binary-port?
    (lambda (port) (and (input-port? port) (binary-port? port) (not (port-closed? port)))))
  (define input-textual-port?
    (lambda (port) (and (input-port? port) (textual-port? port) (not (port-closed? port)))))
  (define output-binary-port?
    (lambda (port) (and (output-port? port) (binary-port? port) (not (port-closed? port)))))
  (define output-textual-port?
    (lambda (port) (and (output-port? port) (textual-port? port) (not (port-closed? port)))))

  #|proc:natural?
  The `natural?` procedure returns whether `object` is an exact nonnegative integer.
  |#
  (define natural? (lambda (object) (and (integer? object) (exact? object) (>= object 0))))

  #|proc:positive-natural?
  The `positive-natural?` procedure returns whether `object` is an exact positive integer.
  |#
  (define positive-natural? (lambda (object) (and (natural? object) (positive? object))))

  #|macro:pcheck-proc
  The `pcheck-proc` macro validates each bound identifier in `(id ...)` as a procedure.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck proc procedure?)
  #|macro:pcheck-pair
  The `pcheck-pair` macro validates each bound identifier in `(id ...)` as a pair.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck pair pair?)
  #|macro:pcheck-list
  The `pcheck-list` macro validates each bound identifier in `(id ...)` as a proper list.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck list list?)
  (gen-pcheck number number?)
  (gen-pcheck fixnum fixnum?)
  (gen-pcheck flonum flonum?)
  #|macro:pcheck-natural
  The `pcheck-natural` macro validates bound identifiers `(id ...)` as exact nonnegative integers.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck natural natural?)
  #|macro:pcheck-vector
  The `pcheck-vector` macro validates each bound identifier in `(id ...)` as a vector.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck vector vector?)
  #|macro:pcheck-fxvector
  The `pcheck-fxvector` macro validates each bound identifier in `(id ...)` as a fixnum vector.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck fxvector fxvector?)
  #|macro:pcheck-flvector
  The `pcheck-flvector` macro validates each bound identifier in `(id ...)` as a flonum vector.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck flvector flvector?)
  #|macro:pcheck-bytevector
  The `pcheck-bytevector` macro validates each bound identifier in `(id ...)` as a bytevector.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck bytevector bytevector?)
  (gen-pcheck symbol symbol?)
  #|macro:pcheck-char
  The `pcheck-char` macro validates each bound identifier in `(id ...)` as a character.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck char char?)
  #|macro:pcheck-string
  The `pcheck-string` macro validates each bound identifier in `(id ...)` as a string.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck string string?)
  #|macro:pcheck-hashtable
  The `pcheck-hashtable` macro validates each bound identifier in `(id ...)` as a hashtable.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck hashtable hashtable?)

  #|macro:pcheck-port
  The `pcheck-port` macro validates each bound identifier in `(id ...)` as a port.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck port port?)
  (gen-pcheck input-port input-port?)
  (gen-pcheck output-port output-port?)
  #|macro:pcheck-binary-port
  The `pcheck-binary-port` macro validates bound identifiers `(id ...)` as binary ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck binary-port binary-port?)
  #|macro:pcheck-textual-port
  The `pcheck-textual-port` macro validates bound identifiers `(id ...)` as textual ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck textual-port textual-port?)
  #|macro:pcheck-open-port
  The `pcheck-open-port` macro validates bound identifiers `(id ...)` as ports that are open.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck open-port open-port?)
  #|macro:pcheck-open-binary-port
  The `pcheck-open-binary-port` macro validates bound identifiers `(id ...)` as open binary ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck open-binary-port open-binary-port?)
  #|macro:pcheck-open-textual-port
  The `pcheck-open-textual-port` macro validates bound identifiers `(id ...)` as open textual ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck open-textual-port open-textual-port?)

  #|macro:pcheck-input-binary-port
  The `pcheck-input-binary-port` macro checks `(id ...)` identifiers for open binary input ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck input-binary-port   input-binary-port?)
  #|macro:pcheck-input-textual-port
  The `pcheck-input-textual-port` macro checks `(id ...)` identifiers for open textual input ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck input-textual-port  input-textual-port?)
  #|macro:pcheck-output-binary-port
  The `pcheck-output-binary-port` macro checks `(id ...)` identifiers for open binary output ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck output-binary-port  output-binary-port?)
  #|macro:pcheck-output-textual-port
  The `pcheck-output-textual-port` macro checks `(id ...)` identifiers for open textual
  output ports.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck output-textual-port output-textual-port?)

  #|macro:pcheck-file
  The `pcheck-file` macro validates bound path identifiers `(id ...)` as existing regular files.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck file file-regular?)
  #|macro:pcheck-directory
  The `pcheck-directory` macro validates bound path identifiers `(id ...)` as existing directories.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck directory file-directory?)
  #|macro:pcheck-symlink
  The `pcheck-symlink` macro validates bound path identifiers `(id ...)` as symbolic links.
  It returns the body's values on success and raises an argument error otherwise.
  |#
  (gen-pcheck symlink file-symbolic-link?)


  #|proc:unreachable!
  The `unreachable!` procedure raises a continuable error indicating an unreachable code path.
  The optional `who` argument identifies the caller in the condition; without it no caller is set.
  Its return values are those supplied if a handler resumes the continuable exception.
  |#
  (define unreachable!
    (case-lambda
      [() (raise-continuable (condition (make-error)
                                        (make-message-condition "Should not reach here!")))]
      [(who) (raise-continuable (condition (make-error)
                                           (make-who-condition who)
                                           (make-message-condition "Should not reach here!")))]))


  #|macro:incr!
  The `incr!` macro increments the numeric variable `n` by one, returning unspecified values.
  The read/modify/write update is not atomic; callers synchronize concurrent access.
  |#
  (define-syntax incr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n) (identifier? #'n)
         #'(set! n (+ n 1))])))

  #|macro:decr!
  The `decr!` macro decrements the numeric variable `n` by one, returning unspecified values.
  The read/modify/write update is not atomic; callers synchronize concurrent access.
  |#
  (define-syntax decr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n) (identifier? #'n)
         #'(set! n (- n 1))])))

  #|macro:add1!
  The `add1!` macro increments numeric variable `n` by one. Its return value is unspecified.
  The update is not atomic; callers synchronize concurrent access.
  |#
  (define-syntax add1!
    (syntax-rules ()
      [(_ n) (incr! n)]))

  #|macro:sub1!
  The `sub1!` macro decrements numeric variable `n` by one. Its return value is unspecified.
  The update is not atomic; callers synchronize concurrent access.
  |#
  (define-syntax sub1!
    (syntax-rules ()
      [(_ n) (decr! n)]))

  (define-syntax $fixnum-update!
    (lambda (stx)
      (syntax-case stx ()
        [(_ operation target operand ...)
         (identifier? #'target)
         (with-syntax ([(current-value value ...)
                        (generate-temporaries #'(target operand ...))])
           #'(let ([current-value target] [value operand] ...)
               (pcheck ([fixnum? current-value value ...])
                 (set! target (operation current-value value ...)))))])))

  #|macro:fx+!
  The `fx+!` macro assigns `(fx+ n x ...)` to fixnum variable `n`, returning unspecified values.
  Each `x` is a fixnum addend. The update is not atomic and requires caller synchronization.
  Operands are evaluated once and checked; overflow raises before assigning `n`, even in release.
  |#
  (define-syntax fx+!
    (syntax-rules ()
      [(_ n x ...) ($fixnum-update! #2%fx+ n x ...)]))

  #|macro:fx-!
  The `fx-!` macro assigns `(fx- n x ...)` to fixnum variable `n`, returning unspecified values.
  Each `x` is a fixnum subtrahend. The update is not atomic and requires caller synchronization.
  Operands are evaluated once and checked; overflow raises before assigning `n`, even in release.
  |#
  (define-syntax fx-!
    (syntax-rules ()
      [(_ n x ...) ($fixnum-update! #2%fx- n x ...)]))

  #|macro:fx*!
  The `fx*!` macro assigns `(fx* n x ...)` to fixnum variable `n`, returning unspecified values.
  Each `x` is a fixnum factor. The update is not atomic and requires caller synchronization.
  Operands are evaluated once and checked; overflow raises before assigning `n`, even in release.
  |#
  (define-syntax fx*!
    (syntax-rules ()
      [(_ n x ...) ($fixnum-update! #2%fx* n x ...)]))

  #|macro:fx/!
  The `fx/!` macro assigns `(fx/ n x ...)` to fixnum variable `n`, returning unspecified values.
  Each `x` is a fixnum divisor. The update is not atomic and requires caller synchronization.
  Operands are evaluated once and checked; arithmetic errors raise before assignment in release.
  |#
  (define-syntax fx/!
    (syntax-rules ()
      [(_ n x ...) ($fixnum-update! #2%fx/ n x ...)]))

  #|macro:cons!
  The `cons!` macro sets variable `p` to a new pair with car `a` and cdr equal to its old value.
  Its return value is unspecified. Updates are not atomic; callers synchronize concurrent access.
  |#
  (define-syntax cons!
    (lambda (stx)
      (syntax-case stx ()
        [(_ a p) (identifier? #'p)
         #'(set! p (cons a p))])))

  #|macro:cdr!
  The `cdr!` macro sets pair-valued variable `v` to its cdr, returning unspecified values.
  The update is not atomic; callers synchronize concurrent access.
  |#
  (define-syntax cdr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ v) (identifier? #'v)
         #'(pcheck ([pair? v]) (set! v (cdr v)))])))


  #|macro:neq?
  The `neq?` macro returns the boolean negation of `eq?` applied to its object expressions.
  |#
  (define-syntax neq?
    (syntax-rules ()
      [(_ e* ...) (not (eq? e* ...))]))
  #|macro:neqv?
  The `neqv?` macro returns the boolean negation of `eqv?` applied to its object expressions.
  |#
  (define-syntax neqv?
    (syntax-rules ()
      [(_ e* ...) (not (eqv? e* ...))]))
  #|macro:nequal?
  The `nequal?` macro returns the boolean negation of `equal?` applied to its object expressions.
  |#
  (define-syntax nequal?
    (syntax-rules ()
      [(_ e* ...) (not (equal? e* ...))]))


  #|proc:bool
  The `bool` procedure returns `#f` when `object` is false and `#t` otherwise.
  |#
  (define bool
    (lambda (object) (if object #t #f)))


  #|proc:octal
  The `octal` procedure returns an octal string for `number`.
  The optional `readable?` selects the Scheme `#o` prefix when true (the default), or `0o`
  otherwise.
  |#
  (define octal
    (case-lambda
      [(number) (octal number #t)]
      [(number readable?)
       (pcheck ([number? number])
               (string-append (if readable? "#o" "0o") (number->string number 8)))]))
  #|proc:hex
  The `hex` procedure returns a hexadecimal string for `number`.
  The optional `readable?` selects the Scheme `#x` prefix when true (the default), or `0x`
  otherwise.
  |#
  (define hex
    (case-lambda
      [(number) (hex number #t)]
      [(number readable?)
       (pcheck ([number? number])
               (string-append (if readable? "#x" "0x") (number->string number 16)))]))
  #|proc:bin
  The `bin` procedure returns a binary string for `number`.
  The optional `readable?` selects the Scheme `#b` prefix when true (the default), or `0b`
  otherwise.
  |#
  (define bin
    (case-lambda
      [(number) (bin number #t)]
      [(number readable?)
       (pcheck ([number? number])
               (string-append (if readable? "#b" "0b") (number->string number 2)))]))


  #|macro:define-who
  The `define-who` macro defines `id` from an expression with `who` bound to the name symbol.
  The `(id . arguments)` form defines a procedure from body expressions. The `#(prefix id)` form
  defines the concatenated name and permits recursion through `id`.
  Definition values are unspecified.
  |#
  (define-syntax define-who
    (lambda (x)
      (syntax-case x ()
        [(k (id . args) b1 b2 ...)
         #'(k id (lambda args b1 b2 ...))]
        [(k #(prefix id) e)
         (and (identifier? #'prefix) (identifier? #'id))
         (with-implicit (k who)
           (with-syntax ([ext-id ($construct-name #'id #'prefix #'id)])
             #'(define ext-id (let ([who 'id]) (rec id e)))))]
        [(k id e)
         (identifier? #'id)
         (with-implicit (k who)
           #'(define id (let ([who 'id]) e)))])))

  #|macro:trace-define-who
  The `trace-define-who` macro defines a traced procedure named `id`, binding `who` to its symbol
  while evaluating the defining expression. Its `(id . arguments)` form accepts a procedure body.
  Calling the procedure traces its arguments and return values. Definition values are unspecified.
  |#
  (define-syntax trace-define-who
    (lambda (x)
      (syntax-case x ()
        [(k (id . args) b1 b2 ...)
         #'(k id (lambda args b1 b2 ...))]
        [(k id e)
         (identifier? #'id)
         (with-implicit (k who)
           #'(trace-define id (let ([who 'id]) e)))])))


  #|proc:p/and
  The `p/and` procedure returns a unary predicate combining its `procs` procedures in order.
  Each callback has signature `object -> object`. The result stops at the first false callback
  result, returning false, or returns the last result if all succeed; no callbacks yields `#t`.
  Callbacks and any captured mutable state are shared with the caller.
  |#
  (define-who p/and
    (lambda procs
      (pcheck ([(lambda (procedures) (andmap procedure? procedures)) procs])
              (lambda (x)
                (andmap (lambda (p) (p x)) procs)))))

  #|proc:p/or
  The `p/or` procedure returns a unary predicate combining its `procs` procedures in order.
  Each callback has signature `object -> object`. The result returns the first nonfalse callback
  result without evaluating later callbacks; no successful callback yields `#f`.
  Callbacks and any captured mutable state are shared with the caller.
  |#
  (define-who p/or
    (lambda procs
      (pcheck ([(lambda (procedures) (andmap procedure? procedures)) procs])
              (lambda (x)
                (ormap (lambda (p) (p x)) procs)))))

  #|proc:p/not
  The `p/not` procedure returns a unary predicate that negates the result of `proc`.
  The `proc` callback has signature `object -> object`; the returned predicate produces a boolean.
  |#
  (define-who p/not
    (lambda (proc)
      (pcheck ([procedure? proc])
              (lambda (x) (not (proc x))))))


  ;; (lambda (x) ...)
  ;; (lambda (x y) ...)
  ;; (lambda (x y . z) ...)
  ;; (lambda x* ...)
  ;; (case-lambda <lam> ...)

  #|proc:procedure-arity
  The `procedure-arity` procedure returns the arity accepted by `proc`. A fixed
  single arity is returned as a number. Multiple fixed arities are returned as
  a list of numbers. Variable arity is returned as a list ending in `*`, where
  the last number before `*` is the variable lower bound and earlier numbers are fixed arities.
  A procedure accepting every arity returns `(*)`; one accepting no arities returns `()`.
  The `proc` parameter is any procedure; it is inspected without calling it.
  |#
  (define procedure-arity
    (lambda (proc)
      (pcheck ([procedure? proc])
              (let ([m (procedure-arity-mask proc)])
                (if (>= m 0)
                    (let ([lb ($make-list-builder)])
                      (let loop ([m m] [i 0])
                        (if (= m 0)
                            (if (= 1 (length (lb)))
                                (car (lb))
                                (lb))
                            (begin (when (logbit? 0 m)
                                     (lb i))
                                   (loop (ash m -1) (add1 i))))))
                    (let ([lb ($make-list-builder)])
                      (let find-min ([i 0])
                        (if (= (ash m (- i)) -1)
                            (begin
                              (when (positive? i)
                                (let collect ([j 0])
                                  (when (<= j i)
                                    (when (logbit? j m)
                                      (lb j))
                                    (collect (add1 j)))))
                              (lb '*)
                              (lb))
                            (find-min (add1 i))))))))))


  #|proc:procedure-variable-arity?
  The `procedure-variable-arity?` procedure returns whether procedure `proc` accepts all argument
  counts above some lower bound. It inspects the procedure without calling it and returns a boolean.
  |#
  (define procedure-variable-arity?
    (lambda (proc)
      (pcheck ([procedure? proc])
              (let ([m (procedure-arity-mask proc)])
                (< m 0)))))


  #|proc:procedure-name
  The `procedure-name` procedure returns the name string of procedure `proc`, or `#f` if unnamed.
  The `proc` parameter is inspected without calling it.
  |#
  (define procedure-name
    (lambda (proc)
      (pcheck ([procedure? proc])
              (#%$procedure-name proc))))


;;;;===----------------------------------------------------------------------===
;;;; random data generators
;;;;===----------------------------------------------------------------------===

  (define $rand (lambda (lower-bound upper-bound)
                  (fx+ lower-bound (random (fx- upper-bound lower-bound)))))
  (define $range-error (lambda (who x y) (errorf who "invalid range: ~a ~a" x y)))

  (define $length-bound?
    (lambda (bound) (and (fixnum? bound) (fx>= bound 0))))

  #|proc:random-char
  The `random-char` procedure returns a uniformly selected Unicode scalar character.
  The character bounds `lower-bound` and `upper-bound` specify an inclusive lower bound
  and exclusive upper bound; surrogate code points are skipped without retries.
  With one bound the lower bound is NUL; with no bounds the range is space through `}`.
  |#
  (define-who random-char
    (case-lambda
      [()   (random-char #\space #\~)]
      [(upper-bound) (random-char #\nul upper-bound)]
      [(lower-bound upper-bound)
       (pcheck ([char? lower-bound upper-bound])
               (when (char>=? lower-bound upper-bound)
                 ($range-error who lower-bound upper-bound))
               (let* ([lower (char->integer lower-bound)]
                      [upper (char->integer upper-bound)]
                      [crosses-gap? (and (fx< lower #xD800) (fx>= upper #xE000))]
                      [value ($rand lower (if crosses-gap? (fx- upper #x800) upper))])
                 (integer->char
                  (if (and crosses-gap? (fx>= value #xD800))
                      (fx+ value #x800)
                      value))))]))

  #|proc:random-string
  The `random-string` procedure returns a string with a random length in
  [`lower-bound`, `upper-bound`), using nonnegative fixnum bounds in increasing order.
  The defaults are 0 and 10; a single bound supplies the upper bound.
  The optional `generator` has signature `() -> character` and is called once per character.
  Its default is `random-char`. Invalid callback results raise an error before string mutation.
  |#
  (define-who random-string
    (case-lambda
      [() (random-string random-char 0 10)]
      [(upper-bound) (random-string random-char 0 upper-bound)]
      [(lower-bound upper-bound) (random-string random-char lower-bound upper-bound)]
      [(generator lower-bound upper-bound)
       (pcheck ([procedure? generator] [$length-bound? lower-bound upper-bound])
               (when (fx>= lower-bound upper-bound)
                 ($range-error who lower-bound upper-bound))
               (let* ([length ($rand lower-bound upper-bound)] [result (make-string length)])
                 (do ([index 0 (fx+ index 1)]) ((fx= index length) result)
                   (let ([character (generator)])
                     (unless (char? character)
                       (errorf who "generator returned a non-character: ~s" character))
                     (string-set! result index character)))))]))

  #|proc:random-symbol
  The `random-symbol` procedure returns an interned symbol whose name length lies in
  [`lower-bound`, `upper-bound`), using nonnegative fixnum bounds in increasing order.
  The defaults are 1 and 10; a single bound supplies the upper bound.
  With no callback, `random-string` generates the name. The optional `generator` has signature
  `() -> string`; it is called once and must return a name within the requested length bounds.
  An invalid callback result raises an error without retrying the callback.
  |#
  (define-who random-symbol
    (case-lambda
      [() (random-symbol 1 10)]
      [(upper-bound) (random-symbol 1 upper-bound)]
      [(lower-bound upper-bound) (string->symbol (random-string lower-bound upper-bound))]
      [(generator lower-bound upper-bound)
       (pcheck ([procedure? generator] [$length-bound? lower-bound upper-bound])
               (when (fx>= lower-bound upper-bound)
                 ($range-error who lower-bound upper-bound))
               (let ([name (generator)])
                 (unless (string? name)
                   (errorf who "generator returned a non-string: ~s" name))
                 (unless (fx<= lower-bound (string-length name) (fx- upper-bound 1))
                   (errorf who "generated name length is outside the requested bounds"))
                 (string->symbol name)))]))

  #|proc:random-datum
  The `random-datum` procedure returns a random fixnum, flonum, character, string, or symbol.
  Numbers are nonnegative and below `most-positive-fixnum` or 999999.999, respectively.
  Other values use their default generators. This procedure takes no arguments.
  |#
  (define-who random-datum
    (lambda ()
      (case (random 5)
        [0 (random (most-positive-fixnum))]
        [1 (random 999999.999)]
        [2 (random-char)]
        [3 (random-string)]
        [4 (random-symbol)])))

  #|proc:random-list
  The `random-list` procedure returns a list with length in [`lower-bound`, `upper-bound`),
  using nonnegative fixnum bounds in increasing order. The defaults are 0 and 10;
  a single bound supplies the upper bound. The optional `generator` has signature
  `() -> object`, defaults to `random-datum`, and is called once per element in list order.
  |#
  (define-who random-list
    (case-lambda
      [() (random-list random-datum 0 10)]
      [(upper-bound) (random-list random-datum 0 upper-bound)]
      [(lower-bound upper-bound) (random-list random-datum lower-bound upper-bound)]
      [(generator lower-bound upper-bound)
       (pcheck ([procedure? generator] [$length-bound? lower-bound upper-bound])
               (when (fx>= lower-bound upper-bound)
                 ($range-error who lower-bound upper-bound))
               (let ([builder ($make-list-builder)] [length ($rand lower-bound upper-bound)])
                 (do ([index 0 (fx+ index 1)]) ((fx= index length) (builder))
                   (builder (generator)))))]))

  #|proc:random-box
  The `random-box` procedure returns a fresh box containing the result of `generator`.
  The optional `generator` has signature `() -> object` and defaults to `random-datum`.
  |#
  (define-who random-box
    (case-lambda
      [()  (box (random-datum))]
      [(generator) (pcheck ([procedure? generator]) (box (generator)))]))

  (define $random-bytevector
    (case-lambda
      [(who)    ($random-bytevector who 0 10)]
      [(who upper-bound) ($random-bytevector who 0 upper-bound)]
      [(who lower-bound upper-bound)
       (pcheck ([$length-bound? lower-bound upper-bound])
        (when (fx>= lower-bound upper-bound) ($range-error who lower-bound upper-bound))
        (let* ([len ($rand lower-bound upper-bound)] [u8vec (make-bytevector len 0)])
          (let loop ([i 0])
            (if (fx= i len)
                u8vec
                (begin (bytevector-u8-set! u8vec i (random 256))
                       (loop (add1 i)))))))]))

  #|proc:random-bytevector
  The `random-bytevector` procedure returns a bytevector containing random octets.
  The optional nonnegative fixnum bounds `lower-bound` and `upper-bound` specify its length
  in [`lower-bound`, `upper-bound`) and must be in increasing order. Defaults are 0 and 10;
  a single bound supplies the upper bound.
  |#
  (define-who random-bytevector
    (case-lambda
      [() ($random-bytevector who)]
      [(upper-bound) ($random-bytevector who upper-bound)]
      [(lower-bound upper-bound) ($random-bytevector who lower-bound upper-bound)]))

  #|proc:random-u8vec
  The `random-u8vec` procedure returns random octets using the `random-bytevector` contract.
  Its optional nonnegative fixnum bounds `lower-bound` and `upper-bound` select a length
  in [`lower-bound`, `upper-bound`); defaults are 0 and 10, with a single bound being upper.
  |#
  (define-who random-u8vec
    (case-lambda
      [() ($random-bytevector who)]
      [(upper-bound) ($random-bytevector who upper-bound)]
      [(lower-bound upper-bound) ($random-bytevector who lower-bound upper-bound)]))

  ;; TODO move random-vector here


;;;;===----------------------------------------------------------------------===
;;;; bit masks and flags
;;;;===----------------------------------------------------------------------===

  #|macro:define-flags
  The `define-flags` macro defines a mask macro `name` and a procedure `name->symbols` from
  `(flag mask)` pairs. Masks are fixnums or expressions referring to earlier flags, combined with
  `or`. The macro combines its named flag arguments into one fixnum mask.
  The generated decoder takes a fixnum and returns overlapping flag symbols in definition order.
  Any shared bit includes a flag, including composite masks and aliases; zero masks never appear.
  The definition's return value is unspecified.
  |#
  (define-syntax define-flags
    (lambda (exp)
      (define mask-environment
        (lambda (flags masks)
          (let f ([flags flags] [masks masks] [e '()])
            (if (null? flags)
                e
                (let ([mask (flag->mask (car masks) e)])
                  (f (cdr flags) (cdr masks)
                     (cons `(,(car flags) . ,mask) e)))))))
      (syntax-case exp ()
        [(_k name (flag mask) ...)
         (with-syntax ([env (datum->syntax #'_k
                                           (mask-environment
                                            (datum (flag ...))
                                            (datum (mask ...))))]
                       [->symbols ($construct-name #'_k #'name "->symbols")])
           #'(begin
               (define-syntax name
                 (lambda (x)
                   (syntax-case x ()
                     ((_k flags (... ...))
                      (datum->syntax #'_k
                                     (flag->mask `(or ,@(datum (flags (... ...)))) 'env))))))
               (define ->symbols
                 (lambda (flags)
                   (pcheck ([fixnum? flags])
                     (let loop ([e 'env] [res '()])
                       (if (null? e)
                           res
                           (if (not (fx= (fxlogand (cdar e) flags) 0))
                               (loop (cdr e) (cons (caar e) res))
                               (loop (cdr e) res)))))))))])))

  #|macro:flag-any-set?
  The `flag-any-set?` macro returns whether any bits in fixnum `mask` occur in fixnum `x`.
  |#
  (define-syntax flag-any-set?
    (syntax-rules ()
      ((_ mask x)
       (let ([bits mask] [value x])
         (pcheck ([fixnum? bits value])
           (not (fx= (fxlogand bits value) 0)))))))

  #|macro:flag-all-set?
  The `flag-all-set?` macro returns whether every bit in fixnum `mask` occurs in fixnum `x`.
  A zero mask always returns `#t`.
  |#
  (define-syntax flag-all-set?
    (syntax-rules ()
      ((_ mask x)
       (let ([bits mask] [value x])
         (pcheck ([fixnum? bits value])
           (fx= (fxlogand bits value) bits))))))

  #|macro:flag-set
  The `flag-set` macro returns fixnum `x` with every bit of fixnum `mask` set, without mutation.
  |#
  (define-syntax flag-set
    (syntax-rules ()
      ((_ mask x)
       (let ([bits mask] [value x])
         (pcheck ([fixnum? bits value]) (fxlogor bits value))))))

  #|macro:flag-reset
  The `flag-reset` macro returns fixnum `x` with every bit of fixnum `mask` cleared.
  Neither argument is mutated.
  |#
  (define-syntax flag-reset
    (syntax-rules ()
      ((_ mask x)
       (let ([bits mask] [value x])
         (pcheck ([fixnum? bits value]) (fxlogand (fxlognot bits) value))))))

  )
