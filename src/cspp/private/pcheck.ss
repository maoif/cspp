  #|macro:pcheck
  The `pcheck` macro checks bound argument identifiers using clauses `[predicate id ...]`.
  Each predicate has signature `object -> object`; a false result raises an argument error.
  Checks run in clause order before the body, whose values are returned on success.
  Use this macro at API argument boundaries; each predicate expression is evaluated once per clause.
  Errors identify `pcheck`; use `pcheck-who` to supply the caller's name explicitly.
  |#
  (define-syntax pcheck
    (syntax-rules ()
      [(_ checks body body* ...)
       (let ([who 'pcheck]) (pcheck-who who checks body body* ...))]))

  #|macro:pcheck-who
  The `pcheck-who` macro checks `[predicate argument ...]` clauses before evaluating its body.
  The `who` form must be a variable reference whose value is a symbol naming the caller.
  Its value is read once before checking arguments; a nonsymbol value raises an argument error.
  Inside `define-who`, pass its `who` binding. A rejection raises an error naming that caller.
  Each predicate has signature `object -> object` and is evaluated once per clause; checks run
  in order and stop at the first rejection. Successful checks return all body values.
  Argument forms must be bound identifiers. An empty clause list performs no argument checks.
  |#
  (define-syntax pcheck-who
    (lambda (stx)
      (define all-ids?
        (lambda (ls*)
          (andmap
           (lambda (ls)
             (andmap identifier? ls))
           ls*)))
      (syntax-case stx ()
        [(k who ([pred x x* ...] ...) e e* ...)
         (and (identifier? #'who) (all-ids? #'((x x* ...) ...)))
         #`(let ([caller who])
             (unless (symbol? caller) (errorf 'pcheck-who "caller is not a symbol: ~s" caller))
             #,(let loop ([preds #'(pred ...)] [id** #'((x x* ...) ...)])
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
                                   (errorf caller "failed predicate ~s for ~s, value: ~s"
                                     'pred '#,(car id*) #,(car id*))))))))))]
        [_ (syntax-error stx "invalid pcheck-who form:")])))
