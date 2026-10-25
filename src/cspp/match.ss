#!chezscheme

#|

Adapted from Andy Keep's implementation:
https://github.com/akeep/scheme-to-llvm/blob/main/src/cspp/main/scheme/match.sls

|#


(library (cspp match)
  (export match
          mlambda mlambda+ mlet mlet*)
  (import (chezscheme)
          (cspp list)
          (cspp vector)
          (cspp irregex)
          (cspp internal)
          (cspp private match)
          (cspp io))

  #|macro:match
  The `match` macro evaluates its input once and tries its pattern clauses in order.
  `,name` captures a value, `` `name `` compares against a bound value, and `_` ignores a value.
  List and vector patterns destructure their elements; `...` repeats a pattern zero or more times,
  collecting its captures in lists. Captures preceding a suffix are visible to its backreferences.
  Records use `,(record Type :fields (field pattern) ...)` for a named subset of fields,
  or `,(record Type :values pattern ...)` for all fields in declaration order. Both native Chez
  and CSPP record types are supported. Empty `:fields` checks only the type; empty `:values`
  requires a type with no fields. Fields belong to the selected type, excluding inherited fields.
  Named fields must be unique; unknown fields and wrong positional counts are errors. These are
  syntax errors for static descriptors and runtime errors for local generative descriptors when
  the input has the selected type. Malformed modes and selectors are always syntax errors.
  Record syntax requires the explicit wrapper; the old `,(Type ...)` form is unsupported.
  The `record`, `:fields`, and `:values` tags are recognized by spelling. Other extension heads
  retain custom-expander dispatch; a record named `hash` uses `,(record hash :values ...)`.
  Datatype, box, hash, and regex extensions retain their existing forms. Expressions embedded in
  extensions are evaluated as expressions and do not declare pattern captures.
  Pattern variables intentionally shadow outer bindings in the selected body. Generated matcher
  bindings remain hygienic, so names such as `rtd`, `fields`, and `indices` in a pattern cannot
  capture or rename the matcher's internal temporaries.
  The reader treats `,(hash ...)` as `(unquote (hash ...))`, which invokes the hash extension.
  To match such a list as data, capture its head and use a guard, for example:
  ```scheme
  (match '(unquote (hash ("key" 42)))
    [(,head (hash ("key" ,value))) (guard (eq? head 'unquote)) value]) ; => 42
  ```
  A clause may have `(guard predicate ...)` before its body. Failed clauses try the next clause;
  a final `else` provides a fallback, and an unmatched input without a fallback raises an error.
  The selected body supplies the result values.
  |#
  (define-syntax match
    (lambda (stx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define literal?
        (lambda (lit)
          (let ([p (syntax->datum lit)])
            (or (number? p)
                (boolean? p)
                (string? p)
                (char? p)))))
      (define handle-extension
        (lambda (ext rho expr-id pat body fk)
          (define (match-expander? ext)
            (let* ([n ($construct-name ext ext "-expander")]
                   [?p (rho n)])
              (and ?p (eq? '*MATCH-EXPANDER* (car ?p)))))
          (cond
           [(eq? 'record (syntax->datum ext))
            (record-expander process-pattern rho expr-id pat body fk)]
           ;; These heads keep their built-in meanings even when a record or custom
           ;; expander has the same spelling; explicit `record` selects that type.
           [(eq? 'box (syntax->datum ext))
            (box-expander process-pattern rho expr-id pat body fk)]
           [(eq? 'regex (syntax->datum ext))
            (regex-expander process-pattern rho expr-id pat body fk)]
           [(eq? 'hash (syntax->datum ext))
            (hash-expander process-pattern rho expr-id pat body fk)]
           [(match-expander? ext)
            (let ([expander (cdr (rho ($construct-name ext ext "-expander")))])
              (assert (procedure? expander))
              (expander process-pattern rho expr-id pat body fk))]
           [else (syntax-error ext "match: invalid extension pattern: ")])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   main logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define process-pattern
        (lambda (rho expr-id pat body fk)
          ;; Conservatively include embedded expressions as well as backreferences.
          (define mentions-variable?
            (lambda (pattern variable)
              (syntax-case pattern ()
                [(head . tail)
                 (or (mentions-variable? #'head variable)
                     (mentions-variable? #'tail variable))]
                [#(items ...) (exists (lambda (item) (mentions-variable? item variable))
                                     #'(items ...))]
                [name (identifier? #'name) (free-identifier=? #'name variable)]
                [_ #f])))
          (define transparent-suffix?
            (lambda (pattern)
              (syntax-case pattern (unquote unquote-splicing quasiquote)
                [,name (identifier? #'name) #t]
                ;; Identifier macros may introduce prefix references at their use site.
                [`name (identifier? #'name) #f]
                [,(extension arguments ...) #f]
                [(unquote-splicing . arguments) #f]
                [(head . tail) (and (transparent-suffix? #'head) (transparent-suffix? #'tail))]
                [#(items ...) (for-all transparent-suffix? #'(items ...))]
                [_ #t])))
          (define bind-pattern-captures
            (lambda (variables expressions body)
              (if (null? variables)
                  body
                  #`(let ([#,(car variables) #,(car expressions)])
                      #,(bind-pattern-captures (cdr variables) (cdr expressions) body)))))
          (define bind-suffix-captures
            (lambda (variables accumulators suffix success generate)
              (let loop ([variables variables] [accumulators accumulators]
                         [early '()] [late '()])
                (define (bind entries body)
                  (bind-pattern-captures (map car entries) (map cadr entries) body))
                (if (null? variables)
                    (bind (reverse early) (generate (bind (reverse late) success)))
                    (let ([entry (list (car variables) #`(reverse #,(car accumulators)))])
                      (if (or (not (transparent-suffix? suffix))
                              (mentions-variable? suffix (car variables)))
                          (loop (cdr variables) (cdr accumulators) (cons entry early) late)
                          (loop (cdr variables) (cdr accumulators) early (cons entry late))))))))
          (define with-pattern-vars
            (lambda (pattern generate)
              (define captures '())
              (define (add! variable)
                (unless (exists (lambda (capture) (bound-identifier=? variable capture)) captures)
                  (set! captures (cons variable captures))))
              (define (walk pattern)
                (syntax-case pattern (unquote unquote-splicing quasisyntax)
                  [,variable (identifier? #'variable) (add! #'variable)]
                  [,@variable (identifier? #'variable) (add! #'variable)]
                  [,@[,names ...]
                   (andmap identifier? #'(names ...))
                   (for-each add! #'(names ...))]
                  [,@[procedure -> ,names ...]
                   (and (andmap identifier? #'(names ...))
                        (identifier? #'procedure) (eq? (datum ->) '->))
                   (for-each add! #'(names ...))]
                  [(#`name subpattern)
                   (identifier? #'name)
                   (begin (add! #'name) (walk #'subpattern))]
                  [,(extension arguments ...)
                   (identifier? #'extension)
                   (for-each walk (extension-subpatterns rho #'(extension arguments ...)))]
                  [(head . tail) (begin (walk #'head) (walk #'tail))]
                  [#(items ...) (for-each walk #'(items ...))]
                  [_ (void)]))
              (walk pattern)
              (generate captures)))
          (define process-vector-pats
            (lambda (vecid vlen veci pats body fk)
              (syntax-case pats ()
                [()
                 #`(if (fx= #,veci #,vlen)
                       #,body
                       (#,fk))]
                [(pat dots)
                 (eq? (datum dots) '...)
                 (with-pattern-vars #'pat
                   (lambda (variables)
                     (with-syntax ([(patvars ...) variables])
                       (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                     [(i viter vitem) (generate-temporaries '(i viter vitem))])
                         #`(let viter ([i #,veci] [bindings '()] ...)
                             (if (fx= i #,vlen)
                                 #,(bind-pattern-captures variables
                                                          (map (lambda (binding) #`(reverse #,binding))
                                                               #'(bindings ...))
                                                          body)
                                 (let ([vitem (vector-ref #,vecid i)])
                                   #,(process-pattern rho #'vitem #'pat
                                                      #'(viter (fx1+ i) (cons patvars bindings) ...)
                                                      fk))))))))]
                [(pat0 dots . pat1)
                 (eq? (datum dots) '...)
                 (with-pattern-vars #'pat0
                   (lambda (variables)
                     (with-syntax ([(patvars ...) variables])
                       (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                     [(i viter vitem new-fk) (generate-temporaries '(i viter vitem new-fk))])
                         #`(let viter ([i #,veci] [bindings '()] ...)
                             (let ([new-fk
                                    (lambda ()
                                      (if (fx< i #,vlen)
                                          (let ([vitem (vector-ref #,vecid i)])
                                            #,(process-pattern rho #'vitem #'pat0
                                                               #'(viter (fx1+ i) (cons patvars bindings) ...)
                                                               fk))
                                          (#,fk)))])
                               #,(bind-suffix-captures
                                  variables #'(bindings ...) #'pat1 body
                                  (lambda (success)
                                    (process-vector-pats vecid vlen #'i #'pat1 success #'new-fk)))))))))]
                [(pat0 pat1 ...)
                 (with-syntax ([(vitem) (generate-temporaries '(vitem))])
                   #`(if (fx< #,veci #,vlen)
                         (let ([vitem (vector-ref #,vecid #,veci)])
                           #,(process-pattern rho #'vitem #'pat0
                                              (process-vector-pats vecid vlen #`(fx1+ #,veci)
                                                                   #'(pat1 ...) body fk)
                                              fk))
                         (#,fk)))])))
          ;;(printf "process-pattern ~s ~s ~s ~s~n" expr-id pat body fk)
          (with-syntax ([expr-id expr-id])
            (syntax-case pat (unquote quasiquote unquote-splicing quasisyntax)
              [,var
               (identifier? #'var)
               #`(let ([var expr-id]) #,body)]
              [`var
               (identifier? #'var)
               #`(if (equal? var expr-id) #,body (#,fk))]
              [()
               #`(if (null? expr-id) #,body (#,fk))]
              [,@var
               (identifier? #'var)
               #`(let ([var (match-loop expr-id)])
                   #,body)]
              [,@[]
               #`(begin (match-loop expr-id)
                        #,body)]
              [,@[,vars ...]
               (andmap identifier? #'(vars ...))
               (if (fx= 1 (length #'(vars ...)))
                   #`(let ([#,(car #'(vars ...)) (match-loop expr-id)])
                       #,body)
                   #`(let-values ([(vars ...) (match-loop expr-id)])
                       #,body))]
              [,@[proc ->]
               (and (identifier? #'proc) (eq? '-> (datum ->)))
               #`(begin (proc expr-id)
                        #,body)]
              [,@[proc -> ,vars ...]
               (and (andmap identifier? #'(vars ...)) (identifier? #'proc) (eq? '-> (datum ->)))
               (if (fx= 1 (length #'(vars ...)))
                   #`(let ([#,(car #'(vars ...)) (proc expr-id)])
                       #,body)
                   #`(let-values ([(vars ...) (proc expr-id)])
                       #,body))]
              [(#`nref pat)
               (identifier? #'nref)
               #`(let ([nref expr-id])
                   #,(process-pattern rho #'expr-id #'pat body fk))]
              [,(?match-extension . special-pats)
               (identifier? #'?match-extension)
               (handle-extension #'?match-extension rho #'expr-id #'(?match-extension . special-pats) body fk)]
              ;; ... in the end
              [(pat dots)
               (eq? (datum dots) '...)
               #`(if (or (null? expr-id) (pair? expr-id))
                     ;; Collect captures from pattern positions, excluding extension expressions.
                     #,(with-pattern-vars #'pat
                         (lambda (variables)
                           (with-syntax ([(patvars ...) variables]
                                         [(lbs ...) (generate-temporaries variables)]
                                         [(iter first rest) (generate-temporaries '(iter first rest))])
                             #`(let ([lbs (make-list-builder)] ...)
                                 (let iter ([ls expr-id])
                                   (cond
                                    [(null? ls)
                                     #,(bind-pattern-captures variables
                                                              (map (lambda (builder) #`(#,builder)) #'(lbs ...))
                                                              body)]
                                    [(pair? ls)
                                     (let ([first (car ls)] [rest (cdr ls)])
                                       #,(process-pattern rho #'first #'pat
                                                          #'(begin (lbs patvars) ...
                                                                   (iter rest))
                                                          fk))]
                                    [else (#,fk)]))))))
                     (#,fk))]
              ;; ... in the middle, prefer `pat1`
              [(pat0 dots . pat1)
               (eq? (datum dots) '...)
               (with-pattern-vars #'pat0
                 (lambda (variables)
                   (with-syntax ([(patvars ...) variables]
                                 [(bindings ...) (generate-temporaries variables)]
                                 [(iter ls first rest new-fk)
                                  (generate-temporaries '(iter ls first rest new-fk))])
                     #`(let iter ([ls expr-id] [bindings '()] ...)
                         (let ([new-fk
                                (lambda ()
                                  ;; Give up one item to match against the repeated pattern.
                                  (if (pair? ls)
                                      (let ([first (car ls)] [rest (cdr ls)])
                                        #,(process-pattern rho #'first #'pat0
                                                           #'(iter rest (cons patvars bindings) ...)
                                                           fk))
                                      (#,fk)))])
                           ;; Prefer the suffix, extending the repeated prefix on failure.
                           #,(bind-suffix-captures
                              variables #'(bindings ...) #'pat1 body
                              (lambda (success)
                                (process-pattern rho #'ls #'pat1 success #'new-fk))))))))]
              ;; encapsulates (pat ...), (pat . pat), (pat ... . pat)
              [(pat0 . pat1)
               (with-syntax ([(first rest) (generate-temporaries '(first rest))])
                 #`(if (pair? expr-id)
                       (let ([first (car expr-id)] [rest (cdr expr-id)])
                         #,(process-pattern rho #'first #'pat0
                                            (process-pattern rho #'rest #'pat1 body fk)
                                            fk))
                       (#,fk)))]
              [#()
               #`(if (and (vector? expr-id) (fx= 0 (vector-length expr-id)))
                     #,body
                     (#,fk))]
              ;; `pats` has at least one pattern
              [#(pats ...)
               #`(if (vector? expr-id)
                     #,(with-syntax ([(vlen) (generate-temporaries '(vlen))])
                         #`(let ([vlen (vector-length expr-id)])
                             #,(process-vector-pats #'expr-id #'vlen #'0 #'(pats ...) body fk)))
                     (#,fk))]
              [lit
               (literal? #'lit)
               #`(if (equal? lit expr-id) #,body (#,fk))]
              [under
               (equal? (datum under) '_)
               body]
              [sym
               (identifier? #'sym)
               #`(if (eq? 'sym expr-id) #,body (#,fk))]))))
      (define process-clause
        (lambda (rho expr-id cl fk)
          ;;(printf "process-clause ~s ~s ~s~n" expr-id cl fk)
          (syntax-case cl (guard)
            [(pat (guard guard-e* ...) e0 e* ...)
             (process-pattern rho expr-id #'pat
                              #`(if (and guard-e* ...)
                                    (begin e0 e* ...)
                                    ;; call fail continuation
                                    (#,fk))
                              fk)]
            [(pat e0 e* ...)
             (process-clause rho expr-id #'[pat (guard #t) e0 e* ...] fk)])))
      (define generate-skeleton
        (lambda (rho expr-id cl* fk)
          ;;(printf "generate-skeleton: ~s ~s ~s~n" expr-id cl* fk)
          (let loop ([cl* cl*])
            (if (null? cl*)
                fk
                (with-syntax ([(fk) (generate-temporaries '(fk))])
                  #`(let ([fk (lambda () #,(loop (cdr cl*)))])
                      #,(process-clause rho expr-id (car cl*) #'fk)))))))
      (syntax-case stx (else)
        [(k e cl* ... [else e0 e* ...])
         ;; `match-loop` used for catamorphism
         ;; `rho`: compile-time environment
         (lambda (rho)
           #`(let match-loop ([v e])
               #,(generate-skeleton rho #'v #'(cl* ...) #'(begin e0 e* ...))))]
        [(k e cl0 cl* ...)
         #'(let ([v e])
             (match v cl0 cl* ...
                    [else (errorf 'match "no match found")]))]
        [_ (syntax-error 'match "bad match form")])))

  (define-syntax mlambda
    (lambda (stx)
      (syntax-case stx (else)
        [(k cl* ... [else e0 e* ...])
         #'(lambda (arg)
             (match arg cl* ... [else e0 e* ...]))]
        [(k cl cl* ...)
         #'(lambda (arg)
             (match arg cl cl* ...
                    [else (errorf 'k "no match found")]))]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax $mlambda+
    (lambda (stx)
      (syntax-case stx (else)
        [(k who cl* ... [else e0 e* ...])
         #'(lambda arg
             (match arg cl* ... [else e0 e* ...]))]
        [(k who cl cl* ...)
         #'(lambda arg
             (match arg cl cl* ...
                    [else (errorf 'who "no match found")]))]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax mlambda+
    (syntax-rules ()
      [(k cl cl* ...) ($mlambda+ mlambda+ cl cl* ...)]))

  (define-syntax mlet
    (lambda (stx)
      (syntax-case stx ()
        [(k () e0 e* ...) #'(let () e0 e* ...)]
        [(k ([pat* rhs*] ...) e0 e* ...)
         #'(($mlambda+ mlet
                       [(pat* ...) e0 e* ...])
            rhs* ...)]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax mlet*
    (lambda (stx)
      (syntax-case stx (else)
        [(k () e0 e* ...) #'(let () e0 e* ...)]
        [(k ([pat* rhs*] ...) e0 e* ...)
         (let loop ([pat* #'(pat* ...)] [rhs* #'(rhs* ...)])
           (if (null? pat*)
               #'(begin e0 e* ...)
               #`(match #,(car rhs*)
                   [#,(car pat*) #,(loop (cdr pat*) (cdr rhs*))]
                   ;; TODO when k is not quoted, the error is "bad form" below,
                   ;; which is not quite precise
                   [else (errorf 'k "no match found")])))]
        [_ (syntax-error stx "bad form:")])))


  )
