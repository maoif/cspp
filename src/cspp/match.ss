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

  ;; match interface
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
          (define (record? ext)
            (or (valid-record-type? rho ext)
                (valid-record-type? rho ($construct-name ext "$record-" ext))))
          (cond
           [(match-expander? ext)
            (let ([expander (cdr (rho ($construct-name ext ext "-expander")))])
              (assert (procedure? expander))
              (expander process-pattern rho expr-id pat body fk))]
           ;; TODO make this dispatch extensible
           [(valid-record-type? rho ext)
            (system-record-expander process-pattern rho expr-id pat body fk)]
           [(record? ext)
            (record-expander process-pattern rho expr-id pat body fk)]
           [(eq? 'box (syntax->datum ext))
            (box-expander process-pattern rho expr-id pat body fk)]
           [(eq? 'regex (syntax->datum ext))
            (regex-expander process-pattern rho expr-id pat body fk)]
           [(eq? 'hash (syntax->datum ext))
            (hash-expander process-pattern rho expr-id pat body fk)]
           [else (syntax-error ext "match: invalid extension pattern: ")])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   main logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define process-pattern
        (lambda (rho expr-id pat body fk)
          (define extract-pat-vars
            (lambda (pat)
              (let loop ([pat pat] [patvars '()])
                (syntax-case pat (unquote unquote-splicing quasisyntax)
                  [,var  (identifier? #'var) (cons #'var patvars)]
                  [,@var (identifier? #'var) (cons #'var patvars)]
                  [,@[,vars ...]
                   (andmap identifier? #'(vars ...))
                   (append #'(vars ...) patvars)]
                  [,@[proc -> ,vars ...]
                   (and (andmap identifier? #'(vars ...)) (identifier? #'proc) (eq? '-> (datum ->)))
                   (append #'(vars ...) patvars)]
                  [(#`nref pat) (identifier? #'nref) (cons #'nref (loop #'pat patvars))]
                  [(pat0 . pat1) (loop #'pat0 (loop #'pat1 patvars))]
                  [#(pat0 pat1 ...) (loop #'pat0 (loop #'(pat1 ...) patvars))]
                  [_ patvars]))))
          (define process-vector-pats
            (lambda (vecid vlen veci pats body fk)
              (syntax-case pats ()
                [()
                 #`(if (fx= #,veci #,vlen)
                       #,body
                       (#,fk))]
                [(pat dots)
                 (eq? (datum dots) '...)
                 (with-syntax ([(patvars ...) (extract-pat-vars #'pat)])
                   (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                 [(i viter vitem) (generate-temporaries '(i viter vitem))])
                     #`(let viter ([i #,veci] [bindings '()] ...)
                         (if (fx= i #,vlen)
                             (let ([patvars (reverse bindings)] ...)
                               #,body)
                             (let ([vitem (vector-ref #,vecid i)])
                               #,(process-pattern rho #'vitem #'pat
                                                  #'(viter (fx1+ i) (cons patvars bindings) ...)
                                                  fk))))))]
                [(pat0 dots . pat1)
                 (eq? (datum dots) '...)
                 (with-syntax ([(patvars ...) (extract-pat-vars #'pat0)])
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
                           #,(process-vector-pats vecid vlen #'i #'pat1
                                                  #`(let ([patvars (reverse bindings)] ...)
                                                      #,body)
                                                  #'new-fk)))))]
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
              [,(?match-extension special-pats ...)
               (identifier? #'?match-extension)
               (handle-extension #'?match-extension rho #'expr-id #'(?match-extension special-pats ...) body fk)]
              ;; ... in the end
              [(pat dots)
               (eq? (datum dots) '...)
               #`(if (or (null? expr-id) (pair? expr-id))
                     ;; Get all pat vars in `pat`,
                     ;; iterate through the input list, for each item,
                     ;; destructure it and accumulate matched components in `bindings ...`.
                     #,(with-syntax ([(patvars ...) (extract-pat-vars #'pat)])
                         (with-syntax ([(lbs ...)      (generate-temporaries #'(patvars ...))]
                                       [(iter first rest) (generate-temporaries '(iter first rest))])
                           #`(let ([lbs (make-list-builder)] ...)
                               (let iter ([ls expr-id])
                                 (cond
                                  [(null? ls)
                                   (let ([patvars (lbs)] ...)
                                     #,body)]
                                  [(pair? ls)
                                   (let ([first (car ls)] [rest (cdr ls)])
                                     #,(process-pattern rho #'first #'pat
                                                        #'(begin (lbs patvars) ...
                                                                 (iter rest))
                                                        fk))]
                                  [else (#,fk)])))))
                     (#,fk))]
              ;; ... in the middle, prefer `pat1`
              [(pat0 dots . pat1)
               (eq? (datum dots) '...)
               (with-syntax ([(patvars ...) (extract-pat-vars #'pat0)])
                 (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                               [(iter ls first rest new-fk) (generate-temporaries '(iter ls first rest new-fk))])
                   #`(let iter ([ls expr-id] [bindings '()] ...)
                       (let ([new-fk (lambda ()
                                       ;; give up one item in the list to be matched against `pat0`
                                       (if (pair? ls)
                                           (let ([first (car ls)] [rest (cdr ls)])
                                             #,(process-pattern rho #'first #'pat0
                                                                #'(iter rest (cons patvars bindings) ...)
                                                                fk))
                                           (#,fk)))])
                         ;; Match the input directly with `pat1` first, if it matches,
                         ;; `pat0` or all pats vars inside `pat0` is '().
                         ;; If the input fails to match `pat1`, call `new-fk`,
                         #,(process-pattern rho #'ls #'pat1
                                            #`(let ([patvars (reverse bindings)] ...)
                                                #,body)
                                            #'new-fk)))))]
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
