#!chezscheme
(library (cspp private match)
  (export system-record-expander record-expander datatype-expander
          box-expander regex-expander hash-expander
          extension-pattern-branches
          valid-record-type?
          define-match-expander)
  (import (cspp chez)
          (cspp utils)
          (cspp internal)
          (cspp io)
          (cspp list)
          (cspp vector)
          (cspp irregex))


  ;; see ChezScheme syntax.ss: define-record-type
  (define get-type-descriptor (lambda (rho dt) (cadr (rho dt))))

  ;; get all field names of the record type, include those from the parents
  (define get-all-fields
    (lambda (rho dt)
      (let ([rtd (get-type-descriptor rho dt)])
        (let loop ([pdesc (record-type-parent rtd)]
                   [fld** (list (record-type-field-names rtd))])
          (if pdesc
              (loop (record-type-parent pdesc) (cons (record-type-field-names pdesc) fld**))
              ;; returns a list of vectors: '(#(f0 f1 ...) ...)
              (reverse fld**))))))

  (define get-fields
    (lambda (rho dt)
      (let ([rtd (get-type-descriptor rho dt)])
        ;; returns vector of names: '#(f0 f1 ...)
        (record-type-field-names rtd))))

  ;; return `t` if it's a record type, otherwise #f
  (define valid-record-type?
    (lambda (rho t)
      (and (identifier? t)
           (let ([r '#{r6rs-record vc7pishgmrh09qm-a}]
                 [info (rho t)])
             (and info (eq? r (car (rho t))) t)))))

  (define variant-of?
    (lambda (rho dt variant)
      (and (valid-record-type? rho dt)
           (valid-record-type? rho variant)
           (let ([pdesc (record-type-parent (get-type-descriptor rho variant))])
             (if pdesc
                 (eq? pdesc (get-type-descriptor rho dt))
                 (errorf 'match "invalid variant ~a of datatype ~a" variant dt))))))

  (define by-pos-record-pats?
    (lambda (rho rcd pats)
      (fx= (length pats) (vector-length (get-fields rho rcd)))))

  (define by-pos-datatype-pats?
    (lambda (rho variant pats)
      (by-pos-record-pats? rho variant pats)))

  (define by-name-record-pats?
    (lambda (rho dt pats)
      (let* ([pats (syntax->datum pats)]
             [fld* (get-fields rho dt)])
        (and (apply = 2 (map (lambda (p) (or (and
                                              ;; in pats like (,x _ ,y), _ is not a list
                                              (list? p) (length p))
                                             -1))
                             pats))
             (<= (length pats) (vector-length fld*))
             ;; TODO check names are unique
             ;; check if each pat has right field names
             (andmap (lambda (pat) (vmemq (car pat) fld*)) pats)))))

  (define by-name-datatype-pats?
    (lambda (rho variant pats)
      (by-name-record-pats? rho variant pats)))


  (define pattern-value
    (lambda (pattern)
      (syntax-case pattern () [(_ value) #'value])))

  ;; Ordinary named-field syntax is also a two-element positional list pattern.
  ;; Both interpretations have the same captures, so their submatchers can be shared.
  (define ordinary-named-patterns?
    (lambda (patterns)
      (and (pair? patterns)
           (for-all (lambda (pattern)
                      (syntax-case pattern ()
                        [(name value)
                         (and (identifier? #'name)
                              (not (memq (syntax->datum #'name)
                                         '(unquote quasiquote unquote-splicing))))]
                        [_ #f]))
                    patterns))))

  ;; A branch pairs a runtime descriptor test with the actual subpatterns.
  ;; Generative native records defer named/positional disambiguation until runtime.
  (define record-pattern-branches
    (lambda (compile-environment record-name patterns)
      (let ([descriptor (get-type-descriptor compile-environment record-name)])
        (if (record-type-descriptor? descriptor)
            (list (list #'#t
                        (if (and (pair? patterns)
                                 (by-name-record-pats? compile-environment record-name patterns))
                            (map pattern-value patterns)
                            patterns)))
            (if (ordinary-named-patterns? patterns)
                (list (list #'#t patterns))
                (if (and (pair? patterns)
                     (andmap (lambda (pattern)
                               (syntax-case pattern ()
                                 [(name value) (identifier? #'name) #t]
                                 [_ #f]))
                             patterns))
                (list
                 (list #`(let ([fields (record-type-field-names
                                       (record-type-descriptor #,record-name))])
                           (andmap (lambda (name) (number? (vmemq name fields)))
                                   '#,(datum->syntax record-name (map (lambda (p)
                                                                      (syntax-case p () [(name value) (datum name)]))
                                                                    patterns))))
                       (map pattern-value patterns))
                 (list #'#t patterns))
                (list (list #'#t patterns))))))))

  #|proc:extension-pattern-branches
  The `extension-pattern-branches` procedure identifies pattern positions in an extension.
  `compile-environment` maps syntax identifiers to their compile-time bindings, and `pattern`
  is the extension syntax without its leading unquote. The result is a list of branches, each
  containing a descriptor-test expression and a list of subpatterns. The final test is `#t`.
  Embedded Scheme expressions and record field names are excluded from the subpatterns.
  |#
  (define extension-pattern-branches
    (lambda (compile-environment pattern)
      (pcheck ([procedure? compile-environment] [list? pattern])
        (let* ([extension (car pattern)]
               [arguments (cdr pattern)]
               [info (compile-environment ($construct-name extension extension "-expander"))]
               [expander (and (pair? info) (eq? (car info) '*MATCH-EXPANDER*) (cdr info))])
          (cond
           [(eq? expander record-expander)
            (record-pattern-branches compile-environment
                                     ($construct-name extension "$record-" extension)
                                     arguments)]
           [(eq? expander datatype-expander)
            (if (and (= 1 (length arguments)) (identifier? (car arguments)))
                (list (list #'#t '()))
                (record-pattern-branches compile-environment
                                         ($construct-name extension "$datatype-" extension "-"
                                                          (car arguments))
                                         (cdr arguments)))]
           [(valid-record-type? compile-environment extension)
            (record-pattern-branches compile-environment extension arguments)]
           [(and (not expander) (eq? (syntax->datum extension) 'regex))
            (list (list #'#t (map pattern-value (cdr arguments))))]
           [(and (not expander) (eq? (syntax->datum extension) 'hash))
            (list (list #'#t (map pattern-value arguments)))]
           ;; Box patterns and other expanders use their arguments as subpatterns.
           [else (list (list #'#t arguments))])))))


  (define-syntax define-match-expander
    (lambda (stx)
      (syntax-case stx ()
        [(_ name expander)
         (identifier? #'name)
         #'(define-syntax name
             (make-compile-time-value
              (let ([p expander])
                (if (procedure? p)
                    (cons '*MATCH-EXPANDER* p)
                    (syntax-error p "define-match-expander: not a procedure:")))))])))


  ;; Local generative records carry a descriptor identifier in the expansion
  ;; environment; their field names and field count are only known at runtime.
  (define small-record-body?
    (lambda (body)
      (let loop ([pending (list body)] [budget 64])
        (cond [(null? pending) #t]
              [(zero? budget) #f]
              [else
               (syntax-case (car pending) ()
                 [(head . tail) (loop (cons #'head (cons #'tail (cdr pending))) (- budget 1))]
                 [#(items ...) (loop (append #'(items ...) (cdr pending)) (- budget 1))]
                 [_ (loop (cdr pending) (- budget 1))])]))))

  (define system-record-expander
    (case-lambda
      [(pp rho expr-id pat body fk)
       (system-record-expander pp rho expr-id pat body fk
                               (lambda (pattern body generate) (generate body)))]
      [(pp rho expr-id pat body fk share-body)
       (system-record-expander pp rho expr-id pat body fk share-body
                               (lambda (pattern patterns body) body))]
      [(pp rho expr-id pat body fk share-body protect-body)
       (syntax-case pat ()
         [(dt pats ...)
          (if (record-type-descriptor? (get-type-descriptor rho #'dt))
              (static-system-record-expander pp rho expr-id pat body fk)
              (with-syntax ([(rtd fields indices named-mode)
                             (generate-temporaries '(rtd fields indices named-mode))])
                (let* ([pats #'(pats ...)]
                       [named? (and (pair? pats) (andmap
                                (lambda (p)
                                  (syntax-case p ()
                                    [(name value) (identifier? #'name) #t]
                                    [_ #f]))
                                pats))]
                       [ordinary? (ordinary-named-patterns? pats)]
                       [protect-small? (and named? (not ordinary?) (small-record-body? body))]
                       [generate-shared-fields
                        (lambda (success)
                          (let loop ([patterns pats] [i 0])
                            (if (null? patterns)
                                success
                                (syntax-case (car patterns) ()
                                  [(name value)
                                   (with-syntax ([(field item) (generate-temporaries '(field item))])
                                     #`(let ([field ((record-accessor rtd
                                                      (if named-mode (list-ref indices #,i) #,i))
                                                    #,expr-id)])
                                         (if (or named-mode
                                                 (and (pair? field)
                                                      #,(if (eq? (datum name) '_) #'#t
                                                            #`(eq? (car field) 'name))
                                                      (pair? (cdr field)) (null? (cddr field))))
                                             (let ([item (if named-mode field (cadr field))])
                                               #,(pp rho #'item #'value
                                                     (loop (cdr patterns) (fx1+ i)) fk))
                                             (#,fk))))]))))]
                       [generate-fields
                        (lambda (patterns named? success)
                          (let ([success (if protect-small?
                                             (protect-body pat patterns success)
                                             success)])
                            (let loop ([patterns patterns] [i 0])
                              (if (null? patterns)
                                  success
                                  (with-syntax ([(field) (generate-temporaries '(field))])
                                    #`(let ([field ((record-accessor rtd
                                                     #,(if named?
                                                           #`(list-ref indices #,i)
                                                           i))
                                                   #,expr-id)])
                                        #,(pp rho #'field (car patterns)
                                              (loop (cdr patterns) (fx1+ i)) fk)))))))])
                 (let ([generate
                        (lambda (success)
                           #`(let ([rtd (record-type-descriptor dt)])
                               (if ((record-predicate rtd) #,expr-id)
                                   (let* ([fields (record-type-field-names rtd)]
                                          [indices #,(if named?
                                                        #`(map (lambda (name) (vmemq name fields))
                                                               '#,(datum->syntax #'dt
                                                                  (map (lambda (p) (syntax-case p ()
                                                                                    [(name value) (datum name)]))
                                                                       pats)))
                                                        #'#f)])
                                     #,(if ordinary?
                                           #`(let ([named-mode (andmap number? indices)])
                                               (unless (or named-mode
                                                           (fx= (vector-length fields) #,(length pats)))
                                                 (errorf 'match "invalid record pattern: ~s" '#,pat))
                                               #,(generate-shared-fields success))
                                     #`(cond
                                      #,@(if named?
                                             (list #`[(andmap number? indices)
                                                      #,(generate-fields
                                                         (map (lambda (p) (syntax-case p ()
                                                                            [(name value) #'value])) pats)
                                                         #t success)])
                                             '())
                                      [(fx= (vector-length fields) #,(length pats))
                                       #,(if named?
                                             ;; An ambiguous named pattern may have no valid positional
                                             ;; interpretation, e.g. a field named `unquote`.
                                             (guard (condition
                                                     [(syntax-violation? condition)
                                                      #`(errorf 'match "invalid record pattern: ~s" '#,pat)])
                                               (generate-fields pats #f success))
                                             (generate-fields pats #f success))]
                                      [else (errorf 'match "invalid record pattern: ~s" '#,pat)])))
                                   (#,fk))))])
                   ;; Share substantial continuations; duplicating a small leaf avoids
                   ;; access closures and boxing when a read-only capture stays local.
                   (if (and named? (not ordinary?) (not protect-small?))
                       (share-body pat body generate)
                       (generate body))))))])]))

  (define static-system-record-expander
    (lambda (pp rho expr-id pat body fk)
      (syntax-case pat ()
        [(dt pat pats ...)
         (by-name-record-pats? rho #'dt #'(pat pats ...))
         ;; use procedural interfaces:
         ;; (record-predicate rtd) (record-accessor rtd i)
         ;; their use can be optimized if `rtd` is given at compile time
         (with-syntax ([(type?) (generate-temporaries '(type?))]
                       [rtd (datum->syntax #'dt (get-type-descriptor rho #'dt))]
                       [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                       [(i ...) (datum->syntax #'dt (map
                                                     (lambda (n) (vmemq (car n) (get-fields rho #'dt)))
                                                     (syntax->datum #'(pat pats ...))))]
                       ;; get the patterns per se: (field <pat>) -> <pat>
                       [((_ npats) ...) #'(pat pats ...)])
           ;; need to quote `rtd`
           #`(let ([type? (record-predicate 'rtd)])
               (if (type? #,expr-id)
                   (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                     #,(let loop ([fields #'(fld* ...)] [npats #'(npats ...)])
                         (if (null? fields)
                             body
                             (pp rho (car fields) (car npats)
                                 (loop (cdr fields) (cdr npats))
                                 fk))))
                   (#,fk))))]
        [(dt pats ...)
         (by-pos-record-pats? rho #'dt #'(pats ...))
         (with-syntax ([(type?) (generate-temporaries '(type?))]
                       [rtd (datum->syntax #'dt (get-type-descriptor rho #'dt))]
                       [(fld* ...) (generate-temporaries (enumerate #'(pats ...)))]
                       [(i ...) (datum->syntax #'dt (enumerate #'(pats ...)))])
           #`(let ([type? (record-predicate 'rtd)])
               (if (type? #,expr-id)
                   (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                     #,(let loop ([fields #'(fld* ...)] [npats #'(pats ...)])
                         (if (null? fields)
                             body
                             (pp rho (car fields) (car npats)
                                 (loop (cdr fields) (cdr npats))
                                 fk))))
                   (#,fk))))]
        [_ (syntax-error pat "match: invalid record pattern:")])))


  (define record-expander
    (lambda (pp rho expr-id pat body fk)
      (syntax-case pat ()
        [(dt . rest)
         (let ([dt ($construct-name #'dt "$record-" #'dt)])
           (syntax-case #'rest ()
             [(pat pats ...)
              (by-name-record-pats? rho dt #'(pat pats ...))
              ;; use procedural interfaces:
              ;; (record-predicate rtd) (record-accessor rtd i)
              ;; their use can be optimized if `rtd` is given at compile time
              (with-syntax ([(type?) (generate-temporaries '(type?))]
                            [rtd (datum->syntax dt (get-type-descriptor rho dt))]
                            [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                            [(i ...) (datum->syntax dt (map
                                                        (lambda (n) (vmemq (car n) (get-fields rho dt)))
                                                        (syntax->datum #'(pat pats ...))))]
                            ;; get the patterns per se: (field <pat>) -> <pat>
                            [((_ npats) ...) #'(pat pats ...)])
                ;; need to quote `rtd`
                #`(let ([type? (record-predicate 'rtd)])
                    (if (type? #,expr-id)
                        (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                          #,(let loop ([fields #'(fld* ...)] [npats #'(npats ...)])
                              (if (null? fields)
                                  body
                                  (pp rho (car fields) (car npats)
                                      (loop (cdr fields) (cdr npats))
                                      fk))))
                        (#,fk))))]
             [(pat pats ...)
              (by-pos-record-pats? rho dt #'(pat pats ...))
              (with-syntax ([(type?) (generate-temporaries '(type?))]
                            [rtd (datum->syntax dt (get-type-descriptor rho dt))]
                            [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                            [(i ...) (datum->syntax dt (enumerate #'(pat pats ...)))])
                #`(let ([type? (record-predicate 'rtd)])
                    (if (type? #,expr-id)
                        (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                          #,(let loop ([fields #'(fld* ...)] [npats #'(pat pats ...)])
                              (if (null? fields)
                                  body
                                  (pp rho (car fields) (car npats)
                                      (loop (cdr fields) (cdr npats))
                                      fk))))
                        (#,fk))))]
             [_ (syntax-error pat "match: invalid record pattern:")]))]
        [_ (syntax-error pat "match: invalid record pattern:")])))


  (define datatype-expander
    (lambda (pp rho expr-id pat body fk)
      (syntax-case pat ()
        [(dt singleton)
         (identifier? #'singleton)
         #`(if (and (record? singleton (record-type-descriptor dt))
                    (eq? #,expr-id singleton))
               #,body
               (#,fk))]
        [(dt variant . rest)
         ;; name of dt is dt,
         ;; name of variant is $datatype-dt-varaint
         (let ([variant ($construct-name #'dt "$datatype-" #'dt "-" #'variant)])
           (if (variant-of? rho #'dt variant)
               (syntax-case #'rest ()
                 [(pat pats ...)
                  (by-name-datatype-pats? rho variant #'(pat pats ...))
                  (with-syntax ([(type?) (generate-temporaries '(type?))]
                                [rtd (datum->syntax variant (get-type-descriptor rho variant))]
                                [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                [(i ...) (datum->syntax variant (map
                                                                 (lambda (n) (vmemq (car n) (get-fields rho variant)))
                                                                 (syntax->datum #'(pat pats ...))))]
                                [((_ npats) ...) #'(pat pats ...)])
                    #`(let ([type? (record-predicate 'rtd)])
                        (if (type? #,expr-id)
                            (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                              #,(let loop ([fields #'(fld* ...)] [npats #'(npats ...)])
                                  (if (null? fields)
                                      body
                                      (pp rho (car fields) (car npats)
                                          (loop (cdr fields) (cdr npats))
                                          fk))))
                            (#,fk))))]
                 [(pat pats ...)
                  (by-pos-datatype-pats? rho variant #'(pat pats ...))
                  (with-syntax ([(type?) (generate-temporaries '(type?))]
                                [rtd (datum->syntax variant (get-type-descriptor rho variant))]
                                [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                [(i ...) (datum->syntax variant (enumerate #'(pat pats ...)))])
                    #`(let ([type? (record-predicate 'rtd)])
                        (if (type? #,expr-id)
                            (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                              #,(let loop ([fields #'(fld* ...)] [npats #'(pat pats ...)])
                                  (if (null? fields)
                                      body
                                      (let ([p (car npats)])
                                        (pp rho (car fields) p
                                            (loop (cdr fields) (cdr npats))
                                            fk)
                                        #;
                                        (if (identifier? p)
                                            ;; `p` has to be singleton: ,(dt p)
                                            (pp rho (car fields) #`,(dt #,p)
                                                (loop (cdr fields) (cdr npats))
                                                fk)
                                            (pp rho (car fields) p
                                                (loop (cdr fields) (cdr npats))
                                                fk))))))
                            (#,fk))))]
                 [_ (syntax-error pat "match: invalid datatype pattern:")])
               (syntax-error pat "match: invalid datatype/variant in:")))]
        [else (syntax-error pat "match: invalid pattern for datatype: ")])))


  (define box-expander
    (lambda (pp rho expr-id pat body fk)
      (syntax-case pat (unquote)
        [(_ pat)
         #`(if (box? #,expr-id)
               #,(with-syntax ([(bval) (generate-temporaries '(bval))])
                   #`(let ([bval (unbox #,expr-id)])
                       #,(pp rho #'bval #'pat body fk)))
               (#,fk))]
        [_ (syntax-error pat "match: invalid box pattern format:")])))


  (define regex-expander
    (lambda (pp rho expr-id pat body fk)
      (define handle-rest
        (lambda (expr-id regex rest body fk)
          (syntax-case rest ()
            [()
             (with-syntax ([(m) (generate-temporaries '(m))])
               #`(let ([m (and (string? #,expr-id) (irregex-search #,regex #,expr-id))])
                   (if m
                       #,body
                       (#,fk))))]
            ;; TODO check for duplicate numbers and names?
            ;; by number or by name
            [((n* v*) ...)
             (and (andmap (lambda (n) (or (and (integer? n) (>= n 0))
                                          (symbol? n)))
                          (syntax->datum #'(n* ...)))
                  (andmap (lambda (v)
                            (syntax-case v ()
                              [,v (identifier? #'v) #t]
                              [_ (syntax-error pat "only one pattern variable is allowed in regex pattern: ")]))
                          #'(v* ...)))
             (with-syntax ([(m) (generate-temporaries '(m))])
               #`(let ([m (and (string? #,expr-id) (irregex-search #,regex #,expr-id))])
                   (if m
                       #,(let loop ([n* #'(n* ...)]
                                    [v* (map (lambda (v) (syntax-case v () [,var #'var])) #'(v* ...))])
                           (if (null? n*)
                               body
                               #`(if (irregex-match-valid-index? m '#,(car n*))
                                     (let ([#,(car v*) (irregex-match-substring m '#,(car n*))])
                                       #,(loop (cdr n*) (cdr v*)))
                                     (errorf 'match "invalid index number or name for regex: ~a" '#,(car n*)))))
                       (#,fk))))]
            [_ (syntax-error pat "match: invalid regex pattern:")])))
      (syntax-case pat ()
        ;; TODO reg is a var ref
        [(_ reg . rest)
         (let ([reg (syntax->datum #'reg)])
           (or (string? reg) (list reg)))
         ;; how to directly embed the compiled regex obj?
         (with-syntax ([(regex) (generate-temporaries '(regex))])
           #`(let ([regex (irregex reg)])
               #,(handle-rest expr-id #'regex #'rest body fk)))]
        [(_ reg . rest)
         ;; `reg` could be arbitrary expression, check whether it evals to string/reg
         (todo)]
        [_ (syntax-error pat "match: invalid regex pattern format:")])))


  (define hash-expander
    (lambda (pp rho expr-id pat body fk)
      (define literal?
        (lambda (lit)
          (let ([p (syntax->datum lit)])
            (or (number? p)
                (boolean? p)
                (string? p)
                (char? p)))))
      (syntax-case pat ()
        ;; `k`s are just literals right now
        [(h (k pat0) (k* pat*) ...)
         (andmap literal? (cons #'k #'(k* ...)))
         #`(if (hashtable? #,expr-id)
               #,(let loop ([k* (cons #'k #'(k* ...))] [pat* (cons #'pat0 #'(pat* ...))])
                   (if (null? k*)
                       body
                       (with-syntax ([(v) (generate-temporaries '(v))])
                         #`(if (hashtable-contains? #,expr-id #,(car k*))
                               (let ([v (hashtable-ref #,expr-id #,(car k*) #f)])
                                 #,(pp rho #'v (car pat*)
                                       (loop (cdr k*) (cdr pat*))
                                       fk))
                               (#,fk)))))
               (#,fk))]
        [else (syntax-error pat "match: invalid pattern for hash: ")])))

  )
