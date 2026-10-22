#!chezscheme
(library (cspp private match)
  (export system-record-expander record-expander datatype-expander
          box-expander regex-expander hash-expander
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
  (define system-record-expander
    (lambda (pp rho expr-id pat body fk)
      (syntax-case pat ()
        [(dt pats ...)
         (if (record-type-descriptor? (get-type-descriptor rho #'dt))
             (static-system-record-expander pp rho expr-id pat body fk)
             (with-syntax ([(rtd fields indices) (generate-temporaries '(rtd fields indices))])
               (let* ([pats #'(pats ...)]
                      [named? (and (pair? pats) (andmap
                               (lambda (p)
                                 (syntax-case p ()
                                   [(name value) (identifier? #'name) #t]
                                   [_ #f]))
                               pats))]
                      [generate-fields
                       (lambda (patterns named?)
                         (let loop ([patterns patterns] [i 0])
                           (if (null? patterns)
                               body
                               (with-syntax ([(field) (generate-temporaries '(field))])
                                 #`(let ([field ((record-accessor rtd
                                                  #,(if named?
                                                        #`(list-ref indices #,i)
                                                        i))
                                                #,expr-id)])
                                     #,(pp rho #'field (car patterns)
                                           (loop (cdr patterns) (fx1+ i)) fk))))))])
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
                           (cond
                            #,@(if named?
                                   (list #`[(andmap number? indices)
                                            #,(generate-fields
                                               (map (lambda (p) (syntax-case p ()
                                                                  [(name value) #'value])) pats)
                                               #t)])
                                   '())
                            [(fx= (vector-length fields) #,(length pats))
                             #,(if named?
                                   ;; An ambiguous named pattern may have no valid positional
                                   ;; interpretation, e.g. a field named `unquote`.
                                   (guard (condition
                                           [(syntax-violation? condition)
                                            #`(errorf 'match "invalid record pattern: ~s" '#,pat)])
                                     (generate-fields pats #f))
                                   (generate-fields pats #f))]
                            [else (errorf 'match "invalid record pattern: ~s" '#,pat)]))
                         (#,fk))))))])))

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
