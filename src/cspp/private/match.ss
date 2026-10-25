#!chezscheme
(library (cspp private match)
  (export record-expander datatype-expander
          box-expander regex-expander hash-expander
          extension-subpatterns
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

  (define pattern-value
    (lambda (pattern)
      (syntax-case pattern () [(_ value) #'value])))

  ;; Explicit record modes fix the capture set before descriptor fields are available.
  (define record-pattern-info
    (lambda (rho pattern)
      (syntax-case pattern ()
        [(_ type mode patterns ...)
         (and (identifier? #'type) (memq (datum mode) '(:fields :values)))
         (let* ([type (or (valid-record-type? rho #'type)
                          (valid-record-type? rho ($construct-name #'type "$record-" #'type)))]
                [named? (eq? (datum mode) ':fields)]
                [patterns #'(patterns ...)])
           (unless type (syntax-error pattern "match: invalid record type:"))
           (when named?
             (let loop ([patterns patterns] [names '()])
               (unless (null? patterns)
                 (syntax-case (car patterns) ()
                   [(name value)
                    (identifier? #'name)
                    (let ([name (datum name)])
                      (when (memq name names)
                        (syntax-error (car patterns) "match: duplicate record field:"))
                      (loop (cdr patterns) (cons name names)))]
                   [_ (syntax-error (car patterns) "match: expected (field pattern):")]))))
           (values type named? patterns))]
        [_ (syntax-error pattern "match: expected (record type :fields/:values pattern ...):")])))

  #|proc:datatype-record-pattern
  The `datatype-record-pattern` helper validates a datatype's variant and returns an equivalent
  explicit record pattern. `rho` resolves declared types and `pattern` is datatype extension
  syntax. For example, `(datatype Tree Leaf :values ,x)` becomes
  `(record $datatype-Tree-Leaf :values ,x)`. Singleton shorthand becomes an empty `:values` form.
  |#
  (define datatype-record-pattern
    (lambda (rho pattern)
      (syntax-case pattern ()
        [(_ type variant rest ...)
         (and (identifier? #'type) (identifier? #'variant))
         (let ([selected ($construct-name #'type "$datatype-" #'type "-" #'variant)])
           (unless (variant-of? rho #'type selected)
             (syntax-error pattern "match: invalid datatype/variant:"))
           (syntax-case #'(rest ...) ()
             [()
              (unless (fxzero?
                       (vector-length (record-type-field-names (get-type-descriptor rho selected))))
                (syntax-error pattern "match: non-singleton variant requires :fields or :values:"))
              #`(record #,selected :values)]
             [(mode patterns ...)
              (memq (datum mode) '(:fields :values))
              #`(record #,selected mode patterns ...)]
             [_ (syntax-error pattern "match: expected datatype variant :fields/:values:")]))]
        [_ (syntax-error pattern "match: expected (datatype type variant mode pattern ...):")])))

  #|proc:extension-subpatterns
  The `extension-subpatterns` procedure returns the pattern positions of an extension.
  `compile-environment` resolves extension expanders, and `pattern` is the syntax after the
  leading comma. The result is a list of syntax objects that declare captures; extension
  expressions, record type names, and named field identifiers are excluded.
  For custom expanders, capture discovery invokes the expander with a collector callback;
  every pattern passed to that callback is treated as a subpattern. Expanders should call the
  callback only for positions that are matched, and should keep expansion free of side effects.

  Example input:
  ```scheme
  (extension-subpatterns rho #'(record Point :fields (x ,x)))
  ```
  Example output: a one-element list containing the syntax object for `,x`.
  |#
  (define extension-subpatterns
    (lambda (compile-environment pattern)
      (pcheck ([procedure? compile-environment] [list? pattern])
        (let* ([extension (car pattern)]
               [arguments (cdr pattern)]
               [info (compile-environment ($construct-name extension extension "-expander"))]
               [expander (and (pair? info) (eq? (car info) '*MATCH-EXPANDER*) (cdr info))])
          (cond
           [(eq? (syntax->datum extension) 'record)
            (let-values ([(type named? patterns)
                          (record-pattern-info compile-environment pattern)])
              (if named? (map pattern-value patterns) patterns))]
           ;; Built-in spelling has the same precedence as matcher generation.
           [(eq? (syntax->datum extension) 'regex)
            (map pattern-value (cdr arguments))]
           [(eq? (syntax->datum extension) 'hash)
            (map pattern-value arguments)]
           [(eq? (syntax->datum extension) 'box) arguments]
           [(eq? (syntax->datum extension) 'datatype)
            (let-values ([(type named? patterns)
                          (record-pattern-info compile-environment
                                               (datatype-record-pattern
                                                compile-environment pattern))])
              (if named? (map pattern-value patterns) patterns))]
           [else
            ;; Custom expanders decide which arguments are patterns.  Ask the
            ;; expander through a collector callback instead of treating every
            ;; argument as a capture-bearing pattern.  This keeps expression
            ;; arguments and ignored selectors out of ellipsis captures.
            (let ([captures '()]
                  [expr-id (car (generate-temporaries '(expr)))]
                  [failure (car (generate-temporaries '(failure)))])
              (define (collect pp-rho pp-expr-id pp-pattern pp-body pp-failure)
                (set! captures (cons pp-pattern captures))
                #'#t)
              (if expander
                  (begin
                    (expander collect compile-environment expr-id pattern #'#t failure)
                    (reverse captures))
                  arguments))])))))


  #|macro:define-match-expander
  The `define-match-expander` macro registers `name` as a custom `match` extension.
  `expander` receives `(process-pattern rho expr-id pattern body failure)` and returns
  expansion syntax. For example, a `pair` expander can make `,(pair ,a ,b)` return the
  two fields, so `(match (cons 1 2) [,(pair ,x ,y) (+ x y)])` returns `3`.
  |#
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


  #|proc:record-expander
  The `record-expander` procedure expands the explicit `record` extension.
  `process-pattern` compiles each field pattern; `compile-environment` resolves the record;
  `expr-id` is the input identifier; `pattern` is `(record Type :fields/:values ...)`; `body` is the
  success expression; and `failure` is a zero-argument failure procedure. It returns matcher
  syntax. For example, a `Point` value `(make-Point 10 20)` with
  `,(record Point :fields (y ,y))` binds `y` to `20`.
  |#
  (define record-expander
    (lambda (pp rho expr-id pat body fk)
      (pcheck ([procedure? pp rho] [identifier? expr-id fk])
        (let-values ([(type named? patterns) (record-pattern-info rho pat)])
          (let* ([descriptor (get-type-descriptor rho type)]
                 [static? (record-type-descriptor? descriptor)]
                 [names (and named? (map (lambda (pattern)
                                           (syntax-case pattern () [(name value) (datum name)]))
                                         patterns))]
                 [subpatterns (if named? (map pattern-value patterns) patterns)]
                 [count (length patterns)])
            (define (field-indices fields)
              (if named? (map (lambda (name) (vmemq name fields)) names) (iota count)))
            (define (valid? fields indices)
              (if named? (andmap number? indices) (fx= count (vector-length fields))))
            (when static?
              (let ([fields (record-type-field-names descriptor)])
                (unless (valid? fields (field-indices fields))
                  (syntax-error pat "match: invalid record pattern:"))))
            ;; Local descriptors already have private identifiers in rho. Using that binding
            ;; directly keeps a capture named after the record type from hiding it later.
            (with-syntax ([(rtd fields) (generate-temporaries '(rtd fields))]
                          [(indices ...) (generate-temporaries patterns)])
              (define (generate offsets)
                (let loop ([patterns subpatterns] [offsets offsets])
                  (if (null? patterns)
                      body
                      (with-syntax ([(field) (generate-temporaries '(field))])
                        #`(let ([field ((record-accessor rtd #,(car offsets)) #,expr-id)])
                            #,(pp rho #'field (car patterns)
                                  (loop (cdr patterns) (cdr offsets)) fk))))))
              #`(let ([rtd #,(if static? #`'#,descriptor descriptor)])
                  (if ((record-predicate rtd) #,expr-id)
                      #,(if static?
                            (generate (field-indices (record-type-field-names descriptor)))
                            (if named?
                                #`(let* ([fields (record-type-field-names rtd)]
                                         #,@(map (lambda (index name)
                                                   #`[#,index
                                                      (vmemq '#,(datum->syntax type name) fields)])
                                                 #'(indices ...) names))
                                    (unless (and indices ...)
                                      (errorf 'match "invalid record pattern: ~s" '#,pat))
                                    #,(generate #'(indices ...)))
                                #`(begin
                                    (unless (fx= (vector-length (record-type-field-names rtd)) #,count)
                                      (errorf 'match "invalid record pattern: ~s" '#,pat))
                                    #,(generate (iota count)))))
                      (#,fk)))))))))


  #|proc:datatype-expander
  The `datatype-expander` procedure expands an explicit CSPP datatype pattern through the record
  compiler. Its callback, environment, input, success, and failure arguments have the meanings
  described by `record-expander`. For example, `,(datatype Tree Leaf :values ,x)` captures the
  leaf value, and `,(datatype Tree Empty)` checks the zero-field variant's type. Variant selection
  uses the declared descriptor, so earlier captures may shadow type and singleton names safely.
  |#
  (define datatype-expander
    (lambda (pp rho expr-id pat body fk)
      (pcheck ([procedure? pp rho] [identifier? expr-id fk])
        (record-expander pp rho expr-id (datatype-record-pattern rho pat) body fk))))


  #|proc:box-expander
  The `box-expander` procedure expands `,(box <pattern>)`.
  It checks that the input is a box, applies `<pattern>` to its contents, and calls `failure`
  otherwise. For example, `(match (box 10) [,(box ,x) x])` returns `10`.
  |#
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


  #|proc:regex-expander
  The `regex-expander` procedure expands `,(regex <expression> <group> ...)`.
  It matches a string and binds each named or numbered group to its captured substring.
  For example, `(match "x=10" [,(regex "x=(.*)" (1 ,x)) x])` returns `"10"`.
  |#
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


  #|proc:hash-expander
  The `hash-expander` procedure expands `,(hash (<literal> <pattern>) ...)`.
  It requires every key and the input hashtable to be present, then applies each value pattern.
  For example, `(match table [,(hash ("key" ,x)) x])` returns the value stored at `"key"`.
  Keys are literal numbers, booleans, strings, characters, or bare symbols, never expressions.
  An empty `,(hash)` checks only that the input is a hashtable.
  A list that merely resembles this syntax is matched structurally with a guard in `match`.
  |#
  (define hash-expander
    (lambda (pp rho expr-id pat body fk)
      (define literal?
        (lambda (lit)
          (let ([p (syntax->datum lit)])
            (or (symbol? p)
                (number? p)
                (boolean? p)
                (string? p)
                (char? p)))))
      (syntax-case pat ()
        ;; `k`s are just literals right now
        [(h (k pat) ...)
         (andmap literal? #'(k ...))
         #`(if (hashtable? #,expr-id)
               #,(let loop ([k* #'(k ...)] [pat* #'(pat ...)])
                   (if (null? k*)
                       body
                       (with-syntax ([(v) (generate-temporaries '(v))])
                         #`(if (hashtable-contains? #,expr-id '#,(car k*))
                               (let ([v (hashtable-ref #,expr-id '#,(car k*) #f)])
                                 #,(pp rho #'v (car pat*)
                                       (loop (cdr k*) (cdr pat*))
                                       fk))
                               (#,fk)))))
               (#,fk))]
        [else (syntax-error pat "match: invalid pattern for hash: ")])))

  )
