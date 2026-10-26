#!chezscheme
(library (cspp test macros)
  (export test-suite test-case test-expand test-compile test)
  (import (cspp chez)
          (cspp test descriptor)
          (for (cspp test private common) expand))

  (define-syntax $test-binding-context
    (make-compile-time-value '(() ())))

  (define-syntax $test-macro-helper
    (lambda (stx)
      (define keyword?
        (lambda (stx)
          (let ([datum (syntax->datum stx)])
            (and (symbol? datum)
                 (let ([text (symbol->string datum)])
                   (and (positive? (string-length text))
                        (char=? (string-ref text 0) #\:)))))))
      (define keyword->option-name
        (lambda (datum)
          (string->symbol (substring (symbol->string datum)
                                     1
                                     (string-length (symbol->string datum))))))
      (define quote-row
        (lambda (context row)
          #`'#,(datum->syntax context (syntax->datum row))))
      (define split-items
        (lambda (items)
          (define option-arity
            (lambda (key)
              (case (syntax->datum key)
                [(:tags :parameterize :skip :xfail :xfail-strict :capture :stdout :stderr :output :raises)
                 1]
                [(:requires-chez-version :requires-file) 1]
                [(:fixture :before :after :condition-handler :warnings) 1]
                [(:skip-when :only-when :xfail-when) 2]
                [else (syntax-error key "unknown test option")])))
          (define take
            (lambda (items n key)
              (cond
               [(fx= n 0) '()]
               [(null? items) (syntax-error key "test option is missing arguments")]
               [else (cons (car items) (take (cdr items) (fx- n 1) key))])))
          (define drop
            (lambda (items n key)
              (cond
               [(fx= n 0) items]
               [(null? items) (syntax-error key "test option is missing arguments")]
               [else (drop (cdr items) (fx- n 1) key)])))
          (let loop ([items items] [options '()])
            (cond
             [(null? items) (values (reverse options) '())]
             [(keyword? (car items))
              (unless (eq? (syntax->datum (car items)) ':parameterize)
                (let ([key (case (syntax->datum (car items))
                             [(:skip :skip-when :only-when) ':skip-when]
                             [(:xfail :xfail-when) ':xfail-when]
                             [else (syntax->datum (car items))])])
                  (when (exists
                          (lambda (option)
                            (eq? key
                              (case (syntax->datum (car option))
                                [(:skip :skip-when :only-when) ':skip-when]
                                [(:xfail :xfail-when) ':xfail-when]
                                [else (syntax->datum (car option))])))
                          options)
                    (syntax-error (car items) "duplicate test option"))))
              (let* ([arity (option-arity (car items))]
                     [args (take (cdr items) arity (car items))]
                     [rest (drop (cdr items) arity (car items))])
                (loop rest (cons (cons (car items) args) options)))]
             [else
              (when (exists keyword? items)
                (syntax-error stx "test options must precede body expressions"))
              (values (reverse options) items)]))))
      (define parse-option
        (lambda (key rest parameter-ids inherited-parameter-ids)
          (let ([datum (syntax->datum key)])
            (case datum
              [(:tags)
               (unless (= (length rest) 1)
                 (syntax-error key ":tags expects one tag list"))
               (syntax-case (car rest) ()
                 [(tag ...)
                  (andmap identifier? #'(tag ...))
                  #'(cons 'tags '(tag ...))]
                 [_ (syntax-error (car rest) "invalid :tags list")])]
              [(:parameterize)
               (unless (= (length rest) 1)
                 (syntax-error key ":parameterize expects one table"))
               (syntax-case (car rest) ()
                 [([name ...] row ...)
                  #`(cons 'parameterize
                          (list (cons '(name ...)
                                      (list #,@(map (lambda (row) (quote-row key row))
                                                    #'(row ...))))))]
                 [_ (syntax-error (car rest) "invalid :parameterize table")])]
              [(:skip)
               (unless (= (length rest) 1)
                 (syntax-error key ":skip expects one reason"))
               #`(cons 'skip-when (cons (lambda (params) #t) #,(car rest)))]
              [(:skip-when)
               (unless (= (length rest) 2)
                 (syntax-error key ":skip-when expects expression and reason"))
               #`(cons 'skip-when
                       (cons (lambda (params)
                               #,(build-callback parameter-ids inherited-parameter-ids
                                                  (list (car rest))))
                             #,(cadr rest)))]
              [(:only-when)
               (unless (= (length rest) 2)
                 (syntax-error key ":only-when expects expression and reason"))
               #`(cons 'skip-when
                       (cons (lambda (params)
                               (not #,(build-callback parameter-ids inherited-parameter-ids
                                                       (list (car rest)))))
                             #,(cadr rest)))]
              [(:xfail)
               (unless (= (length rest) 1)
                 (syntax-error key ":xfail expects one reason"))
               #`(cons 'xfail-when (cons (lambda (params) #t) #,(car rest)))]
              [(:xfail-when)
               (unless (= (length rest) 2)
                 (syntax-error key ":xfail-when expects expression and reason"))
               #`(cons 'xfail-when
                       (cons (lambda (params)
                               #,(build-callback parameter-ids inherited-parameter-ids
                                                  (list (car rest))))
                             #,(cadr rest)))]
              [(:xfail-strict :stdout :stderr :output :raises)
               (unless (= (length rest) 1)
                 (syntax-error key "test option expects one expression"))
               #`(cons '#,(datum->syntax key (keyword->option-name datum)) #,(car rest))]
              [(:capture)
               (unless (= (length rest) 1)
                 (syntax-error key ":capture expects one mode"))
               (let ([mode (car rest)])
                 (if (identifier? mode)
                     #`(cons 'capture '#,mode)
                     #`(cons 'capture #,mode)))]
              [(:requires-chez-version)
               (unless (= (length rest) 1)
                 (syntax-error key ":requires-chez-version expects one version list"))
               (syntax-case (car rest) ()
                 [(major minor patch)
                  #'(cons 'requires-chez-version (list major minor patch))]
                 [_ (syntax-error (car rest) "invalid :requires-chez-version form")])]
              [(:requires-file)
               (unless (= (length rest) 1)
                 (syntax-error key ":requires-file expects one path"))
               #`(cons 'requires-file #,(car rest))]
              [(:fixture)
               (unless (= (length rest) 1)
                 (syntax-error key ":fixture expects one fixture binding list"))
               (syntax-case (car rest) ()
                 [([name expr] ...)
                  #'(cons 'fixtures (list (cons 'name (lambda () expr)) ...))]
                 [_ (syntax-error (car rest) "invalid :fixture binding list")])]
              [(:before :after :condition-handler :warnings)
               (unless (= (length rest) 1)
                 (syntax-error key "test option expects one expression"))
               #`(cons '#,(datum->syntax key (keyword->option-name datum)) #,(car rest))]
              [else (syntax-error key "unknown test option")]))))
      (define build-metadata
        (lambda (options parameter-ids inherited-parameter-ids)
          (let loop ([options options] [entries '()] [parameterize-tables '()])
            (cond
             [(null? options)
              #`(list #,@(reverse entries)
                      #,@(if (null? parameterize-tables)
                             '()
                             (list #`(cons 'parameterize
                                           (append #,@(reverse parameterize-tables))))))]
             [(eq? (syntax->datum (caar options)) ':parameterize)
              (syntax-case (cadar options) ()
                [([name ...] row ...)
                 (loop (cdr options)
                       entries
                       (cons #`(list (cons '(name ...)
                                           (list #,@(map (lambda (row)
                                                           (quote-row (caar options) row))
                                                         #'(row ...)))))
                             parameterize-tables))]
                [_ (loop (cdr options)
                         (cons (parse-option (caar options) (cdar options)
                                             parameter-ids inherited-parameter-ids) entries)
                         parameterize-tables)])]
             [else
              (loop (cdr options)
                    (cons (parse-option (caar options) (cdar options)
                                        parameter-ids inherited-parameter-ids) entries)
                    parameterize-tables)]))))
      (define parameter-names
        (lambda (options)
          (let loop ([options options] [out '()])
            (cond
             [(null? options) (reverse out)]
             [(eq? (syntax->datum (caar options)) ':parameterize)
              (syntax-case (cadar options) ()
                [([name ...] row ...)
                 (loop (cdr options) (append (reverse #'(name ...)) out))]
                [_ (loop (cdr options) out)])]
             [else (loop (cdr options) out)]))))
      (define check-parameter-tables
        (lambda (options)
          (let ([tables
                 (map (lambda (option) (syntax->datum (cadr option)))
                      (filter (lambda (option)
                                (eq? (syntax->datum (car option)) ':parameterize))
                              options))])
            (guard (condition
                    [else (syntax-error stx (condition-message condition))])
              (test-check-parameter-tables 'test-case tables)))))
      (define fixture-names
        (lambda (options)
          (when (> (length
                     (filter (lambda (option) (eq? (syntax->datum (car option)) ':fixture))
                             options))
                   1)
            (syntax-error stx "only one :fixture clause is allowed per descriptor"))
          (let loop ([options options] [out '()])
            (cond
             [(null? options) (reverse out)]
             [(eq? (syntax->datum (caar options)) ':fixture)
              (syntax-case (cadar options) ()
                [([name expr] ...)
                 (loop (cdr options) (append (reverse #'(name ...)) out))]
                [_ (loop (cdr options) out)])]
             [else (loop (cdr options) out)]))))
      (define build-bindings
        (lambda (ids body)
          (let loop ([ids ids] [body body])
            (if (null? ids)
                #`(begin #,@body)
                (let ([id (car ids)])
                  #`(let ([binding (assq '#,(datum->syntax id (syntax->datum id)) params)])
                      (let ([#,id (if binding (cdr binding) #f)])
                        #,(loop (cdr ids) body))))))))
      (define build-callback
        (lambda (local-ids inherited-ids body)
          ;; Rebind suite aliases to fresh lexical variables at each deferred code boundary.
          ;; The expander resolves references hygienically; no body traversal is needed.
          (let ([values (generate-temporaries inherited-ids)])
            #`(let (#,@(map (lambda (id value)
                             #`[#,value
                                (let ([binding
                                       (assq '#,(datum->syntax id (syntax->datum id)) params)])
                                  (if binding (cdr binding) #f))])
                           inherited-ids values))
                (fluid-let-syntax
                  (#,@(map (lambda (id value)
                             #`[#,id
                                (identifier-syntax
                                  [reference #,value]
                                  [(set! reference new-value) (set! #,value new-value)])])
                           inherited-ids values))
                  #,(build-bindings local-ids body))))))
      (define lexical-bindings
        (lambda (ids)
          ;; Establish suite declaration scopes before expanding children. Values exist only
          ;; inside callbacks, where build-callback replaces these placeholder transformers.
          (map (lambda (id)
                 #`[#,id
                    (lambda (reference)
                      (syntax-error reference "test input is unavailable outside its callback"))])
               ids)))
      (lambda (lookup)
        (syntax-case stx ()
          [(_ kind name item ...)
           (let-values ([(options body) (split-items #'(item ...))])
             (unless (and (identifier? #'name) (not (keyword? #'name)))
               (syntax-error #'name "test name must be an identifier"))
             (when (and (memq (syntax->datum #'kind) '(case compile)) (null? body))
               (syntax-error stx "test requires at least one body expression"))
             (check-parameter-tables options)
             (let* ([inherited (lookup #'$test-binding-context)]
                    [local-fixtures?
                     (exists (lambda (option) (eq? (syntax->datum (car option)) ':fixture))
                             options)]
                    [local-fixture-ids (if local-fixtures? (fixture-names options) '())]
                    [fixture-ids
                     (if local-fixtures?
                         local-fixture-ids
                         (car inherited))]
                    [local-parameters?
                     (exists (lambda (option) (eq? (syntax->datum (car option)) ':parameterize))
                             options)]
                    [local-parameter-ids (if local-parameters? (parameter-names options) '())]
                    [parameter-ids
                     (if local-parameters?
                         local-parameter-ids
                         (cadr inherited))]
                    [inherited-fixture-ids (if local-fixtures? '() fixture-ids)]
                    [inherited-parameter-ids (if local-parameters? '() parameter-ids)]
                    [metadata (build-metadata options local-parameter-ids
                                              inherited-parameter-ids)])
               (guard (condition
                       [else (syntax-error stx (condition-message condition))])
                 (test-check-fixture-names 'test-case (map syntax->datum fixture-ids)
                                           (map syntax->datum parameter-ids)))
               (case (syntax->datum #'kind)
                 [(case)
                  #`(let ([descriptor
                           (make-test-case
                            'name
                            (lambda (params)
                              #,(build-callback
                                 (append local-parameter-ids local-fixture-ids)
                                 (append inherited-parameter-ids inherited-fixture-ids)
                                 body))
                            #,metadata)])
                      (test-register! (current-test-registry) descriptor)
                      descriptor)]
                 [(suite group)
                  #`(let ([descriptor
                           (make-test-suite
                            'name
                            (let-syntax
                              (#,@(lexical-bindings local-parameter-ids)
                               #,@(lexical-bindings local-fixture-ids))
                              (fluid-let-syntax
                                ([$test-binding-context
                                  (make-compile-time-value
                                   (list
                                     (list #,@(map (lambda (id) #`(syntax #,id)) fixture-ids))
                                     (list #,@(map (lambda (id) #`(syntax #,id)) parameter-ids))))])
                                (parameterize ([current-test-registry (make-test-registry)])
                                  (list #,@body))))
                            #,metadata)])
                      (test-register! (current-test-registry) descriptor)
                      descriptor)]
                 [(expand)
                  (unless (= (length body) 1)
                    (syntax-error #'name "test-expand expects one body form"))
                  #`(let ([descriptor (make-test-expand 'name '#,(car body) #,metadata)])
                      (test-register! (current-test-registry) descriptor)
                      descriptor)]
                 [(compile)
                  #`(let ([descriptor (make-test-compile 'name '#,body #,metadata)])
                      (test-register! (current-test-registry) descriptor)
                      descriptor)]
                 [else (syntax-error #'kind "unknown test macro kind")])))]
          [_ (syntax-error stx "invalid test macro helper form")]))))

  #|macro:test-case
  The `test-case` macro defines and registers a runtime test descriptor named
  `name`. Config clauses may include `:tags`, `:parameterize`, `:skip-when`,
  `:xfail`, `:capture`, and output expectations. Body forms are evaluated by the
  runner. Conditional rules can reference the case's named parameters.
  `:xfail-strict` overrides the configuration's strictness for this case.
  `:before` and `:after` receive a fixture-value association list with signature
  `(lambda (fixtures) unspecified)`. Cleanup runs even if setup or `:before` raises;
  then `:after` receives only the fixtures successfully acquired in declaration order.
  If setup or the body and cleanup both raise, the primary condition takes precedence.
  A custom `:condition-handler` has signature `(lambda (condition) disposition)`;
  `handled` passes the body, `failed` fails it, and other dispositions mark it errored.
  Captured output expectations are checked separately after successful or handled bodies;
  their failures do not invoke the custom handler.
  Parameters and fixtures are lexical variables around the case body. Parameters are also
  bound around skip and expected-failure conditions; fixtures are unavailable to those rules.
  Fixture names must be unique and cannot overlap effective parameter names,
  including names inherited from a suite.
  At most one `:fixture` clause is allowed; put all fixture bindings in that clause.
  Fixture initializers are zero-argument thunks evaluated outside parameter bindings.
  |#
  (define-syntax test-case
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper case name item ...)]))

  #|macro:test-suite
  The `test-suite` macro defines and registers a suite descriptor named `name`.
  Config clauses are inherited by child descriptors. Each immediate body expression
  must return one descriptor, which becomes one child; registration inside a grouping
  expression does not add extra children. Nested definitions are isolated from the
  surrounding registry, so each collected child runs once. Child bodies
  lexically inherit fixture and parameter names; conditional rules lexically inherit parameters.
  Ordinary Scheme shadowing applies inside child forms.
  A child's explicit fixture list or parameter tables replace the corresponding suite list.
  Inherited conditional callbacks keep their parent declaration bindings; a child replacing
  parameter tables must preserve names those callbacks require or override the conditions too.
  |#
  (define-syntax test-suite
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper suite name item ...)]))

  #|macro:test-expand
The `test-expand` macro defines and registers a macro expansion test descriptor
named `name`. The source is expanded at run time in an immutable CSPP
environment. It must be self-contained: place local macro definitions inside
the tested form. Surrounding lexical or user top-level bindings are not imported;
unbound references are errors. Skip policies run before expansion.
|#
  (define-syntax test-expand
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper expand name item ...)]))

  #|macro:test-compile
The `test-compile` macro defines and registers a compile test descriptor named
`name`.
|#
  (define-syntax test-compile
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper compile name item ...)]))

  #|macro:test
  The `test` macro groups nested descriptors under shared config metadata,
  registers the assembled suite, and returns its descriptor.
  |#
  (define-syntax test
    (syntax-rules ()
      [(_ item ...)
       ($test-macro-helper group test item ...)]))

  )
