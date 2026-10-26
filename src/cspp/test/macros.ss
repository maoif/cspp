#!chezscheme
(library (cspp test macros)
  (export test-suite test-case test-expand test-compile test)
  (import (cspp chez)
          (cspp test descriptor))

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
              (let* ([arity (option-arity (car items))]
                     [args (take (cdr items) arity (car items))]
                     [rest (drop (cdr items) arity (car items))])
                (loop rest (cons (cons (car items) args) options)))]
             [else (values (reverse options) items)]))))
      (define parse-option
        (lambda (key rest parameter-ids)
          (let ([datum (syntax->datum key)])
            (case datum
              [(:tags)
               (unless (= (length rest) 1)
                 (syntax-error key ":tags expects one tag list"))
               (syntax-case (car rest) ()
                 [(tag ...)
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
                               #,(build-bindings parameter-ids (list (car rest))))
                             #,(cadr rest)))]
              [(:only-when)
               (unless (= (length rest) 2)
                 (syntax-error key ":only-when expects expression and reason"))
               #`(cons 'skip-when
                       (cons (lambda (params)
                               (not #,(build-bindings parameter-ids (list (car rest)))))
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
                               #,(build-bindings parameter-ids (list (car rest))))
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
        (lambda (options parameter-ids)
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
                         (cons (parse-option (caar options) (cdar options) parameter-ids) entries)
                         parameterize-tables)])]
             [else
              (loop (cdr options)
                    (cons (parse-option (caar options) (cdar options) parameter-ids) entries)
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
      (define check-duplicate-parameter-names
        (lambda (options)
          (let loop-options ([options options] [seen '()])
            (cond
             [(null? options) #t]
             [(eq? (syntax->datum (caar options)) ':parameterize)
              (syntax-case (cadar options) ()
                [([name ...] row ...)
                 (begin
                   (for-each
                    (lambda (row)
                      (let ([values (syntax->datum row)])
                        (unless (and (list? values) (= (length values) (length #'(name ...))))
                          (syntax-error row "parameter row length must match name list"))))
                    #'(row ...))
                 (let loop-names ([names #'(name ...)] [seen seen])
                   (cond
                    [(null? names) (loop-options (cdr options) seen)]
                    [(ormap (lambda (id) (free-identifier=? id (car names))) seen)
                     (syntax-error (car names) "duplicate parameter name")]
                    [else (loop-names (cdr names) (cons (car names) seen))])))]
                [_ (loop-options (cdr options) seen)])]
             [else (loop-options (cdr options) seen)]))))
      (define fixture-names
        (lambda (options)
          (let loop ([options options] [out '()])
            (cond
             [(null? options) (reverse out)]
             [(eq? (syntax->datum (caar options)) ':fixture)
              (syntax-case (cadar options) ()
                [([name expr] ...)
                 (loop (cdr options) (append (reverse #'(name ...)) out))]
                [_ (loop (cdr options) out)])]
             [else (loop (cdr options) out)]))))
      (define unique-identifiers
        (lambda (ids)
          (let loop ([ids ids] [out '()])
            (cond
             [(null? ids) (reverse out)]
             [(ormap (lambda (id) (bound-identifier=? id (car ids))) out)
              (loop (cdr ids) out)]
             [else (loop (cdr ids) (cons (car ids) out))]))))
      (define build-bindings
        (lambda (ids body)
          (let loop ([ids (unique-identifiers ids)] [body body])
            (if (null? ids)
                #`(begin #,@body)
                (let ([id (car ids)])
                  #`(let ([binding (assq '#,(datum->syntax id (syntax->datum id)) params)])
                      (let ([#,id (if binding (cdr binding) #f)])
                        #,(loop (cdr ids) body))))))))
      (lambda (lookup)
        (syntax-case stx ()
          [(_ kind name item ...)
           (let-values ([(options body) (split-items #'(item ...))])
             (check-duplicate-parameter-names options)
             (let* ([inherited (lookup #'$test-binding-context)]
                    [inherit-names
                     (lambda (symbols)
                       (let loop ([symbols symbols] [seen '()] [out '()])
                         (cond
                          [(null? symbols) (reverse out)]
                          [(memq (car symbols) seen) (loop (cdr symbols) seen out)]
                          [else
                           (let ([symbol (car symbols)] [references '()])
                             ;; Wrapper-generated names may have a different lexical context
                             ;; from their body or option expressions. Bind the actual references.
                             (let visit ([form #'(item ...)])
                               (if (identifier? form)
                                   (when (eq? (syntax->datum form) symbol)
                                     (set! references (cons form references)))
                                   (syntax-case form ()
                                     [(head . tail) (begin (visit #'head) (visit #'tail))]
                                     [#(element ...) (for-each visit #'(element ...))]
                                     [_ (void)])))
                             (loop (cdr symbols) (cons symbol seen)
                                   (append (if (null? references)
                                               (list (datum->syntax #'name symbol))
                                               references)
                                           out)))])))]
                    [fixture-ids
                     (if (exists (lambda (option) (eq? (syntax->datum (car option)) ':fixture))
                                 options)
                         (fixture-names options)
                         (inherit-names (car inherited)))]
                    [parameter-ids
                     (if (exists (lambda (option) (eq? (syntax->datum (car option)) ':parameterize))
                                 options)
                         (parameter-names options)
                         (inherit-names (cadr inherited)))]
                    [metadata (build-metadata options parameter-ids)])
               (case (syntax->datum #'kind)
                 [(case)
                  #`(let ([descriptor
                           (make-test-case
                            'name
                            (lambda (params)
                              #,(build-bindings
                                 (append parameter-ids fixture-ids)
                                 body))
                            #,metadata)])
                      (test-register! (current-test-registry) descriptor)
                      descriptor)]
                 [(suite group)
                  #`(let ([descriptor
                           (make-test-suite
                            'name
                            (fluid-let-syntax
                              ([$test-binding-context
                                (make-compile-time-value
                                 '#,(datum->syntax #'name
                                      (list (map syntax->datum fixture-ids)
                                            (map syntax->datum parameter-ids))))])
                              (parameterize ([current-test-registry (make-test-registry)])
                                (list #,@body)))
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
  |#
  (define-syntax test-case
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper case name item ...)]))

  #|macro:test-suite
  The `test-suite` macro defines and registers a suite descriptor named `name`.
  Config clauses are inherited by child descriptors. Nested definition macros
  register only as children of this suite, so each child runs once. Child bodies
  bind inherited fixture and parameter names; conditional rules bind parameters.
  A child's explicit fixture list or parameter tables replace the corresponding suite list.
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
