#!chezscheme
(library (cspp test macros)
  (export test-suite test-case test-expand test-compile test)
  (import (cspp chez)
          (cspp test descriptor))

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
        (lambda (options)
          (let ([parameter-ids (parameter-names options)])
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
                      parameterize-tables)])))))
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
                 (let loop-names ([names #'(name ...)] [seen seen])
                   (cond
                    [(null? names) (loop-options (cdr options) seen)]
                    [(ormap (lambda (id) (free-identifier=? id (car names))) seen)
                     (syntax-error (car names) "duplicate parameter name")]
                    [else (loop-names (cdr names) (cons (car names) seen))]))]
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
             [(ormap (lambda (id) (free-identifier=? id (car ids))) out)
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
      (syntax-case stx ()
        [(_ kind name item ...)
         (let-values ([(options body) (split-items #'(item ...))])
           (check-duplicate-parameter-names options)
           (let ([metadata (build-metadata options)])
             (case (syntax->datum #'kind)
               [(case)
                #`(let ([descriptor
                         (make-test-case
                          'name
                          (lambda (params)
                            #,(build-bindings
                               (append (parameter-names options) (fixture-names options))
                               body))
                          #,metadata)])
                    (test-register! (current-test-registry) descriptor)
                    descriptor)]
               [(suite group)
                #`(let ([descriptor
                         (make-test-suite
                          'name
                          (parameterize ([current-test-registry (make-test-registry)])
                            (list #,@body))
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
        [_ (syntax-error stx "invalid test macro helper form")])))

  #|macro:test-case
The `test-case` macro defines and registers a runtime test descriptor named
`name`. Config clauses may include `:tags`, `:parameterize`, `:skip-when`,
`:xfail`, `:capture`, and output expectations. Body forms are evaluated by the
runner.
|#
  (define-syntax test-case
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper case name item ...)]))

  #|macro:test-suite
The `test-suite` macro defines and registers a suite descriptor named `name`.
Config clauses are inherited by child descriptors. Nested definition macros
register only as children of this suite, so each child runs once.
|#
  (define-syntax test-suite
    (syntax-rules ()
      [(_ name item ...)
       ($test-macro-helper suite name item ...)]))

  #|macro:test-expand
The `test-expand` macro defines and registers a macro expansion test descriptor
named `name`.
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
