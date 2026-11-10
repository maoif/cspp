(library (cspp parser wasm text)
  (export parser-wat-module-syntax parser-wat-module)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm types)
          (cspp parser wasm text lexical)
          (cspp parser wasm text types)
          (cspp parser wasm text instructions)
          (cspp parser wasm normalize)
          (cspp parser wasm private)
          (cspp parser wasm validate))

;;;;===----------------------------------------------------------------------===
;;;; Module grammar helpers and instruction placeholders
;;;;===----------------------------------------------------------------------===

  (define make-immutable-vector
    (lambda value*
      (list->immutable-vector value*)))

  (define optional-value
    (lambda (value)
      (if (null? value) #f value)))

  (define wat-open
    (<~0> (<char> #\() <wat-trivia>))

  (define wat-close
    (<~0> (<char> #\)) <wat-trivia>))

  (define wat-double-quote (integer->char #x22))
  (define wat-semicolon (integer->char #x3b))

  (define wat-field-head
    (lambda (keyword)
      (<~0> <pos> wat-open (<wat-keyword> keyword))))

  (define wat-parenthesized
    (lambda (keyword parser)
      (<~1> (<~0> wat-open (<wat-keyword> keyword)) parser wat-close)))

  (define reserved-character?
    (lambda (character)
      (and (not (memv character
                      (list #\space #\tab #\newline #\return #\( #\)
                            wat-double-quote)))
           (not (char=? character wat-semicolon)))))

  (define <wat-reserved>
    (<wat-token>
     (<as-string>
      (<some> (<satisfy-char> reserved-character? "not a WebAssembly reserved character")))))

  (define forbidden-placeholder-head?
    (lambda (head)
      (member head '("import" "export" "param" "result" "local" "type"))))

  (declare-lazy-parser <wat-generic-item>)

  (define <wat-generic-parenthesized>
    (<map> list->immutable-vector <wat-folded-instruction>))

  (define <wat-positioned-identifier>
    (<map> (lambda (value) (make-wat-index-reference (car value) (cadr value)))
           (<~> <pos> <wat-identifier>)))

  (define <wat-generic-atom>
    <wat-instruction>)

  (define generic-body
    (begin
      (install-lazy-parser!
       <wat-generic-item>
       (</> <wat-generic-parenthesized> <wat-generic-atom>))
      (<map>
       (lambda (instruction**)
         (apply append instruction**))
       (<~0>
        (<many-until>
         (</> (<map> list <wat-instruction>) <wat-folded-instruction>)
         wat-close)
        wat-close))))

  (define inline-import
    (<map> list->immutable-vector
           (wat-parenthesized "import" (<~> <wat-name> <wat-name>))))

  (define inline-export
    (wat-parenthesized "export" <wat-name>))

  (define field-result
    (lambda (pos kind id data import export* abbreviation)
      (make-wat-module-field
       pos kind id data import (list->immutable-vector export*) abbreviation)))

;;;;===----------------------------------------------------------------------===
;;;; Type and import fields
;;;;===----------------------------------------------------------------------===

  (define type-field
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [subtype (cadr value)]
              [recursive-type
               (make-wat-recursive-type-syntax
                pos (make-immutable-vector subtype))])
         (field-result pos 'recursive-type #f recursive-type #f '() #f)))
     (<~> <pos> <wat-type-definition>)))

  (define recursive-type-field
    (<map> (lambda (value)
             (field-result (car value) 'recursive-type #f (cadr value) #f '() #f))
           (<~> <pos> <wat-recursive-type>)))

  (define function-import-description
    (<map> (lambda (value)
             (make-immutable-vector
              'function (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "func")
                      (<optional> <wat-identifier>) <wat-type-use>)
                 wat-close)))

  (define table-import-description
    (<map> (lambda (value)
             (make-immutable-vector
              'table (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "table")
                      (<optional> <wat-identifier>) <wat-table-type>)
                 wat-close)))

  (define memory-import-description
    (<map> (lambda (value)
             (make-immutable-vector
              'memory (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "memory")
                      (<optional> <wat-identifier>) <wat-memory-type>)
                 wat-close)))

  (define global-import-description
    (<map> (lambda (value)
             (make-immutable-vector
              'global (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "global")
                      (<optional> <wat-identifier>) <wat-global-type>)
                 wat-close)))

  (define tag-import-description
    (<map> (lambda (value)
             (make-immutable-vector
              'tag (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "tag")
                      (<optional> <wat-identifier>) <wat-tag-type>)
                 wat-close)))

  (define import-description
    (</> function-import-description table-import-description
         memory-import-description global-import-description tag-import-description))

  (define import-field
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [module-name (cadr value)]
              [name (caddr value)] [description (cadddr value)]
              [id (vector-ref description 1)])
         (field-result pos 'import id
                       (make-immutable-vector
                        module-name name (vector-ref description 0)
                        (vector-ref description 2))
                       #f '() #f)))
     (<~0> (<~> (wat-field-head "import") <wat-name> <wat-name> import-description)
            wat-close)))

;;;;===----------------------------------------------------------------------===
;;;; Function, table, memory, global, and tag fields
;;;;===----------------------------------------------------------------------===

  (define named-local-clause
    (<map> (lambda (value)
             (list (make-wat-binding (car value) (cadr value) (caddr value))))
           (<~0> (<~> (wat-field-head "local")
                      <wat-identifier> <wat-value-type>)
                 wat-close)))

  (define unnamed-local-clause
    (<map> (lambda (value)
             (let ([pos (car value)])
               (map (lambda (type) (make-wat-binding pos #f type)) (cadr value))))
           (<~0> (<~> (wat-field-head "local") (<many> <wat-value-type>))
                 wat-close)))

  (define local-clause (</> named-local-clause unnamed-local-clause))

  (define table-use
    (wat-parenthesized "table" <wat-index-reference>))

  (define memory-use
    (wat-parenthesized "memory" <wat-index-reference>))

  (define offset-clause (wat-parenthesized "offset" <wat-expression>))

  (define direct-offset <wat-generic-parenthesized>)

  (define element-initializer
    (</> (wat-parenthesized "item" <wat-expression>) <wat-generic-parenthesized>))

  (define segment-offset (</> offset-clause direct-offset))

  (define default-function-reference-type
    (make-wasm-reference-type #f 'func))

  (define typed-element-items
    (<map> (lambda (value)
             (make-immutable-vector
              (car value) 'expressions (list->immutable-vector (cadr value))))
           (<~> <wat-reference-type> (<many> element-initializer))))

  (define indexed-element-items
    (<map> (lambda (value)
             (make-immutable-vector
              default-function-reference-type 'indexes
              (list->immutable-vector (cadr value))))
           (<~> (<wat-keyword> "func")
                (<many> <wat-index-reference>))))

  (define element-items (</> typed-element-items indexed-element-items))

  (define legacy-element-items
    (</> element-items
         (<map> (lambda (indexes)
                  (make-immutable-vector default-function-reference-type 'indexes
                                         (list->immutable-vector indexes)))
                (<many> <wat-index-reference>))))

  (define function-field
    (<bind>
     (<~> (wat-field-head "func")
          (<optional> <wat-identifier>)
          (<many> inline-export)
          (<optional> inline-import)
          <wat-type-use>
          (<many> local-clause))
     (lambda (value)
       (<bind> generic-body
               (lambda (body)
                 (let ([pos (car value)]
                       [id (optional-value (cadr value))]
                       [export* (caddr value)]
                       [import (optional-value (cadddr value))]
                       [type-use (car (cddddr value))]
                       [local** (cadr (cddddr value))])
                   (if (and import (or (pair? local**) (pair? body)))
                       (<fail-with> "an imported function cannot have locals or a body")
                       (<result>
                        (field-result
                         pos 'function id
                         (make-immutable-vector
                          type-use (list->immutable-vector (apply append local**))
                          (list->immutable-vector body))
                         import export* #f)))))))))

  (define optional-address-type
    (<optional> (</> (<as> 'i64 (<wat-keyword> "i64"))
                    (<as> 'i32 (<wat-keyword> "i32")))))

  (define table-expression-items
    (<map> (lambda (item*)
             (make-immutable-vector 'expressions
                                    (list->immutable-vector item*)))
           (<some> element-initializer)))

  (define table-indexed-items
    (<map> (lambda (item*)
             (make-immutable-vector 'indexes (list->immutable-vector item*)))
           (<some> <wat-index-reference>)))

  (define table-element-items
    (</> table-expression-items table-indexed-items
         (<result> (make-immutable-vector 'indexes '#()))))

  (define table-abbreviation
    (<map> (lambda (value)
             (let* ([table-type (caddr value)] [items (cadddr value)]
                    ;; The normative list declares its own type; legacy bare lists
                    ;; inherit the table type, as they did before.
                    [typed? (= 3 (vector-length items))]
                    [element-type (if typed? (vector-ref items 0) table-type)]
                    [kind (vector-ref items (if typed? 1 0))]
                    [initializers (vector-ref items (if typed? 2 1))])
               (make-wat-inline-abbreviation
                (car value) 'table-element
                (make-immutable-vector
                 table-type
                 (make-wat-element-segment-syntax
                  (car value) 'abbreviation #f #f (optional-value (cadr value))
                  element-type kind initializers)))))
           (<~> <pos> optional-address-type <wat-reference-type>
                (wat-parenthesized "elem" (</> element-items table-element-items)))))

  (define table-field
    (<bind>
     (<~> (wat-field-head "table")
          (<optional> <wat-identifier>)
          (<many> inline-export)
          (<optional> inline-import))
     (lambda (value)
       (let ([pos (car value)]
             [id (optional-value (cadr value))]
             [export* (caddr value)]
             [import (optional-value (cadddr value))])
         (<bind> (if import <wat-table-type> (</> table-abbreviation <wat-table-type>))
                 (lambda (type-or-abbreviation)
                   (<map>
                    (lambda (initializer)
                      (if (wat-inline-abbreviation? type-or-abbreviation)
                          (field-result pos 'table id #f #f export* type-or-abbreviation)
                          (field-result
                           pos 'table id
                           (if (zero? (vector-length initializer)) type-or-abbreviation
                               (vector type-or-abbreviation initializer))
                           import export* #f)))
                    (<~0> (if (or import (wat-inline-abbreviation? type-or-abbreviation))
                              (<result> '#())
                              <wat-expression>)
                          wat-close))))))))

  (define memory-abbreviation
    (<map> (lambda (value)
             (make-wat-inline-abbreviation
              (car value) 'memory-data
              (make-wat-data-segment-syntax
               (car value) 'abbreviation #f #f (optional-value (cadr value))
               (list->immutable-vector (caddr value)))))
           (<~> <pos> optional-address-type
                (wat-parenthesized "data" (<many> <wat-string>)))))

  (define memory-field
    (<bind>
     (<~> (wat-field-head "memory")
          (<optional> <wat-identifier>)
          (<many> inline-export)
          (<optional> inline-import))
     (lambda (value)
       (let ([pos (car value)]
             [id (optional-value (cadr value))]
             [export* (caddr value)]
             [import (optional-value (cadddr value))])
         (<bind> (if import <wat-memory-type>
                     (</> memory-abbreviation <wat-memory-type>))
                 (lambda (type-or-abbreviation)
                   (<map>
                    (lambda (unused)
                      (if (wat-inline-abbreviation? type-or-abbreviation)
                          (field-result pos 'memory id #f #f export*
                                        type-or-abbreviation)
                          (field-result pos 'memory id type-or-abbreviation
                                        import export* #f)))
                    wat-close)))))))

  (define global-field
    (<bind>
     (<~> (wat-field-head "global")
          (<optional> <wat-identifier>)
          (<many> inline-export)
          (<optional> inline-import)
          <wat-global-type>)
     (lambda (value)
       (<bind> generic-body
               (lambda (body)
                 (let ([pos (car value)]
                       [id (optional-value (cadr value))]
                       [export* (caddr value)]
                       [import (optional-value (cadddr value))]
                       [type (car (cddddr value))])
                   (cond [(and import (pair? body))
                          (<fail-with> "an imported global cannot have an initializer")]
                         [(and (not import) (null? body))
                          (<fail-with> "a defined global requires an initializer")]
                         [else
                          (<result>
                           (field-result
                            pos 'global id
                            (make-immutable-vector
                             type (list->immutable-vector body))
                            import export* #f))])))))))

  (define tag-field
    (<map>
     (lambda (value)
       (field-result (car value) 'tag (optional-value (cadr value))
                     (car (cddddr value)) (optional-value (cadddr value))
                     (caddr value) #f))
     (<~0> (<~> (wat-field-head "tag")
                (<optional> <wat-identifier>)
                (<many> inline-export)
                (<optional> inline-import)
                <wat-tag-type>)
            wat-close)))

;;;;===----------------------------------------------------------------------===
;;;; Export, start, element, data, and custom fields
;;;;===----------------------------------------------------------------------===

  (define export-description
    (apply </>
           (map (lambda (kind)
                  (<map> (lambda (index) (cons (string->symbol kind) index))
                         (wat-parenthesized kind <wat-index-reference>)))
                '("func" "table" "memory" "global" "tag"))))

  (define export-field
    (<map> (lambda (value)
             (field-result (car value) 'export #f
                           (make-immutable-vector (cadr value) (caddr value))
                           #f '() #f))
           (<~0> (<~> (wat-field-head "export") <wat-name> export-description)
                 wat-close)))

  (define start-field
    (<map> (lambda (value)
             (field-result (car value) 'start #f (cadr value) #f '() #f))
           (<~0> (<~> (wat-field-head "start") <wat-index-reference>)
                 wat-close)))

  (define declarative-element-prefix
    (<as> (make-immutable-vector 'declarative #f #f)
          (<wat-keyword> "declare")))

  (define active-element-prefix
    (<map> (lambda (value)
             (make-immutable-vector
              'active (optional-value (car value)) (cadr value)))
           (<~> (<optional> table-use) segment-offset)))

  (define passive-element-prefix
    (<result> (make-immutable-vector 'passive #f #f)))

  (define element-prefix
    (</> declarative-element-prefix active-element-prefix passive-element-prefix))

  (define element-field
    (<bind>
     (<~> (wat-field-head "elem") (<optional> <wat-identifier>) element-prefix)
     (lambda (value)
       (let ([pos (car value)] [id (optional-value (cadr value))] [prefix (caddr value)])
         (<map>
          (lambda (items)
            (field-result
             pos 'element id #f #f '()
             (make-wat-inline-abbreviation
              pos 'element
              (make-wat-element-segment-syntax
               pos (vector-ref prefix 0) (vector-ref prefix 1)
               (vector-ref prefix 2) #f (vector-ref items 0)
               (vector-ref items 1) (vector-ref items 2)))))
          (<~0> (if (and (eq? 'active (vector-ref prefix 0))
                         (not (vector-ref prefix 1)))
                    legacy-element-items element-items)
                 wat-close))))))

  (define active-data-prefix
    (<map> (lambda (value)
             (make-immutable-vector
              'active (optional-value (car value)) (cadr value)))
           (<~> (<optional> memory-use) segment-offset)))

  (define passive-data-prefix
    (<result> (make-immutable-vector 'passive #f #f)))

  (define data-prefix (</> active-data-prefix passive-data-prefix))

  (define data-field
    (<map>
     (lambda (value)
       (let ([pos (car value)]
             [id (optional-value (cadr value))]
             [prefix (caddr value)]
             [string* (cadddr value)])
         (field-result
          pos 'data id #f #f '()
          (make-wat-inline-abbreviation
           pos 'data
           (make-wat-data-segment-syntax
            pos (vector-ref prefix 0) (vector-ref prefix 1)
            (vector-ref prefix 2) #f
            (list->immutable-vector string*))))))
     (<~0> (<~> (wat-field-head "data")
                (<optional> <wat-identifier>) data-prefix (<many> <wat-string>))
            wat-close)))

  (define custom-section-name
    (apply
     </>
     (map (lambda (entry)
            (<as> (cdr entry) (<wat-keyword> (car entry))))
          '(("type" . type) ("import" . import) ("func" . function)
            ("table" . table) ("memory" . memory) ("tag" . tag)
            ("global" . global) ("export" . export) ("start" . start)
            ("elem" . element) ("datacount" . data-count) ("code" . code) ("data" . data)
            ("first" . type) ("last" . data)))))

  (define custom-anchor
    (</> (<map> (lambda (section) (cons 'after section))
                (wat-parenthesized "after" custom-section-name))
         (<map> (lambda (section) (cons 'before section))
                (wat-parenthesized "before" custom-section-name))))

  (define custom-field
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [anchor (optional-value (caddr value))]
              [before (and anchor (eq? 'before (car anchor)) (cdr anchor))]
              [after (if anchor (and (eq? 'after (car anchor)) (cdr anchor)) 'data)])
         (field-result
          pos 'custom #f
          (make-immutable-vector
           (cadr value) (list->immutable-vector (cadddr value))
           (make-wat-custom-placement pos before after))
          #f '() #f)))
     (<~0> (<~> (<~0> <pos> (<wat-annotation-head> "custom"))
                <wat-name> (<optional> custom-anchor) (<many> <wat-string>))
            wat-close)))

  (define module-field
    (</> custom-field recursive-type-field type-field import-field function-field
         table-field memory-field global-field tag-field export-field start-field
         element-field data-field))

  #|proc:parser-wat-module-syntax
  The `parser-wat-module-syntax` parser reads exactly one Core 3.0 text module and returns an
  internal positioned module syntax record. Source files may omit the module wrapper.
  Trailing WebAssembly script commands are rejected.
  |#
  (define parser-wat-module-syntax
    (<map>
     (lambda (value)
       (make-wat-module (car value) (optional-value (cadr value))
                        (list->immutable-vector (caddr value))))
     (<~1> <wat-trivia>
            (</> (<~0> (<~> (wat-field-head "module")
                             (<optional> <wat-identifier>)
                             (<many-until> module-field wat-close))
                        wat-close <eof>)
                 (<~0> (<~> <pos> (<result> #f) (<many> module-field)) <eof>)))))

  #|proc:parser-wat-module
  The `parser-wat-module` parser reads exactly one Core 3.0 text module and returns its canonical
  module representation. Normalization failures retain their original source position.
  |#
  (define parser-wat-module
    (<bind>
     parser-wat-module-syntax
     (lambda (syntax)
       (let ([result (normalize-wat-module syntax)])
         (if (wasm-issue? result)
             (<pos-at> (wasm-issue-offset result)
                       (<fail-with> (wasm-issue-message result)))
             (<result> result))))))

  )
