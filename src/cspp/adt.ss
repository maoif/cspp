(library (cspp adt)
  (export datatype record)
  (import (cspp chez)
          (cspp string)
          (cspp list)
          (cspp internal))


  #|macro:datatype
  Defines a sum type `dt` with variant constructors, variant predicates, and field accessors.
  `dt` names the parent type and `dt?` names its predicate; omitting `dt?` defaults to `dt?`.
  Each `[Variant field ...]` defines a variant; a field may be a bare name or an option list.
  An option list uses `[name :predicate pred :mutable]`; options may appear in either order.
  `pred` is a one-argument predicate checked during construction and before mutable assignment.
  Fields default to immutable; `:mutable` and `:immutable` are mutually exclusive.
  Expansion defines the type predicate, constructors, variant predicates, accessors, and setters.
  Match a variant with `,(datatype Type Variant :fields (field pattern) ...)` for a named subset,
  or `,(datatype Type Variant :values pattern ...)` for every field in declaration order.
  `:fields` allows an empty subset; named fields must be unique. `:values` requires exactly
  the declared number of fields. A zero-field variant also accepts `,(datatype Type Variant)`.
  Example: after `(datatype Tree [Empty] [Leaf value])`, matching `(Leaf 42)` with
  `,(datatype Tree Leaf :fields (value ,x))` binds `x` to `42`.
  |#
  ;; Syntax:
  ;; (datatype type predicate? [variant field ...] ...)
  ;; (datatype type [variant field ...] ...)
  ;; field := name | (name field-option ...)
  ;; field-option := :predicate predicate | :mutable | :immutable
  (define-syntax datatype
    (lambda (stx)
      (define construct-datatype-name
        (lambda (template-identifier . args)
          (apply $construct-name (append (list template-identifier "$datatype-") args))))
      (define get-vname
        (lambda (v) (if (symbol? v) v (car v))))
      (define check-datatype-name
        (lambda (n)
          ;; TODO
          (if (identifier? #'n)
              #t
              (syntax-error n "bad datatype name:"))))
      (define check-pred-name
        (lambda (n)
          (let ([p (syntax->datum n)])
            (if (symbol? p)
                (let ([p (symbol->string p)])
                  (if (string-endswith? p "?")
                      #t
                      (syntax-error p "bad datatype predicate name (should end with \"?\"): ")))
                (syntax-error p "bad datatype predicate name (not a symbol): ")))))
      (define check-variant-name
        (lambda (v)
          ;; TODO
          (let ([v (syntax->datum v)])
            (or (symbol? v) (symbol? (car v))))))
      (define check-all-names
        (lambda (dt dt? variants)
          (let* ([dt (syntax->datum dt)]
                 [dt? (syntax->datum dt?)]
                 [variants (syntax->datum variants)]
                 [names `(,dt ,dt? ,@(map get-vname variants))])
            (unless (unique? names)
              (syntax-error names "names are not unique in datatype definition:")))))
      (define gen-dtuid
        (lambda (dt variants)
          (let* ([dtname (symbol->string (syntax->datum dt))]
                 [field->string
                  (lambda (field)
                    (string-append
                     (symbol->string (syntax->datum (field-identifier field)))
                     (if (eq? (car (field-info field)) 'mutable) "mutable" "")))]
                 [variant->string
                  (lambda (variant)
                    (syntax-case variant ()
                      [(vname field ...)
                       (apply string-append
                              (symbol->string (syntax->datum #'vname))
                              (map field->string #'(field ...)))]))]
                 [bigname (apply string-append dtname
                                 (map variant->string (syntax->list variants)))]
                 [h (string-hash bigname)])
            (datum->syntax dt (string->symbol (string-append bigname "-" (number->string h)))))))
      (define gen-vuids
        (lambda (dt dtuid variants)
          (let* ([variants (syntax->datum variants)]
                 [dtuidstr (symbol->string (syntax->datum dtuid))])
            (datum->syntax
             dt
             (map (lambda (v)
                    (string->symbol
                     (string-append dtuidstr "-" (symbol->string (get-vname v)))))
                  variants)))))
      (define gen-vnames
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname))
           (map get-vname (syntax->datum variants)))))
      (define gen-vnames?
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname "?"))
           (map get-vname (syntax->datum variants)))))
      (define keyword-token?
        (lambda (form)
          (and (identifier? form)
               (let ([name (symbol->string (syntax->datum form))])
                 (and (> (string-length name) 0)
                      (char=? (string-ref name 0) #\:))))))
      (define parse-field-options
        (lambda (field fid options)
          (let loop ([options (syntax->list options)]
                     [predicate #f]
                     [predicate-seen? #f]
                     [mutability 'default])
            (if (null? options)
                (list (if (eq? mutability 'default) 'immutable mutability)
                      predicate)
                (let ([option (car options)])
                  (case (syntax->datum option)
                    [(:predicate)
                     (when predicate-seen?
                       (syntax-error option "duplicate :predicate option:"))
                     (when (null? (cdr options))
                       (syntax-error field ":predicate requires a predicate identifier:"))
                     (let ([pred (cadr options)])
                       (unless (and (identifier? pred) (not (keyword-token? pred)))
                         (syntax-error pred ":predicate requires an identifier:"))
                       (loop (cddr options) pred #t mutability))]
                    [(:mutable :immutable)
                     (when (not (eq? mutability 'default))
                       (syntax-error option "duplicate or conflicting mutability option:"))
                     (loop (cdr options) predicate predicate-seen?
                           (if (eq? (syntax->datum option) ':mutable)
                               'mutable
                               'immutable))]
                    [else (syntax-error option "unknown datatype field option:")]))))))
      (define field-info
        (lambda (field)
          (syntax-case field (:predicate :mutable :immutable)
            [fid
             (identifier? #'fid)
             '(immutable #f)]
            [(fid option ...)
             (and (identifier? #'fid)
                  (not (null? (syntax->list #'(option ...)))))
             (parse-field-options field #'fid #'(option ...))]
            [_ (syntax-error field "invalid datatype field definition:")])))
      (define field-identifier
        (lambda (field)
          (if (identifier? field)
              field
              (car (syntax->list field)))))
      (define handle-vfields
        (lambda (dt variants)
          (map
           (lambda (variant)
             (syntax-case variant ()
               ;; TODO check field number
               [(vname vfields ...)
                (let f ([vfields #'(vfields ...)] [field* '()])
                  (if (null? vfields)
                      (reverse field*)
                      (let* ([field (car vfields)]
                             [info (field-info field)]
                             [fid (field-identifier field)]
                             [field-def
                              (case (car info)
                                [(mutable)
                                 #`(mutable #,fid
                                            #,($construct-name dt dt "-" #'vname "-" fid)
                                            #,($construct-name dt dt "-" #'vname "-" fid
                                                               "-set!-raw"))]
                                [else
                                 #`(immutable #,fid
                                              #,($construct-name dt dt "-" #'vname "-" fid))])])
                        (f (cdr vfields) (cons field-def field*)))))]))
           variants)))
      (define gen-protocols
        (lambda (variants)
          (map
           (lambda (variant)
             (syntax-case variant ()
               [(vname vfields ...)
                (with-syntax ([(pcon vcon args ...) (generate-temporaries #'(p v vfields ...))])
                  #`(lambda (pcon)
                      (lambda (args ...)
                        (let ([vcon (pcon)])
                          #,(let f ([vfields #'(vfields ...)] [a* #'(args ...)])
                              (if (null? vfields)
                                  #'(vcon args ...)
                                  (let* ([field (car vfields)]
                                         [arg (car a*)]
                                         [predicate (cadr (field-info field))]
                                         [fid (field-identifier field)]
                                         [next (f (cdr vfields) (cdr a*))])
                                    (if predicate
                                        #`(if (#,predicate #,arg)
                                              #,next
                                              (errorf 'vname
                                                      "wrong argument type for field ~a: ~a"
                                                      '#,fid #,arg))
                                        next))))))))]))
           variants)))
      ;; make sure setters also have type checking, if given
      (define gen-setter-wrappers
        (lambda (dt variants* vfields*)
          (let ([wrappers*
                 (map (lambda (variant vfields)
                        (syntax-case variant ()
                          [(vname vflds ...)
                           (fold-left (lambda (wrappers vfld vfield)
                                       (syntax-case vfield (mutable)
                                          [(mutable fid getter raw-setter)
                                           (let* ([info (field-info vfld)]
                                                  [predicate (cadr info)]
                                                  [field-name (field-identifier vfld)]
                                                  [setter ($construct-name dt dt "-" #'vname "-"
                                                                           field-name "-set!")]
                                                  [body (if predicate
                                                            #`(if (#,predicate v)
                                                                  (raw-setter r v)
                                                                  (errorf
                                                                   '#,setter
                                                                   "wrong field ~a value: ~a"
                                                                   '#,field-name v))
                                                            #'(raw-setter r v))])
                                             (cons #`(define #,setter
                                                       (lambda (r v) #,body))
                                                   wrappers))]
                                          [_ wrappers]))
                                      '() #'(vflds ...) vfields)]))
                      variants* vfields*)])
            ;; flatten the list
            (apply append wrappers*))))
      (define get-getters
        (lambda (vfields*)
          (fold-left (lambda (res fields)
                       (append (map (lambda (fld)
                                      (syntax-case fld (mutable immutable)
                                        [(immutable _ getter) #'getter]
                                        [(mutable _ getter _) #'getter]))
                                    fields)
                               res))
                     '() vfields*)))
      (define get-setters
        (lambda (dt variants)
          (apply append
                 (map (lambda (variant)
                        (syntax-case variant ()
                          [(vname vflds ...)
                           (fold-left (lambda (res vfld)
                                        (if (eq? (car (field-info vfld)) 'mutable)
                                            (cons ($construct-name dt dt "-" #'vname "-"
                                                                   (field-identifier vfld) "-set!")
                                                  res)
                                            res))
                                      '() #'(vflds ...))]))
                      variants))))
      (define classify-variants
        (lambda (variants)
          (let loop ([variants variants] [singletons '()] [others '()])
            (if (null? variants)
                (list (reverse singletons) (reverse others))
                (let ([variant (car variants)])
                  (syntax-case variant ()
                    [(vname)
                     (loop (cdr variants) (cons #'vname singletons) others)]
                    [(vname vfields ...)
                     (loop (cdr variants) singletons (cons variant others))]))))))
      (syntax-case stx ()
        [(_ dt dt? variant0 variants ...)
         (and (identifier? #'dt)
              (identifier? #'dt?)
              (check-datatype-name #'dt)
              (check-pred-name #'dt?)
              (andmap check-variant-name #'(variant0 variants ...))
              (check-all-names #'dt #'dt? #'(variant0 variants ...)))
         ;; TODO check uniqueness of names
         ;; names starting with $datatype are for internal use
         (with-syntax
             ([dtuid (gen-dtuid #'dt #'(variant0 variants ...))]
              ;; $datatype-mk-dt
              [mkdt (construct-datatype-name #'dt "mk-" #'dt)]
              [((singletons ...) (mvariants ...)) (classify-variants #'(variant0 variants ...))])
           (with-syntax ([(suids ...) (gen-vuids #'dt #'dtuid #'(singletons ...))]
                         [(muids ...) (gen-vuids #'dt #'dtuid #'(mvariants ...))]
                         [(mksingletons ...) (generate-temporaries #'(singletons ...))]
                         [((mkvariants . _) ...) #'(mvariants ...)]
                         ;; $datatype-dt-variant
                         [(snames ...) (gen-vnames #'dt #'(singletons ...))]
                         [(mnames ...) (gen-vnames #'dt #'(mvariants ...))]
                         ;; $datatype-dt-variant?
                         [(singletons? ...) (gen-vnames? #'dt #'(singletons ...))]
                         [(mvariants? ...)  (gen-vnames? #'dt #'(mvariants ...))]
                         ;; ((mutability name getter ?setter) ...) ...
                         ;; getter: dt-variant-field
                         ;; setter: dt-variant-field-set!
                         [((vfields ...) ...) (handle-vfields #'dt #'(mvariants ...))]
                         [(protocols ...) (gen-protocols #'(mvariants ...))])
             (with-syntax ([(setter-wrappers ...)
                            (gen-setter-wrappers
                             #'dt #'(mvariants ...) #'((vfields ...) ...))]
                           [(getters ...) (get-getters #'((vfields ...) ...))]
                           [(setters ...) (get-setters #'dt #'(mvariants ...))])
               #`(module (dt dt? mnames ... snames ...
                             mkvariants ... mvariants? ... singletons ... singletons? ...
                             getters ... setters ...)
                   (define-record-type (dt mkdt dt?)
                     (nongenerative dtuid))
                   (define-record-type (mnames mkvariants mvariants?)
                     (nongenerative muids)
                     (parent dt)
                     (sealed #t)
                     (fields vfields ...)
                     ;; need protocol to do type checking
                     (protocol protocols))
                   ...
                   (define-record-type (snames mksingletons singletons?)
                     (nongenerative suids)
                     (parent dt)
                     (sealed #t)
                     (fields))
                   ...
                   (define singletons (mksingletons))
                   ...
                   (begin setter-wrappers ...)))))]
        [(_ dt variant0 variants ...)
         (check-datatype-name #'dt)
         (with-syntax ([dt? ($construct-name #'dt #'dt "?")])
           #'(datatype dt dt? variant0 variants ...))]
        [_ (syntax-error stx "bad datatype definition:")])))

  ;; TODO parent?
  ;; check if record def has a parent of datatype (just mask datatype names?)
  ;; or check if the name starts with "$datatype"
  #|macro:record
  Defines a record type `dt` with constructor `dt`, a type predicate, and field accessors.
  `(record dt (field ...))` uses `dt?` as its predicate; `(record dt pred (field ...))` uses `pred`.
  Fields may be bare names or option lists such as `[name :predicate string? :mutable]`.
  Options may appear in either order; a field defaults to immutable when neither mutability option
  is given. `:mutable` and `:immutable` are mutually exclusive options.
  `pred` is a one-argument predicate checked during construction and before mutable assignment.
  Expansion defines the constructor, type predicate, accessors, and setters for mutable fields.
  Match a record with `,(record Type :fields (field pattern) ...)` for a named subset or
  `,(record Type :values pattern ...)` for every field in declaration order. Named fields must
  be unique, and `:values` requires exactly the declared number of fields. The explicit wrapper
  is required; constructor names are not match extension heads. For example, after
  `(record Point (x y))`, matching `(Point 10 20)` with `,(record Point :fields (y ,v))`
  binds `v` to `20`.
  |#
  ;; Syntax:
  ;; (record type predicate? (field ...))
  ;; (record type (field ...))
  ;; field := name | (name field-option ...)
  ;; field-option := :predicate predicate | :mutable | :immutable
  (define-syntax record
    (lambda (stx)
      (define keyword-token?
        (lambda (form)
          (and (identifier? form)
               (let ([name (symbol->string (syntax->datum form))])
                 (and (> (string-length name) 0)
                      (char=? (string-ref name 0) #\:))))))
      (define parse-field-options
        (lambda (field fid options)
          (let loop ([options (syntax->list options)]
                     [predicate #f]
                     [predicate-seen? #f]
                     [mutability 'default])
            (if (null? options)
                (list (if (eq? mutability 'default) 'immutable mutability)
                      predicate)
                (let ([option (car options)])
                  (case (syntax->datum option)
                    [(:predicate)
                     (when predicate-seen?
                       (syntax-error option "duplicate :predicate option:"))
                     (when (null? (cdr options))
                       (syntax-error field ":predicate requires a predicate identifier:"))
                     (let ([pred (cadr options)])
                       (unless (and (identifier? pred) (not (keyword-token? pred)))
                         (syntax-error pred ":predicate requires an identifier:"))
                       (loop (cddr options) pred #t mutability))]
                    [(:mutable :immutable)
                     (when (not (eq? mutability 'default))
                       (syntax-error option "duplicate or conflicting mutability option:"))
                     (loop (cdr options) predicate predicate-seen?
                           (if (eq? (syntax->datum option) ':mutable)
                               'mutable
                               'immutable))]
                    [else (syntax-error option "unknown record field option:")]))))))
      (define field-info
        (lambda (field)
          (syntax-case field (:predicate :mutable :immutable)
            [fid
             (identifier? #'fid)
             '(immutable #f)]
            [(fid option ...)
             (and (identifier? #'fid)
                  (not (null? (syntax->list #'(option ...)))))
             (parse-field-options field #'fid #'(option ...))]
            [_ (syntax-error field "invalid record field definition:")]
            )))
      (define field-identifier
        (lambda (field)
          (if (identifier? field)
              field
              (car (syntax->list field)))))
      (define gen-uid
        (lambda (dt field*)
          (let* ([t dt]
                 [dt (syntax->datum dt)]
                 [field* (syntax->list field*)]
                 [field->string
                  (lambda (field)
                    (string-append
                     (symbol->string (syntax->datum (field-identifier field)))
                     (if (eq? (car (field-info field)) 'mutable) "mutable" "")))]
                 [bigname (string-append (symbol->string dt)
                                         (apply string-append
                                                (map field->string field*)))]
                 [h (string-hash bigname)])
            (datum->syntax t
                           (string->symbol (string-append bigname "-" (number->string h)))))))
      (define handle-fields
        (lambda (dt fields)
          (map
           (lambda (field)
             (let* ([info (field-info field)]
                    [fid (field-identifier field)])
               (case (car info)
                 [(mutable)
                  #`(mutable #,fid
                             #,($construct-name dt dt "-" fid)
                             #,($construct-name dt dt "-" fid "-set!-raw"))]
                 [else
                  #`(immutable #,fid
                               #,($construct-name dt dt "-" fid))])))
           fields)))
      (define gen-protocol
        (lambda (dt fields)
          (with-syntax ([(vcon args ...) (generate-temporaries #`(v #,@fields))])
            #`(lambda (vcon)
                (lambda (args ...)
                  #,(let f ([fields fields] [a* #'(args ...)])
                      (if (null? fields)
                          #'(vcon args ...)
                          (let* ([field (car fields)]
                                 [arg (car a*)]
                                 [predicate (cadr (field-info field))]
                                 [fid (field-identifier field)]
                                 [next (f (cdr fields) (cdr a*))])
                            (if predicate
                                #`(if (#,predicate #,arg)
                                      #,next
                                      (errorf '#,dt "wrong argument type for field ~a: ~a"
                                              '#,fid #,arg))
                                next)))))))))
      ;; make sure setters also have type checking, if given
      (define gen-setter-wrappers
        (lambda (dt fields flds)
          (let loop ([fields fields] [flds flds] [wrappers '()])
            (if (null? fields)
                wrappers
                (let ([w
                       (syntax-case (car fields) (mutable)
                         [(mutable fid getter raw-setter)
                          (let* ([info (field-info (car flds))]
                                 [predicate (cadr info)])
                            (let ([setter ($construct-name dt dt "-" #'fid "-set!")]
                                  [field-name (field-identifier (car flds))])
                              #`(define #,setter
                                  (lambda (r v)
                                    #,(if predicate
                                          #`(if (#,predicate v)
                                                (raw-setter r v)
                                                (errorf '#,setter
                                                        "wrong argument type for field ~a: ~a"
                                                        '#,field-name v))
                                          #'(raw-setter r v))))))]
                         [_ #f])])
                  (if w
                      (loop (cdr fields) (cdr flds) (cons w wrappers))
                      (loop (cdr fields) (cdr flds) wrappers)))))))
      (define get-getters
        (lambda (flds)
          (fold-left (lambda (res fld)
                       (syntax-case fld (mutable immutable)
                         [(immutable _ getter) (cons #'getter res)]
                         [(mutable _ getter _) (cons #'getter res)]))
                     '() flds)))
      (define get-setters
        (lambda (dt flds)
          (fold-left (lambda (res fld)
                       (syntax-case fld (mutable immutable)
                         [(mutable fid _ _) (cons ($construct-name dt dt "-" #'fid "-set!") res)]
                         [_ res]))
                     '() flds)))
      (syntax-case stx ()
        [(k dt pred (fld fld* ...))
         (andmap identifier? #'(dt pred))
         (with-syntax ([mkdt #'dt]
                       [dtname     ($construct-name #'dt "$record-" #'dt)]
                       [uid        (gen-uid #'dt #'(fld fld* ...))]
                       [(flds ...) (handle-fields #'dt #'(fld fld* ...))]
                       [proto      (gen-protocol #'dt #'(fld fld* ...))])
           (with-syntax ([(setter-wrappers ...)
                          (gen-setter-wrappers #'dt #'(flds ...) #'(fld fld* ...))]
                         [(getters ...) (get-getters #'(flds ...))]
                         [(setters ...) (get-setters #'dt #'(flds ...))])
             #`(module (dtname mkdt pred getters ... setters ...)
                 (define-record-type (dtname mkdt pred)
                   (nongenerative uid)
                   (fields flds ...)
                   (protocol proto))
                 (begin setter-wrappers ...))))]
        [(k dt (fld fld* ...))
         (identifier? #'dt)
         (with-syntax ([dt? ($construct-name #'dt #'dt "?")])
           #'(record dt dt? (fld fld* ...)))]
        [_ (syntax-error stx "bad record definition:")])))

  )
