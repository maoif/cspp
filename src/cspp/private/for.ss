(library (cspp private for)
  (export literal? ->literal $for/check-quoted-literal
          valid-iter-ty? kw-iter-clause?
          kw-for-config-clause? kw-for*-config-clause?
          kw:init?
          kw:index?
          kw:finish?
          kw:iter?
          kw:break?
          kw:stop?
          kw:guard?
          kw:bind?
          kw:let?
          kw:let-values?
          kw:length?
          kw:fill?

          make-clause-handler add-clause! true
          handle-init handle-index handle-finish handle-iter handle-config
          handle-length handle-fill
          handle-iter/config-iter handle-iter/config-config compile-binding-clause

          process-iter-clause process-iter-clause/direct process-iter-clauses compile-termination
          fragment-evaluations fragment-setup fragment-bindings fragment-termination fragment-updates
          fragment-getter fragment-source-index fragment-exposed fragment-raw-termination
          fragment-filter fragment-visit)
  (import (cspp chez)
          (cspp utils)
          (cspp internal)
          (cspp io)
          (cspp list)
          (cspp iter))


  #|proc:->literal
  The `->literal` helper returns the datum represented by `literal-syntax`, removing one
  quotation only when its head has the Scheme `quote` binding. For example, `#''3` returns `3`;
  a locally bound procedure named `quote` leaves `(quote 3)` intact as application data.
  |#
  (define (->literal literal-syntax)
    (syntax-case literal-syntax (quote)
      [(quote value) (syntax->datum #'value)]
      [_ (syntax->datum literal-syntax)]))

  #|proc:literal?
  The `literal?` helper reports whether `literal-syntax` denotes supported source shorthand:
  an exact natural number, string, or vector, optionally quoted with the Scheme `quote` binding.
  For example, `#''#(a b)` returns `#t`; an application to a rebound `quote` returns `#f`.
  |#
  (define (literal? literal-syntax)
    (let ([x (->literal literal-syntax)])
      (or (and (natural? x) (exact? x))
          (string? x)
          (vector? x))))

  #|macro:$for/check-quoted-literal
  The `$for/check-quoted-literal` helper verifies quotation in the generated source scope.
  Earlier nested iterator or configuration bindings may hide `quote` after initial parsing.
  For example, `(quote 3)` is rejected below a nested item binding named `quote`.
  A valid quotation expands to an unspecified value without runtime validation.
  |#
  (define-syntax $for/check-quoted-literal
    (lambda (stx)
      (syntax-case stx ()
        [(_ expression)
         (syntax-case #'expression (quote)
           [(quote value) #'(void)]
           [_ (syntax-error #'expression "for: expected quoted literal source:")])])))

  (define (valid-iter-ty? ty)
    (let ([ty (syntax->datum ty)])
      (or (eq? ty ':iota)
          (eq? ty ':fxiota)
          (eq? ty ':nums)
          (eq? ty ':fixnums)
          (eq? ty ':string)
          (eq? ty ':list)
          (eq? ty ':vector)
          (eq? ty ':fxvector)
          (eq? ty ':flvector)
          (eq? ty ':bytevector)
          (eq? ty ':hashtable)
          (eq? ty ':hashtable-keys)
          (eq? ty ':hashtable-values)
          (eq? ty ':iter))))

  (define kw-iter-clause? valid-iter-ty?)

  (define (kw-for-config-clause? kw)
    (let ([kw (syntax->datum kw)])
      (or (eq? kw ':break)
          (eq? kw ':guard)
          (eq? kw ':let)
          (eq? kw ':let*)
          (eq? kw ':letrec)
          (eq? kw ':letrec*)
          (eq? kw ':let-values)
          (eq? kw ':let*-values))))
  (define (kw-for*-config-clause? kw)
    (let ([kw (syntax->datum kw)])
      (or (eq? kw ':break)
          (eq? kw ':guard)
          (eq? kw ':index)
          (eq? kw ':stop)
          (eq? kw ':let)
          (eq? kw ':let*)
          (eq? kw ':letrec)
          (eq? kw ':letrec*)
          (eq? kw ':let-values)
          (eq? kw ':let*-values))))

  (define (gen-kw? kw)
    (lambda (keyword) (eq? kw (syntax->datum keyword))))
  (define kw:init?       (gen-kw? ':init))
  (define kw:index?      (gen-kw? ':index))
  (define kw:finish?     (gen-kw? ':finish))
  (define kw:iter?       (gen-kw? ':iter))
  (define kw:break?      (gen-kw? ':break))
  (define kw:stop?       (gen-kw? ':stop))
  (define kw:guard?      (gen-kw? ':guard))
  (define kw:bind?       (gen-kw? ':bind))
  (define kw:let?        (gen-kw? ':let))
  (define kw:let-values? (gen-kw? ':let-values))
  (define kw:length?     (gen-kw? ':length))
  (define kw:fill?       (gen-kw? ':fill))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   clause handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-clause-handler . table)
    (assert (eq? 'start (caar table)))
    (lambda (s clauses)
      (let loop ([state (caar table)] [cl* clauses])
        (if (eq? 'end state)
            (if (null? cl*)
                (begin
                  (for-each
                   (lambda (name)
                     (when (hashtable-contains? s name)
                       (hashtable-set! s name (reverse (hashtable-ref s name '())))))
                   '(iter config))
                  (when (hashtable-contains? s 'iter/config)
                    (hashtable-set! s 'iter/config
                      (map (lambda (group) (cons (car group) (reverse (cdr group))))
                           (reverse (hashtable-ref s 'iter/config '())))))
                  s)
                (syntax-violation 'for "unexpected trailing clauses" clauses (car cl*)))
            (let loop-trans ([trans (filter (lambda (row)
                                              (eq? (car row) state))
                                            table)])
              (if (null? trans)
                  (syntax-violation 'for "unexpected clause or missing iterator clause"
                                    clauses (and (pair? cl*) (car cl*)))
                  (let* ([row     (car trans)]
                         [to      (list-ref row 1)]
                         [handler (list-ref row 2)])
                    ;; when cl* is '(), only epsilon transition is possible
                    (if (null? cl*)
                        (if (eq? handler 'epsilon)
                            (loop to cl*)
                            (loop-trans (cdr trans)))
                        ;; TODO remove `next?`
                        (if (eq? handler 'epsilon)
                            (loop to cl*)
                            (let-values ([(handled? next?) (handler s (car cl*))])
                              (if handled?
                                  (if next?
                                      (loop to (cdr cl*))
                                      (loop to cl*))
                                  (loop-trans (cdr trans)))))))))))))


  (define true (lambda (state cl) (values #t #f)))

  (define (add-clause! state name cl)
    (let ([?cl (hashtable-ref state name #f)])
      (if ?cl
          (syntax-violation 'for "duplicate clause" cl)
          (hashtable-set! state name cl))))

  (define handle-init
    (lambda (state cl)
      (syntax-case cl ()
        [(:init v e)
         (kw:init? #':init)
         (begin ;;(displayln cl)
                (add-clause! state 'init cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-index
    (lambda (state cl)
      (syntax-case cl ()
        [(:index v)
         (kw:index? #':index)
         (begin ;;(displayln cl)
                (add-clause! state 'index cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-finish
    (lambda (state cl)
      (syntax-case cl ()
        [(:finish v)
         (kw:finish? #':finish)
         (begin ;;(displayln cl)
                (add-clause! state 'finish cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-length
    (lambda (state cl)
      (syntax-case cl ()
        [(:length v)
         (kw:length? #':length)
         (begin ;;(displayln cl)
                (add-clause! state 'length cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-fill
    (lambda (state cl)
      (syntax-case cl ()
        [(:fill v)
         (kw:fill? #':fill)
         (begin ;;(displayln cl)
                (add-clause! state 'fill cl)
                (values #t #t))]
        [_ (values #f #f)])))

  #|proc:iterator-variable?
  The `iterator-variable?` helper accepts an identifier except a reserved clause tag.
  `variable` is proposed iterator-binding syntax. For example, `x` returns `#t`, while
  `:length` returns `#f`, preventing an unsupported option from becoming a literal iterator.
  |#
  (define iterator-variable?
    (lambda (variable)
      (and (identifier? variable)
           (not (or (kw-for*-config-clause? variable)
                    (kw:init? variable) (kw:finish? variable)
                    (kw:length? variable) (kw:fill? variable))))))

  (define handle-iter
    (lambda (state cl)
      (define (add-clause! cl)
        (hashtable-set! state 'iter
                        (cons cl (hashtable-ref state 'iter '()))))
      (syntax-case cl ()
        [((v* ...) iter-ty op* ...)
         (and (andmap identifier? #'(v* ...))
              (valid-iter-ty? #'iter-ty))
         (begin (add-clause! cl)
                (values #t #t))]
        [(v iter-ty op* ...)
         (and (iterator-variable? #'v)
              (valid-iter-ty? #'iter-ty))
         (begin (add-clause! cl)
                (values #t #t))]
        [(v lit)
         (and (iterator-variable? #'v) (literal? #'lit))
         (begin (add-clause! cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-config
    (lambda (state cl)
      (define (add-clause! cl)
        (hashtable-set! state 'config
                        (cons cl (hashtable-ref state 'config '()))))
      (syntax-case cl ()
        [(:break e)
         (kw:break? #':break)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:break e1 e2)
         (kw:break? #':break)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:guard e :index v)
         (and (kw:guard? #':guard) (kw:index? #':index) (identifier? #'v))
         (begin (add-clause! cl)
                (values #t #t))]
        [(:guard e)
         (kw:guard? #':guard)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:let v e)
         (and (memq (datum :let) '(:let :let* :letrec :letrec*)) (identifier? #'v))
         (begin (add-clause! cl)
                (values #t #t))]
        [(:let-values (v* ...) e)
         (and (memq (datum :let-values) '(:let-values :let*-values))
              (andmap identifier? #'(v* ...)))
         (begin (add-clause! cl)
                (values #t #t))]
        [_ (values #f #f)])))

  #|proc:compile-binding-clause
  The `compile-binding-clause` helper compiles one `clause` using its Scheme binding form.
  `continue` receives the list of newly bound identifiers and returns syntax in their scope.
  For example, `[:let x 10]` wraps the continuation in `(let ([x 10]) ...)` and supplies `(x)`;
  `[:let-values (x y) (values 10 20)]` supplies `(x y)` within a `let-values` form.
  |#
  (define (compile-binding-clause clause continue)
    (pcheck ([procedure? continue])
      (syntax-case clause ()
        [(tag variable expression)
         (memq (datum tag) '(:let :let* :letrec :letrec*))
         #`(#,(case (datum tag)
                [(:let) #'let] [(:let*) #'let*] [(:letrec) #'letrec] [(:letrec*) #'letrec*])
             ([variable expression])
             #,(continue #'(variable)))]
        [(tag (variable ...) expression)
         (memq (datum tag) '(:let-values :let*-values))
         #`(let-values ([(variable ...) expression]) #,(continue #'(variable ...)))]
        [_ (syntax-error clause "for: unknown configuration clause:")])))

  (define handle-iter/config-iter
    (lambda (state cl)
      (define (add-clause! cl)
        (let ([res (hashtable-ref state 'iter/config '())])
          (hashtable-set! state 'iter/config
                          (cons (list cl) res))))
      (syntax-case cl ()
        [(:index . rest)
         (kw:index? #':index)
         ;; Reserve the local index tag even when its arguments resemble an iterator.
         (values #f #f)]
        [((v* ...) iter-ty op* ...)
         (and (andmap identifier? #'(v* ...))
              (valid-iter-ty? #'iter-ty))
         (begin (add-clause! cl)
                (values #t #t))]
        [(v iter-ty op* ...)
         (and (iterator-variable? #'v)
              (valid-iter-ty? #'iter-ty))
         (begin (add-clause! cl)
                (values #t #t))]
        [(v lit)
         (and (iterator-variable? #'v) (literal? #'lit))
         (begin (add-clause! cl)
                (values #t #t))]
        [_ (values #f #f)])))

  (define handle-iter/config-config
    (lambda (state cl)
      (define (add-clause! cl)
        (let ([res (hashtable-ref state 'iter/config '())])
          (if (null? res)
              (syntax-violation 'for "configuration clause requires a preceding iterator" cl)
              (set-cdr! (car res) (cons cl (cdar res))))))
      (syntax-case cl ()
        [(:index v)
         (and (kw:index? #':index) (identifier? #'v))
         ;; A local counter advances on every visit reaching this clause.
         (begin (add-clause! #'(:guard #t :index v))
                (values #t #t))]
        [(:break e)
         (kw:break? #':break)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:break e1 e2)
         (kw:break? #':break)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:stop e)
         (kw:stop? #':stop)
         (begin (add-clause! #'(:stop e (void)))
                (values #t #t))]
        [(:stop e1 e2)
         (kw:stop? #':stop)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:guard e :index v)
         (and (kw:guard? #':guard) (kw:index? #':index) (identifier? #'v))
         (begin (add-clause! cl)
                (values #t #t))]
        [(:guard e)
         (kw:guard? #':guard)
         (begin (add-clause! cl)
                (values #t #t))]
        [(:let v e)
         (and (memq (datum :let) '(:let :let* :letrec :letrec*)) (identifier? #'v))
         (begin (add-clause! cl)
                (values #t #t))]
        [(:let-values (v* ...) e)
         (and (memq (datum :let-values) '(:let-values :let*-values))
              (andmap identifier? #'(v* ...)))
         (begin (add-clause! cl)
                (values #t #t))]
        [_ (values #f #f)])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   clause processors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:check-ops
  The `check-ops` helper parses option syntax `ops` using `(tag . arity)` entries in
  `op-configs`, returning option/argument lists. It consumes each option's arguments before
  looking for the next tag, so `:to :end` treats `:end` as an expression, not an option.
  Duplicate tags, unknown tags, and missing arguments are syntax errors.
  |#
  (define (check-ops op-configs ops)
    (let loop ([remaining ops] [seen '()] [result '()])
      (if (null? remaining)
          (reverse result)
          (let* ([tag (syntax->datum (car remaining))]
                 [config (assq tag op-configs)])
            (unless config (syntax-error (car remaining) "for: unknown iterator option:"))
            (when (memq tag seen)
              (syntax-error (car remaining) "for: duplicate iterator option:"))
            (let ([arity (cdr config)])
              (when (< (length (cdr remaining)) arity)
                (syntax-error (car remaining) "for: missing iterator option argument:"))
              (loop (list-tail remaining (fx1+ arity)) (cons tag seen)
                    (cons (cons tag (list-head (cdr remaining) arity)) result)))))))

  (define *sequence-ops*
    ;; op     nop-args
    '((:from  . 1)
      (:to    . 1)
      (:step  . 1)
      (:rev   . 0)))
  (define *bytevector-ops*
    (append *sequence-ops*
            '((:big    . 0)
              (:little . 0)
              ;; type: single double u8 s8 u16 s16 ...
              (:type   . 1))))


  #|doc
  Iterator fragments are immutable compile-time records. Every induction variable, including
  source positions and list cursors, has an entry in `fragment-bindings` and a corresponding
  expression in `fragment-updates`. `fragment-evaluations` holds peer expression bindings for
  the enclosing setup `let`. The other accessors describe source setup, exhaustion,
  item bindings, public aliases, direct filtering, and parallel seeking. A fragment is never
  reinterpreted as a different positional layout during decoration.
  |#
  (define-record-type (iterator-fragment make-iterator-fragment iterator-fragment?)
    (fields (immutable evaluations fragment-evaluations)
            (immutable setup fragment-setup)
            (immutable bindings fragment-bindings)
            (immutable termination fragment-termination)
            (immutable updates fragment-updates)
            (immutable getter fragment-getter)
            (immutable source-index fragment-source-index)
            (immutable exposed fragment-exposed)
            (immutable raw-termination fragment-raw-termination)
            (immutable filter fragment-filter)
            (immutable visit fragment-visit)))

  #|proc:source-fragment
  The `source-fragment` helper constructs an undecorated iterator fragment from source setup,
  one induction binding, termination, advance, and getter syntax. Optional source-index and
  raw-exhaustion syntax describe native positions and iterator fetching. Additional bindings
  and advances are combined with the primary state immediately. For example, a list slice
  carries both its source position and list tail as ordinary resumable induction variables.
  |#
  (define source-fragment
    (case-lambda
      [(setup binding termination advance getter)
       (source-fragment setup binding termination advance getter #f #f '() '())]
      [(setup binding termination advance getter index)
       (source-fragment setup binding termination advance getter index #f '() '())]
      [(setup binding termination advance getter index raw)
       (source-fragment setup binding termination advance getter index raw '() '())]
      [(setup binding termination advance getter index raw bindings updates)
       (make-iterator-fragment '() setup (cons binding bindings) termination (cons advance updates)
                               getter index '() raw #f #f)]))

  #|proc:compile-termination
  The `compile-termination` helper applies an exhaustion check to two syntax continuations.
  `check` is either a boolean expression or a generator accepting `(success exhausted)`.
  `exhausted` leaves the source and `success` visits its current item. A generator may bind
  a fetched value around `success`, keeping that value in the captured continuation.
  For example, expression `done?` produces `(if done? exhausted success)`.
  |#
  (define (compile-termination check exhausted success)
    (if (procedure? check)
        (check success exhausted)
        #`(if #,check #,exhausted #,success)))

  (define (process-iter-clause/direct cl direct-filter?)
    (define (process-multi-var-iter-clause v* ty op*)
      (let ([nops (length op*)])
        (case ty
          [:hashtable
           (unless (= nops 1)
             (syntax-error op* "invalid number of options for iter type `:hashtable`:"))
           (unless (= 2 (length v*))
             (syntax-error v* "invalid number of bound variables for iter type `:hashtable`:"))
           (with-syntax ([(t-ht t-i t-len t-ks t-vs) (generate-temporaries '(t-ht t-i t-len t-ks t-vs))])
             (source-fragment (lambda (e)
                     #`(let ([t-ht #,(car op*)])
                         (unless (hashtable? t-ht)
                           (errorf ':hashtable "not a hashtable: ~a" t-ht))
                         (let-values ([(t-ks t-vs) (hashtable-entries t-ht)])
                           (let ([t-len (vector-length t-ks)])
                             #,e))))
                   #`[t-i 0]
                   #`(fx= t-i t-len)
                   #`(fx1+ t-i)
                   (lambda (e)
                     #`(let ([#,(car  v*) (vector-ref t-ks t-i)]
                             [#,(cadr v*) (vector-ref t-vs t-i)])
                         #,e))))]
          [else (syntax-error ty "bad iter type:")])))
    #|proc:process-list-iter-clause
    The `process-list-iter-clause` helper compiles sliced list traversal for `variable` and
    source/options syntax `arguments`. It returns the iterator fragment described above.
    Forward traversal follows list links with constant auxiliary space; backward traversal
    snapshots only selected positions in a vector. For example, source `(a b c)` with
    `:from 1` yields `b` and `c` with source positions `1` and `2`, without copying the source.
    |#
    (define (process-list-iter-clause variable arguments)
      (let* ([options (check-ops *sequence-ops* (cdr arguments))]
             [from-option (assoc ':from options)]
             [to-option (assoc ':to options)]
             [step-option (assoc ':step options)]
             [reverse-option (assoc ':rev options)])
        (with-syntax ([(source size from to original-from step position forward? tail snapshot complete-snapshot count base stride)
                       (generate-temporaries
                        '(source size from to original-from step position forward? tail snapshot complete-snapshot count base stride))])
          (source-fragment
           (lambda (body)
             #`(let ([source #,(car arguments)])
                 (unless (list? source) (errorf ':list "not a list: ~a" source))
                 (let* ([from #,(if from-option (cadr from-option) #'0)]
                        [to #,(if to-option (cadr to-option) #'#f)]
                        ;; The common full reverse already needs every source element.
                        [complete-snapshot
                         #,(if (and reverse-option (not from-option) (not to-option)
                                    (not step-option))
                               #'(list->vector source) #'#f)]
                        [size (if complete-snapshot
                                  (vector-length complete-snapshot) (length source))]
                        [to #,(if to-option #'to #'size)]
                        [original-from
                         (begin
                           (unless (and (fixnum? from) (fixnum? to)
                                        (if (fx<= from to)
                                            (fx<= 0 from to size)
                                            (fx<= -1 to from (fx1- size))))
                             (errorf ':list "invalid source bounds: ~a to ~a, length ~a"
                                     from to size))
                           from)]
                        [from #,(if reverse-option
                                    #'(if (fx<= from to) (fx1- to) (fx1+ to))
                                    #'from)]
                        [to #,(if reverse-option
                                  #'(if (fx<= original-from to)
                                        (fx1- original-from) (fx1+ original-from))
                                  #'to)]
                        [forward? (fx<= from to)]
                        [step #,(if step-option
                                    #`(let ([stride #,(cadr step-option)])
                                        (unless (and (positive-natural? stride) (exact? stride))
                                          (errorf ':list
                                                  "step has to be an exact positive integer: ~a"
                                                  stride))
                                        (let ([stride (min stride (abs (fx- to from)))])
                                          (if forward? stride (fx- stride))))
                                    #'(if forward? 1 -1))]
                        [tail (if (fx< from to) (list-tail source from) '())]
                        [stride (if forward? step (fx- step))]
                        [count (if (fx> from to)
                                   (fx1+ (fxquotient (fx1- (fx- from to)) stride)) 0)]
                        [base (if (fxzero? count) 0
                                  (fx- from (fx* (fx1- count) stride)))]
                        [snapshot
                         (or complete-snapshot
                             (and (not forward?)
                                  (let ([result (make-vector count)])
                                    (unless (fxzero? count)
                                      (let fill ([source (list-tail source base)] [index 0])
                                        (vector-set! result index (car source))
                                        (unless (fx= (fx1+ index) count)
                                          (fill (list-tail source stride) (fx1+ index)))))
                                    result)))])
                   #,body)))
           #'[position from]
           #'(if forward? (fx>= position to) (fx<= position to))
           (if step-option
               #'(if forward?
                     (if (fx>= step (fx- to position)) to (fx+ position step))
                     (if (fx<= step (fx- to position)) to (fx+ position step)))
               #'(if forward? (fx1+ position) (fx1- position)))
           (lambda (body)
             #`(let ([#,variable (if forward? (car tail) (vector-ref snapshot (fxquotient (fx- position base) stride)))])
                 #,body))
           #'position
           #f
           #'([tail tail])
           (list (if step-option
                     #'(if (and forward? (fx< step (fx- to position)))
                           (list-tail tail step)
                           tail)
                     #'(if forward? (cdr tail) tail)))))))
    #|proc:process-indexed-sequence
    The `process-indexed-sequence` helper compiles `variable` over sequence type `type` with
    source/options syntax `arguments`. It chooses hygienic primitive predicates, lengths, and
    accessors at expansion time, then shares bounds, reverse, stride, and source-index logic.
    For example, vector `#(a b c)` with `:rev :step 2` yields `c`, then `a`, at positions 2 and 0.
    |#
    (define (process-indexed-sequence variable type arguments)
      (when (null? arguments) (syntax-error cl "for: missing sequence source:"))
      (let* ([options (check-ops *sequence-ops* (cdr arguments))]
             [from-option (assq ':from options)]
             [to-option (assq ':to options)]
             [step-option (assq ':step options)]
             [reverse-option (assq ':rev options)])
        (let-values ([(predicate length-procedure reference)
                      (case type
                        [(:vector) (values #'vector? #'vector-length #'vector-ref)]
                        [(:string) (values #'string? #'string-length #'string-ref)]
                        [(:fxvector) (values #'fxvector? #'fxvector-length #'fxvector-ref)]
                        [(:flvector) (values #'flvector? #'flvector-length #'flvector-ref)])])
          (with-syntax ([(source size position from to original-from step)
                         (generate-temporaries '(source size position from to original-from step))]
                        [who (datum->syntax variable type)])
            (source-fragment
             (lambda (body)
               #`(let ([source #,(car arguments)])
                   (unless (#,predicate source)
                     (errorf 'who "invalid sequence source: ~a" source))
                   (let* ([size (#,length-procedure source)]
                          [from #,(if from-option (cadr from-option) #'0)]
                          [to #,(if to-option (cadr to-option) #'size)]
                          [original-from
                           (begin
                             (unless (and (fixnum? from) (fixnum? to)
                                          (if (fx<= from to)
                                              (fx<= 0 from to size)
                                              (fx<= -1 to from (fx1- size))))
                               (errorf 'who "invalid source bounds: ~a to ~a, length ~a"
                                       from to size))
                             from)]
                          [from #,(if reverse-option
                                      #'(if (fx<= from to) (fx1- to) (fx1+ to)) #'from)]
                          [to #,(if reverse-option
                                    #'(if (fx<= original-from to)
                                          (fx1- original-from) (fx1+ original-from)) #'to)]
                          [step #,(if step-option
                                      #`(let ([stride #,(cadr step-option)])
                                          (unless (and (positive-natural? stride) (exact? stride))
                                            (errorf 'who
                                                    "step has to be an exact positive integer: ~a"
                                                    stride))
                                          (let ([stride (min stride (abs (fx- to from)))])
                                            (if (fx<= from to) stride (fx- stride))))
                                      #'(if (fx<= from to) 1 -1))])
                     #,body)))
             #'[position from]
             #'(if (fx<= from to) (fx>= position to) (fx<= position to))
             #'(if (fx> step 0)
                   (if (fx>= step (fx- to position)) to (fx+ position step))
                   (if (fx<= step (fx- to position)) to (fx+ position step)))
             (lambda (body)
               #`(let ([#,variable (#,reference source position)]) #,body))
             #'position)))))
    (define (process-single-var-iter-clause v ty op*)
      (define (numeric-next step)
        #`(let ([next (+ #,v #,step)])
            ;; Ordered comparison also rejects NaN from opposing infinities.
            (unless (or (< next #,v) (> next #,v))
              (errorf ':nums "step does not advance source value: ~a, step ~a" #,v #,step))
            next))
      (let ([nops (length op*)])
        (case ty
          [:iota
           (unless (= 1 nops)
             (syntax-error op* "invalid number of options for iter type `:iota`:"))
           (with-syntax ([(t-stop) (generate-temporaries '(t-stop))])
             (source-fragment (lambda (e)
                     #`(let ([t-stop #,(car op*)])
                         (unless (and (natural? t-stop) (exact? t-stop))
                           (errorf ':iota "not an exact natural number: ~a" t-stop))
                         #,e))
                   #`[#,v 0]
                   #`(= #,v t-stop)
                   #`(+ #,v 1)
                   #f))]
          [:fxiota
           (unless (= 1 nops)
             (syntax-error op* "invalid number of options for iter type `:fxiota`:"))
           (with-syntax ([(t-stop) (generate-temporaries '(t-stop))])
             (source-fragment (lambda (e)
                     #`(let ([t-stop #,(car op*)])
                         (unless (and (fixnum? t-stop) (fx>= t-stop 0))
                           (errorf ':fxiota "not a nonnegative fixnum: ~a" t-stop))
                         #,e))
                   #`[#,v 0]
                   #`(fx= #,v t-stop)
                   ;; The last increment reaches the representable exclusive bound.
                   #`(fx1+ #,v)
                   #f))]
          [:nums
           (when (not (<= 1 nops 3))
             (syntax-error op* "invalid number of options for iter type `:nums`:"))
           (with-syntax ([(t-start t-stop t-step) (generate-temporaries '(t-start t-stop t-step))])
             (case nops
               [1 (source-fragment (lambda (e)
                          #`(let ([t-stop #,(car op*)])
                              (unless (and (real? t-stop) (nonnegative? t-stop))
                                (errorf ':nums "not a nonnegative real number: ~a" t-stop))
                              #,e))
                        #`[#,v 0]
                        #`(>= #,v t-stop)
                        (numeric-next #'1)
                        #f)]
               [2 (source-fragment (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)])
                              (unless (and (real? t-start) (real? t-stop) (<= t-start t-stop))
                                (errorf ':nums "invalid range from ~a to ~a" t-start t-stop))
                              #,e))
                        #`[#,v t-start]
                        #`(>= #,v t-stop)
                        (numeric-next #'1)
                        #f)]
               [3 (source-fragment (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)] [t-step #,(caddr op*)])
                              (unless (and (real? t-start) (real? t-stop) (real? t-step)
                                           (or (and (<= t-start t-stop) (> t-step 0))
                                               (and (>= t-start t-stop) (< t-step 0))))
                                (errorf ':nums "invalid range from ~a to ~a, step ~a" t-start t-stop t-step))
                              #,e))
                        #`[#,v t-start]
                        #`(if (<= t-start t-stop)
                              (>= #,v t-stop)
                              (<= #,v t-stop))
                        (numeric-next #'t-step)
                        #f)]))]
          [:fixnums
           (when (not (<= 1 nops 3))
             (syntax-error op* "invalid number of options for iter type `:fixnums`:"))
           (with-syntax ([(t-start t-stop t-step) (generate-temporaries '(t-start t-stop t-step))])
             (case nops
               [1 (source-fragment (lambda (e)
                          #`(let ([t-stop #,(car op*)])
                              (unless (and (fixnum? t-stop) (fx>= t-stop 0))
                                (errorf ':fixnums "not a nonnegative fixnum: ~a" t-stop))
                              #,e))
                        #`[#,v 0]
                        #`(fx= #,v t-stop)
                        #`(fx+ #,v 1)
                        #f)]
               [2 (source-fragment (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)])
                              (unless (and (fixnum? t-start) (fixnum? t-stop) (fx<= t-start t-stop))
                                (errorf ':fixnums "invalid range from ~a to ~a" t-start t-stop))
                              #,e))
                        #`[#,v t-start]
                        #`(>= #,v t-stop)
                        #`(fx+ #,v 1)
                        #f)]
               [3 (source-fragment (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)] [t-step #,(caddr op*)])
                              (unless (and (fixnum? t-start) (fixnum? t-stop) (fixnum? t-step)
                                           (or (and (fx<= t-start t-stop) (fx> t-step 0))
                                               (and (fx>= t-start t-stop) (fx< t-step 0))))
                                (errorf ':fixnums "invalid range from ~a to ~a, step ~a" t-start t-stop t-step))
                              #,e))
                        #`[#,v t-start]
                        #`(if (fx<= t-start t-stop)
                              (fx>= #,v t-stop)
                              (fx<= #,v t-stop))
                        ;; Saturate at the exclusive bound if the next fixnum step
                        ;; would overflow; the next termination check then succeeds.
                        #`(if (if (fx> t-step 0)
                                  (fx> #,v (fx- (most-positive-fixnum) t-step))
                                  (fx< #,v (fx- (most-negative-fixnum) t-step)))
                              t-stop
                              (fx+ #,v t-step))
                        #f)]))]
          [:list
           (unless (= nops 1)
             (syntax-error op* "invalid number of options for iter type `:list`:"))
           (with-syntax ([(t-ls) (generate-temporaries '(t-ls))])
             (source-fragment (lambda (e)
                     #`(let ([t-ls #,(car op*)])
                         (unless (list? t-ls)
                           (errorf ':list "not a list: ~a" t-ls))
                         #,e))
                   #`[t-ls t-ls]
                   #`(null? t-ls)
                   #`(cdr t-ls)
                   (lambda (e)
                     #`(let ([#,v (car t-ls)])
                         #,e))))]
          [(:vector :string :fxvector :flvector)
           (process-indexed-sequence v ty op*)]
          [:bytevector
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:bytevector`:"))
           (let* ([ops (check-ops *bytevector-ops* (cdr op*))])
             (let* ([*bv-table* '((single 4 bytevector-ieee-single-ref)
                                  (double 8 bytevector-ieee-double-ref)
                                  (u8     1 bytevector-u8-ref)
                                  (s8     1 bytevector-s8-ref)
                                  (u16    2 bytevector-u16-ref)
                                  (s16    2 bytevector-s16-ref)
                                  (u24    3 bytevector-u24-ref)
                                  (s24    3 bytevector-s24-ref)
                                  (u32    4 bytevector-u32-ref)
                                  (s32    4 bytevector-s32-ref)
                                  (u40    5 bytevector-u40-ref)
                                  (s40    5 bytevector-s40-ref)
                                  (u48    6 bytevector-u48-ref)
                                  (s48    6 bytevector-s48-ref)
                                  (u56    7 bytevector-u56-ref)
                                  (s56    7 bytevector-s56-ref)
                                  (u64    8 bytevector-u64-ref)
                                  (s64    8 bytevector-s64-ref))]
                    [op-from   (assoc ':from   ops)]
                    [op-to     (assoc ':to     ops)]
                    [op-step   (assoc ':step   ops)]
                    [op-rev    (assoc ':rev    ops)]
                    [op-type   (assoc ':type   ops)] ; default: u8
                    [op-big    (assoc ':big    ops)]
                    [op-little (assoc ':little ops)]
                    [op-type   (if op-type
                                   (let ([t (syntax->datum (cadr op-type))])
                                     (unless (memq t '(single double u8 s8 u16 s16 u24 s24 u32 s32 u40 s40
                                                              u48 s48 u56 s56 u64 s64))
                                       (syntax-violation ':bytevector "unknown access type"
                                                         cl (cadr op-type)))
                                     t)
                                   'u8)]
                    [endianness (if op-little
                                    (if op-big
                                        (syntax-violation ':bytevector
                                                          ":big and :little cannot be combined" cl)
                                        #'(endianness little))
                                    (if op-big
                                        #'(endianness big)
                                        #'(endianness little)))])
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (let ([vref (datum->syntax #'bytevector-u8-ref (caddr (assoc op-type *bv-table*)))]
                       [size (datum->syntax v (cadr  (assoc op-type *bv-table*)))])
                   (source-fragment (lambda (e)
                           #`(let ([t-vec #,(car op*)])
                               (unless (bytevector? t-vec)
                                 (errorf ':bytevector "not a bytevector: ~a" t-vec))
                               (let* ([t-len (bytevector-length t-vec)]
                                      ;; Bounds are byte offsets.
                                      [t-from #,(if op-from
                                                    (cadr op-from)
                                                    #'0)]
                                      [t-to   #,(if op-to
                                                    (cadr op-to)
                                                    #'t-len)]
                                      ;; handle :rev
                                      [tt-from
                                       (begin
                                         (unless (and (fixnum? t-from) (fixnum? t-to)
                                                      (if (fx<= t-from t-to)
                                                          (fx<= 0 t-from t-to t-len)
                                                          (fx<= -1 t-to t-from (fx1- t-len))))
                                           (errorf ':bytevector "invalid source bounds: ~a to ~a, length ~a"
                                                   t-from t-to t-len))
                                         t-from)]
                                      [t-from #,(if op-rev
                                                    #`(if (fx<= t-from t-to)
                                                          (fx- t-to 1
                                                               (fxmodulo (fx- t-to t-from) #,size))
                                                          (fx+ t-to 1
                                                               (fxmodulo (fx- t-from t-to) #,size)))
                                                    #'t-from)]
                                      [t-to   #,(if op-rev
                                                    #'(if (<= tt-from t-to)
                                                          (sub1 tt-from)
                                                          (add1 tt-from))
                                                    #'t-to)]
                                      ;; `step` depends on and scaled by `size`
                                      [t-step #,(if op-step
                                                    (let ([s (cadr op-step)])
                                                      #`(let ([s #,s])
                                                          (unless (and (positive-natural? s) (exact? s))
                                                            (errorf ':bytevector "step has to be an exact positive integer: ~a" s))
                                                          (let* ([distance (abs (fx- t-to t-from))]
                                                                 [stride (if (> s (fxquotient distance #,size))
                                                                             distance
                                                                             (fx* s #,size))])
                                                            (if (fx<= t-from t-to)
                                                                stride
                                                                (fx- stride)))))
                                                    #`(if (<= t-from t-to)
                                                          (fx*  1 #,size)
                                                          (fx* -1 #,size)))])
                                 #,e)))
                         #`[t-i t-from]
                         #`(if (fx<= t-from t-to)
                               (fx>= (fx+ t-i #,size -1) t-to)
                               (fx<= (fx- t-i #,size -1) t-to))
                         #`(if (fx> t-step 0)
                               (if (fx>= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step))
                               (if (fx<= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step)))
                         (if (memq op-type '(u8 s8))
                             (lambda (e)
                               #`(let ([#,v (#,vref t-vec t-i)])
                                   #,e))
                             (lambda (e)
                               #`(let ([#,v (#,vref t-vec (if (fx> t-step 0) t-i (fx- t-i #,size -1)) #,endianness)])
                                   #,e)))
                         #`(if (fx> t-step 0) t-i (fx- t-i #,size -1)))))))]
          [:hashtable-keys
           (unless (= nops 1)
             (syntax-error op* "invalid number of options for iter type `:hashtable-keys`:"))
           (with-syntax ([(t-ht t-i t-len t-vec) (generate-temporaries '(t-ht t-i t-len t-vec))])
             (source-fragment (lambda (e)
                     #`(let ([t-ht #,(car op*)])
                         (unless (hashtable? t-ht)
                           (errorf ':hashtable-keys "not a hashtable: ~a" t-ht))
                         (let* ([t-vec (hashtable-keys t-ht)]
                                [t-len (vector-length t-vec)])
                           #,e)))
                   #`[t-i 0]
                   #`(fx= t-i t-len)
                   #`(fx1+ t-i)
                   (lambda (e)
                     #`(let ([#,v (vector-ref t-vec t-i)])
                         #,e))))]
          [:hashtable-values
           (unless (= nops 1)
             (syntax-error op* "invalid number of options for iter type `:hashtable-values`:"))
           (with-syntax ([(t-ht t-i t-len t-vec) (generate-temporaries '(t-ht t-i t-len t-vec))])
             (source-fragment (lambda (e)
                     #`(let ([t-ht #,(car op*)])
                         (unless (hashtable? t-ht)
                           (errorf ':hashtable-values "not a hashtable: ~a" t-ht))
                         (let* ([t-vec (hashtable-values t-ht)]
                                [t-len (vector-length t-vec)])
                           #,e)))
                   #`[t-i 0]
                   #`(fx= t-i t-len)
                   #`(fx1+ t-i)
                   (lambda (e)
                     #`(let ([#,v (vector-ref t-vec t-i)])
                         #,e))))]
          [:iter
           (unless (= nops 1)
             (syntax-error op* "invalid number of options for iter type `:iter`:"))
           (with-syntax ([(t-it t-v unused) (generate-temporaries '(t-it t-v unused))])
             (source-fragment (lambda (e)
                     #`(let ([t-it #,(car op*)])
                         (unless (iter? t-it)
                           (errorf ':iter "not a iter: ~a" t-it))
                         #,e))
                   #'[unused #f]
                   ;; Fetch after known companion bounds, at most once per visit.
                   (lambda (success exhausted)
                     #`(let ([t-v (iter-next! t-it)])
                         (if (iter-end? t-v) #,exhausted #,success)))
                   #'#f
                   (lambda (e)
                     #`(let ([#,v t-v])
                         #,e))
                   #f
                   #'#f))]
          [else (syntax-error ty "bad iter type:")])))
    (define (process-literal-iter-clause v lit)
      (let ([lit (->literal lit)])
        (cond [(natural? lit)
               (source-fragment #f
                     #`[#,v 0]
                     #`(= #,v #,lit)
                     #`(+ #,v 1)
                     #f)]
              [(string? lit)
               (with-syntax ([(t-str t-i) (generate-temporaries '(t-str t-i))])
                 (source-fragment (lambda (e)
                         #`(let ([t-str #,lit])
                             #,e))
                       #'[t-i 0]
                       #'(fx= t-i (string-length t-str))
                       #'(fx1+ t-i)
                       (lambda (e)
                         #`(let ([#,v (string-ref t-str t-i)])
                             #,e))))]
              [(vector? lit)
               (with-syntax ([(t-vec t-i) (generate-temporaries '(t-vec t-i))])
                 (source-fragment (lambda (e)
                         #`(let ([t-vec '#,(datum->syntax v lit)])
                             #,e))
                       #'[t-i 0]
                       #'(fx= t-i (vector-length t-vec))
                       #'(fx1+ t-i)
                       (lambda (e)
                         #`(let ([#,v (vector-ref t-vec t-i)])
                             #,e))))]
              [else (syntax-error lit "bad iter literal type:")])))
    ;; Numeric induction state remains distinct from the public item alias.
    (define (numeric-fragment variable indexed? generate)
      (let* ([state (car (generate-temporaries '(numeric-state)))]
             [fragment (generate state)])
        (make-iterator-fragment
         (fragment-evaluations fragment)
         (fragment-setup fragment) (fragment-bindings fragment) (fragment-termination fragment)
         (fragment-updates fragment) (lambda (body) #`(let ([#,variable #,state]) #,body))
         (and indexed? state) '() (fragment-raw-termination fragment) #f #f)))
    ;; Common options filter each iterator independently. Sequence indexes describe
    ;; source positions; other iterators count their unfiltered source values from zero.
    (define (split-common-options type options)
      (define evaluations '())
      (define (bind-expression expression)
        (let ([name (car (generate-temporaries '(source-expression)))])
          (set! evaluations (cons #`[#,name #,expression] evaluations))
          name))
      (when (null? options) (syntax-violation type "missing iterator source" cl))
      (let loop ([remaining (cdr options)] [arguments (list (bind-expression (car options)))]
                 [positional 1] [predicate #f] [index #f])
        (if (null? remaining)
            (values (reverse arguments) predicate index (reverse evaluations))
            (let* ([option (syntax->datum (car remaining))]
                   [config (assq option
                                 (append '((:guard . 1) (:index . 1))
                                         (case type
                                           [(:list :vector :string :fxvector :flvector)
                                            *sequence-ops*]
                                           [(:bytevector) *bytevector-ops*]
                                           [else '()])))])
              (cond
               [config
                (when (< (length (cdr remaining)) (cdr config))
                  (syntax-error (car remaining) "for: missing iterator option argument:"))
                (case option
                  [(:guard)
                   (when predicate (syntax-error cl "duplicate iterator guard:"))
                   (loop (cddr remaining) arguments positional (cadr remaining) index)]
                  [(:index)
                   (when index (syntax-error cl "duplicate iterator index:"))
                   (unless (identifier? (cadr remaining))
                     (syntax-error (cadr remaining) "iterator index is not an identifier:"))
                   (loop (cddr remaining) arguments positional predicate (cadr remaining))]
                  [else
                   (let ([count (fx1+ (cdr config))])
                     (loop (list-tail remaining count)
                           (append (reverse
                                    (if (memq option '(:from :to :step))
                                        (list (car remaining) (bind-expression (cadr remaining)))
                                        (list-head remaining count)))
                                   arguments)
                           positional predicate index))])]
               [(and (memq type '(:nums :fixnums)) (< positional 3)
                     (not (memq option '(:from :to :step :rev :type :big :little))))
                (loop (cdr remaining) (cons (bind-expression (car remaining)) arguments)
                      (fx1+ positional) predicate index)]
               [else (syntax-error (car remaining) "for: unknown iterator option:")])))))
    (define (decorate-fragment fragment variables predicate index)
      (let* ([native-index (fragment-source-index fragment)]
             [exposed (if index (append variables (list index)) variables)]
             [position (and index (not native-index)
                            (car (generate-temporaries '(source-position))))]
             [getter (or (fragment-getter fragment) (lambda (body) body))]
             [indexed
              (make-iterator-fragment
               (fragment-evaluations fragment)
               (fragment-setup fragment)
               (append (fragment-bindings fragment) (if position (list #`[#,position 0]) '()))
               (fragment-termination fragment)
               (append (fragment-updates fragment)
                       (if position (list #`(+ #,position 1)) '()))
               (if index
                   (lambda (body) (getter #`(let ([#,index #,(or native-index position)]) #,body)))
                   (fragment-getter fragment))
               native-index exposed
               (or (fragment-raw-termination fragment)
                   (and predicate (fragment-termination fragment)))
               #f #f)])
        (if predicate
            (if direct-filter?
                (make-iterator-fragment
                 (fragment-evaluations indexed)
                 (fragment-setup indexed) (fragment-bindings indexed) (fragment-termination indexed)
                 (fragment-updates indexed) (fragment-getter indexed) native-index exposed
                 (fragment-raw-termination indexed)
                 (lambda (body retry)
                   ((or (fragment-getter indexed) (lambda (body) body))
                    #`(if #,predicate #,body #,retry)))
                 #f)
                (syntax-error cl "parallel filter requires private source bindings:"))
            indexed)))

    ;; Parallel visitors bind private item locations before exposing source-local aliases.
    (define (parallel-filter-fragment variables predicate index generate)
      (let* ([private-variables (generate-temporaries variables)]
             [private-index (and index (car (generate-temporaries '(source-index))))]
             [fragment (decorate-fragment (generate private-variables)
                                          private-variables #f private-index)]
             [public (if index (append variables (list index)) variables)]
             [private (if index (append private-variables (list private-index)) private-variables)]
             [getter (or (fragment-getter fragment) (lambda (body) body))]
             [bindings (fragment-bindings fragment)]
             [termination (fragment-termination fragment)]
             [updates (fragment-updates fragment)]
             [bind-public
              (lambda (body)
                (let bind ([names public] [values private])
                  (if (null? names) body
                      #`(let-syntax
                            ([#,(car names)
                              (identifier-syntax
                               (reference #,(car values))
                               ((set! reference expression) (set! #,(car values) expression)))])
                          #,(bind (cdr names) (cdr values))))))])
        (with-syntax ([(seek) (generate-temporaries '(seek))])
          (make-iterator-fragment
           (fragment-evaluations fragment)
           (fragment-setup fragment) bindings termination updates bind-public
           (fragment-source-index fragment) public
           (or (fragment-raw-termination fragment) termination) #f
           (lambda (body exhausted)
             #`(let seek (#,@(map (lambda (binding)
                                   (syntax-case binding () [(name initial) #'[name name]]))
                                 bindings))
                 #,(compile-termination
                    termination exhausted
                    (getter #`(if #,(bind-public predicate) #,body (seek #,@updates))))))))))
    (define (single-fragment variable type arguments)
      (cond
       [(memq type '(:iota :fxiota :nums :fixnums))
        (numeric-fragment variable (memq type '(:iota :fxiota))
                          (lambda (state) (process-single-var-iter-clause state type arguments)))]
       [(and (eq? type ':list) (> (length arguments) 1))
        (process-list-iter-clause variable arguments)]
       [else (process-single-var-iter-clause variable type arguments)]))
    #|proc:with-evaluations
    The `with-evaluations` helper attaches peer source-expression bindings to `fragment`.
    `evaluations` is a list of generated `(name expression)` bindings. For example, a vector
    source and `:from start` supply two bindings evaluated by the surrounding setup `let`,
    before the fragment validates its source and initializes traversal state.
    |#
    (define (with-evaluations evaluations fragment)
      (make-iterator-fragment
       evaluations (fragment-setup fragment) (fragment-bindings fragment)
       (fragment-termination fragment) (fragment-updates fragment) (fragment-getter fragment)
       (fragment-source-index fragment) (fragment-exposed fragment)
       (fragment-raw-termination fragment) (fragment-filter fragment) (fragment-visit fragment)))
    (syntax-case cl ()
      [(v lit)
       (literal? #'lit)
       (with-evaluations
        (syntax-case #'lit (quote)
          [(quote value)
           (with-syntax ([(checked) (generate-temporaries '(checked))])
             #'([checked ($for/check-quoted-literal lit)]))]
          [_ '()])
        (decorate-fragment
         (if (natural? (->literal #'lit))
             (numeric-fragment #'v #t (lambda (state) (process-literal-iter-clause state #'lit)))
             (process-literal-iter-clause #'v #'lit))
         (list #'v) #f #f))]
      [((v* ...) iter-ty op* ...)
       (let-values ([(arguments predicate index evaluations)
                     (split-common-options (datum iter-ty) #'(op* ...))])
         ;; Check caller identifiers before a parallel filter replaces them with private ones.
         (let ([variables #'(v* ...)])
           (when (and (= 2 (length variables))
                      (bound-identifier=? (car variables) (cadr variables)))
             (syntax-error cl "for: duplicate hashtable tuple binding:")))
         (with-evaluations evaluations
          (if (and predicate (not direct-filter?))
             (parallel-filter-fragment
              #'(v* ...) predicate index
              (lambda (variables) (process-multi-var-iter-clause variables (datum iter-ty) arguments)))
             (decorate-fragment (process-multi-var-iter-clause #'(v* ...) (datum iter-ty) arguments)
                                #'(v* ...) predicate index))))]
      [(v iter-ty op* ...)
       (let-values ([(arguments predicate index evaluations)
                     (split-common-options (datum iter-ty) #'(op* ...))])
         (with-evaluations evaluations
          (if (and predicate (not direct-filter?))
             (parallel-filter-fragment
              (list #'v) predicate index
              (lambda (variables) (single-fragment (car variables) (datum iter-ty) arguments)))
             (decorate-fragment (single-fragment #'v (datum iter-ty) arguments)
                                (list #'v) predicate index))))]
      [_ (syntax-error cl "unknown iter clause:")]))

  (define (process-iter-clause clause)
    (process-iter-clause/direct clause #f))

  #|proc:process-iter-clauses
  The `process-iter-clauses` helper compiles parallel iterator syntax `clauses` into fragments.
  Item and source-index identifiers must be distinct across clauses under `bound-identifier=?`.
  For example, `([x 2] [x 3])` is rejected, while hygienically distinct identifiers spelled
  `x` are allowed. An index may still shadow an item within its own clause.
  A table groups identifiers by spelling before comparing their scopes.
  |#
  (define (process-iter-clauses clauses)
    (pcheck ([list? clauses])
      (let ([fragments
             (map (lambda (clause)
                    (process-iter-clause/direct clause (null? (cdr clauses))))
                  clauses)]
            [seen (make-eq-hashtable)])
        (for-each
         (lambda (fragment)
           (let ([names (fragment-exposed fragment)])
             ;; Check the whole clause before recording its same-clause aliases.
             (for-each
              (lambda (name)
                (when (exists (lambda (previous) (bound-identifier=? name previous))
                              (hashtable-ref seen (syntax->datum name) '()))
                  (syntax-error name "for: duplicate parallel iterator binding:")))
              names)
             (for-each
              (lambda (name)
                (let ([spelling (syntax->datum name)])
                  (hashtable-set! seen spelling
                                  (cons name (hashtable-ref seen spelling '())))))
              names)))
         fragments)
        fragments)))


  )
