(library (cspp private for)
  (export literal? ->literal
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
          handle-iter/config-iter handle-iter/config-config

          process-iter-clause process-iter-clause/direct process-iter-clauses)
  (import (cspp chez)
          (cspp utils)
          (cspp internal)
          (cspp io)
          (cspp list)
          (cspp iter))


  (define (literal? lit)
    (let* ([x (syntax->datum lit)]
           [x (if (and (pair? x) (= 2 (length x)) (eq? 'quote (car x)))
                  (cadr x)
                  x)])
      (or (and (natural? x) (exact? x))
          (string? x)
          (vector? x))))

  (define (->literal lit)
    (let ([x (syntax->datum lit)])
      (if (and (pair? x) (= 2 (length x)) (eq? 'quote (car x)))
          (cadr x)
          x)))

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
      (let loop ([n 0] [state (caar table)] [cl* clauses])
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
                (errorf 'for "clauses are not handled entirely: ~a" cl*))
            (let loop-trans ([trans (filter (lambda (row)
                                              (eq? (car row) state))
                                            table)])
              (if (null? trans)
                  (errorf 'for "no handler for clauses: ~a" cl*)
                  (let* ([row     (car trans)]
                         [to      (list-ref row 1)]
                         [handler (list-ref row 2)])
                    ;; when cl* is '(), only epsilon transition is possible
                    (if (null? cl*)
                        (if (eq? handler 'epsilon)
                            (loop (fx1+ n) to cl*)
                            (loop-trans (cdr trans)))
                        ;; TODO remove `next?`
                        (if (eq? handler 'epsilon)
                            (loop (fx1+ n) to cl*)
                            (let-values ([(handled? next?) (handler s (car cl*))])
                              (if handled?
                                  (if next?
                                      (loop (fx1+ n) to (cdr cl*))
                                      (loop (fx1+ n) to cl*))
                                  (loop-trans (cdr trans)))))))))))))


  (define true (lambda (state cl) (values #t #f)))

  (define (add-clause! state name cl)
    (let ([?cl (hashtable-ref state name #f)])
      (if ?cl
          (errorf 'for "clause already exists: ~a" cl)
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
              (errorf 'for "no iter clause before config clause: ~a" cl)
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

  (define *vector-ops*
    ;; op     nop-args
    '((:from  . 1)
      (:to    . 1)
      (:step  . 1)
      (:rev   . 0)))
  (define *string-ops*   *vector-ops*)
  (define *fxvector-ops* *vector-ops*)
  (define *flvector-ops* *vector-ops*)
  (define *bytevector-ops*
    (append *vector-ops*
            '((:big    . 0)
              (:little . 0)
              ;; type: single double u8 s8 u16 s16 ...
              (:type   . 1))))


  #|
  For each iter clause, the following syntax/procedures are returned:
  - gen-preloop-proc: generate code binding variables needed before the loop,
      plus type checking of inputs
  - loop-var: induction variables that correspond to each loop variable
  - terminate: termination checks for the loop variable
  - next: code that advances the loop variable
  - gen-getter: generate code that binds the value to be used in the body
  - source-index: source position expression when the iterator has a native index, otherwise #f
  - exposed-variables: public item and iterator-local index identifiers bound by the getter
  - raw-termination: exhaustion check before filtering, or #f for an unfiltered source
  - direct-filter: optional (body retry) generator that binds and filters without seeking
  - visit: optional (body exhausted) generator that seeks in private item binding scopes
  - extra-bindings / extra-updates: additional resumable induction variables and their updates
  |#
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
             (list (lambda (e)
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
    snapshots the source in a vector. For example, source `(a b c)` with `:from 1` yields
    `b` and `c` with source positions `1` and `2`, without copying the source.
    |#
    (define (process-list-iter-clause variable arguments)
      (let* ([options (check-ops *vector-ops* (cdr arguments))]
             [from-option (assoc ':from options)]
             [to-option (assoc ':to options)]
             [step-option (assoc ':step options)]
             [reverse-option (assoc ':rev options)])
        (with-syntax ([(source size from to original-from step position forward? tail snapshot)
                       (generate-temporaries
                        '(source size from to original-from step position forward? tail snapshot))])
          (list
           (lambda (body)
             #`(let ([source #,(car arguments)])
                 (unless (list? source) (errorf ':list "not a list: ~a" source))
                 (let* ([from #,(if from-option (cadr from-option) #'0)]
                        [to #,(if to-option (cadr to-option) #'#f)]
                        ;; A backward snapshot supplies its own length, avoiding another scan.
                        [snapshot
                         (and (fixnum? from)
                              #,(if to-option
                                    #`(and (fixnum? to)
                                           (#,(if reverse-option #'fx< #'fx>) from to))
                                    (if reverse-option #'#t #'#f))
                              (list->vector source))]
                        [size (if snapshot (vector-length snapshot) (length source))]
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
                        [tail (if (fx< from to) (list-tail source from) '())])
                   #,body)))
           #'[position from]
           #'(if forward? (fx>= position to) (fx<= position to))
           (if step-option
               #'(if forward?
                     (if (fx>= step (fx- to position)) to (fx+ position step))
                     (if (fx<= step (fx- to position)) to (fx+ position step)))
               #'(if forward? (fx1+ position) (fx1- position)))
           (lambda (body)
             #`(let ([#,variable (if forward? (car tail) (vector-ref snapshot position))])
                 #,body))
           #'position
           #f
           #'([tail tail])
           (list (if step-option
                     #'(if (and forward? (fx< step (fx- to position)))
                           (list-tail tail step)
                           tail)
                     #'(if forward? (cdr tail) tail)))))))
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
             (list (lambda (e)
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
             (list (lambda (e)
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
               [1 (list (lambda (e)
                          #`(let ([t-stop #,(car op*)])
                              (unless (and (real? t-stop) (nonnegative? t-stop))
                                (errorf ':nums "not a nonnegative real number: ~a" t-stop))
                              #,e))
                        #`[#,v 0]
                        #`(>= #,v t-stop)
                        (numeric-next #'1)
                        #f)]
               [2 (list (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)])
                              (unless (and (real? t-start) (real? t-stop) (<= t-start t-stop))
                                (errorf ':nums "invalid range from ~a to ~a" t-start t-stop))
                              #,e))
                        #`[#,v t-start]
                        #`(>= #,v t-stop)
                        (numeric-next #'1)
                        #f)]
               [3 (list (lambda (e)
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
             (syntax-error op* "invalid number of options for iter type `:fxnums`:"))
           (with-syntax ([(t-start t-stop t-step) (generate-temporaries '(t-start t-stop t-step))])
             (case nops
               [1 (list (lambda (e)
                          #`(let ([t-stop #,(car op*)])
                              (unless (and (fixnum? t-stop) (fx>= t-stop 0))
                                (errorf ':fxnums "not a nonnegative fixnum: ~a" t-stop))
                              #,e))
                        #`[#,v 0]
                        #`(fx= #,v t-stop)
                        #`(fx+ #,v 1)
                        #f)]
               [2 (list (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)])
                              (unless (and (fixnum? t-start) (fixnum? t-stop) (fx<= t-start t-stop))
                                (errorf ':fxnums "invalid range from ~a to ~a" t-start t-stop))
                              #,e))
                        #`[#,v t-start]
                        #`(>= #,v t-stop)
                        #`(fx+ #,v 1)
                        #f)]
               [3 (list (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)] [t-step #,(caddr op*)])
                              (unless (and (fixnum? t-start) (fixnum? t-stop) (fixnum? t-step)
                                           (or (and (fx<= t-start t-stop) (fx> t-step 0))
                                               (and (fx>= t-start t-stop) (fx< t-step 0))))
                                (errorf ':fxnums "invalid range from ~a to ~a, step ~a" t-start t-stop t-step))
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
             (list (lambda (e)
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
          [:vector
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:vector`:"))
           ;; op*: ((:from 10) (:until 20) (:step 2) (:rev))
           (let* ([ops (check-ops *vector-ops* (cdr op*))])
             ;; (println "ops: ~a" ops)
             ;; all args are syntax objs
             (let ([op-from (assoc ':from ops)]
                   [op-to   (assoc ':to   ops)]
                   [op-step (assoc ':step ops)]
                   [op-rev  (assoc ':rev  ops)])
               ;; (println "op-from: ~a" op-from)
               ;; (println "op-to:   ~a" op-to)
               ;; (println "op-step: ~a" op-step)
               ;; (println "op-rev:  ~a" op-rev)
               ;; Q: how to use these ops?
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (list (lambda (e)
                         #`(let ([t-vec #,(car op*)]) ; it seems tycheck can be put here
                             (unless (vector? t-vec)
                               (errorf ':vector "not a vector: ~a" t-vec))
                             (let* ([t-len (vector-length t-vec)]
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
                                         (errorf ':vector "invalid source bounds: ~a to ~a, length ~a"
                                                 t-from t-to t-len))
                                       t-from)]
                                    [t-from #,(if op-rev
                                                  #'(if (<= t-from t-to)
                                                        (sub1 t-to)
                                                        (add1 t-to))
                                                  #'t-from)]
                                    [t-to   #,(if op-rev
                                                  #'(if (<= tt-from t-to)
                                                        (sub1 tt-from)
                                                        (add1 tt-from))
                                                  #'t-to)]
                                    [t-step #,(if op-step
                                                  (let ([s (cadr op-step)])
                                                    #`(let ([s #,s])
                                                        (unless (and (positive-natural? s) (exact? s))
                                                          (errorf ':vector "step has to be an exact positive integer: ~a" s))
                                                        (let ([stride (min s (abs (fx- t-to t-from)))])
                                                          (if (fx<= t-from t-to)
                                                              stride
                                                              (fx- stride)))))
                                                  #'(if (<= t-from t-to)
                                                        1
                                                        -1))])
                               ;; (println ":vector :from ~a :to ~a :step ~a" t-from t-to t-step)
                               #,e)))
                       #`[t-i t-from]
                       #`(if (fx<= t-from t-to)
                             (fx>= t-i t-to)
                             (fx<= t-i t-to))
                       #`(if (fx> t-step 0)
                               (if (fx>= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step))
                               (if (fx<= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step)))
                       (lambda (e)
                         #`(let ([#,v (vector-ref t-vec t-i)])
                             #,e))
                       #'t-i))))]
          [:string
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:string`:"))
           (let* ([ops (check-ops *string-ops* (cdr op*))])
             ;; all args are syntax objs
             (let ([op-from (assoc ':from ops)]
                   [op-to   (assoc ':to   ops)]
                   [op-step (assoc ':step ops)]
                   [op-rev  (assoc ':rev  ops)])
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (list (lambda (e)
                         #`(let ([t-vec #,(car op*)]) ; it seems tycheck can be put here
                             (unless (string? t-vec)
                               (errorf ':string "not a string: ~a" t-vec))
                             (let* ([t-len (string-length t-vec)]
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
                                         (errorf ':string "invalid source bounds: ~a to ~a, length ~a"
                                                 t-from t-to t-len))
                                       t-from)]
                                    [t-from #,(if op-rev
                                                  #'(if (<= t-from t-to)
                                                        (sub1 t-to)
                                                        (add1 t-to))
                                                  #'t-from)]
                                    [t-to   #,(if op-rev
                                                  #'(if (<= tt-from t-to)
                                                        (sub1 tt-from)
                                                        (add1 tt-from))
                                                  #'t-to)]
                                    [t-step #,(if op-step
                                                  (let ([s (cadr op-step)])
                                                    #`(let ([s #,s])
                                                        (unless (and (positive-natural? s) (exact? s))
                                                          (errorf ':string "step has to be an exact positive integer: ~a" s))
                                                        (let ([stride (min s (abs (fx- t-to t-from)))])
                                                          (if (fx<= t-from t-to)
                                                              stride
                                                              (fx- stride)))))
                                                  #'(if (<= t-from t-to)
                                                        1
                                                        -1))])
                               ;; (println ":string :from ~a :to ~a :step ~a" t-from t-to t-step)
                               #,e)))
                       #`[t-i t-from]
                       #`(if (fx<= t-from t-to)
                             (fx>= t-i t-to)
                             (fx<= t-i t-to))
                       #`(if (fx> t-step 0)
                               (if (fx>= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step))
                               (if (fx<= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step)))
                       (lambda (e)
                         #`(let ([#,v (string-ref t-vec t-i)])
                             #,e))
                       #'t-i))))]
          [:fxvector
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:fxvector`:"))
           (let* ([ops (check-ops *fxvector-ops* (cdr op*))])
             ;; all args are syntax objs
             (let ([op-from (assoc ':from ops)]
                   [op-to   (assoc ':to   ops)]
                   [op-step (assoc ':step ops)]
                   [op-rev  (assoc ':rev  ops)])
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (list (lambda (e)
                         #`(let ([t-vec #,(car op*)]) ; it seems tycheck can be put here
                             (unless (fxvector? t-vec)
                               (errorf ':fxvector "not a fxvector: ~a" t-vec))
                             (let* ([t-len (fxvector-length t-vec)]
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
                                         (errorf ':fxvector "invalid source bounds: ~a to ~a, length ~a"
                                                 t-from t-to t-len))
                                       t-from)]
                                    [t-from #,(if op-rev
                                                  #'(if (<= t-from t-to)
                                                        (sub1 t-to)
                                                        (add1 t-to))
                                                  #'t-from)]
                                    [t-to   #,(if op-rev
                                                  #'(if (<= tt-from t-to)
                                                        (sub1 tt-from)
                                                        (add1 tt-from))
                                                  #'t-to)]
                                    [t-step #,(if op-step
                                                  (let ([s (cadr op-step)])
                                                    #`(let ([s #,s])
                                                        (unless (and (positive-natural? s) (exact? s))
                                                          (errorf ':fxvector "step has to be an exact positive integer: ~a" s))
                                                        (let ([stride (min s (abs (fx- t-to t-from)))])
                                                          (if (fx<= t-from t-to)
                                                              stride
                                                              (fx- stride)))))
                                                  #'(if (<= t-from t-to)
                                                        1
                                                        -1))])
                               ;; (println ":fxvector :from ~a :to ~a :step ~a" t-from t-to t-step)
                               #,e)))
                       #`[t-i t-from]
                       #`(if (fx<= t-from t-to)
                             (fx>= t-i t-to)
                             (fx<= t-i t-to))
                       #`(if (fx> t-step 0)
                               (if (fx>= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step))
                               (if (fx<= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step)))
                       (lambda (e)
                         #`(let ([#,v (fxvector-ref t-vec t-i)])
                             #,e))
                       #'t-i))))]
          [:flvector
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:flvector`:"))
           (let* ([ops (check-ops *flvector-ops* (cdr op*))])
             ;; all args are syntax objs
             (let ([op-from (assoc ':from ops)]
                   [op-to   (assoc ':to   ops)]
                   [op-step (assoc ':step ops)]
                   [op-rev  (assoc ':rev  ops)])
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (list (lambda (e)
                         #`(let ([t-vec #,(car op*)]) ; it seems tycheck can be put here
                             (unless (flvector? t-vec)
                               (errorf ':flvector "not a flvector: ~a" t-vec))
                             (let* ([t-len (flvector-length t-vec)]
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
                                         (errorf ':flvector "invalid source bounds: ~a to ~a, length ~a"
                                                 t-from t-to t-len))
                                       t-from)]
                                    [t-from #,(if op-rev
                                                  #'(if (<= t-from t-to)
                                                        (sub1 t-to)
                                                        (add1 t-to))
                                                  #'t-from)]
                                    [t-to   #,(if op-rev
                                                  #'(if (<= tt-from t-to)
                                                        (sub1 tt-from)
                                                        (add1 tt-from))
                                                  #'t-to)]
                                    [t-step #,(if op-step
                                                  (let ([s (cadr op-step)])
                                                    #`(let ([s #,s])
                                                        (unless (and (positive-natural? s) (exact? s))
                                                          (errorf ':flvector "step has to be an exact positive integer: ~a" s))
                                                        (let ([stride (min s (abs (fx- t-to t-from)))])
                                                          (if (fx<= t-from t-to)
                                                              stride
                                                              (fx- stride)))))
                                                  #'(if (<= t-from t-to)
                                                        1
                                                        -1))])
                               ;; (println ":flvector :from ~a :to ~a :step ~a" t-from t-to t-step)
                               #,e)))
                       #`[t-i t-from]
                       #`(if (fx<= t-from t-to)
                             (fx>= t-i t-to)
                             (fx<= t-i t-to))
                       #`(if (fx> t-step 0)
                               (if (fx>= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step))
                               (if (fx<= t-step (fx- t-to t-i))
                                   t-to
                                   (fx+ t-i t-step)))
                       (lambda (e)
                         #`(let ([#,v (flvector-ref t-vec t-i)])
                             #,e))
                       #'t-i))))]
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
                                       (errorf ':bytevector "unknown access type: ~a" t))
                                     t)
                                   'u8)]
                    [endianness (if op-little
                                    (if op-big
                                        (errorf ':bytevector ":big and :little cannot be given at the same time")
                                        #'(endianness little))
                                    (if op-big
                                        #'(endianness big)
                                        #'(endianness little)))])
               (with-syntax ([(t-vec t-len t-i t-from t-to t-step)
                              (generate-temporaries '(t-vec t-len t-i t-from t-to t-step))])
                 (let ([vref (datum->syntax #'bytevector-u8-ref (caddr (assoc op-type *bv-table*)))]
                       [size (datum->syntax v (cadr  (assoc op-type *bv-table*)))])
                   ;; (println "vref: ~a size: ~a" vref size)
                   (list (lambda (e)
                           #`(let ([t-vec #,(car op*)])
                               (unless (bytevector? t-vec)
                                 (errorf ':bytevector "not a bytevector: ~a" t-vec))
                               (let* ([t-len (bytevector-length t-vec)]
                                      ;; TODO in terms of what, bytes?
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
                                 ;; (println ":bytevector :from ~a :to ~a :step ~a" t-from t-to t-step)
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
             (list (lambda (e)
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
             (list (lambda (e)
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
           (with-syntax ([(t-it t-v pending unused) (generate-temporaries '(t-it t-v pending unused))])
             (list (lambda (e)
                     #`(let ([t-it #,(car op*)])
                         (unless (iter? t-it)
                           (errorf ':iter "not a iter: ~a" t-it))
                         (let ([t-v #f] [pending #t]) #,e)))
                   #'[unused #f]
                   ;; Fetch after known companion bounds, at most once per visit.
                   #'(begin
                       (when pending
                         (set! t-v (iter-next! t-it))
                         (set! pending #f))
                       (iter-end? t-v))
                   #'(begin (set! pending #t) #f)
                   (lambda (e)
                     #`(let ([#,v t-v])
                         #,e))
                   #f
                   #'#f))]
          [else (syntax-error ty "bad iter type:")])))
    (define (process-literal-iter-clause v lit)
      (let ([lit (->literal lit)])
        (cond [(natural? lit)
               (list #f
                     #`[#,v 0]
                     #`(= #,v #,lit)
                     #`(+ #,v 1)
                     #f)]
              [(string? lit)
               (with-syntax ([(t-str t-i) (generate-temporaries '(t-str t-i))])
                 (list (lambda (e)
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
                 (list (lambda (e)
                         #`(let ([t-vec '#,(datum->syntax v lit)])
                             #,e))
                       #'[t-i 0]
                       #'(fx= t-i (vector-length t-vec))
                       #'(fx1+ t-i)
                       (lambda (e)
                         #`(let ([#,v (vector-ref t-vec t-i)])
                             #,e))))]
              [else (syntax-error lit "bad iter literal type:")])))
    ;; Numeric induction state must remain distinct from the exposed item binding.
    (define (numeric-fragment variable indexed? generate)
      (let* ([state (car (generate-temporaries '(numeric-state)))]
             [fragment (generate state)])
        (append (list-head fragment 4)
                (list (lambda (body) #`(let ([#,variable #,state]) #,body)))
                (if indexed? (list state) '()))))
    ;; Common options filter each iterator independently. Sequence indexes describe
    ;; source positions; other iterators count their unfiltered source values from zero.
    (define (split-common-options type options)
      (when (null? options) (syntax-error cl "for: missing iterator source:"))
      (let loop ([remaining (cdr options)] [arguments (list (car options))]
                 [positional 1] [predicate #f] [index #f])
        (if (null? remaining)
            (values (reverse arguments) predicate index)
            (let* ([option (syntax->datum (car remaining))]
                   [config (assq option
                                 (append '((:guard . 1) (:index . 1))
                                         (case type
                                           [(:list :vector :string :fxvector :flvector)
                                            *vector-ops*]
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
                           (append (reverse (list-head remaining count)) arguments)
                           positional predicate index))])]
               [(and (memq type '(:nums :fixnums)) (< positional 3)
                     (not (memq option '(:from :to :step :rev :type :big :little))))
                (loop (cdr remaining) (cons (car remaining) arguments)
                      (fx1+ positional) predicate index)]
               [else (syntax-error (car remaining) "for: unknown iterator option:")])))))
    (define (decorate-fragment fragment variables predicate index)
      (let* ([source-index (and (> (length fragment) 5) (list-ref fragment 5))]
             [raw-termination (or (and (> (length fragment) 6) (list-ref fragment 6))
                                  (and predicate (list-ref fragment 2)))]
             [extra-bindings (if (> (length fragment) 7) (list-ref fragment 7) '())]
             [extra-updates (if (> (length fragment) 8) (list-ref fragment 8) '())]
             [exposed (if index (append variables (list index)) variables)])
        (define (finish fragment)
          (append (list-head fragment 5)
                  (list source-index exposed raw-termination #f #f extra-bindings extra-updates)))
        (cond
         [(not (or predicate index)) (finish fragment)]
         [(and predicate direct-filter?)
          ;; A single source can filter in the item's lexical scope and retry its
          ;; own loop. No accepted-item cache or mutable seeking state is needed.
          (let* ([indexed (decorate-fragment fragment variables #f index)]
                 [getter (or (list-ref indexed 4) (lambda (body) body))])
            (append (list-head indexed 8)
                    (list (lambda (body retry)
                            (getter #`(if #,predicate #,body #,retry)))
                          #f)
                    (list-tail indexed 10)))]
         [(not predicate)
          ;; Index-only sources need neither filtering nor accepted-item caches.
          (with-syntax ([(position) (generate-temporaries '(position))])
            (let ([preloop (or (car fragment) (lambda (body) body))]
                  [getter (or (list-ref fragment 4) (lambda (body) body))])
              (finish
               (list (if source-index
                         (car fragment)
                         (lambda (body) (preloop #`(let ([position 0]) #,body))))
                     (list-ref fragment 1)
                     (list-ref fragment 2)
                     (if source-index
                         (list-ref fragment 3)
                         #`(begin (set! position (+ position 1)) #,(list-ref fragment 3)))
                     (lambda (body)
                       (getter #`(let ([#,index #,(or source-index #'position)]) #,body)))))))]
         [else (syntax-error cl "parallel filter requires private source bindings:")])))
    ;; Each visitor binds fresh private variables. Public aliases are visible only
    ;; to that source's predicate, and later to the body, so parallel guards cannot
    ;; accidentally capture a previously visited companion's item bindings.
    (define (parallel-filter-fragment variables predicate index generate)
      (let* ([private-variables (generate-temporaries variables)]
             [private-index (and index (car (generate-temporaries '(source-index))))]
             [fragment (decorate-fragment (generate private-variables)
                                          private-variables #f private-index)]
             [public (if index (append variables (list index)) variables)]
             [private (if index (append private-variables (list private-index)) private-variables)]
             [getter (or (list-ref fragment 4) (lambda (body) body))]
             [binding (list-ref fragment 1)]
             [state (syntax-case binding () [(state initial) #'state])]
             [termination (list-ref fragment 2)]
             [advance (list-ref fragment 3)]
             [extra-bindings (list-ref fragment 10)]
             [extra-updates (list-ref fragment 11)]
             [bind-public
              (lambda (body)
                ;; The index may shadow the item, as in the direct-source getter.
                (let bind ([names public] [values private])
                  (if (null? names)
                      body
                      #`(let-syntax
                            ([#,(car names)
                              (identifier-syntax
                               (reference #,(car values))
                               ((set! reference expression) (set! #,(car values) expression)))])
                          #,(bind (cdr names) (cdr values))))))])
        (with-syntax ([(seek) (generate-temporaries '(seek))])
          (list (car fragment) binding termination advance bind-public
                (list-ref fragment 5) public
                (or (list-ref fragment 7) termination)
                #f
                (lambda (body exhausted)
                  #`(let seek ([#,state #,state]
                               #,@(map (lambda (binding)
                                         (syntax-case binding () [(name initial) #'[name name]]))
                                       extra-bindings))
                      (if #,termination
                          #,exhausted
                          #,(getter #`(if #,(bind-public predicate)
                                          #,body
                                          (seek #,advance #,@extra-updates))))))
                extra-bindings extra-updates))))
    (define (single-fragment variable type arguments)
      (cond
       [(memq type '(:iota :fxiota :nums :fixnums))
        (numeric-fragment variable (memq type '(:iota :fxiota))
                          (lambda (state) (process-single-var-iter-clause state type arguments)))]
       [(and (eq? type ':list) (> (length arguments) 1))
        (process-list-iter-clause variable arguments)]
       [else (process-single-var-iter-clause variable type arguments)]))
    (syntax-case cl ()
      [(v lit)
       (decorate-fragment
        (if (natural? (->literal #'lit))
            (numeric-fragment #'v #t (lambda (state) (process-literal-iter-clause state (datum lit))))
            (process-literal-iter-clause #'v (datum lit)))
        (list #'v) #f #f)]
      [((v* ...) iter-ty op* ...)
       (let-values ([(arguments predicate index) (split-common-options (datum iter-ty) #'(op* ...))])
         (if (and predicate (not direct-filter?))
             (parallel-filter-fragment
              #'(v* ...) predicate index
              (lambda (variables) (process-multi-var-iter-clause variables (datum iter-ty) arguments)))
             (decorate-fragment (process-multi-var-iter-clause #'(v* ...) (datum iter-ty) arguments)
                                #'(v* ...) predicate index)))]
      [(v iter-ty op* ...)
       (let-values ([(arguments predicate index) (split-common-options (datum iter-ty) #'(op* ...))])
         (if (and predicate (not direct-filter?))
             (parallel-filter-fragment
              (list #'v) predicate index
              (lambda (variables) (single-fragment (car variables) (datum iter-ty) arguments)))
             (decorate-fragment (single-fragment #'v (datum iter-ty) arguments)
                                (list #'v) predicate index)))]
      [_ (syntax-error cl "unknown iter clause:")]))

  (define (process-iter-clause clause)
    (process-iter-clause/direct clause #f))

  (define (process-iter-clauses cl*)
    ;; (for-each displayln cl*)
    (map (lambda (clause) (process-iter-clause/direct clause (null? (cdr cl*)))) cl*))


  )
