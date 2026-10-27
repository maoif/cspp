(library (cspp for)
  (export for/fold  for  for/list  for/vector
          for*/fold for* for*/list for*/vector)
  (import (cspp chez)
          (cspp private for)
          (cspp internal)
          (cspp iter)
          (cspp list)
          (cspp vector)
          (cspp utils)
          (cspp io))




  #|doc
(for/fold (<init-clause>?
           <index-clause>?    ; global index
           <finish-clause>?
           <iter-clause>+
           <for-config-clause>*)
  <body>+)

;; used to introduce an accumulator value
<init-clause> := (:init <var> <expr>)

<iter-clause> := [<var>    <iter-type> <option>*]
              |  [(<var>+) <iter-type> <option>*] ; number of <var> depends on the iter type
              |  [<var>    <literal>]             ; cannot have options, and binds only one <var>

<iter-type> := :iota <expr>
            |  :fxiota <expr>
            |  :nums <expr>
            |  :nums <expr> <expr>
            |  :nums <expr> <expr> <expr>
            |  :fixnums <expr>
            |  :fixnums <expr> <expr>
            |  :fixnums <expr> <expr> <expr>
            |  :string <expr>
            |  :list <expr>
            |  :vector  <expr>
            |  :fxvector <expr>
            |  :flvector <expr>
            |  :bytevector <expr>
            |  :hashtable <expr>
            |  :hashtable-values <expr>
            |  :hashtable-keys <expr>
            |  :iter <expr>

<option> := :from  <expr>
         |  :to    <expr>
         |  :step  <expr>
         |  :guard <expr>
         |  :index <var>
         |  :rev
         |  :type <bytevector-type> ; bytevectors only
         |  :big                    ; bytevectors only
         |  :little                 ; bytevectors only

<bytevector-type> := single | double
                   | u8 | s8 | u16 | s16 | u24 | s24 | u32 | s32
                   | u40 | s40 | u48 | s48 | u56 | s56 | u64 | s64

<literal> := <exact-nonnegative-integer> ; iter from 0 to the exclusive bound
          |  <string>   ; iter chars in the string from left to right
          |  <vector>   ; iter items in the vector from left to right

<for-config-clause> := <break-clause>
                    |  <guard-clause>
                    |  <bind-clause>

<index-clause> := [:index <var>]

<finish-clause> := [:finish <expr>]

<break-clause> := [:break <expr>]
               |  [:break <expr> <expr>]  ; value of 2nd expr computes the return value

<guard-clause> := [:guard <expr>]
               |  [:guard <expr> :index <var>] ; index incrs only when guard passes

<bind-clause> := [:let <var> <expr>]
              |  [:let* <var> <expr>]
              |  [:letrec <var> <expr>]
              |  [:letrec* <var> <expr>]
              |  [:let-values (<var> ...) <expr>]
              |  [:let*-values (<var> ...) <expr>]

<stop-clause> := [:stop <expr>]
              |  [:stop <expr> <expr>]
  |#
  #|macro:for/fold
  The `for/fold` macro iterates its source clauses in parallel and evaluates the body.
  `:init` binds an accumulator updated from the body result; `:finish` supplies the final value.
  Binding clauses scope their variables over subsequent clauses and the body. Each binding clause
  uses its corresponding Scheme binding form with one binding. An indexed guard binds a zero-based
  index counting passes through that guard, including passes rejected by a later guard.
  Clause tags such as `:length` and `:guard` are reserved as bare iterator-binding names.
  Source expressions are evaluated left to right in clause order.
  Iterator-local `:guard` filters its own source before parallel items are paired. Iterator-local
  `:index` binds the original source position, before filtering, and is available to the guard.
  Local guards share item and index bindings with the body. They see surrounding lexical
  bindings and their own source bindings, before the fold accumulator and global index aliases.
  Sequence positions survive `:from`, `:to`, `:step`, and `:rev`; typed bytevector positions are
  byte offsets. Typed bytevector traversal ignores an incomplete final chunk; `:rev` preserves
  the complete chunks anchored at the original start, then traverses them in reverse order.
  Numeric ranges, hashtables, and iterators count source items starting from zero.
  Lists support the sequence traversal options. Forward list slices use constant auxiliary space;
  backward traversal snapshots selected values in a vector. Bounds checks scan the whole list.
  Sequence `:from` and `:to` require fixnum source positions within the selected source.
  Ascending slices use `0 <= from <= to <= length`; descending slices use
  `-1 <= to <= from < length`. Sequence `:step` requires an exact positive integer.
  Large strides stop after the first selected item; inexact integers are rejected.
  `[var :iota bound]` visits integers from zero up to an exact nonnegative integer `bound`.
  `[var :fxiota bound]` requires a nonnegative fixnum bound and uses fixnum arithmetic.
  Both forms evaluate `bound` once per source setup and accept only `:guard` and `:index` options.
  All `for` variants support these sources; nested variants set up each source per outer item.
  `:nums` and `:fixnums` accept a stop value, a start and stop, or a start, stop, and step.
  The default start is zero and the default step is one; the stop is exclusive.
  Equal endpoints produce an empty range with either a positive or negative nonzero step.
  `:nums` uses real numbers; `:fixnums` requires fixnums and avoids overflow on its final step.
  `:from`, `:to`, `:step`, and `:rev` apply to lists, strings, and vector-like sources.
  Bytevectors default to `:type u8` and little endianness; `:big` and `:little` are exclusive.
  Their positions are byte offsets and their stride is measured in values of the selected type.
  A `:nums` range raises an error if its next value is unchanged or unordered, including NaN.
  Known exhausted parallel sources stop before a companion `:iter` advances. With only
  unknown-length iterators, discovering exhaustion can consume one unpaired companion value.
  Independent invocations keep their loop state separate and may run in different fibers or
  threads; callbacks and bodies may yield or block. Sources and accumulator objects remain
  caller-owned. The loop does not synchronize shared mutation or consumption of one iterator.
  Callers must coordinate shared mutable state and avoid changing a source's traversal structure
  during iteration. A `:iter` source advances destructively and is not rewound by continuation
  re-entry; callers remain responsible for its reset and finalization, including on early exit.
  Source positions, indexes, guard counts, and fold accumulators are loop parameters, restored
  on continuation replay. Mutation of an accumulator object or external source is not undone.
  |#
  (define-syntax for/fold
    (syntax-rules ()
      [(_ clauses body ...) ($for/fold #t #f clauses body ...)]))

  #|macro:$for/fold
  The `$for/fold` helper parses and compiles one loop. `folding?` permits accumulator and finish
  clauses only for the public fold form. Wrappers delegate here without reparsing the clauses.
  Optional private identifier `counter` adds a body-visit counter as a loop parameter. It is
  separate from public indexes and is also in scope for an internal collection finish clause.
  |#
  (define-syntax $for/fold
    (lambda (stx)
      ;; this differs from that of for*
      (define handler
        (make-clause-handler
         ;; order matters
         ;; state0 state1 action
         `(start init ,handle-init)
         `(start e0   epsilon)

         `(init index ,handle-index)
         `(init e1    epsilon)

         `(e0   index ,handle-index)
         `(e0   e1    epsilon)

         `(index finish ,handle-finish)
         `(index e2     epsilon)

         `(e1    finish ,handle-finish)
         `(e1    e2     epsilon)

         `(finish iter0 ,handle-iter)
         `(e2     iter0 ,handle-iter)

         `(iter0 iter ,handle-iter)
         `(iter0 e3   epsilon)

         `(iter iter   ,handle-iter)
         `(iter config ,handle-config)
         `(iter e4     epsilon)

         `(e3  config  ,handle-config)
         `(e3  e4      epsilon)

         `(e4  end     epsilon)

         `(config config ,handle-config)
         `(config end    epsilon)))
      (syntax-case stx ()
        [(_ folding? counter (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init   (hashtable-ref fragments 'init #f)]
                [frag-index  (hashtable-ref fragments 'index #f)]
                [frag-finish (hashtable-ref fragments 'finish #f)]
                [frag-config (hashtable-ref fragments 'config #f)]
                [guard-indices
                 (filter id
                         (map (lambda (conf)
                                (syntax-case conf ()
                                  [(:guard e :index v)
                                   (and (kw:guard? #':guard) (kw:index? #':index))
                                   (cons conf (car (generate-temporaries '(guard-index))))]
                                  [_ #f]))
                              (or frag-config '())))])
           (unless (datum folding?)
             (when (or frag-init frag-finish)
               (syntax-error stx "for: :init and :finish require a fold form:")))
           (with-syntax ([(for-loop t-acc t-finish state-acc state-index)
                          (generate-temporaries '(for-loop t-acc t-finish state-acc state-index))])
             (let* ([iter-frags (process-iter-clauses (hashtable-ref fragments 'iter #f))]
                    [direct-filter (and (null? (cdr iter-frags)) (fragment-filter (car iter-frags)))]
                    [direct-exposed (if direct-filter (fragment-exposed (car iter-frags)) '())]
                    [init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[state-acc e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[state-index 0]])
                                   #f)]
                    [init-var (if frag-init
                                  #'state-acc
                                  #f)]
                    [index-var (if frag-index
                                   #'state-index
                                   #f)]
                    [gen-preloops (fold-left (lambda (res x*)
                                               (if (fragment-setup x*)
                                                   (cons (fragment-setup x*) res)
                                                   res))
                                             '() (reverse iter-frags))]
                    [loop-vars (let ([res (apply append
                                                (map fragment-bindings iter-frags))])
                                 ;; order matters
                                 (when frag-index (set! res (cons index-def res)))
                                 (when frag-init  (set! res (cons init-def  res)))
                                 (append res (if (identifier? #'counter) #'([counter 0]) '())
                                         (map (lambda (entry) #`[#,(cdr entry) 0])
                                                  guard-indices)))]
                    [termination-checks
                     (append
                      ;; Check all known bounds before any filter can seek indefinitely.
                      (if (exists (lambda (fragment) (fragment-raw-termination fragment)) iter-frags)
                          (map (lambda (fragment)
                                 (or (fragment-raw-termination fragment) (fragment-termination fragment)))
                               iter-frags)
                          '())
                      (map (lambda (fragment) (or (fragment-visit fragment) (fragment-termination fragment)))
                           iter-frags))]
                    [updates (let ([res (apply append
                                              (map fragment-updates iter-frags))])
                               (when frag-index
                                 (set! res (cons (syntax-case frag-index ()
                                                   [(_ v) #'(fx1+ state-index)])
                                                 res)))
                               (when frag-init
                                 (set! res (cons #'t-acc res)))
                               (append res (if (identifier? #'counter) #'((fx1+ counter)) '())
                                       (map cdr guard-indices)))]
                    ;; Rejected configuration guards advance sources, not the body count or fold.
                    [skip-updates (append (if frag-init (list #'state-acc) '())
                                          (if frag-index (list #'state-index) '())
                                          (apply append
                                                 (map fragment-updates iter-frags))
                                          (if (identifier? #'counter) #'(counter) '())
                                          (map cdr guard-indices))]
                    [bind-loop-state
                     (lambda (body)
                             #`(let (#,@(if (and frag-init
                                                (not (exists
                                                      (lambda (v) (bound-identifier=? v
                                                        (syntax-case frag-init () [(_ v e) #'v])))
                                                      direct-exposed)))
                                            (list #`[#,(syntax-case frag-init () [(_ v e) #'v]) state-acc])
                                            '())
                                     #,@(if (and frag-index
                                                (not (exists
                                                      (lambda (v) (bound-identifier=? v
                                                        (syntax-case frag-index () [(_ v) #'v])))
                                                      direct-exposed)))
                                            (list #`[#,(syntax-case frag-index () [(_ v) #'v]) state-index])
                                            '()))
                                 #,body))]
                    [gen-getters
                     (if direct-filter
                         (list (lambda (body)
                                 (direct-filter
                                  (bind-loop-state body)
                                  #`(for-loop #,@(if frag-init (list #'state-acc) '())
                                              #,@(if frag-index (list #'state-index) '())
                                              #,@(fragment-updates (car iter-frags))
                                              #,@(if (identifier? #'counter) #'(counter) '())
                                              #,@(map cdr guard-indices)))))
                         (cons bind-loop-state
                               (map (lambda (x*) (fragment-getter x*)) iter-frags)))]
                    [call/finish-proc (if frag-finish
                                          (let ([params (if (identifier? #'counter) #'(counter) '())])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@(if frag-init (list #'state-acc) '())
                                                                  #,@(if frag-index (list #'state-index) '())
                                                                  #,@(if (identifier? #'counter) #'(counter) '()))
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               (unless (= (length loop-vars) (length updates))
                 (syntax-error stx "loop vars and recur call args do not match:"))
               #`(let ([t-finish #,(cadr call/finish-proc)])
                   #,(let lp-preloops ([gen-preloops gen-preloops])
                       (if (null? gen-preloops)
                           #`(let for-loop (#,@loop-vars)
                               #,(let lp-checks ([termination-checks termination-checks])
                                   (if (null? termination-checks)
                                       (let lp-getters ([gen-getters gen-getters])
                                         (if (null? gen-getters)
                                             (let lp-configs ([configs (if frag-config frag-config '())])
                                               (if (null? configs)
                                                   (if frag-init
                                                       #`(let ([t-acc (begin body* ...)])
                                                           (for-loop #,@updates))
                                                       #`(begin body* ...
                                                                (for-loop #,@updates)))
                                                   (let ([conf (car configs)])
                                                     (syntax-case conf ()
                                                       [(:break e)
                                                        (kw:break? #':break)
                                                        #`(if e
                                                              (void)
                                                              #,(lp-configs (cdr configs)))]
                                                       [(:break e1 e2)
                                                        (kw:break? #':break)
                                                        #`(if e1
                                                              e2
                                                              #,(lp-configs (cdr configs)))]
                                                       [(:guard e :index v)
                                                        (kw:guard? #':guard)
                                                        (let ([counter (cdr (assq conf guard-indices))])
                                                          #`(if e
                                                                (let ([v #,counter] [#,counter (fx1+ #,counter)])
                                                                  #,(lp-configs (cdr configs)))
                                                                (for-loop #,@skip-updates)))]
                                                       [(:guard e)
                                                        (kw:guard? #':guard)
                                                        #`(if e
                                                              #,(lp-configs (cdr configs))
                                                              (for-loop #,@skip-updates))]
                                                       [_ (compile-binding-clause
                                                           conf (lambda (names)
                                                                  (lp-configs (cdr configs))))]))))
                                             (if (car gen-getters)
                                                 ((car gen-getters) (lp-getters (cdr gen-getters)))
                                                 (lp-getters (cdr gen-getters)))))
                                       (let ([check (car termination-checks)]
                                             [success (lp-checks (cdr termination-checks))])
                                         (if (procedure? check)
                                             (check success (car call/finish-proc))
                                             #`(if #,check #,(car call/finish-proc) #,success))))))
                           (if (car gen-preloops)
                               ((car gen-preloops) (lp-preloops (cdr gen-preloops)))
                               (lp-preloops (cdr gen-preloops)))))))))])))




  #|doc
(for*/fold (<init-clause>?
            <index-clause>?    ; global index
            <finish-clause>?
            <for*-clause>+)
  <body>+)

<for*-clause> := <iter-clause> <for*-config-clause>*

<for*-config-clause> := <break-clause>      ; in for*, break one level of loop
                     |  <guard-clause>
                     |  <index-clause>      ; local index, counts visits reaching this clause
                     |  <bind-clause>
                     |  <stop-clause>       ; only in for*, stop the entire iter
  |#
  #|macro:for*/fold
  The `for*/fold` macro nests its source clauses and evaluates the body for each combination.
  Inner sources can use outer bindings and are evaluated on every entry to their loop level.
  `:init` binds an accumulator updated from the body result; `:finish` supplies the final value.
  Binding clauses use the corresponding Scheme binding form and scope over subsequent clauses.
  Indexed guards count passes starting at zero and reset on every entry to their loop level.
  A standalone `[:index name]` after an iterator binds a zero-based counter of visits reaching
  that clause, equivalent to `[:guard #t :index name]`. It resets whenever its loop level is
  entered, is visible to subsequent clauses and inner sources, and may shadow outer bindings.
  Earlier guards filter its count; later guards and empty inner sources do not undo an increment.
  Mutation or shadowing of `name` does not change the private counter. Multiple local index
  clauses keep independent counters. A leading `[:index name]` still counts body evaluations
  globally; iterator-local `:index` options still expose original source positions.
  For example, `(for*/list ([x 4] [:guard (odd? x)] [:index i]) (list x i))`
  returns `((1 0) (3 1))`.
  Iterator-local guards and source indexes follow the `for/fold` rules and restart on each entry.
  `:break` leaves one loop level. Its optional result replaces the fold accumulator on an inner
  break and is returned directly on an outermost break. Without an accumulator, inner results
  are evaluated and discarded. `:stop` leaves all levels, returning its optional second expression
  or an unspecified value when that expression is omitted.
  Source ownership, shared-state synchronization, and iterator lifecycle follow `for/fold`.
  |#
  (define-syntax for*/fold
    (syntax-rules ()
      [(_ clauses body ...) ($for*/fold #t #f clauses body ...)]))

  #|macro:$for*/fold
  The `$for*/fold` helper parses and compiles one loop. `folding?` permits accumulator and finish
  clauses only for the public fold form. Wrappers delegate here without reparsing the clauses.
  Optional private identifier `counter` travels through all loop levels as a body-visit counter,
  independently of public index bindings. An internal collection finish clause may use it.
  |#
  (define-syntax $for*/fold
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start init ,handle-init)
         `(start e0   epsilon)

         `(init index ,handle-index)
         `(init e1    epsilon)

         `(e0   index ,handle-index)
         `(e0   e1    epsilon)

         `(index finish ,handle-finish)
         `(index e2     epsilon)

         `(e1    finish ,handle-finish)
         `(e1    e2     epsilon)

         `(finish iter0 ,handle-iter/config-iter)
         `(e2     iter0 ,handle-iter/config-iter)

         `(iter0  config0 ,handle-iter/config-config)
         `(iter0  e3      epsilon)

         `(config0 config0 ,handle-iter/config-config)
         `(config0 e4     epsilon)

         `(e3 e4 epsilon)

         `(e4 iter ,handle-iter/config-iter)
         `(e4 e5   epsilon)

         `(iter config ,handle-iter/config-config)
         `(iter e5     epsilon)

         `(config config ,handle-iter/config-config)
         `(config iter   ,handle-iter/config-iter)
         `(config end    epsilon)

         `(e5 iter ,handle-iter/config-iter)
         `(e5 end  epsilon)))
      (syntax-case stx ()
        [(_ folding? counter (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init        (hashtable-ref fragments 'init #f)]
                [frag-index       (hashtable-ref fragments 'index #f)]
                [frag-finish      (hashtable-ref fragments 'finish #f)]
                [frag-iter/config (hashtable-ref fragments 'iter/config #f)])
           (unless (datum folding?)
             (when (or frag-init frag-finish)
               (syntax-error stx "for: :init and :finish require a fold form:")))
           (with-syntax ([(t-acc t-finish state-acc state-index)
                          (generate-temporaries '(t-acc t-finish state-acc state-index))])
             (let* ([init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[state-acc e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[state-index 0]])
                                   #f)]
                    [init-var (if frag-init
                                  #'state-acc
                                  #f)]
                    [index-var (if frag-index
                                   #'state-index
                                   #f)]
                    [init/index-vars (let* ([vars (if frag-index (list index-var)    '())]
                                            [vars (if frag-init  (cons init-var vars) vars)]
                                            [vars (append vars (if (identifier? #'counter) #'(counter) '()))])
                                       vars)]
                    ;; Each loop level pairs a named iterator fragment with its configurations.
                    [iters/configs (map (lambda (i/c)
                                          (cons (process-iter-clause/direct (car i/c) #t)
                                                (cdr i/c)))
                                        frag-iter/config)]
                    [call/finish-proc (if frag-finish
                                          (let ([params (if (identifier? #'counter) #'(counter) '())])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@(if frag-init (list #'state-acc) '())
                                                                  #,@(if frag-index (list #'state-index) '())
                                                                  #,@(if (identifier? #'counter) #'(counter) '()))
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               #`(let ([t-finish #,(cadr call/finish-proc)])
                   #,(let lp-for-loop ([i/c* iters/configs] [last-forloop #f] [last-update '()] [shadowed '()])
                     (let ([call-lastforloop #`(#,last-forloop #,@init/index-vars #,@last-update)])
                       (if (null? i/c*)
                           (let ([call-loop (lambda (init?)
                                              (if frag-index
                                                  #`(#,last-forloop #,@init? (fx1+ #,index-var)
                                                     #,@(if (identifier? #'counter) #'((fx1+ counter)) '())
                                                     #,@last-update)
                                                  #`(#,last-forloop #,@init?
                                                     #,@(if (identifier? #'counter) #'((fx1+ counter)) '())
                                                     #,@last-update)))])
                             (if frag-init
                                 #`(let ([t-acc (begin body* ...)])
                                     #,(call-loop (list #'t-acc)))
                                 #`(begin body* ...
                                          #,(call-loop '()))))
                           (with-syntax ([(forloop) (generate-temporaries '(forloop))])
                             (let* ([i/c (car i/c*)] [iter-cl (car i/c)] [configs (cdr i/c)]
                                    [exposed (fragment-exposed iter-cl)]
                                    [guard-indices
                                     (filter id
                                             (map (lambda (conf)
                                                    (syntax-case conf ()
                                                      [(:guard e :index v)
                                                       (and (kw:guard? #':guard) (kw:index? #':index))
                                                       (cons conf (car (generate-temporaries '(guard-index))))]
                                                      [_ #f]))
                                                  configs))]
                                    [gen-preloop (or (fragment-setup iter-cl) (lambda (body) body))]
                                    [loop-var          (fragment-bindings iter-cl)]
                                    [termination-check (fragment-termination iter-cl)]
                                    [update (append (fragment-updates iter-cl) (map cdr guard-indices))]
                                    [gen-getter        (fragment-getter iter-cl)]
                                    [direct-filter     (fragment-filter iter-cl)]
                                    [loop-var
                                     (let ([vars (if last-forloop
                                                     (let* ([vars (if frag-index (list #`[#,index-var #,index-var])     '())]
                                                            [vars (if frag-init  (cons #`[#,init-var  #,init-var] vars) vars)])
                                                       vars)
                                                     (let* ([vars (if frag-index (list index-def)     '())]
                                                            [vars (if frag-init  (cons init-def vars) vars)])
                                                       vars))])
                                       `(,@vars
                                         ,@(if (identifier? #'counter)
                                               (if last-forloop #'([counter counter]) #'([counter 0])) '())
                                         ,@loop-var
                                         ,@(map (lambda (entry) #`[#,(cdr entry) 0]) guard-indices)))])
                               ;; if current level terminates, call forloop of last level
                               (gen-preloop
                                #`(let forloop (#,@loop-var)
                                   #,(compile-termination termination-check
                                       ;; continue on the outer loop
                                       (if last-forloop
                                             call-lastforloop
                                             ;; already outermost loop, stop
                                             (car call/finish-proc))
                                       (let* ([state-shadowed (if direct-filter
                                                                    (append exposed shadowed)
                                                                    shadowed)]
                                                [bind-loop-state
                                              (lambda (body)
                                                #`(let (#,@(if (and frag-init
                                                                    (not (exists (lambda (v) (bound-identifier=? v
                                                                             (syntax-case frag-init () [(_ n e) #'n])))
                                                                                 state-shadowed)))
                                                               (list #`[#,(syntax-case frag-init () [(_ n e) #'n]) state-acc])
                                                               '())
                                                        #,@(if (and frag-index
                                                                    (not (exists (lambda (v) (bound-identifier=? v
                                                                             (syntax-case frag-index () [(_ n) #'n])))
                                                                                 state-shadowed)))
                                                               (list #`[#,(syntax-case frag-index () [(_ n) #'n]) state-index])
                                                               '()))
                                                    #,body))]
                                                [gen-bindings
                                                 (lambda (body)
                                                   (if direct-filter
                                                       (direct-filter
                                                        (bind-loop-state body)
                                                        #`(forloop #,@init/index-vars #,@update))
                                                       (bind-loop-state
                                                        (if gen-getter (gen-getter body) body))))])
                                           (gen-bindings
                                            (let lp-configs ([configs (if configs configs '())] [shadowed (append exposed shadowed)])
                                              (if (null? configs)
                                                  (lp-for-loop (cdr i/c*) #'forloop update shadowed)
                                                  (let ([conf (car configs)])
                                                    (syntax-case conf ()
                                                      [(:break e)
                                                       (kw:break? #':break)
                                                       #`(if e
                                                             #,(if last-forloop
                                                                   call-lastforloop
                                                                   ;; already outermost loop, stop
                                                                   #`(void))
                                                             #,(lp-configs (cdr configs) shadowed))]
                                                      [(:break e1 e2)
                                                       (kw:break? #':break)
                                                       #`(if e1
                                                             #,(if last-forloop
                                                                   (if frag-init
                                                                       #`(let ([break-result e2])
                                                                           (#,last-forloop break-result
                                                                            #,@(if frag-index (list index-var) '())
                                                                            #,@(if (identifier? #'counter) #'(counter) '())
                                                                            #,@last-update))
                                                                       #`(begin e2 #,call-lastforloop))
                                                                   #'e2)
                                                             #,(lp-configs (cdr configs) shadowed))]
                                                      [(:stop e1 e2)
                                                       (kw:stop? #':stop)
                                                       #`(if e1
                                                             e2
                                                             #,(lp-configs (cdr configs) shadowed))]
                                                      [(:guard e :index v)
                                                       (kw:guard? #':guard)
                                                       (let ([counter (cdr (assq conf guard-indices))])
                                                         #`(if e
                                                               (let ([v #,counter] [#,counter (fx1+ #,counter)])
                                                                 #,(lp-configs (cdr configs) (cons #'v shadowed)))
                                                               (forloop #,@init/index-vars #,@update)))]
                                                      [(:guard e)
                                                       (kw:guard? #':guard)
                                                       #`(if e
                                                             #,(lp-configs (cdr configs) shadowed)
                                                             (forloop #,@init/index-vars #,@update))]
                                                      [_ (compile-binding-clause
                                                          conf (lambda (names)
                                                                 (lp-configs (cdr configs)
                                                                             (append names shadowed))))]))))))))))))))))))])))




  #|
(<for> (<index-clause>?
        <iter-clause>+
        <for-config-clause>*)
  <body>+)
  |#
  #|macro:for
  The `for` macro runs its iterators in parallel and evaluates the body for each accepted tuple.
  It returns an unspecified value after exhaustion, or an outermost `:break` result. The optional
  global `:index` counts body evaluations; iterator options and configuration clauses follow
  `for/fold`.
  |#
  (define-syntax for
    (syntax-rules ()
      [(_ clauses body ...) ($for/fold #f #f clauses body ...)]))


  ;; A block's start and buffer are fixed; replay may reuse and overwrite its buffer.
  ;; The next link caches allocated storage, while numeric positions remain loop parameters.
  (define-record-type collection-block
    (fields start data (mutable next)))

  #|proc:collection-block-add!
  The `collection-block-add!` helper writes `value` at logical `position` and returns its block.
  `block` is the block used by the preceding iteration. At a block boundary it reuses the next
  block or allocates one, growing from 16 slots to at most 1024 slots per block.
  For example, position 16 after a full 16-slot block writes slot zero of its successor.
  Positions and block cursors are loop parameters; buffers and cached next links stay mutable.
  Replaying a continuation can overwrite these buffers, without restoring their earlier contents.
  |#
  (define (collection-block-add! block position value)
    (let* ([data (collection-block-data block)]
           [block (if (fx< position (fx+ (collection-block-start block) (vector-length data)))
                      block
                      (or (collection-block-next block)
                          (let ([next (make-collection-block
                                       position
                                       (make-vector (fxmin 1024 (fx* 2 (vector-length data))))
                                       #f)])
                            (collection-block-next-set! block next)
                            next)))])
      (vector-set! (collection-block-data block) (fx- position (collection-block-start block)) value)
      block))

  #|proc:collection-block->vector
  The `collection-block->vector` helper copies `count` collected values from `first` into a new
  exact-sized vector. `first` is the initial block and `count` is the restored loop count.
  For example, blocks holding `a`, `b`, and stale later values with count 2 return `#(a b)`.
  Later buffered positions are ignored. Concurrent access to shared buffers needs synchronization.
  |#
  (define (collection-block->vector first count)
    (let ([result (make-vector count)])
      (let copy ([block first])
        (let ([start (collection-block-start block)])
          (when (fx< start count)
            (let* ([data (collection-block-data block)]
                   [size (fxmin (vector-length data) (fx- count start))])
              (vector-copy! data 0 result start size)
              (when (fx< (fx+ start size) count)
                (copy (collection-block-next block)))))))
      result))

  #|macro:$for/collect
  The `$for/collect` helper parses collection options for `loop`, either `for` or `for*`, then
  delegates to the fold compiler with a private result count. `vector?` selects vector storage
  and `who` names the public form in errors. List tails and vector block cursors are fold state.
  A leading global index precedes length and fill clauses. For example, `:length 2` limits a
  traversal to two accepted results, including after replay; zero skips source setup and body.
  Storage is mutable and shared by saved continuations. Replay can change earlier results.
  |#
  (define-syntax $for/collect
    (lambda (stx)
      (define (generate loop vector? who clauses body)
        (let* ([index (and (pair? clauses) (syntax-case (car clauses) ()
                          [(tag name) (kw:index? #'tag) (car clauses)] [_ #f]))]
               [clauses (if index (cdr clauses) clauses)]
               [length (and (pair? clauses) (syntax-case (car clauses) ()
                           [(tag value) (kw:length? #'tag) #'value] [_ #f]))]
               [clauses (if length (cdr clauses) clauses)]
               [fill (and (pair? clauses) (syntax-case (car clauses) ()
                         [(tag value) (kw:fill? #'tag) #'value] [_ #f]))]
               [clauses (if fill (cdr clauses) clauses)]
               [nested? (eq? (syntax->datum loop) 'for*)])
          (when (and fill (not (and vector? length)))
            (syntax-error stx "for: :fill requires a vector form with :length:"))
          (when (and length (pair? clauses)
                     (syntax-case (car clauses) () [(tag . rest) (kw:index? #'tag)] [_ #f]))
            (syntax-error (car clauses) "global :index must precede :length:"))
          ;; Infer only a single unfiltered source, retaining its once-only type check.
          (let ([inferred
                 (and vector? (not length)
                      (syntax-case clauses ()
                        [((variable tag bound))
                         (eq? (datum tag) ':fxiota)
                         (with-syntax ([(size) (generate-temporaries '(size))])
                           #`(let ([size bound])
                               (unless (and (fixnum? size) (fx>= size 0))
                                 (errorf ':fxiota "not a nonnegative fixnum: ~a" size))
                               #,(generate loop #t who
                                           (append (if index (list index) '())
                                                   #'((:length size) (variable tag size))) body)))]
                        [_ #f]))])
            (or inferred
                (with-syntax ([(storage cursor count limit finish next)
                               (generate-temporaries '(storage cursor count limit finish next))]
                              [compiler (if nested? #'$for*/fold #'$for/fold)]
                              [who who] [(body ...) body])
                  (let* ([counter? (or vector? length)]
                         [result (cond [(not vector?) #'(begin (set-cdr! cursor '()) (cdr storage))]
                                       [length #'storage]
                                       [else #'(collection-block->vector storage count)])]
                         [mapped
                          (let map-clauses ([remaining clauses] [level 0])
                            (if (null? remaining) '()
                                (let* ([clause (car remaining)]
                                       [configuration?
                                        (syntax-case clause ()
                                          [(tag . arguments)
                                           (and (identifier? #'tag)
                                                (kw-for*-config-clause? #'tag))]
                                          [_ #f])]
                                       [level (if configuration? level (fx1+ level))]
                                       [exit (if (and nested? (fx> level 1)) #'cursor result)]
                                       [clause
                                        (syntax-case clause ()
                                          [(tag condition)
                                           (kw:break? #'tag) #`(tag condition #,exit)]
                                          [(tag condition value)
                                           (kw:break? #'tag) #`(tag condition (begin value #,exit))]
                                          [(tag condition)
                                           (and nested? (kw:stop? #'tag)) #`(tag condition #,result)]
                                          [(tag condition value)
                                           (and nested? (kw:stop? #'tag))
                                           #`(tag condition (begin value #,result))]
                                          [_ clause])])
                                  (cons clause (map-clauses (cdr remaining) level)))))]
                         [append-value
                          (cond
                           [(not vector?)
                            #'(let ([next (cons (begin body ...) '())])
                                (set-cdr! cursor next)
                                next)]
                           [length #'(begin (vector-set! storage count (begin body ...)) cursor)]
                           [else #'(collection-block-add! cursor count (begin body ...))])]
                         [traversal
                          #`(compiler #t #,(if counter? #'count #'#f)
                              ((:init cursor storage)
                               #,@(if index (list index) '())
                               (:finish #,result)
                               #,@mapped)
                              #,(if length
                                    #`(let ([next #,append-value])
                                        (when (fx= (fx1+ count) limit)
                                          ;; Leave every level with the completed shared storage.
                                          (finish #,(if vector? #'storage #'(cdr storage))))
                                        next)
                                    append-value))]
                         [storage #`(let ([storage #,(cond [(not vector?) #'(cons #f '())]
                                                          [length #`(make-vector limit #,(or fill #'#f))]
                                                          [else #'(make-collection-block 0 (make-vector 16) #f)])])
                                      #,(if length
                                            #`(if (fxzero? limit)
                                                  #,(if vector? #'storage #'(cdr storage))
                                                  ;; A captured general continuation promotes this escape.
                                                  (call/1cc (lambda (finish) #,traversal)))
                                            traversal))])
                    (if length
                        #`(let ([limit #,length])
                            (unless (and (fixnum? limit) (fx>= limit 0))
                              (errorf 'who "length is not a nonnegative fixnum: ~a" limit))
                            #,storage)
                        storage)))))))
      (syntax-case stx ()
        [(_ loop vector? who (clause ...) body ...)
         (generate #'loop (datum vector?) #'who #'(clause ...) #'(body ...))])))

  #|doc
(for/list (<index-clause>?
           <length-clause>?
           <iter-clause>+
           <for-config-clause>*)
  <body>+)
  |#
  #|macro:for/list
  The `for/list` macro runs `for` and collects each body result in source order.
  It returns a newly built list. `:index`, iterator options, guards, bindings, and breaks follow
  `for/fold`; the body result is evaluated once for every accepted tuple. An optional `:length`
  after the global index limits the number of collected results to a nonnegative fixnum.
  Its expression is evaluated once before the sources; zero skips the sources and body.
  For example, `(for/list ([:length 2] [x 5]) x)` returns `(0 1)`.
  The list tail and optional length counter are loop parameters restored by continuation replay.
  List cells and the output head remain shared mutable storage: replay may overwrite links or
  change previously returned lists, and does not reconstruct detached prefixes. Mutating shared
  results while replaying a saved continuation is caller responsibility. Independent invocations
  have separate storage; the ownership and synchronization rules of `for/fold` apply here.
  |#
  (define-syntax for/list
    (syntax-rules ()
      [(_ clauses body ...) ($for/collect for #f for/list clauses body ...)]))


  #|doc
(for/vector (<index-clause>?
             <vector-size-clauses>?
             <iter-clause>+
             <for-config-clause>*)
  <body>+)

<length-clause> := [:length <expr>] ; <expr> must evaluate to a non-negative fixnum
<fill-clause>   := [:fill   <expr>]
<vector-size-clauses> := <length-clause> <fill-clause>?
  |#
  #|macro:for/vector
  The `for/vector` macro runs `for` and collects body results in a vector.
  With `:length`, it preallocates that many slots and stops when full; `:fill` supplies the
  initial value. Without `:length`, it collects values in vector blocks and copies them into
  the final vector. A single unfiltered `:fxiota` source supplies an inferred exact length.
  `:fill` requires `:length`; a fill-only form is a syntax violation. The fill expression is
  evaluated once, even when the length is zero.
  Independent invocations have separate collection storage. Source and result objects are not
  protected against concurrent caller mutation; the ownership rules of `for/fold` apply here.
  Result counts and block cursors are loop parameters restored by continuation replay, keeping
  length limits and write positions valid. Buffers and cached block links remain shared mutable
  storage and are not rolled back. With an explicit or inferred length, replay can overwrite
  the previously returned vector; slots not written by a shorter replay retain their contents.
  Without a length, finalization copies the current buffered
  prefix into a new vector; intervening buffer writes remain visible to later finalizations.
  |#
  (define-syntax for/vector
    (syntax-rules ()
      [(_ clauses body ...) ($for/collect for #t for/vector clauses body ...)]))


  #|doc
(for* (<index-clause>?
       <for*-clause>+)
   <body>+)
  |#
  #|macro:for*
  The `for*` macro nests its iterator clauses, evaluating each inner source for every outer item.
  It returns an unspecified value after exhaustion, or an outermost `:break` or `:stop` result.
  Configuration clauses belong to the iterator they follow; global `:index` and binding semantics
  follow `for*/fold`.
  |#
  (define-syntax for*
    (syntax-rules ()
      [(_ clauses body ...) ($for*/fold #f #f clauses body ...)]))


  #|doc
(for*/list (<index-clause>?
              <length-clause>?
              <for*-clause>+)
  <body>+)
  |#
  #|macro:for*/list
  The `for*/list` macro runs `for*` and collects each body result in nested traversal order.
  It returns a newly built list; iterator-local guards, indexes, bindings, `:break`, and `:stop`
  apply at the iterator level where they appear. Optional `:length` follows `for/list` and limits
  the total results across all levels. For example,
  `(for*/list ([:length 3] [x 2] [y 2]) (list x y))` returns `((0 0) (0 1) (1 0))`.
  Independent invocations have separate builders; ownership and synchronization follow `for/list`.
  |#
  (define-syntax for*/list
    (syntax-rules ()
      [(_ clauses body ...) ($for/collect for* #f for*/list clauses body ...)]))


  #|doc
(for*/vector (<index-clause>?
              <vector-size-clauses>?
              <for*-clause>+)
  <body>+)
  |#
  #|macro:for*/vector
  The `for*/vector` macro runs `for*` and collects body results in a vector.
  With `:length`, it preallocates the vector and stops when full; `:fill` supplies its initial
  value. Without `:length`, it collects nested results in vector blocks and creates the final
  vector. A single unfiltered `:fxiota` source supplies an inferred exact length.
  As with `for/vector`, `:fill` requires `:length` and its expression is evaluated once,
  including when the length is zero.
  Independent invocations have separate storage; ownership and synchronization follow `for/vector`.
  |#
  (define-syntax for*/vector
    (syntax-rules ()
      [(_ clauses body ...) ($for/collect for* #t for*/vector clauses body ...)]))

  )
