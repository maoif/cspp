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
           <for-config-clasue>*)
  <body>+)

;; used to introduce an accumulator value
<init-clause> := (:init <var> <expr>)

<iter-clause> := [<var>    <iter-type> <option>*]
              |  [(<var>+) <iter-type> <option>*] ; number of <var> depends on the iter type
              |  [<var>    <literal>]             ; cannot have options, and binds only one <var>

<iter-type> := :iota <expr>
            |  :nums <expr> <expr> <expr>
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

<literal> := <integer>  ; iter from 0 to <integer> - 1
          |  <string>   ; iter chars in the string from left to right
          |  <vector>   ; iter items in the vector from left to right

<for-config-clause> := <break-clause>
                    |  <guard-clasue>
                    |  <bind-clause>

<index-clause> := [:index <var>]

<finish-clause> := [:finish <expr>]

<break-clause> := [:break <expr>]
               |  [:break <expr> <expr>]  ; value of 2nd expr computes the return value

<guard-clasue> := [:guard <expr>]
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
  (define-syntax for/fold
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
        [(_ (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init   (hashtable-ref fragments 'init #f)]
                [frag-index  (hashtable-ref fragments 'index #f)]
                [frag-finish (hashtable-ref fragments 'finish #f)]
                [frag-config (hashtable-ref fragments 'config #f)])
           (with-syntax ([(for-loop t-acc t-finish) (generate-temporaries '(for-loop t-acc t-finish))])
             (let* ([iter-frags (process-iter-clauses (hashtable-ref fragments 'iter #f))]
                    [init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[v e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[v 0]])
                                   #f)]
                    [init-var (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'v])
                                  #f)]
                    [index-var (if frag-index
                                   (syntax-case frag-index () [(_ v) #'v])
                                   #f)]
                    [gen-preloops (fold-left (lambda (res x*)
                                               (if (list-ref x* 0)
                                                   (cons (list-ref x* 0) res)
                                                   res))
                                             '() iter-frags)]
                    [loop-vars (let ([res (map (lambda (x*) (list-ref x* 1)) iter-frags)])
                                 ;; order matters
                                 (when frag-index (set! res (cons index-def res)))
                                 (when frag-init  (set! res (cons init-def  res)))
                                 res)]
                    [termination-checks (map (lambda (x*) (list-ref x* 2)) iter-frags)]
                    [updates (let ([res (map (lambda (x*) (list-ref x* 3)) iter-frags)])
                               (when frag-index
                                 (set! res (cons (syntax-case frag-index ()
                                                   [(_ v) #'(fx1+ v)])
                                                 res)))
                               (when frag-init
                                 (set! res (cons #'t-acc res)))
                               res)]
                    [gen-getters (map (lambda (x*) (list-ref x* 4)) iter-frags)]
                    [call/finish-proc (if frag-finish
                                          (let ([params '()])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@params)
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               (unless (= (length loop-vars) (length updates))
                 (syntax-error stx "loop vars and recur call args do not match:"))
               ;; (println "for -------------------------")
               ;; (printf "gen-preloops: ~a~n" gen-preloops)
               ;; (printf "loop-vars:    ~a~n" loop-vars)
               ;; (printf "termination-checks: ~a~n" termination-checks)
               ;; (printf "updates:      ~a~n" updates)
               ;; (printf "gen-getters:  ~a~n" gen-getters)
               ;; (printf "call/finish-proc: ~a~n" call/finish-proc)
               ;; (printf "frag-config:  ~a~n" frag-config)
               ;; (println "for -------------------------")
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
                                                       [(:guard e)
                                                        (kw:guard? #':guard)
                                                        #`(if e
                                                              #,(lp-configs (cdr configs))
                                                              #,(if frag-init
                                                                    #`(let ([t-acc #,(syntax-case frag-init ()
                                                                                       [(_ v e) #'v])])
                                                                        (for-loop #,@updates))
                                                                    #`(for-loop #,@updates)))]
                                                       [(:let v e)
                                                        (kw:let? #':let)
                                                        #`(let ([v e])
                                                            #,(lp-configs (cdr configs)))]
                                                       [(:let-values (v* ...) e)
                                                        (kw:let-values? #':let-values)
                                                        #`(let-values ([(v* ...) e])
                                                            #,(lp-configs (cdr configs)))]
                                                       [_ (syntax-error conf "unknown config clause:")]))))
                                             (if (car gen-getters)
                                                 ((car gen-getters) (lp-getters (cdr gen-getters)))
                                                 (lp-getters (cdr gen-getters)))))
                                       #`(if #,(car termination-checks)
                                             #,(car call/finish-proc)
                                             #,(lp-checks (cdr termination-checks))))))
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
                     |  <guard-clasue>
                     |  <index-clause>      ; TODO for* can have local indices
                     |  <bind-clause>
                     |  <stop-clause>       ; only in for*, stop the entire iter
  |#
  (define-syntax for*/fold
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
        [(_ (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init        (hashtable-ref fragments 'init #f)]
                [frag-index       (hashtable-ref fragments 'index #f)]
                [frag-finish      (hashtable-ref fragments 'finish #f)]
                [frag-iter/config (hashtable-ref fragments 'iter/config #f)])
           (with-syntax ([(t-acc t-finish) (generate-temporaries '(t-acc t-finish))])
             (let* ([init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[v e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[v 0]])
                                   #f)]
                    [init-var (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'v])
                                  #f)]
                    [index-var (if frag-index
                                   (syntax-case frag-index () [(_ v) #'v])
                                   #f)]
                    [init/index-vars (let* ([vars (if frag-index (list index-var)    '())]
                                            [vars (if frag-init  (cons init-var vars) vars)])
                                       vars)]
                    ;; (((gen-preloop loop-var tycheck term-check update gen-getter) config0 config1 ...) ...)
                    [iters/configs (map (lambda (i/c)
                                          (cons (process-iter-clause (car i/c))
                                                (cdr i/c)))
                                        frag-iter/config)]
                    [call/finish-proc (if frag-finish
                                          (let ([params '()])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@params)
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               ;; (println "for* -------------------------")
               ;; (for-each displayln iters/configs)
               ;; (printf "index-def: ~a~n" index-def)
               ;; (printf "index-var: ~a~n" index-var)
               ;; (println "for* -------------------------")
               #`(let ([t-finish #,(cadr call/finish-proc)])
                   #,(let lp-for-loop ([i/c* iters/configs] [last-forloop #f] [last-update #f])
                     (let ([call-lastforloop #`(#,last-forloop #,@init/index-vars #,last-update)])
                       (if (null? i/c*)
                           (let ([call-loop (lambda (init?)
                                              (if frag-index
                                                  #`(#,last-forloop #,@init? (fx1+ #,index-var) #,last-update)
                                                  #`(#,last-forloop #,@init?                    #,last-update)))])
                             (if frag-init
                                 #`(let ([t-acc (begin body* ...)])
                                     #,(call-loop (list #'t-acc)))
                                 #`(begin body* ...
                                          #,(call-loop '()))))
                           (with-syntax ([(forloop) (generate-temporaries '(forloop))])
                             (let* ([i/c (car i/c*)] [iter-cl (car i/c)] [configs (cdr i/c)]
                                    [gen-preloop       (or (car iter-cl) (lambda (e) e))]
                                    [loop-var          (list-ref iter-cl 1)]
                                    [termination-check (list-ref iter-cl 2)]
                                    [update            (list-ref iter-cl 3)]
                                    [gen-getter        (list-ref iter-cl 4)]
                                    [loop-var
                                     (let ([vars (if last-forloop
                                                     (let* ([vars (if frag-index (list #`[#,index-var #,index-var])     '())]
                                                            [vars (if frag-init  (cons #`[#,init-var  #,init-var] vars) vars)])
                                                       vars)
                                                     (let* ([vars (if frag-index (list index-def)     '())]
                                                            [vars (if frag-init  (cons init-def vars) vars)])
                                                       vars))])
                                       `(,@vars ,loop-var))])
                               ;; if current level terminates, call forloop of last level
                               (gen-preloop
                                #`(let forloop (#,@loop-var)
                                   (if #,termination-check
                                       ;; continue on the outer loop
                                       #,(if last-forloop
                                             call-lastforloop
                                             ;; already outermost loop, stop
                                             (car call/finish-proc))
                                       #,(let ([gen-getter (lambda (e) (if gen-getter (gen-getter e) e))])
                                           (gen-getter
                                            (let lp-configs ([configs (if configs configs '())])
                                              (if (null? configs)
                                                  (lp-for-loop (cdr i/c*) #'forloop update)
                                                  (let ([conf (car configs)])
                                                    (syntax-case conf ()
                                                      [(:break e)
                                                       (kw:break? #':break)
                                                       #`(if e
                                                             #,(if last-forloop
                                                                   call-lastforloop
                                                                   ;; already outermost loop, stop
                                                                   #`(void))
                                                             #,(lp-configs (cdr configs)))]
                                                      [(:stop e1 e2)
                                                       (kw:stop? #':stop)
                                                       #`(if e1
                                                             e2
                                                             #,(lp-configs (cdr configs)))]
                                                      [(:guard e)
                                                       (kw:guard? #':guard)
                                                       #`(if e
                                                             #,(lp-configs (cdr configs))
                                                             (forloop #,@init/index-vars #,update))]
                                                      [(:let v e)
                                                       (kw:let? #':let)
                                                       #`(let ([v e])
                                                           #,(lp-configs (cdr configs)))]
                                                      [(:let-values (v* ...) e)
                                                       (kw:let-values? #':let-values)
                                                       #`(let-values ([(v* ...) e])
                                                           #,(lp-configs (cdr configs)))]
                                                      [_ (syntax-error conf "unknown config clause:")]))))))))))))))))))])))



  #|
(<for> (<index-clause>?
        <iter-clause>+
        <for-config-clause>*)
  <body>+)
  |#
  (define-syntax for
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start index ,handle-index)
         `(start e0    epsilon)

         `(index iter0 ,handle-iter)
         `(e0    iter0 ,handle-iter)

         `(iter0 iter ,handle-iter)
         `(iter0 e1   epsilon)

         `(iter iter   ,handle-iter)
         `(iter config ,handle-config)
         `(iter e2     epsilon)

         `(e1 config ,handle-config)
         `(e1 e2     epsilon)

         `(config config ,handle-config)
         `(config end    epsilon)

         `(e2 end epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let ([_ (handler (make-eq-hashtable) #'(cl* ...))])
           #'(for/fold (cl* ...) body* ...))])))


  #|doc
(for/list (<index-clause>?
           <length-clause>?
           <for-config-clause>*)
  <body>+)
  |#
  (define-syntax for/list
    (lambda (stx)
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         #'(let ([lb (make-list-builder)])
             (for (cl* ...)
               (lb (begin body* ...)))
             (lb))])))


  #|doc
(for/vector (<index-clause>?
             <length-clause>?
             <fill-clause>?
             <iter-clause>+
             <for-config-clause>*)
  <body>+)

<length-clause> := [:length <expr>] ; <expr> must evaluate to a non-negative fixnum
<fill-clause>   := [:fill   <expr>]
  |#
  (define-syntax for/vector
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start index ,handle-index)
         `(start e0    epsilon)

         `(index length ,handle-length)
         `(index e1     epsilon)

         `(e0 length ,handle-length)
         `(e0 e1     epsilon)

         `(length fill ,handle-fill)
         `(length e2   epsilon)

         `(e1 fill ,handle-fill)
         `(e1 e2   epsilon)

         `(fill iter0 ,handle-iter)
         `(e2   iter0 ,handle-iter)

         `(iter0 iter ,handle-iter)
         `(iter0 e3   epsilon)

         `(iter iter   ,handle-iter)
         `(iter config ,handle-config)
         `(iter e4     epsilon)

         `(e3 config ,handle-config)
         `(e3 e4     epsilon)

         `(config config ,handle-config)
         `(config end    epsilon)

         `(e4 end epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments  (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-len   (hashtable-ref fragments 'length #f)]
                [frag-index (hashtable-ref fragments 'index  #f)]
                [frag-fill  (hashtable-ref fragments 'fill   #f)]
                ;; feed the clauses w/o for/vector extensions
                [cl* (filter id `(,(hashtable-ref fragments 'index  #f)
                                  ;; TODO what if there's no iter clauses?
                                  ,@(hashtable-ref fragments 'iter '(#f))
                                  ,@(hashtable-ref fragments 'config  '())))])
           ;; (println "len: ~a~ncl* ~a" frag-len cl*)
           (if frag-len
               #`(let ([len #,(syntax-case frag-len () [(_ l) #'l])])
                   (unless (and (fixnum? len) (nonnegative? len))
                     (errorf 'for/vector "length is not a nonnegative fixnum: ~a" len))
                   (let ([vec (make-vector len
                                           #,(if frag-fill
                                                 (syntax-case frag-fill () [(_ v) #'v])
                                                 #'#f))]
                         [position 0])
                     (unless (fx= len 0)
                       ;; Escape all loop levels as soon as the vector is full.
                       (call/1cc
                        (lambda (finish)
                          (for (#,@cl*)
                            (vector-set! vec position (begin body* ...))
                            (set! position (fx1+ position))
                            (when (fx= position len)
                              (finish (void)))))))
                     vec))
               #`(let ([lb (make-list-builder)])
                   (for (#,@cl*)
                     (lb (begin body* ...)))
                   (list->vector (lb)))))])))


  #|doc
(for* (<index-clause>?
       <for*-clause>+)
   <body>+)
  |#
  (define-syntax for*
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start index ,handle-index)
         `(start e0    epsilon)

         `(index iter0 ,handle-iter/config-iter)
         `(e0    iter0 ,handle-iter/config-iter)

         `(iter0 config0 ,handle-iter/config-config)
         `(iter0 e1       epsilon)

         `(config0 config0 ,handle-iter/config-config)
         `(config0 e2      epsilon)

         `(e1 e2   epsilon)
         `(e2 iter ,handle-iter/config-iter)
         `(e2 end  epsilon)

         `(iter config ,handle-iter/config-config)
         `(iter iter   ,handle-iter/config-iter)
         `(iter end    epsilon)

         `(config config ,handle-iter/config-config)
         `(config iter   ,handle-iter/config-iter)
         `(config end    epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let ([_ (handler (make-eq-hashtable) #'(cl* ...))])
           #'(for*/fold (cl* ...) body* ...))])))


  #|doc
(for*/vector (<index-clause>?
              <for*-clause>+)
  <body>+)
  |#
  (define-syntax for*/list
    (lambda (stx)
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         #'(let ([lb (make-list-builder)])
             (for* (cl* ...)
               (lb (begin body* ...)))
             (lb))])))


  #|doc
(for*/vector (<index-clause>?
              <length-clause>?
              <fill-clause>?
              <for*-clause>+)
  <body>+)
  |#
  (define-syntax for*/vector
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start index ,handle-index)
         `(start e0    epsilon)

         `(index length ,handle-length)
         `(index e1     epsilon)

         `(e0 length ,handle-length)
         `(e0 e1     epsilon)

         `(length fill ,handle-fill)
         `(length e2   epsilon)

         `(e1 fill ,handle-fill)
         `(e1 e2   epsilon)

         `(fill iter0 ,handle-iter/config-iter)
         `(e2   iter0 ,handle-iter/config-iter)

         `(iter0 config0 ,handle-iter/config-config)
         `(iter0 e3       epsilon)

         `(config0 config0 ,handle-iter/config-config)
         `(config0 e4      epsilon)

         `(e3 e4   epsilon)
         `(e4 iter ,handle-iter/config-iter)
         `(e4 end  epsilon)

         `(iter config ,handle-iter/config-config)
         `(iter iter   ,handle-iter/config-iter)
         `(iter end    epsilon)

         `(config config ,handle-iter/config-config)
         `(config iter   ,handle-iter/config-iter)
         `(config end    epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments  (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-len   (hashtable-ref fragments 'length #f)]
                [frag-index (hashtable-ref fragments 'index  #f)]
                [frag-fill  (hashtable-ref fragments 'fill   #f)]
                ;; feed the clauses w/o for/vector extensions
                [cl* (filter id `(,(hashtable-ref fragments 'index #f)
                                  ,@(apply append
                                           (hashtable-ref fragments 'iter/config '(#f)))))])
           ;; (println "len: ~a~ncl* ~a" frag-len cl*)
           (if frag-len
               #`(let ([len #,(syntax-case frag-len () [(_ l) #'l])])
                   (unless (and (fixnum? len) (nonnegative? len))
                     (errorf 'for*/vector "length is not a nonnegative fixnum: ~a" len))
                   (let ([vec (make-vector len
                                           #,(if frag-fill
                                                 (syntax-case frag-fill () [(_ v) #'v])
                                                 #'#f))]
                         [position 0])
                     (unless (fx= len 0)
                       ;; Escape all loop levels as soon as the vector is full.
                       (call/1cc
                        (lambda (finish)
                          (for* (#,@cl*)
                            (vector-set! vec position (begin body* ...))
                            (set! position (fx1+ position))
                            (when (fx= position len)
                              (finish (void)))))))
                     vec))
               #`(let ([lb (make-list-builder)])
                   (for* (#,@cl*)
                     (lb (begin body* ...)))
                   (list->vector (lb)))))])))

  )
