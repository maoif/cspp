(library (cspp concurrency)
  (export abox abox?
          unabox abox-get
          abox-set! set-abox! abox-cas!
          abox-get&add! abox-get&add1! abox-get&sub! abox-get&sub1!
          abox-add&get! abox-add1&get! abox-sub&get! abox-sub1&get!

          make-spinlock spinlock? spinlock-acquire spinlock-release with-spinlock spinlock-name

          define-threaded
          run-threads spawn-thread
          make-threadpool threadpool-add! threadpool-shutdown!

          future? future-get future-complete?)
  (import (cspp chez)
          (cspp list)
          (cspp hashset)
          (cspp system)
          (only (cspp utils) define-who natural? pcheck positive-natural?)
          (cspp private common))


  #|doc
  Use at most `n` threads to run the thunks.
  If `n` is 0, all thunks are run in a separate thread.

  Return a *vector* consisting of the results produced by the thunks,
  in the same order as the thunks in the arguments.
  If exceptions are raised in any of the thunks, the raised exception (condition object)
  is stored in the result vector instead of the computation result.
  |#
  (define-who run-threads
    (lambda (n . thunks)
      (pcheck ([natural? n])
              (unless (andmap procedure? thunks)
                (errorf who "arguments contain non-procedures"))
              (unless (null? thunks)
                (let* ([len (length thunks)] [res* (make-vector len (void))])
                  (define gen-worker
                    (lambda (i+th*)
                      (lambda ()
                        (let loop ([p* i+th*])
                          (unless (null? p*)
                            (vector-set! res* (caar p*)
                                         ;; TODO why do I need call/1cc?
                                         (call/1cc
                                          (lambda (k)
                                            (with-exception-handler
                                                (lambda (con) (k con))
                                              (lambda () ((cdar p*)))))))
                            (loop (cdr p*)))))))
                  (if (or (fx= 0 n) (fx<= len n))
                      ;; spawn all threads immediately
                      (let ([t* (map/i (lambda (i th) (fork-thread (gen-worker (list (cons i th)))))
                                       thunks)])
                        (for-each thread-join t*)
                        res*)
                      ;; divide `len` thunks into `n` groups
                      (let-values ([(q r) (div-and-mod len n)])
                        (let next ([th* thunks] [work** '()] [nn n] [i 0])
                          (if (fx= nn 0)
                              (let ([work** (if (null? th*)
                                                work**
                                                ;; add remaining thunks to the last group
                                                (cons (append
                                                       (map cons (list-tail (iota len) i) th*)
                                                       (car work**))
                                                      (cdr work**)))])
                                (let ([t* (map fork-thread (map gen-worker work**))])
                                  (for-each thread-join t*)
                                  res*))
                              (let loop ([th* th*] [w* '()] [q q] [i i])
                                (if (fx= q 0)
                                    (next th* (cons w* work**) (fx1- nn) i)
                                    (loop (cdr th*)
                                          (cons (cons i (car th*)) w*)
                                          (fx1- q)
                                          (fx1+ i)))))))))))))



  ;; from ChezScheme cmacros.ss
  #|doc
  Define thread-local variables.
  Thread-local variables defined using this macro can be used just as ordinary ones.

  Usage:
  Define a thread-local variable with its symbol name as default value:
  (define-threaded v)

  Define a thread-local variable with a given default value:
  (define-threaded v 10)

  Update the thread-local variable to 42:
  (set! v 42)
  |#
  (define-syntax define-threaded
    (syntax-rules ()
      [(_ var) (define-threaded var 'var)]
      [(_ var expr)
       (begin
         (define tmp (make-thread-parameter expr))
         (define-syntax var
           (identifier-syntax
            (id (tmp))
            ((set! id val) (tmp val)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   atomics
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; abox: atomic box

  ;; TODO need fxbox and fxabox? Or set-fixnum!?
  ;; TODO use atomic instructions.
  ;; Not easy, atomic ones (ftype-locked-incr!, ftype-locked-decr!)
  ;; operates on raw memory data directly.

  (define-record-type ($abox mk-abox abox?)
    (nongenerative) (opaque #t)
    (fields (immutable bx abox-bx)))


  #|doc
  Return a newly allocated atomic box and initialize its value to 0.
  If a value `v` is given, it is used as the initialization value.
  |#
  (define-who abox
    (case-lambda
      [()  (abox 0)]
      [(v) (mk-abox (box v))]))

  (define $abox-get
    (lambda (who a)
      (pcheck ([abox? a])
              ;; correct?
              (unbox (abox-bx a)))))

  #|doc
  Return the current value stored in the abox.
  |#
  (define-who abox-get
    (lambda (a)
      ($abox-get who a)))

  #|doc
  Same as `abox-get`.
  |#
  (define-who unabox
    (lambda (a)
      ($abox-get who a)))


  (define $abox-set!
    (lambda (who a v)
      (pcheck ([abox? a])
              (let ([bx (abox-bx a)])
                (memory-order-acquire)
                (set-box! bx v)
                (memory-order-release)))))

  #|doc
  Set the value of the abox `a` to the value `v`.
  |#
  (define-who abox-set!
    (lambda (a v)
      (pcheck ([abox? a])
              ($abox-set! who a v))))

  #|doc
  Same as `abox-set!`.
  |#
  (define-who set-abox!
    (lambda (a v)
      (pcheck ([abox? a])
              ($abox-set! who a v))))


  ;; `proc` must be side-effect free since it may be called
  ;; multiple times to attempt to update the value.
  ;; return old value
  (define $abox-get&update!
    (lambda (who a proc)
      (pcheck ([abox? a] [procedure? proc])
              (let ([bx (abox-bx a)])
                (let loop ()
                  (let* ([old (unbox bx)] [new (proc old)])
                    (if (box-cas! bx old new)
                        old
                        (loop))))))))
  ;; return new value
  (define $abox-update&get!
    (lambda (who a proc)
      (pcheck ([abox? a] [procedure? proc])
              (let ([bx (abox-bx a)])
                (let loop ()
                  (let* ([old (unbox bx)] [new (proc old)])
                    (if (box-cas! bx old new)
                        new
                        (loop))))))))

  #|doc
  Apply the procedure `proc` to the value currently stored in the abox `a`.
  The abox's value is then atomically updated to `proc`'s return value,
  and the old value is returned.

  Procedure `proc` may be invoked more than once, so its side effects,
  if any, have to be carefully managed.
  |#
  (define-who abox-get&update!
    (lambda (a proc)
      ($abox-get&update! who a proc)))


  #|doc
  Apply the procedure `proc` to the value currently stored in the abox `a`.
  The abox's value is then atomically updated to `proc`'s return value,
  and the new value is returned.

  Procedure `proc` may be invoked more than once, so its side effects,
  if any, have to be carefully managed.
  |#
  (define-who abox-update&get!
    (lambda (a proc)
      ($abox-update&get! who a proc)))


  #|doc
  Atomically add `x` to the value stored in the abox `a` and return the old value.
  |#
  (define-who abox-get&add!
    (lambda (a x)
      ($abox-get&update! who a (lambda (old) (+ old x)))))

  #|doc
  Atomically add 1 to the value stored in the abox `a` and return the old value.
  |#
  (define-who abox-get&add1!
    (lambda (a)
      ($abox-get&update! who a (lambda (old) (+ old 1)))))

  #|doc
  Atomically subtract `x` from the value stored in the abox `a` and return the old value.
  |#
  (define-who abox-get&sub!
    (lambda (a x)
      ($abox-get&update! who a (lambda (old) (- old x)))))

  #|doc
  Atomically subtract 1 from the value stored in the abox `a` and return the old value.
  |#
  (define-who abox-get&sub1!
    (lambda (a)
      ($abox-get&update! who a (lambda (old) (- old 1)))))


  #|doc
  Atomically add `x` to the value stored in the abox `a` and return the new value.
  |#
  (define-who abox-add&get!
    (lambda (a x)
      ($abox-update&get! who a (lambda (old) (+ old x)))))

  #|doc
  Atomically add 1 to the value stored in the abox `a` and return the new value.
  |#
  (define-who abox-add1&get!
    (lambda (a)
      ($abox-update&get! who a (lambda (old) (+ old 1)))))

  #|doc
  Atomically subtract `x` from the value stored in the abox `a` and return the new value.
  |#
  (define-who abox-sub&get!
    (lambda (a x)
      ($abox-update&get! who a (lambda (old) (- old x)))))

  #|doc
  Atomically subtract 1 from the value stored in the abox `a` and return the new value.
  |#
  (define-who abox-sub1&get!
    (lambda (a)
      ($abox-update&get! who a (lambda (old) (- old 1)))))


  #|doc
  Atomically try to replace the old value in the abox with the new value.
  If the abox contains the old value `old` and the operation succeeds,
  `new` is stored in the abox and `old` is returned.
  If the procedure returns any value other than `old`, the operation fails.
  |#
  (define-who abox-cas!
    (lambda (a old new)
      (pcheck ([abox? a])
              (let ([bx (abox-bx a)])
                (let spin ()
                  (let ([v (unbox bx)])
                    (if (eq? v old)
                        (if (box-cas! bx old new)
                            ;; make sure other threads see the new value
                            (begin (memory-order-release)
                                   old)
                            (spin))
                        v)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   locks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; spin lock


  (define-record-type (spinlock mk-spinlock spinlock?)
    (nongenerative) (opaque #t)
    (fields (immutable name) (mutable bx)))


  #|doc
  Return a newly constructed spinlock.
  `name`, if given, must be a symbol, and is used to identify the spinlock.
  The name is printed every time the spinlock is printed, which is useful for debugging.
  |#
  (define make-spinlock
    (case-lambda
      [()
       (mk-spinlock #f (box 0))]
      [(name)
       (pcheck ([symbol? name])
               (mk-spinlock name (box 0)))]))


  #|doc
  Acquire the spinlock `lock`; busy-loop if it is currently held by other threads.
  |#
  (define spinlock-acquire
    (lambda (lock)
      (pcheck ([spinlock? lock])
              (let ([bx (spinlock-bx lock)])
                (let spin ([i 0])
                  #;
                  (when (and (> i 0) (= 0 (modulo i #e1e9)))
                    (printf "spun ~a times [~a]~n" i lock))
                  (if (fx= 0 (unbox bx))
                      (let ([res (box-cas! bx 0 1)])
                        (if res
                            (begin (memory-order-acquire) #t)
                            (spin (fx1+ i))))
                      (spin (fx1+ i))))))))


  #|doc
  Release the spinlock `lock`.
  |#
  (define spinlock-release
    (lambda (lock)
      (pcheck ([spinlock? lock])
              (memory-order-release)
              (set-box! (spinlock-bx lock) 0))))


  #|doc
  `with-spinlock` evaluates the expression `splock`, which must evaluate to a spinlock, acquires
  the spinlock, evaluates the expressions e1, e2, ..., and releases the spinlock. The spinlock is
  released whether the body returns normally or via a control operation (that is, throw
  to a continuation, perhaps because of an error) that results in a nonlocal exit from the
  `with-spinlock` form. If control subsequently returns to the body via a continuation invocation,
  the spinlock is reacquired.

  The return value of the `with-spinlock` form is the value of the last expression in the expressions e1, e2, ...
  |#
  (define-syntax with-spinlock
    (syntax-rules ()
      ((_ splock e1 e2 ...)
       (let ([m splock])
         (pcheck ([spinlock? m])
                 (dynamic-wind
                   (lambda () (spinlock-acquire m))
                   (lambda () e1 e2 ...)
                   (lambda () (spinlock-release m))))))))



;;;; read-write lock

  ;; TODO use libc or not?


  (define-record-type (rwlock mk-rwlock rwlock?)
    (nongenerative) (opaque #t)
    (fields))


  (define-who make-rwlock
    (lambda () (todo)))


  (define-who rwlock-read-acquire
    (case-lambda
      [(lock) (rwlock-read-acquire lock #t)]
      [(lock block?) (todo)]))


  (define-who rwlock-read-release
    (lambda (lock) (todo)))


  (define-who rwlock-write-acquire
    (case-lambda
      [(lock) (rwlock-write-acquire lock #t)]
      [(lock block?) (todo)]))


  (define-who rwlock-write-release
    (lambda (lock) (todo)))


  (define-syntax with-rwlock-read
    (syntax-rules ()
      ((_ m-expr e1 e2 ...)
       (let ([m m-expr])
         (dynamic-wind
           (lambda () (rwlock-read-acquire m))
           (lambda () e1 e2 ...)
           (lambda () (rwlock-read-release m)))))))


  (define-syntax with-rwlock-write
    (syntax-rules ()
      ((_ m-expr e1 e2 ...)
       (let ([m m-expr])
         (dynamic-wind
           (lambda () (rwlock-write-acquire m))
           (lambda () e1 e2 ...)
           (lambda () (rwlock-write-release m)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   barrier
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-record-type (barrier mk-barrier barrier?)
    (nongenerative) (opaque #t)
    (fields))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   blocking queue
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Follow Java's LinkedBlockingQueue

  (define-record-type (bqueue mk-bqueue bqueue?)
    (nongenerative) (opaque #t)
    (fields (immutable cap)
            ;; abox
            (immutable count)
            (immutable lock-get)
            (immutable lock-put)
            (immutable cond-not-full)
            (immutable cond-not-empty)
            ;; invariant: (car head) => '()
            (mutable head)
            ;; invariant: (cdr tail) => '()
            (mutable tail)))


  (define-who make-bqueue
    (case-lambda
      [() (make-bqueue (most-positive-fixnum))]
      [(cap)
       (pcheck ([positive-natural? cap])
               (let* ([head-tail (cons '() '())]
                      [bq (mk-bqueue cap (abox 0)
                                     (make-mutex 'bqueue-get) (make-mutex 'bqueue-put)
                                     (make-condition 'bqueue-not-full) (make-condition 'bqueue-not-empty)
                                     head-tail head-tail)])
                 bq))]))


  (define bqueue-enq!
    (lambda (who bq x)
      (let ([n (list x)] [tail (bqueue-tail bq)])
        (set-cdr! tail n)
        (bqueue-tail-set! bq n))))


  (define bqueue-deq!
    (lambda (who bq)
      (let* ([head (bqueue-head bq)] [rest (cdr head)]
             [v (car rest)])
        (bqueue-head-set! bq rest)
        (set-car! rest '())
        v)))


  ;; acquires lock-put
  ;; wait for not-full condition
  ;; signal not-empty condition
  (define-who bqueue-put!
    (lambda (bq v)
      (pcheck ([bqueue? bq])
              (let* ([lock-put (bqueue-lock-put bq)]
                     [lock-get (bqueue-lock-get bq)]
                     [cond-not-full (bqueue-cond-not-full bq)]
                     [cond-not-empty (bqueue-cond-not-empty bq)]
                     [cap (bqueue-cap bq)]
                     [ctr (bqueue-count bq)] [c (unabox ctr)]
                     [await-not-full (lambda () (let loop ()
                                                  (let ([c (unabox ctr)])
                                                    (when (fx= c cap)
                                                      (condition-wait cond-not-full lock-put)
                                                      (loop)))))]
                     [signal-not-empty (lambda () (with-mutex lock-get
                                                    (condition-signal cond-not-empty)))])
                (with-mutex lock-put
                  (await-not-full)
                  (bqueue-enq! who bq v)
                  (let ([c (abox-get&add1! ctr)])
                    (when (fx< (fx1+ c) cap) (condition-signal cond-not-full))))
                (when (fx= 0 c) (signal-not-empty))))))


  ;; acquires lock-get
  ;; wait for not-empty condition
  ;; signal not-full condition
  (define-who bqueue-get
    (lambda (bq)
      (pcheck ([bqueue? bq])
              (let* ([lock-put (bqueue-lock-put bq)]
                     [lock-get (bqueue-lock-get bq)]
                     [cond-not-full (bqueue-cond-not-full bq)]
                     [cond-not-empty (bqueue-cond-not-empty bq)]
                     [cap (bqueue-cap bq)]
                     [ctr (bqueue-count bq)] [c (unabox ctr)]
                     [await-not-empty (lambda () (let loop ()
                                                   (let ([c (unabox ctr)])
                                                     (when (fx= c 0)
                                                       (condition-wait cond-not-empty lock-get)
                                                       (loop)))))]
                     [signal-not-full (lambda () (with-mutex lock-put
                                                   (condition-signal cond-not-full)))])
                (with-mutex lock-get
                  (await-not-empty)
                  (let ([v (bqueue-deq! who bq)] [c (abox-get&sub1! ctr)])
                    (when (fx> c 1) (condition-signal cond-not-empty))
                    (when (fx= c cap) (signal-not-full))
                    v))))))


  ;; bqueue-try-get
  ;; bqueue-try-put!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   threadpool & future
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-record-type (threadpool mk-threadpool threadpool?)
    (nongenerative) (opaque #t)
    (fields (immutable name) (mutable size) (immutable workers) (immutable rq)
            ;; an abox, currently two states:
            ;; running: Accept new tasks and process queued tasks
            ;; shutdown: Don't accept new tasks, but process queued tasks
            (immutable state)))

  (define-record-type (future mk-future future?)
    (nongenerative) (opaque #t)
    (fields (immutable thunk) (mutable result) (immutable mtx) (immutable cond)))

  (define *tp-count* (abox 0))
  (define *signal-shutdown* (list))

  (define tps-running  0)
  (define tps-shutdown 1)

  (define tp-shutdown? (lambda (s) (fx= tps-shutdown (unabox s))))


  #|doc
  Construct a threadpool for parallel task execution.
  By default, the number of worker threads created is equal to the number of available CPU cores.

  `size`, if given, must be a postive natural number
  specifying the number of worker threads in the threadpool.

  `name`, if given, must be a symbol, and will be used to identify the threadpool.
  |#
  (define-who make-threadpool
    (case-lambda
      [()
       (make-threadpool (cpu-count)
                        (string->symbol (format "threadpool-~a" (abox-get&add1! *tp-count*))))]
      [(size)
       (make-threadpool size
                        (string->symbol (format "threadpool-~a" (abox-get&add1! *tp-count*))))]
      [(size name)
       (pcheck ([positive-natural? size] [symbol? name])
               (let* ([workers (make-eq-hashset)] [rq (make-bqueue)] [state (abox tps-running)]
                      [tp (mk-threadpool name size workers rq state)])
                 (define worker
                   (lambda (i)
                     (lambda ()
                       (let loop ()
                         (let ([task (bqueue-get rq)])
                           (unless (eq? task *signal-shutdown*)
                             (let ([res
                                    (call/1cc
                                     (lambda (k)
                                       (with-exception-handler
                                           (lambda (con) (k con))
                                         (lambda () ((future-thunk task))))))])
                               (with-mutex (future-mtx task)
                                 (future-result-set! task res))
                               (condition-broadcast (future-cond task))
                               (loop))))))))
                 (for-each (lambda (i) (hashset-add! workers (fork-thread (worker i)))) (iota size))
                 tp))]))


  #|doc
  Add the thunk `th` to the threadpool `tp` to be scheduled to run later.
  Return a future object that can be used to retrieve the result of the computation.
  |#
  (define-who threadpool-add!
    (lambda (tp th)
      (pcheck ([threadpool? tp] [procedure? th])
              (let ([s (threadpool-state tp)])
                (when (tp-shutdown? s)
                  (errorf who "threadpool ~a is already shut down" (threadpool-name tp)))
                (let ([t (mk-future th *future-nil*
                                    (make-mutex 'future-mutex)
                                    (make-condition 'future-condition))])
                  (bqueue-put! (threadpool-rq tp) t)
                  t)))))


  (define-who threadpool-set-exception-handler!
    (lambda (tp handler)
      (todo)))


  #|doc
  Shutdown the threadpool `tp`.
  After shutdown, the threadpool stops receiving new tasks via `threadpool-add!`.
  Any subsequent calls to `threadpool-add!` will be ignored and #f is returned.

  Tasks already added will continue to be executed.

  `threadpool-shutdown!` blocks until all remaining tasks are executed.
  |#
  (define-who threadpool-shutdown!
    (case-lambda
      [(tp) (threadpool-shutdown! tp #f)]
      [(tp await?)
       (pcheck ([threadpool? tp] [boolean? await?])
               (let ([s (threadpool-state tp)] [workers (threadpool-workers tp)]
                     [rq (threadpool-rq tp)])
                 (when (tp-shutdown? s)
                   (errorf who "threadpool ~a is already shut down" (threadpool-name tp)))
                 (abox-set! s tps-shutdown)
                 (for-each (lambda (x) (bqueue-put! rq *signal-shutdown*)) (iota (hashset-size workers)))
                 (when await? (hashset-for-each (lambda (w) (thread-join w)) workers))))]))


  (define *default-threadpool* (make-threadpool (cpu-count) 'default-threadpool))


  #|doc
  Create a new thread in the default global threadpool to run the thunk `th`.
  This procedure returns a future object to retrieve the result of the thunk.
  |#
  (define-who spawn-thread
    (lambda (th)
      (pcheck ([procedure? th])
              (threadpool-add! *default-threadpool* th))))


  (define *future-nil* (mk-future #f #f #f #f))

  #|doc
  Check if the value is nil future.
  `future-try-get` may return this value if a future has not completed yet.
  |#
  (define future-nil? (lambda (x) (eq? x *future-nil*)))


  ;; TODO timeout?
  #|doc
  Get the value of the future; block if the value is not available yet.
  |#
  (define-who future-get
    (lambda (fut)
      (pcheck ([future? fut])
              (let ([mtx (future-mtx fut)] [cond (future-cond fut)])
                (with-mutex mtx
                  (let loop ()
                    (let ([res (future-result fut)])
                      (if (future-nil? res)
                          (begin (condition-wait cond mtx)
                                 (loop))
                          res))))))))


  #|doc
  Try to get the value of the future.

  If the value is not available (e.g., the task is still running),
  this procedure returns a nil future object such that (future-nil? x) => #t.
  |#
  (define-who future-try-get
    (lambda (fut)
      (pcheck ([future? fut])
              (let ([res (future-result fut)])
                res))))


  #|doc
  Check if the future's task has completed.
  |#
  (define-who future-complete?
    (lambda (fut)
      (pcheck ([future? fut])
              (let ([res (future-result fut)])
                (not (future-nil? res))))))





  (record-writer (type-descriptor $abox)
                 (lambda (r p wr)
                   (display "#<abox " p)
                   (wr (unbox (abox-bx r)) p)
                   (display ">" p)))

  (record-writer (type-descriptor spinlock)
                 (lambda (r p wr)
                   (display "#<spinlock " p)
                   (when (spinlock-name r)
                     (wr (spinlock-name r) p)
                     (display " " p))
                   (wr (unbox (spinlock-bx r)) p)
                   (display ">" p)))

  (record-writer (type-descriptor threadpool)
                 (lambda (r p wr)
                   (todo)))

  (record-writer (type-descriptor future)
                 (lambda (r p wr)
                   (todo)))



  )
