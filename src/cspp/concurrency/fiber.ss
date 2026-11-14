(library (cspp concurrency fiber)
  (export run-fibers spawn-fiber
          fiber-quit fiber-yield

          event-never event-always
          event-wrap event-select event-sync
          net-operation-event
          net-operation-event-pool-metrics
          wrap-operation choice-operation perform-operation

          make-channel
          channel-get-event channel-put-event get-operation put-operation
          channel-get channel-put!
          get-message put-message

          make-fiber-mutex fiber-mutex-acquire fiber-mutex-release

          make-fiber-cvar fiber-cvar-wait fiber-cvar-signal fiber-cvar-broadcast

          with-timer-off with-fiber-mutex
          milisleep-event sleep-event fiber-milisleep fiber-sleep)
  (import (cspp chez)
          (only (cspp utils) define-who milisleep natural? pcheck)
          (cspp dlist)
          (cspp concurrency)
          (cspp system)
          (cspp io)
          (cspp private common)
          (cspp net operation)
          (cspp net private operation))

;;;;===----------------------------------------------------------------------===
;;;; Implementation origin and design references
;;;;===----------------------------------------------------------------------===

  ;; The event vocabulary and poll/register/commit protocol follow Concurrent ML:
  ;; https://cml.cs.uchicago.edu/
  ;; The original cvar implementation also compared its API with PCML and Guile
  ;; Fibers. Here a cvar wait explicitly releases and reacquires a caller-owned mutex.
  ;; Guile Fibers reference: https://github.com/wingo/fibers
  ;; Tests adapted from the legacy `fiber.ss.bak` include examples labeled as coming
  ;; from Loko and Guile Fibers benchmarks; see the attribution in `tests/fiber.ss`.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   fiber core
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define *default-ticks* 30000)

  ;; Task dispatch and continuation resumption consume instruction ticks before
  ;; reaching user code. Keep a supported minimum above this overhead; otherwise
  ;; a worker can repeatedly preempt its own dispatch without advancing the fiber.
  (define *minimum-ticks* 100)

  ;; Chez continuations migrate between OS threads, but thread parameters do not.
  ;; Each worker keeps its scheduler parameter; its task slot is set before dispatch.
  (define-threaded current-scheduler #f)
  ;; Chez serializes thread-parameter writes through its global thread-context mutex.
  ;; Keep the frequently updated task in the worker-owned scheduler record instead.
  ;; Reading the scheduler and its task must be one preemption-free snapshot: a
  ;; continuation may migrate between those reads and otherwise see a source worker's task.
  (define-syntax current-fiber-task
    (identifier-syntax
      (id (with-timer-off
            (and current-scheduler (scheduler-task current-scheduler))))
      ((set! id value) (scheduler-task-set! current-scheduler value))))

  (define enqueue!
    (lambda (queue value)
      (dlist-add! queue value)))

  (define dequeue!
    (lambda (queue)
      (if (dlist-empty? queue)
          #f
          (let ([value (dlist-ref queue 0)])
            (dlist-delete! queue 0)
            value))))

  ;; One top-level `run-fibers` lifetime. `schedulers` is published before workers
  ;; start; `stop-flag` is atomic. `init-values` and `terminal-error` are read after
  ;; all workers join; `terminal-error?` distinguishes a raised `#f` from no error.
  ;; `park-mutex` protects sleeping transitions and the ordered `timers` list.
  ;; `sleepers` is an atomic hint: publication checks it after releasing its queue lock.
  ;; `next-deadline` is an atomic timer hint, so ordinary dispatch avoids the park lock.
  ;; `park-condition` wakes idle workers after publication, timer changes, or shutdown.
  ;; `worker-count` and `quantum` are immutable scheduler settings captured at entry.
  ;; `transactions` is an intrusive pending-transaction list under `transaction-mutex`.
  ;; Shutdown snapshots it before acquiring domains to detach registrations once.
  (define-record-type fiber-run-context
    (fields (mutable schedulers)
            stop-flag
            (mutable init-values)
            (mutable terminal-error?)
            (mutable terminal-error)
            worker-count quantum
            park-mutex park-condition
            sleepers next-deadline
            transaction-mutex (mutable transactions)
            (mutable timers)))

  ;; A `scheduler`'s `i` identifies its slot in `context`; `rq` holds runnable `fiber-task`
  ;; values. Only `rq-lock` protects queue access; it is released before taking park.
  ;; Slow workers inspect queues while holding park, so the reverse order is forbidden.
  ;; `running?` is visible to the network driver. `thread` is the OS thread joined on shutdown;
  ;; `thread-id` verifies ownership despite inherited parameters. `k` is the current
  ;; `scheduler` escape and may only be invoked on that `scheduler`'s own OS thread.
  ;; `task` is written only by this worker with preemption disabled. Other OS threads
  ;; may inherit `current-scheduler`, but must clear that reference without touching `task`.
  (define-record-type scheduler
    (fields i context rq rq-lock (mutable running?) (mutable thread)
            (mutable thread-id) (mutable k) (mutable task)))

  (define on-scheduler-thread?
    (lambda ()
      ;; `fork-thread` inherits thread parameters, including a parent's scheduler.
      ;; Only the OS thread actually running that `scheduler` may use its escapes
      ;; or current task token. Read `current-scheduler` afresh after migration.
      ;; Keep both IDs in one preemption-free snapshot: migration between the
      ;; reads would compare the old worker's ID with the resumed scheduler's ID.
      (with-timer-off
        (and current-scheduler
             (eqv? (get-thread-id) (scheduler-thread-id current-scheduler))))))

  ;; A schedulable slice of one logical fiber. `token` is its immutable mutex-owner
  ;; identity; `init?` determines whether completion stops the whole run. `thunk` is a
  ;; zero-argument entry procedure or a one-shot continuation-resumption closure.
  ;; Resumed tasks keep their original `token` and init status across OS threads.
  ;; `ticks` is the saved instruction budget, including zero for a disabled timer;
  ;; `#f` requests a fresh context quantum for a new task or automatic preemption.
  (define-record-type fiber-task
    (fields token init? thunk (mutable ticks)))

  (define context-stopped?
    (lambda (context)
      (unabox (fiber-run-context-stop-flag context))))

  (define scheduler-return
    (lambda (type value)
      ;; Never return to the `scheduler` captured on the continuation's source thread.
      ((scheduler-k current-scheduler) type value)))

  (define request-context-stop!
    (lambda (context error? error)
      (let ([registry (fiber-run-context-transaction-mutex context)]
            [park (fiber-run-context-park-mutex context)])
        ;; Mark stop and snapshot pending transactions under the registry lock.
        ;; Drop it before taking park or any event domain, avoiding lock inversions
        ;; with a committing transaction. Registrations recheck stop under registry.
        (mutex-acquire registry)
        (unless (context-stopped? context)
          (when error?
            (fiber-run-context-terminal-error?-set! context #t)
            (fiber-run-context-terminal-error-set! context error))
          (abox-set! (fiber-run-context-stop-flag context) #t))
        (let ([transactions
               (let loop ([transaction (fiber-run-context-transactions context)]
                          [result '()])
                 (if transaction
                     (loop (sync-transaction-next transaction) (cons transaction result))
                     result))])
          (mutex-release registry)
          (mutex-acquire park)
          (condition-broadcast (fiber-run-context-park-condition context))
          (mutex-release park)
          ;; Each cancellation resolves the latest root: a choice may have joined
          ;; this transaction's original domain with another one since registration.
          (for-each cancel-sync-transaction! transactions)))))

  #|macro:with-timer-off
  Evaluate expressions `expression ...` with instruction-count preemption disabled. Restore
  the previous timer on normal return or an exception, and return all expression values.
  Explicit yielding or blocking is allowed; the timer stays disabled after resumption.
  |#
  (define-syntax with-timer-off
    (syntax-rules ()
      [(_ expression ...)
       (let ([previous-timer (set-timer 0)])
         (call-with-values
          (lambda ()
            ;; Exception cleanup avoids `dynamic-wind` exit/entry on an ordinary fiber yield.
            (guard (condition
                    [else (set-timer previous-timer) (raise condition)])
              (begin expression ...)))
          (lambda results
            (set-timer previous-timer)
            (apply values results))))]))

  #|proc:fiber-quit
  The `fiber-quit` procedure terminates the current fiber without returning. When the init
  fiber quits, its entire run context stops and `run-fibers` returns an unspecified value.
  A child fiber quitting lets its init fiber and siblings continue.
  |#
  (define-who fiber-quit
    (lambda ()
      (pcheck ()
        (unless (on-scheduler-thread?)
          (errorf who "not running scheduler"))
        (set-timer 0)
        (when (fiber-task-init? current-fiber-task)
          (request-context-stop! (scheduler-context current-scheduler) #f #f))
        (scheduler-return 'quit #f))))

  #|proc:fiber-yield
  The `fiber-yield` procedure suspends the current fiber and requeues it to run later.
  When the fiber resumes, it returns an unspecified value.
  |#
  (define-who fiber-yield
    (lambda ()
      (pcheck ()
        ;; Suspension already needs a timer-disabled snapshot. Validate ownership
        ;; in that same snapshot instead of disabling/restoring the timer twice.
        (let ([ticks (set-timer 0)])
          (unless (and current-scheduler
                       (eqv? (get-thread-id) (scheduler-thread-id current-scheduler)))
            ;; An invalid caller still owns its timer; restore it before raising.
            (set-timer ticks)
            (errorf who "not running scheduler"))
          (fiber-task-ticks-set! current-fiber-task ticks)
          ;; The yielded continuation is resumed at most once.
          (call/1cc (lambda (resume) (scheduler-return 'yield resume)))))))

  (define put-task!
    (case-lambda
      [(task) (put-task! current-scheduler task)]
      [(sched task)
       (assert (= 0 (set-timer 0)))
       (let* ([context (scheduler-context sched)]
              [lock (scheduler-rq-lock sched)])
         (unless (context-stopped? context)
           ;; Publish under this queue's lock; no context-wide lock on the common path.
           (spinlock-acquire lock)
           (enqueue! (scheduler-rq sched) task)
           (spinlock-release lock)
           (memory-order-release)
           ;; Release publication before reading the sleep hint. A worker announces
           ;; its intent under park, then checks every queue before condition-wait.
           ;; Either that check sees this task, or we see its intent and signal under
           ;; park after condition-wait atomically releases park. No wakeup is lost.
           (when (fx> (unabox (fiber-run-context-sleepers context)) 0)
             (let ([mutex (fiber-run-context-park-mutex context)])
               (mutex-acquire mutex)
               (when (fx> (unabox (fiber-run-context-sleepers context)) 0)
                 (condition-signal (fiber-run-context-park-condition context)))
               (mutex-release mutex)))))]))

  ;; Queue consumers need only the selected queue's lock, including local dispatch.
  (define get-task!
    (lambda (sched)
      (let ([lock (scheduler-rq-lock sched)])
        (spinlock-acquire lock)
        (let ([task (dequeue! (scheduler-rq sched))])
          (spinlock-release lock)
          task))))

  (define get-or-steal-task!
    (lambda (sched)
      (or (get-task! sched)
          (let* ([schedulers (fiber-run-context-schedulers (scheduler-context sched))]
                 [count (vector-length schedulers)]
                 [start (scheduler-i sched)])
            ;; Rotate victims by worker identity so idle workers do not all start
            ;; at queue zero. Hold at most one queue lock at any time.
            (let loop ([offset 1])
              (and (fx< offset count)
                   (or (get-task! (vector-ref schedulers
                                             (fxmod (fx+ start offset) count)))
                       (loop (fx1+ offset)))))))))

  (define time-duration-expired?
    (lambda (duration)
      (let ([seconds (time-second duration)]
            [nanoseconds (time-nanosecond duration)])
        (or (< seconds 0)
            (and (= seconds 0) (<= nanoseconds 0))))))

  ;; A pending timer registration: `deadline` is an absolute monotonic time,
  ;; `transaction` owns its cancellation cleanup, and `resume` commits its event.
  ;; Timers live in their `context`'s ordered list under `park-mutex`. Expiration
  ;; removes entries there, then invokes `resume` under its transaction's domain.
  (define-record-type fiber-timer
    (fields deadline transaction resume))

  ;; The timer list is ordered by absolute monotonic deadline. The caller holds park.
  (define timer-before?
    (lambda (left right)
      (time-duration-expired?
       (time-difference (fiber-timer-deadline left) (fiber-timer-deadline right)))))

  (define update-timer-hint-locked!
    (lambda (context)
      (let ([timers (fiber-run-context-timers context)])
        (memory-order-release)
        (abox-set! (fiber-run-context-next-deadline context)
                   (and (pair? timers) (fiber-timer-deadline (car timers)))))))

  (define insert-timer-locked!
    (lambda (context timer)
      (let loop ([before '()] [rest (fiber-run-context-timers context)])
        (if (or (null? rest) (timer-before? timer (car rest)))
            (fiber-run-context-timers-set!
             context (append (reverse before) (cons timer rest)))
            (loop (cons (car rest) before) (cdr rest))))
      (update-timer-hint-locked! context)))

  (define remove-timer-locked!
    (lambda (context timer)
      (fiber-run-context-timers-set!
       context (remq timer (fiber-run-context-timers context)))
      (update-timer-hint-locked! context)))

  (define expire-timers-locked!
    (lambda (context now)
      (let loop ([rest (fiber-run-context-timers context)] [expired '()])
        (if (and (pair? rest)
                 (time-duration-expired?
                  (time-difference (fiber-timer-deadline (car rest)) now)))
            (loop (cdr rest) (cons (car rest) expired))
            (begin
              (fiber-run-context-timers-set! context rest)
              (update-timer-hint-locked! context)
              (reverse expired))))))

  ;; Expiry commits only after park is released; `event` protocol is acquired by caller.
  (define commit-expired-timers!
    (lambda (expired)
      (unless (null? expired)
        (for-each
         (lambda (timer)
           (with-event-protocol (sync-transaction-domain (fiber-timer-transaction timer))
             ((fiber-timer-resume timer))))
         expired))))

  (define await-task-slow!
    (lambda (sched)
      (let* ([context (scheduler-context sched)]
             [mutex (fiber-run-context-park-mutex context)]
             [condition (fiber-run-context-park-condition context)]
             [sleepers (fiber-run-context-sleepers context)])
        (mutex-acquire mutex)
        ;; Intent covers both the final queue scan and the atomic release-and-wait.
        ;; Producers can read this hint without park, but signal only while holding it.
        (abox-get&add1! sleepers)
        ;; Chez's release fence includes store-load ordering: the intent is visible
        ;; before our queue scan, pairing with the publisher's post-publication fence.
        (memory-order-release)
        (let loop ()
          (cond
           [(context-stopped? context)
            (abox-get&sub1! sleepers)
            (mutex-release mutex)
            #f]
           [else
            (let ([expired (expire-timers-locked! context (current-time 'time-monotonic))])
              (if (pair? expired)
                  (begin
                    ;; Timer commit can publish work; never enter event while holding park.
                    (mutex-release mutex)
                    (commit-expired-timers! expired)
                    (mutex-acquire mutex)
                    (loop))
                  (let ([task (get-or-steal-task! sched)])
                    (if task
                        (begin
                          (abox-get&sub1! sleepers)
                          (mutex-release mutex)
                          task)
                        (begin
                          (let ([timers (fiber-run-context-timers context)])
                            ;; Every wake rechecks stop, timers, and all runnable queues.
                            (if (null? timers)
                                (condition-wait condition mutex)
                                (let ([duration
                                       (time-difference
                                        (fiber-timer-deadline (car timers))
                                        (current-time 'time-monotonic))])
                                  (unless (time-duration-expired? duration)
                                    (condition-wait condition mutex duration)))))
                          (loop))))))])))))

  (define await-task!
    (lambda (sched)
      (let* ([context (scheduler-context sched)]
             [deadline (unabox (fiber-run-context-next-deadline context))])
        (memory-order-acquire)
        (cond
         [(context-stopped? context) #f]
         ;; The hint only chooses the slow path. Actual expiry always holds park.
         [(and deadline
               (time-duration-expired?
                (time-difference deadline (current-time 'time-monotonic))))
          (await-task-slow! sched)]
         [else (or (get-or-steal-task! sched) (await-task-slow! sched))]))))

  (define continue-task
    (lambda (task thunk)
      (make-fiber-task (fiber-task-token task) (fiber-task-init? task)
                       thunk (fiber-task-ticks task))))

  (define run-scheduler
    (lambda (sched)
      (let ([original-handler (timer-interrupt-handler)]
            [context (scheduler-context sched)])
        ;; A timer-handler setter takes Chez's global thread-context mutex.
        ;; Reuse one handler for this worker instead of installing and restoring
        ;; it on every slice. User code may replace it, so dispatch checks below.
        (define interrupt
          (lambda ()
            (set-timer 0)
            ;; `fork-thread` inherits this handler and `current-scheduler`, but
            ;; its timer belongs to a different OS thread. Validate the captured
            ;; worker before touching its task or invoking its one-shot escape.
            (if (and (eq? current-scheduler sched)
                     (eqv? (get-thread-id) (scheduler-thread-id sched)))
                (begin
                  (fiber-task-ticks-set! current-fiber-task #f)
                  ;; The interrupted continuation is resumed once.
                  (call/1cc (lambda (resume) (scheduler-return 'time resume))))
                (begin
                  ;; Restore the caller's handler on this foreign thread before
                  ;; invoking it: it may rearm the timer, raise, or fork again.
                  ;; An inherited ancestor handler delegates the same way, even
                  ;; after its worker has stopped or a nested context has joined.
                  (timer-interrupt-handler original-handler)
                  (original-handler)))))
        (set-timer 0)
        (let loop ()
          (let ([task (await-task! sched)])
            (when task
              (unless (context-stopped? context)
                (set! current-fiber-task task)
                (let-values ([(type value)
                              ;; Each `scheduler` escape is invoked exactly once.
                              (call/1cc
                               (lambda (return)
                                 (scheduler-k-set! sched return)
                                 ;; Reads are thread-local. Reinstall after a fiber changes
                                 ;; the handler; the normal path avoids the global mutex.
                                 (unless (eq? interrupt (timer-interrupt-handler))
                                   (timer-interrupt-handler interrupt))
                                 (with-exception-handler
                                     (lambda (condition)
                                       (set-timer 0)
                                       (scheduler-return 'error condition))
                                   (lambda ()
                                     ;; A suspended disabled scope remains disabled after
                                     ;; migration; sibling tasks use their own budgets.
                                     (set-timer (or (fiber-task-ticks task)
                                                    (fiber-run-context-quantum context)))
                                     ((fiber-task-thunk task))
                                     (set-timer 0)
                                     (scheduler-return 'complete #f)))))])
                  (set-timer 0)
                  (set! current-fiber-task #f)
                  (unless (context-stopped? context)
                    (case type
                      [(yield time)
                       (put-task! sched (continue-task task (lambda () (value (void)))))]
                      [(callback)
                       ;; Registration executes on the `scheduler`, with preemption disabled.
                       ;; Keep task identity visible for `event` registration and ownership.
                       (set! current-fiber-task task)
                       (guard (condition
                               [else (request-context-stop! context #t condition)])
                         (value))
                       (set! current-fiber-task #f)]
                      [(error) (request-context-stop! context #t value)]
                      [else (void)]))))
              (loop))))
        (set-timer 0)
        ;; Restore once, after the timer is disabled and this worker stops dispatching.
        (timer-interrupt-handler original-handler)
        (scheduler-k-set! sched #f))))

  (define wrap-thunk
    (lambda (context init? thunk)
      (make-fiber-task
       (cons 'fiber-task '()) init?
       (lambda ()
         (call-with-values
          thunk
          (lambda results
            (set-timer 0)
            (when init?
              (fiber-run-context-init-values-set! context results)
              (request-context-stop! context #f #f))
            ;; This boundary also handles completion after continuation migration.
            (scheduler-return 'complete #f))))
       #f)))

  (define random-sched
    (lambda (context)
      (let* ([schedulers (fiber-run-context-schedulers context)]
             [count (vector-length schedulers)])
        (vector-ref schedulers (fxmod (random 99999) count)))))

  #|proc:spawn-fiber
  The `spawn-fiber` procedure schedules `thunk`, a zero-argument procedure, as a child fiber
  in the current run context. Its result is discarded. Returns an unspecified value.
  |#
  (define-who spawn-fiber
    (lambda (thunk)
      (pcheck ([procedure? thunk])
        (unless (on-scheduler-thread?)
          (errorf who "not running scheduler"))
        (with-timer-off
          (let ([context (scheduler-context current-scheduler)])
            (put-task! (random-sched context) (wrap-thunk context #f thunk)))))))

  (define positive-fixnum? (lambda (value) (and (fixnum? value) (fx> value 0))))

  (define scheduler-quantum?
    (lambda (value) (and (fixnum? value) (fx>= value *minimum-ticks*))))

  #|proc:run-fibers
  The `run-fibers` procedure runs `thunk`, a zero-argument procedure, as the init fiber in a new
  context. Init completion or quit stops the context and drops pending child work. All workers
  join before its init values are returned or the first fiber failure is raised to the caller.
  A nested call enqueues `thunk` as a child in the current context and returns an unspecified
  value immediately, without waiting for the nested thunk to finish.
  Optional `worker-count` is the number of OS workers, a positive fixnum defaulting to
  `(cpu-count)`. Optional `quantum` is a fixnum of at least 100 Scheme timer ticks per slice;
  it defaults to 30000. Smaller budgets cannot reliably advance past scheduler dispatch and
  are rejected before any workers start. Ticks measure Scheme work, not wall-clock time. A larger
  quantum favors throughput at the cost of wakeup latency. Both settings are captured for
  the lifetime of a new context; nested calls keep their parent's scheduler configuration.
  A call on a separate OS thread creates its own context, including threads forked by a fiber.
  |#
  (define-who run-fibers
    (case-lambda
      [(thunk) (run-fibers thunk (cpu-count) *default-ticks*)]
      [(thunk worker-count) (run-fibers thunk worker-count *default-ticks*)]
      [(thunk worker-count quantum)
       (pcheck ([procedure? thunk]
                [positive-fixnum? worker-count]
                [scheduler-quantum? quantum])
         (if (on-scheduler-thread?)
             (spawn-fiber thunk)
             (let* ([context (make-fiber-run-context
                              #f (abox #f) (list (void)) #f #f worker-count quantum
                              (make-mutex 'fiber-run-context)
                              (make-condition 'fiber-run-condition)
                              (abox 0) (abox #f)
                              (make-mutex 'fiber-transactions) #f '())]
                    [schedulers
                     (list->vector
                      (map (lambda (i)
                             (make-scheduler i context (make-dlist)
                                             (make-spinlock 'fiber-scheduler-queue)
                                             #f #f #f #f #f))
                           (iota worker-count)))])
               (fiber-run-context-schedulers-set! context schedulers)
               (with-timer-off
                 (put-task! (vector-ref schedulers 0) (wrap-thunk context #t thunk)))
               (let ([threads
                      (vector-map
                       (lambda (sched)
                         (let ([thread
                                (fork-thread
                                 (lambda ()
                                   (let ([previous-scheduler current-scheduler])
                                     (set! current-scheduler sched)
                                     (set! current-fiber-task #f)
                                     (scheduler-thread-id-set! sched (get-thread-id))
                                     (scheduler-running?-set! sched #t)
                                     (run-scheduler sched)
                                     (scheduler-running?-set! sched #f)
                                     (set! current-scheduler previous-scheduler))))])
                           (scheduler-thread-set! sched thread)
                           thread))
                       schedulers)])
                 (vector-for-each thread-join threads)
                 (if (fiber-run-context-terminal-error? context)
                     (raise (fiber-run-context-terminal-error context))
                     (apply values (fiber-run-context-init-values context)))))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   first-class events
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; Common type tag for immutable base and selection events. Constructing an
  ;; `event` does not register a wait; `event-sync` creates a fresh transaction.
  (define-record-type event (fields))

  ;; A protocol lock domain begins at one resource. `id` orders joins; `parent`
  ;; redirects an obsolete root after a choice connects resources. Parent links only
  ;; point to lower IDs and are published while both roots are locked. Access always
  ;; resolves, locks, and revalidates the root, so a concurrent join cannot split a
  ;; channel rendezvous or mutate a losing choice registration under a different lock.
  ;; Joined domains remain joined: unrelated resources retain independent locks.
  ;; This conservatively serializes resources once connected by a choice; it avoids
  ;; partial two-party commits and acquiring losing-branch locks during cleanup.
  (define-record-type event-domain
    (fields id mutex parent))

  (define event-domain-counter (abox 0))

  (define new-event-domain
    (lambda ()
      (make-event-domain (abox-get&add1! event-domain-counter)
                         (make-mutex 'fiber-event-domain) (abox #f))))

  (define event-domain-root
    (lambda (domain)
      (let ([parent (unabox (event-domain-parent domain))])
        (memory-order-acquire)
        (if parent (event-domain-root parent) domain))))

  ;; No `dynamic-wind`: a fiber suspension must never leave a protocol critical
  ;; section. Internal callbacks run with timers off; user result thunks run later.
  (define call-with-protocol-domain
    (lambda (domain thunk)
      (with-timer-off
        (if (not domain)
            (thunk)
            (let retry ()
              (let* ([root (event-domain-root domain)]
                     [mutex (event-domain-mutex root)])
                (mutex-acquire mutex)
                (if (not (eq? root (event-domain-root domain)))
                    (begin (mutex-release mutex) (retry))
                    (call-with-values
                     (lambda ()
                       (guard (condition
                               [else (mutex-release mutex) (raise condition)])
                         (thunk)))
                     (lambda results
                       (mutex-release mutex)
                       (apply values results))))))))))

  (define-syntax with-event-protocol
    (syntax-rules ()
      [(_ domain expression ...)
       (call-with-protocol-domain domain (lambda () expression ...))]))

  (define join-event-domains!
    (lambda (left right)
      (with-timer-off
        (cond [(not left) right]
              [(not right) left]
              [else
               (let retry ()
                 (let ([a (event-domain-root left)] [b (event-domain-root right)])
                   (if (eq? a b)
                       a
                       (let* ([first (if (< (event-domain-id a) (event-domain-id b)) a b)]
                              [second (if (eq? first a) b a)]
                              [first-lock (event-domain-mutex first)]
                              [second-lock (event-domain-mutex second)])
                         ;; Every join locks roots in increasing ID order. Ordinary
                         ;; operations hold exactly one root and never perform joins.
                         (mutex-acquire first-lock)
                         (mutex-acquire second-lock)
                         (if (and (eq? a (event-domain-root left))
                                  (eq? b (event-domain-root right)))
                             (begin
                               (abox-set! (event-domain-parent second) first)
                               (mutex-release second-lock)
                               (mutex-release first-lock)
                               first)
                             (begin
                               (mutex-release second-lock)
                               (mutex-release first-lock)
                               (retry)))))))]))))

  (define event-protocol-domain
    (lambda (event)
      (if (base-event? event) (base-event-domain event) (select-event-domain event))))

  (define selection-domain
    (lambda (bases)
      (let loop ([index 0] [domain #f])
        (if (fx= index (vector-length bases))
            domain
            (loop (fx1+ index)
                  (join-event-domains! domain
                                       (base-event-domain (vector-ref bases index))))))))

  ;; One `event-sync` attempt, shared by all branches of a choice. `context` owns its
  ;; lifetime, and `resume` accepts a result `thunk` and only publishes runnable work.
  ;; `state` changes once from `waiting` to `committed`/`cancelled`; `cleanups` unlink every
  ;; registration, including losing branches. `domain` guards state and cleanup;
  ;; `previous`, `next`, and `linked?` belong to the context's registry mutex.
  (define-record-type sync-transaction
    (fields context domain resume (mutable state) (mutable cleanups)
            (mutable previous) (mutable next) (mutable linked?)))

  (define sync-transaction-live?
    (lambda (transaction)
      (and (eq? 'waiting (sync-transaction-state transaction))
           (not (context-stopped? (sync-transaction-context transaction))))))

  ;; Each cleanup is installed before publishing its registration. Finishing first
  ;; clears the cleanup list, so cleanup and cancellation are idempotent. Callers hold
  ;; the transaction's domain; `cleanups` only unlink storage and never reacquire it.
  (define add-sync-cleanup!
    (lambda (transaction cleanup)
      (sync-transaction-cleanups-set!
       transaction (cons cleanup (sync-transaction-cleanups transaction)))))

  (define register-sync-transaction!
    (lambda (transaction)
      (let* ([context (sync-transaction-context transaction)]
             [mutex (fiber-run-context-transaction-mutex context)])
        (mutex-acquire mutex)
        (let ([registered? (not (context-stopped? context))])
          (when registered?
            (let ([head (fiber-run-context-transactions context)])
              (sync-transaction-next-set! transaction head)
              (when head (sync-transaction-previous-set! head transaction))
              (fiber-run-context-transactions-set! context transaction)
              (sync-transaction-linked?-set! transaction #t)))
          (mutex-release mutex)
          registered?))))

  ;; The caller holds the context registry mutex; domain ownership alone does not
  ;; protect neighboring transactions, which may finish on independent resources.
  (define unregister-sync-transaction-locked!
    (lambda (transaction)
      (when (sync-transaction-linked? transaction)
        (let ([context (sync-transaction-context transaction)]
              [previous (sync-transaction-previous transaction)]
              [next (sync-transaction-next transaction)])
          (if previous
              (sync-transaction-next-set! previous next)
              (fiber-run-context-transactions-set! context next))
          (when next (sync-transaction-previous-set! next previous))
          (sync-transaction-previous-set! transaction #f)
          (sync-transaction-next-set! transaction #f)
          (sync-transaction-linked?-set! transaction #f)))))

  (define finish-sync-transaction!
    (lambda (transaction state)
      (let* ([context (sync-transaction-context transaction)]
             [mutex (fiber-run-context-transaction-mutex context)])
        (mutex-acquire mutex)
        ;; Claim under registry as well as domain: shutdown sets stop under registry,
        ;; so a successful commit has one definite ordering relative to context stop.
        (let ([finish? (and (eq? 'waiting (sync-transaction-state transaction))
                            (or (eq? state 'cancelled) (not (context-stopped? context))))])
          (when finish?
            (sync-transaction-state-set! transaction state)
            (unregister-sync-transaction-locked! transaction))
          (mutex-release mutex)
          (when finish?
            (let ([cleanups (sync-transaction-cleanups transaction)])
              (sync-transaction-cleanups-set! transaction '())
              ;; Drop registry before cleanup acquires park or publishes any work.
              (for-each (lambda (cleanup) (cleanup)) cleanups)))
          finish?))))

  (define cancel-sync-transaction!
    (lambda (transaction)
      (with-event-protocol (sync-transaction-domain transaction)
        (finish-sync-transaction! transaction 'cancelled))))

  (define commit-sync-transaction!
    (lambda (transaction thunk)
      (and (finish-sync-transaction! transaction 'committed)
           (begin
             ;; Resume only publishes a task: wrappers and result thunks run later,
             ;; outside this domain. `put-task!` releases its queue lock before park.
             ((sync-transaction-resume transaction) thunk)
             #t))))

  ;; One synchronization branch. `wrap-fn` is `#f` or a procedure transforming all
  ;; result values; `try-fn` polls readiness and returns a deferred result thunk.
  ;; `block-fn` installs a cancellable wait only after every choice branch was polled.
  ;; `domain` guards poll/register callbacks; result thunks/wrappers run later
  ;; on a resumed fiber, where user code can yield, raise, or synchronize again.
  (define-record-type base-event
    (parent event)
    (sealed #t)
    (fields domain wrap-fn
            ;; Signature: `() -> result-thunk-or-#f`, under the resource domain.
            try-fn
            ;; Signature: `(transaction resume) -> unspecified`, under the same mutex.
            block-fn))

  ;; `events` is a flattened vector of `base-event` alternatives. The graph is
  ;; immutable after construction, so choices/wrappers can share their sources.
  ;; `domain` joins all alternatives, including previously registered transactions.
  (define-record-type select-event
    (parent event)
    (sealed #t)
    ;; Immutable vector of base events; wrapping always allocates a new graph.
    (fields events domain))

  #|proc:event-never
  The `event-never` procedure returns a disabled event. Synchronizing it waits forever,
  unless the run context stops or another branch of a selection succeeds.
  |#
  (define event-never
    (let ([event (make-base-event #f #f (lambda () #f)
                                  (lambda (transaction resume) (void)))])
      (lambda () (pcheck () event))))

  #|proc:event-always
  The `event-always` procedure returns an event that immediately synchronizes with value `value`.
  |#
  (define event-always
    (lambda (value)
      (pcheck ()
        (make-base-event #f #f (lambda () (lambda () value))
                         (lambda (transaction resume) (assert-unreachable))))))

  #|proc:event-wrap
  The `event-wrap` procedure returns a new event wrapping `event`, leaving `event` unchanged.
  After synchronization, procedure receives all event result values as its arguments and its
  return values become the synchronization results. Repeated wrappers run in construction order.
  |#
  (define-who event-wrap
    (lambda (event procedure)
      (pcheck ([event? event] [procedure? procedure])
        (if (select-event? event)
            (make-select-event
             (vector-map (lambda (base) (event-wrap base procedure))
                         (select-event-events event))
             (select-event-domain event))
            (let ([wrap (base-event-wrap-fn event)])
              (make-base-event
               (base-event-domain event)
               (if wrap
                   (lambda arguments
                     (call-with-values (lambda () (apply wrap arguments)) procedure))
                   procedure)
               (base-event-try-fn event) (base-event-block-fn event)))))))

  (define all-events? (lambda (events) (andmap event? events)))

  #|proc:event-select
  The `event-select` procedure returns an event choosing one of `events` nondeterministically.
  Each argument must be an event. Nested selections are flattened without changing their sources.
  With no arguments, it returns a disabled event equivalent to `(event-never)`.
  |#
  (define-who event-select
    (lambda events
      (pcheck ([all-events? events])
        (if (null? events)
            (event-never)
            (let ([bases
                   (list->vector
                    (apply append
                           (map (lambda (event)
                                  (if (base-event? event)
                                      (list event)
                                      (vector->list (select-event-events event))))
                                events)))])
              (make-select-event bases (selection-domain bases)))))))


  (define sync-event-slow
    (lambda (event)
      (let ([scheduler current-scheduler] [task current-fiber-task]
            ;; Neutral events have no resource state, but a blocked transaction still
            ;; needs its own domain for cancellation to serialize with registration.
            [domain (or (event-protocol-domain event) (new-event-domain))])
        (define wrap-result
          (lambda (base thunk)
            (let ([wrap (base-event-wrap-fn base)])
              (if wrap (lambda () (call-with-values thunk wrap)) thunk))))
        (let ([ticks (set-timer 0)])
          (fiber-task-ticks-set! current-fiber-task ticks))
        ;; A synchronization continuation is committed and resumed at most once.
        ((call/1cc
          (lambda (continuation)
            (scheduler-return
             'callback
             (lambda ()
               (with-event-protocol domain
                 (let* ([context (scheduler-context scheduler)]
                        [transaction
                         (make-sync-transaction
                          context domain
                          (lambda (thunk)
                            (put-task! scheduler
                                       (continue-task task
                                         (lambda () (continuation thunk)))))
                          'waiting '() #f #f #f)]
                        [bases (if (base-event? event)
                                   (vector event) (select-event-events event))]
                        [length (vector-length bases)]
                        [start (fxmod (random 99999) length)])
                   (when (register-sync-transaction! transaction)
                     ;; Poll all branches before registering any. The same mutex covers
                     ;; the second phase, closing the poll-to-registration wakeup gap.
                     (let poll ([index 0])
                       (if (fx= index length)
                           (vector-for-each
                            (lambda (base)
                              (when (sync-transaction-live? transaction)
                                ((base-event-block-fn base)
                                 transaction
                                 (lambda (thunk)
                                   (commit-sync-transaction!
                                    transaction (wrap-result base thunk))))))
                            bases)
                           (let* ([base (vector-ref bases (fxmod (fx+ start index) length))]
                                  [thunk ((base-event-try-fn base))])
                             (if thunk
                                 (commit-sync-transaction! transaction (wrap-result base thunk))
                                 (poll (fx1+ index)))))))))))))))))

  (define poll-event
    (lambda (event)
      (let* ([bases (if (base-event? event) (vector event) (select-event-events event))]
             [length (vector-length bases)]
             [start (fxmod (random 99999) length)])
        (let loop ([index 0])
          (and (fx< index length)
               (let* ([base (vector-ref bases (fxmod (fx+ start index) length))]
                      [thunk ((base-event-try-fn base))])
                 (if thunk
                     (let ([wrap (base-event-wrap-fn base)])
                       (if wrap (lambda () (call-with-values thunk wrap)) thunk))
                     (loop (fx1+ index)))))))))

  #|proc:event-sync
  The `event-sync` procedure synchronizes the current fiber on event `event`. It returns the
  event's synchronization values after applying wrappers in their construction order.
  A blocked synchronization parks the fiber without blocking its scheduler worker. Successful
  selection and context shutdown detach all of that synchronization's losing registrations.
  |#
  (define-who event-sync
    (lambda (event)
      (pcheck ([event? event])
        (unless (on-scheduler-thread?) (errorf who "not running scheduler"))
        (let ([ready (with-event-protocol (event-protocol-domain event) (poll-event event))])
          ;; Only a miss captures a continuation. The slow protocol polls again
          ;; under its registration lock, so readiness changing here cannot be lost.
          ;; Invoke the result after releasing the lock and restoring preemption:
          ;; wrappers can yield, raise, return multiple values, or synchronize again.
          (if ready (ready) (sync-event-slow event))))))

  #|proc:wrap-operation
  Alias of `event-wrap`. Accepts an event and a procedure receiving its synchronization values.
  Returns a new event whose synchronization returns the procedure's result values.
  |#
  (define wrap-operation event-wrap)

  #|proc:choice-operation
  Alias of `event-select`. Accepts zero or more events and returns an event choosing one of them.
  No arguments returns a disabled event.
  |#
  (define choice-operation event-select)

  #|proc:perform-operation
  Alias of `event-sync`. Accepts an event and returns its synchronization values after waiting
  for it to become ready and applying its wrappers.
  |#
  (define perform-operation event-sync)

  ;;;;===----------------------------------------------------------------------===
  ;;;; Network operation events
  ;;;;===----------------------------------------------------------------------===

  ;; The existing four-worker driver and bounded pools remain a compatibility path.
  ;; Pool ownership and waiter membership use `net-driver-domain`. Operation advance
  ;; and result inspection use ordinary network mutexes only after releasing it.
  (define net-driver-domain (new-event-domain))
  (define net-driver-job* '())
  (define net-driver-workers 0)
  (define net-driver-job-free (map (lambda (_) (vector #f '() #f)) (iota 64)))
  (define net-driver-waiter-free (map (lambda (_) (make-vector 5 #f)) (iota 256)))
  (define net-driver-waiters-in-use 0)
  (define net-driver-waiter-high-water 0)

  (define release-net-waiter-locked!
    (lambda (waiter)
      (vector-fill! waiter #f)
      (set! net-driver-waiter-free (cons waiter net-driver-waiter-free))
      (set! net-driver-waiters-in-use (fx1- net-driver-waiters-in-use))))

  (define net-waiter-live?
    (lambda (waiter)
      ;; Context liveness and `scheduler` liveness are separate from `transaction` state.
      (and (sync-transaction-live? (vector-ref waiter 0))
           (not (context-stopped? (vector-ref waiter 3)))
           (scheduler-running? (vector-ref waiter 4)))))

  (define detach-net-job-locked!
    (lambda (job)
      ;; A claimed job cannot be recycled while a driver still holds its identity.
      (assert (not (vector-ref job 2)))
      (assert (null? (vector-ref job 1)))
      (set! net-driver-job* (remq job net-driver-job*))
      (vector-set! job 0 #f)
      (set! net-driver-job-free (cons job net-driver-job-free))))

  (define remove-net-waiter-locked!
    (lambda (job waiter)
      (when (memq waiter (vector-ref job 1))
        (vector-set! job 1 (remq waiter (vector-ref job 1)))
        (release-net-waiter-locked! waiter)
        (when (and (null? (vector-ref job 1)) (not (vector-ref job 2)))
          (detach-net-job-locked! job)))))

  (define snapshot-net-result
    (lambda (operation)
      (case (net-operation-state operation)
        [(completed)
         (let ([result (net-operation-result operation)]) (lambda () result))]
        [(failed cancelled)
         (let ([failure (net-operation-condition operation)]) (lambda () (raise failure)))]
        [else #f])))

  (define drive-net-job!
    (lambda (job operation)
      ;; The job is claimed, but no event/park/queue lock is held over user callbacks.
      (guard (ignored [else (void)])
        (when (eq? 'pending (net-operation-state operation))
          (net-operation-step! operation)))
      (let ([result (snapshot-net-result operation)])
        (with-event-protocol net-driver-domain
          (when result
            ;; Capture delivery data before any commit recycles pooled vectors. One
            ;; `transaction` may have several branches registered on this same job.
            (let ([deliveries
                   (map (lambda (waiter)
                          (and (net-waiter-live? waiter)
                               (vector (vector-ref waiter 0) (vector-ref waiter 1)
                                       (vector-ref waiter 2))))
                        (vector-ref job 1))])
              (for-each
               (lambda (delivery)
                 (when (and delivery (sync-transaction-live? (vector-ref delivery 0)))
                   (abox-set! (vector-ref delivery 2) result)
                   ((vector-ref delivery 1) result)))
               deliveries)))
          (vector-set! job 2 #f)
          (when (null? (vector-ref job 1)) (detach-net-job-locked! job))))))

  (define drive-net-operations
    (lambda ()
      ;; The driver is a separate OS thread. Clearing its inherited scheduler also
      ;; hides its parent's task; clearing the record slot would corrupt that worker.
      (set! current-scheduler #f)
      (let loop ()
        (let ([claim
               (with-event-protocol net-driver-domain
                 (if (null? net-driver-job*)
                     (begin (set! net-driver-workers (fx1- net-driver-workers)) 'stop)
                     (let ([job (find (lambda (job) (not (vector-ref job 2)))
                                      net-driver-job*)])
                       (and job
                            (begin
                              (vector-set! job 2 #t)
                              ;; Rotate claims so a pending job cannot starve later work.
                              (set! net-driver-job*
                                    (append (remq job net-driver-job*) (list job)))
                              (cons job (vector-ref job 0)))))))])
          (unless (eq? claim 'stop)
            (when claim (drive-net-job! (car claim) (cdr claim)))
            (milisleep 1)
            (loop))))))

  #|proc:net-operation-event-pool-metrics
  The `net-operation-event-pool-metrics` procedure returns a fresh vector of driver job count,
  active waiter count, and waiter high-water count. Four workers serve up to 64 jobs and
  256 waiters.
  |#
  (define net-operation-event-pool-metrics
    (lambda ()
      (pcheck ()
        (with-event-protocol net-driver-domain
          (vector (length net-driver-job*) net-driver-waiters-in-use
                  net-driver-waiter-high-water)))))

  #|proc:net-operation-event
  The `net-operation-event` procedure creates a fiber event for network `operation`.
  Synchronization drives the operation on a bounded OS worker pool and returns its result.
  The original failure or cancellation condition is raised when the fiber resumes.
  Losing choices and context shutdown immediately detach their waiters. An already running
  driver callback may finish, but cleanup never cancels a shared operation owned by the caller.
  |#
  (define net-operation-event
    (lambda (operation)
      (pcheck ([net-operation? operation])
        (let ([result (abox #f)])
          (define try (lambda () (unabox result)))
          (define block
            (lambda (transaction resume)
              (let ([job (find (lambda (job) (eq? operation (vector-ref job 0)))
                               net-driver-job*)])
                (if (and (pair? net-driver-waiter-free)
                         (or job (pair? net-driver-job-free)))
                    (begin
                      (unless job
                        (set! job (car net-driver-job-free))
                        (set! net-driver-job-free (cdr net-driver-job-free))
                        (vector-set! job 0 operation)
                        (set! net-driver-job* (cons job net-driver-job*)))
                      (let ([waiter (car net-driver-waiter-free)])
                        (set! net-driver-waiter-free (cdr net-driver-waiter-free))
                        (vector-set! waiter 0 transaction)
                        (vector-set! waiter 1 resume)
                        (vector-set! waiter 2 result)
                        (vector-set! waiter 3 (scheduler-context current-scheduler))
                        (vector-set! waiter 4 current-scheduler)
                        (add-sync-cleanup!
                         transaction (lambda () (remove-net-waiter-locked! job waiter)))
                        (vector-set! job 1 (cons waiter (vector-ref job 1)))
                        (set! net-driver-waiters-in-use (fx1+ net-driver-waiters-in-use))
                        (set! net-driver-waiter-high-water
                              (fxmax net-driver-waiter-high-water net-driver-waiters-in-use)))
                      (let start-workers ()
                        (when (fx< net-driver-workers 4)
                          (set! net-driver-workers (fx1+ net-driver-workers))
                          (fork-thread drive-net-operations)
                          (start-workers))))
                    (resume (lambda () (errorf 'net-operation-event
                                                "network event waiter pool exhausted")))))))
          (make-base-event net-driver-domain #f try block)))))



  ;; A synchronous rendezvous point. `putQ` stores pending sends and their messages;
  ;; `getQ` stores pending receives. Both contain transaction/resume/payload entries
  ;; and are accessed only under `domain`'s current root. Cleanup removes each
  ;; losing or cancelled waiter before another synchronization can match it.
  (define-record-type (channel %make-channel channel?)
    (fields domain putQ getQ))

  #|proc:make-channel
  The `make-channel` procedure returns a new synchronous channel for rendezvous between fibers.
  |#
  (define make-channel
    (lambda () (pcheck () (%make-channel (new-event-domain) (make-dlist) (make-dlist)))))

  ;; Channel, mutex, and cvar queues share `#(transaction resume payload)` entries.
  ;; Their caller holds the resource domain while taking or registering a waiter.
  ;; Exclude the calling `transaction` when matching opposite `channel` branches.
  (define take-sync-waiter!
    (lambda (queue transaction)
      (let loop ()
        (let ([index (dlist-find-index queue
                       (lambda (item) (not (eq? transaction (vector-ref item 0)))))])
          (and index
               (let ([item (dlist-ref queue index)])
                 (dlist-delete! queue index)
                 (if (sync-transaction-live? (vector-ref item 0)) item (loop))))))))

  (define register-sync-waiter!
    (lambda (queue transaction resume message)
      (let ([item (vector transaction resume message)])
        (add-sync-cleanup!
         transaction
         (lambda ()
           (let ([index (dlist-find-index queue (lambda (entry) (eq? entry item)))])
             (when index (dlist-delete! queue index)))))
        (enqueue! queue item))))

  #|proc:channel-get-event
  The `channel-get-event` procedure returns an event receiving a message from channel `channel`.
  Synchronization returns the sender's message, waiting if no sender is available.
  |#
  (define-who channel-get-event
    (lambda (channel)
      (pcheck ([channel? channel])
        (define try
          (lambda ()
            (let loop ()
              (let ([item (take-sync-waiter! (channel-putQ channel) #f)])
                (and item
                     (if ((vector-ref item 1) void)
                         (let ([message (vector-ref item 2)]) (lambda () message))
                         (loop)))))))
        (define block
          (lambda (transaction resume)
            (let ([item (take-sync-waiter! (channel-putQ channel) transaction)])
              (if item
                  (if ((vector-ref item 1) void)
                      (resume (let ([message (vector-ref item 2)]) (lambda () message)))
                      (block transaction resume))
                  (register-sync-waiter! (channel-getQ channel) transaction resume #f)))))
        (make-base-event (channel-domain channel) #f try block))))

  #|proc:channel-put-event
  The `channel-put-event` procedure returns an event sending `message` on channel `channel`.
  Synchronization waits for a receiver and returns an unspecified value after handing it
  the message.
  |#
  (define-who channel-put-event
    (lambda (channel message)
      (pcheck ([channel? channel])
        (define try
          (lambda ()
            (let loop ()
              (let ([item (take-sync-waiter! (channel-getQ channel) #f)])
                (and item
                     (if ((vector-ref item 1) (lambda () message)) void (loop)))))))
        (define block
          (lambda (transaction resume)
            (let ([item (take-sync-waiter! (channel-getQ channel) transaction)])
              (if item
                  (if ((vector-ref item 1) (lambda () message))
                      (resume void)
                      (block transaction resume))
                  (register-sync-waiter! (channel-putQ channel) transaction resume message)))))
        (make-base-event (channel-domain channel) #f try block))))

  #|proc:get-operation
  Alias of `channel-get-event`. Accepts a channel and returns an event whose synchronization
  receives and returns a message from that channel.
  |#
  (define get-operation channel-get-event)

  #|proc:put-operation
  Alias of `channel-put-event`. Accepts a channel and any message, and returns an event sending
  that message. Its synchronization waits for a receiver and returns an unspecified value.
  |#
  (define put-operation channel-put-event)


  (define $channel-get
    (lambda (who ch)
      (pcheck ([channel? ch])
              (event-sync (channel-get-event ch)))))

  (define $channel-put!
    (lambda (who ch m)
      (pcheck ([channel? ch])
              (event-sync (channel-put-event ch m)))))

  #|doc
  Get a message from channel `ch`, block if senders are available.
  |#
  (define-who channel-get
    (lambda (ch)
      ($channel-get who ch)))

  #|doc
  Put a message to channel `ch`, block is no receivers are available.
  |#
  (define-who channel-put!
    (lambda (ch m)
      ($channel-put! who ch m)))

  #|doc
  Alias of `channel-get`.
  |#
  (define-who get-message
    (lambda (ch)
      ($channel-get who ch)))

  #|doc
  Alias of `channel-put!`.
  |#
  (define-who put-message
    (lambda (ch m)
      ($channel-put! who ch m)))




;;;;===----------------------------------------------------------------------===
;;;; Fiber mutexes and condition variables
;;;;===----------------------------------------------------------------------===

  ;; `domain` guards ownership and the acquiring queue. Tokens are
  ;; stable across continuation migration; OS thread identity does not own a mutex.
  ;; `name` is diagnostic, `owner` is `#f` or a `fiber-task` `token`, and `blockQ` contains
  ;; acquiring fibers. Release transfers `owner` before publishing the next resume.
  (define-record-type (fiber-mutex %make-fiber-mutex fiber-mutex?)
    (fields domain name (mutable owner) blockQ))

  ;; `name` is diagnostic; `domain` guards condition waiters in `waitQ`.
  ;; Each waiter carries its own mutex reacquisition closure; the cvar stores no
  ;; persistent signal and is not permanently associated with one fiber mutex.
  (define-record-type (fiber-cvar %make-fiber-cvar fiber-cvar?)
    (fields domain name waitQ))

  #|proc:make-fiber-mutex
  The `make-fiber-mutex` procedure returns a new, initially unlocked fiber mutex.
  The optional `name` parameter is any object used as a diagnostic label; its default is
  the symbol `fiber-mutex`. Mutex ownership belongs to a fiber and survives yielding.
  |#
  (define make-fiber-mutex
    (case-lambda
      [() (make-fiber-mutex 'fiber-mutex)]
      [(name) (pcheck () (%make-fiber-mutex (new-event-domain) name #f (make-dlist)))]))

  #|proc:make-fiber-cvar
  The `make-fiber-cvar` procedure returns a new fiber condition variable with no waiters.
  The optional `name` parameter is any object used as a diagnostic label; its default is
  the symbol `fiber-cvar`. A condition variable retains no signal when no fiber is waiting.
  |#
  (define make-fiber-cvar
    (case-lambda
      [() (make-fiber-cvar 'fiber-cvar)]
      [(name) (pcheck () (%make-fiber-cvar (new-event-domain) name (make-dlist)))]))

  (define require-fiber-token
    (lambda (who)
      (unless (and (on-scheduler-thread?) current-fiber-task)
        (errorf who "not running a fiber"))
      (fiber-task-token current-fiber-task)))

  ;; The caller holds the mutex's domain for ownership validation and transfer.
  (define check-fiber-mutex-owner!
    (lambda (who mutex token)
      (unless (eq? token (fiber-mutex-owner mutex))
        (errorf who "fiber does not own fiber-mutex ~a" (fiber-mutex-name mutex)))))

  (define release-fiber-mutex-locked!
    (lambda (who mutex token)
      (check-fiber-mutex-owner! who mutex token)
      (let loop ()
        (let ([waiter (take-sync-waiter! (fiber-mutex-blockQ mutex) #f)])
          (if waiter
              (begin
                ;; Transfer before publication. A stopped context may reject commit;
                ;; in that case continue instead of leaving an unreachable owner.
                (fiber-mutex-owner-set! mutex (vector-ref waiter 2))
                (unless ((vector-ref waiter 1) void) (loop)))
              (fiber-mutex-owner-set! mutex #f))))
      (void)))

  #|proc:fiber-mutex-acquire
  The `fiber-mutex-acquire` procedure acquires fiber mutex `mutex` for the current fiber.
  If it is already held, the fiber waits without blocking its scheduler worker. Acquisition
  is not recursive. It returns an unspecified value after becoming the owner.
  |#
  (define-who fiber-mutex-acquire
    (lambda (mutex)
      (pcheck ([fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          (event-sync
           (make-base-event
            (fiber-mutex-domain mutex) #f
            (lambda ()
              (and (not (fiber-mutex-owner mutex))
                   (begin (fiber-mutex-owner-set! mutex token) void)))
            (lambda (transaction resume)
              (register-sync-waiter! (fiber-mutex-blockQ mutex)
                                     transaction resume token))))))))

  #|proc:fiber-mutex-release
  The `fiber-mutex-release` procedure releases fiber mutex `mutex`, transferring ownership
  to a waiting fiber if one exists. It raises an error unless the current fiber owns `mutex`.
  It returns an unspecified value.
  |#
  (define-who fiber-mutex-release
    (lambda (mutex)
      (pcheck ([fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          (with-event-protocol (fiber-mutex-domain mutex)
            (release-fiber-mutex-locked! who mutex token))))))

  #|macro:with-fiber-mutex
  The `with-fiber-mutex` macro evaluates `mutex` once, acquires that fiber mutex, and evaluates
  `expression ...` while owning it. Yielding preserves ownership. Normal return releases the
  mutex and returns all expression values; an exception releases it and raises the same object.
  |#
  (define-syntax with-fiber-mutex
    (syntax-rules ()
      [(_ mutex expression ...)
       (let ([held-mutex mutex])
         (fiber-mutex-acquire held-mutex)
         (call-with-values
          (lambda ()
            ;; A continuation yield must not run a `dynamic-wind` exit/entry cleanup.
            (guard (condition
                    [else (fiber-mutex-release held-mutex) (raise condition)])
              expression ...))
          (lambda results
            (fiber-mutex-release held-mutex)
            (apply values results))))]))

  #|proc:fiber-cvar-wait
  The `fiber-cvar-wait` procedure waits on condition variable `cvar` with fiber mutex `mutex`.
  The current fiber must own `mutex`. Waiting registers the fiber and releases the mutex
  atomically. A signal or broadcast wakes the fiber, which reacquires `mutex` before returning
  an unspecified value. Callers should recheck their protected predicate after every wakeup.
  |#
  (define-who fiber-cvar-wait
    (lambda (cvar mutex)
      (pcheck ([fiber-cvar? cvar] [fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          ;; Report invalid ownership on the calling fiber, where its guard can catch it.
          (join-event-domains! (fiber-cvar-domain cvar) (fiber-mutex-domain mutex))
          (with-event-protocol (fiber-mutex-domain mutex)
            (check-fiber-mutex-owner! who mutex token))
          (event-sync
           (make-base-event
            (fiber-cvar-domain cvar) #f (lambda () #f)
            (lambda (transaction resume)
              ;; Publish before unlocking, under the same protocol used by signal/broadcast.
              (register-sync-waiter!
               (fiber-cvar-waitQ cvar) transaction
               (lambda (ignored)
                 ;; Reacquisition executes on the resumed fiber, outside the protocol mutex.
                 (resume (lambda () (fiber-mutex-acquire mutex))))
               #f)
              (release-fiber-mutex-locked! who mutex token))))))))

  #|proc:fiber-cvar-signal
  The `fiber-cvar-signal` procedure wakes one fiber waiting on condition variable `cvar`, if any.
  It returns an unspecified value; the awakened fiber reacquires its mutex before returning.
  |#
  (define-who fiber-cvar-signal
    (lambda (cvar)
      (pcheck ([fiber-cvar? cvar])
        (with-event-protocol (fiber-cvar-domain cvar)
          (let loop ()
            (let ([waiter (take-sync-waiter! (fiber-cvar-waitQ cvar) #f)])
              (when (and waiter (not ((vector-ref waiter 1) void))) (loop))))
          (void)))))

  #|proc:fiber-cvar-broadcast
  The `fiber-cvar-broadcast` procedure wakes every fiber waiting on condition variable `cvar`.
  It returns an unspecified value; each awakened fiber reacquires its mutex before returning.
  |#
  (define-who fiber-cvar-broadcast
    (lambda (cvar)
      (pcheck ([fiber-cvar? cvar])
        (with-event-protocol (fiber-cvar-domain cvar)
          (let loop ()
            (let ([waiter (take-sync-waiter! (fiber-cvar-waitQ cvar) #f)])
              (when waiter
                ((vector-ref waiter 1) void)
                (loop))))
          (void)))))




;;;; time ops


  (define make-timer-event
    (lambda (duration)
      (make-base-event
       (new-event-domain) #f
       (lambda ()
         (if (time-duration-expired? duration)
             (lambda () (void))
             #f))
       (lambda (transaction resume)
         (let* ([context (sync-transaction-context transaction)]
                [deadline (add-duration (current-time 'time-monotonic) duration)]
                [timer #f])
           (set! timer (make-fiber-timer
                        deadline transaction
                        ;; Resume through the `event` protocol callback so wrappers and
                        ;; selections apply their result transformation before wakeup.
                        (lambda () (resume (lambda () (void))))))
           ;; Cleanup runs under event-protocol and may briefly acquire park.
           (add-sync-cleanup!
            transaction
            (lambda ()
              (let ([mutex (fiber-run-context-park-mutex context)])
                (mutex-acquire mutex)
                (remove-timer-locked! context timer)
                (mutex-release mutex))))
           (let ([mutex (fiber-run-context-park-mutex context)])
             (mutex-acquire mutex)
             (if (sync-transaction-live? transaction)
                 (begin
                   (insert-timer-locked! context timer)
                   ;; Wake a worker if this became the earliest deadline.
                   (condition-signal (fiber-run-context-park-condition context)))
                 (void))
             (mutex-release mutex)))))))

  #|proc:milisleep-event
  The `milisleep-event` procedure returns an event that completes after at least `t`
  milliseconds. `t` is a natural number and zero completes immediately.
  |#
  (define-who milisleep-event
    (lambda (t)
      (pcheck ([natural? t])
        (make-timer-event
         (make-time 'time-duration
                    (* (modulo t 1000) 1000000)
                    (quotient t 1000))))))

  #|proc:sleep-event
  The `sleep-event` procedure returns an event that completes after at least `t` seconds.
  `t` is a natural number and zero completes immediately.
  |#
  (define-who sleep-event
    (lambda (t)
      (pcheck ([natural? t])
        (make-timer-event (make-time 'time-duration 0 t)))))

  #|proc:fiber-milisleep
  The `fiber-milisleep` procedure suspends the current fiber for at least `t` milliseconds.
  `t` is a natural number; the return value is unspecified.
  |#
  (define-who fiber-milisleep
    (lambda (t)
      (pcheck ([natural? t]) (event-sync (milisleep-event t)))))

  #|proc:fiber-sleep
  The `fiber-sleep` procedure suspends the current fiber for at least `t` seconds.
  `t` is a natural number; the return value is unspecified.
  |#
  (define-who fiber-sleep
    (lambda (t)
      (pcheck ([natural? t]) (event-sync (sleep-event t)))))




  (record-writer (type-descriptor base-event)
                 (lambda (r p wr)
                   (display "#<base-event>" p)))

  (record-writer (type-descriptor select-event)
                 (lambda (r p wr)
                   (display "#<select-event " p)
                   (wr (vector-length (select-event-events r)) p)
                   (display ">" p)))

  (record-writer (type-descriptor channel)
                 (lambda (r p wr)
                   (display "#<channel>" p)))

  (record-writer (type-descriptor fiber-mutex)
                 (lambda (r p wr)
                   (display "#<fiber-mutex " p)
                   (wr (fiber-mutex-name r) p)
                   (display ">" p)))

  (%net-operation-wait-hook
   (lambda (operation blocking-wait)
     (if (on-scheduler-thread?)
         (event-sync (net-operation-event operation))
         (blocking-wait operation))))

  )
