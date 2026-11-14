(library (cspp net private lws-reactor)
  (export lws-reactor?
          make-lws-reactor
          make-lws-server-reactor
          lws-reactor-state
          lws-reactor-owner-id
          lws-reactor-wakeup-fd
          lws-reactor-start!
          lws-reactor-shutdown!
          make-lws-reactor-operation
          lws-reactor-operation-events
          lws-reactor-drain-operation-events!
          lws-reactor-release-operation!
          lws-reactor-operation-lifecycle
          lws-reactor-register-waiter!
          lws-reactor-client-acquire!
          lws-reactor-client-release!
          lws-reactor-client-start!
          lws-reactor-submit-body!
          lws-reactor-consume-body!
          lws-reactor-close-stream!
          lws-reactor-server-request-dequeue!
          lws-reactor-server-submit-response!
          lws-reactor-inject-event!
          lws-reactor-inject-poll!
          lws-reactor-poll-snapshot
          lws-reactor-pool-metrics)
  (import (cspp chez)
          (only (cspp utils) define-who natural? pcheck positive-natural?)
          (cspp net errors)
          (cspp net operation)
          (cspp net poll)
          (cspp net private lws-ffi))

  (define-record-type (reactor-command %make-reactor-command reactor-command?)
    (sealed #t)
    (opaque #t)
    (fields (mutable next)
            (mutable tag)
            (mutable arguments)))

  (define-record-type (reactor-waiter %make-reactor-waiter reactor-waiter?)
    (sealed #t)
    (opaque #t)
    (fields (mutable next)
            (mutable operation)
            (mutable procedure)
            (mutable active?)))

  (define-record-type (reactor-operation-state %make-reactor-operation-state
                                               reactor-operation-state?)
    (sealed #t)
    (opaque #t)
    (fields (immutable connection-id)
            (immutable stream-id)
            (immutable generation)
            (mutable signal)
            (immutable poll-target)
            (immutable deadline-ms)
            (immutable retain?)
            (mutable lifecycle)
            (mutable events)
            (mutable result)
            (mutable failure)
            (mutable waiters)))

  (define-record-type (lws-reactor %make-lws-reactor %lws-reactor?)
    (sealed #t)
    (opaque #t)
    (fields (immutable native-context)
            (immutable mutex)
            (immutable condition)
            (immutable command-capacity)
            (immutable command-storage)
            (mutable command-free)
            (mutable command-head)
            (mutable command-tail)
            (mutable command-in-use)
            (mutable command-high-water)
            (mutable command-misses)
            (mutable command-exhaustions)
            (immutable waiter-capacity)
            (immutable waiter-storage)
            (mutable waiter-free)
            (mutable waiter-in-use)
            (mutable waiter-high-water)
            (mutable waiter-misses)
            (mutable operations)
            (mutable server-events)
            (mutable current-poll-snapshot)
            (mutable lifecycle)
            (mutable owner-thread-id)
            (mutable thread)
            (mutable failure)))

  (define lws-event-tag?
    (lambda (value)
      (and (symbol? value)
           (memq value '(connected headers readable writable complete closed failed
                                   reset goaway)))))

  (define lws-observed-protocol?
    (lambda (value)
      (or (fixnum? value) (memq value '(unknown http1 http2)))))

  (define lws-terminal-scope?
    (lambda (value)
      (or (fixnum? value) (memq value '(none stream connection)))))

  (define lws-poll-operation?
    (lambda (value)
      (and (symbol? value) (memq value '(add change delete)))))

  (define reactor-file-descriptor?
    (lambda (value)
      (and (fixnum? value) (fx>= value 0))))

  (define reactor?
    (lambda (value)
      (%lws-reactor? value)))

  (define initialize-command-pool!
    (lambda (reactor)
      (let ([storage (lws-reactor-command-storage reactor)])
        (let loop ([index 0] [free #f])
          (if (fx= index (vector-length storage))
              (lws-reactor-command-free-set! reactor free)
              (let ([command (%make-reactor-command free #f #f)])
                (vector-set! storage index command)
                (loop (fx1+ index) command)))))))

  (define initialize-waiter-pool!
    (lambda (reactor)
      (let ([storage (lws-reactor-waiter-storage reactor)])
        (let loop ([index 0] [free #f])
          (if (fx= index (vector-length storage))
              (lws-reactor-waiter-free-set! reactor free)
              (let ([waiter (%make-reactor-waiter free #f #f #f)])
                (vector-set! storage index waiter)
                (loop (fx1+ index) waiter)))))))

  (define make-network-condition
    (lambda (who message irritants)
      (condition (make-error)
                 (make-who-condition who)
                 (make-message-condition message)
                 (make-irritants-condition irritants))))

  (define find-operation-state-locked
    (lambda (reactor operation)
      (let ([entry (assq operation (lws-reactor-operations reactor))])
        (and entry (cdr entry)))))

  (define find-event-operation-locked
    (lambda (reactor connection-id stream-id generation)
      (let loop ([operation+state* (lws-reactor-operations reactor)])
        (and (pair? operation+state*)
             (let* ([operation+state (car operation+state*)]
                    [state (cdr operation+state)])
               (if (and (= connection-id
                           (reactor-operation-state-connection-id state))
                        (= stream-id (reactor-operation-state-stream-id state))
                        (= generation (reactor-operation-state-generation state)))
                   operation+state
                   (loop (cdr operation+state*))))))))

  (define command-acquire-locked!
    (lambda (reactor)
      (let ([command (lws-reactor-command-free reactor)])
        (if command
            (begin
              (lws-reactor-command-free-set! reactor (reactor-command-next command))
              (reactor-command-next-set! command #f)
              (lws-reactor-command-in-use-set!
               reactor (fx1+ (lws-reactor-command-in-use reactor)))
              (when (fx> (lws-reactor-command-in-use reactor)
                         (lws-reactor-command-high-water reactor))
                (lws-reactor-command-high-water-set!
                 reactor (lws-reactor-command-in-use reactor)))
              command)
            (begin
              (lws-reactor-command-misses-set!
               reactor (fx1+ (lws-reactor-command-misses reactor)))
              (lws-reactor-command-exhaustions-set!
               reactor (fx1+ (lws-reactor-command-exhaustions reactor)))
              #f)))))

  (define command-release-locked!
    (lambda (reactor command)
      (reactor-command-tag-set! command #f)
      (reactor-command-arguments-set! command #f)
      (reactor-command-next-set! command (lws-reactor-command-free reactor))
      (lws-reactor-command-free-set! reactor command)
      (lws-reactor-command-in-use-set!
       reactor (fx1- (lws-reactor-command-in-use reactor)))))

  (define enqueue-command!
    (lambda (reactor tag arguments)
      (let ([accepted?
             (with-mutex (lws-reactor-mutex reactor)
               (if (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
                   #f
                   ;; Consumption acknowledgements are additive. Merge pending bytes for the
                   ;; same stream so a burst of small chunks cannot fill the command pool.
                   (let ([pending
                          (and (eq? tag 'consume-body)
                               (let find ([command (lws-reactor-command-head reactor)])
                                 (and command
                                      (let ([queued (reactor-command-arguments command)])
                                        (if (and (eq? (reactor-command-tag command) 'consume-body)
                                                 (= (car arguments) (car queued))
                                                 (= (cadr arguments) (cadr queued))
                                                 (= (caddr arguments) (caddr queued)))
                                            command
                                            (find (reactor-command-next command)))))))])
                     (if pending
                         (let ([queued (reactor-command-arguments pending)])
                           (set-car! (cdddr queued) (+ (cadddr queued) (cadddr arguments)))
                           #t)
                         (let ([command (command-acquire-locked! reactor)])
                           (and command
                                (begin
                                  (reactor-command-tag-set! command tag)
                                  (reactor-command-arguments-set! command arguments)
                                  (if (lws-reactor-command-tail reactor)
                                      (reactor-command-next-set!
                                       (lws-reactor-command-tail reactor) command)
                                      (lws-reactor-command-head-set! reactor command))
                                  (lws-reactor-command-tail-set! reactor command)
                                  (condition-signal (lws-reactor-condition reactor))
                                  #t)))))))])
        (when accepted?
          (with-mutex (lws-reactor-mutex reactor)
            ;; Shutdown may begin after queue acceptance; never wake a destroyed context.
            (unless (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
              (lws-context-wakeup (lws-reactor-native-context reactor)))))
        accepted?)))

  (define dequeue-command!
    (lambda (reactor)
      (with-mutex (lws-reactor-mutex reactor)
        (let ([command (lws-reactor-command-head reactor)])
          (when command
            (lws-reactor-command-head-set! reactor (reactor-command-next command))
            (when (not (lws-reactor-command-head reactor))
              (lws-reactor-command-tail-set! reactor #f))
            (reactor-command-next-set! command #f))
          command))))

  (define waiter-release-locked!
    (lambda (reactor waiter)
      (reactor-waiter-operation-set! waiter #f)
      (reactor-waiter-procedure-set! waiter #f)
      (reactor-waiter-active?-set! waiter #f)
      (reactor-waiter-next-set! waiter (lws-reactor-waiter-free reactor))
      (lws-reactor-waiter-free-set! reactor waiter)
      (lws-reactor-waiter-in-use-set!
       reactor (fx1- (lws-reactor-waiter-in-use reactor)))))

  (define release-operation-waiters-locked!
    (lambda (reactor state)
      (let loop ([waiter (reactor-operation-state-waiters state)] [procedure* '()])
        (if waiter
            (let ([next (reactor-waiter-next waiter)]
                  [procedure (and (reactor-waiter-active? waiter)
                                  (reactor-waiter-procedure waiter))])
              (waiter-release-locked! reactor waiter)
              (loop next (if procedure (cons procedure procedure*) procedure*)))
            (begin
              (reactor-operation-state-waiters-set! state #f)
              procedure*)))))

  (define terminal-event?
    (lambda (tag)
      (memq tag '(complete closed failed reset goaway))))

  ;; User waiters are notifications, like net-operation listeners. One failing observer
  ;; must not kill the owner thread or prevent native context shutdown.
  (define notify-waiter!
    (lambda (procedure operation)
      (guard (failure [else (void)]) (procedure operation))))

  (define publish-event!
    (lambda (reactor event)
      (let ([operation #f]
            [signal #f]
            [procedure* '()])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([entry (find-event-operation-locked
                        reactor (vector-ref event 2) (vector-ref event 3)
                        (vector-ref event 4))])
            (if entry
              (set! operation (car entry))
              (lws-reactor-server-events-set!
               reactor (append (lws-reactor-server-events reactor) (list event))))
            (when entry
              (let* ([state (cdr entry)]
                     [tag (vector-ref event 0)])
                (when (eq? 'pending (reactor-operation-state-lifecycle state))
                  (set! signal (reactor-operation-state-signal state))
                  (reactor-operation-state-events-set!
                   state (cons event (reactor-operation-state-events state)))
                  (when (terminal-event? tag)
                    (if (eq? tag 'complete)
                        (begin
                          (reactor-operation-state-lifecycle-set! state 'completed)
                          (reactor-operation-state-result-set! state event))
                        (begin
                          (reactor-operation-state-lifecycle-set! state 'failed)
                          (reactor-operation-state-failure-set!
                           state
                           (make-net-error
                            'lws-reactor tag (format "HTTP stream ~a" tag)
                            (list (vector-ref event 2) (vector-ref event 3)
                                  (vector-ref event 4) (vector-ref event 5)
                                  (vector-ref (vector-ref event 7) 3))))))
                    (set! procedure*
                          (release-operation-waiters-locked! reactor state))))))))
        (for-each (lambda (procedure) (notify-waiter! procedure operation)) procedure*)
        (when signal
          ;; Cleanup can return this signal to the pool while waiter callbacks run.
          ;; Recheck the owning state and notify under the same mutex as signal release.
          (with-mutex (lws-reactor-mutex reactor)
            (let ([state (find-operation-state-locked reactor operation)])
              (when (and state (= signal (reactor-operation-state-signal state)))
                (lws-signal-notify signal))))))))

  (define drain-native-events!
    (lambda (reactor)
      (let loop ()
        (let ([event (lws-context-next-event (lws-reactor-native-context reactor))])
          (when event
            ;; Native poll callbacks already update the authoritative poll snapshot. They
            ;; are not application events: retaining them in the server queue makes each
            ;; receive-flow change copy an ever-growing history, even in a client reactor.
            (unless (memq (vector-ref event 0) '(poll-add poll-change poll-delete))
              (publish-event! reactor event))
            (loop))))))

  (define process-command!
    (lambda (reactor command)
      (let ([tag (reactor-command-tag command)]
            [arguments (reactor-command-arguments command)]
            [result #t])
        (set! result (case tag
          [(inject-event)
           (apply lws-context-inject-event!
                  (lws-reactor-native-context reactor) arguments)]
          [(inject-poll)
           (apply lws-context-inject-poll!
                  (lws-reactor-native-context reactor) arguments)]
          [(start)
           (apply lws-client-start
                (lws-reactor-native-context reactor) arguments)]
          [(acquire)
           (apply lws-client-acquire
                  (lws-reactor-native-context reactor) arguments)]
          [(release)
           (apply lws-client-release
                  (lws-reactor-native-context reactor) arguments)]
          [(submit-body)
           (apply lws-client-body-submit
                  (lws-reactor-native-context reactor) arguments)]
          [(consume-body)
           (apply lws-body-consumed
                  (lws-reactor-native-context reactor) arguments)]
          [(server-response)
           (apply lws-server-response-submit
                  (lws-reactor-native-context reactor) arguments)]
          [(cancel)
           (apply lws-stream-cancel
                  (lws-reactor-native-context reactor) arguments)]
          [else #t]))
        ;; Command acceptance only means the command entered the queue. A rejected native
        ;; command must become a deterministic terminal failure for its owning operation.
        (when (eq? result #f)
          (let ([identity*
                 (case tag
                   [(inject-event)
                    (and (>= (length arguments) 4)
                         (list (cadr arguments) (caddr arguments)
                               (cadddr arguments)))]
                   [(start acquire release submit-body consume-body cancel server-response)
                    (and (>= (length arguments) 3)
                         (list (car arguments) (cadr arguments)
                               (caddr arguments)))]
                   [else #f])])
            (when identity*
              (publish-event!
               reactor
               (vector 'failed 0 (car identity*) (cadr identity*) (caddr identity*)
                       -1 (make-bytevector 0)
                       (vector 'unknown #f #f 'stream))))))
        (with-mutex (lws-reactor-mutex reactor)
          (command-release-locked! reactor command))
        (drain-native-events! reactor))))

  (define drain-commands!
    (lambda (reactor)
      (let loop ()
        (let ([command (dequeue-command! reactor)])
          (when command
            (process-command! reactor command)
            (loop))))))

  (define service-poll-pass!
    (lambda (reactor)
      (lws-context-service (lws-reactor-native-context reactor) 50)
      (drain-native-events! reactor)))

  ;; Must be called under the reactor mutex, before context storage is destroyed.
  (define close-operation-signal-locked!
    (lambda (state)
      (let ([signal (reactor-operation-state-signal state)])
        (unless (zero? signal)
          (lws-signal-close signal)
          (reactor-operation-state-signal-set! state 0)))))

  (define fail-pending-operations!
    (lambda (reactor failure)
      (let ([notification* '()])
        (with-mutex (lws-reactor-mutex reactor)
          (for-each
           (lambda (entry)
             (let ([state (cdr entry)])
               (when (eq? 'pending (reactor-operation-state-lifecycle state))
                 (reactor-operation-state-lifecycle-set! state 'failed)
                 (reactor-operation-state-failure-set! state failure)
                 (set! notification*
                       (cons (cons (car entry)
                                   (release-operation-waiters-locked! reactor state))
                             notification*)))
               ;; Terminal native events may still await their Scheme cleanup callback.
               ;; Invalidate every handle so stepping/cancelling later cannot touch freed C data.
               (close-operation-signal-locked! state)))
           (lws-reactor-operations reactor)))
        (for-each
         (lambda (notification)
           (for-each (lambda (procedure) (notify-waiter! procedure (car notification)))
                     (cdr notification)))
         notification*))))

  (define reactor-main
    (lambda (reactor)
      (guard (failure
              [else
               (with-mutex (lws-reactor-mutex reactor)
                 (lws-reactor-failure-set! reactor failure)
                 (lws-reactor-lifecycle-set! reactor 'stopping))])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-owner-thread-id-set! reactor (get-thread-id))
          (when (eq? 'starting (lws-reactor-lifecycle reactor))
            (lws-reactor-lifecycle-set! reactor 'running))
          (condition-broadcast (lws-reactor-condition reactor)))
        (let loop ()
          (drain-commands! reactor)
          (unless (eq? 'stopping (lws-reactor-lifecycle reactor))
            (service-poll-pass! reactor)
            (loop))))
      (let ([failure
             (or (lws-reactor-failure reactor)
                 (make-network-condition 'lws-reactor-shutdown!
                                         "reactor stopped" '()))])
        (fail-pending-operations! reactor failure))
      (with-mutex (lws-reactor-mutex reactor)
        (lws-reactor-current-poll-snapshot-set!
         reactor (lws-context-poll-snapshot (lws-reactor-native-context reactor))))
      (lws-context-close (lws-reactor-native-context reactor))
      (with-mutex (lws-reactor-mutex reactor)
        (lws-reactor-lifecycle-set! reactor 'stopped)
        (condition-broadcast (lws-reactor-condition reactor)))))

  #|proc:lws-reactor?
The `lws-reactor?` procedure returns whether `value` is a private LWS reactor.
The `value` parameter may be any Scheme object.
|#
  (define lws-reactor?
    (lambda (value)
      (pcheck () (reactor? value))))

  (define make-reactor-from-context
    (lambda (context command-capacity)
      (let ([reactor
             (%make-lws-reactor
              context (make-mutex 'lws-reactor) (make-condition 'lws-reactor)
              command-capacity (make-vector command-capacity) #f #f #f 0 0 0 0
              command-capacity (make-vector command-capacity) #f 0 0 0
              '() '() '#() 'created -1 #f #f)])
        (initialize-command-pool! reactor)
        (initialize-waiter-pool! reactor)
        reactor)))

  #|proc:make-lws-reactor
The `make-lws-reactor` procedure creates an unstarted serialized LWS reactor.
`event-capacity` bounds native events, `payload-capacity` bounds copied payloads, and
`command-capacity` bounds queued commands and operation waiters. `tls-context-handle` is zero or a
native TLS context. `proxy-address` and `proxy-port` select the context proxy. It returns the
reactor.
|#
  (define make-lws-reactor
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-reactor event-capacity payload-capacity command-capacity 0 "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (make-lws-reactor event-capacity payload-capacity command-capacity
                         tls-context-handle "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
                [natural? tls-context-handle proxy-port]
                [string? proxy-address])
         (make-reactor-from-context
          (lws-context-open event-capacity payload-capacity tls-context-handle
                            proxy-address proxy-port)
          command-capacity))]))

  #|proc:make-lws-server-reactor
The `make-lws-server-reactor` procedure creates an unstarted listening reactor. `event-capacity`,
`payload-capacity`, and `command-capacity` bound native and Scheme queues. `interface-name` and
`port` select the listener, while `tls-context-handle` is zero or a native TLS context. It returns
the reactor. Optional `version` selects `h1` (default) or `h2`, using prior knowledge for
cleartext H2 and ALPN for TLS.
|#
  (define make-lws-server-reactor
    (case-lambda
      [(event-capacity payload-capacity command-capacity interface-name port tls-context-handle)
       (make-lws-server-reactor event-capacity payload-capacity command-capacity interface-name
                                port tls-context-handle 'h1)]
      [(event-capacity payload-capacity command-capacity interface-name port tls-context-handle version)
      (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
               [string? interface-name]
               [fixnum? port]
               [natural? tls-context-handle] [symbol? version])
        (make-reactor-from-context
         (lws-server-context-open event-capacity payload-capacity interface-name port
                                  tls-context-handle version)
         command-capacity))]))

  #|proc:lws-reactor-state
The `lws-reactor-state` procedure returns the lifecycle symbol for `reactor`.
The result is `created`, `starting`, `running`, `stopping`, or `stopped`.
|#
  (define lws-reactor-state
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-lifecycle reactor)))))

  #|proc:lws-reactor-owner-id
The `lws-reactor-owner-id` procedure returns the owner thread id for `reactor`.
It returns zero before the owner thread starts.
|#
  (define lws-reactor-owner-id
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (max 0 (lws-reactor-owner-thread-id reactor))))))

  #|proc:lws-reactor-wakeup-fd
The `lws-reactor-wakeup-fd` procedure returns the wakeup descriptor owned by `reactor`,
or `-1` once shutdown has begun.
|#
  (define lws-reactor-wakeup-fd
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (if (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
              -1
              (lws-context-wakeup-fd (lws-reactor-native-context reactor)))))))

  #|proc:lws-reactor-start!
The `lws-reactor-start!` procedure starts the owner thread for `reactor`.
It returns `#t` when started and `#f` when the reactor was already started.
|#
  (define lws-reactor-start!
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (if (eq? 'created (lws-reactor-lifecycle reactor))
              (begin
                (lws-reactor-lifecycle-set! reactor 'starting)
                (lws-reactor-thread-set!
                 reactor (fork-thread (lambda () (reactor-main reactor))))
                #t)
              #f)))))

  #|proc:lws-reactor-shutdown!
The `lws-reactor-shutdown!` procedure stops `reactor`, joins its owner, and releases its context.
Concurrent calls wait for the same shutdown; repeated calls are inert. A call from the reactor
owner requests shutdown and returns without joining itself. The return value is `reactor`.
|#
  (define lws-reactor-shutdown!
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (let ([action
               (with-mutex (lws-reactor-mutex reactor)
                 (case (lws-reactor-lifecycle reactor)
                   [(created)
                    (lws-reactor-owner-thread-id-set! reactor (get-thread-id))
                    (lws-reactor-lifecycle-set! reactor 'stopping)
                    'close]
                   [(starting running)
                    (lws-reactor-lifecycle-set! reactor 'stopping)
                    ;; Keep lifecycle validation and the native wakeup atomic with close.
                    (lws-context-wakeup (lws-reactor-native-context reactor))
                    'wait]
                   [(stopping) 'wait]
                   [else 'done]))])
          (case action
            [(close)
             (fail-pending-operations!
              reactor
              (make-network-condition 'lws-reactor-shutdown! "reactor stopped" '()))
             (with-mutex (lws-reactor-mutex reactor)
               (lws-reactor-current-poll-snapshot-set!
                reactor (lws-context-poll-snapshot (lws-reactor-native-context reactor))))
             (lws-context-close (lws-reactor-native-context reactor))
             (with-mutex (lws-reactor-mutex reactor)
               (lws-reactor-lifecycle-set! reactor 'stopped)
               (condition-broadcast (lws-reactor-condition reactor)))]
            [(wait)
             (unless (with-mutex (lws-reactor-mutex reactor)
                       (= (get-thread-id) (lws-reactor-owner-thread-id reactor)))
               (with-mutex (lws-reactor-mutex reactor)
                 (let wait ()
                   (unless (eq? 'stopped (lws-reactor-lifecycle reactor))
                     (condition-wait (lws-reactor-condition reactor)
                                     (lws-reactor-mutex reactor))
                     (wait))))
               (let ([thread (lws-reactor-thread reactor)])
                 (when thread (thread-join thread))))]
            [else (void)]))
        reactor)))

  #|proc:make-lws-reactor-operation
The `make-lws-reactor-operation` procedure registers a pending operation with `reactor`.
`kind` identifies the operation. The three identity parameters route copied native events.
`deadline-ms` is an absolute monotonic deadline or `#f`. The optional `retain?` keeps routing state
until `lws-reactor-release-operation!` is called. It returns a network operation.
|#
  (define make-lws-reactor-operation
    (case-lambda
      [(reactor kind connection-id stream-id generation deadline-ms)
       (make-lws-reactor-operation reactor kind connection-id stream-id generation
                                   deadline-ms #f)]
      [(reactor kind connection-id stream-id generation deadline-ms retain?)
       (pcheck ([reactor? reactor] [symbol? kind]
                [natural? connection-id stream-id generation]
                [(lambda (value) (or (not value) (natural? value))) deadline-ms]
                [boolean? retain?])
        (with-mutex (lws-reactor-mutex reactor)
          (when (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
            (errorf 'make-lws-reactor-operation "reactor is shutting down"))
          (let* ([signal (lws-signal-open (lws-reactor-native-context reactor))]
                 [state
                  (begin
                    (when (zero? signal)
                      (errorf 'make-lws-reactor-operation
                              "reactor operation signal pool is exhausted"))
                    (%make-reactor-operation-state
                     connection-id stream-id generation signal
                     (make-poll-target (lws-signal-fd signal) '(read))
                     deadline-ms retain? 'pending '() #f #f #f))]
                [operation #f])
            (set! operation
                  (make-net-operation
                   kind
                   (lambda ()
                     (with-mutex (lws-reactor-mutex reactor)
                       (let ([signal (reactor-operation-state-signal state)])
                         (unless (zero? signal) (lws-signal-drain signal))))
                     (when deadline-ms
                       (let* ([time (current-time 'time-monotonic)]
                              [now (+ (* (time-second time) 1000)
                                      (quotient (time-nanosecond time) 1000000))]
                              [expired?
                               (with-mutex (lws-reactor-mutex reactor)
                                 (and (eq? 'pending (reactor-operation-state-lifecycle state))
                                      (>= now deadline-ms)
                                      (begin
                                        (reactor-operation-state-lifecycle-set! state 'failed)
                                        (reactor-operation-state-failure-set!
                                         state (make-net-error 'lws-reactor 'timeout
                                                               "HTTP operation deadline expired"
                                                               (list connection-id stream-id
                                                                     generation)))
                                        #t)))])
                         (when expired?
                           (enqueue-command! reactor 'cancel
                                             (list connection-id stream-id generation 0)))))
                     (with-mutex (lws-reactor-mutex reactor)
                       (case (reactor-operation-state-lifecycle state)
                         [(pending)
                          (net-operation-pending
                           (list (reactor-operation-state-poll-target state))
                           (reactor-operation-state-deadline-ms state))]
                         [(completed)
                          (net-operation-completed
                           (reactor-operation-state-result state))]
                         [else
                          (net-operation-failed
                           (reactor-operation-state-failure state))])))
                   (lambda ()
                     (with-mutex (lws-reactor-mutex reactor)
                       (reactor-operation-state-lifecycle-set! state 'cancelled)
                       (reactor-operation-state-failure-set!
                        state (make-network-condition kind "operation cancelled" '())))
                     (enqueue-command!
                      reactor 'cancel
                      (list connection-id stream-id generation 0)))
                   (lambda ()
                     (with-mutex (lws-reactor-mutex reactor)
                       (close-operation-signal-locked! state)
                       (release-operation-waiters-locked! reactor state)
                       (unless (reactor-operation-state-retain? state)
                         (lws-reactor-operations-set!
                          reactor
                          (remp (lambda (entry) (eq? operation (car entry)))
                                (lws-reactor-operations reactor))))))))
            (lws-reactor-operations-set!
             reactor (cons (cons operation state) (lws-reactor-operations reactor)))
            operation)))]))

  #|proc:lws-reactor-release-operation!
The `lws-reactor-release-operation!` procedure removes retained `operation` routing state from
`reactor` and closes its signal. Both the native lifecycle and the Scheme operation must be
terminal, so no waiter can still depend on that signal. The return value is unspecified.
|#
  (define-who lws-reactor-release-operation!
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        ;; Read the operation state before taking the reactor mutex to preserve lock order.
        (when (eq? 'pending (net-operation-state operation))
          (errorf who "cannot release an operation before Scheme completion"))
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (when state
              (when (eq? 'pending (reactor-operation-state-lifecycle state))
                (errorf who "cannot release a pending operation"))
              (close-operation-signal-locked! state)
              (release-operation-waiters-locked! reactor state)
              (lws-reactor-operations-set!
               reactor
               (remp (lambda (entry) (eq? operation (car entry)))
                     (lws-reactor-operations reactor)))))))))

  #|proc:lws-reactor-operation-events
The `lws-reactor-operation-events` procedure returns copied events received for `operation`.
The `reactor` parameter owns `operation`. Events are returned in callback order.
|#
  (define-who lws-reactor-operation-events
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (reverse (reactor-operation-state-events state)))))))

  #|proc:lws-reactor-drain-operation-events!
The `lws-reactor-drain-operation-events!` procedure destructively removes and returns all copied
events currently queued for `operation`. The reactor retains no payload after this call.
|#
  (define-who lws-reactor-drain-operation-events!
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (let ([events (reverse (reactor-operation-state-events state))])
              (reactor-operation-state-events-set! state '())
              events))))))

  #|proc:lws-reactor-operation-lifecycle
The `lws-reactor-operation-lifecycle` procedure returns the internal lifecycle of `operation`.
The `reactor` parameter owns `operation`.
|#
  (define-who lws-reactor-operation-lifecycle
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (reactor-operation-state-lifecycle state))))))

  #|proc:lws-reactor-register-waiter!
The `lws-reactor-register-waiter!` procedure registers `procedure` for `operation` completion.
`procedure` has signature `(net-operation) -> unspecified`. The `reactor` owns the operation.
Notification exceptions are ignored so one observer cannot interrupt other waiters or shutdown.
It returns an unregister procedure with signature `() -> unspecified`, or `#f` on exhaustion.
|#
  (define-who lws-reactor-register-waiter!
    (lambda (reactor operation procedure)
      (pcheck ([reactor? reactor] [net-operation? operation] [procedure? procedure])
        (let ([notify? #f]
              [unregister #f])
          (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)]
                [waiter (lws-reactor-waiter-free reactor)])
            (unless state (errorf who "operation is not owned by reactor"))
            (cond
             [(not (eq? 'pending (reactor-operation-state-lifecycle state)))
              (set! notify? #t)
              (set! unregister void)]
             [(not waiter)
                (begin
                  (lws-reactor-waiter-misses-set!
                   reactor (fx1+ (lws-reactor-waiter-misses reactor)))
                  (set! unregister #f))]
             [else
                  (lws-reactor-waiter-free-set! reactor (reactor-waiter-next waiter))
                  (reactor-waiter-operation-set! waiter operation)
                  (reactor-waiter-procedure-set! waiter procedure)
                  (reactor-waiter-active?-set! waiter #t)
                  (reactor-waiter-next-set!
                   waiter (reactor-operation-state-waiters state))
                  (reactor-operation-state-waiters-set! state waiter)
                  (lws-reactor-waiter-in-use-set!
                   reactor (fx1+ (lws-reactor-waiter-in-use reactor)))
                  (when (fx> (lws-reactor-waiter-in-use reactor)
                             (lws-reactor-waiter-high-water reactor))
                    (lws-reactor-waiter-high-water-set!
                     reactor (lws-reactor-waiter-in-use reactor)))
                  (set!
                   unregister
                   (lambda ()
                     (with-mutex (lws-reactor-mutex reactor)
                       (when (reactor-waiter-active? waiter)
                         (let remove ([current (reactor-operation-state-waiters state)]
                                      [previous #f])
                           (cond
                            [(not current) (void)]
                            [(eq? current waiter)
                             (if previous
                                 (reactor-waiter-next-set!
                                  previous (reactor-waiter-next current))
                                 (reactor-operation-state-waiters-set!
                                  state (reactor-waiter-next current)))
                             (waiter-release-locked! reactor current)]
                            [else
                             (remove (reactor-waiter-next current) current)]))))))])))
          (when notify? (notify-waiter! procedure operation))
          unregister))))

  #|proc:lws-reactor-client-acquire!
The `lws-reactor-client-acquire!` procedure reserves a logical stream lease on the reactor's
serialized owner. It returns whether the bounded command was accepted.
|#
  (define lws-reactor-client-acquire!
    (lambda (reactor connection-id stream-id generation)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation])
        (enqueue-command! reactor 'acquire
                          (list connection-id stream-id generation)))))

  #|proc:lws-reactor-client-release!
The `lws-reactor-client-release!` procedure queues release of a terminal stream lease. It returns
whether the bounded command was accepted.
|#
  (define lws-reactor-client-release!
    (lambda (reactor connection-id stream-id generation)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation])
        (enqueue-command! reactor 'release
                          (list connection-id stream-id generation)))))

  #|proc:lws-reactor-client-start!
The `lws-reactor-client-start!` procedure queues an HTTP client start command on `reactor`.
The identity and generation parameters route events. Address, port, TLS, method, host, and path
parameters configure the LWS request. It returns whether the bounded command pool accepted it.
|#
  (define lws-reactor-client-start!
    (case-lambda
      [(reactor connection-id stream-id generation address port tls? method host path)
       (lws-reactor-client-start! reactor connection-id stream-id generation address port tls?
                                   method host path #vu8() #vu8() #f "http/1.1")]
      [(reactor connection-id stream-id generation address port tls? method host path headers
                initial-body has-body?)
       (lws-reactor-client-start! reactor connection-id stream-id generation address port tls?
                                   method host path headers initial-body has-body? "http/1.1")]
      [(reactor connection-id stream-id generation address port tls? method host path headers
                initial-body has-body? alpn)
       (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
                [string? address method host path] [fixnum? port] [boolean? tls?]
                [bytevector? headers initial-body] [boolean? has-body?] [string? alpn])
         (enqueue-command!
          reactor 'start
          (list connection-id stream-id generation address port tls? method host path headers
                initial-body has-body? alpn)))]))

  #|proc:lws-reactor-submit-body!
The `lws-reactor-submit-body!` procedure queues one request body `payload` on `reactor`.
The identity and generation parameters select the stream. `final?` marks the last chunk.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-submit-body!
    (lambda (reactor connection-id stream-id generation payload final?)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
               [bytevector? payload] [boolean? final?])
        (enqueue-command! reactor 'submit-body
                          (list connection-id stream-id generation payload final?)))))

  #|proc:lws-reactor-consume-body!
The `lws-reactor-consume-body!` procedure acknowledges `byte-count` response bytes on `reactor`.
The identity and generation parameters select the stream. Pending acknowledgements for that
stream are combined in the bounded queue. It returns command acceptance.
|#
  (define lws-reactor-consume-body!
    (lambda (reactor connection-id stream-id generation byte-count)
      (pcheck ([reactor? reactor]
               [natural? connection-id stream-id generation byte-count])
        (enqueue-command! reactor 'consume-body
                          (list connection-id stream-id generation byte-count)))))

  #|proc:lws-reactor-close-stream!
The `lws-reactor-close-stream!` procedure queues cancellation of a stream on `reactor`.
The identity and generation parameters select the stream, and `status` describes the reason.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-close-stream!
    (lambda (reactor connection-id stream-id generation status)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
               [fixnum? status])
        (enqueue-command! reactor 'cancel
                          (list connection-id stream-id generation status)))))

  #|proc:lws-reactor-server-request-dequeue!
The `lws-reactor-server-request-dequeue!` procedure removes the oldest copied server event from
`reactor`. Poll bookkeeping is excluded. It returns `#f` when no logical request event is ready.
|#
  (define lws-reactor-server-request-dequeue!
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([event* (lws-reactor-server-events reactor)])
            (if (null? event*)
                #f
                (begin
                  (lws-reactor-server-events-set! reactor (cdr event*))
                  (car event*))))))))

  #|proc:lws-reactor-server-submit-response!
The `lws-reactor-server-submit-response!` procedure queues response `payload` and `status` for the
identified logical request. `final?` marks the final body chunk. It returns command acceptance.
Optional `headers` contains encoded metadata to send before the first chunk.
|#
  (define lws-reactor-server-submit-response!
    (case-lambda
      [(reactor connection-id stream-id generation status payload final?)
       (lws-reactor-server-submit-response! reactor connection-id stream-id generation status
                                           #vu8() payload final?)]
      [(reactor connection-id stream-id generation status headers payload final?)
      (pcheck ([reactor? reactor]
               [natural? connection-id stream-id generation]
               [fixnum? status]
               [bytevector? headers payload]
               [boolean? final?])
        (enqueue-command! reactor 'server-response
                          (list connection-id stream-id generation status headers payload
                                final?)))]))

  #|proc:lws-reactor-inject-event!
The `lws-reactor-inject-event!` procedure queues a copied fake native event on `reactor`.
The identity, generation, `status`, and bytevector `payload` parameters form the event.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-inject-event!
    (case-lambda
      [(reactor event-tag connection-id stream-id generation status payload)
       (lws-reactor-inject-event! reactor event-tag connection-id stream-id generation status
                                   payload 0 #f 0 #f 0)]
      [(reactor event-tag connection-id stream-id generation status payload protocol reusable?
                peer-h2-capacity peer-h2-capacity-known terminal-scope)
       (pcheck ([reactor? reactor] [lws-event-tag? event-tag]
                [natural? connection-id stream-id generation]
                [fixnum? status peer-h2-capacity]
                [lws-observed-protocol? protocol]
                [lws-terminal-scope? terminal-scope]
                [bytevector? payload]
                [boolean? reusable? peer-h2-capacity-known])
         (enqueue-command! reactor 'inject-event
                           (list event-tag connection-id stream-id generation status payload
                                 protocol reusable? peer-h2-capacity peer-h2-capacity-known
                                 terminal-scope)))]))

  #|proc:lws-reactor-inject-poll!
The `lws-reactor-inject-poll!` procedure queues fake poll `operation` for `descriptor`.
The `events` parameter is the native poll mask. It returns whether the command was accepted.
|#
  (define lws-reactor-inject-poll!
    (lambda (reactor operation descriptor events)
      (pcheck ([reactor? reactor] [lws-poll-operation? operation]
               [reactor-file-descriptor? descriptor] [fixnum? events])
        (enqueue-command! reactor 'inject-poll (list operation descriptor events)))))

  #|proc:lws-reactor-poll-snapshot
The `lws-reactor-poll-snapshot` procedure returns the latest native poll snapshot for `reactor`.
After shutdown it returns the final snapshot captured before closing the native context.
|#
  (define lws-reactor-poll-snapshot
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          ;; Native descriptor storage changes under its own lock. Copy only for observers;
          ;; after shutdown retain the final snapshot without touching freed storage.
          (unless (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
            (lws-reactor-current-poll-snapshot-set!
             reactor (lws-context-poll-snapshot (lws-reactor-native-context reactor))))
          (lws-reactor-current-poll-snapshot reactor)))))

  #|proc:lws-reactor-pool-metrics
The `lws-reactor-pool-metrics` procedure returns command and waiter pool metrics for `reactor`.
The vector contains command capacity, in-use, high-water, misses, exhaustions, then waiter
capacity, in-use, high-water, and misses.
|#
  (define lws-reactor-pool-metrics
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (vector (lws-reactor-command-capacity reactor)
                  (lws-reactor-command-in-use reactor)
                  (lws-reactor-command-high-water reactor)
                  (lws-reactor-command-misses reactor)
                  (lws-reactor-command-exhaustions reactor)
                  (lws-reactor-waiter-capacity reactor)
                  (lws-reactor-waiter-in-use reactor)
                  (lws-reactor-waiter-high-water reactor)
                  (lws-reactor-waiter-misses reactor))))))
  )
