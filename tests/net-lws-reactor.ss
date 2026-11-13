(import (cspp) (cspp system) (cspp net)
        (cspp net socket)
        (cspp net address)
        (cspp net poll)
        (cspp net private lws-ffi)
        (cspp net private lws-reactor))

(define wait-until
  (lambda (predicate)
    (let loop ([remaining 200])
      (cond
       [(predicate) #t]
       [(zero? remaining) #f]
       [else
        (milisleep 1)
        (loop (- remaining 1))]))))

(define poll-snapshot-find
  (lambda (snapshot descriptor)
    (let loop ([index 0])
      (and (< index (vector-length snapshot))
           (let ([entry (vector-ref snapshot index)])
             (if (= descriptor (vector-ref entry 0))
                 entry
                 (loop (+ index 1))))))))

(define call-with-lws-context
  (lambda (procedure)
    (let ([context (lws-context-open 32 16)])
      (dynamic-wind
        void
        (lambda () (procedure context))
        (lambda () (lws-context-close context))))))

(mat net-lws-context-lifecycle
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (positive? context)
               (fixnum? (lws-context-wakeup-fd context))
               (vector? (lws-context-poll-snapshot context))
               (memv (lws-context-timeout-ms context 1000) '(0 1000))
               (begin
                 (lws-context-wakeup context)
                 (lws-context-service-fd context (lws-context-wakeup-fd context) 1)
                 #t))))))

(define concurrent-context-lifecycle
  (lambda (mixed?)
    (let* ([worker-count 8] [rounds 100]
           [mutex (make-mutex)] [ready (make-condition)] [started 0]
           [results (make-vector worker-count #f)]
           [workers
            (map
             (lambda (index)
               (fork-thread
                (lambda ()
                  (with-mutex mutex
                    (set! started (fx1+ started))
                    (condition-broadcast ready)
                    (let wait ()
                      (unless (fx= started worker-count)
                        (condition-wait ready mutex)
                        (wait))))
                  (vector-set! results index
                    (guard (failure [else failure])
                      (let loop ([remaining rounds])
                        (or (fxzero? remaining)
                            (and
                             (if (and mixed? (odd? index))
                                 (let ([server (websocket-listen "127.0.0.1" 0)])
                                   (dynamic-wind
                                     void
                                     (lambda ()
                                       (and (websocket-server? server)
                                            (net-would-block?
                                             (websocket-accept/nonblocking server))))
                                     (lambda () (websocket-server-close server))))
                                 (call-with-lws-context
                                  (lambda (context)
                                    (and (fx>= (lws-context-service-fd context -1 0) 0)
                                         (vector? (lws-context-poll-snapshot context))))))
                             (loop (fx1- remaining))))))))))
             (iota worker-count))])
      (for-each thread-join workers)
      (for-all (lambda (result) (eq? #t result)) (vector->list results)))))

(mat net-lws-independent-contexts-concurrent-lifecycle
     ;; Independent reactors may create, service, and destroy contexts on different
     ;; OS threads. Sharing LWS's unprotected default logging refcount can abort even
     ;; with logging disabled when the dependency was built with `LWS_MAX_SMP=1`.
     (mat-requires (websockets)
       (concurrent-context-lifecycle #f))

     ;; Separate lifecycle locks for HTTP and WebSocket would miss this race.
     (mat-requires (websockets)
       (concurrent-context-lifecycle #t)))

(mat net-lws-native-service-wakeup-and-collection
     ;; A native wait must release Chez so a peer can collect and wake the owner.
     (mat-requires (websockets)
       (guard (failure [else #f])
         (let ([service (foreign-procedure "cspp_lws_http_context_service" (uptr int) int)])
           (call-with-lws-context
            (lambda (context)
              (let* ([worker (fork-thread
                              (lambda ()
                                (milisleep 10)
                                (collect)
                                (lws-context-wakeup context)))]
                     [ready (service context 1000)])
                (thread-join worker)
      (and (positive? ready)
                     (null? (poll-target-ready-events
                             (car (poll (list (make-poll-target
                                               (lws-context-wakeup-fd context) '(read)))
                                         0))))))))))))

(mat net-lws-copied-event-order
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (lws-context-inject-event! context 'headers 10 11 3 200 #vu8(1 2))
          (lws-context-inject-event! context 'readable 10 11 3 0 #vu8(3 4 5))
          (lws-context-inject-event! context 'complete 10 11 3 0 #vu8())
          (let* ([headers (lws-context-next-event context)]
                 [readable (lws-context-next-event context)]
                 [_ (lws-body-consumed context 10 11 3 3)]
                 [complete (lws-context-next-event context)])
            (and (eq? (vector-ref headers 0) 'headers)
                 (positive? (vector-ref headers 1))
                 (= (vector-ref headers 2) 10)
                 (= (vector-ref headers 3) 11)
                 (= (vector-ref headers 4) 3)
                 (= (vector-ref headers 5) 200)
                 (equal? (vector-ref headers 6) #vu8(1 2))
                 (eq? (vector-ref readable 0) 'readable)
                 (equal? (vector-ref readable 6) #vu8(3 4 5))
                 (eq? (vector-ref complete 0) 'complete)
                 (not (lws-context-next-event context))))))))

(mat net-lws-terminal-waits-for-readable-consumption
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (lws-context-inject-event! context 'headers 12 13 4 200 #vu8(1))
               (lws-context-inject-event! context 'readable 12 13 4 0 #vu8(2 3 4))
               (lws-context-inject-event! context 'headers 12 13 4 -1 #vu8(5))
               (lws-context-inject-event! context 'complete 12 13 4 0 #vu8())
               (eq? (vector-ref (lws-context-next-event context) 0) 'headers)
               (eq? (vector-ref (lws-context-next-event context) 0) 'readable)
               (eq? (vector-ref (lws-context-next-event context) 0) 'headers)
               (not (lws-context-next-event context))
               (lws-body-consumed context 12 13 4 3)
               (eq? (vector-ref (lws-context-next-event context) 0) 'complete)
               (not (lws-context-next-event context)))))))

(mat net-lws-fake-callback-tags
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (let ([tags '(connected headers writable closed failed reset goaway)])
            (let inject ([pending tags] [stream-id 21])
              (unless (null? pending)
                (lws-context-inject-event!
                 context (car pending) 20 stream-id 7 0 #vu8(9))
                (inject (cdr pending) (+ stream-id 1))))
            (let loop ([expected tags] [stream-id 21])
              (if (null? expected)
                  (not (lws-context-next-event context))
                  (let ([event (lws-context-next-event context)])
                    (and (eq? (vector-ref event 0) (car expected))
                         (= (vector-ref event 2) 20)
                         (= (vector-ref event 3) stream-id)
                         (= (vector-ref event 4) 7)
                         (loop (cdr expected) (+ stream-id 1)))))))))))

(mat net-lws-observed-transport-metadata
     (mat-requires (websockets)
       (guard (condition [else #f])
         (call-with-lws-context
          (lambda (context)
            (and (lws-context-inject-event!
                  context 'connected 24 25 1 200 #vu8() 'http2 #f 37 #t 'none)
                 (lws-context-inject-event!
                  context 'complete 26 27 1 0 #vu8() 'http1 #t 0 #f 'stream)
                 (lws-context-inject-event!
                  context 'reset 28 29 1 8 #vu8() 'http2 #f 37 #t 'stream)
                 (lws-context-inject-event!
                  context 'goaway 30 31 1 11 #vu8() 'http2 #f 37 #t 'connection)
                 (equal? (vector-ref (lws-context-next-event context) 7)
                         '#(http2 #f 37 none))
                 (equal? (vector-ref (lws-context-next-event context) 7)
                         '#(http1 #t #f stream))
                 (equal? (vector-ref (lws-context-next-event context) 7)
                         '#(http2 #f 37 stream))
                 (equal? (vector-ref (lws-context-next-event context) 7)
                         '#(http2 #f 37 connection))))))))

(mat net-lws-poll-change-seam
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (let loop ([descriptor 101])
                 (if (> descriptor 200)
                     #t
                     (and (lws-context-inject-poll! context 'add descriptor 1)
                          (lws-context-inject-poll! context 'change descriptor 5)
                          (lws-context-inject-poll! context 'delete descriptor 0)
                          (begin
                            (lws-context-next-event context)
                            (lws-context-next-event context)
                            (lws-context-next-event context)
                            (loop (+ descriptor 1))))))
               (lws-context-inject-poll! context 'add 101 1)
               (lws-context-inject-poll! context 'change 101 5)
               (= (vector-length (lws-context-poll-snapshot context)) 1)
               (= (vector-ref (vector-ref (lws-context-poll-snapshot context) 0) 0) 101)
               (= (vector-ref (vector-ref (lws-context-poll-snapshot context) 0) 1) 5)
               (lws-context-inject-poll! context 'delete 101 0)
               (zero? (vector-length (lws-context-poll-snapshot context)))
               (equal? (map (lambda (ignored)
                              (vector-ref (lws-context-next-event context) 0))
                            '(1 2 3))
                       '(poll-add poll-change poll-delete)))))))

(mat net-lws-live-poll-keeps-application-event-pool-free
     ;; Native descriptor changes update the snapshot without consuming body/event slots.
     (mat-requires (websockets)
       (let ([context (lws-context-open 8 65536)]
             [listener (open-socket 'inet 'stream)])
         (dynamic-wind
           void
           (lambda ()
             (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
             (socket-listen! listener 1)
             (let ([port (socket-address-port (socket-local-address listener))])
               (and (lws-client-start context 1 1 1 "127.0.0.1" port #f
                                      "GET" "127.0.0.1" "/")
                    (positive? (vector-length (lws-context-poll-snapshot context)))
                    (zero? (vector-ref (lws-context-pool-metrics context) 1)))))
           (lambda () (lws-context-close context) (close-socket listener))))))

(mat net-lws-stream-exhaustion-releases-connection
     ;; Error case: acquiring a connection before stream exhaustion must not leak the connection.
     (mat-requires (websockets)
       (let ([context (lws-context-open 2 4)])
         (dynamic-wind
           void
           (lambda ()
             (and (lws-context-inject-event! context 'writable 1 1 1 0 #vu8())
                  (begin (lws-context-next-event context) #t)
                  (lws-context-inject-event! context 'writable 1 2 1 0 #vu8())
                  (begin (lws-context-next-event context) #t)
                  (not (lws-context-inject-event! context 'writable 2 3 1 0 #vu8()))
                  (lws-context-inject-event! context 'complete 1 1 1 0 #vu8())
                  (begin (lws-context-next-event context) #t)
                  (lws-context-inject-event! context 'complete 1 2 1 0 #vu8())
                  (begin (lws-context-next-event context) #t)
                  (zero? (vector-ref (lws-context-pool-metrics context) 8))))
           (lambda () (lws-context-close context))))))

(mat net-lws-body-flow-control
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(1 2 3))
               (not (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(4)))
               (begin (lws-context-next-event context) #t)
               (lws-body-consumed context 30 31 2 3)
               (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(4))
               (begin (lws-context-next-event context) #t)
               (lws-body-consumed context 30 31 2 1))))))

(mat net-lws-consumption-does-not-wake-owner
     ;; Consumption runs on the context owner; it must not leave a spurious poll wakeup.
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (lws-context-inject-event! context 'readable 32 33 2 0 #vu8(1 2 3))
               (begin (lws-context-next-event context) #t)
               (lws-body-consumed context 32 33 2 3)
               (null? (poll-target-ready-events
                       (car (poll (list (make-poll-target
                                         (lws-context-wakeup-fd context) '(read))) 0)))))))))

(mat net-lws-generation-filtering
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (and (lws-context-inject-event! context 'writable 40 41 9 0 #vu8())
               (not (lws-context-inject-event! context 'writable 40 41 8 0 #vu8()))
               (begin (lws-context-next-event context) #t)
               (lws-context-inject-event! context 'reset 40 41 9 1 #vu8())
               (begin (lws-context-next-event context) #t)
               (not (lws-context-inject-event! context 'writable 40 41 8 0 #vu8()))
               (lws-context-inject-event! context 'writable 40 41 10 0 #vu8())
               (begin (lws-context-next-event context) #t))))))

(mat net-lws-bounded-event-pool
     ;; Error case: a full callback queue must fail deterministically instead of growing.
     (mat-requires (websockets)
       (let ([context (lws-context-open 2 4)])
         (dynamic-wind
           void
           (lambda ()
             (and (lws-context-inject-event! context 'writable 1 1 1 0 #vu8(1 2 3 4))
                  (lws-context-inject-event! context 'complete 1 1 1 0 #vu8())
                  (not (lws-context-inject-event! context 'writable 1 1 2 0 #vu8()))
                  (let ([metrics (lws-context-pool-metrics context)])
                    (and (= (vector-ref metrics 0) 2)
                         (= (vector-ref metrics 1) 2)
                         (= (vector-ref metrics 2) 2)
                         (positive? (vector-ref metrics 4))))))
           (lambda () (lws-context-close context))))))

(mat net-lws-pool-reuse
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (let loop ([generation 1])
            (if (> generation 100)
                (let ([metrics (lws-context-pool-metrics context)])
                  (and (= (vector-ref metrics 1) 0)
                       (= (vector-ref metrics 2) 1)
                       (zero? (vector-ref metrics 4))))
                (and (lws-context-inject-event!
                      context 'complete 50 51 generation 0 #vu8())
                     (vector? (lws-context-next-event context))
                     (loop (+ generation 1)))))))))

(mat net-lws-private-operation-boundary
     (and (procedure? lws-client-start)
          (procedure? lws-client-body-submit)
          (procedure? lws-client-body-drain)
          (procedure? lws-server-request-dequeue)
          (procedure? lws-server-response-submit)
          (procedure? lws-stream-cancel)
          (procedure? lws-body-consumed)))

(mat net-lws-reactor-lifecycle
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 16 16 8)])
         (and (lws-reactor? reactor)
              (eq? 'created (lws-reactor-state reactor))
              (lws-reactor-start! reactor)
              (wait-until (lambda () (eq? 'running (lws-reactor-state reactor))))
              (positive? (lws-reactor-owner-id reactor))
              (fixnum? (lws-reactor-wakeup-fd reactor))
              (eq? reactor (lws-reactor-shutdown! reactor))
              (eq? 'stopped (lws-reactor-state reactor))
              (eq? reactor (lws-reactor-shutdown! reactor))))))

(mat net-lws-reactor-enforces-deadline
     ;; Error case: a pending native operation must fail once its absolute deadline expires.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 8 64 8)]
              [operation (make-lws-reactor-operation reactor 'http2 1 1 1 0)])
         (dynamic-wind
           void
           (lambda ()
             (net-operation-step! operation)
             (and (eq? 'failed (net-operation-state operation))
                  (net-error? (net-operation-condition operation))
                  (eq? 'timeout (net-error-kind (net-operation-condition operation)))))
           (lambda () (lws-reactor-shutdown! reactor))))))

(mat net-lws-reactor-completion-fanout
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 8)]
              [first (make-lws-reactor-operation reactor 'first 1 11 1 #f)]
              [second (make-lws-reactor-operation reactor 'second 1 12 1 #f)]
              [notifications '()])
         (lws-reactor-register-waiter!
          reactor first (lambda (operation) (set! notifications (cons operation notifications))))
         (lws-reactor-register-waiter!
          reactor second (lambda (operation) (set! notifications (cons operation notifications))))
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 1 12 1 0 #vu8(2))
         (lws-reactor-inject-event! reactor 'complete 1 11 1 0 #vu8(1))
         (let ([ready?
                (wait-until
                 (lambda ()
                   (and (pair? (lws-reactor-operation-events reactor first))
                        (pair? (lws-reactor-operation-events reactor second)))))])
           (net-operation-step! second)
           (net-operation-step! first)
           (lws-reactor-shutdown! reactor)
           (and ready?
                (eq? 'completed (net-operation-state first))
                (eq? 'completed (net-operation-state second))
                (equal? (vector-ref (net-operation-result first) 6) #vu8(1))
                (equal? (vector-ref (net-operation-result second) 6) #vu8(2))
                (= (length notifications) 2))))))

(mat net-lws-reactor-sequential-connection-reuse
     ;; Two terminal transactions may reuse one physical connection identity.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 8)]
              [first (make-lws-reactor-operation reactor 'first 71 1 1 #f)]
              [second (make-lws-reactor-operation reactor 'second 71 2 2 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 71 1 1 0 #vu8(1))
         (let ([first-ready?
                (wait-until (lambda ()
                              (eq? 'completed (lws-reactor-operation-lifecycle reactor first))))])
           (let* ([first-event (car (lws-reactor-operation-events reactor first))]
                  [_ (lws-reactor-inject-event! reactor 'complete 71 2 2 0 #vu8(2))]
                  [second-ready? (wait-until (lambda ()
                                               (eq? 'completed (lws-reactor-operation-lifecycle reactor second))))]
                  [second-event (car (lws-reactor-operation-events reactor second))])
             (net-operation-step! first)
             (net-operation-step! second)
             (lws-reactor-shutdown! reactor)
             (and first-ready? second-ready?
                  (= (vector-ref first-event 2) 71)
                  (= (vector-ref second-event 2) 71)))))))

(mat net-lws-reactor-multiplexed-stream-identities
     ;; Distinct logical streams route independently over one physical connection.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 8)]
              [first (make-lws-reactor-operation reactor 'h2-a 72 3 1 #f)]
              [second (make-lws-reactor-operation reactor 'h2-b 72 5 1 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 72 5 1 0 #vu8(5))
         (lws-reactor-inject-event! reactor 'complete 72 3 1 0 #vu8(3))
         (let ([ready?
                (wait-until (lambda ()
                              (and (eq? 'completed (lws-reactor-operation-lifecycle reactor first))
                                   (eq? 'completed (lws-reactor-operation-lifecycle reactor second)))))])
           (let* ([first-event (car (lws-reactor-operation-events reactor first))]
                  [second-event (car (lws-reactor-operation-events reactor second))])
             (net-operation-step! first)
             (net-operation-step! second)
             (lws-reactor-shutdown! reactor)
             (and ready?
                  (= (vector-ref first-event 3) 3)
                  (= (vector-ref second-event 3) 5)))))))

(mat net-lws-reactor-stale-generation-after-release
     ;; Events for a released generation must not reach a later operation.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 8)]
              [old (make-lws-reactor-operation reactor 'old 73 7 1 #f #t)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 73 7 1 0 #vu8())
         (let ([ready? (wait-until (lambda ()
                                    (pair? (lws-reactor-operation-events reactor old))))])
           (net-operation-step! old)
           (lws-reactor-release-operation! reactor old)
           (let ([new (make-lws-reactor-operation reactor 'new 73 7 2 #f)])
             (lws-reactor-inject-event! reactor 'complete 73 7 1 0 #vu8(9))
             (let ([stale? (wait-until (lambda ()
                                        (pair? (lws-reactor-operation-events reactor new))))])
               (lws-reactor-shutdown! reactor)
               (and ready? (not stale?)
                    (null? (lws-reactor-operation-events reactor new)))))))))

(mat net-lws-reactor-completed-operation-releases-signal
     ;; Completed operations must return their signal to the bounded native pool.
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 2 16 4)])
         (lws-reactor-start! reactor)
         (let loop ([identity 1])
           (if (> identity 4)
               (begin
                 (lws-reactor-shutdown! reactor)
                 #t)
               (let ([operation
                      (make-lws-reactor-operation reactor 'signal identity identity 1 #f)])
                 (lws-reactor-inject-event! reactor 'complete identity identity 1 0 #vu8())
                 (and (wait-until
                       (lambda ()
                         (pair? (lws-reactor-operation-events reactor operation))))
                      (begin
                        (net-operation-step! operation)
                        (eq? 'completed (net-operation-state operation)))
                      (loop (+ identity 1)))))))))

(mat net-lws-reactor-cancellation
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 8)]
              [operation (make-lws-reactor-operation reactor 'cancel 2 21 3 #f)]
              [unregister (lws-reactor-register-waiter! reactor operation void)])
         (lws-reactor-start! reactor)
         (net-operation-cancel! operation)
         (let ([cancelled?
                (wait-until
                 (lambda ()
                   (zero? (vector-ref (lws-reactor-pool-metrics reactor) 1))))])
           (lws-reactor-shutdown! reactor)
           (and cancelled?
                (zero? (vector-ref (lws-reactor-pool-metrics reactor) 6))
                (eq? 'cancelled (net-operation-state operation)))))))

(mat net-lws-reactor-waiter-unregister
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 4)]
              [operation (make-lws-reactor-operation reactor 'waiter 4 41 1 #f)]
              [notification-count 0]
              [unregister
               (lws-reactor-register-waiter!
                reactor operation
                (lambda (ignored) (set! notification-count (+ notification-count 1))))])
         (unregister)
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 4 41 1 0 #vu8())
         (let ([ready?
                (wait-until
                 (lambda ()
                   (pair? (lws-reactor-operation-events reactor operation))))])
           (net-operation-step! operation)
           (lws-reactor-shutdown! reactor)
           (and ready?
                (zero? notification-count)
                (zero? (vector-ref (lws-reactor-pool-metrics reactor) 6)))))))

(mat net-lws-reactor-late-waiter
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 4)]
              [operation (make-lws-reactor-operation reactor 'late-waiter 4 42 1 #f)]
              [notification-count 0])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 4 42 1 0 #vu8())
         (let ([ready?
                (wait-until
                 (lambda ()
                   (pair? (lws-reactor-operation-events reactor operation))))])
           (let ([unregister
                  (lws-reactor-register-waiter!
                   reactor operation
                   (lambda (ignored)
                     (set! notification-count (+ notification-count 1))))])
             (unregister)
             (net-operation-step! operation)
             (lws-reactor-shutdown! reactor)
             (and ready?
                  (= notification-count 1)
                  (zero? (vector-ref (lws-reactor-pool-metrics reactor) 6))))))))

(mat net-lws-reactor-poll-updates
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 16 16 8)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-poll! reactor 'add 301 1)
         (lws-reactor-inject-poll! reactor 'change 301 5)
         (let ([changed?
                (wait-until
                 (lambda ()
                   (let ([entry (poll-snapshot-find
                                 (lws-reactor-poll-snapshot reactor) 301)])
                     (and entry (= (vector-ref entry 1) 5)))))])
           (lws-reactor-inject-poll! reactor 'delete 301 0)
           (let ([deleted?
                  (wait-until
                   (lambda ()
                     (not (poll-snapshot-find
                           (lws-reactor-poll-snapshot reactor) 301))))])
             (lws-reactor-shutdown! reactor)
             (and changed? deleted?))))))

(mat net-lws-reactor-shutdown-fails-pending
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 4)]
              [operation (make-lws-reactor-operation reactor 'pending 5 51 1 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-shutdown! reactor)
         (net-operation-step! operation)
         (and (eq? 'failed (net-operation-state operation))
              (condition? (net-operation-condition operation))))))

(mat net-lws-reactor-failure-event
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 16 4)]
              [operation (make-lws-reactor-operation reactor 'failure 6 61 1 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'failed 6 61 1 111 #vu8())
         (let ([ready?
                (wait-until
                 (lambda ()
                   (pair? (lws-reactor-operation-events reactor operation))))])
           (net-operation-step! operation)
           (lws-reactor-shutdown! reactor)
           (and ready?
                (eq? 'failed (net-operation-state operation))
                (condition? (net-operation-condition operation)))))))

(mat net-lws-reactor-rejects-after-shutdown
     ;; Error case: a stopped reactor must reject new commands and operations.
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 16 16 4)])
         (lws-reactor-shutdown! reactor)
         (and (not (lws-reactor-inject-event! reactor 'writable 7 71 1 0 #vu8()))
              (guard (failure [else (condition? failure)])
                (make-lws-reactor-operation reactor 'late 7 71 1 #f)
                #f)))))

(mat net-lws-reactor-command-pool
     ;; Error case: commands beyond the configured pool fail without growing the pool.
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 16 16 2)])
         (and (lws-reactor-inject-event! reactor 'writable 3 31 1 0 #vu8())
              (lws-reactor-inject-event! reactor 'writable 3 32 1 0 #vu8())
              (not (lws-reactor-inject-event! reactor 'writable 3 33 1 0 #vu8()))
              (let ([metrics (lws-reactor-pool-metrics reactor)])
                (and (= (vector-ref metrics 0) 2)
                     (= (vector-ref metrics 1) 2)
                     (= (vector-ref metrics 2) 2)
                     (positive? (vector-ref metrics 4))))
              (lws-reactor-start! reactor)
              (wait-until (lambda () (zero? (vector-ref (lws-reactor-pool-metrics reactor) 1))))
              (lws-reactor-inject-event! reactor 'writable 3 34 1 0 #vu8())
              (begin (lws-reactor-shutdown! reactor) #t)))))

(mat net-lws-reactor-command-boundary
     (and (procedure? lws-reactor-client-start!)
          (procedure? lws-reactor-submit-body!)
          (procedure? lws-reactor-consume-body!)
          (procedure? lws-reactor-close-stream!)))

(mat net-lws-reactor-coalesces-body-consumption
     ;; Error case: many consumed chunks must not exhaust the bounded acknowledgement queue.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 8 1024 2)]
              [operation (make-lws-reactor-operation reactor 'consume 91 92 1 #f)])
         (dynamic-wind
           void
           (lambda ()
             (and (lws-reactor-inject-event! reactor 'readable 91 92 1 0
                                             (make-bytevector 100 7))
                  (for-all (lambda (index) (lws-reactor-consume-body! reactor 91 92 1 1))
                           (iota 100))
                  ;; Acknowledgements for another identity cannot merge into this stream.
                  (not (lws-reactor-consume-body! reactor 91 93 1 1))

                  ;; Error case: the same stream ID in a new generation is a separate lease.
                  (not (lws-reactor-consume-body! reactor 91 92 2 1))
                  (let ([metrics (lws-reactor-pool-metrics reactor)])
                    (and (= 2 (vector-ref metrics 0)) (= 2 (vector-ref metrics 1))))
                  (begin (lws-reactor-start! reactor) #t)
                  (wait-until
                   (lambda () (zero? (vector-ref (lws-reactor-pool-metrics reactor) 1))))
                  (lws-reactor-inject-event! reactor 'complete 91 92 1 0 #vu8())
                  (wait-until
                   (lambda () (eq? 'completed
                                    (lws-reactor-operation-lifecycle reactor operation))))
                  (begin (net-operation-step! operation)
                         (eq? 'completed (net-operation-state operation)))))
           (lambda () (lws-reactor-shutdown! reactor))))))

(mat net-lws-reactor-drain-operation-events-is-destructive
     ;; Draining copied events must remove them so retained payloads do not grow unbounded.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 8 8 4)]
              [operation (make-lws-reactor-operation reactor 'drain 81 82 1 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 81 82 1 0 #vu8(7))
         (let ([ready? (wait-until
                        (lambda ()
                          (eq? 'completed
                               (lws-reactor-operation-lifecycle reactor operation))))]
               [events (begin
                         (wait-until
                          (lambda ()
                            (pair? (lws-reactor-operation-events reactor operation))))
                         (lws-reactor-drain-operation-events! reactor operation))])
           (lws-reactor-shutdown! reactor)
           (and ready?
                (= (length events) 1)
                (null? (lws-reactor-drain-operation-events! reactor operation)))))))

(mat net-lws-reactor-command-rejection-publishes-terminal
     ;; A native command rejected after enqueue must still terminate its owning operation.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 8 8 4)]
              [operation (make-lws-reactor-operation reactor 'reject 91 92 1 #f)])
         (lws-reactor-start! reactor)
         ;; No native stream exists, so release is deterministically rejected.
         (lws-reactor-client-release! reactor 91 92 1)
         (let ([failed? (wait-until
                         (lambda ()
                           (eq? 'failed
                                (lws-reactor-operation-lifecycle reactor operation))))]
               [events (begin
                         (wait-until
                          (lambda ()
                            (pair? (lws-reactor-operation-events reactor operation))))
                         (lws-reactor-drain-operation-events! reactor operation))])
           (lws-reactor-shutdown! reactor)
           (and failed?
                (= (length events) 1)
                (eq? 'failed (vector-ref (car events) 0)))))))

(mat net-lws-native-rejects-oversized-request-metadata
     ;; Error case: headers and initial body larger than native payload storage are rejected.
     (mat-requires (websockets)
       (call-with-lws-context
        (lambda (context)
          (let ([metrics-before (lws-context-pool-metrics context)]
                [oversized (make-bytevector 17 1)])
            (and (not (lws-client-start context 101 102 1 "127.0.0.1" 1 #f
                                        "GET" "example.test" "/"
                                        oversized #vu8() #f))
                 (not (lws-client-start context 103 104 1 "127.0.0.1" 1 #f
                                        "POST" "example.test" "/"
                                        #vu8() oversized #t))
                 (= (vector-ref (lws-context-pool-metrics context) 8)
                    (vector-ref metrics-before 8))
                 (not (lws-context-next-event context))))))))

(mat net-lws-reactor-timeout-cancellation-ordering
     ;; Error case: an expired operation reports timeout before a later cancellation is inert.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 8 8 8)]
              [operation (make-lws-reactor-operation reactor 'timeout-order 201 202 1 0)])
         (dynamic-wind
           void
           (lambda ()
             (net-operation-step! operation)
             (let ([condition-before (net-operation-condition operation)])
               (net-operation-cancel! operation)
               (and (eq? 'failed (net-operation-state operation))
                    (eq? 'timeout (net-error-kind condition-before))
                    (eq? condition-before (net-operation-condition operation)))))
           (lambda () (lws-reactor-shutdown! reactor))))))

(mat net-lws-reactor-poll-events-are-not-server-requests
     ;; Poll bookkeeping must not accumulate in an idle client/server application queue.
     (mat-requires (websockets)
       (let ([reactor (make-lws-reactor 16 64 8)])
         (dynamic-wind
           (lambda () (lws-reactor-start! reactor))
           (lambda ()
             (lws-reactor-inject-poll! reactor 'add 901 1)
             (and
              (wait-until
               (lambda () (poll-snapshot-find (lws-reactor-poll-snapshot reactor) 901)))
              (let loop ([count 0])
                (if (= count 100)
                    #t
                    (let ([mask (if (even? count) 5 1)])
                      (and (lws-reactor-inject-poll! reactor 'change 901 mask)
                           (wait-until
                            (lambda ()
                              (let ([entry (poll-snapshot-find
                                            (lws-reactor-poll-snapshot reactor) 901)])
                                (and entry (= mask (vector-ref entry 1))))))
                           (loop (fx1+ count))))))
              (begin (lws-reactor-inject-poll! reactor 'delete 901 0) #t)
              (wait-until
               (lambda ()
                 (not (poll-snapshot-find (lws-reactor-poll-snapshot reactor) 901))))
              (begin
                ;; A logical server event is a barrier after the preceding poll commands.
                (lws-reactor-inject-event! reactor 'headers 42 43 1 200 #vu8(1))
                (lws-reactor-inject-event! reactor 'headers 42 44 1 200 #vu8(2))
                #t)
              (let ([first #f] [second #f])
                (and (wait-until
                      (lambda ()
                        (set! first (lws-reactor-server-request-dequeue! reactor)) first))
                     (wait-until
                      (lambda ()
                        (set! second (lws-reactor-server-request-dequeue! reactor)) second))
                     (eq? 'headers (vector-ref first 0))
                     (= 43 (vector-ref first 3))
                     (eq? 'headers (vector-ref second 0))
                     (= 44 (vector-ref second 3))
                     (not (lws-reactor-server-request-dequeue! reactor))))))
           (lambda () (lws-reactor-shutdown! reactor))))))

(mat net-lws-reactor-shutdown-before-terminal-cleanup
     ;; Error case: shutdown must invalidate signals before the context storage is freed.
     (mat-requires (websockets)
       (andmap
        (lambda (outcome)
          (let* ([reactor (make-lws-reactor 16 64 8)]
                 [operation (make-lws-reactor-operation reactor 'shutdown 501 502 1 #f #t)])
            (lws-reactor-start! reactor)
            (unless (eq? outcome 'pending)
              (lws-reactor-inject-event! reactor outcome 501 502 1 0 #vu8())
              (unless (wait-until
                       (lambda () (pair? (lws-reactor-operation-events reactor operation))))
                (error 'shutdown-test "terminal event was not published")))
            (lws-reactor-shutdown! reactor)
            (net-operation-step! operation)
            (let ([expected (if (eq? outcome 'complete) 'completed 'failed)])
              (lws-reactor-release-operation! reactor operation)
              (and (eq? expected (net-operation-state operation))
                   (eq? reactor (lws-reactor-shutdown! reactor))))))
        '(pending complete failed))))

(mat net-lws-reactor-cancel-after-shutdown
     ;; Error case: late cancellation must not close a signal from the destroyed context.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 64 8)]
              [operation (make-lws-reactor-operation reactor 'shutdown 601 602 1 #f)])
         (lws-reactor-start! reactor)
         (lws-reactor-shutdown! reactor)
         (net-operation-cancel! operation)
         (eq? 'cancelled (net-operation-state operation)))))

(mat net-lws-reactor-concurrent-shutdown-before-start
     ;; Error case: a second shutdown must wait for the first even without an owner thread.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 64 8)]
              [operation (make-lws-reactor-operation reactor 'shutdown 701 702 1 #f)]
              [callback-entered? #f] [release? #f] [failure #f])
         (lws-reactor-register-waiter!
          reactor operation
          (lambda (ignored)
            (set! callback-entered? #t)
            (let wait () (unless release? (milisleep 1) (wait)))))
         (let ([first (fork-thread (lambda () (lws-reactor-shutdown! reactor)))])
           (unless (wait-until (lambda () callback-entered?))
             (error 'shutdown-test "first shutdown did not reach its callback"))
           (let ([second
                  (fork-thread
                   (lambda ()
                     (guard (condition [else (set! failure condition)])
                       (lws-reactor-shutdown! reactor))))])
             (milisleep 10)
             (set! release? #t)
             (thread-join first)
             (thread-join second)
             (and (not failure)
                  (eq? 'stopped (lws-reactor-state reactor))
                  (begin (net-operation-step! operation) #t)
                  (eq? 'failed (net-operation-state operation))))))))

(mat net-lws-reactor-shutdown-from-owner-callback
     ;; Error case: a terminal waiter running on the owner must never join its own thread.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 64 8)]
              [operation (make-lws-reactor-operation reactor 'shutdown 801 802 1 #f #t)]
              [returned? #f])
         (lws-reactor-register-waiter!
          reactor operation
          (lambda (ignored)
            (lws-reactor-shutdown! reactor)
            (set! returned? #t)))
         (lws-reactor-start! reactor)
         (lws-reactor-inject-event! reactor 'complete 801 802 1 0 #vu8())
         (and (wait-until (lambda () (eq? 'stopped (lws-reactor-state reactor))))
              returned?
              (begin (net-operation-step! operation) #t)
              (eq? 'completed (net-operation-state operation))))))

(mat net-lws-reactor-rejects-release-before-scheme-cleanup
     ;; Error case: releasing before Scheme completion could drain a signal still used by waiters.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 1 64 8)]
              [operation (make-lws-reactor-operation reactor 'release 901 902 1 #f #t)])
         (dynamic-wind
           (lambda () (lws-reactor-start! reactor))
           (lambda ()
             (lws-reactor-inject-event! reactor 'complete 901 902 1 0 #vu8())
             (and (wait-until
                   (lambda ()
                     (eq? 'completed (lws-reactor-operation-lifecycle reactor operation))))
                  (guard (condition
                          [else (and (message-condition? condition)
                                     (string-contains? (condition-message condition)
                                                       "before Scheme completion") #t)])
                    (lws-reactor-release-operation! reactor operation)
                    #f)
                  (begin
                    (net-operation-step! operation)
                    (lws-reactor-release-operation! reactor operation)
                    ;; Normal terminal release leaves the one-slot signal pool reusable.
                    (make-lws-reactor-operation reactor 'next 901 903 1 #f)
                    #t)
                  (eq? 'completed (net-operation-state operation))))
           (lambda () (lws-reactor-shutdown! reactor))))))


(mat net-lws-reactor-shutdown-survives-waiter-failure
     ;; Error case: a user waiter throwing must not skip context release or the stopped broadcast.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 64 8)]
              [operation (make-lws-reactor-operation reactor 'shutdown 1001 1002 1 #f)]
              [called? #f])
         (lws-reactor-register-waiter!
          reactor operation
          (lambda (ignored)
            (set! called? #t)
            (error 'shutdown-waiter "deliberate callback failure")))
         (guard (condition [else #f])
           (and (eq? reactor (lws-reactor-shutdown! reactor))
                called?
                (eq? 'stopped (lws-reactor-state reactor))
                (= -1 (lws-reactor-wakeup-fd reactor))
                (eq? reactor (lws-reactor-shutdown! reactor)))))))


(mat net-lws-reactor-waiter-failure-is-isolated
     ;; Error case: a throwing observer must not stop publication or block later notifications.
     (mat-requires (websockets)
       (let* ([reactor (make-lws-reactor 16 64 8)]
              [first (make-lws-reactor-operation reactor 'waiter 1101 1102 1 #f #t)]
              [second (make-lws-reactor-operation reactor 'waiter 1101 1103 1 #f #t)]
              [notified? #f] [immediate? #f])
         (dynamic-wind
           (lambda () (lws-reactor-start! reactor))
           (lambda ()
             (lws-reactor-register-waiter!
              reactor first (lambda (ignored) (error 'waiter "deliberate callback failure")))
             (lws-reactor-register-waiter!
              reactor second (lambda (ignored) (set! notified? #t)))
             (lws-reactor-inject-event! reactor 'complete 1101 1102 1 0 #vu8())
             (lws-reactor-inject-event! reactor 'complete 1101 1103 1 0 #vu8())
             (and (wait-until (lambda () notified?))
                  (begin (net-operation-step! first) (net-operation-step! second) #t)
                  (eq? 'completed (net-operation-state first))
                  (eq? 'completed (net-operation-state second))
                  (procedure?
                   (lws-reactor-register-waiter!
                    reactor first
                    (lambda (ignored)
                      (set! immediate? #t)
                      (error 'waiter "deliberate immediate callback failure"))))
                  immediate?))
           (lambda () (lws-reactor-shutdown! reactor))))))
