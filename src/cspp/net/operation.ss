(library (cspp net operation)
  (export net-operation?
          make-net-operation
          net-operation-pending
          net-operation-completed
          net-operation-failed
          net-operation-kind
          net-operation-state
          net-operation-poll-targets
          net-operation-deadline-ms
          net-operation-remaining-timeout-ms
          net-operation-step!
          net-operation-cancel!
          net-operation-result
          net-operation-condition
          net-operation-register-listener!
          net-operation-wait
          net-would-block?
          make-net-would-block
          net-would-block-resource
          net-would-block-events)
  (import (cspp chez)
          (only (cspp utils) define-who natural? pcheck)
          (cspp net poll)
          (cspp net private operation))

  #|proc:net-operation?
The `net-operation?` procedure reports whether `value` is a network operation.
The `value` parameter may be any Scheme object.
The return value is `#t` for a network operation and `#f` otherwise.
|#
  (define net-operation?
    (lambda (value)
      (pcheck ()
              (%net-operation? value))))

  #|proc:make-net-operation
The `make-net-operation` procedure constructs a pending network operation of `kind`.
The `kind` parameter is a symbol identifying the operation.
The `advance` parameter has signature `() -> operation-update` and must never block.
The `cancel` parameter has signature `() -> unspecified` and cancels pending native work.
The optional `cleanup` parameter has signature `() -> unspecified` and releases resources.
The return value is a new pending network operation. Omitting `cleanup` uses `void`.
|#
  (define make-net-operation
    (case-lambda
      [(kind advance cancel)
       (make-net-operation kind advance cancel void)]
      [(kind advance cancel cleanup)
       (pcheck ([symbol? kind] [procedure? advance cancel cleanup])
               (%make-net-operation kind advance cancel cleanup
                                    (make-mutex 'net-operation)
                                    'pending '() #f #f #f '()))]))

  #|proc:net-operation-kind
The `net-operation-kind` procedure returns the kind symbol of `operation`.
The `operation` parameter is a network operation.
|#
  (define net-operation-kind
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (%net-operation-kind operation))))

  #|proc:net-operation-state
The `net-operation-state` procedure returns the lifecycle state of `operation`.
The `operation` parameter is a network operation.
The return value is `pending`, `completed`, `failed`, or `cancelled`.
|#
  (define net-operation-state
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (with-mutex (%net-operation-mutex operation)
                (%net-operation-state operation)))))

  #|proc:net-operation-poll-targets
The `net-operation-poll-targets` procedure returns the current poll targets of `operation`.
The `operation` parameter is a network operation.
The return value is a list of poll targets, empty when no descriptor readiness is required.
|#
  (define net-operation-poll-targets
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (with-mutex (%net-operation-mutex operation)
                (%net-operation-poll-targets operation)))))

  #|proc:net-operation-deadline-ms
The `net-operation-deadline-ms` procedure returns the absolute deadline of `operation`.
The `operation` parameter is a network operation.
The return value is monotonic milliseconds or `#f` when there is no deadline.
|#
  (define net-operation-deadline-ms
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (with-mutex (%net-operation-mutex operation)
                (%net-operation-deadline-ms operation)))))

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  #|proc:net-operation-remaining-timeout-ms
The `net-operation-remaining-timeout-ms` procedure computes the poll timeout for `operation`.
The `operation` parameter is a network operation with an optional absolute deadline.
The return value is `-1` without a deadline, or a nonnegative fixnum of milliseconds remaining.
|#
  (define net-operation-remaining-timeout-ms
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (let ([deadline-ms (net-operation-deadline-ms operation)])
                (if deadline-ms
                    (min (most-positive-fixnum)
                         (max 0 (- deadline-ms (current-monotonic-ms))))
                    -1)))))

  (define operation-terminal?
    (lambda (operation)
      (memq (%net-operation-state operation) '(completed failed cancelled))))

  (define conditionize
    (lambda (value)
      (if (condition? value)
          value
          (condition (make-error)
                     (make-message-condition "non-condition object was raised")
                     (make-irritants-condition (list value))))))

  (define cleanup-operation!
    (lambda (operation)
      (unless (%net-operation-cleaned? operation)
        (%net-operation-cleaned?-set! operation #t)
        ((%net-operation-cleanup operation)))))

  (define capture-callback-failure
    (lambda (callback)
      (guard (failure [else (conditionize failure)])
        (callback)
        #f)))

  (define make-cancel-condition
    (lambda (operation)
      (condition (make-error)
                 (make-who-condition 'net-operation-cancel!)
                 (make-message-condition "network operation was cancelled")
                 (make-irritants-condition (list (%net-operation-kind operation))))))

  (define validate-update
    (lambda (who update)
      (unless (net-operation-update? update)
        (errorf who "advance procedure returned an invalid update: ~s" update))
      (case (net-operation-update-state update)
        [(pending)
         (let ([target* (net-operation-update-poll-targets update)]
               [deadline-ms (net-operation-update-deadline-ms update)])
           (unless (and (list? target*) (andmap poll-target? target*))
             (errorf who "pending update contains invalid poll targets: ~s" target*))
           (unless (or (not deadline-ms) (natural? deadline-ms))
             (errorf who "pending update contains an invalid deadline: ~s" deadline-ms)))]
        [(completed) (void)]
        [(failed)
         (unless (condition? (net-operation-update-value update))
           (errorf who "failed update does not contain a condition"))]
        [else
         (errorf who "advance procedure returned an update with invalid state: ~s"
                 (net-operation-update-state update))])
      update))

  (define advance-operation
    (lambda (who operation)
      (guard (failure
              [else (net-operation-failed (conditionize failure))])
        (validate-update who ((%net-operation-advance operation))))))

  (define apply-update!
    (lambda (operation update)
      (%net-operation-state-set! operation (net-operation-update-state update))
      (%net-operation-poll-targets-set!
       operation
       (net-operation-update-poll-targets update))
      (%net-operation-deadline-ms-set!
       operation
       (net-operation-update-deadline-ms update))
      (%net-operation-value-set! operation (net-operation-update-value update))))

  (define cleanup-terminal-operation!
    (lambda (operation)
      (let ([failure
             (capture-callback-failure
              (lambda () (cleanup-operation! operation)))])
        (when (and failure (not (eq? 'failed (%net-operation-state operation))))
          (apply-update! operation (net-operation-failed failure))))
      operation))

  (define fail-operation!
    (lambda (operation failure)
      (apply-update! operation (net-operation-failed (conditionize failure)))
      (cleanup-terminal-operation! operation)))

  (define take-listeners!
    (lambda (operation)
      (let ([listener* (%net-operation-listeners operation)])
        (%net-operation-listeners-set! operation '())
        listener*)))

  (define notify-listeners!
    (lambda (operation listener*)
      (for-each
       (lambda (entry)
         (when (cdr entry)
           (set-cdr! entry #f)
           (guard (ignored [else (void)])
             ((car entry) operation))))
       listener*)))

  #|proc:net-operation-step!
The `net-operation-step!` procedure advances pending `operation` once without waiting.
The `operation` parameter is a pending network operation.
The return value is the same operation after its state and readiness metadata are updated.
Conditions from advancement or cleanup are stored as a failed result.
|#
  (define-who net-operation-step!
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (let ([listener* '()])
                (with-mutex (%net-operation-mutex operation)
                  (unless (eq? 'pending (%net-operation-state operation))
                    (errorf who "cannot step an operation in state ~s"
                            (%net-operation-state operation)))
                  (let ([update (advance-operation who operation)])
                    ;; A user callback can cancel this operation while advance is running.
                    ;; Its terminal state and cleanup must survive the stale returned update.
                    (when (eq? 'pending (%net-operation-state operation))
                      (apply-update! operation update)))
                  (when (operation-terminal? operation)
                    (cleanup-terminal-operation! operation)
                    (set! listener* (take-listeners! operation))))
                (notify-listeners! operation listener*)
                operation))))

  #|proc:net-operation-cancel!
The `net-operation-cancel!` procedure cancels pending `operation` and cleans it up once.
The `operation` parameter is a network operation in any lifecycle state.
The return value is the same operation. Repeated cancellation of a terminal operation is inert.
If cancellation or cleanup fails, the first callback condition becomes a failed result.
|#
  (define net-operation-cancel!
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (let ([listener* '()])
                (with-mutex (%net-operation-mutex operation)
                  (when (eq? 'pending (%net-operation-state operation))
                    (%net-operation-state-set! operation 'cancelled)
                    (%net-operation-poll-targets-set! operation '())
                    (%net-operation-deadline-ms-set! operation #f)
                    (%net-operation-value-set! operation (make-cancel-condition operation))
                    (let ([cancel-failure
                           (capture-callback-failure (%net-operation-cancel operation))])
                      (when cancel-failure
                        (apply-update! operation (net-operation-failed cancel-failure)))
                      (cleanup-terminal-operation! operation)
                      (set! listener* (take-listeners! operation)))))
                (notify-listeners! operation listener*)
                operation))))

  #|proc:net-operation-result
The `net-operation-result` procedure returns the successful value of `operation`.
The `operation` parameter is a completed network operation.
It is an error to read a result from a pending, failed, or cancelled operation.
|#
  (define-who net-operation-result
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (with-mutex (%net-operation-mutex operation)
                (if (eq? 'completed (%net-operation-state operation))
                    (%net-operation-value operation)
                    (errorf who "operation has no successful result in state ~s"
                            (%net-operation-state operation)))))))

  #|proc:net-operation-condition
The `net-operation-condition` procedure returns the terminal condition of `operation`.
The `operation` parameter is a failed or cancelled network operation.
It is an error to read a condition from a pending or completed operation.
|#
  (define-who net-operation-condition
    (lambda (operation)
      (pcheck ([net-operation? operation])
              (with-mutex (%net-operation-mutex operation)
                (if (memq (%net-operation-state operation) '(failed cancelled))
                    (%net-operation-value operation)
                    (errorf who "operation has no terminal condition in state ~s"
                            (%net-operation-state operation)))))))

  #|proc:net-operation-register-listener!
The `net-operation-register-listener!` procedure registers `listener` on `operation`.
The listener has signature `(net-operation) -> unspecified` and runs once after termination.
The return value is an idempotent zero-argument procedure that unregisters the listener.
|#
  (define net-operation-register-listener!
    (lambda (operation listener)
      (pcheck ([net-operation? operation] [procedure? listener])
        (let ([entry (cons listener #t)] [notify? #f])
          (with-mutex (%net-operation-mutex operation)
            (if (operation-terminal? operation)
                (set! notify? #t)
                (%net-operation-listeners-set!
                 operation (cons entry (%net-operation-listeners operation)))))
          (when notify?
            (set-cdr! entry #f)
            (listener operation))
          (lambda ()
            (with-mutex (%net-operation-mutex operation)
              (when (cdr entry)
                (set-cdr! entry #f)
                (%net-operation-listeners-set!
                 operation (remq entry (%net-operation-listeners operation))))))))))

  #|proc:net-operation-wait
The `net-operation-wait` procedure drives `operation` to a terminal state using blocking poll.
The `operation` parameter is a network operation to advance and wait for.
The return value is the successful operation result; failure conditions are raised.
Poll conditions fail the operation and run cleanup once before the same condition is raised.
It is an error to wait on a cancelled operation.
|#
  (define net-operation-wait/blocking
    (lambda (operation)
      (let loop ()
        (case (net-operation-state operation)
          [(pending)
           (net-operation-step! operation)
           (when (eq? 'pending (net-operation-state operation))
             (guard (failure
                     [else
                      (with-mutex (%net-operation-mutex operation)
                        (when (eq? 'pending (%net-operation-state operation))
                          (fail-operation! operation failure)))])
               (poll (net-operation-poll-targets operation)
                     (net-operation-remaining-timeout-ms operation))))
           (loop)]
          [(completed) (net-operation-result operation)]
          [(failed cancelled) (raise (net-operation-condition operation))]
          [else (assert-unreachable)]))))

  (define-who net-operation-wait
    (lambda (operation)
      (pcheck ([net-operation? operation])
        (let ([hook (%net-operation-wait-hook)])
          (if hook
              (hook operation net-operation-wait/blocking)
              (net-operation-wait/blocking operation))))))

  (define valid-poll-events?
    (lambda (event*)
      (andmap (lambda (event)
                (memq event '(read write priority error hup invalid)))
              event*)))

  (define-record-type (net-would-block %make-net-would-block %net-would-block?)
    (sealed #t)
    (opaque #f)
    (fields (immutable resource %net-would-block-resource)
            (immutable events %net-would-block-events)))

  #|proc:net-would-block?
The `net-would-block?` procedure reports whether `value` is a would-block result.
The `value` parameter may be any Scheme object.
The return value is `#t` for a would-block result and `#f` otherwise.
|#
  (define net-would-block?
    (lambda (value)
      (pcheck ()
              (%net-would-block? value))))

  #|proc:make-net-would-block
The `make-net-would-block` procedure constructs a would-block result.
The `resource` parameter is the socket, descriptor, or protocol resource awaiting readiness.
The `events` parameter is a list of poll event symbols required by the resource.
The return value is a would-block result containing `resource` and `events`.
|#
  (define-who make-net-would-block
    (lambda (resource events)
      (pcheck ([list? events])
              (unless (valid-poll-events? events)
                (errorf who "invalid would-block events: ~s" events))
              (%make-net-would-block resource events))))

  #|proc:net-would-block-resource
The `net-would-block-resource` procedure returns the resource stored in `would-block`.
The `would-block` parameter is a would-block result.
|#
  (define net-would-block-resource
    (lambda (would-block)
      (pcheck ([net-would-block? would-block])
              (%net-would-block-resource would-block))))

  #|proc:net-would-block-events
The `net-would-block-events` procedure returns the requested events stored in `would-block`.
The `would-block` parameter is a would-block result.
The return value is a list of poll event symbols.
|#
  (define net-would-block-events
    (lambda (would-block)
      (pcheck ([net-would-block? would-block])
              (%net-would-block-events would-block))))
  )
