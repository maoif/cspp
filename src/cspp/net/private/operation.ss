(library (cspp net private operation)
  (export %make-net-operation
          %net-operation?
          %net-operation-kind
          %net-operation-advance
          %net-operation-cancel
          %net-operation-cleanup
          %net-operation-mutex
          %net-operation-state
          %net-operation-state-set!
          %net-operation-poll-targets
          %net-operation-poll-targets-set!
          %net-operation-deadline-ms
          %net-operation-deadline-ms-set!
          %net-operation-value
          %net-operation-value-set!
          %net-operation-cleaned?
          %net-operation-cleaned?-set!
          %net-operation-listeners
          %net-operation-listeners-set!
          %net-operation-wait-hook
          net-operation-update?
          net-operation-update-state
          net-operation-update-poll-targets
          net-operation-update-deadline-ms
          net-operation-update-value
          net-operation-pending
          net-operation-completed
          net-operation-failed)
  (import (cspp chez)
          (cspp utils))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Dependency-neutral operation storage
  ;;;;===----------------------------------------------------------------------===

  (define-record-type (net-operation %make-net-operation %net-operation?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind %net-operation-kind)
            (immutable advance %net-operation-advance)
            (immutable cancel %net-operation-cancel)
            (immutable cleanup %net-operation-cleanup)
            (immutable mutex %net-operation-mutex)
            (mutable state %net-operation-state %net-operation-state-set!)
            (mutable poll-targets %net-operation-poll-targets
                     %net-operation-poll-targets-set!)
            (mutable deadline-ms %net-operation-deadline-ms
                     %net-operation-deadline-ms-set!)
            (mutable value %net-operation-value %net-operation-value-set!)
            (mutable cleaned? %net-operation-cleaned? %net-operation-cleaned?-set!)
            (mutable listeners %net-operation-listeners %net-operation-listeners-set!)))

  (define net-operation-wait-hook #f)
  (define %net-operation-wait-hook
    (case-lambda
      [() net-operation-wait-hook]
      [(procedure) (set! net-operation-wait-hook procedure)]))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Operation updates
  ;;;;===----------------------------------------------------------------------===

  (define-record-type (net-operation-update %make-net-operation-update
                                            net-operation-update?)
    (sealed #t)
    (opaque #f)
    (fields (immutable state net-operation-update-state)
            (immutable poll-targets net-operation-update-poll-targets)
            (immutable deadline-ms net-operation-update-deadline-ms)
            (immutable value net-operation-update-value)))

  #|proc:net-operation-pending
The `net-operation-pending` procedure constructs a pending operation update.
The `poll-targets` parameter is a list of poll targets on which the operation is waiting.
The `deadline-ms` parameter is an absolute monotonic deadline in milliseconds, or `#f`.
The return value is an immutable operation update.
|#
  (define net-operation-pending
    (lambda (poll-targets deadline-ms)
      (pcheck ([list? poll-targets]
               [(lambda (value) (or (not value) (natural? value))) deadline-ms])
              (%make-net-operation-update 'pending poll-targets deadline-ms #f))))

  #|proc:net-operation-completed
The `net-operation-completed` procedure constructs a completed operation update.
The `value` parameter is the operation's successful result.
The return value is an immutable operation update containing `value`.
|#
  (define net-operation-completed
    (lambda (value)
      (pcheck ()
              (%make-net-operation-update 'completed '() #f value))))

  #|proc:net-operation-failed
The `net-operation-failed` procedure constructs a failed operation update.
The `failure` parameter is the condition that caused the operation to fail.
The return value is an immutable operation update containing `failure`.
|#
  (define net-operation-failed
    (lambda (failure)
      (pcheck ([condition? failure])
              (%make-net-operation-update 'failed '() #f failure))))
  )
