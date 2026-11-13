(library (cspp net poll)
  (export make-poll-target
          poll-target?
          poll-target-resource
          poll-target-fd
          poll-target-events
          poll-target-ready-events
          poll
          poll-until
          poll/nonblocking)
  (import (cspp chez)
          (cspp utils)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net private operation))

  #|record:poll-target
The `poll-target` record is an immutable snapshot of one requested or completed poll target.
Its resource retains the descriptor, fd is its integer descriptor, events are requested event
symbols, and ready-events are the separate event symbols reported by `poll`.
|#
  (define-record-type (poll-target %make-poll-target poll-target?)
    (sealed #t)
    (opaque #f)
    (fields (immutable resource poll-target-resource)
            (immutable fd poll-target-fd)
            (immutable events poll-target-events)
            (immutable ready-events poll-target-ready-events)))

  (define event-symbol->mask
    (lambda (who sym)
      (case sym
        [(read) (net-pollin)]
        [(write) (net-pollout)]
        [(priority) (net-pollpri)]
        [(error) (net-pollerr)]
        [(hup) (net-pollhup)]
        [(invalid) (net-pollnval)]
        [else (errorf who "invalid poll event ~s" sym)])))

  (define mask->event-list
    (lambda (mask)
      (let ([pairs `((read . ,(net-pollin))
                     (write . ,(net-pollout))
                     (priority . ,(net-pollpri))
                     (error . ,(net-pollerr))
                     (hup . ,(net-pollhup))
                     (invalid . ,(net-pollnval)))])
        (fold-right (lambda (entry acc)
                      (if (fx= 0 (fxlogand mask (cdr entry)))
                          acc
                          (cons (car entry) acc)))
                    '()
                    pairs))))

  (define event-list->mask
    (lambda (who event*)
      (unless (list? event*)
        (errorf who "poll events must be a list, given ~s" event*))
      (fold-left (lambda (acc ev)
                   (fxlogor acc (event-symbol->mask who ev)))
                 0
                 event*)))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms -1)
        (errorf who "poll timeout must be -1 or non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define resource->fd
    (lambda (who resource)
      (cond
       [(socket? resource) (socket-fd resource)]
       [(fixnum? resource) resource]
       [(and (port? resource) (binary-port? resource))
        (let ([descriptor
               (guard (failure [else #f])
                 (port-file-descriptor resource))])
          (if (fixnum? descriptor)
              descriptor
              (errorf who "binary port has no file descriptor: ~s" resource)))]
       [(%net-operation? resource)
        (let ([target* (%net-operation-poll-targets resource)])
          (if (and (pair? target*) (null? (cdr target*)))
              (poll-target-fd (car target*))
              (errorf who "operation must have exactly one poll target, given ~s" resource)))]
       [else
        (errorf who
                "expected socket, descriptor, binary port, or net operation, given ~s"
                resource)])))

  (define target->spec
    (lambda (who target)
      (let ([v (make-vector 2 #f)])
        (vector-set! v 0 (poll-target-fd target))
        (vector-set! v 1 (event-list->mask who (poll-target-events target)))
        v)))

  #|proc:make-poll-target
The `make-poll-target` procedure constructs a readiness target for `resource`.
The `resource` parameter is a socket, integer descriptor, descriptor-backed binary port, or
network operation with exactly one current poll target.
The `event*` parameter is a list containing `read`, `write`, `priority`, `error`, `hup`, or
`invalid` symbols.
The return value is a poll target retaining `resource` and its extracted descriptor.
|#
  (define-who make-poll-target
    (lambda (resource event*)
      (unless (list? event*)
        (errorf who "poll events must be a list, given ~s" event*))
      (pcheck ([list? event*])
              (let ([fd (resource->fd who resource)])
                (event-list->mask who event*)
                (%make-poll-target resource fd event* '())))))

  #|proc:poll
The `poll` procedure waits for readiness across `target*`.
The `target*` parameter is a list of poll targets.
The optional `timeout-ms` parameter is `-1` to wait indefinitely or a nonnegative timeout.
The return value is a corresponding list of targets whose ready events include all native flags.
|#
  (define-who poll
    (case-lambda
      [(target*) (poll target* -1)]
      [(target* timeout-ms)
       (pcheck ([list? target*] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (for-each (lambda (target)
                           (unless (poll-target? target)
                             (errorf who "poll target expected, given ~s" target)))
                         target*)
               (let ([ans (ffi-net-poll (map (lambda (target) (target->spec who target)) target*)
                                        timeout-ms)])
                 (when (ffi-error? ans)
                   (raise-net-error who 'poll (ffi-error-message ans) ans))
                 (map (lambda (target spec)
                        (%make-poll-target (poll-target-resource target)
                                           (poll-target-fd target)
                                           (poll-target-events target)
                                           (mask->event-list (vector-ref spec 2))))
                      target*
                      ans)))]))

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  #|proc:poll-until
The `poll-until` procedure waits for `target*` until the absolute `deadline-ms`.
The `target*` parameter is a list of poll targets.
The `deadline-ms` parameter is an absolute monotonic deadline in milliseconds.
The return value is a corresponding list of targets populated with their ready events.
An elapsed deadline performs a nonblocking poll.
|#
  (define poll-until
    (lambda (target* deadline-ms)
      (pcheck ([list? target*] [natural? deadline-ms])
              (poll target*
                    (min (most-positive-fixnum)
                         (max 0 (- deadline-ms (current-monotonic-ms))))))))

  #|proc:poll/nonblocking
The `poll/nonblocking` procedure polls `target*` without waiting.
The `target*` parameter is a list of poll targets.
The return value is a corresponding list of targets populated with current ready events.
|#
  (define-who poll/nonblocking
    (lambda (target*)
      (pcheck ([list? target*])
              (poll target* 0))))
  )
