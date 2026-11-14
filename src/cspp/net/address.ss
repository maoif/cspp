(library (cspp net address)
  (export make-socket-address
          socket-address?
          socket-address-family
          socket-address-host
          socket-address-port
          socket-address-path
          resolve-address
          resolve-addresses
          address-select
          address-interleave
          connect-addresses/nonblocking
          name->address
          address->name)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net socket)
          (cspp net poll)
          (cspp net operation))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'address (ffi-error-message x) x))
      x))

  (define service->port
    (lambda (who service type)
      (if (fixnum? service)
          (begin (check-port who service) service)
          (let ([answer (ffi-net-service->port service (type-symbol->int who (or type 'stream)))])
            (ensure-success who answer)))))

  #|proc:make-socket-address
The `make-socket-address` procedure constructs an internet or Unix-domain socket address.
|#
  (define-who make-socket-address
    (case-lambda
      [(family path)
       (pcheck ([string? path])
               (unless (eq? family 'unix)
                 (errorf who "two-argument socket addresses require family 'unix"))
               (%make-socket-address family #f #f path))]
      [(family host port)
       (pcheck ([string? host] [fixnum? port])
               (unless (memq family '(inet inet6))
                 (errorf who "three-argument socket addresses require family 'inet or 'inet6"))
               (check-port who port)
               (%make-socket-address family host port #f))]))

  #|proc:resolve-addresses
The `resolve-addresses` procedure resolves `host` and numeric or service-name `port`.
The optional `family` and `type` parameters restrict the returned socket address list.
|#
  (define-who resolve-addresses
    (case-lambda
      [(host port) (resolve-addresses host port #f #f)]
      [(host port family) (resolve-addresses host port family #f)]
      [(host port family type)
       (pcheck ([string? host]
                [(lambda (value) (or (fixnum? value) (string? value))) port])
               (let* ([numeric-port (service->port who port type)]
                      [ans (ensure-success
                           who
                           (ffi-net-resolve-addresses host numeric-port
                                                      (family-symbol->int who family)
                                                      (type-symbol->int who type)))])
                 (unless (and (vector? ans) (fx= (vector-length ans) 2)
                              (list? (vector-ref ans 1)))
                   (raise-net-error who 'internal-ffi
                                    "malformed address resolution result" ans))
                 (map %socket-address-from-ffi (vector-ref ans 1))))]))

  #|proc:address-select
The `address-select` procedure returns addresses from `address*` accepted by `predicate`.
The `predicate` parameter has signature `(socket-address) -> boolean`; source order is preserved.
|#
  (define-who address-select
    (lambda (address* predicate)
      (pcheck ([list? address*] [procedure? predicate])
        (for-each (lambda (address)
                    (unless (socket-address? address)
                      (errorf who "expected socket address, given ~s" address)))
                  address*)
        (filter predicate address*))))

  (define alternate-addresses
    (lambda (left right)
      (cond
       [(null? left) right]
       [(null? right) left]
       [else (cons (car left) (cons (car right)
                                    (alternate-addresses (cdr left) (cdr right))))])))

  #|proc:address-interleave
The `address-interleave` procedure stably alternates IPv6 and IPv4 values in `address*`.
IPv6 is emitted first, and addresses of other families follow in source order.
|#
  (define-who address-interleave
    (lambda (address*)
      (pcheck ([list? address*])
        (for-each (lambda (address)
                    (unless (socket-address? address)
                      (errorf who "expected socket address, given ~s" address)))
                  address*)
        (append (alternate-addresses
                 (filter (lambda (address) (eq? 'inet6 (socket-address-family address))) address*)
                 (filter (lambda (address) (eq? 'inet (socket-address-family address))) address*))
                (filter (lambda (address)
                          (not (memq (socket-address-family address) '(inet inet6))))
                        address*)))))

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define connect-stagger-ms 250)

  #|proc:connect-addresses/nonblocking
The `connect-addresses/nonblocking` procedure races stream connections to `address*`.
Attempts begin in interleaved order, 250 milliseconds apart, under total `timeout-ms`.
The returned network operation completes with the first connected socket and closes all losers.
|#
  (define-who connect-addresses/nonblocking
    (case-lambda
      [(address*) (connect-addresses/nonblocking address* 5000)]
      [(address* timeout-ms)
       (pcheck ([list? address*] [fixnum? timeout-ms])
         (when (fx< timeout-ms 0)
           (errorf who "connection timeout must be nonnegative, given ~s" timeout-ms))
         (for-each (lambda (address)
                     (unless (and (socket-address? address)
                                  (memq (socket-address-family address) '(inet inet6 unix)))
                       (errorf who "invalid connection address ~s" address)))
                   address*)
         (when (null? address*)
           (errorf who "at least one connection address is required"))
         (let ([remaining (address-interleave address*)]
               [active '()]
               [failure* '()]
               [deadline-ms (+ (current-monotonic-ms) timeout-ms)]
               [next-start-ms 0])
           (define cancel-active!
             (lambda ()
               (for-each
                (lambda (entry)
                  (let ([operation (car entry)] [sock (cadr entry)])
                    (when (eq? 'pending (net-operation-state operation))
                      (net-operation-cancel! operation))
                    (unless (socket-closed? sock) (close-socket sock))))
                active)
               (set! active '())))
           (define start-next!
             (lambda (now)
               (when (pair? remaining)
                 (let* ([address (car remaining)]
                        [sock (open-socket (socket-address-family address) 'stream)]
                        [operation (socket-connect/nonblocking sock address timeout-ms)])
                   (set! remaining (cdr remaining))
                   (net-operation-step! operation)
                   (set! active (append active (list (list operation sock address))))
                   (set! next-start-ms (fx+ now connect-stagger-ms))))))
           (define ready-to-step?
             (lambda (operation)
               (let ([target* (net-operation-poll-targets operation)])
                 (or (null? target*)
                     (exists (lambda (target)
                               (pair? (poll-target-ready-events target)))
                             (poll/nonblocking target*))))))
           (define process-active!
             (lambda ()
               (let loop ([entry* active] [pending '()] [winner #f])
                 (if (null? entry*)
                     (begin (set! active (reverse pending)) winner)
                     (let* ([entry (car entry*)]
                            [operation (car entry)]
                            [sock (cadr entry)])
                       (when (and (eq? 'pending (net-operation-state operation))
                                  (ready-to-step? operation))
                         (net-operation-step! operation))
                       (case (net-operation-state operation)
                         [(completed)
                          (if winner
                              (begin
                                (unless (socket-closed? sock) (close-socket sock))
                                (loop (cdr entry*) pending winner))
                              (loop (cdr entry*) pending entry))]
                         [(failed cancelled)
                          (set! failure* (cons (net-operation-condition operation) failure*))
                          (unless (socket-closed? sock) (close-socket sock))
                          (loop (cdr entry*) pending winner)]
                         [else (loop (cdr entry*) (cons entry pending) winner)]))))))
           (make-net-operation
            'connect-addresses
            (lambda ()
              (let ([now (current-monotonic-ms)])
                (when (fx>= now deadline-ms)
                  (cancel-active!)
                  (raise-net-error 'address 'connect "address connection timed out"
                                   'timeout #f #f #t #f failure*))
                (when (and (pair? remaining)
                           (or (null? active) (fx>= now next-start-ms)))
                  (start-next! now))
                (let ([winner (process-active!)])
                  (cond
                   [winner
                    (let ([sock (cadr winner)])
                      (cancel-active!)
                      (net-operation-completed sock))]
                   [(and (null? remaining) (null? active))
                    (raise-net-error 'address 'connect "all address connections failed"
                                     'connection-failed #f #f #f #f (reverse failure*))]
                   [else
                    (net-operation-pending
                     (apply append
                            (map (lambda (entry)
                                   (net-operation-poll-targets (car entry)))
                                 active))
                     (min deadline-ms
                          (if (pair? remaining) next-start-ms deadline-ms)))]))))
            cancel-active!
            cancel-active!)))]))

  #|proc:resolve-address
The `resolve-address` procedure resolves a host and port.
It returns the first matching socket address, or `#f` when no address matches.
|#
  (define-who resolve-address
    (case-lambda
      [(host port) (resolve-address host port #f #f)]
      [(host port family) (resolve-address host port family #f)]
      [(host port family type)
       (let ([addresses (resolve-addresses host port family type)])
         (and (pair? addresses) (car addresses)))]))

  #|proc:name->address
The `name->address` procedure is an alias for `resolve-address`.
|#
  (define-who name->address
    (case-lambda
      [(host port) (resolve-address host port)]
      [(host port family) (resolve-address host port family)]
      [(host port family type) (resolve-address host port family type)]))

  #|proc:address->name
The `address->name` procedure performs a reverse lookup for socket `address`.
The return value is the resolved hostname string.
|#
  (define-who address->name
    (lambda (address)
      (pcheck ([socket-address? address])
              (ensure-success
               who
               (ffi-net-address->name
                (family-symbol->int who (socket-address-family address))
                (or (socket-address-host address) "")
                (or (socket-address-port address) -1)
                (or (socket-address-path address) ""))))))
  )
