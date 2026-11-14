(library (cspp net socket)
  (export open-socket
          socket?
          close-socket
          socket-closed?
          socket-bind!
          socket-listen!
          socket-accept
          socket-connect!
          socket-connect/nonblocking
          socket-shutdown!
          socket-send
          socket-send-all
          socket-recv
          socket-recv!
          socket-send/nonblocking
          socket-send-all/nonblocking
          socket-send-to
          socket-send-to/nonblocking
          socket-recv/nonblocking
          socket-recv!/nonblocking
          socket-recv-from
          socket-recv-from/nonblocking
          socket-recv-from!
          socket-recv-from!/nonblocking
          socket-set-option!
          socket-get-option
          socket-local-address
          socket-peer-address
          socket-fd
          socket-blocking?
          socket-set-blocking!
          call-with-socket
          call-with-connected-socket
          open-socket-port
          open-socket-input-port
          open-socket-output-port
          open-socket-binary-input-port
          open-socket-binary-output-port
          open-socket-text-input-port
          open-socket-text-output-port
          call-with-socket-ports
          socket-accept/nonblocking)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net poll)
          (cspp net operation))

  (define ensure-open
    (lambda (who sock)
      (when (socket-closed? sock)
        (raise-net-error who 'socket "socket is closed" sock))))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

  (define check-backlog
    (lambda (who backlog)
      (when (fx< backlog 0)
        (errorf who "backlog must be non-negative, given ~s" backlog))
      backlog))

  (define ensure-ffi-success
    (lambda (who x kind)
      (when (ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x))
      x))

  (define ffi-result->would-block
    (lambda (resource answer)
      (and (ffi-would-block? answer)
           (make-net-would-block resource
                                 (list (ffi-would-block-event answer))))))

  (define dup-socket-fd
    (lambda (who sock)
      (let ([ans (ensure-ffi-success who (ffi-net-socket-dup (socket-fd sock)) 'socket)])
        ans)))

  (define address->ffi-host
    (lambda (address)
      (or (socket-address-host address) "")))

  (define address->ffi-path
    (lambda (address)
      (or (socket-address-path address) "")))

  (define send-result
    (lambda (who sock answer)
      (cond
       [(fixnum? answer) answer]
       [(ffi-would-block? answer) (ffi-result->would-block sock answer)]
       [else (ensure-ffi-success who answer 'socket)])))

  (define recv-result
    (lambda (who sock answer)
      (cond
       [(or (bytevector? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer) (ffi-result->would-block sock answer)]
       [else (ensure-ffi-success who answer 'socket)])))

  (define recv-into-result
    (lambda (who sock answer)
      (cond
       [(or (fixnum? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer) (ffi-result->would-block sock answer)]
       [else (ensure-ffi-success who answer 'socket)])))

  (define check-connect-timeout
    (lambda (who timeout-ms)
      (unless (or (not timeout-ms)
                  (and (fixnum? timeout-ms) (fx>= timeout-ms 0)))
        (errorf who "timeout must be #f or a nonnegative fixnum, given ~s" timeout-ms))
      timeout-ms))

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  #|proc:open-socket
The `open-socket` procedure opens a new socket and returns a socket object.
|#
  (define-who open-socket
    (case-lambda
      [(family type) (open-socket family type 0)]
      [(family type proto)
       (pcheck ([fixnum? proto])
               (let ([ans (ffi-net-socket-open (family-symbol->int who family)
                                               (type-symbol->int who type)
                                               proto)])
                 (if (fixnum? ans)
                     (%make-socket ans family type proto #t #f)
                     (ensure-ffi-success who ans 'socket))))]))

  #|proc:close-socket
The `close-socket` procedure closes a socket object.
|#
  (define-who close-socket
    (lambda (sock)
      (pcheck ([socket? sock])
              (unless (socket-closed? sock)
                (ensure-ffi-success who (ffi-net-socket-close (socket-fd sock)) 'socket)
                (socket-closed-set! sock #t)
                (socket-fd-set! sock -1)))))

  #|proc:socket-bind!
The `socket-bind!` procedure binds a socket to a socket address.
|#
  (define-who socket-bind!
    (lambda (sock address)
      (pcheck ([socket? sock] [socket-address? address])
              (ensure-open who sock)
              (ensure-ffi-success
               who
               (ffi-net-socket-bind (socket-fd sock)
                                    (family-symbol->int who (socket-address-family address))
                                    (address->ffi-host address)
                                    (or (socket-address-port address) -1)
                                    (address->ffi-path address))
               'socket))))

  #|proc:socket-listen!
The `socket-listen!` procedure marks a bound stream socket as listening.
|#
  (define-who socket-listen!
    (case-lambda
      [(sock) (socket-listen! sock 128)]
      [(sock backlog)
       (pcheck ([socket? sock] [fixnum? backlog])
               (check-backlog who backlog)
               (ensure-open who sock)
               (ensure-ffi-success who (ffi-net-socket-listen (socket-fd sock) backlog) 'socket))]))

  (define make-accepted-socket
    (lambda (parent fd address)
      (%make-socket fd
                    (socket-address-family address)
                    (socket-type parent)
                    (socket-proto parent)
                    (socket-blocking? parent)
                    #f)))

  #|proc:socket-accept
The `socket-accept` procedure accepts an incoming connection from `sock`.
The `sock` parameter is an open listening socket.
The return values are the accepted socket and its peer address.
A nonblocking listening socket may instead return a would-block value requesting `read`.
|#
  (define-who socket-accept
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (let ([ans (ffi-net-socket-accept (socket-fd sock) 0)])
                (cond
                 [(ffi-would-block? ans) (ffi-result->would-block sock ans)]
                 [(vector? ans)
                  (let ([address (%socket-address-from-ffi (vector-ref ans 1))])
                    (values (make-accepted-socket sock (vector-ref ans 0) address)
                            address))]
                 [else (ensure-ffi-success who ans 'socket)])))))

  #|proc:socket-accept/nonblocking
The `socket-accept/nonblocking` procedure attempts one accept from `sock` without waiting.
The `sock` parameter is an open listening socket.
The return values are the accepted socket and peer address when a connection is ready.
The return value is otherwise a would-block value naming `sock` and requesting `read`.
|#
  (define-who socket-accept/nonblocking
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (let ([ans (ffi-net-socket-accept (socket-fd sock) 1)])
                (cond
                 [(ffi-would-block? ans) (ffi-result->would-block sock ans)]
                 [(vector? ans)
                  (let ([address (%socket-address-from-ffi (vector-ref ans 1))])
                    (values (make-accepted-socket sock (vector-ref ans 0) address)
                            address))]
                 [else (ensure-ffi-success who ans 'socket)])))))

  #|proc:socket-connect!
The `socket-connect!` procedure connects `sock` to `address`, waiting for readiness as needed.
The `sock` parameter is an open socket.
The `address` parameter is the remote socket address.
The return value is `#t` after the connection succeeds. Connection failures are raised.
|#
  (define-who socket-connect!
    (lambda (sock address)
      (pcheck ([socket? sock] [socket-address? address])
              (net-operation-wait (socket-connect/nonblocking sock address #f)))))

  #|proc:socket-connect/nonblocking
The `socket-connect/nonblocking` procedure creates a nonblocking connect operation.
The `sock` parameter is an open socket that the operation connects and owns on cancellation.
The `address` parameter is the remote socket address.
The optional `timeout-ms` parameter is `#f` or a nonnegative relative timeout in milliseconds.
The return value is a pending network operation with a write target and absolute deadline as
needed. Stepping never waits; completion returns `#t`, failure stores a condition, and cancellation
closes `sock`. Terminal cleanup restores the socket's original blocking mode when it remains open.
|#
  (define-who socket-connect/nonblocking
    (case-lambda
      [(sock address) (socket-connect/nonblocking sock address #f)]
      [(sock address timeout-ms)
       (pcheck ([socket? sock] [socket-address? address])
               (ensure-open who sock)
               (check-connect-timeout who timeout-ms)
               (let ([original-blocking? (socket-blocking? sock)]
                     [attempted? #f]
                     [deadline-ms
                      (and timeout-ms (+ (current-monotonic-ms) timeout-ms))])
                 (socket-set-blocking! sock #f)
                 (make-net-operation
                  'socket-connect
                  (lambda ()
                    (ensure-open who sock)
                    (if attempted?
                        (begin
                          (when (and deadline-ms
                                     (>= (current-monotonic-ms) deadline-ms))
                            (raise-net-error who 'timeout "socket connection timed out" sock))
                          (ensure-ffi-success
                           who
                           (ffi-net-socket-connect-status (socket-fd sock))
                           'socket)
                          (net-operation-completed #t))
                        (let ([answer
                               (ffi-net-socket-connect
                                (socket-fd sock)
                                (family-symbol->int
                                 who (socket-address-family address))
                                (address->ffi-host address)
                                (or (socket-address-port address) -1)
                                (address->ffi-path address))])
                          (if (ffi-would-block? answer)
                              (begin
                                (set! attempted? #t)
                                (net-operation-pending
                                 (list (make-poll-target sock '(write)))
                                 deadline-ms))
                              (begin
                                (ensure-ffi-success who answer 'socket)
                                (net-operation-completed #t))))))
                  (lambda ()
                    (unless (socket-closed? sock)
                      (close-socket sock)))
                  (lambda ()
                    (unless (socket-closed? sock)
                      (socket-set-blocking! sock original-blocking?))))))]))

  #|proc:socket-shutdown!
The `socket-shutdown!` procedure shuts down reading, writing, or both directions of a socket.
|#
  (define-who socket-shutdown!
    (lambda (sock how)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (ensure-ffi-success
               who
               (ffi-net-socket-shutdown (socket-fd sock) (shutdown-symbol->int who how))
               'socket))))

  #|proc:socket-send
The `socket-send` procedure writes a slice of `bytevector` to `sock`.
The `sock` parameter is an open socket. The `bytevector` parameter supplies the bytes.
The optional `start` and `stop` parameters delimit the half-open slice to write.
The return value is the number of bytes written, or a write would-block value when applicable.
|#
  (define-who socket-send
    (case-lambda
      [(sock bytevector) (socket-send sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-send sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (pcheck ([socket? sock] [bytevector? bytevector])
               (ensure-open who sock)
               (check-slice who (bytevector-length bytevector) start stop)
               (send-result who sock
                            (ffi-net-socket-send
                             (socket-fd sock) bytevector start stop 0)))]))

  #|proc:socket-send/nonblocking
The `socket-send/nonblocking` procedure attempts one write to `sock` without waiting.
The `sock` parameter is an open socket. The `bytevector` parameter supplies the bytes.
The optional `start` and `stop` parameters delimit the half-open slice to write.
The return value is the number of bytes written or a would-block value requesting `write`.
|#
  (define-who socket-send/nonblocking
    (case-lambda
      [(sock bytevector)
       (socket-send/nonblocking sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-send/nonblocking sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (pcheck ([socket? sock] [bytevector? bytevector])
               (ensure-open who sock)
               (check-slice who (bytevector-length bytevector) start stop)
               (send-result who sock
                            (ffi-net-socket-send
                             (socket-fd sock) bytevector start stop 1)))]))

  #|proc:socket-send-all
The `socket-send-all` procedure writes an entire bytevector slice to a socket before returning.
|#
  (define-who socket-send-all
    (case-lambda
      [(sock bv) (socket-send-all sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-send-all sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (socket-send sock bv i stop))))))]))

  #|proc:socket-send-all/nonblocking
The `socket-send-all/nonblocking` procedure writes a slice to `sock` without waiting.
The `sock` parameter is an open socket. The `bytevector` parameter supplies the bytes.
The optional `start` and `stop` parameters delimit the half-open slice to write.
The return value is the bytes written after progress, the full slice length after completion, or
a would-block value requesting `write` when no bytes could be written.
|#
  (define-who socket-send-all/nonblocking
    (case-lambda
      [(sock bytevector)
       (socket-send-all/nonblocking sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-send-all/nonblocking sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (pcheck ([socket? sock] [bytevector? bytevector])
               (ensure-open who sock)
               (check-slice who (bytevector-length bytevector) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (socket-send/nonblocking sock bytevector i stop)])
                       (cond
                        [(net-would-block? n)
                         (if (fx> i start) (fx- i start) n)]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  (define socket-send-to*
    (lambda (who sock bytevector start stop address nonblocking?)
      (pcheck ([socket? sock] [bytevector? bytevector] [socket-address? address])
        (ensure-open who sock)
        (check-slice who (bytevector-length bytevector) start stop)
        (send-result who sock
                     (ffi-net-socket-send-to
                      (socket-fd sock) bytevector start stop
                      (family-symbol->int who (socket-address-family address))
                      (or (socket-address-host address) "")
                      (or (socket-address-port address) -1)
                      (or (socket-address-path address) "")
                      (if nonblocking? 1 0))))))

  #|proc:socket-send-to
The `socket-send-to` procedure sends a datagram from `sock` to `address`.
The `sock` parameter is an open datagram socket, and `bytevector` supplies the bytes to send.
The optional `start` and `stop` parameters delimit the half-open bytevector slice to send.
The `address` parameter is the destination socket address.
The return value is the number of bytes sent.
|#
  (define-who socket-send-to
    (case-lambda
      [(sock bytevector address)
       (socket-send-to sock bytevector 0 (bytevector-length bytevector) address)]
      [(sock bytevector start stop address)
       (socket-send-to* 'socket-send-to sock bytevector start stop address #f)]))

  #|proc:socket-send-to/nonblocking
The `socket-send-to/nonblocking` procedure attempts one datagram send without waiting.
The `sock` parameter is an open datagram socket, and `bytevector` supplies the bytes to send.
The optional `start` and `stop` parameters delimit the half-open bytevector slice to send.
The `address` parameter is the destination socket address.
The return value is the number of bytes sent or a would-block value requesting `write`.
|#
  (define-who socket-send-to/nonblocking
    (case-lambda
      [(sock bytevector address)
       (socket-send-to/nonblocking sock bytevector 0 (bytevector-length bytevector) address)]
      [(sock bytevector start stop address)
       (socket-send-to* 'socket-send-to/nonblocking sock bytevector start stop address #t)]))

  #|proc:socket-recv
The `socket-recv` procedure reads up to `size` bytes from `sock`.
The `sock` parameter is an open socket. The `size` parameter is the maximum byte count.
The return value is a bytevector, an EOF object, or a read would-block value when applicable.
|#
  (define-who socket-recv
    (lambda (sock size)
      (pcheck ([socket? sock] [fixnum? size])
              (check-size who size)
              (ensure-open who sock)
              (recv-result who sock (ffi-net-socket-recv (socket-fd sock) size 0)))))

  #|proc:socket-recv/nonblocking
The `socket-recv/nonblocking` procedure attempts one read from `sock` without waiting.
The `sock` parameter is an open socket. The `size` parameter is the maximum byte count.
The return value is a bytevector, an EOF object, or a would-block value requesting `read`.
|#
  (define-who socket-recv/nonblocking
    (lambda (sock size)
      (pcheck ([socket? sock] [fixnum? size])
              (check-size who size)
              (ensure-open who sock)
              (recv-result who sock (ffi-net-socket-recv (socket-fd sock) size 1)))))

  #|proc:socket-recv!
The `socket-recv!` procedure reads from `sock` into `bytevector`.
The `sock` parameter is an open socket. The `bytevector` parameter receives bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is the number of bytes read, an EOF object, or a read would-block value.
|#
  (define-who socket-recv!
    (case-lambda
      [(sock bytevector) (socket-recv! sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-recv! sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (pcheck ([socket? sock] [bytevector? bytevector])
               (ensure-open who sock)
               (check-slice who (bytevector-length bytevector) start stop)
               (recv-into-result
                who sock
                (ffi-net-socket-recv-into
                 (socket-fd sock) bytevector start stop 0)))]))

  #|proc:socket-recv!/nonblocking
The `socket-recv!/nonblocking` procedure attempts one read into `bytevector` without waiting.
The `sock` parameter is an open socket. The `bytevector` parameter receives bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is a byte count, an EOF object, or a would-block value requesting `read`.
|#
  (define-who socket-recv!/nonblocking
    (case-lambda
      [(sock bytevector)
       (socket-recv!/nonblocking sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-recv!/nonblocking sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (pcheck ([socket? sock] [bytevector? bytevector])
               (ensure-open who sock)
               (check-slice who (bytevector-length bytevector) start stop)
               (recv-into-result
                who sock
                (ffi-net-socket-recv-into
                 (socket-fd sock) bytevector start stop 1)))]))

  (define socket-recv-from*
    (lambda (who sock size nonblocking?)
      (pcheck ([socket? sock] [fixnum? size])
        (check-size who size)
        (ensure-open who sock)
        (let ([answer (ffi-net-socket-recv-from (socket-fd sock) size
                                                (if nonblocking? 1 0))])
          (cond
           [(ffi-would-block? answer)
            (make-net-would-block sock (ffi-would-block-events answer))]
           [(ffi-error? answer)
            (raise-net-error who 'socket (ffi-error-message answer) answer)]
           [(and (vector? answer) (fx= (vector-length answer) 2)
                 (bytevector? (vector-ref answer 0)))
            (values (vector-ref answer 0)
                    (%socket-address-from-ffi (vector-ref answer 1)))]
           [else
            (raise-net-error who 'internal-ffi "malformed recv-from result" answer)])))))

  (define socket-recv-from-into*
    (lambda (who sock bytevector start stop nonblocking?)
      (pcheck ([socket? sock] [bytevector? bytevector])
        (ensure-open who sock)
        (check-slice who (bytevector-length bytevector) start stop)
        (let ([answer
               (ffi-net-socket-recv-from-into
                (socket-fd sock) bytevector start stop (if nonblocking? 1 0))])
          (cond
           [(ffi-would-block? answer)
            (make-net-would-block sock (ffi-would-block-events answer))]
           [(ffi-error? answer)
            (raise-net-error who 'socket (ffi-error-message answer) answer)]
           [(and (vector? answer) (fx= (vector-length answer) 2)
                 (fixnum? (vector-ref answer 0)))
            (values (vector-ref answer 0)
                    (%socket-address-from-ffi (vector-ref answer 1)))]
           [else
            (raise-net-error who 'internal-ffi "malformed recv-from-into result" answer)])))))

  #|proc:socket-recv-from
The `socket-recv-from` procedure receives one datagram from `sock`.
The `sock` parameter is an open datagram socket, and `size` is the maximum byte count to receive.
The procedure returns two values: a payload bytevector and its source socket address.
|#
  (define-who socket-recv-from
    (lambda (sock size) (socket-recv-from* who sock size #f)))

  #|proc:socket-recv-from/nonblocking
The `socket-recv-from/nonblocking` procedure attempts to receive one datagram without waiting.
The `sock` parameter is an open datagram socket, and `size` is the maximum byte count to receive.
It returns a read would-block value, or two values containing the payload and source address.
|#
  (define-who socket-recv-from/nonblocking
    (lambda (sock size) (socket-recv-from* who sock size #t)))

  #|proc:socket-recv-from!
The `socket-recv-from!` procedure receives one datagram into `bytevector` from `sock`.
The optional `start` and `stop` parameters delimit the half-open destination slice.
It returns two values: the received byte count and the source socket address.
|#
  (define-who socket-recv-from!
    (case-lambda
      [(sock bytevector)
       (socket-recv-from! sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-recv-from! sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (socket-recv-from-into* who sock bytevector start stop #f)]))

  #|proc:socket-recv-from!/nonblocking
The `socket-recv-from!/nonblocking` procedure attempts a datagram receive into `bytevector`.
The optional `start` and `stop` parameters delimit the half-open destination slice.
It returns a read would-block value, or two values containing the byte count and source address.
|#
  (define-who socket-recv-from!/nonblocking
    (case-lambda
      [(sock bytevector)
       (socket-recv-from!/nonblocking sock bytevector 0 (bytevector-length bytevector))]
      [(sock bytevector start)
       (socket-recv-from!/nonblocking sock bytevector start (bytevector-length bytevector))]
      [(sock bytevector start stop)
       (socket-recv-from-into* who sock bytevector start stop #t)]))

  (define boolean-socket-options
    '(reuse-address reuse-port keepalive broadcast tcp-nodelay ipv6-only))

  (define positive-socket-options
    '(recv-buffer send-buffer keepalive-idle keepalive-interval keepalive-count))

  (define check-socket-option
    (lambda (who option value setting?)
      (unless (symbol? option)
        (errorf who "expected socket option symbol, given ~s" option))
      (cond
       [(memq option boolean-socket-options)
        (when (and setting? (not (boolean? value)))
          (errorf who "option ~s requires a boolean, given ~s" option value))]
       [(memq option positive-socket-options)
        (when (and setting? (not (and (fixnum? value) (fx> value 0))))
          (errorf who "option ~s requires a positive fixnum, given ~s" option value))]
       [(eq? option 'multicast-ttl)
        (when (and setting? (not (and (fixnum? value) (fx<= 0 value 255))))
          (errorf who "multicast-ttl requires a fixnum from 0 through 255, given ~s" value))]
       [else (errorf who "unsupported socket option ~s" option)])))

  #|proc:socket-set-option!
The `socket-set-option!` procedure assigns `value` to socket `option` on open `sock`.
The return value is `#t`; unsupported options and invalid values raise an error.
|#
  (define-who socket-set-option!
    (lambda (sock option value)
      (pcheck ([socket? sock] [symbol? option])
              (ensure-open who sock)
              (check-socket-option who option value #t)
              (ensure-ffi-success
               who
               (ffi-net-socket-set-option (socket-fd sock) (symbol->string option) value)
               'socket))))

  #|proc:socket-get-option
The `socket-get-option` procedure returns the current value of `option` on open `sock`.
|#
  (define-who socket-get-option
    (lambda (sock option)
      (pcheck ([socket? sock] [symbol? option])
              (ensure-open who sock)
              (check-socket-option who option #f #f)
              (ensure-ffi-success
               who
               (ffi-net-socket-get-option (socket-fd sock) (symbol->string option))
               'socket))))

  #|proc:socket-local-address
The `socket-local-address` procedure returns the current local socket address.
|#
  (define-who socket-local-address
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (%socket-address-from-ffi
               (ensure-ffi-success who (ffi-net-socket-local-address (socket-fd sock)) 'socket)))))

  #|proc:socket-peer-address
The `socket-peer-address` procedure returns the current peer socket address.
|#
  (define-who socket-peer-address
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (%socket-address-from-ffi
               (ensure-ffi-success who (ffi-net-socket-peer-address (socket-fd sock)) 'socket)))))

  #|proc:socket-set-blocking!
The `socket-set-blocking!` procedure toggles blocking mode on a socket.
|#
  (define-who socket-set-blocking!
    (lambda (sock blocking?)
      (pcheck ([socket? sock] [boolean? blocking?])
              (ensure-open who sock)
              (ensure-ffi-success who (ffi-net-socket-set-blocking (socket-fd sock)
                                                                   (if blocking? 1 0))
                                  'socket)
              (socket-blocking-set! sock blocking?)
              blocking?)))

  #|proc:call-with-socket
The `call-with-socket` procedure opens a socket, passes it to a thunk, and always closes it
afterwards.
|#
  (define-who call-with-socket
    (lambda (family type proto proc)
      (pcheck ([procedure? proc])
              (let ([sock (open-socket family type proto)])
                (dynamic-wind
                  void
                  (lambda () (proc sock))
                  (lambda () (close-socket sock)))))))

  #|proc:call-with-connected-socket
The `call-with-connected-socket` procedure opens, connects, passes, and closes a socket around a
thunk.
|#
  (define-who call-with-connected-socket
    (lambda (family type proto address proc)
      (pcheck ([socket-address? address] [procedure? proc])
              (call-with-socket family type proto
                (lambda (sock)
                  (socket-connect! sock address)
                  (proc sock))))))

  (define open-dup-input-port
    (lambda (who sock transcoder)
      (open-fd-input-port (dup-socket-fd who sock) 'block transcoder)))

  (define open-dup-output-port
    (lambda (who sock transcoder)
      (open-fd-output-port (dup-socket-fd who sock) 'block transcoder)))

  #|proc:open-socket-port
The `open-socket-port` procedure opens a bidirectional binary port for a socket using a duplicated
file descriptor.
|#
  (define-who open-socket-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-fd-input/output-port (dup-socket-fd who sock) 'block #f))))

  #|proc:open-socket-input-port
The `open-socket-input-port` procedure opens a binary input port for a socket using a duplicated
file descriptor.
|#
  (define-who open-socket-input-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-input-port who sock #f))))

  #|proc:open-socket-output-port
The `open-socket-output-port` procedure opens a binary output port for a socket using a duplicated
file descriptor.
|#
  (define-who open-socket-output-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-output-port who sock #f))))

  #|proc:open-socket-binary-input-port
The `open-socket-binary-input-port` procedure is an alias for `open-socket-input-port`.
|#
  (define-who open-socket-binary-input-port
    (lambda (sock)
      (open-socket-input-port sock)))

  #|proc:open-socket-binary-output-port
The `open-socket-binary-output-port` procedure is an alias for `open-socket-output-port`.
|#
  (define-who open-socket-binary-output-port
    (lambda (sock)
      (open-socket-output-port sock)))

  #|proc:open-socket-text-input-port
The `open-socket-text-input-port` procedure opens a text input port for a socket using the native
transcoder.
|#
  (define-who open-socket-text-input-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-input-port who sock (native-transcoder)))))

  #|proc:open-socket-text-output-port
The `open-socket-text-output-port` procedure opens a text output port for a socket using the native
transcoder.
|#
  (define-who open-socket-text-output-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-output-port who sock (native-transcoder)))))

  #|proc:call-with-socket-ports
The `call-with-socket-ports` procedure opens binary input and output ports for a socket, passes them
to a thunk, and closes them afterwards.
|#
  (define-who call-with-socket-ports
    (lambda (sock proc)
      (pcheck ([socket? sock] [procedure? proc])
              (ensure-open who sock)
              (let ([ip (open-socket-input-port sock)]
                    [op (open-socket-output-port sock)])
                (dynamic-wind
                  void
                  (lambda () (proc ip op))
                  (lambda ()
                    (close-port ip)
                    (close-port op)))))))
  )
