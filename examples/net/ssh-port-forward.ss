(import (cspp) (cspp net) (cspp logging))
(load "examples/common.ss")
(load "examples/net/ssh-example-common.ss")

(define monotonic-ms
  (lambda ()
    (let ([time (current-time 'time-monotonic)])
      (+ (* (time-second time) 1000) (quotient (time-nanosecond time) 1000000)))))

;; One bounded buffer per direction provides backpressure without unbounded allocation.
;; Fields: socket, forwarding, upload, upload-offset, download, download-offset,
;; local-read-EOF, SSH-read-EOF, SSH-write-EOF, socket-write-EOF.
(define make-relay (lambda (socket forwarding) (vector socket forwarding #f 0 #f 0 #f #f #f #f)))

(define close-relay!
  (lambda (relay)
    (guard (condition [else #f]) (close-socket (vector-ref relay 0)))
    (guard (condition [else #f]) (ssh-close-forwarding (vector-ref relay 1)))))

(define forwarding-connection-error!
  (lambda (log condition)
    ;; A failed client connection must not close the shared forwarding listener.
    (if (net-error? condition)
        (with-logger log (log-warnf "forwarded connection failed: ~a" condition))
        (raise condition))))

(define step-relay!
  (lambda (relay wait!)
    (let ([socket (vector-ref relay 0)]
          [channel (ssh-forwarding-channel (vector-ref relay 1))])
      (unless (or (vector-ref relay 2) (vector-ref relay 6))
        ;; Read local data only while the upload buffer is empty.
        (let ([answer (socket-recv/nonblocking socket 65536)])
          (cond [(net-would-block? answer) (wait! answer)]
                [(eof-object? answer) (vector-set! relay 6 #t)]
                [else (vector-set! relay 2 answer) (vector-set! relay 3 0)])))
      (when (vector-ref relay 2)
        ;; Advance the offset after a partial SSH write; retain the unconsumed suffix.
        (let* ([buffer (vector-ref relay 2)]
               [offset (vector-ref relay 3)]
               [answer (ssh-write/nonblocking channel buffer offset (bytevector-length buffer))])
          (if (net-would-block? answer)
              (wait! answer)
              (let ([next (fx+ offset answer)])
                (vector-set! relay 3 next)
                (when (fx= next (bytevector-length buffer)) (vector-set! relay 2 #f))))))
      (when (and (vector-ref relay 6) (not (vector-ref relay 2))
                 (not (vector-ref relay 8)))
        ;; Preserve the remote readable stream after local stdin reaches EOF.
        (ssh-send-eof! channel)
        (vector-set! relay 8 #t))
      (unless (or (vector-ref relay 4) (vector-ref relay 7))
        ;; Read forwarded SSH data only while the download buffer is empty.
        (let ([answer (ssh-read/nonblocking channel 65536)])
          (cond [(net-would-block? answer) (wait! answer)]
                [(eof-object? answer) (vector-set! relay 7 #t)]
                [else (vector-set! relay 4 answer) (vector-set! relay 5 0)])))
      (when (vector-ref relay 4)
        ;; Retain partial local writes until the socket can accept the remaining bytes.
        (let* ([buffer (vector-ref relay 4)]
               [offset (vector-ref relay 5)]
               [answer (socket-send/nonblocking socket buffer offset (bytevector-length buffer))])
          (if (net-would-block? answer)
              (wait! answer)
              (let ([next (fx+ offset answer)])
                (vector-set! relay 5 next)
                (when (fx= next (bytevector-length buffer)) (vector-set! relay 4 #f))))))
      (when (and (vector-ref relay 7) (not (vector-ref relay 4))
                 (not (vector-ref relay 9)))
        ;; Half-close local output after delivering every remote byte.
        (socket-shutdown! socket 'write)
        (vector-set! relay 9 #t))
      (and (vector-ref relay 8) (vector-ref relay 9)))))

;; Pending setup fields: local socket, network operation, accepted remote forwarding or #f.
(define close-pending-forward!
  (lambda (pending)
    ;; Cancellation releases a pending native channel or socket before session shutdown.
    (net-operation-cancel! (vector-ref pending 1))
    (guard (condition [else #f]) (close-socket (vector-ref pending 0)))
    (when (vector-ref pending 2)
      (guard (condition [else #f]) (ssh-close-forwarding (vector-ref pending 2))))))

(define serve-forwarding!
  (lambda (session log direction listen-host listen-port target-host target-port duration)
    (let ([listener #f] [relay* '()] [pending* '()] [stop? #f]
          [deadline (and (> duration 0) (+ (monotonic-ms) duration))]
          [old-interrupt (keyboard-interrupt-handler)])
      (dynamic-wind
        (lambda () (keyboard-interrupt-handler (lambda () (set! stop? #t))))
        (lambda ()
          (set! listener
                (if (eq? direction 'local)
                    (let ([socket (open-socket 'inet 'stream)])
                      (guard (condition [else (close-socket socket) (raise condition)])
                        (socket-set-option! socket 'reuse-address #t)
                        (socket-bind! socket (make-socket-address 'inet listen-host listen-port))
                        (socket-listen! socket)
                        socket))
                    ;; Ask the remote SSH server to bind the reverse listener.
                    (ssh-request-remote-forward! session listen-host listen-port)))
          (with-logger log
            (log-infof "~a forwarding listening on ~a:~a" direction listen-host
                       (if (socket? listener)
                           (socket-address-port (socket-local-address listener))
                           (ssh-forwarding-port listener))))
          (let loop ()
            (unless (or stop? (and deadline (>= (monotonic-ms) deadline)))
              (let ([waiting '()])
                (define wait! (lambda (answer) (set! waiting (cons answer waiting))))
                (if (eq? direction 'local)
                    (call-with-values
                        (lambda () (socket-accept/nonblocking listener))
                      (lambda (answer . peer)
                        (if (net-would-block? answer)
                            (wait! answer)
                            (guard (condition
                                    [else (close-socket answer)
                                          (forwarding-connection-error! log condition)])
                              ;; Retain a pending direct channel across nonblocking setup turns.
                              (set! pending*
                                    (cons (vector
                                           answer
                                           (ssh-open-local-forward/nonblocking
                                            session target-host target-port listen-host listen-port)
                                           #f)
                                          pending*))))))
                    ;; Accept reverse channels immediately; connect local targets asynchronously.
                    (let ([answer (ssh-accept-remote-forward listener)])
                      (if (net-would-block? answer)
                          (wait! answer)
                          (let ([socket (open-socket 'inet 'stream)])
                            (guard (condition
                                    [else (close-relay! (make-relay socket answer))
                                          (forwarding-connection-error! log condition)])
                              (set! pending*
                                    (cons (vector
                                           socket
                                           (socket-connect/nonblocking
                                            socket (make-socket-address 'inet target-host target-port)
                                            30000)
                                           answer)
                                          pending*)))))))
                (set! pending*
                      (filter
                       (lambda (pending)
                         (guard (condition
                                 [else (close-pending-forward! pending)
                                       (forwarding-connection-error! log condition)
                                       #f])
                           (let ([operation (vector-ref pending 1)])
                             ;; TCP connect needs readiness: SO_ERROR can be zero in SYN_SENT.
                             ;; SSH may already have buffered replies read by another channel.
                             (let ([targets (net-operation-poll-targets operation)])
                               (when (or (not (vector-ref pending 2))
                                         (null? targets)
                                         (zero? (net-operation-remaining-timeout-ms operation))
                                         (exists (lambda (target)
                                                   (pair? (poll-target-ready-events target)))
                                                 (poll targets 0)))
                                 (net-operation-step! operation)))
                             (case (net-operation-state operation)
                               [(pending)
                                (for-each
                                 (lambda (target)
                                   (wait! (make-net-would-block (poll-target-resource target)
                                                               (poll-target-events target))))
                                 (net-operation-poll-targets operation))
                                #t]
                               [(completed)
                                (set! relay*
                                      (cons (make-relay
                                             (vector-ref pending 0)
                                             (or (vector-ref pending 2)
                                                 (net-operation-result operation)))
                                            relay*))
                                #f]
                               [else (raise (net-operation-condition operation))]))))
                       pending*))
                (set! relay*
                      (filter
                       (lambda (relay)
                         (let ([relay-waiting '()])
                           (guard (condition
                                   [else (close-relay! relay)
                                         (forwarding-connection-error! log condition)
                                         #f])
                             (if (step-relay!
                                  relay
                                  (lambda (answer)
                                    (set! relay-waiting (cons answer relay-waiting))))
                                 (begin (close-relay! relay) #f)
                                 (begin
                                   ;; Poll only resources still owned by active connections.
                                   (set! waiting (append relay-waiting waiting))
                                   #t)))))
                       relay*))
                ;; Ctrl-C can interrupt the native poll after setting the stop flag.
                (guard (condition [stop? (void)] [else (raise condition)])
                  (ssh-example-wait waiting))
                (loop)))))
        (lambda ()
          (keyboard-interrupt-handler old-interrupt)
          (for-each close-pending-forward! pending*)
          (for-each close-relay! relay*)
          (when listener
            (if (socket? listener) (close-socket listener) (ssh-close-forwarding listener)))
          (with-logger log (log-info "forwarding stopped")))))))

(ssh-example-run!
 'ssh-port-forward
 "Relay TCP through SSH. --direction=local|remote; --duration=milliseconds (0 runs until Ctrl-C)."
 (append ssh-example-options '((direction . "Required: local (local listener to remote target) or remote (remote listener to local target).")
   (listen-host . "Listener bind address, interpreted on the selected listener side (default: 127.0.0.1).")
   (listen-port . "Listener port, 0 through 65535 (default: 0, request a free port).")
   (target-host . "Target hostname or IP, resolved on the target side (default: 127.0.0.1).")
   (target-port . "Target port, 0 through 65535 (default: 22).")
   (duration . "Run time in milliseconds, a nonnegative integer (default: 0, run until Ctrl-C).")))
 (lambda (ref)
   (let ([direction (string->symbol (example-required 'ssh-port-forward ref 'direction))])
     (unless (memq direction '(local remote))
       (errorf 'ssh-port-forward "--direction must be local or remote"))
     (call-with-example-ssh
      'ssh-port-forward ref
      (lambda (session log)
        (serve-forwarding!
         session log direction (or (ref 'listen-host) "127.0.0.1")
         (ssh-example-port 'ssh-port-forward ref 'listen-port 0)
         (or (ref 'target-host) "127.0.0.1")
         (ssh-example-port 'ssh-port-forward ref 'target-port 22)
         (example-option-natural 'ssh-port-forward ref 'duration 0))))))
 (command-line-arguments))
