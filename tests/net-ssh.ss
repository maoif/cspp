(import (cspp)
        (cspp file)
        (cspp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define ssh-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (or (string-contains? (net-error-message c) "timed out")
                        (string-contains? (net-error-message c) "Timeout")))])
      (thunk)
      #f)))

(define ssh-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-stalled-ssh-banner-server
  (lambda (delay-ms)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (milisleep delay-ms)
                     (close-socket client)
                     (close-socket listener)))))))))

(mat net-ssh
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (and
                 (call-with-ssh-session "127.0.0.1" port user ssh-session?)
                 (call-with-ssh-session "127.0.0.1" port user 2000 ssh-session?)
                 (let ([session (ssh-open "127.0.0.1" port user)])
                   (dynamic-wind
                     void
                     (lambda ()
                       (and
                        (guard (c [else (net-error? c)])
                          (ssh-auth-password! session user "bad-password")
                          #f)
                        (guard (c [else (net-error? c)])
                          (ssh-auth-agent! session user)
                          #f)
                        (eq? (ssh-auth-publickey! session user) session)
                        (ssh-test-pty session)
                        (ssh-test-read-slice session)
                        (ssh-test-session-exec session)
                        (ssh-test-blocking-write session)
                        (ssh-test-read-timeout session ssh-net-error-timeout?)
                        (ssh-test-setup-timeouts session remote-root ssh-net-error-timeout?)
                        (ssh-test-nonblocking-io session)
                        (ssh-test-shell session)
                        (ssh-test-port-wrappers session)))
                     (lambda ()
                       (ssh-close session))))))))
           (lambda ()
             (stop-server))))))
     (mat optional-net-ssh-operation
       (mat-requires (ssh)
         (let-values ([(listener port th)
                       (start-stalled-ssh-banner-server 200)])
           (dynamic-wind
             void
             (lambda ()
               (ssh-net-error-timeout?
                (lambda ()
                  (ssh-open "127.0.0.1" port #f 50))))
             (lambda ()
               (thread-join th)
               (guard (c [else #f])
                 (close-socket listener)))))))

(mat net-ssh-stderr-readiness
     (mat-requires (ssh)
       (with-test-ssh-channel
        "sh -c 'sleep 1; printf err >&2'"
        (lambda (channel)
          (let ([answer (ssh-read-stderr/nonblocking channel 16)]
                [into-answer
                 (ssh-read-stderr!/nonblocking channel (make-bytevector 16 0))])
            (and (net-would-block? answer)
                 (fixnum? (net-would-block-resource answer))
                 (not (not (memq 'read (net-would-block-events answer))))
                 (net-would-block? into-answer)
                 (eq? (net-would-block-resource answer)
                      (net-would-block-resource into-answer))
                 (not (not (memq 'read (net-would-block-events into-answer)))))))))

     (mat-requires (ssh)
       (with-test-ssh-channel
        "sh -c 'printf err >&2'"
        (lambda (channel)
          (equal? (ssh-read-stderr channel 3) (string->utf8 "err")))))

     (mat-requires (ssh)
       (with-test-ssh-channel
        "sh -c 'printf err >&2'"
        (lambda (channel)
          (let ([buffer (make-bytevector 3 0)])
            (and (fx= 3 (ssh-read-stderr! channel buffer 0 3))
                 (equal? buffer (string->utf8 "err")))))))
     )

(mat net-ssh-channel-requests
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME" home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (ssh-auth-publickey! session user)
                      (let ([channel (ssh-open-channel session)]
                            [subsystem-channel (ssh-open-channel session)])
                        (dynamic-wind
                          void
                          (lambda ()
                            (and
                             (eq? (ssh-request-environment!
                                   channel "CSPP_TEST_ENV" "request-value") channel)
                             (ssh-error-message-contains?
                              "subsystem must not be empty"
                              (lambda () (ssh-request-subsystem! subsystem-channel "")))
                             (eq? (ssh-request-subsystem! subsystem-channel "sftp")
                                  subsystem-channel)))
                          (lambda ()
                            (ssh-close-channel subsystem-channel)
                            (ssh-close-channel channel)))))
                    (lambda () (ssh-close session)))))))
           (lambda () (stop-server))))))

(mat net-ssh-known-hosts
     ;; Negative test: strict host-key verification rejects a server when HOME
     ;; has no known_hosts entry.
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (let ([empty-home (format "/tmp/cspp-net-ssh-empty-home-~a" port)])
           (dynamic-wind
             (lambda () (mkdirs (string-append empty-home "/.ssh")))
             (lambda ()
               (with-env
                "HOME"
                empty-home
                (lambda ()
                  (ssh-error-message-contains?
                   "host key"
                   (lambda ()
                     (ssh-open "127.0.0.1" port user 2000))))))
             (lambda ()
               (stop-server)
               (when (file-exists? empty-home)
                 (file-removetree empty-home #f)))))))
     ;; `accept-new` is an explicit bypass that trusts an unknown server once
     ;; and records the host key in the configured known_hosts file.
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (let ([known-hosts (string-append home "/.ssh/known_hosts")])
           (dynamic-wind
             (lambda ()
               (when (file-exists? known-hosts)
                 (delete-file known-hosts)))
             (lambda ()
               (with-env
                "HOME"
                home
                (lambda ()
                  (let ([session (ssh-open-with-policy "127.0.0.1" port user 2000 'accept-new)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (and (ssh-session? session)
                             (file-exists? known-hosts)))
                      (lambda ()
                        (ssh-close session)))))))
             (lambda ()
               (stop-server))))))
     ;; `insecure` is an explicit bypass that skips known-host verification.
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (let ([empty-home (format "/tmp/cspp-net-ssh-insecure-home-~a" port)])
           (dynamic-wind
             (lambda () (mkdirs (string-append empty-home "/.ssh")))
             (lambda ()
               (with-env
                "HOME"
                empty-home
                (lambda ()
                  (let ([session (ssh-open-with-policy "127.0.0.1" port user 2000 'insecure)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (ssh-session? session))
                      (lambda ()
                        (ssh-close session)))))))
             (lambda ()
               (stop-server)
               (when (file-exists? empty-home)
                 (file-removetree empty-home #f))))))))

(mat net-ssh-timeout-validation
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (and
                 (ssh-error-message-contains?
                  "timeout must be non-negative"
                  (lambda ()
                    (ssh-open "127.0.0.1" port user -1)))
                 (ssh-error-message-contains?
                  "timeout must be non-negative"
                  (lambda ()
                    (call-with-ssh-session "127.0.0.1" port user -1 ssh-session?)))
                 (let ([session (ssh-open "127.0.0.1" port user)])
                   (dynamic-wind
                     void
                     (lambda ()
                       (and
                        (eq? (ssh-auth-publickey! session user) session)
                        (ssh-error-message-contains?
                         "timeout must be non-negative"
                         (lambda ()
                           (ssh-open-channel session -1)))
                        (ssh-error-message-contains?
                         "timeout must be non-negative"
                         (lambda ()
                           (call-with-ssh-channel session -1 ssh-channel?)))
                        (let ([read-ch (ssh-exec session "printf data")]
                              [write-bv (string->utf8 "x")]
                              [buf (make-bytevector 4 0)])
                          (dynamic-wind
                            void
                            (lambda ()
                              (and
                               (ssh-error-message-contains?
                                "timeout must be non-negative"
                                (lambda ()
                                  (ssh-read read-ch 1 -1)))
                               (ssh-error-message-contains?
                                "timeout must be non-negative"
                                (lambda ()
                                  (ssh-read! read-ch buf 0 1 -1)))
                               (ssh-error-message-contains?
                                "timeout must be non-negative"
                                (lambda ()
                                  (ssh-write read-ch write-bv 0 1 -1)))
                               (ssh-error-message-contains?
                                "timeout must be non-negative"
                                (lambda ()
                                  (ssh-write-all read-ch write-bv 0 1 -1)))))
                            (lambda ()
                              (ssh-close-channel read-ch))))))
                     (lambda ()
                       (ssh-close session))))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-auth-known-hosts-and-forwarding
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME" home
              (lambda ()
                (let* ([known-hosts (string-append home "/.ssh/managed_hosts")]
                       [private-key (string-append home "/.ssh/id_ed25519")]
                       [public-key (string-append private-key ".pub")]
                       [listener (open-socket 'inet 'stream)])
                  (dynamic-wind
                    (lambda ()
                      (socket-set-option! listener 'reuse-address #t)
                      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
                      (socket-listen! listener 1))
                    (lambda ()
                      (let* ([echo-port
                              (socket-address-port (socket-local-address listener))]
                             [echo-thread
                              (fork-thread
                               (lambda ()
                                 (let-values ([(client peer) (socket-accept listener)])
                                   (dynamic-wind
                                     void
                                     (lambda ()
                                       (let ([payload (socket-recv client 4)])
                                         (socket-send-all client payload)))
                                     (lambda () (close-socket client))))))]
                             [session
                              (ssh-open-with-policy "127.0.0.1" port user 30000 'insecure)])
                        (dynamic-wind
                          void
                          (lambda ()
                            (and
                             (eq? session
                                  (ssh-auth-private-key! session user public-key private-key #f))
                             (memq (ssh-check-known-host session known-hosts)
                                   '(not-found unknown))
                             (ssh-known-host? (ssh-add-known-host! session known-hosts))
                             (eq? (ssh-check-known-host session known-hosts) 'ok)
                             (= (length (ssh-list-known-hosts session known-hosts)) 1)
                             (let ([forward
                                    (ssh-open-local-forward
                                     session "127.0.0.1" echo-port "127.0.0.1" 0)])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (let ([channel (ssh-forwarding-channel forward)])
                                     (and (fixnum? (ssh-forwarding-descriptor forward))
                                          (= (ssh-write-all channel (string->utf8 "ping")) 4)
                                          (equal? (ssh-read channel 4) (string->utf8 "ping")))))
                                 (lambda ()
                                   (ssh-close-forwarding forward)
                                   (ssh-close-forwarding forward))))
                             (= (ssh-remove-known-host! session
                                                       (car (ssh-list-known-hosts
                                                             session known-hosts))
                                                       known-hosts)
                                1)
                             (null? (ssh-list-known-hosts session known-hosts))))
                          (lambda ()
                            (ssh-close session)
                            (thread-join echo-thread)))))
                    (lambda () (close-socket listener)))))))
           (lambda () (stop-server))))))

(mat net-ssh-port-validation
     (mat-requires (ssh)
       (and
        (ssh-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (ssh-open "127.0.0.1" -1)))
        (ssh-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (call-with-ssh-session "127.0.0.1" 70000 #f 1000 ssh-session?))))))

(mat net-ssh-read-size-validation
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([channel (ssh-exec session "printf data")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (and
                              (ssh-error-message-contains?
                               "size must be non-negative"
                               (lambda ()
                                 (ssh-read channel -1)))
                              (ssh-error-message-contains?
                               "size must be non-negative"
                               (lambda ()
                                 (ssh-read/nonblocking channel -1)))))
                           (lambda ()
                             (ssh-close-channel channel))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-handle-closed-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (begin
                         (ssh-close session)
                         #t)
                       (ssh-error-message-contains?
                        "SSH session is closed"
                        (lambda ()
                          (%ssh-session-handle session)))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-close-channel-closed-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([channel (ssh-exec session "printf data")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (and
                              (begin
                                (ssh-close session)
                                #t)
                              (eq? (ssh-close-channel channel) channel)
                              (eq? (ssh-close-channel channel) channel)))
                           (lambda ()
                             (ssh-close-channel channel))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-port-read-closed-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([channel (ssh-exec session "printf data")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (call-with-port
                              (open-ssh-channel-input-port channel)
                              (lambda (ip)
                                (and
                                 (begin
                                   (ssh-close session)
                                   #t)
                                 (ssh-error-message-contains?
                                  "SSH session is closed"
                                  (lambda ()
                                    (get-bytevector-n ip 1)))))))
                           (lambda ()
                             (ssh-close-channel channel))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-error-port-read-closed-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([channel (ssh-exec session "sh -c 'printf err 1>&2'")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (call-with-port
                              (open-ssh-channel-error-port channel)
                              (lambda (ep)
                                (and
                                 (begin
                                   (ssh-close session)
                                   #t)
                                 (ssh-error-message-contains?
                                  "open-ssh-channel-error-port"
                                  (lambda ()
                                    (get-bytevector-n ep 1)))
                                 (ssh-error-message-contains?
                                  "SSH session is closed"
                                  (lambda ()
                                    (get-bytevector-n ep 1)))))))
                           (lambda ()
                             (ssh-close-channel channel))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-port-ops-closed-channel
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([read-ch (ssh-exec session "printf data")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (call-with-port
                              (open-ssh-channel-input-port read-ch)
                              (lambda (ip)
                                (and
                                 (begin
                                   (ssh-close-channel read-ch)
                                   #t)
                                 (ssh-error-message-contains?
                                  "SSH channel is closed"
                                  (lambda ()
                                    (get-bytevector-n ip 1)))))))
                           (lambda ()
                             (ssh-close-channel read-ch))))
                       (let ([err-ch (ssh-exec session "sh -c 'printf err 1>&2'")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (call-with-port
                              (open-ssh-channel-error-port err-ch)
                              (lambda (ep)
                                (and
                                 (begin
                                   (ssh-close-channel err-ch)
                                   #t)
                                 (ssh-error-message-contains?
                                  "open-ssh-channel-error-port"
                                  (lambda ()
                                    (get-bytevector-n ep 1)))
                                 (ssh-error-message-contains?
                                  "SSH channel is closed"
                                  (lambda ()
                                    (get-bytevector-n ep 1)))))))
                           (lambda ()
                             (ssh-close-channel err-ch))))
                       (let ([write-ch (ssh-exec session "sh -c 'IFS= read -r line; printf \"%s\" \"$line\"'")])
                         (dynamic-wind
                           void
                           (lambda ()
                             (let ([op (open-ssh-channel-output-port write-ch)])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (and
                                    (begin
                                      (ssh-close-channel write-ch)
                                      #t)
                                    (ssh-error-message-contains?
                                     "SSH channel is closed"
                                     (lambda ()
                                       (put-bytevector op (string->utf8 "x"))
                                       (flush-output-port op)
                                       (close-port op)))))
                                 (lambda ()
                                   (unless (port-closed? op)
                                     (guard (c [else #f])
                                       (close-port op)))))))
                           (lambda ()
                             (ssh-close-channel write-ch))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-ssh-stdin-eof
     (mat-requires (ssh)
       (with-test-ssh-channel
        "cat; printf eof-observed; exit 7"
        (lambda (channel)
          (ssh-write-all channel (string->utf8 "input:"))
          ;; Half-close stdin while preserving the channel's readable output streams.
          (ssh-send-eof! channel)
          (let loop ([chunks '()])
            (let ([chunk (ssh-read channel 65536 2000)])
              (if (eof-object? chunk)
                  (and (string=? (apply string-append (reverse chunks)) "input:eof-observed")
                       (= (ssh-channel-exit-status channel) 7))
                  (loop (cons (utf8->string chunk) chunks)))))))))

(mat net-ssh-fixed-reverse-port
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME" home
              (lambda ()
                (call-with-ssh-session
                 "127.0.0.1" port user
                 (lambda (session)
                   (ssh-auth-publickey! session user)
                   (let* ([requested (reserve-loopback-port)]
                          [listener (ssh-request-remote-forward! session "127.0.0.1" requested)])
                     (dynamic-wind
                       void
                       (lambda () (= requested (ssh-forwarding-port listener)))
                       (lambda () (ssh-cancel-remote-forward! listener)))))))))
           stop-server))))
