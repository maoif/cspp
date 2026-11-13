;; Shared SSH CLI, authentication, output draining, and lifecycle helpers.
(import (cspp system user) (cspp net) (cspp cli) (cspp logging))
(load "examples/net/net-example-common.ss")

(define ssh-example-options '((port . "SSH server port, 0 through 65535 (default: 22).")
   (user . "Remote SSH username (default: current local user).")
   (auth . "Authentication: publickey, agent, or password (default: publickey). Password uses CSPP_SSH_PASSWORD; agent uses SSH_AUTH_SOCK.")
   (private-key . "Private key path for publickey auth (default: discover keys in ~/.ssh). Passphrase uses CSPP_SSH_PASSPHRASE.")))

;; All SSH examples share one required positional HOST and the same named options.
(define ssh-example-command
  (lambda (name overview option-spec* exec)
    (let ([command (example-command name overview option-spec* exec)]
          [host (example-make-option 'host
                  "SSH server IP address or hostname; known_hosts verification is strict.")])
      (option-positional?-set! host #t)
      (command-options-add! command host)
      command)))

;; CSPP CLI consumes the remaining arguments at the first positional. Move named
;; options before HOST so either HOST --port=22 or --port=22 HOST works. Preserve
;; everything after -- verbatim for remote command words and escaped positionals.
(define ssh-example-arguments
  (lambda (arguments)
    (let ([options (make-list-builder)] [positionals (make-list-builder)])
      (let loop ([remaining arguments])
        (cond
         [(null? remaining) (append (options) (positionals))]
         [(string=? (car remaining) "--")
          (append (options) (positionals) remaining)]
         [else
          (if (string-startswith? (car remaining) "-")
              (options (car remaining))
              (positionals (car remaining)))
          (loop (cdr remaining))])))))

(define ssh-example-run!
  (lambda (name overview option-spec* exec arguments)
    (run-cli-command!
     (ssh-example-command name overview option-spec* exec)
     (ssh-example-arguments arguments))))

(define ssh-example-port
  (lambda (who ref name default)
    (let ([port (example-option-natural who ref name default)])
      (unless (<= 0 port 65535) (errorf who "--~a must be between 0 and 65535" name))
      port)))

(define call-with-example-ssh
  (lambda (who ref proc)
    (let* ([host (ref 'host)]
           [port (ssh-example-port who ref 'port 22)]
           [user (or (ref 'user) (uid->user-name (current-uid)))]
           [auth (or (ref 'auth) "publickey")]
           [log (make-logger who)]
           [old-interrupt (keyboard-interrupt-handler)])
      (unless (member auth '("password" "publickey" "agent"))
        (errorf who "--auth must be password, publickey, or agent"))
      (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler (lambda () (errorf who "interrupted"))))
        (lambda ()
          ;; ssh-open uses strict known-host verification against $HOME/.ssh/known_hosts.
          (call-with-ssh-session
           host port user
           (lambda (session)
             (with-logger log (log-infof "opening SSH session to ~a:~a" host port))
             (cond
              [(string=? auth "password")
               (let ([password (getenv "CSPP_SSH_PASSWORD")])
                 (unless password (errorf who "set CSPP_SSH_PASSWORD for password auth"))
                 ;; Authenticate with an environment-supplied password.
                 (ssh-auth-password! session user password))]
              [(string=? auth "publickey")
               (let ([key (ref 'private-key)] [passphrase (getenv "CSPP_SSH_PASSPHRASE")])
                 (if key
                     ;; Load the explicitly selected private key, with optional passphrase.
                     (ssh-auth-private-key! session user #f key passphrase)
                     ;; Discover a private key in the user's SSH directory.
                     (ssh-auth-publickey! session user passphrase)))]
              [else
               ;; Authenticate through the agent selected by SSH_AUTH_SOCK.
               (ssh-auth-agent! session user)])
             (proc session log))))
        (lambda ()
          (keyboard-interrupt-handler old-interrupt)
          (with-logger log (log-info "closing SSH session"))
          (logger-close! log))))))

(define ssh-example-wait
  (lambda (answer*)
    (let ([targets
           (map (lambda (answer)
                  (make-poll-target (net-would-block-resource answer)
                                    (net-would-block-events answer)))
                (filter net-would-block? answer*))])
      (if (null? targets) (milisleep 5) (poll targets 25)))))

(define ssh-example-drain
  (lambda (channel input-step)
    (call-with-example-output-ports
     (lambda (out err)
       (let loop ([out-eof? #f] [err-eof? #f])
         ;; Read stdout and stderr independently so neither stream can block the other.
         (let* ([stdout (and (not out-eof?) (ssh-read/nonblocking channel 65536))]
                [stderr (and (not err-eof?) (ssh-read-stderr/nonblocking channel 65536))]
                [out-eof? (or out-eof? (eof-object? stdout))]
                [err-eof? (or err-eof? (eof-object? stderr))])
           (when (bytevector? stdout) (put-bytevector out stdout) (flush-output-port out))
           (when (bytevector? stderr) (put-bytevector err stderr) (flush-output-port err))
           (let ([input (and (not (and out-eof? err-eof?)) (input-step))])
             (if (and out-eof? err-eof?)
                 ;; Drain both streams before retrieving the remote exit status.
                 (let ([status (ssh-channel-exit-status channel)])
                   (if (>= status 0) status 255))
                 (begin
                   ;; Wait only when neither direction moved bytes in this iteration.
                   (unless (or (and (bytevector? stdout)
                                    (fxpositive? (bytevector-length stdout)))
                               (and (bytevector? stderr)
                                    (fxpositive? (bytevector-length stderr)))
                               (and (fixnum? input) (fx> input 0)))
                     (ssh-example-wait (list stdout stderr input)))
                   (loop out-eof? err-eof?))))))))))
