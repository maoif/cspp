(library (cspp net ssh)
  (export ssh-session?
          ssh-channel?
          %ssh-session-handle
          ssh-open
          ssh-open-with-policy
          ssh-close
          ssh-auth-password!
          ssh-auth-publickey!
          ssh-auth-private-key!
          ssh-auth-keyboard-interactive!
          ssh-auth-agent!
          ssh-known-host?
          ssh-known-host-hostname
          ssh-known-host-key-type
          ssh-known-host-key
          ssh-known-host-comment
          ssh-known-host-raw
          ssh-list-known-hosts
          ssh-check-known-host
          ssh-add-known-host!
          ssh-remove-known-host!
          ssh-update-known-host!
          ssh-forwarding?
          ssh-forwarding-kind
          ssh-forwarding-channel
          ssh-forwarding-port
          ssh-forwarding-descriptor
          ssh-forwarding-closed?
          ssh-open-local-forward
          ssh-open-local-forward/nonblocking
          ssh-request-remote-forward!
          ssh-accept-remote-forward
          ssh-cancel-remote-forward!
          ssh-close-forwarding
          ssh-open-channel
          ssh-close-channel
          ssh-send-eof!
          ssh-exec
          ssh-shell
          ssh-read
          ssh-read!
          ssh-write
          ssh-write-all
          ssh-read/nonblocking
          ssh-read!/nonblocking
          ssh-read-stderr
          ssh-read-stderr!
          ssh-read-stderr/nonblocking
          ssh-read-stderr!/nonblocking
          ssh-write/nonblocking
          ssh-write-all/nonblocking
          ssh-request-pty!
          ssh-change-pty-size!
          ssh-request-environment!
          ssh-request-subsystem!
          ssh-channel-exit-status
          call-with-ssh-session
          call-with-ssh-channel
          open-ssh-channel-input-port
          open-ssh-channel-output-port
          open-ssh-channel-error-port)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp string)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net operation)
          (cspp net poll))

  #|record:ssh-session
The `ssh-session` record owns an authenticated or unauthenticated SSH transport.
Host, port, and known-hosts path identify the connection; user changes during authentication.
`ssh-close` releases the handle and all channels, after which session operations raise an error.
|#
  (define-record-type (ssh-session %make-ssh-session ssh-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle ssh-session-handle ssh-session-handle-set!)
            (immutable host ssh-session-host)
            (immutable port ssh-session-port)
            (mutable user ssh-session-user ssh-session-user-set!)
            (immutable known-hosts-path ssh-session-known-hosts-path)
            (mutable closed? ssh-session-closed? ssh-session-closed?-set!)))

  #|record:ssh-channel
The `ssh-channel` record owns one channel retained by its SSH session.
`ssh-close-channel` releases the native handle and marks it closed; closing the owning session also
invalidates it. Operations on a closed channel raise an error.
|#
  (define-record-type (ssh-channel %make-ssh-channel ssh-channel?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle ssh-channel-handle ssh-channel-handle-set!)
            (immutable session ssh-channel-session)
            (mutable closed? ssh-channel-closed? ssh-channel-closed?-set!)))

  #|record:ssh-known-host
An `ssh-known-host` is an immutable parsed known-hosts entry. `hostname` contains the host pattern,
`key-type` contains the SSH key algorithm, `key` contains its base64 data, `comment` contains the
optional trailing text, and `raw` contains the original line. The accessors return those fields.
|#
  (define-record-type (ssh-known-host %make-ssh-known-host ssh-known-host?)
    (sealed #t)
    (opaque #f)
    (fields (immutable hostname ssh-known-host-hostname)
            (immutable key-type ssh-known-host-key-type)
            (immutable key ssh-known-host-key)
            (immutable comment ssh-known-host-comment)
            (immutable raw ssh-known-host-raw)))

  #|record:ssh-forwarding
An `ssh-forwarding` owns either a direct channel or a remote listener. `kind` is `local`, `remote`,
or `accepted`; `channel` is an SSH channel or `#f`; `port` is the bound port; `descriptor` is the
pollable SSH descriptor; and `closed?` reports whether the forwarding was closed.
|#
  (define-record-type (ssh-forwarding %make-ssh-forwarding ssh-forwarding?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind ssh-forwarding-kind)
            (immutable session ssh-forwarding-session)
            (immutable channel ssh-forwarding-channel)
            (immutable address ssh-forwarding-address)
            (immutable port ssh-forwarding-port)
            (immutable descriptor ssh-forwarding-descriptor)
            (mutable closed? ssh-forwarding-closed? ssh-forwarding-closed?-set!)))

  (define ssh-default-timeout-ms 30000)

  (define default-known-hosts-path
    (lambda ()
      (let ([home (getenv "HOME")])
        (if (and home (not (string=? home "")))
            (string-append home "/.ssh/known_hosts")
            "known_hosts"))))

  (define ensure-success
    (lambda (who kind x)
      (cond
       [(ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (ssh-session-closed? session)
        (raise-net-error who 'ssh "SSH session is closed" session))))

  (define ensure-channel-open
    (lambda (who channel)
      (when (ssh-channel-closed? channel)
        (raise-net-error who 'ssh "SSH channel is closed" channel))
      (ensure-session-open who (ssh-channel-session channel))))

  (define channel-resource
    (lambda (who channel)
      (ensure-success
       who
       'ssh
       (ffi-net-ssh-session-fd
        (ssh-session-handle (ssh-channel-session channel))))))

  (define read-result
    (lambda (who channel answer)
      (cond
       [(or (bytevector? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block (channel-resource who channel)
                              (ffi-would-block-events answer))]
       [else (ensure-success who 'ssh answer)])))

  (define read-into-result
    (lambda (who channel answer)
      (cond
       [(or (fixnum? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block (channel-resource who channel)
                              (ffi-would-block-events answer))]
       [else (ensure-success who 'ssh answer)])))

  (define write-result
    (lambda (who channel answer)
      (cond
       [(fixnum? answer) answer]
       [(ffi-would-block? answer)
        (make-net-would-block (channel-resource who channel)
                              (ffi-would-block-events answer))]
       [else (ensure-success who 'ssh answer)])))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

  (define ensure-user-maybe
    (lambda (who user)
      (unless (or (string? user) (eq? user #f))
        (errorf who "expected string or #f, given ~s" user))))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-time-ms
    (lambda ()
      (let ([t (current-time)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (+ (current-time-ms) timeout-ms)))

  (define remaining-timeout-ms
    (lambda (deadline-ms)
      (let ([remaining (fx- deadline-ms (current-time-ms))])
        (if (fx<= remaining 0) 0 remaining))))

  (define make-binary-input-port
    (lambda (who-name channel stderr?)
      (make-custom-binary-input-port
       (if stderr? "cspp-ssh-error" "cspp-ssh-input")
       (lambda (bv start count)
         (ensure-channel-open who-name channel)
         (let ([stop (fx+ start count)])
           (let ([n ((if stderr? ssh-read-stderr! ssh-read!)
                     channel bv start stop)])
             (cond
              [(fixnum? n) n]
              [(eof-object? n) 0]
              [else (errorf who-name
                            "unexpected nonblocking result from blocking SSH port read")]))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (channel)
      (make-custom-binary-output-port
       "cspp-ssh-output"
       (lambda (bv start count)
         (ssh-write-all channel bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define request-exec!
    (case-lambda
      [(who channel cmd)
       (request-exec! who channel cmd -1)]
      [(who channel cmd timeout-ms)
       (ensure-success who 'ssh
                       (ffi-net-ssh-channel-request-exec (ssh-channel-handle channel)
                                                         cmd
                                                         timeout-ms))
       channel]))

  (define request-shell!
    (case-lambda
      [(who channel)
       (request-shell! who channel -1)]
      [(who channel timeout-ms)
       (ensure-success who 'ssh
                       (ffi-net-ssh-channel-request-shell (ssh-channel-handle channel)
                                                          timeout-ms))
       channel]))

  #|proc:%ssh-session-handle
The `%ssh-session-handle` procedure returns the foreign handle stored inside an SSH session record.
|#
  (define-who %ssh-session-handle
    (lambda (session)
      (pcheck ([ssh-session? session])
              (ensure-session-open who session)
              (ssh-session-handle session))))

  #|proc:ssh-open
The `ssh-open` procedure opens a network SSH session to a remote host using strict host-key
verification.
|#
  (define-who ssh-open
    (case-lambda
      [(host) (ssh-open host 22 #f ssh-default-timeout-ms)]
      [(host port) (ssh-open host port #f ssh-default-timeout-ms)]
      [(host port user-or-timeout)
       (if (fixnum? user-or-timeout)
           (ssh-open host port #f user-or-timeout)
           (ssh-open host port user-or-timeout ssh-default-timeout-ms))]
      [(host port user timeout-ms)
       (pcheck ([string? host] [fixnum? port])
               (check-port who port)
               (ensure-user-maybe who user)
               (ssh-open-with-policy host port user timeout-ms 'strict))]))

  #|proc:ssh-open-with-policy
The `ssh-open-with-policy` procedure opens an SSH session using an explicit host-key policy.
The policy must be one of `strict`, `accept-new`, or `insecure`.
|#
  (define-who ssh-open-with-policy
    (lambda (host port user timeout-ms policy)
      (pcheck ([string? host] [fixnum? port])
              (check-port who port)
              (ensure-user-maybe who user)
              (check-timeout-ms who timeout-ms)
              (let ([policy-int (case policy
                                  [(strict) 0]
                                  [(accept-new) 1]
                                  [(insecure) 2]
                                  [else
                                   (errorf who "invalid SSH host-key policy ~s" policy)])])
                (let ([ans (ensure-success who 'ssh
                                           (ffi-net-ssh-open host
                                                             port
                                                             (or user "")
                                                             timeout-ms
                                                             policy-int))])
                  (%make-ssh-session ans host port user (default-known-hosts-path) #f))))))

  #|proc:ssh-close
The `ssh-close` procedure closes an SSH session and releases its foreign resources.
|#
  (define-who ssh-close
    (lambda (session)
      (pcheck ([ssh-session? session])
              (unless (ssh-session-closed? session)
                (ensure-success who 'ssh (ffi-net-ssh-close (ssh-session-handle session)))
                (ssh-session-handle-set! session 0)
                (ssh-session-closed?-set! session #t))
              session)))

  #|proc:ssh-auth-password!
The `ssh-auth-password!` procedure authenticates an SSH session with a password.
|#
  (define-who ssh-auth-password!
    (case-lambda
      [(session password)
       (ssh-auth-password! session #f password)]
      [(session user password)
       (pcheck ([ssh-session? session] [string? password])
               (ensure-user-maybe who user)
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-password (ssh-session-handle session)
                                                          (or user "")
                                                          password))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  #|proc:ssh-auth-publickey!
The `ssh-auth-publickey!` procedure authenticates an SSH session using libssh's automatic
public-key discovery.
|#
  (define-who ssh-auth-publickey!
    (case-lambda
      [(session)
       (ssh-auth-publickey! session #f #f)]
      [(session user)
       (ssh-auth-publickey! session user #f)]
      [(session user passphrase)
       (pcheck ([ssh-session? session])
               (ensure-user-maybe who user)
               (unless (or (string? passphrase) (eq? passphrase #f))
                 (errorf who "expected string or #f for passphrase"))
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-publickey-auto (ssh-session-handle session)
                                                                (or user "")
                                                                (or passphrase "")))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  #|proc:ssh-auth-private-key!
The `ssh-auth-private-key!` procedure authenticates `session` as `user` using `public-key-path`
and `private-key-path`. `public-key-path` may be `#f` to skip the preliminary public-key offer,
and `passphrase` may be `#f` for an unencrypted private key. It returns `session` on success.
|#
  (define-who ssh-auth-private-key!
    (lambda (session user public-key-path private-key-path passphrase)
      (pcheck ([ssh-session? session] [string? private-key-path])
              (ensure-user-maybe who user)
              (unless (or (string? public-key-path) (eq? public-key-path #f))
                (errorf who "public-key-path must be a string or #f, given ~s" public-key-path))
              (unless (or (string? passphrase) (eq? passphrase #f))
                (errorf who "passphrase must be a string or #f, given ~s" passphrase))
              (ensure-session-open who session)
              (ensure-success who 'ssh
                              (ffi-net-ssh-auth-publickey
                               (ssh-session-handle session)
                               (or user "")
                               (or public-key-path "")
                               private-key-path
                               (or passphrase "")))
              (when user
                (ssh-session-user-set! session user))
              session)))

  #|proc:ssh-auth-keyboard-interactive!
The `ssh-auth-keyboard-interactive!` procedure authenticates `session` as `user`. `responder` has
signature `(name instruction prompts echo-flags) -> list-of-strings`; it receives prompt strings
and matching booleans indicating whether replies may be echoed. The procedure returns `session`.
|#
  (define-who ssh-auth-keyboard-interactive!
    (lambda (session user responder)
      (pcheck ([ssh-session? session] [procedure? responder])
              (ensure-user-maybe who user)
              (ensure-session-open who session)
              (let loop ()
                (let ([answer
                       (ffi-net-ssh-auth-keyboard-interactive-step
                        (ssh-session-handle session) (or user ""))])
                  (cond
                   [(eq? answer #t)
                    (when user
                      (ssh-session-user-set! session user))
                    session]
                   [(vector? answer)
                    (let* ([prompts (vector->list (vector-ref answer 2))]
                           [echo-flags (vector->list (vector-ref answer 3))]
                           [responses (responder (vector-ref answer 0)
                                                 (vector-ref answer 1)
                                                 prompts
                                                 echo-flags)])
                      (unless (and (list? responses)
                                   (= (length responses) (length prompts))
                                   (for-all string? responses))
                        (errorf who
                                "responder must return one string per prompt, given ~s"
                                responses))
                      (let answer-loop ([rest responses] [index 0])
                        (unless (null? rest)
                          (ensure-success
                           who 'ssh
                           (ffi-net-ssh-auth-keyboard-interactive-answer
                            (ssh-session-handle session) index (car rest)))
                          (answer-loop (cdr rest) (fx1+ index))))
                      (loop))]
                   [else (ensure-success who 'ssh answer)]))))))

  #|proc:ssh-auth-agent!
The `ssh-auth-agent!` procedure authenticates `session` as `user` using the local SSH agent.
`identity` may be a selected identity path or `#f` to allow the agent's available identities. It
returns `session` on success.
|#
  (define-who ssh-auth-agent!
    (case-lambda
      [(session)
       (ssh-auth-agent! session #f #f)]
      [(session user)
       (ssh-auth-agent! session user #f)]
      [(session user identity)
       (pcheck ([ssh-session? session])
               (ensure-user-maybe who user)
               (unless (or (string? identity) (eq? identity #f))
                 (errorf who "identity must be a string or #f, given ~s" identity))
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-agent-identity
                                (ssh-session-handle session)
                                (or user "")
                                (or identity "")))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  (define known-hosts-path
    (lambda (who session path)
      (ensure-session-open who session)
      (cond
       [(eq? path #f) (ssh-session-known-hosts-path session)]
       [(string? path) path]
       [else (errorf who "known-hosts path must be a string or #f, given ~s" path)])))

  (define nonempty-string-parts
    (lambda (line)
      (let ([n (string-length line)])
        (let loop ([index 0] [start #f] [out '()])
          (cond
           [(fx= index n)
            (reverse (if start (cons (substring line start index) out) out))]
           [(char-whitespace? (string-ref line index))
            (loop (fx1+ index) #f
                  (if start (cons (substring line start index) out) out))]
           [else (loop (fx1+ index) (or start index) out)])))))

  (define join-string-parts
    (lambda (part*)
      (let loop ([rest part*] [out ""])
        (if (null? rest)
            out
            (loop (cdr rest)
                  (if (string=? out "")
                      (car rest)
                      (string-append out " " (car rest))))))))

  (define parse-known-host-line
    (lambda (line)
      (let ([part* (nonempty-string-parts line)])
        (and (not (string=? line ""))
             (not (char=? (string-ref line 0) #\#))
             (>= (length part*) 3)
             (%make-ssh-known-host (car part*)
                                   (cadr part*)
                                   (caddr part*)
                                   (join-string-parts (cdddr part*))
                                   line)))))

  (define read-known-host-lines
    (lambda (path)
      (if (not (file-exists? path))
          '()
          (call-with-port
           (open-file-input-port path
                                 (file-options)
                                 (buffer-mode block)
                                 (native-transcoder))
           (lambda (input)
             (let loop ([out '()])
               (let ([line (get-line input)])
                 (if (eof-object? line)
                     (reverse out)
                     (loop (cons line out))))))))))

  (define write-known-host-lines
    (lambda (path line*)
      (call-with-port
       (open-file-output-port path
                              (file-options no-fail replace)
                              (buffer-mode block)
                              (native-transcoder))
       (lambda (output)
         (for-each (lambda (line) (put-string output line) (newline output)) line*)))))

  #|proc:ssh-list-known-hosts
The `ssh-list-known-hosts` procedure parses entries from `path`, or from `session`'s known-hosts
path when `path` is `#f` or omitted. It returns a list of `ssh-known-host` records without changing
the session's trust policy.
|#
  (define-who ssh-list-known-hosts
    (case-lambda
      [(session) (ssh-list-known-hosts session #f)]
      [(session path)
       (pcheck ([ssh-session? session])
               (let loop ([line* (read-known-host-lines (known-hosts-path who session path))]
                          [out '()])
                 (if (null? line*)
                     (reverse out)
                     (let ([entry (parse-known-host-line (car line*))])
                       (loop (cdr line*) (if entry (cons entry out) out))))))]))

  #|proc:ssh-check-known-host
The `ssh-check-known-host` procedure checks the connected server against `path`, or `session`'s
known-hosts path when omitted. It returns `ok`, `not-found`, `unknown`, `changed`, or `other` and
does not add or update an entry.
|#
  (define-who ssh-check-known-host
    (case-lambda
      [(session) (ssh-check-known-host session #f)]
      [(session path)
       (pcheck ([ssh-session? session])
               (ensure-success
                who 'ssh
                (ffi-net-ssh-known-host-check
                 (ssh-session-handle session) (known-hosts-path who session path))))]))

  (define export-known-host
    (lambda (who session)
      (let* ([raw (ensure-success who 'ssh
                                  (ffi-net-ssh-known-host-export
                                   (ssh-session-handle session)))]
             [n (string-length raw)]
             [line (if (and (fx> n 0) (char=? (string-ref raw (fx1- n)) #\newline))
                       (substring raw 0 (fx1- n))
                       raw)]
             [entry (parse-known-host-line line)])
        (or entry (errorf who "libssh returned an invalid known-host entry ~s" raw)))))

  #|proc:ssh-add-known-host!
The `ssh-add-known-host!` procedure adds the connected server to `path`, or `session`'s path when
omitted. It accepts only `unknown` or `not-found` state and returns the added `ssh-known-host`.
|#
  (define-who ssh-add-known-host!
    (case-lambda
      [(session) (ssh-add-known-host! session #f)]
      [(session path)
       (pcheck ([ssh-session? session])
               (let* ([target (known-hosts-path who session path)]
                      [state (ssh-check-known-host session target)])
                 (unless (memq state '(unknown not-found))
                   (errorf who "known host cannot be added from state ~s" state))
                 (ensure-success who 'ssh
                                 (ffi-net-ssh-known-host-update
                                  (ssh-session-handle session) target))
                 (export-known-host who session)))]))

  #|proc:ssh-remove-known-host!
The `ssh-remove-known-host!` procedure removes entries matching `hostname` from `path`, or from
`session`'s path when omitted. `hostname` may be a string or an `ssh-known-host`; the return value
is the number of removed entries.
|#
  (define-who ssh-remove-known-host!
    (case-lambda
      [(session hostname) (ssh-remove-known-host! session hostname #f)]
      [(session hostname path)
       (pcheck ([ssh-session? session])
               (let* ([name (if (ssh-known-host? hostname)
                                (ssh-known-host-hostname hostname)
                                hostname)]
                      [target (known-hosts-path who session path)])
                 (unless (string? name)
                   (errorf who "hostname must be a string or ssh-known-host, given ~s" hostname))
                 (let loop ([line* (read-known-host-lines target)] [kept '()] [removed 0])
                   (if (null? line*)
                       (begin
                         (write-known-host-lines target (reverse kept))
                         removed)
                       (let ([entry (parse-known-host-line (car line*))])
                         (if (and entry (string=? name (ssh-known-host-hostname entry)))
                             (loop (cdr line*) kept (fx1+ removed))
                             (loop (cdr line*) (cons (car line*) kept) removed)))))))]))

  #|proc:ssh-update-known-host!
The `ssh-update-known-host!` procedure explicitly replaces the connected server's entry in `path`,
or in `session`'s path when omitted. It returns the new `ssh-known-host` and never changes policy.
|#
  (define-who ssh-update-known-host!
    (case-lambda
      [(session) (ssh-update-known-host! session #f)]
      [(session path)
       (pcheck ([ssh-session? session])
               (let* ([target (known-hosts-path who session path)]
                      [entry (export-known-host who session)])
                 (ssh-remove-known-host! session (ssh-known-host-hostname entry) target)
                 (ensure-success who 'ssh
                                 (ffi-net-ssh-known-host-update
                                  (ssh-session-handle session) target))
                 (export-known-host who session)))]))

  #|proc:ssh-open-channel
The `ssh-open-channel` procedure opens a new SSH session channel.
|#
  (define-who ssh-open-channel
    (case-lambda
      [(session)
       (pcheck ([ssh-session? session])
               (ensure-session-open who session)
               (%make-ssh-channel
                (ensure-success who 'ssh
                                (ffi-net-ssh-channel-open (ssh-session-handle session) -1))
                session
                #f))]
      [(session timeout-ms)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-session-open who session)
               (%make-ssh-channel
                (ensure-success who 'ssh
                                (ffi-net-ssh-channel-open (ssh-session-handle session)
                                                          timeout-ms))
                session
                #f))]))

  #|proc:ssh-open-local-forward
The `ssh-open-local-forward` procedure opens a direct TCP/IP forwarding channel through `session`.
`remote-host` and `remote-port` select the destination; `source-host` and `source-port` describe
the originator; and `timeout-ms` bounds setup. It returns an `ssh-forwarding` with a channel.
|#
  (define-who ssh-open-local-forward
    (case-lambda
      [(session remote-host remote-port)
       (ssh-open-local-forward session remote-host remote-port "127.0.0.1" 0
                               ssh-default-timeout-ms)]
      [(session remote-host remote-port source-host source-port)
       (ssh-open-local-forward session remote-host remote-port source-host source-port
                               ssh-default-timeout-ms)]
      [(session remote-host remote-port source-host source-port timeout-ms)
       (pcheck ([ssh-session? session] [string? remote-host source-host]
                [fixnum? remote-port source-port timeout-ms])
               (check-port who remote-port)
               (check-port who source-port)
               (check-timeout-ms who timeout-ms)
               (ensure-session-open who session)
               (let* ([handle
                       (ensure-success
                        who 'ssh
                        (ffi-net-ssh-channel-open-forward
                         (ssh-session-handle session) remote-host remote-port
                         source-host source-port timeout-ms))]
                      [channel (%make-ssh-channel handle session #f)])
                 (%make-ssh-forwarding 'local session channel remote-host remote-port
                                       (channel-resource who channel) #f)))]))

  #|proc:ssh-open-local-forward/nonblocking
The `ssh-open-local-forward/nonblocking` procedure constructs a direct TCP/IP open operation.
The authenticated `session` must remain open until the operation terminates and its result closes.
The `remote-host` and `remote-port` parameters identify the destination resolved by the server.
Optional `source-host` and `source-port` identify the originator (default: `127.0.0.1`, zero).
Optional `timeout-ms` bounds setup in milliseconds (default: 30000, nonnegative fixnum).
Returns a pending network operation. Each step attempts setup without waiting; completion returns
an `ssh-forwarding` that the caller must close. Failure or cancellation frees the pending channel.
|#
  (define-who ssh-open-local-forward/nonblocking
    (case-lambda
      [(session remote-host remote-port)
       (ssh-open-local-forward/nonblocking session remote-host remote-port "127.0.0.1" 0
                                           ssh-default-timeout-ms)]
      [(session remote-host remote-port source-host source-port)
       (ssh-open-local-forward/nonblocking session remote-host remote-port source-host source-port
                                           ssh-default-timeout-ms)]
      [(session remote-host remote-port source-host source-port timeout-ms)
       (pcheck ([ssh-session? session] [string? remote-host source-host]
                [fixnum? remote-port source-port timeout-ms])
         (ensure-session-open who session)
         (check-port who remote-port)
         (check-port who source-port)
         (check-timeout-ms who timeout-ms)
         (let* ([now-ms (lambda ()
                          (let ([time (current-time 'time-monotonic)])
                            (+ (* (time-second time) 1000)
                               (quotient (time-nanosecond time) 1000000))))]
                [deadline (+ (now-ms) timeout-ms)]
                [handle (ensure-success who 'ssh
                          (ffi-net-ssh-channel-forward-start (ssh-session-handle session)))]
                [transferred? #f])
           (make-net-operation
            'ssh-open-local-forward
            (lambda ()
              (ensure-session-open who session)
              (when (>= (now-ms) deadline)
                (raise-net-error who 'timeout "SSH forwarding channel open timed out" session))
              (let ([answer (ffi-net-ssh-channel-forward-step
                             handle remote-host remote-port source-host source-port)])
                (if (ffi-would-block? answer)
                    (net-operation-pending
                     (list (make-poll-target
                            (ffi-net-ssh-session-fd (ssh-session-handle session))
                            (ffi-would-block-events answer))) deadline)
                    (begin
                      (ensure-success who 'ssh answer)
                      (let* ([channel (%make-ssh-channel handle session #f)]
                             [forwarding (%make-ssh-forwarding
                                          'local session channel remote-host remote-port
                                          (channel-resource who channel) #f)])
                        (set! transferred? #t)
                        (net-operation-completed forwarding))))))
            void
            (lambda ()
              (unless transferred?
                (ffi-net-ssh-channel-forward-discard handle))))))]))

  #|proc:ssh-request-remote-forward!
The `ssh-request-remote-forward!` procedure asks `session` to listen on `address` and `port` on the
server. Port zero requests an available port. It returns an `ssh-forwarding` listener containing
the bound port and a pollable descriptor.
|#
  (define-who ssh-request-remote-forward!
    (lambda (session address port)
      (pcheck ([ssh-session? session] [string? address] [fixnum? port])
              (check-port who port)
              (ensure-session-open who session)
              (let ([bound-port
                     (ensure-success
                      who 'ssh
                      (ffi-net-ssh-remote-forward-listen
                       (ssh-session-handle session) address port))])
                (%make-ssh-forwarding
                 'remote session #f address bound-port
                 (ffi-net-ssh-session-fd (ssh-session-handle session)) #f)))))

  #|proc:ssh-accept-remote-forward
The `ssh-accept-remote-forward` procedure accepts one pending channel from remote `listener`. It
returns an `ssh-forwarding` containing the accepted channel, or a `net-would-block` carrying the
listener descriptor and `read` event when no connection is ready.
|#
  (define-who ssh-accept-remote-forward
    (lambda (listener)
      (pcheck ([ssh-forwarding? listener])
              (unless (eq? (ssh-forwarding-kind listener) 'remote)
                (errorf who "expected a remote forwarding listener"))
              (when (ssh-forwarding-closed? listener)
                (raise-net-error who 'ssh "SSH forwarding listener is closed" listener))
              (let ([answer
                     (ffi-net-ssh-remote-forward-accept
                      (ssh-session-handle (ssh-forwarding-session listener)))])
                (cond
                 [(ffi-would-block? answer)
                  (make-net-would-block (ssh-forwarding-descriptor listener) '(read))]
                 [else
                  (let ([channel
                         (%make-ssh-channel (ensure-success who 'ssh answer)
                                            (ssh-forwarding-session listener) #f)])
                    (%make-ssh-forwarding
                     'accepted (ssh-forwarding-session listener) channel
                     (ssh-forwarding-address listener) (ssh-forwarding-port listener)
                     (ssh-forwarding-descriptor listener) #f))])))))

  #|proc:ssh-cancel-remote-forward!
The `ssh-cancel-remote-forward!` procedure cancels remote `listener` and returns it. Closing an
already closed listener is harmless.
|#
  (define-who ssh-cancel-remote-forward!
    (lambda (listener)
      (pcheck ([ssh-forwarding? listener])
              (unless (eq? (ssh-forwarding-kind listener) 'remote)
                (errorf who "expected a remote forwarding listener"))
              (ssh-close-forwarding listener))))

  #|proc:ssh-close-forwarding
The `ssh-close-forwarding` procedure closes `forwarding` idempotently. It closes local or accepted
channels and cancels a remote listener. The return value is `forwarding`.
|#
  (define-who ssh-close-forwarding
    (lambda (forwarding)
      (pcheck ([ssh-forwarding? forwarding])
              (unless (ssh-forwarding-closed? forwarding)
                (case (ssh-forwarding-kind forwarding)
                  [(remote)
                   (ensure-session-open who (ssh-forwarding-session forwarding))
                   (ensure-success
                    who 'ssh
                    (ffi-net-ssh-remote-forward-cancel
                     (ssh-session-handle (ssh-forwarding-session forwarding))
                     (ssh-forwarding-address forwarding)
                     (ssh-forwarding-port forwarding)))]
                  [(local accepted)
                   (ssh-close-channel (ssh-forwarding-channel forwarding))]
                  [else (errorf who "invalid SSH forwarding kind ~s"
                                (ssh-forwarding-kind forwarding))])
                (ssh-forwarding-closed?-set! forwarding #t))
              forwarding)))

  #|proc:ssh-request-environment!
The `ssh-request-environment!` procedure requests environment variable `name` with `value` on open
`channel`. Both parameters are strings. The return value is `channel` on success.
|#
  (define-who ssh-request-environment!
    (lambda (channel name value)
      (pcheck ([ssh-channel? channel] [string? name value])
              (ensure-channel-open who channel)
              (when (string=? name "")
                (errorf who "environment variable name must not be empty"))
              (ensure-success who 'ssh
                              (ffi-net-ssh-channel-request-environment
                               (ssh-channel-handle channel) name value))
              channel)))

  #|proc:ssh-request-subsystem!
The `ssh-request-subsystem!` procedure requests non-empty `subsystem` on open `channel`. The
`subsystem` parameter is a string. The return value is `channel` on success.
|#
  (define-who ssh-request-subsystem!
    (lambda (channel subsystem)
      (pcheck ([ssh-channel? channel] [string? subsystem])
              (ensure-channel-open who channel)
              (when (string=? subsystem "")
                (errorf who "subsystem must not be empty"))
              (ensure-success who 'ssh
                              (ffi-net-ssh-channel-request-subsystem
                               (ssh-channel-handle channel) subsystem))
              channel)))

  #|proc:ssh-send-eof!
The `ssh-send-eof!` procedure half-closes the stdin stream of open `channel`.
The `channel` parameter is an SSH channel. Remote stdout and stderr remain readable.
It returns `channel` after sending EOF; later writes to the channel must be avoided.
|#
  (define-who ssh-send-eof!
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (ensure-success who 'ssh
                              (ffi-net-ssh-channel-send-eof (ssh-channel-handle channel)))
              channel)))

  #|proc:ssh-close-channel
The `ssh-close-channel` procedure closes an SSH channel and releases its foreign resources.
|#
  (define-who ssh-close-channel
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (unless (ssh-channel-closed? channel)
                (when (guard (c [else #f])
                        (not (fx= 0 (%ssh-session-handle (ssh-channel-session channel)))))
                  (ensure-success who 'ssh
                                  (ffi-net-ssh-channel-close (ssh-channel-handle channel))))
                (ssh-channel-handle-set! channel 0)
                (ssh-channel-closed?-set! channel #t))
              channel)))

  #|proc:ssh-exec
The `ssh-exec` procedure requests remote command execution on an SSH channel.
|#
  (define-who ssh-exec
    (case-lambda
      [(channel cmd)
       (ssh-exec channel cmd -1)]
      [(channel cmd timeout-ms)
       (cond
        [(ssh-channel? channel)
         (pcheck ([string? cmd] [fixnum? timeout-ms])
                 (when (fx>= timeout-ms 0)
                   (check-timeout-ms who timeout-ms))
                 (ensure-channel-open who channel)
                 (request-exec! who channel cmd timeout-ms))]
        [(ssh-session? channel)
         (pcheck ([string? cmd] [fixnum? timeout-ms])
                 (let* ([deadline-ms (and (fx>= timeout-ms 0)
                                          (begin
                                            (check-timeout-ms who timeout-ms)
                                            (timeout->deadline-ms timeout-ms)))]
                        [ch (if deadline-ms
                                (ssh-open-channel channel
                                                  (remaining-timeout-ms deadline-ms))
                                (ssh-open-channel channel))])
                   (guard (c [else (ssh-close-channel ch) (raise c)])
                     (request-exec! who
                                    ch
                                    cmd
                                    (if deadline-ms
                                        (remaining-timeout-ms deadline-ms)
                                        -1)))))]
        [else
         (errorf who "expected ssh channel or session, given ~s" channel)])]))

  #|proc:ssh-shell
The `ssh-shell` procedure requests an interactive shell on an SSH channel.
|#
  (define-who ssh-shell
    (case-lambda
      [(target)
       (ssh-shell target -1)]
      [(target timeout-ms)
       (cond
        [(ssh-channel? target)
         (pcheck ([fixnum? timeout-ms])
                 (when (fx>= timeout-ms 0)
                   (check-timeout-ms who timeout-ms))
                 (ensure-channel-open who target)
                 (request-shell! who target timeout-ms))]
        [(ssh-session? target)
         (pcheck ([fixnum? timeout-ms])
                 (let* ([deadline-ms (and (fx>= timeout-ms 0)
                                          (begin
                                            (check-timeout-ms who timeout-ms)
                                            (timeout->deadline-ms timeout-ms)))]
                        [ch (if deadline-ms
                                (ssh-open-channel target
                                                  (remaining-timeout-ms deadline-ms))
                                (ssh-open-channel target))])
                   (guard (c [else (ssh-close-channel ch) (raise c)])
                     (request-shell! who
                                     ch
                                     (if deadline-ms
                                         (remaining-timeout-ms deadline-ms)
                                         -1)))))]
        [else
         (errorf who "expected ssh channel or session, given ~s" target)])]))

  #|proc:ssh-request-pty!
The `ssh-request-pty!` procedure requests a pseudo-terminal on open `channel` and returns it.
`terminal` names its TERM value, and positive `columns` and `rows` give the character size.
The defaults are xterm, 80 columns, and 24 rows. `timeout-ms` bounds the wait in milliseconds;
-1 waits without a deadline. Request the terminal before starting a shell or command.
|#
  (define-who ssh-request-pty!
    (case-lambda
      [(channel)
       (ssh-request-pty! channel -1)]
      [(channel timeout-ms)
       (ssh-request-pty! channel "xterm" 80 24 timeout-ms)]
      [(channel terminal columns rows)
       (ssh-request-pty! channel terminal columns rows -1)]
      [(channel terminal columns rows timeout-ms)
       (pcheck ([ssh-channel? channel] [string? terminal] [fixnum? columns rows timeout-ms])
               (when (or (string=? terminal "") (string-contains? terminal (string #\nul)))
                 (errorf who "terminal must be nonempty and contain no NUL"))
               (unless (and (fx<= 1 columns 65535) (fx<= 1 rows 65535))
                 (errorf who "terminal columns and rows must be between 1 and 65535"))
               (when (fx>= timeout-ms 0)
                 (check-timeout-ms who timeout-ms))
               (ensure-channel-open who channel)
               (ensure-success who 'ssh
                               (ffi-net-ssh-channel-request-pty-size
                                (ssh-channel-handle channel) terminal columns rows #f timeout-ms))
               channel)]))

  #|proc:ssh-change-pty-size!
The `ssh-change-pty-size!` procedure updates the PTY on open `channel` and returns it.
Positive `columns` and `rows` specify the new character size, each at most 65535.
`timeout-ms` bounds the wait in milliseconds; its default -1 waits without a deadline.
|#
  (define-who ssh-change-pty-size!
    (case-lambda
      [(channel columns rows) (ssh-change-pty-size! channel columns rows -1)]
      [(channel columns rows timeout-ms)
       (pcheck ([ssh-channel? channel] [fixnum? columns rows timeout-ms])
         (unless (and (fx<= 1 columns 65535) (fx<= 1 rows 65535))
           (errorf who "terminal columns and rows must be between 1 and 65535"))
         (when (fx>= timeout-ms 0) (check-timeout-ms who timeout-ms))
         (ensure-channel-open who channel)
         (ensure-success who 'ssh (ffi-net-ssh-channel-request-pty-size
                                   (ssh-channel-handle channel) "" columns rows #t timeout-ms))
         channel)]))

  #|proc:ssh-read
The `ssh-read` procedure reads up to `size` bytes from an SSH channel's stdout stream.
|#
  (define-who ssh-read
    (case-lambda
      [(channel size)
       (pcheck ([ssh-channel? channel] [fixnum? size])
               (check-size who size)
               (ensure-channel-open who channel)
               (read-result who channel
                            (ffi-net-ssh-channel-read (ssh-channel-handle channel)
                                                      size
                                                      0
                                                      0
                                                      -1)))]
      [(channel size timeout-ms)
       (pcheck ([ssh-channel? channel] [fixnum? size])
               (check-size who size)
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (read-result who channel
                            (ffi-net-ssh-channel-read (ssh-channel-handle channel)
                                                      size
                                                      0
                                                      0
                                                      timeout-ms)))]))

  #|proc:ssh-read/nonblocking
The `ssh-read/nonblocking` procedure attempts one stdout read from `channel`.
The `channel` parameter is an open SSH channel. The `size` parameter is the maximum byte count.
The return value is a bytevector, EOF, or a would-block value naming the SSH descriptor.
|#
  (define-who ssh-read/nonblocking
    (lambda (channel size)
      (pcheck ([ssh-channel? channel] [fixnum? size])
              (check-size who size)
              (ensure-channel-open who channel)
              (read-result who channel
                           (ffi-net-ssh-channel-read (ssh-channel-handle channel) size 0 1 -1)))))

  #|proc:ssh-read!
The `ssh-read!` procedure reads into a bytevector slice from an SSH channel's stdout stream.
|#
  (define-who ssh-read!
    (case-lambda
      [(channel bv) (ssh-read! channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-read! channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                channel
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               0
                                               -1)))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                channel
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               0
                                               timeout-ms)))]))

  #|proc:ssh-read!/nonblocking
The `ssh-read!/nonblocking` procedure attempts one stdout read into `bv`.
The `channel` parameter is an open SSH channel. The `bv` parameter receives the bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is a byte count, EOF, or a would-block value naming the SSH descriptor.
|#
  (define-who ssh-read!/nonblocking
    (case-lambda
      [(channel bv) (ssh-read!/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-read!/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                channel
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               1
                                               -1)))]))

  #|proc:ssh-read-stderr
The `ssh-read-stderr` procedure reads up to `size` bytes from `channel`'s stderr stream.
The `channel` parameter is an open SSH channel. The `size` parameter is the maximum byte count.
The optional `timeout-ms` parameter is a nonnegative timeout in milliseconds.
The return value is a bytevector or EOF. A timeout or transport failure is raised.
|#
  (define-who ssh-read-stderr
    (case-lambda
      [(channel size)
       (pcheck ([ssh-channel? channel] [fixnum? size])
               (check-size who size)
               (ensure-channel-open who channel)
               (read-result
                who channel
                (ffi-net-ssh-channel-read
                 (ssh-channel-handle channel) size 1 0 -1)))]
      [(channel size timeout-ms)
       (pcheck ([ssh-channel? channel] [fixnum? size] [fixnum? timeout-ms])
               (check-size who size)
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (read-result
                who channel
                (ffi-net-ssh-channel-read
                 (ssh-channel-handle channel) size 1 0 timeout-ms)))]))

  #|proc:ssh-read-stderr/nonblocking
The `ssh-read-stderr/nonblocking` procedure attempts one stderr read from `channel`.
The `channel` parameter is an open SSH channel. The `size` parameter is the maximum byte count.
The return value is a bytevector, EOF, or a would-block value naming the SSH descriptor and
its requested transport events.
|#
  (define-who ssh-read-stderr/nonblocking
    (lambda (channel size)
      (pcheck ([ssh-channel? channel] [fixnum? size])
              (check-size who size)
              (ensure-channel-open who channel)
              (read-result
               who channel
               (ffi-net-ssh-channel-read
                (ssh-channel-handle channel) size 1 1 -1)))))

  #|proc:ssh-read-stderr!
The `ssh-read-stderr!` procedure reads `channel`'s stderr into `bytevector`.
The `channel` parameter is an open SSH channel. The `bytevector` parameter receives bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The optional `timeout-ms` parameter is a nonnegative timeout in milliseconds.
The return value is a byte count or EOF. A timeout or transport failure is raised.
|#
  (define-who ssh-read-stderr!
    (case-lambda
      [(channel bytevector)
       (ssh-read-stderr! channel bytevector 0 (bytevector-length bytevector))]
      [(channel bytevector start)
       (ssh-read-stderr! channel bytevector start (bytevector-length bytevector))]
      [(channel bytevector start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bytevector])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bytevector) start stop)
               (read-into-result
                who channel
                (ffi-net-ssh-channel-read-into
                 (ssh-channel-handle channel) bytevector start stop 1 0 -1)))]
      [(channel bytevector start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bytevector] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bytevector) start stop)
               (read-into-result
                who channel
                (ffi-net-ssh-channel-read-into
                 (ssh-channel-handle channel)
                 bytevector start stop 1 0 timeout-ms)))]))

  #|proc:ssh-read-stderr!/nonblocking
The `ssh-read-stderr!/nonblocking` procedure attempts one stderr read into `bytevector`.
The `channel` parameter is an open SSH channel. The `bytevector` parameter receives bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is a byte count, EOF, or a would-block value naming the SSH descriptor and
its requested transport events.
|#
  (define-who ssh-read-stderr!/nonblocking
    (case-lambda
      [(channel bytevector)
       (ssh-read-stderr!/nonblocking
        channel bytevector 0 (bytevector-length bytevector))]
      [(channel bytevector start)
       (ssh-read-stderr!/nonblocking
        channel bytevector start (bytevector-length bytevector))]
      [(channel bytevector start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bytevector])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bytevector) start stop)
               (read-into-result
                who channel
                (ffi-net-ssh-channel-read-into
                 (ssh-channel-handle channel) bytevector start stop 1 1 -1)))]))

  #|proc:ssh-write
The `ssh-write` procedure writes a bytevector slice to an SSH channel's stdin stream.
|#
  (define-who ssh-write
    (case-lambda
      [(channel bv) (ssh-write channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                channel
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           0
                                           -1)))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                channel
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           0
                                           timeout-ms)))]))

  #|proc:ssh-write/nonblocking
The `ssh-write/nonblocking` procedure attempts one write from a bytevector slice.
The `channel` parameter is an open SSH channel. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value naming the SSH descriptor.
|#
  (define-who ssh-write/nonblocking
    (case-lambda
      [(channel bv) (ssh-write/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                channel
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           1
                                           -1)))]))

  #|proc:ssh-write-all
The `ssh-write-all` procedure writes an entire bytevector slice to an SSH channel.
|#
  (define-who ssh-write-all
    (case-lambda
      [(channel bv) (ssh-write-all channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write-all channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (ssh-write channel bv i stop))))))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let ([deadline-ms (and (fixnum? timeout-ms) (fx>= timeout-ms 0)
                                       (timeout->deadline-ms timeout-ms))])
                 (let loop ([i start])
                   (if (fx= i stop)
                       (fx- stop start)
                       (let ([step-timeout (if deadline-ms
                                               (remaining-timeout-ms deadline-ms)
                                               -1)])
                         (loop (fx+ i (ssh-write channel bv i stop step-timeout))))))))]))

  #|proc:ssh-write-all/nonblocking
The `ssh-write-all/nonblocking` procedure writes as much of a bytevector slice as possible.
The `channel` parameter is an open SSH channel. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value when no bytes were written.
|#
  (define-who ssh-write-all/nonblocking
    (case-lambda
      [(channel bv) (ssh-write-all/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write-all/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (ssh-write/nonblocking channel bv i stop)])
                       (cond
                        [(net-would-block? n)
                         (if (fx> i start) (fx- i start) n)]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:ssh-channel-exit-status
The `ssh-channel-exit-status` procedure returns the remote process exit status for an SSH channel.
|#
  (define-who ssh-channel-exit-status
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (ensure-success who 'ssh
                              (ffi-net-ssh-channel-exit-status (ssh-channel-handle channel))))))

  #|proc:call-with-ssh-session
The `call-with-ssh-session` procedure opens an SSH session, applies a procedure, and closes the
session afterwards.
|#
  (define-who call-with-ssh-session
    (case-lambda
      [(host proc)
       (call-with-ssh-session host 22 #f ssh-default-timeout-ms proc)]
      [(host port proc)
       (call-with-ssh-session host port #f ssh-default-timeout-ms proc)]
      [(host port user-or-timeout proc)
       (pcheck ([procedure? proc])
               (if (fixnum? user-or-timeout)
                   (call-with-ssh-session host port #f user-or-timeout proc)
                   (call-with-ssh-session host port user-or-timeout ssh-default-timeout-ms proc)))]
      [(host port user timeout-ms proc)
       (pcheck ([procedure? proc])
               (let ([session (ssh-open host port user timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (ssh-close session)))))]))

  #|proc:call-with-ssh-channel
The `call-with-ssh-channel` procedure opens an SSH channel, applies a procedure, and closes the
channel afterwards.
|#
  (define-who call-with-ssh-channel
    (case-lambda
      [(session proc)
       (pcheck ([ssh-session? session] [procedure? proc])
               (let ([channel (ssh-open-channel session)])
                 (dynamic-wind
                   void
                   (lambda () (proc channel))
                   (lambda () (ssh-close-channel channel)))))]
      [(session timeout-ms proc)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([channel (ssh-open-channel session timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc channel))
                   (lambda () (ssh-close-channel channel)))))]))

  #|proc:open-ssh-channel-input-port
The `open-ssh-channel-input-port` procedure opens a binary input port over an SSH channel's stdout
stream.
|#
  (define-who open-ssh-channel-input-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-input-port who channel #f))))

  #|proc:open-ssh-channel-output-port
The `open-ssh-channel-output-port` procedure opens a binary output port over an SSH channel's stdin
stream.
|#
  (define-who open-ssh-channel-output-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-output-port channel))))

  #|proc:open-ssh-channel-error-port
The `open-ssh-channel-error-port` procedure opens a binary input port over an SSH channel's stderr
stream.
|#
  (define-who open-ssh-channel-error-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-input-port who channel #t))))
  )
