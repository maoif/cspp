(define sftp-state-environment-variable "CSPP_SFTP_EXAMPLE_STATE")

(define sftp-home-path
  (lambda (state-root)
    (path-join state-root "home")))

(define sftp-private-key-path
  (lambda (state-root)
    (path-join (path-join (sftp-home-path state-root) ".ssh") "id_ed25519")))

(define sftp-public-key-path
  (lambda (state-root)
    (string-append (sftp-private-key-path state-root) ".pub")))

(define sftp-host-key-path
  (lambda (state-root)
    (path-join state-root "ssh_host_ed25519_key")))

(define sftp-authorized-keys-path
  (lambda (state-root)
    (path-join state-root "authorized_keys")))

(define sftp-log-path
  (lambda (state-root)
    (path-join state-root "sshd.log")))

(define sftp-config-path
  (lambda (state-root)
    (path-join state-root "sshd_config")))

(define sftp-target-dir-path
  (lambda (state-root)
    (path-join state-root "target-dir.txt")))

(define sftp-ready-path
  (lambda (state-root)
    (path-join state-root "ready")))

(define required-sftp-state-root
  (lambda (who)
    (let ([state-root (getenv sftp-state-environment-variable)])
      (if (and state-root (not (string=? state-root "")))
          state-root
          (errorf who
                  "set ~a to the server's private state directory"
                  sftp-state-environment-variable)))))

(define make-sftp-state-root
  (lambda ()
    (let ([state-root (required-sftp-state-root 'sftp-file-server)])
      (when (file-exists? state-root)
        (errorf 'sftp-file-server
                "SFTP state path already exists: ~a"
                state-root))
      (mkdirs state-root)
      (file-chmod state-root #o700)
      state-root)))

(define current-login-user
  (lambda ()
    (or (getenv "USER")
        (getenv "LOGNAME")
        (errorf 'current-login-user "missing USER/LOGNAME environment variable"))))

(define write-string-file
  (lambda (path text)
    (write-bytevector-file path (string->utf8 text))))

(define read-string-file
  (lambda (path)
    (utf8->string (read-u8vec path))))

(define sftp-start-sshd!
  (lambda (dir state-root)
    (let* ([user (current-login-user)]
           [home (sftp-home-path state-root)]
           [ssh-dir (path-join home ".ssh")]
           [private-key (sftp-private-key-path state-root)]
           [public-key (sftp-public-key-path state-root)]
           [authorized-keys (sftp-authorized-keys-path state-root)]
           [host-key (sftp-host-key-path state-root)]
           [log-path (sftp-log-path state-root)]
           [config-path (sftp-config-path state-root)]
           [target-path (sftp-target-dir-path state-root)]
           [ready-path (sftp-ready-path state-root)])
      (mkdirs ssh-dir)
      (file-chmod home #o700)
      (file-chmod ssh-dir #o700)
      (write-string-file target-path dir)
      (process-run/check "ssh-keygen" "-q" "-t" "ed25519" "-N" "" "-f"
                         (string-copy private-key)
                         :stdout null :stderr null)
      (write-bytevector-file authorized-keys (read-u8vec public-key))
      (process-run/check "ssh-keygen" "-q" "-t" "ed25519" "-N" "" "-f"
                         (string-copy host-key)
                         :stdout null :stderr null)
      (file-chmod private-key #o600)
      (file-chmod authorized-keys #o600)
      (file-chmod host-key #o600)
      (write-string-file
       config-path
       (format "Port ~a\nListenAddress 127.0.0.1\nHostKey ~a\nAuthorizedKeysFile ~a\nPasswordAuthentication no\nKbdInteractiveAuthentication no\nChallengeResponseAuthentication no\nPubkeyAuthentication yes\nUsePAM no\nPermitRootLogin no\nStrictModes no\nLogLevel ERROR\nSubsystem sftp internal-sftp -d ~a\nAllowUsers ~a\n"
               sftp-file-transfer-port
               host-key
               authorized-keys
               dir
               user))
      (let ([process (process-spawn "/usr/bin/sshd"
                                    (list "-D" "-f" config-path "-E" log-path)
                                    '((stdin . null)
                                      (stdout . null)
                                      (stderr . null)))])
        (guard (c [else
                   (when (process-running? process)
                     (process-terminate process))
                   (process-wait process)
                   (raise c)])
          (wait-for-ready-server file-transfer-host sftp-file-transfer-port)
          (unless (process-running? process)
            (errorf 'sftp-file-server "sshd exited before becoming ready"))
          (write-bytevector-file ready-path #vu8())
          process)))))

(define sftp-stop-sshd!
  (lambda (process state-root)
    (dynamic-wind
      void
      (lambda ()
        (when (process-running? process)
          (process-terminate process))
        (process-wait process))
      (lambda ()
        (when (file-exists? state-root)
          (file-removetree state-root #f))))))

#|proc:sftp-file-server
The `sftp-file-server` procedure starts a localhost `sshd`-backed SFTP service,
waits until the client uploads the end-marker file into `dir`, then stops the
server and removes the marker. `CSPP_SFTP_EXAMPLE_STATE` must name a new
private state directory shared with the client process.
|#
(define sftp-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (delete-file/ignore (done-marker-path dir))
      (let ([state-root (make-sftp-state-root)])
        (let ([process
               (guard (c [else
                          (when (file-exists? state-root)
                            (file-removetree state-root #f))
                          (raise c)])
                 (sftp-start-sshd! dir state-root))])
          (dynamic-wind
            void
            (lambda ()
              (wait-for-file (done-marker-path dir))
              (delete-file/ignore (done-marker-path dir))
              dir)
            (lambda ()
              (sftp-stop-sshd! process state-root))))))))

#|proc:sftp-file-client
The `sftp-file-client` procedure uploads each file in `path*` to the localhost
SFTP example server and then uploads the end-marker file. The client reads the
server state named by `CSPP_SFTP_EXAMPLE_STATE`.
|#
(define sftp-file-client
  (lambda (path*)
    (validate-file-list 'sftp-file-client path*)
    (let* ([state-root (required-sftp-state-root 'sftp-file-client)]
           [_ (wait-for-file (sftp-ready-path state-root))]
           [target-dir
            (string-trim-right (read-string-file (sftp-target-dir-path state-root)) #\newline)]
           [user (current-login-user)]
           [done-path (path-join state-root "done-marker-upload")])
      (dynamic-wind
        (lambda ()
          (write-bytevector-file done-path #vu8()))
        (lambda ()
          (with-env
           "HOME"
           (sftp-home-path state-root)
           (lambda ()
             (let ([session (ssh-open-with-policy file-transfer-host
                                                  sftp-file-transfer-port
                                                  user
                                                  30000
                                                  'accept-new)])
               (dynamic-wind
                 void
                 (lambda ()
                   (ssh-auth-publickey! session user)
                   (let ([sftp (sftp-open session)])
                     (dynamic-wind
                       void
                       (lambda ()
                         (for-each
                          (lambda (path)
                            (sftp-upload sftp
                                         path
                                         (path-join target-dir (path-basename path))))
                          path*)
                         (sftp-upload sftp
                                      done-path
                                      (done-marker-path target-dir))
                         path*)
                       (lambda ()
                         (sftp-close sftp)))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (delete-file/ignore done-path))))))

#|proc:sftp-transfer-once
The `sftp-transfer-once` procedure uploads or downloads one file through an SSH `session`.
`direction` is `upload` or `download`; `source` and `destination` name the corresponding local and
remote paths. The procedure uses bounded chunks and returns the resulting local path.
|#
(define sftp-transfer-once
  (lambda (session direction source destination)
    (pcheck ([ssh-session? session] [symbol? direction] [string? source destination])
      (let ([sftp (sftp-open session)])
        (dynamic-wind
          void
          (lambda ()
            (case direction
              [(upload)
               (sftp-upload sftp source destination default-transfer-policy)
               source]
              [(download)
               (sftp-download sftp source destination default-transfer-policy)
               destination]
              [else (errorf 'sftp-transfer-once "direction must be upload or download")]))
          (lambda () (sftp-close sftp)))))))
