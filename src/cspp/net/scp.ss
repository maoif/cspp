(library (cspp net scp)
  (export scp-session?
          scp-attributes?
          scp-attributes-path
          scp-attributes-type
          scp-attributes-size
          scp-attributes-permissions
          scp-attributes-modification-time
          scp-stat
          scp-open
          scp-close
          scp-cancel-pending!
          scp-download
          scp-upload
          scp-download/nonblocking
          scp-upload/nonblocking
          scp-copy-directory
          scp-copy-directory/nonblocking
          call-with-scp-session)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp file)
          (cspp list)
          (cspp string)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net poll)
          (cspp net operation)
          (cspp net transfer)
          (cspp net address)
          (cspp net socket)
          (cspp net private common)
          (cspp net ssh)
          (cspp net sftp))

  #|record:scp-session
The `scp-session` record owns SCP operations over an SSH session with a fixed timeout.
It records whether it owns that SSH session and tracks one pending operation. `scp-close` cancels
pending work and closes the underlying SSH session only when ownership was requested.
|#
  (define-record-type (scp-session %make-scp-session scp-session?)
    (sealed #t)
    (opaque #f)
    (fields (immutable ssh-session scp-session-ssh-session)
            (immutable timeout-ms scp-session-timeout-ms)
            (immutable owns-ssh? scp-session-owns-ssh?)
            (mutable pending scp-session-pending scp-session-pending-set!)
            (mutable closed? scp-session-closed? scp-session-closed?-set!)
            (mutable child scp-session-child scp-session-child-set!)
            (mutable directory-active? scp-session-directory-active?
                     scp-session-directory-active?-set!)))

  #|record:scp-attributes
The `scp-attributes` record is an immutable snapshot of a remote path. Its fields contain the
path, type, optional byte size, numeric permissions, and Unix modification time.
|#
  (define-record-type (scp-attributes %make-scp-attributes scp-attributes?)
    (sealed #t)
    (opaque #f)
    (fields (immutable path scp-attributes-path)
            (immutable type scp-attributes-type)
            (immutable size scp-attributes-size)
            (immutable permissions scp-attributes-permissions)
            (immutable modification-time scp-attributes-modification-time)))

  (define scp-default-timeout-ms 30000)

  (define scp-current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define ensure-success
    (lambda (who x)
      (cond
       [(ffi-error? x)
        (raise-net-error who 'scp (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (scp-session-closed? session)
        (raise-net-error who 'scp "SCP session is closed" session))
      (%ssh-session-handle (scp-session-ssh-session session))
      session))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define ensure-user-maybe
    (lambda (who user)
      (unless (or (string? user) (eq? user #f))
        (errorf who "expected string or #f, given ~s" user))))

  (define normalize-auth-kind
    (lambda (who auth-kind)
      (cond
       [(symbol? auth-kind) auth-kind]
       [(string? auth-kind)
        (let ([sym (string->symbol auth-kind)])
          (case sym
            [(agent password publickey) sym]
            [else (errorf who "invalid scp auth kind ~s" auth-kind)]))]
       [else
        (errorf who "expected auth kind symbol or string, given ~s" auth-kind)])))

  (define authenticate-ssh!
    (lambda (who session user auth-kind auth-arg)
      (case (normalize-auth-kind who auth-kind)
        [(agent)
         (when (not (eq? auth-arg #f))
           (errorf who "agent authentication does not take an extra argument"))
         (ssh-auth-agent! session user)]
        [(password)
         (unless (string? auth-arg)
           (errorf who "password authentication requires a string password"))
         (ssh-auth-password! session user auth-arg)]
        [(publickey)
         (unless (or (string? auth-arg) (eq? auth-arg #f))
           (errorf who "publickey authentication requires a string passphrase or #f"))
         (ssh-auth-publickey! session user auth-arg)])))

  (define scp-vector->attributes
    (lambda (path vector)
      (%make-scp-attributes
       path
       (case (vector-ref vector 1)
         [(1) 'regular] [(2) 'directory] [(3) 'symlink] [(4) 'special]
         [else 'unknown])
       (vector-ref vector 2)
       (vector-ref vector 3)
       (vector-ref vector 7))))

  #|proc:scp-stat
The `scp-stat` procedure returns a stable `scp-attributes` record for remote `path` through
`session`, or `#f` when the path does not exist.
|#
  (define-who scp-stat
    (lambda (session path)
      (pcheck ([scp-session? session] [string? path])
              (ensure-session-open who session)
              (let ([answer (ensure-success
                             who
                             (ffi-net-scp-stat
                              (%ssh-session-handle (scp-session-ssh-session session)) path))])
                (and answer (scp-vector->attributes path answer))))))

  (define ensure-scp-resume-supported
    (lambda (who policy)
      (unless (eq? 'never (transfer-policy-resume policy))
        (raise-net-error who 'unsupported
                         "SCP resume requires a verified server-side restart helper" policy))))

  (define scp-download/policy
    (lambda (who session remote-path local-path policy)
      (ensure-scp-resume-supported who policy)
      (let ([exists? (file-exists? local-path)]
            [overwrite (transfer-policy-overwrite policy)])
        (cond [(and exists? (eq? overwrite 'skip)) local-path]
              [(and exists? (eq? overwrite 'error))
               (errorf who "local destination exists: ~a" local-path)]
              [else
               (scp-transfer/policy who session 'scp-download remote-path local-path
                                    (scp-session-timeout-ms session) policy)]))))

  (define scp-upload/policy
    (lambda (who session local-path remote-path policy)
      (ensure-scp-resume-supported who policy)
      (let ([attributes (scp-stat session remote-path)]
            [overwrite (transfer-policy-overwrite policy)])
        (cond [(and attributes (eq? overwrite 'skip)) remote-path]
              [(and attributes (eq? overwrite 'error))
               (errorf who "remote destination exists: ~a" remote-path)]
              [else
               (scp-transfer/policy who session 'scp-upload local-path remote-path
                                    (scp-session-timeout-ms session) policy)]))))

  (define ensure-no-pending-mismatch
    (lambda (who session kind args)
      (let ([pending (scp-session-pending session)])
        (when (and pending
                   (eq? 'pending (net-operation-state pending))
                   (not (eq? (net-operation-kind pending) kind)))
          (raise-net-error who 'scp "another nonblocking SCP operation is pending" pending)))))

  (define cancel-pending!
    (lambda (session pending)
      (net-operation-cancel! pending)
      (scp-session-pending-set! session #f)
      session))

  ;; The outer directory operation owns its currently executing child transfer.
  ;; Cancelling it from a progress hook is safe after the native step has returned.
  (define cancel-child!
    (lambda (session)
      (let ([child (scp-session-child session)])
        (when child (net-operation-cancel! child)))))

  (define scp-wait-child
    (lambda (session operation)
      (dynamic-wind
        (lambda () (scp-session-child-set! session operation))
        (lambda () (net-operation-wait operation))
        (lambda ()
          (when (eq? (net-operation-state operation) 'pending)
            (net-operation-cancel! operation))
          (scp-session-child-set! session #f)))))

  ;; A separate wrapper lets directory-copy operations run child transfers while pending.
  ;; The authenticated SSH session is borrowed, never closed by this wrapper.
  (define scp-transfer/policy
    (lambda (who session kind source target timeout-ms policy)
      (let* ([pending (scp-session-pending session)]
             [nested? (and pending (eq? 'pending (net-operation-state pending))
                           (eq? 'scp-copy-directory (net-operation-kind pending)))]
             [transfer-session (if nested?
                                   (scp-open (scp-session-ssh-session session) timeout-ms)
                                   session)])
        (dynamic-wind
          void
          (lambda ()
            (scp-wait-child session
             (scp-transfer/nonblocking who transfer-session kind
                                       (list source target timeout-ms) void policy)))
          (lambda () (when nested? (scp-close transfer-session)))))))

  (define scp-transfer/nonblocking
    (case-lambda
      [(who session kind args thunk)
       (scp-transfer/nonblocking who session kind args thunk #f)]
      [(who session kind args thunk policy)
       (ensure-session-open who session)
       (ensure-no-pending-mismatch who session kind args)
       (let ([pending (scp-session-pending session)])
         (if (and pending (eq? 'pending (net-operation-state pending)))
             pending
             (let* ([native? (memq kind '(scp-download scp-upload))]
                    [deadline-ms
                     (and native?
                          (+ (scp-current-monotonic-ms) (caddr args)))]
                    [handle (and native?
                                 (let ([started
                                        (ffi-net-scp-transfer-start
                                         (%ssh-session-handle (scp-session-ssh-session session))
                                         (if (eq? kind 'scp-download) 0 1)
                                         (if (eq? kind 'scp-download) (car args) (car args))
                                         (if (eq? kind 'scp-download) (cadr args) (cadr args)))])
                                   (if (ffi-error? started)
                                       (raise-net-error who 'scp (ffi-error-message started) started)
                                       (vector-ref started 1))))]
                    [operation
                     (make-net-operation
                      kind
                      (lambda ()
                        (guard (failure [else (net-operation-failed failure)])
                          (if native?
                              (let ([status (ffi-net-scp-transfer-step handle)])
                                (when (and policy (vector? status)
                                           (= 3 (vector-length status))
                                           (memq (vector-ref status 0) '(pending completed)))
                                  (let ([counts (vector-ref status 2)])
                                    (transfer-report-progress!
                                     policy 'scp
                                     (if (eq? kind 'scp-download) 'download 'upload)
                                     (if (eq? kind 'scp-download) (car args) (cadr args))
                                     (car counts) (cdr counts))))
                                (case (and (vector? status) (vector-ref status 0))
                                  [(pending)
                                   (let ([target (vector-ref status 1)])
                                     (net-operation-pending
                                      (list (make-poll-target
                                             (vector-ref target 0)
                                             (vector-ref target 1)))
                                      deadline-ms))]
                                  [(completed) (net-operation-completed
                                                (if (eq? kind 'scp-download) (cadr args) (cadr args)))]
                                  [else (net-operation-failed
                                         (make-net-error who 'scp "SCP transfer failed" status))]))
                              (net-operation-completed (thunk)))))
                      (if native? (lambda () (ffi-net-scp-transfer-cancel handle))
                          (lambda () (cancel-child! session)))
                      (lambda ()
                        (when native? (ffi-net-scp-transfer-close handle))
                        (scp-session-pending-set! session #f)))])
               (scp-session-pending-set! session operation)
               operation)))]))

  ;; Bounded sibling batches use a single native channel and keep per-file progress.
  (define scp-transfer-many/policy
    (lambda (who session direction local-directory remote-directory names timeout-ms policy)
      (let* ([encoded (string->utf8
                       (apply string-append
                              (map (lambda (name) (string-append name (string #\nul))) names)))]
             [started (ensure-success
                       who (ffi-net-scp-transfer-start-many
                            (%ssh-session-handle (scp-session-ssh-session session))
                            (if (eq? direction 'download) 0 1)
                            local-directory remote-directory encoded))]
             [handle (vector-ref started 1)]
             [deadline (+ (scp-current-monotonic-ms) timeout-ms)]
             [operation
              (make-net-operation
               'scp-directory-files
               (lambda ()
                 (guard (failure [else (net-operation-failed failure)])
                   ;; A poll deadline only bounds waiting; the next step must enforce expiry.
                   (when (>= (scp-current-monotonic-ms) deadline)
                     (raise-net-error who 'timeout "SCP batch transfer timed out" remote-directory))
                   (let ([status (ffi-net-scp-transfer-step handle)])
                     (if (and (vector? status) (= 4 (vector-length status)))
                         (let ([tag (vector-ref status 0)] [counts (vector-ref status 2)])
                           (transfer-report-progress! policy 'scp direction (vector-ref status 3)
                                                      (car counts) (cdr counts))
                           (case tag
                             [(completed) (net-operation-completed remote-directory)]
                             [(file-completed)
                              (set! deadline (+ (scp-current-monotonic-ms) timeout-ms))
                              (net-operation-pending
                               (list (make-poll-target (vector-ref status 1) '(write))) deadline)]
                             [(pending)
                              (let ([target (vector-ref status 1)])
                                (net-operation-pending
                                 (list (make-poll-target (vector-ref target 0)
                                                         (vector-ref target 1))) deadline))]
                             [else (net-operation-failed
                                    (make-net-error who 'scp "SCP batch transfer failed" status))]))
                         (net-operation-failed
                          (make-net-error who 'scp "SCP batch transfer failed" status))))))
               (lambda () (ffi-net-scp-transfer-cancel handle))
               (lambda () (ffi-net-scp-transfer-close handle)))])
        (scp-wait-child session operation))))

  (define scp-download*
    (lambda (who session remote-path local-path timeout-ms)
      (ensure-session-open who session)
      (ensure-success who
                      (ffi-net-scp-download-file
                       (%ssh-session-handle (scp-session-ssh-session session))
                       remote-path
                       local-path
                       timeout-ms))
      local-path))

  (define scp-upload*
    (lambda (who session local-path remote-path timeout-ms)
      (ensure-session-open who session)
      (unless (file-regular? local-path #t)
        (errorf who "local file expected, given ~a" local-path))
      (ensure-success who
                      (ffi-net-scp-upload-file
                       (%ssh-session-handle (scp-session-ssh-session session))
                       local-path
                       remote-path
                       timeout-ms))
      remote-path))

  (define local-scp-attributes
    (lambda (path relative-path)
      (%make-scp-attributes relative-path
                            (cond [(file-symbolic-link? path) 'symlink]
                                  [(file-directory? path) 'directory]
                                  [(file-regular? path) 'regular]
                                  [else 'special])
                            (and (file-regular? path) (file-size path))
                            ;; Inspect the link itself so dangling links can be preserved.
                            (file-mode path #f)
                            (time-second (file-modification-time path #f)))))

  (define scp-child-path
    (lambda (parent name)
      (if (or (string=? parent "")
              (char=? (string-ref parent (fx1- (string-length parent))) #\/))
          (string-append parent name)
          (string-append parent "/" name))))

  (define path-inside-root?
    (lambda (root path)
      (let ([root (normalize-scp-path root)] [path (normalize-scp-path path)])
        (or (string=? root path)
            (and (> (string-length path) (string-length root))
                 (string=? root (substring path 0 (string-length root)))
                 (or (char=? (string-ref root (fx1- (string-length root))) #\/)
                     (char=? (string-ref path (string-length root)) #\/)))))))

  (define normalize-scp-path
    (lambda (path)
      (let ([absolute? (and (> (string-length path) 0)
                            (char=? (string-ref path 0) #\/))])
        (let loop ([part* (string-split path #\/)] [stack '()])
          (if (null? part*)
              (let join ([rest (reverse stack)] [out (if absolute? "/" "")])
                (if (null? rest)
                    (if (string=? out "") "." out)
                    (join (cdr rest)
                          (if (or (string=? out "") (string=? out "/"))
                              (string-append out (car rest))
                              (string-append out "/" (car rest))))))
              (let ([part (car part*)])
                (cond [(or (string=? part "") (string=? part "."))
                       (loop (cdr part*) stack)]
                      [(string=? part "..")
                       (loop (cdr part*) (if (null? stack) stack (cdr stack)))]
                      [else (loop (cdr part*) (cons part stack))])))))))

  (define remote-scp-attributes
    (lambda (path attributes)
      (%make-scp-attributes path
                            (sftp-attributes-type attributes)
                            (sftp-attributes-size attributes)
                            (sftp-attributes-permissions attributes)
                            (sftp-attributes-modification-time attributes))))

  (define sftp-stat/maybe
    (lambda (sftp path)
      (guard (condition [else #f]) (sftp-stat sftp path))))

  (define scp-readonly-mode?
    (lambda (mode)
      (and (fixnum? mode) (fxzero? (fxand mode #o222)))))

  (define scp-readonly-remote-file?
    (lambda (attributes)
      ;; Only the unchanged absolute namespace above uses SFTP attributes for stat.
      ;; Raw SCP attributes must never authorize a lexically normalized SFTP delete.
      (and (sftp-attributes? attributes)
           (eq? (sftp-attributes-type attributes) 'regular)
           (scp-readonly-mode? (sftp-attributes-permissions attributes)))))

  (define ensure-local-scp-directory
    (lambda (path)
      (cond [(file-directory? path) path]
            [(file-exists? path) (errorf 'scp-copy-directory "local path is not a directory: ~a" path)]
            [else (mkdirs path) path])))

  (define ensure-remote-scp-directory
    (lambda (sftp path)
      (let ([attributes (sftp-stat/maybe sftp path)])
        (cond [(not attributes) (sftp-mkdir! sftp path)]
              [(eq? (sftp-attributes-type attributes) 'directory) sftp]
              [else (errorf 'scp-copy-directory "remote path is not a directory: ~a" path)]))))

  (define replace-local-symlink!
    (lambda (who target link-path policy)
      (let ([exists? (file-exists? link-path #f)])
        (case (and exists? (transfer-policy-overwrite policy))
          [(skip) #f]
          [(error) (errorf who "local destination exists: ~a" link-path)]
          [else
           ;; Replace the leaf itself; directory-aware helpers can follow a directory link.
           (when exists? (delete-file link-path))
           (file-symlink target link-path)
           #t]))))

  (define replace-remote-symlink!
    (lambda (who sftp target link-path policy)
      ;; Directory entries describe links without following dangling targets.
      (let* ([slash (let find ([index (fx1- (string-length link-path))])
                      (cond [(fx< index 0) #f]
                            [(char=? (string-ref link-path index) #\/) index]
                            [else (find (fx1- index))]))]
             [parent (cond [(not slash) "."] [(fxzero? slash) "/"]
                           [else (substring link-path 0 slash)])]
             [name (substring link-path (if slash (fx1+ slash) 0)
                              (string-length link-path))]
             [exists? (find (lambda (entry) (string=? name (sftp-attributes-name entry)))
                            (sftp-list sftp parent))])
        (case (and exists? (transfer-policy-overwrite policy))
          [(skip) #f]
          [(error) (errorf who "remote destination exists: ~a" link-path)]
          [else
           (when exists? (sftp-delete! sftp link-path))
           (sftp-symlink! sftp target link-path)
           #t]))))

  (define scp-upload-tree-file
    (lambda (who session local-path remote-path timeout-ms policy)
      (let ([exists? (scp-stat session remote-path)])
        (cond [(and exists? (eq? (transfer-policy-overwrite policy) 'skip)) remote-path]
              [(and exists? (eq? (transfer-policy-overwrite policy) 'error))
               (errorf who "remote destination exists: ~a" remote-path)]
              [else
               (if (transfer-policy-progress policy)
                   (scp-transfer/policy who session 'scp-upload local-path remote-path
                                        timeout-ms policy)
                   (scp-upload* who session local-path remote-path timeout-ms))]))))

  (define scp-download-tree-file
    (lambda (who session remote-path local-path timeout-ms policy)
      (let ([exists? (file-exists? local-path)])
        (cond [(and exists? (eq? (transfer-policy-overwrite policy) 'skip)) local-path]
              [(and exists? (eq? (transfer-policy-overwrite policy) 'error))
               (errorf who "local destination exists: ~a" local-path)]
              [else
               (if (transfer-policy-progress policy)
                   (scp-transfer/policy who session 'scp-download remote-path local-path
                                        timeout-ms policy)
                   (scp-download* who session remote-path local-path timeout-ms))]))))

  (define scp-copy-directory*
    (lambda (who session direction source-path target-path timeout-ms policy filter symlink-policy)
      (ensure-session-open who session)
      (ensure-scp-resume-supported who policy)
      (unless (or (eq? filter #f) (procedure? filter))
        (errorf who "filter must be a procedure or #f, given ~s" filter))
      (unless (memq symlink-policy '(preserve follow reject))
        (errorf who "invalid SCP symlink policy ~s" symlink-policy))
      (let ([sftp (sftp-open (scp-session-ssh-session session))]
            [operation (scp-session-pending session)])
        ;; Keep the parent identity: cancellation clears session-pending immediately.
        (define check-active!
          (lambda ()
            (when (and operation
                       (memq (net-operation-state operation) '(cancelled failed)))
              (raise (net-operation-condition operation)))
            (ensure-session-open who session)))
        (define selected-entry?
          (lambda (relative attributes)
            (check-active!)
            (let ([selected? (or (not filter) (filter relative attributes))])
              ;; A filter may cancel/close the session and return either true or false.
              (check-active!)
              selected?)))
        (dynamic-wind
          (lambda () (scp-session-directory-active?-set! session #t))
          (lambda ()
            (case direction
              [(upload)
               (unless (file-directory? source-path #t)
                 (errorf who "local directory expected, given ~a" source-path))
               (ensure-remote-scp-directory sftp target-path)
               (let ([root (normalize-scp-path source-path)])
                 (let walk ([local source-path] [remote target-path] [prefix ""] [seen '()])
                   (let ([normalized (normalize-scp-path local)]
                         [batch (make-list-builder)] [batch-count 0])
                     (define flush-files!
                       (lambda ()
                         (check-active!)
                         (when (fxpositive? batch-count)
                           (scp-transfer-many/policy who session 'upload local remote
                                                     (batch) timeout-ms policy)
                           (set! batch (make-list-builder))
                           (set! batch-count 0))))
                     (when (member normalized seen)
                       (raise-net-error who 'scp "recursive SCP upload found a cycle" local))
                     (for-each
                      (lambda (name)
                        (let* ([local-child (scp-child-path local name)]
                               [remote-child (scp-child-path remote name)]
                               [relative (if (string=? prefix "")
                                             name (scp-child-path prefix name))]
                               [attributes (local-scp-attributes local-child relative)])
                          (when (selected-entry? relative attributes)
                            (case (scp-attributes-type attributes)
                              [(directory)
                               (flush-files!)
                               (ensure-remote-scp-directory sftp remote-child)
                               (walk local-child remote-child relative (cons normalized seen))]
                              [(regular)
                               (let ([exists?
                                      ;; Reuse only names unchanged by SFTP normalization.
                                      ;; Relative paths and dot components need raw server
                                      ;; resolution, including '..' after a symbolic link.
                                      (if (and (fxpositive? (string-length remote-child))
                                               (char=? #\/ (string-ref remote-child 0))
                                               (string=? remote-child
                                                         (sftp-normalize-path sftp remote-child)))
                                          (sftp-stat/maybe sftp remote-child)
                                          (scp-stat session remote-child))])
                                 (case (and exists? (transfer-policy-overwrite policy))
                                   [(skip) (void)]
                                   [(error) (errorf who "remote destination exists: ~a" remote-child)]
                                   [else
                                    (let ([recreate? (scp-readonly-remote-file? exists?)])
                                      ;; Replace read-only files through their writable parent,
                                      ;; without changing permissions on an existing inode.
                                      ;; Transfer it immediately so later targets stay intact.
                                      (when recreate?
                                        (flush-files!)
                                        (sftp-delete! sftp remote-child))
                                      (batch name)
                                      (set! batch-count (fx1+ batch-count))
                                      (when (or recreate? (fx= batch-count 32))
                                        (flush-files!)))]))]
                              [(symlink)
                               (flush-files!)
                               (case symlink-policy
                                 [(reject)
                                  (raise-net-error who 'scp
                                                   "recursive SCP upload rejects symbolic links"
                                                   local-child)]
                                 [(preserve)
                                  (replace-remote-symlink!
                                   who sftp (readlink local-child) remote-child policy)]
                                 [(follow)
                                  (let ([resolved (normalize-scp-path (readlink2 local-child #t))])
                                    (unless (path-inside-root? root resolved)
                                      (raise-net-error who 'scp
                                                       "followed link escapes source root"
                                                       local-child))
                                    (if (file-directory? resolved)
                                        (begin
                                          (ensure-remote-scp-directory sftp remote-child)
                                          (walk resolved remote-child relative
                                                (cons normalized seen)))
                                        (scp-upload-tree-file
                                         who session resolved remote-child timeout-ms policy)))])]
                              [else
                               (raise-net-error who 'scp "unsupported local file type"
                                                local-child)]))))
                      (directory-list local))
                     (flush-files!))))
               target-path]
              [(download)
               (ensure-local-scp-directory target-path)
               (let ([root (sftp-normalize-path sftp source-path)])
                 (let walk ([remote source-path] [local target-path] [prefix ""] [seen '()])
                   (let ([normalized (sftp-normalize-path sftp remote)]
                         [batch (make-list-builder)] [batch-count 0] [command-size 16])
                     (define flush-files!
                       (lambda ()
                         (check-active!)
                         (when (fxpositive? batch-count)
                           (scp-transfer-many/policy who session 'download local remote
                                                     (batch) timeout-ms policy)
                           (set! batch (make-list-builder))
                           (set! batch-count 0)
                           (set! command-size 16))))
                     (when (member normalized seen)
                       (raise-net-error who 'scp "recursive SCP download found a cycle" remote))
                     (for-each
                      (lambda (entry)
                        (let ([name (sftp-attributes-name entry)])
                          (unless (member name '("." ".."))
                            (let* ([remote-child (scp-child-path remote name)]
                                   [local-child (scp-child-path local name)]
                                   [relative (if (string=? prefix "")
                                                 name (scp-child-path prefix name))]
                                   [attributes (remote-scp-attributes relative entry)])
                              (when (selected-entry? relative attributes)
                                (case (scp-attributes-type attributes)
                                  [(directory)
                                   (flush-files!)
                                   (ensure-local-scp-directory local-child)
                                   (walk remote-child local-child relative
                                         (cons normalized seen))]
                                  [(regular)
                                   (case (and (file-exists? local-child)
                                              (transfer-policy-overwrite policy))
                                     [(skip) (void)]
                                     [(error) (errorf who "local destination exists: ~a" local-child)]
                                     [else
                                      (let ([recreate?
                                             (and (file-regular? local-child #f)
                                                  (scp-readonly-mode?
                                                   (file-mode local-child #f)))])
                                        (when recreate?
                                          (flush-files!)
                                          (delete-file local-child))
                                        ;; Bound the shell command as well as its file count.
                                        ;; Four bytes per UTF-8 byte covers quote escaping.
                                        (let ([size (fx+ 8 (fx* 4 (bytevector-length
                                                                  (string->utf8 remote-child))))])
                                          (when (fx> (fx+ command-size size) 65536) (flush-files!))
                                          (set! command-size (fx+ command-size size)))
                                        (batch name)
                                        (set! batch-count (fx1+ batch-count))
                                        (when (or recreate? (fx= batch-count 32))
                                          (flush-files!)))])]
                                  [(symlink)
                                   (flush-files!)
                                   (case symlink-policy
                                     [(reject)
                                      (raise-net-error who 'scp
                                                       "recursive SCP download rejects links"
                                                       remote-child)]
                                     [(preserve)
                                      (replace-local-symlink!
                                       who (sftp-readlink sftp remote-child)
                                       local-child policy)]
                                     [(follow)
                                      (let* ([target (sftp-readlink sftp remote-child)]
                                             [slash (let find ([i (fx1-
                                                                  (string-length remote-child))])
                                                      (cond [(fx< i 0) #f]
                                                            [(char=? (string-ref remote-child i)
                                                                     #\/)
                                                             i]
                                                            [else (find (fx1- i))]))]
                                             [parent (if slash
                                                         (substring remote-child 0 slash) ".")]
                                             [resolved
                                              (sftp-normalize-path
                                               sftp (scp-child-path parent target))]
                                             [resolved-attributes (sftp-stat sftp resolved)])
                                        (unless (path-inside-root? root resolved)
                                          (raise-net-error who 'scp
                                                           "followed link escapes source root"
                                                           remote-child))
                                        (if (eq? (sftp-attributes-type resolved-attributes)
                                                 'directory)
                                            (begin
                                              (ensure-local-scp-directory local-child)
                                              (walk resolved local-child relative
                                                    (cons normalized seen)))
                                            (scp-download-tree-file
                                             who session resolved local-child timeout-ms policy)))])]
                                  [else
                                   (raise-net-error who 'scp "unsupported remote file type"
                                                    remote-child)]))))))
                      (sftp-list sftp remote))
                     (flush-files!))))
               target-path]
              [else
               (errorf who "direction must be one of '(upload download), given ~s" direction)]))
          (lambda ()
            (sftp-close sftp)
            (scp-session-directory-active?-set! session #f)
            (when (scp-session-closed? session) (close-owned-ssh! session)))))))

  #|proc:scp-open
The `scp-open` procedure wraps an authenticated SSH session, or opens and authenticates one, for
subsequent SCP transfers.
|#
  (define-who scp-open
    (case-lambda
      [(session)
       (scp-open session scp-default-timeout-ms)]
      [(session timeout-ms)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (%make-scp-session session timeout-ms #f #f #f #f #f))]
      [(host port user auth-kind auth-arg)
       (scp-open host port user auth-kind auth-arg scp-default-timeout-ms)]
      [(host port user auth-kind auth-arg timeout-ms)
       (pcheck ([string? host] [fixnum? port timeout-ms])
               (check-port who port)
               (check-timeout-ms who timeout-ms)
               (ensure-user-maybe who user)
               (let ([ssh-session (ssh-open host port user timeout-ms)]
                     [ok? #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (authenticate-ssh! who ssh-session user auth-kind auth-arg)
                     (set! ok? #t)
                     (%make-scp-session ssh-session timeout-ms #t #f #f #f #f))
                   (lambda ()
                     (unless ok?
                       (guard (c [else #f])
                         (ssh-close ssh-session)))))))]))

  (define close-owned-ssh!
    (lambda (session)
      (when (and (scp-session-owns-ssh? session)
                 (guard (failure [else #f])
                   (not (fxzero? (%ssh-session-handle (scp-session-ssh-session session))))))
        (ssh-close (scp-session-ssh-session session)))))

  #|proc:scp-close
The `scp-close` procedure closes an SCP session and, if it owns the wrapped SSH session, closes
that SSH session as well. The `session` parameter is an SCP session; it is also returned.
When called from a directory progress callback, owned SSH teardown waits for traversal to unwind.
|#
  (define-who scp-close
    (lambda (session)
      (pcheck ([scp-session? session])
              (unless (scp-session-closed? session)
                (let ([pending (scp-session-pending session)])
                  (when pending
                    (cancel-pending! session pending)))
                ;; Directory traversal still owns an SFTP handle during progress callbacks.
                ;; Its unwind closes that handle before releasing an owned SSH session.
                (unless (scp-session-directory-active? session)
                  (close-owned-ssh! session))
                (scp-session-closed?-set! session #t))
              session)))

  #|proc:scp-cancel-pending!
The `scp-cancel-pending!` procedure cancels pending work, including an active directory child.
The `session` parameter is an open SCP session.
The return value is `session`; partial downloads are removed during cleanup.
|#
  (define-who scp-cancel-pending!
    (lambda (session)
      (pcheck ([scp-session? session])
              (let ([pending (scp-session-pending session)])
                (when pending
                  (cancel-pending! session pending)))
              session)))

  #|proc:scp-download
The `scp-download` procedure downloads `remote-path` through SCP `session` to `local-path`.
The optional fourth argument is a timeout in milliseconds or a transfer policy. A policy controls
resume and overwrite behavior and receives byte progress through its progress hook, with signature
`(protocol direction remote-path completed-bytes total-bytes-or-#f) -> unspecified`.
The total is unknown until the remote file header arrives. Returns `local-path` on success.
|#
  (define-who scp-download
    (case-lambda
      [(session remote-path local-path)
       (scp-download session remote-path local-path (scp-session-timeout-ms session))]
      [(session remote-path local-path timeout-ms)
       (pcheck ([scp-session? session] [string? remote-path local-path])
               (if (transfer-policy? timeout-ms)
                   (scp-download/policy who session remote-path local-path timeout-ms)
                   (begin
                     (check-timeout-ms who timeout-ms)
                     (net-operation-wait
                      (scp-download/nonblocking session remote-path local-path timeout-ms)))))]))

  #|proc:scp-upload
The `scp-upload` procedure uploads `local-path` through SCP `session` to `remote-path`.
The optional fourth argument is a timeout in milliseconds or a transfer policy. A policy controls
resume and overwrite behavior and receives byte progress through its progress hook, with signature
`(protocol direction remote-path completed-bytes total-bytes-or-#f) -> unspecified`.
Returns `remote-path` on success. Skipped files do not invoke the progress hook.
|#
  (define-who scp-upload
    (case-lambda
      [(session local-path remote-path)
       (scp-upload session local-path remote-path (scp-session-timeout-ms session))]
      [(session local-path remote-path timeout-ms)
       (pcheck ([scp-session? session] [string? local-path remote-path])
               (if (transfer-policy? timeout-ms)
                   (scp-upload/policy who session local-path remote-path timeout-ms)
                   (begin
                     (check-timeout-ms who timeout-ms)
                     (net-operation-wait
                      (scp-upload/nonblocking session local-path remote-path timeout-ms)))))]))

  #|proc:scp-download/nonblocking
The `scp-download/nonblocking` procedure constructs a file download operation.
The `session`, `remote-path`, `local-path`, and optional `timeout-ms` describe the transfer.
The return value is a `net-operation` whose successful result is `local-path`.
|#
  (define-who scp-download/nonblocking
    (case-lambda
      [(session remote-path local-path)
       (scp-download/nonblocking session remote-path local-path (scp-session-timeout-ms session))]
      [(session remote-path local-path timeout-ms)
       (pcheck ([scp-session? session] [string? remote-path local-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'scp-download
                (list remote-path local-path timeout-ms)
                (lambda ()
                  (scp-download* who session remote-path local-path timeout-ms))))]))

  #|proc:scp-upload/nonblocking
The `scp-upload/nonblocking` procedure constructs a file upload operation.
The `session`, `local-path`, `remote-path`, and optional `timeout-ms` describe the transfer.
The return value is a `net-operation` whose successful result is `remote-path`.
|#
  (define-who scp-upload/nonblocking
    (case-lambda
      [(session local-path remote-path)
       (scp-upload/nonblocking session local-path remote-path (scp-session-timeout-ms session))]
      [(session local-path remote-path timeout-ms)
       (pcheck ([scp-session? session] [string? local-path remote-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'scp-upload
                (list local-path remote-path timeout-ms)
                (lambda ()
                  (scp-upload* who session local-path remote-path timeout-ms))))]))

  #|proc:scp-copy-directory
The `scp-copy-directory` procedure recursively copies a directory tree in `direction`. `policy`
controls overwrite and resume behavior. `filter` has signature
`(relative-path scp-attributes) -> boolean`, and `symlink-policy` is `preserve`, `follow`, or
`reject`. The default rejects links and includes every entry. The policy progress hook receives
`(protocol direction remote-path completed-bytes total-bytes-or-#f) -> unspecified` for each copied
regular file. Skipped files do not invoke the hook. Consecutive selected regular files in one
directory share a channel in bounded batches of at most 32 files; progress remains per file.
With `replace`, read-only regular destinations can be recreated through a writable parent.
|#
  (define-who scp-copy-directory
    (case-lambda
      [(session direction source-path target-path)
       (scp-copy-directory session
                           direction
                           source-path
                           target-path
                           (scp-session-timeout-ms session))]
      [(session direction source-path target-path timeout-ms)
       (pcheck ([scp-session? session] [string? source-path target-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (net-operation-wait
                (scp-copy-directory/nonblocking
                 session direction source-path target-path timeout-ms)))]
      [(session direction source-path target-path policy filter symlink-policy)
       (scp-copy-directory session direction source-path target-path
                           (scp-session-timeout-ms session) policy filter symlink-policy)]
      [(session direction source-path target-path timeout-ms policy filter symlink-policy)
       (pcheck ([scp-session? session] [string? source-path target-path]
                [fixnum? timeout-ms] [transfer-policy? policy])
               (check-timeout-ms who timeout-ms)
               (net-operation-wait
                (scp-copy-directory/nonblocking session direction source-path target-path
                                                timeout-ms policy filter symlink-policy)))]))

  #|proc:scp-copy-directory/nonblocking
The `scp-copy-directory/nonblocking` procedure constructs a recursive copy operation.
The `direction`, source, target, and optional timeout parameters describe the transfer.
The return value is a `net-operation` whose successful result is the target path.
|#
  (define-who scp-copy-directory/nonblocking
    (case-lambda
      [(session direction source-path target-path)
       (scp-copy-directory/nonblocking session
                                       direction
                                       source-path
                                       target-path
                                       (scp-session-timeout-ms session))]
      [(session direction source-path target-path timeout-ms)
       (pcheck ([scp-session? session] [string? source-path target-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'scp-copy-directory
                (list direction source-path target-path timeout-ms)
                (lambda ()
                  (scp-copy-directory* who
                                       session
                                       direction
                                       source-path
                                       target-path
                                       timeout-ms
                                       default-transfer-policy
                                       #f
                                       'reject))))]
      [(session direction source-path target-path timeout-ms policy filter symlink-policy)
       (pcheck ([scp-session? session] [string? source-path target-path]
                [fixnum? timeout-ms] [transfer-policy? policy])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who session 'scp-copy-directory
                (list direction source-path target-path timeout-ms policy filter symlink-policy)
                (lambda ()
                  (scp-copy-directory* who session direction source-path target-path timeout-ms
                                       policy filter symlink-policy))))]))

  #|proc:call-with-scp-session
The `call-with-scp-session` procedure opens an SCP session, applies a procedure, and closes the
session afterwards.
|#
  (define-who call-with-scp-session
    (case-lambda
      [(session proc)
       (call-with-scp-session session scp-default-timeout-ms proc)]
      [(session timeout-ms proc)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([scp-session (scp-open session timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc scp-session))
                   (lambda () (scp-close scp-session)))))]
      [(host port user auth-kind auth-arg proc)
       (call-with-scp-session host
                              port
                              user
                              auth-kind
                              auth-arg
                              scp-default-timeout-ms
                              proc)]
      [(host port user auth-kind auth-arg timeout-ms proc)
       (pcheck ([string? host] [fixnum? port timeout-ms] [procedure? proc])
               (ensure-user-maybe who user)
               (check-port who port)
               (check-timeout-ms who timeout-ms)
               (let ([scp-session (scp-open host port user auth-kind auth-arg timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc scp-session))
                   (lambda () (scp-close scp-session)))))]))
  )
