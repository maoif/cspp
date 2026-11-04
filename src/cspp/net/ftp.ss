(library (cspp net ftp)
  (export ftp-session?
          ftp-mode
          ftp-file?
          ftp-file-direction
          ftp-file-path
          ftp-file-closed?
          ftp-directory-entry?
          ftp-directory-entry-name
          ftp-directory-entry-type
          ftp-directory-entry-size
          ftp-directory-entry-modify
          ftp-directory-entry-unique
          ftp-directory-entry-permissions
          ftp-directory-entry-owner
          ftp-directory-entry-group
          ftp-directory-entry-facts
          ftp-parse-mlsd-line
          ftp-open-file
          ftp-close-file
          ftp-read
          ftp-read!
          ftp-read-all
          ftp-write
          ftp-write-all
          ftp-read/nonblocking
          ftp-read!/nonblocking
          ftp-write/nonblocking
          ftp-write-all/nonblocking
          call-with-ftp-file
          ftp-open
          ftp-close
          ftp-cancel-pending!
          ftp-verify-peer?
          ftp-verify-host?
          ftp-set-tls-verification!
          ftp-login!
          ftp-quit!
          ftp-list
          ftp-list/raw
          ftp-list/nonblocking
          ftp-stat
          ftp-download
          ftp-download/nonblocking
          ftp-upload
          ftp-upload/nonblocking
          ftp-download-directory
          ftp-upload-directory
          ftp-delete!
          ftp-mkdir!
          ftp-rmdir!
          ftp-rename!
          ftp-cwd!
          ftp-pwd
          ftp-passive-mode!
          ftp-active-mode!
          call-with-ftp-session
          open-ftp-input-port
          open-ftp-output-port)
  (import (cspp chez)
          (cspp utils)
          (cspp string)
          (cspp net uri)
          (cspp net errors)
          (cspp net address)
          (cspp net socket)
          (cspp net poll)
          (cspp net operation)
          (cspp net transfer)
          (cspp net ffi)
          (cspp net private)
          (only (cspp file) file-size))

  #|record:ftp-session
The `ftp-session` record owns one FTP or FTPS control connection for its immutable URI and mode.
It tracks credentials, working directory, transfer mode, timeout, TLS verification, pending work,
and the active file. `ftp-close` cancels work, closes the active file, and releases the handle.
|#
  (define-record-type (ftp-session %make-ftp-session ftp-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle ftp-session-handle ftp-session-handle-set!)
            (immutable uri ftp-session-uri)
            (immutable mode ftp-session-mode)
            (mutable username ftp-session-username ftp-session-username-set!)
            (mutable password ftp-session-password ftp-session-password-set!)
            (mutable cwd ftp-session-cwd ftp-session-cwd-set!)
            (mutable passive? ftp-session-passive? ftp-session-passive?-set!)
            (mutable timeout-ms ftp-session-timeout-ms ftp-session-timeout-ms-set!)
            (mutable verify-peer? ftp-session-verify-peer? ftp-session-verify-peer?-set!)
            (mutable verify-host? ftp-session-verify-host? ftp-session-verify-host?-set!)
            (mutable pending ftp-session-pending ftp-session-pending-set!)
            (mutable active-file ftp-session-active-file ftp-session-active-file-set!)
            (mutable closed? ftp-session-closed? ftp-session-closed?-set!)))

  #|record:ftp-file
The `ftp-file` record represents one sequential remote FTP file transfer.
The record stores its owning session, native handle, direction, remote path, transfer policy,
completed byte count, current poll targets and timer deadline, EOF state, and closed state.
|#
  (define-record-type (ftp-file %make-ftp-file ftp-file?)
    (sealed #t)
    (opaque #f)
    (fields (immutable session ftp-file-session)
            (mutable handle ftp-file-handle ftp-file-handle-set!)
            (immutable direction ftp-file-direction)
            (immutable path ftp-file-path)
            (immutable policy ftp-file-policy)
            (mutable completed-bytes ftp-file-completed-bytes ftp-file-completed-bytes-set!)
            (mutable targets ftp-file-targets ftp-file-targets-set!)
            (mutable deadline-ms ftp-file-deadline-ms ftp-file-deadline-ms-set!)
            (mutable terminal? ftp-file-terminal? ftp-file-terminal?-set!)
            (mutable closed? ftp-file-closed? ftp-file-closed?-set!)))

  #|record:ftp-directory-entry
The `ftp-directory-entry` record describes one MLSD or MLST result.
It stores the entry name, type, optional size, modification timestamp, unique identifier,
permissions, owner, group, and complete raw fact alist.
|#
  (define-record-type (ftp-directory-entry %make-ftp-directory-entry ftp-directory-entry?)
    (fields (immutable name ftp-directory-entry-name)
            (immutable type ftp-directory-entry-type)
            (immutable size ftp-directory-entry-size)
            (immutable modify ftp-directory-entry-modify)
            (immutable unique ftp-directory-entry-unique)
            (immutable permissions ftp-directory-entry-permissions)
            (immutable owner ftp-directory-entry-owner)
            (immutable group ftp-directory-entry-group)
            (immutable facts ftp-directory-entry-facts)))

  (define ftp-default-timeout-ms 30000)

  (define ftp-current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (div (time-nanosecond time) 1000000)))))

  (define ensure-session-open
    (lambda (who session)
      (when (ftp-session-closed? session)
        (raise-net-error who 'ftp "FTP session is closed" session))))

  (define ensure-file-open
    (lambda (who file direction)
      (when (ftp-file-closed? file)
        (raise-net-error who 'ftp "FTP file is closed" file))
      (unless (eq? direction (ftp-file-direction file))
        (raise-net-error who 'ftp "FTP file direction does not support this operation" file))
      (ensure-session-open who (ftp-file-session file))))

  (define status-error?
    (lambda (status)
      (and (vector? status)
           (> (vector-length status) 0)
           (eq? 'error (vector-ref status 0)))))

  (define ftp-target-vector->list
    (lambda (target-vector)
      (map (lambda (target)
             (let ([events (vector-ref target 1)])
               (make-poll-target
                (vector-ref target 0)
                (append (if (zero? (fxlogand events (net-pollin))) '() '(read))
                        (if (zero? (fxlogand events (net-pollout))) '() '(write))))))
           (vector->list target-vector))))

  (define ftp-ready-targets->vector
    (lambda (target*)
      (list->vector
       (map (lambda (target)
              (vector (poll-target-fd target)
                      (fold-left
                       (lambda (mask event)
                         (fxlogor mask
                                  (case event
                                    [(read) (net-pollin)] [(write) (net-pollout)]
                                    [(error) (net-pollerr)] [(hup) (net-pollhup)]
                                    [(invalid) (net-pollnval)] [else 0])))
                       0
                       (poll-target-ready-events target))))
            target*))))

  (define ftp-file-drive!
    (lambda (who file ready-target* timer-expired?)
      (let ([status (ffi-net-ftp-file-step
                     (ftp-file-handle file)
                     (ftp-ready-targets->vector ready-target*)
                     (if timer-expired? 1 0))])
        (when (or (status-error? status) (not (vector? status)))
          (raise-net-error who 'ftp
                           (if (ffi-error? status)
                               (ffi-error-message status)
                               "FTP file transfer failed")
                           status))
        (case (vector-ref status 0)
          [(pending)
           (ftp-file-targets-set! file
                                  (ftp-target-vector->list (vector-ref status 1)))
           (ftp-file-deadline-ms-set!
            file
            (and (vector-ref status 2)
                 (+ (ftp-current-monotonic-ms) (vector-ref status 2))))]
          [(completed)
           (ftp-file-targets-set! file '())
           (ftp-file-deadline-ms-set! file #f)
           (ftp-file-terminal?-set! file #t)]
          [else (raise-net-error who 'ftp "invalid FTP file transfer state" status)])
        status)))

  (define check-ftp-slice
    (lambda (who length start stop)
      (unless (and (fixnum? start) (fixnum? stop)
                   (fx<= 0 start) (fx<= start stop) (fx<= stop length))
        (errorf who "invalid bytevector slice [~s, ~s) for length ~s"
                start stop length))))

  (define native-would-block?
    (lambda (value)
      (and (vector? value)
           (> (vector-length value) 0)
           (eq? 'would-block (vector-ref value 0)))))

  (define ftp-file-pump/nonblocking!
    (lambda (who file)
      (let* ([target* (ftp-file-targets file)]
             [ready* (if (null? target*)
                         '()
                         (filter (lambda (target)
                                   (pair? (poll-target-ready-events target)))
                                 (poll/nonblocking target*)))]
             [deadline (ftp-file-deadline-ms file)]
             [timer-expired? (or (and (null? target*) (not deadline))
                                 (and deadline
                                      (>= (ftp-current-monotonic-ms) deadline)))])
        (ftp-file-drive! who file ready* timer-expired?))))

  (define ftp-file-would-block
    (lambda (file fallback-event)
      (let ([target* (ftp-file-targets file)])
        (if (pair? target*)
            (make-net-would-block (poll-target-fd (car target*))
                                  (poll-target-events (car target*)))
            (make-net-would-block -1 (list fallback-event))))))

  (define wait-for-ftp-file!
    (lambda (who file)
      (let ([target* (ftp-file-targets file)]
            [deadline (ftp-file-deadline-ms file)])
        (poll target*
              (if deadline
                  (max 0 (- deadline (ftp-current-monotonic-ms)))
                  -1))
        (ftp-file-drive!
         who file
         (if (null? target*)
             '()
             (filter (lambda (target) (pair? (poll-target-ready-events target)))
                     (poll/nonblocking target*)))
         (and deadline (>= (ftp-current-monotonic-ms) deadline))))))

  (define ensure-no-pending-mismatch
    (lambda (who session kind args)
      (let ([pending (ftp-session-pending session)])
        (when (and pending
                   (eq? 'pending (net-operation-state pending))
                   (not (eq? (net-operation-kind pending) kind)))
          (raise-net-error who 'ftp "another nonblocking FTP operation is pending" pending)))))

  (define ftp-list-bytevector->entries
    (lambda (bv)
      (let loop ([lines (string-split (utf8->string bv) #\newline)] [out '()])
        (if (null? lines)
            (reverse out)
            (let ([line (string-trim-right (car lines) #\return)])
              (loop (cdr lines)
                    (if (string=? line "") out (cons line out))))))))

  (define ftp-fact-ref
    (lambda (facts name)
      (let ([entry (assoc name facts)])
        (and entry (cdr entry)))))

  (define ftp-permission-symbols
    (lambda (text)
      (and text
           (let loop ([mapping '((#\r . read) (#\w . write) (#\a . append)
                                  (#\c . create) (#\d . delete) (#\f . rename)
                                  (#\l . list) (#\m . mkdir) (#\p . purge))]
                       [out '()])
             (if (null? mapping)
                 (reverse out)
                 (loop (cdr mapping)
                       (if (string-contains? text (caar mapping))
                           (cons (cdar mapping) out)
                           out)))))))

  #|proc:ftp-parse-mlsd-line
The `ftp-parse-mlsd-line` procedure parses one MLSD or MLST fact line.
The `line` parameter contains semicolon-delimited facts, one space, and the entry name.
The return value is an `ftp-directory-entry`; malformed or duplicate facts raise an FTP error.
Unknown facts remain available through `ftp-directory-entry-facts`.
|#
  (define-who ftp-parse-mlsd-line
    (lambda (line)
      (pcheck ([string? line])
              (let ([separator (string-search line #\space)])
                (unless separator
                  (raise-net-error who 'ftp "MLSD line is missing the name delimiter" line))
                (let* ([name (substring line (+ separator 1) (string-length line))]
                       [fact-text (substring line 0 separator)]
                       [facts
                        (let loop ([part* (string-split fact-text #\;)] [out '()])
                          (if (null? part*)
                              (reverse out)
                              (let ([part (car part*)])
                                (if (string=? part "")
                                    (loop (cdr part*) out)
                                    (let ([equals (string-search part #\=)])
                                      (unless equals
                                        (raise-net-error who 'ftp
                                                         "MLSD fact is missing '='" part))
                                      (let ([key (string-downcase (substring part 0 equals))]
                                            [value (substring part (+ equals 1)
                                                              (string-length part))])
                                        (when (assoc key out)
                                          (raise-net-error who 'ftp
                                                           "MLSD contains a duplicate fact" key))
                                        (loop (cdr part*)
                                              (cons (cons key value) out))))))))])
                  (when (string=? name "")
                    (raise-net-error who 'ftp "MLSD entry name is empty" line))
                  (let* ([type-text (ftp-fact-ref facts "type")]
                         [size-text (ftp-fact-ref facts "size")]
                         [size (and size-text (string->number size-text))])
                    (when (and size-text (not (and size (natural? size))))
                      (raise-net-error who 'ftp "MLSD size is invalid" size-text))
                    (%make-ftp-directory-entry
                     name
                     (cond [(and type-text (string=? type-text "file")) 'file]
                           [(and type-text (member type-text '("dir" "cdir" "pdir")))
                            'directory]
                           [(and type-text (string-startswith? type-text "os.unix=slink"))
                            'symlink]
                           [else 'unknown])
                     size
                     (ftp-fact-ref facts "modify")
                     (ftp-fact-ref facts "unique")
                     (ftp-permission-symbols (ftp-fact-ref facts "perm"))
                     (or (ftp-fact-ref facts "unix.owner")
                         (ftp-fact-ref facts "unix.ownername"))
                     (or (ftp-fact-ref facts "unix.group")
                         (ftp-fact-ref facts "unix.groupname"))
                     facts)))))))

  (define ftp-mlsd-bytevector->entries
    (lambda (bytevector)
      (map ftp-parse-mlsd-line (ftp-list-bytevector->entries bytevector))))

  (define normalize-ftp-uri
    (lambda (who value)
      (let ([u (cond
                [(uri? value) value]
                [(string? value)
                 (or (string->uri value)
                     (errorf who "invalid URI string ~s" value))]
                [else
                 (errorf who "expected URI object or string, given ~s" value)])])
        (unless (member (uri-scheme u) '("ftp" "ftps"))
          (errorf who "expected ftp or ftps URI, given ~s" (uri-scheme u)))
        (unless (uri-host u)
          (errorf who "FTP URI requires a host: ~s" (uri->string u)))
        u)))

  (define split-userinfo
    (lambda (userinfo)
      (if (and userinfo (not (string=? userinfo "")))
          (let ([i (string-search userinfo #\:)])
            (if i
                (values (substring userinfo 0 i)
                        (substring userinfo (+ i 1) (string-length userinfo)))
                (values userinfo "")))
          (values "" ""))))

  (define path-join
    (lambda (segments)
      (let loop ([rest segments] [out ""])
        (if (null? rest)
            out
            (loop (cdr rest)
                  (if (string=? out "")
                      (car rest)
                      (string-append out "/" (car rest))))))))

  (define normalize-absolute-path
    (lambda (path)
      (let loop ([rest (string-split path #\/)] [stack '()])
        (if (null? rest)
            (let ([joined (path-join (reverse stack))])
              (if (string=? joined "")
                  "/"
                  (string-append "/" joined)))
            (let ([part (car rest)])
              (cond
               [(or (string=? part "") (string=? part "."))
                (loop (cdr rest) stack)]
               [(string=? part "..")
                (loop (cdr rest) (if (null? stack) '() (cdr stack)))]
               [else
                (loop (cdr rest) (cons part stack))]))))))

  (define resolve-session-path
    (lambda (session path)
      (let ([path (if (or (not path) (string=? path "")) "." path)])
        (normalize-absolute-path
         (if (and (> (string-length path) 0)
                  (char=? (string-ref path 0) #\/))
             path
             (let ([cwd (ftp-session-cwd session)])
               (if (string=? cwd "/")
                   (string-append "/" path)
                   (string-append cwd "/" path))))))))

  (define session-origin
    (lambda (session)
      (let* ([u (ftp-session-uri session)]
             [scheme (if (eq? 'implicit (ftp-session-mode session)) "ftps" "ftp")]
             [host (uri-host u)]
             [port (uri-port u)]
             [default-port (if (eq? 'implicit (ftp-session-mode session)) 990 21)])
        (string-append scheme
                       "://"
                       host
                       (if (and port (not (= port default-port)))
                           (format ":~a" port)
                           "")))))

  (define session-path-url
    (lambda (session path)
      (string-append (session-origin session) (resolve-session-path session path))))

  (define session-directory-url
    (lambda (session path)
      (let ([url (session-path-url session path)])
        (if (char=? (string-ref url (- (string-length url) 1)) #\/)
            url
            (string-append url "/")))))

  (define session-base-url
    (lambda (session)
      (let ([cwd (ftp-session-cwd session)])
        (string-append (session-origin session)
                       (if (string=? cwd "/")
                           "/"
                           (string-append cwd "/"))))))

  (define session-use-tls?
    (lambda (session)
      (not (eq? 'plain (ftp-session-mode session)))))

  #|proc:ftp-mode
The `ftp-mode` procedure returns the TLS mode of `session`.
The `session` parameter is an FTP session. The result is `plain`, `explicit`, or `implicit`.
|#
  (define-who ftp-mode
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ftp-session-mode session))))

  (define normalize-ftp-mode
    (lambda (who uri mode)
      (let ([mode (or mode (if (string=? (uri-scheme uri) "ftps") 'implicit 'plain))])
        (unless (memq mode '(plain explicit implicit))
          (errorf who "FTP mode must be `plain`, `explicit`, or `implicit`, given ~s" mode))
        mode)))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'ftp (ffi-error-message x) x))
      x))

  (define ftp-list*
    (lambda (who session path)
      (ensure-success
       who
       (ffi-net-ftp-list (session-directory-url session path)
                         (ftp-session-username session)
                         (ftp-session-password session)
                         (if (ftp-session-passive? session) 1 0)
                         (ftp-session-timeout-ms session)
                         (if (session-use-tls? session) 1 0)
                         (if (ftp-session-verify-peer? session) 1 0)
                         (if (ftp-session-verify-host? session) 1 0)))))

  (define ftp-stat*
    (lambda (who session path)
      (let ([answer
             (ffi-net-ftp-stat (session-base-url session)
                               (ftp-session-username session)
                               (ftp-session-password session)
                               (if (ftp-session-passive? session) 1 0)
                               (ftp-session-timeout-ms session)
                               (if (session-use-tls? session) 1 0)
                               (if (ftp-session-verify-peer? session) 1 0)
                               (if (ftp-session-verify-host? session) 1 0)
                               (resolve-session-path session path))])
        (if answer (ensure-success who answer) #f))))

  (define ftp-command*
    (lambda (who session cmd)
      (ensure-success
       who
       (ffi-net-ftp-command (session-base-url session)
                            (ftp-session-username session)
                            (ftp-session-password session)
                            (if (ftp-session-passive? session) 1 0)
                            (ftp-session-timeout-ms session)
                            (if (session-use-tls? session) 1 0)
                            (if (ftp-session-verify-peer? session) 1 0)
                            (if (ftp-session-verify-host? session) 1 0)
                            cmd))))

  (define cancel-pending!
    (lambda (session pending)
      (net-operation-cancel! pending)
      (ftp-session-pending-set! session #f)
      session))

  (define ftp-transfer/nonblocking
    (lambda (who session kind args thunk)
      (ensure-session-open who session)
      (ensure-no-pending-mismatch who session kind args)
      (let ([pending (ftp-session-pending session)])
        (if (and pending (eq? 'pending (net-operation-state pending)))
            pending
            (let* ([native-kind (case kind [(ftp-list) 0] [(ftp-download) 1] [(ftp-upload) 2])]
                   [url (case kind
                          [(ftp-list) (session-directory-url session (car args))]
                          [(ftp-download) (session-path-url session (car args))]
                          [(ftp-upload) (session-path-url session (cadr args))])]
                   [path (case kind [(ftp-list) ""] [(ftp-download) (cadr args)]
                               [(ftp-upload) (car args)])]
                   [started (ffi-net-ftp-transfer-start
                             native-kind url path
                             (ftp-session-username session) (ftp-session-password session)
                             (if (ftp-session-passive? session) 1 0)
                             (ftp-session-timeout-ms session)
                             (if (session-use-tls? session) 1 0)
                             (if (ftp-session-verify-peer? session) 1 0)
                             (if (ftp-session-verify-host? session) 1 0))]
                   [handle (and (vector? started) (eq? (vector-ref started 0) 'ok)
                                (vector-ref started 1))])
              (when (or (not handle) (ffi-error? started))
                (raise-net-error who 'ftp "failed to start FTP transfer" started))
              (let ([target* '()]
                    [timer-deadline-ms #f]
                    [first-step? #t]
                    [operation #f])
                (set! operation
                     (make-net-operation
                      kind
                      (lambda ()
                        (guard (failure [else (net-operation-failed failure)])
                          (let* ([now-ms (ftp-current-monotonic-ms)]
                                 [ready-target*
                                  (if (null? target*)
                                      '()
                                      (filter (lambda (target)
                                                (pair? (poll-target-ready-events target)))
                                              (poll/nonblocking target*)))]
                                 [ready
                                  (list->vector
                                   (map (lambda (target)
                                          (vector
                                           (poll-target-fd target)
                                           (fold-left
                                            (lambda (mask event)
                                              (fxlogor
                                               mask
                                               (case event
                                                 [(read) (net-pollin)]
                                                 [(write) (net-pollout)]
                                                 [(error) (net-pollerr)]
                                                 [(hup) (net-pollhup)]
                                                 [(invalid) (net-pollnval)]
                                                 [else 0])))
                                            0
                                            (poll-target-ready-events target))))
                                        ready-target*))]
                                 [timer-expired?
                                  (or first-step?
                                      (and timer-deadline-ms
                                           (>= now-ms timer-deadline-ms)))]
                                 [status
                                  (ffi-net-ftp-transfer-step
                                   handle ready (if timer-expired? 1 0))])
                            (set! first-step? #f)
                            (case (and (vector? status) (vector-ref status 0))
                              [(pending)
                               (set! target*
                                     (map (lambda (target)
                                            (let ([events (vector-ref target 1)])
                                              (make-poll-target
                                               (vector-ref target 0)
                                               (append
                                                (if (zero? (fxlogand events (net-pollin)))
                                                    '() '(read))
                                                (if (zero? (fxlogand events (net-pollout)))
                                                    '() '(write))))))
                                          (vector->list (vector-ref status 1))))
                               (set! timer-deadline-ms
                                     (and (vector-ref status 2)
                                          (+ now-ms (vector-ref status 2))))
                               (net-operation-pending
                                target* timer-deadline-ms)]
                              [(completed)
                               (net-operation-completed
                                (if (= native-kind 0)
                                    (vector-ref status 1)
                                    (if (= native-kind 1) (cadr args) (cadr args))))]
                              [else (net-operation-failed
                                     (make-net-error
                                      who
                                      'ftp
                                      (if (ffi-error? status)
                                          (ffi-error-message status)
                                          "FTP transfer failed")
                                      status))]))))
                      (lambda () (ffi-net-ftp-transfer-cancel handle))
                      (lambda ()
                        (ffi-net-ftp-transfer-close handle)
                        (ftp-session-pending-set! session #f))))
                (ftp-session-pending-set! session operation)
                operation))))))

  (define make-ftp-input-port
    (lambda (session remote-path)
      (let ([file (ftp-open-file session remote-path 'read)]
            [closed? #f])
        (define close!
          (lambda ()
            (unless closed?
              (set! closed? #t)
              (ensure-session-open 'open-ftp-input-port session)
              (ftp-close-file file))))
        (make-custom-binary-input-port
         "cspp-ftp-input"
         (lambda (bytevector start count)
           (let ([answer (ftp-read! file bytevector start (fx+ start count))])
             (if (eof-object? answer) 0 answer)))
         (lambda () #f)
         (lambda (position)
           (errorf 'open-ftp-input-port
                   "FTP input ports do not support positioning: ~s" position))
         (lambda () (close!) #t)))))

  (define make-ftp-output-port
    (lambda (session remote-path)
      (let ([file (ftp-open-file session remote-path 'write)]
            [closed? #f])
        (define close!
          (lambda ()
            (unless closed?
              (set! closed? #t)
              (ensure-session-open 'open-ftp-output-port session)
              (ftp-close-file file))))
        (make-custom-binary-output-port
         "cspp-ftp-output"
         (lambda (bytevector start count)
           (ftp-write file bytevector start (fx+ start count)))
         (lambda () #t)
         (lambda (position)
           (errorf 'open-ftp-output-port
                   "FTP output ports do not support positioning: ~s" position))
         (lambda ()
           (close!)
           #t)))))

  (define native-ftp-session-open
    (lambda (who)
      (let ([status (ffi-net-ftp-session-open)])
        (if (and (vector? status) (eq? 'ok (vector-ref status 0)))
            (vector-ref status 1)
            (raise-net-error who 'ftp
                             (if (ffi-error? status) (ffi-error-message status)
                                 "failed to initialize FTP session")
                             status)))))

  #|proc:ftp-open
The `ftp-open` procedure constructs an FTP or FTPS session from an endpoint or host and port.
An endpoint may be followed by a TLS `mode` and timeout. Modes are `plain`, `explicit`, or
`implicit`; FTP defaults to `plain` and FTPS defaults to `implicit`.
|#
  (define-who ftp-open
    (case-lambda
      [(endpoint)
       (ftp-open endpoint #f ftp-default-timeout-ms)]
      [(endpoint mode-or-timeout)
       (if (symbol? mode-or-timeout)
           (ftp-open endpoint mode-or-timeout ftp-default-timeout-ms)
           (ftp-open endpoint #f mode-or-timeout))]
      [(endpoint mode timeout-ms)
       (pcheck ([fixnum? timeout-ms])
         (when (fx< timeout-ms 0)
           (errorf who "timeout must be non-negative, given ~s" timeout-ms))
         (let ([u (normalize-ftp-uri who endpoint)])
           (let-values ([(user pass) (split-userinfo (uri-userinfo u))])
             (let ([mode (normalize-ftp-mode who u mode)])
               (%make-ftp-session (native-ftp-session-open who)
                                  u
                                  mode
                                  user
                                  pass
                                  (normalize-absolute-path
                                   (if (or (not (uri-path u)) (string=? (uri-path u) ""))
                                       "/"
                                       (uri-path u)))
                                  #t
                                  timeout-ms
                                  (not (eq? mode 'plain))
                                  (not (eq? mode 'plain))
                                  #f
                                  #f
                                  #f)))))]
      [(host port)
       (ftp-open host port #f ftp-default-timeout-ms)]
      [(host port secure?)
       (ftp-open host port secure? ftp-default-timeout-ms)]
      [(host port secure? timeout-ms)
       (pcheck ([string? host] [fixnum? port] [boolean? secure?])
               (check-port who port)
               (unless (fixnum? timeout-ms)
                 (errorf who "expected timeout fixnum, given ~s" timeout-ms))
               (when (fx< timeout-ms 0)
                 (errorf who "timeout must be non-negative, given ~s" timeout-ms))
               (ftp-open
                (format "~a://~a:~a/"
                        (if secure? "ftps" "ftp")
                        host
                        port)
                (if secure? 'implicit 'plain)
                timeout-ms))]))

  #|proc:ftp-close
The `ftp-close` procedure marks an FTP session as closed.
|#
  (define-who ftp-close
    (lambda (session)
      (pcheck ([ftp-session? session])
              (let ([pending (ftp-session-pending session)])
                (when pending
                  (cancel-pending! session pending)))
              (unless (ftp-session-closed? session)
                (ensure-success who (ffi-net-ftp-session-close (ftp-session-handle session)))
                (let ([file (ftp-session-active-file session)])
                  (when file
                    (ftp-file-handle-set! file 0)
                    (ftp-file-closed?-set! file #t)
                    (ftp-session-active-file-set! session #f)))
                (ftp-session-handle-set! session 0)
                (ftp-session-closed?-set! session #t))
              session)))

  #|proc:ftp-verify-peer?
The `ftp-verify-peer?` procedure returns whether an FTPS session verifies the server certificate
chain.
|#
  (define-who ftp-verify-peer?
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ftp-session-verify-peer? session))))

  #|proc:ftp-verify-host?
The `ftp-verify-host?` procedure returns whether an FTPS session verifies the server certificate
hostname.
|#
  (define-who ftp-verify-host?
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ftp-session-verify-host? session))))

  #|proc:ftp-set-tls-verification!
The `ftp-set-tls-verification!` procedure sets FTPS certificate-chain and hostname verification
flags on a session.
|#
  (define-who ftp-set-tls-verification!
    (lambda (session verify-peer? verify-host?)
      (pcheck ([ftp-session? session] [boolean? verify-peer? verify-host?])
              (ensure-session-open who session)
              (ftp-session-verify-peer?-set! session verify-peer?)
              (ftp-session-verify-host?-set! session verify-host?)
              session)))

  #|proc:ftp-cancel-pending!
The `ftp-cancel-pending!` procedure cancels the pending native transfer on `session`, if any.
The `session` parameter is an open FTP session.
The return value is `session`; partial downloads are removed during cancellation cleanup.
|#
  (define-who ftp-cancel-pending!
    (lambda (session)
      (pcheck ([ftp-session? session])
              (let ([pending (ftp-session-pending session)])
                (when pending
                  (cancel-pending! session pending)))
              session)))

  #|proc:ftp-quit!
The `ftp-quit!` procedure closes an FTP session.
|#
  (define-who ftp-quit!
    (lambda (session)
      (ftp-close session)))

  #|proc:ftp-login!
The `ftp-login!` procedure updates the username and password stored on an FTP session.
|#
  (define-who ftp-login!
    (lambda (session username password)
      (pcheck ([ftp-session? session] [string? username password])
              (ensure-session-open who session)
              (ftp-session-username-set! session username)
              (ftp-session-password-set! session password)
              session)))

  #|proc:ftp-passive-mode!
The `ftp-passive-mode!` procedure switches an FTP session into passive mode.
|#
  (define-who ftp-passive-mode!
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-passive?-set! session #t)
              #t)))

  #|proc:ftp-active-mode!
The `ftp-active-mode!` procedure switches an FTP session into active mode.
|#
  (define-who ftp-active-mode!
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-passive?-set! session #f)
              #f)))

  #|proc:ftp-cwd!
The `ftp-cwd!` procedure updates the current working directory stored on an FTP session.
|#
  (define-who ftp-cwd!
    (lambda (session path)
      (pcheck ([ftp-session? session] [string? path])
              (ensure-session-open who session)
              (ftp-session-cwd-set! session (resolve-session-path session path))
              session)))

  #|proc:ftp-pwd
The `ftp-pwd` procedure returns the current working directory stored on an FTP session.
|#
  (define-who ftp-pwd
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-cwd session))))

  #|proc:ftp-list
The `ftp-list` procedure returns structured entries from an MLSD remote directory listing.
|#
  (define-who ftp-list
    (case-lambda
      [(session)
       (ftp-list session ".")]
      [(session path)
       (pcheck ([ftp-session? session] [string? path])
               (ensure-session-open who session)
               (ftp-mlsd-bytevector->entries
                (net-operation-wait
                 (ftp-transfer/nonblocking who session 'ftp-list (list path)
                                           (lambda () (ftp-list* who session path))))))]))

  #|proc:ftp-list/raw
The `ftp-list/raw` procedure returns the raw MLSD bytevector for remote directory `path`.
The `session` parameter is an open FTP session. The optional `path` defaults to `.`.
|#
  (define-who ftp-list/raw
    (case-lambda
      [(session) (ftp-list/raw session ".")]
      [(session path)
       (pcheck ([ftp-session? session] [string? path])
               (net-operation-wait (ftp-list/nonblocking session path)))]))

  #|proc:ftp-stat
The `ftp-stat` procedure returns structured metadata for remote `path`.
The `session` parameter is an open FTP session and `path` is a remote path.
The return value is an `ftp-directory-entry`, or `#f` when the path is absent.
|#
  (define-who ftp-stat
    (lambda (session path)
      (pcheck ([ftp-session? session] [string? path])
              (let ([raw (ftp-stat* who session path)])
                (and raw
                     (let loop ([line* (ftp-list-bytevector->entries raw)])
                       (cond [(null? line*)
                              (raise-net-error who 'ftp "MLST response has no fact line" raw)]
                             [(and (string-contains? (car line*) #\;)
                                   (string-contains? (car line*) #\=))
                              (ftp-parse-mlsd-line
                               (if (char=? #\space (string-ref (car line*) 0))
                                   (substring (car line*) 1 (string-length (car line*)))
                                   (car line*)))]
                             [else (loop (cdr line*))])))))))

  #|proc:ftp-list/nonblocking
The `ftp-list/nonblocking` procedure starts an incremental directory listing on `session` for
`path`. It returns a network operation whose completed value is the raw listing bytevector.
|#
  (define-who ftp-list/nonblocking
    (case-lambda
      [(session)
       (ftp-list/nonblocking session ".")]
      [(session path)
       (pcheck ([ftp-session? session] [string? path])
               (ftp-transfer/nonblocking who
                                         session
                                         'ftp-list
                                         (list path)
                                         (lambda () (ftp-list* who session path))))]))

  #|proc:ftp-download
The `ftp-download` procedure downloads `remote-path` to `local-path` through `session`.
The optional `policy` controls resume, overwrite, chunk size, and progress. The return value is
`local-path`, including when overwrite mode `skip` leaves an existing file unchanged.
|#
  (define-who ftp-download
    (case-lambda
      [(session remote-path local-path)
       (ftp-download session remote-path local-path default-transfer-policy)]
      [(session remote-path local-path policy)
       (pcheck ([ftp-session? session] [string? remote-path local-path]
                [transfer-policy? policy])
               (ensure-session-open who session)
               (let* ([exists? (file-exists? local-path)]
                      [overwrite (transfer-policy-overwrite policy)]
                      [resume (transfer-policy-resume policy)])
                 (cond [(and exists? (eq? overwrite 'skip)) local-path]
                       [(and exists? (eq? overwrite 'error) (eq? resume 'never))
                        (errorf who "local destination exists: ~a" local-path)]
                       [else
                        (let* ([offset (cond [(natural? resume) resume]
                                             [(and (eq? resume 'resume) exists?)
                                              (file-size local-path)]
                                             [else 0])]
                               [effective (make-transfer-policy
                                           offset overwrite
                                           (transfer-policy-chunk-size policy)
                                           (transfer-policy-progress policy))]
                               [file (ftp-open-file session remote-path 'read effective)]
                               [op (open-file-output-port local-path
                                                         (if (fx= offset 0)
                                                             (file-options no-fail replace)
                                                             (file-options no-fail no-truncate))
                                                         (buffer-mode block) #f)]
                               [completed? #f])
                          (dynamic-wind
                            (lambda () (file-position op offset))
                            (lambda ()
                              (let loop ()
                                (let ([chunk (ftp-read file
                                                       (transfer-policy-chunk-size policy))])
                                  (unless (eof-object? chunk)
                                    (put-bytevector op chunk)
                                    (loop))))
                              (set! completed? #t)
                              local-path)
                            (lambda ()
                              (close-port op)
                              (ftp-close-file file)
                              (when (and (not completed?) (not (eq? resume 'resume))
                                         (file-exists? local-path))
                                (delete-file local-path #f)))))])))]))

  #|proc:ftp-download/nonblocking
The `ftp-download/nonblocking` procedure incrementally downloads `remote-path` from `session` to
`local-path`. It returns a network operation whose completed value is `local-path`.
|#
  (define-who ftp-download/nonblocking
    (lambda (session remote-path local-path)
      (pcheck ([ftp-session? session] [string? remote-path local-path])
              (ftp-transfer/nonblocking who
                                        session
                                        'ftp-download
                                        (list remote-path local-path)
                                        (lambda ()
                                          (ensure-success
                                           who
                                           (ffi-net-ftp-download
                                            (session-path-url session remote-path)
                                            local-path
                                            (ftp-session-username session)
                                            (ftp-session-password session)
                                            (if (ftp-session-passive? session) 1 0)
                                            (ftp-session-timeout-ms session)
                                            (if (session-use-tls? session) 1 0)
                                            (if (ftp-session-verify-peer? session) 1 0)
                                            (if (ftp-session-verify-host? session) 1 0)))
                                          local-path)))))

  #|proc:ftp-upload
The `ftp-upload` procedure uploads `local-path` to `remote-path` through `session`.
The optional `policy` controls resume, overwrite, chunk size, and progress. The return value is
`remote-path`, including when overwrite mode `skip` leaves an existing remote file unchanged.
|#
  (define-who ftp-upload
    (case-lambda
      [(session local-path remote-path)
       (ftp-upload session local-path remote-path default-transfer-policy)]
      [(session local-path remote-path policy)
       (pcheck ([ftp-session? session] [string? local-path remote-path]
                [transfer-policy? policy])
               (ensure-session-open who session)
               (let* ([entry (ftp-stat session remote-path)]
                      [overwrite (transfer-policy-overwrite policy)]
                      [resume (transfer-policy-resume policy)])
                 (cond [(and entry (eq? overwrite 'skip)) remote-path]
                       [(and entry (eq? overwrite 'error) (eq? resume 'never))
                        (errorf who "remote destination exists: ~a" remote-path)]
                       [else
                        (let* ([offset (cond [(natural? resume) resume]
                                             [(and (eq? resume 'resume) entry)
                                              (or (ftp-directory-entry-size entry) 0)]
                                             [else 0])]
                               [effective (make-transfer-policy
                                           offset overwrite
                                           (transfer-policy-chunk-size policy)
                                           (transfer-policy-progress policy))]
                               [file (ftp-open-file session remote-path 'write effective)]
                               [ip (open-file-input-port local-path)])
                          (dynamic-wind
                            (lambda () (file-position ip offset))
                            (lambda ()
                              (let loop ()
                                (let ([chunk (get-bytevector-n
                                              ip (transfer-policy-chunk-size policy))])
                                  (unless (eof-object? chunk)
                                    (ftp-write-all file chunk)
                                    (loop))))
                              remote-path)
                            (lambda ()
                              (close-port ip)
                              (ftp-close-file file))))])))]))

  #|proc:ftp-upload/nonblocking
The `ftp-upload/nonblocking` procedure incrementally uploads `local-path` through `session` to
`remote-path`. It returns a network operation whose completed value is `remote-path`.
|#
  (define-who ftp-upload/nonblocking
    (lambda (session local-path remote-path)
      (pcheck ([ftp-session? session] [string? local-path remote-path])
              (ftp-transfer/nonblocking who
                                        session
                                        'ftp-upload
                                        (list local-path remote-path)
                                        (lambda ()
                                          (ensure-success
                                           who
                                           (ffi-net-ftp-upload
                                            (session-path-url session remote-path)
                                            local-path
                                            (ftp-session-username session)
                                            (ftp-session-password session)
                                            (if (ftp-session-passive? session) 1 0)
                                            (ftp-session-timeout-ms session)
                                            (if (session-use-tls? session) 1 0)
                                            (if (ftp-session-verify-peer? session) 1 0)
                                            (if (ftp-session-verify-host? session) 1 0)))
                                          remote-path)))))

  #|proc:ftp-open-file
The `ftp-open-file` procedure opens `path` for sequential `read` or `write` access on `session`.
The optional `policy` parameter is a transfer policy and defaults to `default-transfer-policy`.
The return value is a new open FTP file. Only one FTP file may be active on a session at a time.
|#
  (define-who ftp-open-file
    (case-lambda
      [(session path direction)
       (ftp-open-file session path direction default-transfer-policy)]
      [(session path direction policy)
       (pcheck ([ftp-session? session] [string? path] [symbol? direction]
                [transfer-policy? policy])
               (ensure-session-open who session)
               (unless (memq direction '(read write))
                 (errorf who "direction must be `read` or `write`, given ~s" direction))
               (let* ([resume (transfer-policy-resume policy)]
                      [offset (if (natural? resume) resume 0)]
                      [status
                      (ffi-net-ftp-file-open
                       (ftp-session-handle session)
                       (if (eq? direction 'read) 0 1)
                       (session-path-url session path)
                       (ftp-session-username session)
                       (ftp-session-password session)
                       (if (ftp-session-passive? session) 1 0)
                       (ftp-session-timeout-ms session)
                       (if (session-use-tls? session) 1 0)
                       (if (ftp-session-verify-peer? session) 1 0)
                       (if (ftp-session-verify-host? session) 1 0)
                       offset)])
                 (unless (and (vector? status) (eq? 'ok (vector-ref status 0)))
                   (raise-net-error who 'ftp "failed to open FTP file" status))
                 (let ([file (%make-ftp-file session (vector-ref status 1) direction
                                             (resolve-session-path session path) policy offset
                                             '() #f #f #f)])
                   (ftp-session-active-file-set! session file)
                   (ftp-file-drive! who file '() #t)
                   file)))]))

  #|proc:ftp-read/nonblocking
The `ftp-read/nonblocking` procedure reads at most `size` bytes from readable FTP `file`.
The return value is a bytevector, EOF, or a `net-would-block` readiness value.
|#
  (define-who ftp-read/nonblocking
    (lambda (file size)
      (pcheck ([ftp-file? file] [fixnum? size])
              (ensure-file-open who file 'read)
              (when (fx< size 0) (errorf who "size must be nonnegative, given ~s" size))
              (ftp-file-pump/nonblocking! who file)
              (let ([answer (ffi-net-ftp-file-read (ftp-file-handle file) size)])
                (if (native-would-block? answer)
                    (ftp-file-would-block file 'read)
                    (ensure-success who answer))))))

  #|proc:ftp-read
The `ftp-read` procedure reads at most `size` bytes from readable FTP `file`.
The return value is a bytevector or EOF. It waits for network readiness when necessary.
|#
  (define-who ftp-read
    (lambda (file size)
      (pcheck ([ftp-file? file] [fixnum? size])
              (let loop ()
                (let ([answer (ftp-read/nonblocking file size)])
                  (if (net-would-block? answer)
                      (begin (wait-for-ftp-file! who file) (loop))
                      (begin
                        (when (bytevector? answer)
                          (let ([completed (+ (ftp-file-completed-bytes file)
                                              (bytevector-length answer))])
                            (ftp-file-completed-bytes-set! file completed)
                            (transfer-report-progress! (ftp-file-policy file) 'ftp 'download
                                                       (ftp-file-path file) completed #f)))
                        answer)))))))

  #|proc:ftp-read!/nonblocking
The `ftp-read!/nonblocking` procedure reads into `bytevector` between `start` and `stop`.
The return value is a byte count, EOF, or a `net-would-block` readiness value.
|#
  (define-who ftp-read!/nonblocking
    (case-lambda
      [(file bytevector)
       (ftp-read!/nonblocking file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start)
       (ftp-read!/nonblocking file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (pcheck ([ftp-file? file] [bytevector? bytevector]
                [fixnum? start stop])
               (ensure-file-open who file 'read)
               (check-ftp-slice who (bytevector-length bytevector) start stop)
               (let ([chunk (ftp-read/nonblocking file (fx- stop start))])
                 (if (bytevector? chunk)
                     (let ([count (bytevector-length chunk)])
                       (bytevector-copy! chunk 0 bytevector start count)
                       count)
                     chunk)))]))

  #|proc:ftp-read!
The `ftp-read!` procedure reads into `bytevector` between optional `start` and `stop` indices.
The return value is a byte count or EOF. It waits for network readiness when necessary.
|#
  (define-who ftp-read!
    (case-lambda
      [(file bytevector) (ftp-read! file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start) (ftp-read! file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (pcheck ([ftp-file? file] [bytevector? bytevector] [fixnum? start stop])
               (let loop ()
                 (let ([answer (ftp-read!/nonblocking file bytevector start stop)])
                   (if (net-would-block? answer)
                       (begin (wait-for-ftp-file! who file) (loop))
                       answer))))]))

  #|proc:ftp-read-all
The `ftp-read-all` procedure reads all remaining bytes from readable FTP `file`.
The return value is a bytevector containing the remaining remote file content.
|#
  (define-who ftp-read-all
    (lambda (file)
      (pcheck ([ftp-file? file])
              (let-values ([(op get) (open-bytevector-output-port)])
                (let loop ()
                  (let ([chunk (ftp-read file (transfer-policy-chunk-size
                                               (ftp-file-policy file)))])
                    (if (eof-object? chunk)
                        (get)
                        (begin (put-bytevector op chunk) (loop)))))))))

  #|proc:ftp-write/nonblocking
The `ftp-write/nonblocking` procedure queues a bytevector slice for writable FTP `file`.
The return value is the queued byte count or a `net-would-block` readiness value.
|#
  (define-who ftp-write/nonblocking
    (case-lambda
      [(file bytevector)
       (ftp-write/nonblocking file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start)
       (ftp-write/nonblocking file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (pcheck ([ftp-file? file] [bytevector? bytevector] [fixnum? start stop])
               (ensure-file-open who file 'write)
               (check-ftp-slice who (bytevector-length bytevector) start stop)
               (ftp-file-pump/nonblocking! who file)
               (let ([answer (ffi-net-ftp-file-write
                              (ftp-file-handle file) bytevector
                              (bytevector-length bytevector) start (fx- stop start))])
                 (if (native-would-block? answer)
                     (ftp-file-would-block file 'write)
                     (ensure-success who answer))))]))

  #|proc:ftp-write
The `ftp-write` procedure writes a bytevector slice to writable FTP `file`.
The return value is the written byte count. It waits for network readiness when necessary.
|#
  (define-who ftp-write
    (case-lambda
      [(file bytevector) (ftp-write file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start) (ftp-write file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (pcheck ([ftp-file? file] [bytevector? bytevector] [fixnum? start stop])
               (let loop ()
                 (let ([answer (ftp-write/nonblocking file bytevector start stop)])
                   (if (net-would-block? answer)
                       (begin (wait-for-ftp-file! who file) (loop))
                       (begin
                         (let ([completed (+ (ftp-file-completed-bytes file) answer)])
                           (ftp-file-completed-bytes-set! file completed)
                           (transfer-report-progress! (ftp-file-policy file) 'ftp 'upload
                                                      (ftp-file-path file) completed #f))
                         answer)))))]))

  #|proc:ftp-write-all/nonblocking
The `ftp-write-all/nonblocking` procedure writes as much of a bytevector slice as possible.
The return value is a byte count or a `net-would-block` readiness value.
|#
  (define-who ftp-write-all/nonblocking
    (case-lambda
      [(file bytevector)
       (ftp-write-all/nonblocking file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start)
       (ftp-write-all/nonblocking file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (ftp-write/nonblocking file bytevector start stop)]))

  #|proc:ftp-write-all
The `ftp-write-all` procedure writes an entire bytevector slice to writable FTP `file`.
The return value is the number of bytes written.
|#
  (define-who ftp-write-all
    (case-lambda
      [(file bytevector) (ftp-write-all file bytevector 0 (bytevector-length bytevector))]
      [(file bytevector start) (ftp-write-all file bytevector start (bytevector-length bytevector))]
      [(file bytevector start stop)
       (pcheck ([ftp-file? file] [bytevector? bytevector] [fixnum? start stop])
               (let loop ([index start])
                 (if (fx= index stop)
                     (fx- stop start)
                     (let ([count (ftp-write file bytevector index stop)])
                       (loop (fx+ index count))))))]))

  #|proc:ftp-close-file
The `ftp-close-file` procedure finishes or cancels `file` and releases its native resources.
The `file` parameter is an FTP file. Upload close waits for the final server response.
The return value is `file`; repeated close calls are inert.
|#
  (define-who ftp-close-file
    (lambda (file)
      (pcheck ([ftp-file? file])
              (unless (ftp-file-closed? file)
                (dynamic-wind
                  void
                  (lambda ()
                    (if (eq? 'write (ftp-file-direction file))
                        (begin
                          (ensure-success who (ffi-net-ftp-file-finish
                                               (ftp-file-handle file)))
                          (ftp-file-drive! who file '() #t)
                          (let loop ()
                            (unless (ftp-file-terminal? file)
                              (wait-for-ftp-file! who file)
                              (loop))))
                        (unless (ftp-file-terminal? file)
                          (ensure-success who (ffi-net-ftp-file-cancel
                                               (ftp-file-handle file))))))
                  (lambda ()
                    (ffi-net-ftp-file-close (ftp-file-handle file))
                    (ftp-file-handle-set! file 0)
                    (ftp-session-active-file-set! (ftp-file-session file) #f)
                    (ftp-file-closed?-set! file #t))))
              file)))

  #|proc:call-with-ftp-file
The `call-with-ftp-file` procedure opens a remote file, calls `procedure`, and closes the file.
The `procedure` parameter has signature `(ftp-file) -> value`; its value is returned.
The optional `policy` parameter defaults to `default-transfer-policy`.
|#
  (define-who call-with-ftp-file
    (case-lambda
      [(session path direction procedure)
       (call-with-ftp-file session path direction default-transfer-policy procedure)]
      [(session path direction policy procedure)
       (pcheck ([ftp-session? session] [string? path] [symbol? direction]
                [transfer-policy? policy] [procedure? procedure])
               (let ([file (ftp-open-file session path direction policy)])
                 (dynamic-wind
                   void
                   (lambda () (procedure file))
                   (lambda () (ftp-close-file file)))))]))

  (define ftp-child-path
    (lambda (parent name)
      (if (string=? parent "/")
          (string-append "/" name)
          (string-append parent "/" name))))

  (define local-child-path
    (lambda (parent name)
      (if (or (string=? parent "")
              (char=? #\/ (string-ref parent (fx- (string-length parent) 1))))
          (string-append parent name)
          (string-append parent "/" name))))

  (define ensure-local-directory
    (lambda (path)
      (unless (file-exists? path)
        (let ([slash (let loop ([index (fx- (string-length path) 1)])
                       (cond [(fx< index 0) #f]
                             [(char=? #\/ (string-ref path index)) index]
                             [else (loop (fx- index 1))]))])
          (when (and slash (fx> slash 0))
            (ensure-local-directory (substring path 0 slash)))
          (mkdir path)))))

  #|proc:ftp-download-directory
The `ftp-download-directory` procedure recursively downloads remote `remote-root` through
`session` into local directory `local-root`. The optional `policy` applies to every file.
Symbolic links and unknown entry types are rejected. The return value is `local-root`.
|#
  (define-who ftp-download-directory
    (case-lambda
      [(session remote-root local-root)
       (ftp-download-directory session remote-root local-root default-transfer-policy)]
      [(session remote-root local-root policy)
       (pcheck ([ftp-session? session] [string? remote-root local-root]
                [transfer-policy? policy])
               (ensure-session-open who session)
               (ensure-local-directory local-root)
               (let walk ([remote (resolve-session-path session remote-root)]
                          [local local-root])
                 (for-each
                  (lambda (entry)
                    (let ([name (ftp-directory-entry-name entry)])
                      (when (member name '("." ".."))
                        (raise-net-error who 'ftp "unsafe FTP directory entry" name))
                      (let ([remote-child (ftp-child-path remote name)]
                            [local-child (local-child-path local name)])
                        (case (ftp-directory-entry-type entry)
                          [(directory)
                           (ensure-local-directory local-child)
                           (walk remote-child local-child)]
                          [(file) (ftp-download session remote-child local-child policy)]
                          [else
                           (raise-net-error who 'ftp
                                            "recursive FTP download rejects links or unknown types"
                                            remote-child)]))))
                  (ftp-list session remote)))
               local-root)]))

  #|proc:ftp-upload-directory
The `ftp-upload-directory` procedure recursively uploads local directory `local-root` through
`session` into remote directory `remote-root`. The optional `policy` applies to every file.
Local symbolic links are rejected. Remote directories are created before files are uploaded.
The return value is `remote-root`.
|#
  (define-who ftp-upload-directory
    (case-lambda
      [(session local-root remote-root)
       (ftp-upload-directory session local-root remote-root default-transfer-policy)]
      [(session local-root remote-root policy)
       (pcheck ([ftp-session? session] [string? local-root remote-root]
                [transfer-policy? policy])
               (ensure-session-open who session)
               (unless (file-directory? local-root)
                 (errorf who "local source is not a directory: ~a" local-root))
               (unless (ftp-stat session remote-root)
                 (ftp-mkdir! session remote-root))
               (let walk ([local local-root]
                          [remote (resolve-session-path session remote-root)])
                 (for-each
                  (lambda (name)
                    (let ([local-child (local-child-path local name)]
                          [remote-child (ftp-child-path remote name)])
                      (cond [(file-symbolic-link? local-child)
                             (raise-net-error who 'ftp
                                              "recursive FTP upload rejects symbolic links"
                                              local-child)]
                            [(file-directory? local-child)
                             (unless (ftp-stat session remote-child)
                               (ftp-mkdir! session remote-child))
                             (walk local-child remote-child)]
                            [(file-regular? local-child)
                             (ftp-upload session local-child remote-child policy)]
                            [else
                             (raise-net-error who 'ftp "unsupported local file type"
                                              local-child)])) )
                  (directory-list local)))
               remote-root)]))

  #|proc:ftp-delete!
The `ftp-delete!` procedure deletes a remote file.
|#
  (define-who ftp-delete!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "DELE ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-mkdir!
The `ftp-mkdir!` procedure creates a remote directory.
|#
  (define-who ftp-mkdir!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "MKD ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-rmdir!
The `ftp-rmdir!` procedure removes an empty remote directory.
|#
  (define-who ftp-rmdir!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "RMD ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-rename!
The `ftp-rename!` procedure renames or moves a remote file or directory.
|#
  (define-who ftp-rename!
    (lambda (session from-path to-path)
      (pcheck ([ftp-session? session] [string? from-path to-path])
              (ensure-session-open who session)
              (ensure-success
               who
               (ffi-net-ftp-rename (session-base-url session)
                                   (ftp-session-username session)
                                   (ftp-session-password session)
                                   (if (ftp-session-passive? session) 1 0)
                                   (ftp-session-timeout-ms session)
                                   (if (session-use-tls? session) 1 0)
                                   (if (ftp-session-verify-peer? session) 1 0)
                                   (if (ftp-session-verify-host? session) 1 0)
                                   (resolve-session-path session from-path)
                                   (resolve-session-path session to-path)))
              session)))

  #|proc:call-with-ftp-session
The `call-with-ftp-session` procedure opens an FTP session, applies a procedure to it, and closes it
afterward.
|#
  (define-who call-with-ftp-session
    (case-lambda
      [(endpoint proc)
       (call-with-ftp-session endpoint ftp-default-timeout-ms proc)]
      [(endpoint timeout-ms proc)
       (pcheck ([procedure? proc])
         (let ([session (ftp-open endpoint timeout-ms)])
           (dynamic-wind
             void
             (lambda () (proc session))
             (lambda () (ftp-close session)))))]
      [(host port proc)
       (call-with-ftp-session host port #f ftp-default-timeout-ms proc)]
      [(host port secure?-or-timeout proc)
       (pcheck ([procedure? proc])
         (if (fixnum? secure?-or-timeout)
             (call-with-ftp-session host port #f secure?-or-timeout proc)
             (call-with-ftp-session host port secure?-or-timeout ftp-default-timeout-ms proc)))]
      [(host port secure? timeout-ms proc)
       (pcheck ([procedure? proc])
         (let ([session (ftp-open host port secure? timeout-ms)])
           (dynamic-wind
             void
             (lambda () (proc session))
             (lambda () (ftp-close session)))))]))

  #|proc:open-ftp-input-port
The `open-ftp-input-port` procedure opens a binary input port for a remote FTP file.
|#
  (define-who open-ftp-input-port
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (make-ftp-input-port session remote-path))))

  #|proc:open-ftp-output-port
The `open-ftp-output-port` procedure opens a binary output port that uploads its contents to a
remote FTP file when closed.
|#
  (define-who open-ftp-output-port
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (make-ftp-output-port session remote-path))))
  )
