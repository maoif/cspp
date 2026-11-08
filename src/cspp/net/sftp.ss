(library (cspp net sftp)
  (export sftp-session?
          sftp-file?
          sftp-attributes?
          sftp-attributes-name
          sftp-attributes-type
          sftp-attributes-size
          sftp-attributes-permissions
          sftp-attributes-uid
          sftp-attributes-gid
          sftp-attributes-access-time
          sftp-attributes-modification-time
          sftp-directory?
          sftp-directory-path
          sftp-directory-closed?
          sftp-open
          sftp-close
          sftp-list
          sftp-stat
          sftp-open-directory
          sftp-read-directory
          sftp-read-directory/nonblocking
          sftp-close-directory
          call-with-sftp-directory
          sftp-chmod!
          sftp-chown!
          sftp-utime!
          sftp-symlink!
          sftp-readlink
          sftp-normalize-path
          sftp-cwd!
          sftp-pwd
          sftp-download
          sftp-upload
          sftp-download-directory
          sftp-upload-directory
          sftp-delete!
          sftp-mkdir!
          sftp-rmdir!
          sftp-rename!
          sftp-open-file
          sftp-seek!
          sftp-close-file
          sftp-read
          sftp-read!
          sftp-write
          sftp-write-all
          sftp-read/nonblocking
          sftp-read!/nonblocking
          sftp-write/nonblocking
          sftp-write-all/nonblocking
          call-with-sftp-session
          open-sftp-input-port
          open-sftp-output-port)
  (import (cspp chez)
          (cspp file)
          (cspp string)
          (cspp system)
          (cspp utils)
          (cspp net errors)
          (cspp net ffi)
          (cspp net private)
          (cspp net ssh)
          (cspp net transfer)
          (cspp net poll)
          (cspp net operation))

  #|record:sftp-session
The `sftp-session` record owns an SFTP subsystem attached to an SSH session.
It tracks the remote working directory. `sftp-close` releases its native handle and closes open
operations; later session operations raise an error. The underlying SSH session remains owned by
its caller.
|#
  (define-record-type (sftp-session %make-sftp-session sftp-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle sftp-session-handle sftp-session-handle-set!)
            (immutable ssh-session sftp-session-ssh-session)
            (mutable cwd sftp-session-cwd sftp-session-cwd-set!)
            (mutable closed? sftp-session-closed? sftp-session-closed?-set!)))

  #|record:sftp-file
The `sftp-file` record owns one remote file handle and retains its SFTP session.
`sftp-close-file` releases the handle and marks the file closed; later file operations raise an
error. Closing the owning session also invalidates the file.
|#
  (define-record-type (sftp-file %make-sftp-file sftp-file?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle sftp-file-handle sftp-file-handle-set!)
            (immutable session sftp-file-session)
            (mutable closed? sftp-file-closed? sftp-file-closed?-set!)))

  #|record:sftp-attributes
The `sftp-attributes` record is an immutable snapshot of remote filesystem metadata. Its fields
contain the entry name, type, optional size, permissions, numeric owner and group identifiers,
access time, and modification time.
|#
  (define-record-type (sftp-attributes %make-sftp-attributes sftp-attributes?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name sftp-attributes-name)
            (immutable type sftp-attributes-type)
            (immutable size sftp-attributes-size)
            (immutable permissions sftp-attributes-permissions)
            (immutable uid sftp-attributes-uid)
            (immutable gid sftp-attributes-gid)
            (immutable access-time sftp-attributes-access-time)
            (immutable modification-time sftp-attributes-modification-time)))

  #|record:sftp-directory
The `sftp-directory` record owns a remote directory stream. Its path identifies the normalized
remote directory, and its closed field reports whether the native stream has been released.
|#
  (define-record-type (sftp-directory %make-sftp-directory sftp-directory?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle sftp-directory-handle sftp-directory-handle-set!)
            (immutable session sftp-directory-session)
            (immutable path sftp-directory-path)
            (mutable closed? sftp-directory-closed? sftp-directory-closed?-set!)))

  (define sftp-default-mode #o755)
  (define sftp-open-rdonly (ffi-net-sftp-flag-read))
  (define sftp-open-wronly (ffi-net-sftp-flag-write))
  (define sftp-open-rdwr (ffi-net-sftp-flag-read/write))
  (define sftp-open-append (ffi-net-sftp-flag-append))
  (define sftp-open-creat (ffi-net-sftp-flag-create))
  (define sftp-open-trunc (ffi-net-sftp-flag-truncate))
  (define sftp-open-excl (ffi-net-sftp-flag-exclusive))
  (define sftp-open-text (ffi-net-sftp-flag-text))

  (define ensure-success
    (lambda (who kind x)
      (cond
       [(ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (sftp-session-closed? session)
        (raise-net-error who 'sftp "SFTP session is closed" session))
      (ensure-ssh-session-open who (sftp-session-ssh-session session))))

  (define ensure-ssh-session-open
    (lambda (who session)
      (when (fx= 0 (%ssh-session-handle session))
        (raise-net-error who 'ssh "SSH session is closed" session))))

  (define ensure-file-open
    (lambda (who file)
      (when (sftp-file-closed? file)
        (raise-net-error who 'sftp "SFTP file is closed" file))
      (ensure-session-open who (sftp-file-session file))))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

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

  (define await-timeout-result
    (lambda (who message timeout-ms thunk)
      (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
        (let loop ()
          (let ([remaining-ms (remaining-timeout-ms deadline-ms)])
            (when (fx= remaining-ms 0)
              (raise-net-error who 'sftp message timeout-ms))
            (let ([x (thunk remaining-ms)])
              (if (net-would-block? x)
                  (begin
                    (poll
                     (list
                      (make-poll-target
                       (net-would-block-resource x)
                       (net-would-block-events x)))
                     remaining-ms)
                    (loop))
                  x)))))))

  (define await-ready-result
    (lambda (thunk)
      (let loop ()
        (let ([answer (thunk)])
          (if (net-would-block? answer)
              (begin
                (poll
                 (list
                  (make-poll-target
                   (net-would-block-resource answer)
                   (net-would-block-events answer)))
                 -1)
                (loop))
              answer)))))

  (define file-resource
    (lambda (who file)
      (let* ([session (sftp-file-session file)]
             [ssh-session (sftp-session-ssh-session session)])
        (ensure-success
         who 'ssh
         (ffi-net-ssh-session-fd (%ssh-session-handle ssh-session))))))

  (define read-result
    (lambda (who file answer)
      (cond
       [(or (bytevector? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (file-resource who file)
         (ffi-would-block-events answer))]
       [else (ensure-success who 'sftp answer)])))

  (define read-into-result
    (lambda (who file answer)
      (cond
       [(or (fixnum? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (file-resource who file)
         (ffi-would-block-events answer))]
       [else (ensure-success who 'sftp answer)])))

  (define write-result
    (lambda (who file answer)
      (cond
       [(fixnum? answer) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (file-resource who file)
         (ffi-would-block-events answer))]
       [else (ensure-success who 'sftp answer)])))

  (define open-flags->int
    (lambda (who flags)
      (let ([flag* (cond
                    [(symbol? flags) (list flags)]
                    [(list? flags) flags]
                    [else
                     (errorf who "expected symbol or list of symbols for sftp open flags")])])
        (define access
          (cond
           [(memq 'read/write flag*) sftp-open-rdwr]
           [(and (memq 'read flag*) (memq 'write flag*)) sftp-open-rdwr]
           [(memq 'write flag*) sftp-open-wronly]
           [else sftp-open-rdonly]))
        (let loop ([rest flag*] [out access])
          (if (null? rest)
              out
              (loop
               (cdr rest)
               (case (car rest)
                 [(read write read/write) out]
                 [(append) (fxlogor out sftp-open-append)]
                 [(create) (fxlogor out sftp-open-creat)]
                 [(truncate) (fxlogor out sftp-open-trunc)]
                 [(exclusive) (fxlogor out sftp-open-excl)]
                 [(text) (fxlogor out sftp-open-text)]
                 [else (errorf who "invalid sftp open flag ~s" (car rest))])))))))

  (define stat-vector->attributes
    (lambda (v)
      (%make-sftp-attributes
       (vector-ref v 0)
       (case (vector-ref v 1)
         [(1) 'regular] [(2) 'directory] [(3) 'symlink] [(4) 'special]
         [else 'unknown])
       (vector-ref v 2) (vector-ref v 3) (vector-ref v 4) (vector-ref v 5)
       (vector-ref v 6) (vector-ref v 7))))

  (define normalize-sftp-components
    (lambda (path)
      (let loop ([part* (string-split path #\/)] [out '()])
        (cond [(null? part*) (reverse out)]
              [(or (string=? (car part*) "") (string=? (car part*) "."))
               (loop (cdr part*) out)]
              [(string=? (car part*) "..")
               (loop (cdr part*) (if (null? out) out (cdr out)))]
              [else (loop (cdr part*) (cons (car part*) out))]))))

  (define resolve-sftp-path
    (lambda (session path)
      (let* ([absolute? (and (fx> (string-length path) 0)
                             (char=? #\/ (string-ref path 0)))]
             [combined (if absolute? path
                           (string-append (sftp-session-cwd session) "/" path))]
             [part* (normalize-sftp-components combined)])
        (if (null? part*)
            "/"
            (let loop ([rest (cdr part*)] [out (string-append "/" (car part*))])
              (if (null? rest) out
                  (loop (cdr rest) (string-append out "/" (car rest)))))))))

  (define make-binary-input-port
    (lambda (file)
      (make-custom-binary-input-port
       "cspp-sftp-input"
       (lambda (bv start count)
         (let ([n (sftp-read! file bv start (fx+ start count))])
           (cond
            [(fixnum? n) n]
            [(eof-object? n) 0]
            [else (errorf 'open-sftp-input-port
                          "unexpected nonblocking result from blocking SFTP port read")])))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (file)
      (make-custom-binary-output-port
       "cspp-sftp-output"
       (lambda (bv start count)
         (sftp-write-all file bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define copy-port-chunks
    (lambda (reader writer)
      (let loop ()
        (let ([chunk (reader)])
          (if (eof-object? chunk)
              #t
              (begin
                (writer chunk)
                (loop)))))))

  (define sftp-path-exists
    (lambda (session path)
      (guard (condition [else #f]) (sftp-stat session path))))

  (define ensure-local-directory
    (lambda (path)
      (unless (file-exists? path)
        (let loop ([index (fx- (string-length path) 1)])
          (cond [(fx< index 0) (mkdir path)]
                [(char=? #\/ (string-ref path index))
                 (when (fx> index 0)
                   (ensure-local-directory (substring path 0 index)))
                 (mkdir path)]
                [else (loop (fx1- index))])))))

  (define child-path
    (lambda (parent name)
      (if (or (string=? parent "")
              (char=? #\/ (string-ref parent (fx- (string-length parent) 1))))
          (string-append parent name)
          (string-append parent "/" name))))

  #|proc:sftp-open
The `sftp-open` procedure opens an SFTP session on top of an authenticated SSH session.
|#
  (define-who sftp-open
    (lambda (session)
      (pcheck ([ssh-session? session])
              (ensure-ssh-session-open who session)
              (%make-sftp-session
               (ensure-success who 'sftp
                               (ffi-net-sftp-open (%ssh-session-handle session)))
               session
               "/"
               #f))))

  #|proc:sftp-close
The `sftp-close` procedure closes an SFTP session.
|#
  (define-who sftp-close
    (lambda (session)
      (pcheck ([sftp-session? session])
              (unless (sftp-session-closed? session)
                (when (guard (c [else #f])
                        (not (fx= 0 (%ssh-session-handle (sftp-session-ssh-session session)))))
                  (ensure-success who 'sftp (ffi-net-sftp-close (sftp-session-handle session))))
                (sftp-session-handle-set! session 0)
                (sftp-session-closed?-set! session #t))
              session)))

  #|proc:sftp-list
The `sftp-list` procedure returns stable attribute records for entries in remote `path` through
`session`. The optional `path` defaults to the session working directory.
|#
  (define-who sftp-list
    (case-lambda
      [(session) (sftp-list session ".")]
      [(session path)
       (pcheck ([sftp-session? session] [string? path])
               (call-with-sftp-directory
                session path
                (lambda (directory)
                  (let loop ([out '()])
                    (let ([entry (sftp-read-directory directory)])
                      (if (eof-object? entry) (reverse out)
                          (loop (cons entry out))))))))]))

  #|proc:sftp-stat
The `sftp-stat` procedure returns a stable attribute record for remote `path` through `session`.
|#
  (define-who sftp-stat
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (stat-vector->attributes
               (ensure-success who 'sftp
                               (ffi-net-sftp-stat (sftp-session-handle session)
                                                  (resolve-sftp-path session path)))))))

  #|proc:sftp-normalize-path
The `sftp-normalize-path` procedure resolves `path` against the client-side working directory of
`session`. Absolute paths bypass that directory. The return value is an absolute normalized path.
|#
  (define-who sftp-normalize-path
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (resolve-sftp-path session path))))

  #|proc:sftp-pwd
The `sftp-pwd` procedure returns the client-side working directory of `session`.
|#
  (define-who sftp-pwd
    (lambda (session)
      (pcheck ([sftp-session? session])
              (ensure-session-open who session)
              (sftp-session-cwd session))))

  #|proc:sftp-cwd!
The `sftp-cwd!` procedure validates remote directory `path` and changes only the client-side path
resolution directory of `session`. The return value is `session`.
|#
  (define-who sftp-cwd!
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (let* ([normalized (sftp-normalize-path session path)]
                     [attributes (sftp-stat session normalized)])
                (unless (eq? 'directory (sftp-attributes-type attributes))
                  (errorf who "remote path is not a directory: ~a" path))
                (sftp-session-cwd-set! session normalized)
                session))))

  #|proc:sftp-open-directory
The `sftp-open-directory` procedure opens remote directory `path` through `session`. The return
value is a new `sftp-directory` stream that retains its SFTP session reference.
|#
  (define-who sftp-open-directory
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (let ([normalized (resolve-sftp-path session path)])
                (%make-sftp-directory
                 (ensure-success who 'sftp
                                 (ffi-net-sftp-open-directory
                                  (sftp-session-handle session) normalized))
                 session normalized #f)))))

  (define ensure-directory-open
    (lambda (who directory)
      (when (sftp-directory-closed? directory)
        (raise-net-error who 'sftp "SFTP directory is closed" directory))
      (ensure-session-open who (sftp-directory-session directory))))

  #|proc:sftp-read-directory/nonblocking
The `sftp-read-directory/nonblocking` procedure reads one entry from `directory`. It returns an
`sftp-attributes` record, EOF, or a would-block value naming the SSH session descriptor.
|#
  (define-who sftp-read-directory/nonblocking
    (lambda (directory)
      (pcheck ([sftp-directory? directory])
              (ensure-directory-open who directory)
              (let ([answer (ffi-net-sftp-read-directory (sftp-directory-handle directory))])
                (cond [(vector? answer) (stat-vector->attributes answer)]
                      [(ffi-would-block? answer)
                       (let* ([session (sftp-directory-session directory)]
                              [ssh-session (sftp-session-ssh-session session)])
                         (make-net-would-block
                          (ensure-success who 'ssh
                                          (ffi-net-ssh-session-fd
                                           (%ssh-session-handle ssh-session)))
                          (ffi-would-block-events answer)))]
                      [else (ensure-success who 'sftp answer)])))))

  #|proc:sftp-read-directory
The `sftp-read-directory` procedure reads one entry from `directory`, waiting for readiness when
necessary. It returns an `sftp-attributes` record or EOF.
|#
  (define-who sftp-read-directory
    (lambda (directory)
      (pcheck ([sftp-directory? directory])
              (await-ready-result (lambda () (sftp-read-directory/nonblocking directory))))))

  #|proc:sftp-close-directory
The `sftp-close-directory` procedure idempotently releases `directory`. The return value is the
same directory record.
|#
  (define-who sftp-close-directory
    (lambda (directory)
      (pcheck ([sftp-directory? directory])
              (unless (sftp-directory-closed? directory)
                (when (guard (c [else #f])
                        (begin (ensure-directory-open who directory) #t))
                  (ensure-success who 'sftp
                                  (ffi-net-sftp-close-directory
                                   (sftp-directory-handle directory))))
                (sftp-directory-handle-set! directory 0)
                (sftp-directory-closed?-set! directory #t))
              directory)))

  #|proc:call-with-sftp-directory
The `call-with-sftp-directory` procedure opens `path`, invokes `procedure`, and closes the stream.
The `procedure` parameter has signature `(sftp-directory) -> value`; its value is returned.
|#
  (define-who call-with-sftp-directory
    (lambda (session path procedure)
      (pcheck ([sftp-session? session] [string? path] [procedure? procedure])
              (let ([directory (sftp-open-directory session path)])
                (dynamic-wind void
                  (lambda () (procedure directory))
                  (lambda () (sftp-close-directory directory)))))))

  #|proc:sftp-delete!
The `sftp-delete!` procedure deletes a remote file.
|#
  (define-who sftp-delete!
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-delete (sftp-session-handle session)
                                                   (resolve-sftp-path session path)))
              session)))

  #|proc:sftp-mkdir!
The `sftp-mkdir!` procedure creates a remote directory.
|#
  (define-who sftp-mkdir!
    (case-lambda
      [(session path)
       (sftp-mkdir! session path sftp-default-mode)]
      [(session path mode)
       (pcheck ([sftp-session? session] [string? path] [fixnum? mode])
               (ensure-session-open who session)
               (ensure-success who 'sftp
                               (ffi-net-sftp-mkdir (sftp-session-handle session)
                                                   (resolve-sftp-path session path) mode))
               session)]))

  #|proc:sftp-rmdir!
The `sftp-rmdir!` procedure removes an empty remote directory.
|#
  (define-who sftp-rmdir!
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-rmdir (sftp-session-handle session)
                                                  (resolve-sftp-path session path)))
              session)))

  #|proc:sftp-rename!
The `sftp-rename!` procedure renames a remote path.
|#
  (define-who sftp-rename!
    (lambda (session from-path to-path)
      (pcheck ([sftp-session? session] [string? from-path to-path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-rename (sftp-session-handle session)
                                                   (resolve-sftp-path session from-path)
                                                   (resolve-sftp-path session to-path)))
              session)))

  #|proc:sftp-open-file
The `sftp-open-file` procedure opens a remote file handle using one or more access flags.
|#
  (define-who sftp-open-file
    (case-lambda
      [(session path flags)
       (sftp-open-file session path flags sftp-default-mode)]
      [(session path flags mode)
       (pcheck ([sftp-session? session] [string? path] [fixnum? mode])
               (ensure-session-open who session)
               (%make-sftp-file
                (ensure-success who 'sftp
                                (ffi-net-sftp-open-file (sftp-session-handle session)
                                                        (resolve-sftp-path session path)
                                                        (open-flags->int who flags)
                                                        mode))
                session
                #f))]))

  #|proc:sftp-seek!
The `sftp-seek!` procedure moves the remote `file` position to non-negative byte `offset` and
returns `file`. It is used by resumable transfers and is safe for callers that own the handle.
|#
  (define-who sftp-seek!
    (lambda (file offset)
      (pcheck ([sftp-file? file] [natural? offset])
              (ensure-file-open who file)
              (ensure-success who 'sftp
                              (ffi-net-sftp-seek (sftp-file-handle file) offset))
              file)))

  #|proc:sftp-chmod!
The `sftp-chmod!` procedure sets numeric `permissions` on remote `path` through `session`. It
returns `session`.
|#
  (define-who sftp-chmod!
    (lambda (session path permissions)
      (pcheck ([sftp-session? session] [string? path] [natural? permissions])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-chmod (sftp-session-handle session)
                                                  (resolve-sftp-path session path) permissions))
              session)))

  #|proc:sftp-chown!
The `sftp-chown!` procedure sets numeric `uid` and `gid` ownership on remote `path` through
`session`. It returns `session`.
|#
  (define-who sftp-chown!
    (lambda (session path uid gid)
      (pcheck ([sftp-session? session] [string? path] [natural? uid gid])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-chown (sftp-session-handle session)
                                                  (resolve-sftp-path session path) uid gid))
              session)))

  #|proc:sftp-utime!
The `sftp-utime!` procedure sets nonnegative Unix `access-time` and `modification-time` values on
remote `path` through `session`. It returns `session`.
|#
  (define-who sftp-utime!
    (lambda (session path access-time modification-time)
      (pcheck ([sftp-session? session] [string? path]
               [natural? access-time modification-time])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-utimes
                               (sftp-session-handle session)
                               (resolve-sftp-path session path) access-time modification-time))
              session)))

  #|proc:sftp-symlink!
The `sftp-symlink!` procedure creates remote symbolic link `destination` with literal `target`
through `session`. It returns `session`.
|#
  (define-who sftp-symlink!
    (lambda (session target destination)
      (pcheck ([sftp-session? session] [string? target destination])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-symlink
                               (sftp-session-handle session) target
                               (resolve-sftp-path session destination)))
              session)))

  #|proc:sftp-readlink
The `sftp-readlink` procedure returns the literal target string of remote symbolic link `path`
through `session`.
|#
  (define-who sftp-readlink
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-readlink
                               (sftp-session-handle session)
                               (resolve-sftp-path session path))))))

  #|proc:sftp-close-file
The `sftp-close-file` procedure closes an SFTP file handle.
|#
  (define-who sftp-close-file
    (lambda (file)
      (pcheck ([sftp-file? file])
              (unless (sftp-file-closed? file)
                (when (guard (c [else #f])
                        (begin
                          (ensure-session-open who (sftp-file-session file))
                          #t))
                  (ensure-success who 'sftp (ffi-net-sftp-close-file (sftp-file-handle file))))
                (sftp-file-handle-set! file 0)
                (sftp-file-closed?-set! file #t))
              file)))

  #|proc:sftp-read
The `sftp-read` procedure reads up to `size` bytes from an SFTP file handle.
|#
  (define-who sftp-read
    (case-lambda
      [(file size)
       (pcheck ([sftp-file? file] [fixnum? size])
               (check-size who size)
               (ensure-file-open who file)
               (await-ready-result
                (lambda ()
                  (read-result who file
                               (ffi-net-sftp-read
                                (sftp-file-handle file) size 0 -1)))))]
      [(file size timeout-ms)
       (pcheck ([sftp-file? file] [fixnum? size])
               (check-size who size)
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (await-timeout-result
                who
                "sftp read timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (read-result who file
                               (ffi-net-sftp-read (sftp-file-handle file)
                                                  size
                                                  0
                                                  remaining-ms)))))]))

  #|proc:sftp-read/nonblocking
The `sftp-read/nonblocking` procedure attempts one read from `file` for up to `size` bytes.
The `file` parameter is an open SFTP file. The `size` parameter is the maximum byte count.
The return value is a bytevector, EOF, or a would-block value naming the SSH descriptor.
|#
  (define-who sftp-read/nonblocking
    (lambda (file size)
      (pcheck ([sftp-file? file] [fixnum? size])
              (check-size who size)
              (ensure-file-open who file)
              (read-result who file
                           (ffi-net-sftp-read (sftp-file-handle file) size 1 -1)))))

  #|proc:sftp-read!
The `sftp-read!` procedure reads into a bytevector slice from an SFTP file handle.
|#
  (define-who sftp-read!
    (case-lambda
      [(file bv) (sftp-read! file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-read! file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-ready-result
                (lambda ()
                  (read-into-result
                   who file
                   (ffi-net-sftp-read-into
                    (sftp-file-handle file) bv start stop 0 -1)))))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-timeout-result
                who
                "sftp read timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (read-into-result who file
                                    (ffi-net-sftp-read-into (sftp-file-handle file)
                                                            bv
                                                            start
                                                            stop
                                                            0
                                                            remaining-ms)))))]))

  #|proc:sftp-read!/nonblocking
The `sftp-read!/nonblocking` procedure attempts one read into a bytevector slice.
The `file` parameter is an open SFTP file. The `bv` parameter receives the bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is a byte count, EOF, or a would-block value naming the SSH descriptor.
|#
  (define-who sftp-read!/nonblocking
    (case-lambda
      [(file bv) (sftp-read!/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-read!/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let ([chunk (sftp-read/nonblocking file (fx- stop start))])
                 (cond
                  [(bytevector? chunk)
                   (let ([n (bytevector-length chunk)])
                     (bytevector-copy! chunk 0 bv start n)
                     n)]
                  [else chunk])))]))

  #|proc:sftp-write
The `sftp-write` procedure writes a bytevector slice to an SFTP file handle.
|#
  (define-who sftp-write
    (case-lambda
      [(file bv) (sftp-write file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-ready-result
                (lambda ()
                  (write-result
                   who file
                   (ffi-net-sftp-write
                    (sftp-file-handle file) bv start stop 0 -1)))))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-timeout-result
                who
                "sftp write timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (write-result who file
                                (ffi-net-sftp-write (sftp-file-handle file)
                                                    bv
                                                    start
                                                    stop
                                                    0
                                                    remaining-ms)))))]))

  #|proc:sftp-write/nonblocking
The `sftp-write/nonblocking` procedure attempts one write from a bytevector slice.
The `file` parameter is an open SFTP file. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value naming the SSH descriptor.
|#
  (define-who sftp-write/nonblocking
    (case-lambda
      [(file bv) (sftp-write/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
                (write-result who file
                             (ffi-net-sftp-write (sftp-file-handle file)
                                                 bv
                                                 start
                                                 stop
                                                 1
                                                 -1)))]))

  #|proc:sftp-write-all
The `sftp-write-all` procedure writes an entire bytevector slice to an SFTP file handle.
|#
  (define-who sftp-write-all
    (case-lambda
      [(file bv) (sftp-write-all file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write-all file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (sftp-write file bv i stop))))))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
                 (let loop ([i start])
                   (if (fx= i stop)
                       (fx- stop start)
                       (let ([step-timeout (remaining-timeout-ms deadline-ms)])
                         (loop (fx+ i (sftp-write file bv i stop step-timeout))))))))]))

  #|proc:sftp-write-all/nonblocking
The `sftp-write-all/nonblocking` procedure writes as much of a bytevector slice as possible.
The `file` parameter is an open SFTP file. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value when no bytes were written.
|#
  (define-who sftp-write-all/nonblocking
    (case-lambda
      [(file bv) (sftp-write-all/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write-all/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (sftp-write/nonblocking file bv i stop)])
                       (cond
                        [(net-would-block? n)
                         (if (fx> i start) (fx- i start) n)]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:sftp-download
The `sftp-download` procedure downloads a remote file to a local pathname and returns
`local-path`.
|#
  (define-who sftp-download
    (case-lambda
      [(session remote-path local-path)
       (sftp-download session remote-path local-path default-transfer-policy)]
      [(session remote-path local-path policy)
       (pcheck ([sftp-session? session] [string? remote-path local-path]
                [transfer-policy? policy])
              (ensure-session-open who session)
              (let* ([attributes (sftp-stat session remote-path)]
                     [total (and attributes (sftp-attributes-size attributes))]
                     [existing? (file-exists? local-path)]
                     [overwrite (transfer-policy-overwrite policy)])
                (cond
                 [(and existing? (eq? overwrite 'skip)) local-path]
                 [(and existing? (eq? overwrite 'error))
                  (errorf who "local destination exists: ~a" local-path)]
                 [else
                  (let* ([local-size (if existing? (file-size local-path) 0)]
                         [resume (transfer-policy-resume policy)]
                         [offset (cond [(natural? resume) resume]
                                       [(eq? resume 'resume) local-size]
                                       [else 0])])
                    (when (and total (> offset total))
                      (errorf who "resume offset ~a exceeds remote size ~a" offset total))
                    (let ([file (sftp-open-file session remote-path 'read)])
                      (dynamic-wind void
                        (lambda ()
                          (when (positive? offset) (sftp-seek! file offset))
                          (call-with-port
                           (open-file-output-port
                            local-path
                            (if (positive? offset)
                                (file-options no-fail no-truncate)
                                (file-options no-fail replace))
                            (buffer-mode block) #f)
                           (lambda (op)
                             (when (positive? offset) (file-position op offset))
                             (let loop ([completed offset])
                               (let ([chunk (sftp-read file (transfer-policy-chunk-size policy))])
                                 (unless (eof-object? chunk)
                                   (put-bytevector op chunk)
                                   (let ([next (+ completed (bytevector-length chunk))])
                                     (transfer-report-progress! policy 'sftp 'download remote-path
                                                                  next total)
                                     (loop next))))))))
                        (lambda () (sftp-close-file file)))))]))
              local-path)]))

  #|proc:sftp-upload
The `sftp-upload` procedure uploads a local file to a remote pathname and returns `remote-path`.
|#
  (define-who sftp-upload
    (case-lambda
      [(session local-path remote-path)
       (sftp-upload session local-path remote-path default-transfer-policy)]
      [(session local-path remote-path policy)
       (pcheck ([sftp-session? session] [string? local-path remote-path]
                [transfer-policy? policy])
              (ensure-session-open who session)
              (let* ([existing (sftp-path-exists session remote-path)]
                     [overwrite (transfer-policy-overwrite policy)])
                (cond
                 [(and existing (eq? overwrite 'skip)) remote-path]
                 [(and existing (eq? overwrite 'error))
                  (errorf who "remote destination exists: ~a" remote-path)]
                 [else
                  (let* ([local-size (file-size local-path)]
                         [resume (transfer-policy-resume policy)]
                         [remote-size (and existing (sftp-attributes-size existing))]
                         [offset (cond [(natural? resume) resume]
                                       [(eq? resume 'resume) (or remote-size 0)]
                                       [else 0])])
                    (when (and remote-size (> offset remote-size))
                      (errorf who "resume offset ~a exceeds remote size ~a" offset remote-size))
                    (when (> offset local-size)
                      (errorf who "resume offset ~a exceeds local size ~a" offset local-size))
                    (let ([file (sftp-open-file
                                 session remote-path
                                 (if (zero? offset) '(write create truncate) '(write create)) )])
                      (dynamic-wind void
                        (lambda ()
                          (when (positive? offset) (sftp-seek! file offset))
                          (call-with-port
                           (open-file-input-port local-path (file-options) (buffer-mode block) #f)
                           (lambda (ip)
                             (when (positive? offset) (file-position ip offset))
                             (let loop ([completed offset])
                               (let ([chunk (get-bytevector-n ip (transfer-policy-chunk-size policy))])
                                 (unless (eof-object? chunk)
                                   (sftp-write-all file chunk)
                                   (let ([next (+ completed (bytevector-length chunk))])
                                     (transfer-report-progress! policy 'sftp 'upload remote-path
                                                                  next local-size)
                                     (loop next))))))))
                        (lambda () (sftp-close-file file)))))]))
              remote-path)]))

  #|proc:sftp-download-directory
The `sftp-download-directory` procedure recursively downloads `remote-root` to `local-root`. The
optional `policy` applies per file; optional `preserve?` copies permissions and times. Links and
unknown entry types are rejected. The return value is `local-root`.
|#
  (define-who sftp-download-directory
    (case-lambda
      [(session remote-root local-root)
       (sftp-download-directory session remote-root local-root default-transfer-policy #f)]
      [(session remote-root local-root policy)
       (sftp-download-directory session remote-root local-root policy #f)]
      [(session remote-root local-root policy preserve?)
       (pcheck ([sftp-session? session] [string? remote-root local-root]
                [transfer-policy? policy] [boolean? preserve?])
               (ensure-local-directory local-root)
               (let walk ([remote (resolve-sftp-path session remote-root)] [local local-root])
                 (for-each
                  (lambda (entry)
                    (let ([name (sftp-attributes-name entry)])
                      (unless (member name '("." ".."))
                        (let ([remote-child (child-path remote name)]
                              [local-child (child-path local name)])
                          (case (sftp-attributes-type entry)
                            [(directory)
                             (ensure-local-directory local-child)
                             (walk remote-child local-child)]
                            [(regular)
                             (sftp-download session remote-child local-child policy)
                             (when preserve?
                               (file-chmod local-child
                                           (fxlogand #o7777
                                                     (sftp-attributes-permissions entry))))]
                            [else
                             (raise-net-error who 'sftp
                                              "recursive SFTP download rejects links or unknown types"
                                              remote-child)])))))
                  (sftp-list session remote)))
               local-root)]))

  #|proc:sftp-upload-directory
The `sftp-upload-directory` procedure recursively uploads `local-root` to `remote-root`. The
optional `policy` applies per file; optional `preserve?` copies permissions and modification
times. Local links and unknown file types are rejected. The return value is `remote-root`.
|#
  (define-who sftp-upload-directory
    (case-lambda
      [(session local-root remote-root)
       (sftp-upload-directory session local-root remote-root default-transfer-policy #f)]
      [(session local-root remote-root policy)
       (sftp-upload-directory session local-root remote-root policy #f)]
      [(session local-root remote-root policy preserve?)
       (pcheck ([sftp-session? session] [string? local-root remote-root]
                [transfer-policy? policy] [boolean? preserve?])
               (unless (file-directory? local-root)
                 (errorf who "local source is not a directory: ~a" local-root))
               (unless (sftp-path-exists session remote-root)
                 (sftp-mkdir! session remote-root))
               (let walk ([local local-root] [remote (resolve-sftp-path session remote-root)])
                 (for-each
                  (lambda (name)
                    (let ([local-child (child-path local name)]
                          [remote-child (child-path remote name)])
                      (cond [(file-symbolic-link? local-child)
                             (raise-net-error who 'sftp
                                              "recursive SFTP upload rejects symbolic links"
                                              local-child)]
                            [(file-directory? local-child)
                             (unless (sftp-path-exists session remote-child)
                               (sftp-mkdir! session remote-child))
                             (walk local-child remote-child)]
                            [(file-regular? local-child)
                             (sftp-upload session local-child remote-child policy)
                             (when preserve?
                               (sftp-chmod! session remote-child
                                            (fxlogand #o7777 (file-mode local-child)))
                               (sftp-utime! session remote-child
                                            (time-second (file-access-time local-child))
                                            (time-second
                                             (file-modification-time local-child))))]
                            [else
                             (raise-net-error who 'sftp "unsupported local file type"
                                              local-child)])))
                  (directory-list local)))
               remote-root)]))

  #|proc:call-with-sftp-session
The `call-with-sftp-session` procedure opens an SFTP session, applies a procedure, and closes it
afterwards.
|#
  (define-who call-with-sftp-session
    (lambda (ssh-session proc)
      (pcheck ([ssh-session? ssh-session] [procedure? proc])
              (let ([session (sftp-open ssh-session)])
                (dynamic-wind
                  void
                  (lambda () (proc session))
                  (lambda () (sftp-close session)))))))

  #|proc:open-sftp-input-port
The `open-sftp-input-port` procedure opens a binary input port over an SFTP file handle.
|#
  (define-who open-sftp-input-port
    (lambda (file)
      (pcheck ([sftp-file? file])
              (ensure-file-open who file)
              (make-binary-input-port file))))

  #|proc:open-sftp-output-port
The `open-sftp-output-port` procedure opens a binary output port over an SFTP file handle.
|#
  (define-who open-sftp-output-port
    (lambda (file)
      (pcheck ([sftp-file? file])
              (ensure-file-open who file)
              (make-binary-output-port file))))
  )
