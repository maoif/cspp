(library (cspp system user lookup)
  (export getuser getgroup user-ref group-ref user-ref/default group-ref/default
          user-exists? group-exists? uid->user user->uid gid->group group->gid
          uid->user-name user-name->uid gid->group-name group-name->gid
          get-user-dir get-user-shell get-user-group current-user current-group)
  (import (cspp chez) (cspp utils) (cspp system common) (cspp system private)
          (cspp system user records) (cspp system user credentials))

;;;;===----------------------------------------------------------------------===
;;;; credentials
;;;;===----------------------------------------------------------------------===

  (define $raise-user-not-found
    (lambda (who kind id)
      (raise (make-system-not-found-error who
                                          (format "~a not found: ~a" kind id)
                                          `((kind . ,kind) (id . ,id))))))

  (define $ffi-lookup
    (lambda (who kind id ffi)
      (let ([x (ffi id)])
        (cond [(string? x)
               ;; The current C helper returns "internal error" for passwd/group
               ;; misses where errno is 0.
               (if (string=? x "internal error")
                   ($raise-user-not-found who kind id)
                   (raise-system-error who #f x `((kind . ,kind) (id . ,id))))]
              [(and (vector? x)
                    (fx< 0 (vector-length x))
                    (let ([tag (vector-ref x 0)])
                      (memq tag '(ok errno not-found unsupported))))
               (ffi-result-ref x)]
              [else x]))))

  (define $user-key?
    (lambda (id) (or ($unsigned-32? id) ($os-string? id))))

  (define $getpwnam
    (let ([ffi (foreign-procedure "cspp_getpwnam" (string) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'user id ffi))))

  (define $getpwuid
    (let ([ffi (foreign-procedure "cspp_getpwuid" (unsigned-32) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'user id ffi))))

  (define $getgrnam
    (let ([ffi (foreign-procedure "cspp_getgrnam" (string) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'group id ffi))))

  (define $getgrgid
    (let ([ffi (foreign-procedure "cspp_getgrgid" (unsigned-32) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'group id ffi))))

  (define $getpw-name   (lambda (v) (vector-ref v 0)))
  (define $getpw-passwd (lambda (v) (vector-ref v 1)))
  (define $getpw-uid    (lambda (v) (vector-ref v 2)))
  (define $getpw-gid    (lambda (v) (vector-ref v 3)))
  (define $getpw-gecos  (lambda (v) (vector-ref v 4)))
  (define $getpw-dir    (lambda (v) (vector-ref v 5)))
  (define $getpw-shell  (lambda (v) (vector-ref v 6)))

  (define $getgr-name   (lambda (v) (vector-ref v 0)))
  (define $getgr-passwd (lambda (v) (vector-ref v 1)))
  (define $getgr-gid    (lambda (v) (vector-ref v 2)))
  (define $getgr-mems   (lambda (v) (vector-ref v 3)))

  (define mk-unix-user
    (lambda (v)
      (let ([name   ($getpw-name v)]
            [passwd ($getpw-passwd v)]
            [uid    ($getpw-uid v)]
            [gid    ($getpw-gid v)]
            [gecos  ($getpw-gecos v)]
            [dir    ($getpw-dir v)]
            [shell  ($getpw-shell v)])
        (make-unix-user name passwd uid gid gecos dir shell))))

  (define mk-unix-group
    (lambda (v)
      (let ([name   ($getgr-name v)]
            [passwd ($getgr-passwd v)]
            [gid    ($getgr-gid v)]
            [mems   ($getgr-mems v)])
        (make-unix-group name passwd gid mems))))

  #|proc:user-ref
  The `user-ref` procedure returns a Unix user record for `id`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define-who user-ref
    (lambda (id)
      (pcheck ([$user-key? id])
        (cond [($unsigned-32? id)
             (mk-unix-user ($getpwuid who id))]
            [($os-string? id)
             (mk-unix-user ($getpwnam who id))]
            [else (errorf who "invalid user id: ~a" id)]))))

  #|proc:getuser
  The `getuser` procedure returns a Unix user record for `id`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define getuser user-ref)

  #|proc:group-ref
  The `group-ref` procedure returns a Unix group record for `id`.
  The `id` parameter is either a exact unsigned 32-bit group ID or a group name string.
  |#
  (define-who group-ref
    (lambda (id)
      (pcheck ([$user-key? id])
        (cond [($unsigned-32? id)
             (mk-unix-group ($getgrgid who id))]
            [($os-string? id)
             (mk-unix-group ($getgrnam who id))]
            [else (errorf who "invalid group id: ~a" id)]))))

  #|proc:getgroup
  The `getgroup` procedure returns a Unix group record for `id`.
  The `id` parameter is either a exact unsigned 32-bit group ID or a group name string.
  |#
  (define getgroup group-ref)

  #|proc:user-ref/default
  The `user-ref/default` procedure returns a Unix user record for `id`, or `default` when `id` is
  not found.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  The `default` parameter is the value returned when `id` does not identify an existing user.
  |#
  (define user-ref/default
    (lambda (id default)
      (guard (c [(system-not-found-error? c) default])
        (user-ref id))))

  #|proc:group-ref/default
  The `group-ref/default` procedure returns a Unix group record for `id`, or `default` when `id`
  is not found.
  The `id` parameter is either a exact unsigned 32-bit group ID or a group name string.
  The `default` parameter is the value returned when `id` does not identify an existing group.
  |#
  (define group-ref/default
    (lambda (id default)
      (guard (c [(system-not-found-error? c) default])
        (group-ref id))))

  #|proc:uid->user
  The `uid->user` procedure returns the user name for `id`.
  The `id` parameter is a exact unsigned 32-bit user ID.
  |#
  (define-who uid->user
    (lambda (id)
      (pcheck ([$unsigned-32? id])
                      ($getpw-name ($getpwuid who id)))))

  #|proc:uid->user-name
  The `uid->user-name` procedure returns the user name for `uid`.
  The `uid` parameter is a exact unsigned 32-bit user ID.
  |#
  (define uid->user-name uid->user)

  #|proc:user->uid
  The `user->uid` procedure returns the numeric user ID for `name`.
  The `name` parameter is a user name string.
  |#
  (define-who user->uid
    (lambda (name)
      (pcheck ([$os-string? name])
                     ($getpw-uid ($getpwnam who name)))))

  #|proc:user-name->uid
  The `user-name->uid` procedure returns the numeric user ID for `name`.
  The `name` parameter is a user name string.
  |#
  (define user-name->uid user->uid)

  #|proc:gid->group
  The `gid->group` procedure returns the group name for `id`.
  The `id` parameter is a exact unsigned 32-bit group ID.
  |#
  (define-who gid->group
    (lambda (id)
      (pcheck ([$unsigned-32? id])
                      ($getgr-name ($getgrgid who id)))))

  #|proc:gid->group-name
  The `gid->group-name` procedure returns the group name for `gid`.
  The `gid` parameter is a exact unsigned 32-bit group ID.
  |#
  (define gid->group-name gid->group)

  #|proc:group->gid
  The `group->gid` procedure returns the numeric group ID for `name`.
  The `name` parameter is a group name string.
  |#
  (define-who group->gid
    (lambda (name)
      (pcheck ([$os-string? name])
                     ($getgr-gid ($getgrnam who name)))))

  #|proc:group-name->gid
  The `group-name->gid` procedure returns the numeric group ID for `name`.
  The `name` parameter is a group name string.
  |#
  (define group-name->gid group->gid)

  #|proc:get-user-dir
  The `get-user-dir` procedure returns the home directory for `id`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define-who get-user-dir
    (lambda (id)
      (pcheck ([$user-key? id])
        (cond [($os-string? id)  ($getpw-dir ($getpwnam who id))]
            [($unsigned-32? id) ($getpw-dir ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)]))))

  #|proc:get-user-shell
  The `get-user-shell` procedure returns the login shell for `id`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define-who get-user-shell
    (lambda (id)
      (pcheck ([$user-key? id])
        (cond [($os-string? id)  ($getpw-shell ($getpwnam who id))]
            [($unsigned-32? id) ($getpw-shell ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)]))))

  #|proc:get-user-group
  The `get-user-group` procedure returns the primary numeric group ID for `id`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define-who get-user-group
    (lambda (id)
      (pcheck ([$user-key? id])
        (cond [($os-string? id)  ($getpw-gid ($getpwnam who id))]
            [($unsigned-32? id) ($getpw-gid ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)]))))

  #|proc:user-exists?
  The `user-exists?` procedure returns `#t` when `id` identifies an existing user, otherwise `#f`.
  The `id` parameter is either a exact unsigned 32-bit user ID or a user name string.
  |#
  (define-who user-exists?
    (lambda (id)
      (guard (c [(system-not-found-error? c) #f])
        (if (user-ref id) #t #f))))

  #|proc:group-exists?
  The `group-exists?` procedure returns `#t` when `id` identifies an existing group, otherwise
  `#f`.
  The `id` parameter is either a exact unsigned 32-bit group ID or a group name string.
  |#
  (define-who group-exists?
    (lambda (id)
      (guard (c [(system-not-found-error? c) #f])
        (if (group-ref id) #t #f))))

  #|proc:current-user
  The `current-user` procedure returns the Unix user record for the real user ID of the calling
  process.
  |#
  (define current-user
    (lambda ()
      (user-ref (current-uid))))

  #|proc:current-group
  The `current-group` procedure returns the Unix group record for the real group ID of the calling
  process.
  |#
  (define current-group
    (lambda ()
      (group-ref (current-gid))))

  )
