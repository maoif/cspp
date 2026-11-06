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

  (define $user-key?
    (lambda (id) (or ($unsigned-32? id) ($os-string? id))))

  (define $getpwnam
    (let ([ffi (foreign-procedure "cspp_getpwnam" (string) scheme-object)])
      (lambda (id) (ffi-result-ref (ffi id)))))

  (define $getpwuid
    (let ([ffi (foreign-procedure "cspp_getpwuid" (unsigned-32) scheme-object)])
      (lambda (id) (ffi-result-ref (ffi id)))))

  (define $getgrnam
    (let ([ffi (foreign-procedure "cspp_getgrnam" (string) scheme-object)])
      (lambda (id) (ffi-result-ref (ffi id)))))

  (define $getgrgid
    (let ([ffi (foreign-procedure "cspp_getgrgid" (unsigned-32) scheme-object)])
      (lambda (id) (ffi-result-ref (ffi id)))))

  ;; Public callers validate the key before dispatching to the native lookup.
  (define $lookup-user
    (lambda (id)
      (if (string? id) ($getpwnam id) ($getpwuid id))))

  (define $lookup-group
    (lambda (id)
      (if (string? id) ($getgrnam id) ($getgrgid id))))

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
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define-who user-ref
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        (mk-unix-user ($lookup-user id)))))

  #|proc:getuser
  The `getuser` procedure returns a Unix user record for `id`.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define getuser user-ref)

  #|proc:group-ref
  The `group-ref` procedure returns a Unix group record for `id`.
  The `id` parameter is either an exact unsigned 32-bit group ID or a group name string without NUL
  characters.
  |#
  (define-who group-ref
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        (mk-unix-group ($lookup-group id)))))

  #|proc:getgroup
  The `getgroup` procedure returns a Unix group record for `id`.
  The `id` parameter is either an exact unsigned 32-bit group ID or a group name string without NUL
  characters.
  |#
  (define getgroup group-ref)

  #|proc:user-ref/default
  The `user-ref/default` procedure returns a Unix user record for `id`, or `default` when `id` is
  not found.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  The `default` parameter is the value returned when `id` does not identify an existing user.
  |#
  (define-who user-ref/default
    (lambda (id default)
      (pcheck-who who ([$user-key? id])
        (guard (c [(system-not-found-error? c) default])
          (user-ref id)))))

  #|proc:group-ref/default
  The `group-ref/default` procedure returns a Unix group record for `id`, or `default` when `id`
  is not found.
  The `id` parameter is either an exact unsigned 32-bit group ID or a group name string without NUL
  characters.
  The `default` parameter is the value returned when `id` does not identify an existing group.
  |#
  (define-who group-ref/default
    (lambda (id default)
      (pcheck-who who ([$user-key? id])
        (guard (c [(system-not-found-error? c) default])
          (group-ref id)))))

  #|proc:uid->user
  The `uid->user` procedure returns the user name for `id`.
  The `id` parameter is an exact unsigned 32-bit user ID.
  |#
  (define-who uid->user
    (lambda (id)
      (pcheck-who who ([$unsigned-32? id])
                      ($getpw-name ($getpwuid id)))))

  #|proc:uid->user-name
  The `uid->user-name` procedure returns the user name for `uid`.
  The `uid` parameter is an exact unsigned 32-bit user ID.
  |#
  (define uid->user-name uid->user)

  #|proc:user->uid
  The `user->uid` procedure returns the numeric user ID for `name`.
  The `name` parameter is a user name string without NUL characters.
  |#
  (define-who user->uid
    (lambda (name)
      (pcheck-who who ([$os-string? name])
                     ($getpw-uid ($getpwnam name)))))

  #|proc:user-name->uid
  The `user-name->uid` procedure returns the numeric user ID for `name`.
  The `name` parameter is a user name string without NUL characters.
  |#
  (define user-name->uid user->uid)

  #|proc:gid->group
  The `gid->group` procedure returns the group name for `id`.
  The `id` parameter is an exact unsigned 32-bit group ID.
  |#
  (define-who gid->group
    (lambda (id)
      (pcheck-who who ([$unsigned-32? id])
                      ($getgr-name ($getgrgid id)))))

  #|proc:gid->group-name
  The `gid->group-name` procedure returns the group name for `gid`.
  The `gid` parameter is an exact unsigned 32-bit group ID.
  |#
  (define gid->group-name gid->group)

  #|proc:group->gid
  The `group->gid` procedure returns the numeric group ID for `name`.
  The `name` parameter is a group name string without NUL characters.
  |#
  (define-who group->gid
    (lambda (name)
      (pcheck-who who ([$os-string? name])
                     ($getgr-gid ($getgrnam name)))))

  #|proc:group-name->gid
  The `group-name->gid` procedure returns the numeric group ID for `name`.
  The `name` parameter is a group name string without NUL characters.
  |#
  (define group-name->gid group->gid)

  #|proc:get-user-dir
  The `get-user-dir` procedure returns the home directory for `id`.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define-who get-user-dir
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        ($getpw-dir ($lookup-user id)))))

  #|proc:get-user-shell
  The `get-user-shell` procedure returns the login shell for `id`.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define-who get-user-shell
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        ($getpw-shell ($lookup-user id)))))

  #|proc:get-user-group
  The `get-user-group` procedure returns the primary numeric group ID for `id`.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define-who get-user-group
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        ($getpw-gid ($lookup-user id)))))

  #|proc:user-exists?
  The `user-exists?` procedure returns `#t` when `id` identifies an existing user, otherwise `#f`.
  The `id` parameter is either an exact unsigned 32-bit user ID or a user name string without NUL
  characters.
  |#
  (define-who user-exists?
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        (guard (c [(system-not-found-error? c) #f])
          (if (user-ref id) #t #f)))))

  #|proc:group-exists?
  The `group-exists?` procedure returns `#t` when `id` identifies an existing group, otherwise
  `#f`.
  The `id` parameter is either an exact unsigned 32-bit group ID or a group name string without NUL
  characters.
  |#
  (define-who group-exists?
    (lambda (id)
      (pcheck-who who ([$user-key? id])
        (guard (c [(system-not-found-error? c) #f])
          (if (group-ref id) #t #f)))))

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
