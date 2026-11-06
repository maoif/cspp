(library (cspp system user records)
  (export
          ;; account records
          make-unix-user unix-user? unix-user-name unix-user-passwd unix-user-uid unix-user-gid
          unix-user-gecos unix-user-dir unix-user-shell

          ;; group records
          make-unix-group unix-group? unix-group-name unix-group-passwd unix-group-gid unix-group-mems)
  (import (cspp chez) (cspp utils) (cspp system private))

  #|proc:unix-user?
  The `unix-user?` procedure returns `#t` when its argument is a Unix user record, otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:unix-user
  Unix user records hold name, password field, uid, gid, GECOS, home directory, and shell.
  The exported `unix-user?` predicate accepts any object and returns a boolean.
  String fields remain mutable and are shared with callers of the constructor and accessors.
  |#
  (define-record-type (unix-user %make-unix-user unix-user?)
    (nongenerative)
    (fields (immutable name $unix-user-name)
            (immutable passwd $unix-user-passwd)
            (immutable uid $unix-user-uid)
            (immutable gid $unix-user-gid)
            (immutable gecos $unix-user-gecos)
            (immutable dir $unix-user-dir)
            (immutable shell $unix-user-shell)))

  #|proc:unix-user-name
  The `unix-user-name` procedure returns the login name of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-name
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-name user))))

  #|proc:unix-user-passwd
  The `unix-user-passwd` procedure returns the password field of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-passwd
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-passwd user))))

  #|proc:unix-user-uid
  The `unix-user-uid` procedure returns the numeric user ID of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-uid
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-uid user))))

  #|proc:unix-user-gid
  The `unix-user-gid` procedure returns the primary numeric group ID of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-gid
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-gid user))))

  #|proc:unix-user-gecos
  The `unix-user-gecos` procedure returns the GECOS field of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-gecos
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-gecos user))))

  #|proc:unix-user-dir
  The `unix-user-dir` procedure returns the home directory of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-dir
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-dir user))))

  #|proc:unix-user-shell
  The `unix-user-shell` procedure returns the login shell of a Unix user record.
  The `user` parameter is the user record returned by `user-ref`.
  |#
  (define-who unix-user-shell
    (lambda (user)
      (pcheck-who who ([unix-user? user]) ($unix-user-shell user))))

  #|proc:make-unix-user
  The `make-unix-user` procedure returns a Unix user record from the supplied fields.
  `name`, `passwd`, `gecos`, `dir`, and `shell` are strings holding the login name, password
  field, user information, home directory, and login shell. `uid` and `gid` are exact
  unsigned 32-bit user and primary group IDs.
  |#
  (define-who make-unix-user
    (lambda (name passwd uid gid gecos dir shell)
      (pcheck-who who ([string? name passwd gecos dir shell] [$unsigned-32? uid gid])
        (%make-unix-user name passwd uid gid gecos dir shell))))

  #|proc:unix-group?
  The `unix-group?` procedure returns `#t` when its argument is a Unix group record, otherwise
  `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:unix-group
  Unix group records hold name, password field, gid, and a member-name vector or `#f`.
  The exported `unix-group?` predicate accepts any object and returns a boolean.
  Strings and the member vector remain mutable and are shared with constructor/accessor callers.
  |#
  (define-record-type (unix-group %make-unix-group unix-group?)
    (nongenerative)
    (fields (immutable name $unix-group-name)
            (immutable passwd $unix-group-passwd)
            (immutable gid $unix-group-gid)
            (immutable mems $unix-group-mems)))

  #|proc:unix-group-name
  The `unix-group-name` procedure returns the name of a Unix group record.
  The `group` parameter is the group record returned by `group-ref`.
  |#
  (define-who unix-group-name
    (lambda (group)
      (pcheck-who who ([unix-group? group]) ($unix-group-name group))))

  #|proc:unix-group-passwd
  The `unix-group-passwd` procedure returns the password field of a Unix group record.
  The `group` parameter is the group record returned by `group-ref`.
  |#
  (define-who unix-group-passwd
    (lambda (group)
      (pcheck-who who ([unix-group? group]) ($unix-group-passwd group))))

  #|proc:unix-group-gid
  The `unix-group-gid` procedure returns the numeric group ID of a Unix group record.
  The `group` parameter is the group record returned by `group-ref`.
  |#
  (define-who unix-group-gid
    (lambda (group)
      (pcheck-who who ([unix-group? group]) ($unix-group-gid group))))

  #|proc:unix-group-mems
  The `unix-group-mems` procedure returns a vector of member names for a Unix group record, or
  `#f` when none are listed.
  The `group` parameter is the group record returned by `group-ref`.
  |#
  (define-who unix-group-mems
    (lambda (group)
      (pcheck-who who ([unix-group? group]) ($unix-group-mems group))))

  (define $group-members?
    (lambda (members)
      (or (not members)
          (and (vector? members)
               (let loop ([index 0])
                 (or (fx= index (vector-length members))
                     (and (string? (vector-ref members index)) (loop (fx+ index 1)))))))))

  #|proc:make-unix-group
  The `make-unix-group` procedure returns a Unix group record.
  `name` and `passwd` are group-name and password-field strings. `gid` is an exact unsigned
  32-bit group ID. `members` is a vector of member-name strings or `#f` when none are listed.
  |#
  (define-who make-unix-group
    (lambda (name passwd gid members)
      (pcheck-who who ([string? name passwd] [$unsigned-32? gid] [$group-members? members])
        (%make-unix-group name passwd gid members))))

)
