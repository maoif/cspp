(library (cspp system user)
  (export
          ;; generic lookup helpers
          getuser
          getgroup
          user-ref
          group-ref
          user-ref/default
          group-ref/default
          user-exists?
          group-exists?

          ;; user records
          unix-user?
          unix-user-name
          unix-user-passwd
          unix-user-uid
          unix-user-gid
          unix-user-gecos
          unix-user-dir
          unix-user-shell

          ;; group records
          unix-group?
          unix-group-name
          unix-group-passwd
          unix-group-gid
          unix-group-mems

          ;; id/name conversion
          uid->user
          user->uid
          gid->group
          group->gid
          uid->user-name
          user-name->uid
          gid->group-name
          group-name->gid
          get-user-dir
          get-user-shell
          get-user-group

          ;; current credentials
          getuid
          getgid
          geteuid
          getegid
          current-uid
          current-gid
          current-euid
          current-egid
          current-user
          current-group)
  (import (cspp system private user-records) (cspp system private user-credentials)
          (cspp system private user-lookup)))
