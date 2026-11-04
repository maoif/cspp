(import (cspp))

(mat system-user-unicode-context

     ;; Error case: a missing Unicode account name must survive native error-context conversion.
     (let ([name (format "cspp-no-user-~a-中文" (getpid))])
       (guard (condition
                [(system-not-found-error? condition)
                 (let ([entry (assoc "name" (system-error-context condition))])
                   (and entry (string=? name (cdr entry))))])
         (user-ref name)
         #f)))

(mat system-user-current

     (let ([u (user-ref (getuid))])
       (and (unix-user? u)
            (string? (unix-user-name u))
            (= (getuid) (unix-user-uid u))
            (= (getgid) (unix-user-gid u))
            (string? (unix-user-dir u))
            (string? (unix-user-shell u))))

     (unix-user? (getuser (uid->user-name (getuid))))

     (let ([g (group-ref (getgid))])
       (and (unix-group? g)
            (string? (unix-group-name g))
            (= (getgid) (unix-group-gid g))
            (or (vector? (unix-group-mems g))
                (not (unix-group-mems g)))))

     (unix-group? (getgroup (gid->group-name (getgid))))

     (unix-user? (current-user))
     (unix-group? (current-group)))

(mat system-user-conversions

     (let ([name (uid->user-name (getuid))])
       (and (string? name)
            (= (getuid) (user-name->uid name))
            (= (getuid) (user->uid name))))

     (let ([name (gid->group-name (getgid))])
       (and (string? name)
            (= (getgid) (group-name->gid name))
            (= (getgid) (group->gid name))))

     (string? (get-user-dir (getuid)))
     (string? (get-user-dir (uid->user-name (getuid))))
     (string? (get-user-shell (getuid)))
     (string? (get-user-shell (uid->user-name (getuid))))
     (integer? (get-user-group (getuid)))
     (integer? (get-user-group (uid->user-name (getuid))))

     (user-exists? (getuid))
     (user-exists? (uid->user-name (getuid)))
     (group-exists? (getgid))
     (group-exists? (gid->group-name (getgid))))

;; Error case: looking up an impossible user name should report not-found.
(mat system-user-missing

     (guard (c [(system-not-found-error? c) #t] [else #f])
       (user-ref "__cspp_user_that_should_not_exist__")
       #f)

     (guard (c [(system-not-found-error? c) #t] [else #f])
       (group-ref "__cspp_group_that_should_not_exist__")
       #f)

     (eq? 'missing (user-ref/default "__cspp_user_that_should_not_exist__"
                                     'missing))

     (eq? 'missing (group-ref/default "__cspp_group_that_should_not_exist__"
                                      'missing))

     (not (user-exists? "__cspp_user_that_should_not_exist__"))

     (not (group-exists? "__cspp_group_that_should_not_exist__")))

(mat system-user-concurrent-lookups
  ;; Error case: simultaneous NSS lookups must not mix names and numeric identities.
  (= 0 (system (if (file-exists? "system-user-thread.ss")
                   "timeout 15 ../cspp --script=system-user-thread.ss"
                   "timeout 15 ./cspp --script=tests/system-user-thread.ss"))))

(mat system-user-id-validation
  ;; Error case: inexact or out-of-range IDs must not reach a narrowing foreign call.
  (error? (user-ref 1.0))

  (error? (group-ref #x100000000))

  ;; Error case: embedded NUL must not truncate an account name.
  (error? (user-ref "root\x0;ignored")))
