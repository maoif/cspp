(import (cspp)
        (cspp system linux os-release)
        (only (cspp system process expert) make-process-exit-status)
        (only (cspp system user records) make-unix-user make-unix-group))

(mat linux-os-release-quoting

     ;; Error case: shell escapes in quoted release names must not leak into returned values.
     (let ([path (format "/tmp/cspp-os-release-~a" (getpid))])
       (dynamic-wind
         (lambda ()
           (call-with-output-file path
             (lambda (port)
               (put-string port
                 (string-append
                   "  # comment\n\nID=example\nEMPTY=\n"
                   "PRETTY_NAME=\"Example \\\"Edition\\\"\"\n"
                   "ESCAPED=\"\\$HOME \\`literal\\` \\\\path\"\n"
                   "SINGLE='\\$HOME \\path'\n"
                   "UNQUOTED=Example\\ OS\n"
                   "KEEP=\"a\\q ${USER}\"\n")))))
         (lambda ()
           (equal? '((ID . "example") (EMPTY . "") (PRETTY_NAME . "Example \"Edition\"")
                     (ESCAPED . "$HOME `literal` \\path") (SINGLE . "\\$HOME \\path")
                     (UNQUOTED . "Example OS") (KEEP . "a\\q ${USER}"))
                   ($read-os-release path)))
         (lambda () (delete-file path)))))

(mat system-record-constructor-safety

     ;; Error case: exit statuses require a known kind and an exact nonnegative code.
     (and (guard (condition [else #t]) (make-process-exit-status 'unknown 0) #f)
          (guard (condition [else #t]) (make-process-exit-status 'exit "zero") #f)
          (guard (condition [else #t]) (make-process-exit-status 'exit -1) #f))

     ;; Error case: Unix records require string fields and unsigned 32-bit identities.
     (and (guard (condition [else #t])
            (make-unix-user #f "x" 0 0 "" "/" "/bin/sh") #f)
          (guard (condition [else #t])
            (make-unix-user "user" "x" -1 0 "" "/" "/bin/sh") #f)
          (guard (condition [else #t])
            (make-unix-group "group" "x" #x100000000 #f) #f))

     ;; Error case: group members must be a vector of strings or #f.
     (and (guard (condition [else #t]) (make-unix-group "g" "x" 0 '("member")) #f)
          (guard (condition [else #t]) (make-unix-group "g" "x" 0 '#(1)) #f))

     (and (process-exit-success? (make-process-exit-status 'exit 0))
          (unix-user? (make-unix-user "u" "x" 0 0 "" "/" "/bin/sh"))
          (equal? '#("member") (unix-group-mems (make-unix-group "g" "x" 0 '#("member"))))))

(mat system-record-argument-safety

     ;; Error case: a vector with plausible fields is not a record of any system type.
     (let ([invalid (make-vector 16 0)])
       (andmap
         (lambda (accessor)
           (guard (condition [else #t]) (accessor invalid) #f))
         (list process-exit-status-kind process-exit-status-code
               process-pid process-command process-arguments
               process-stdin process-stdout process-stderr
               process-result-status process-result-stdout process-result-stderr
               process-result-pid process-result-command
               filesystem-info-path filesystem-info-device filesystem-info-type
               filesystem-info-block-size filesystem-info-blocks filesystem-info-blocks-free
               filesystem-info-blocks-available filesystem-info-files filesystem-info-files-free
               filesystem-info-read-only?
               mounted-filesystem-source mounted-filesystem-target
               mounted-filesystem-type mounted-filesystem-options
               signal-name signal-number
               unix-user-name unix-user-passwd unix-user-uid unix-user-gid
               unix-user-gecos unix-user-dir unix-user-shell
               unix-group-name unix-group-passwd unix-group-gid unix-group-mems))))

(mat system-errors

     ;; Error case: system context must be an alist and native error codes must be exact.
     (and (guard (condition [else #t]) (make-system-error 'test #f "failure" '(invalid)) #f)
          (guard (condition [else #t]) (make-system-error 'test 1.0 "failure" '()) #f))

     (os-error? (make-system-error 'compatibility #f "failure" '()))

     (system-error? (guard (c [else c])
                      (raise-system-unsupported 'test-op "unsupported")))

     (system-unsupported-error? (guard (c [else c])
                                  (raise-system-unsupported 'test-op "unsupported")))

     (let ([c (guard (c [else c])
                (raise-system-error 'test-op 1 "operation not permitted" '((path . "/"))))])
       (and (system-error? c)
            (eq? 'test-op (system-error-operation c))
            (= 1 (system-error-code c))
            (string? (system-error-message c))
            (pair? (system-error-context c))))

     (eq? 'value (ffi-result-ref '#(ok value)))

     (system-not-found-error? (guard (c [else c])
                                (ffi-result-ref '#(not-found test-op ((path . "/missing"))))))

     (let ([c (guard (c [else c])
                (ffi-result-ref '#(errno test-op 13 "permission denied" ((path . "/")))))])
       (and (system-permission-error? c)
            (= 13 (system-error-code c))))

     (let ([c (make-system-error 'test-op 5 "io error" '((path . "/tmp")))])
       (and (system-error? c)
            (eq? 'test-op (system-error-operation c))
            (= 5 (system-error-code c))))

     (system-not-found-error?
      (make-system-not-found-error 'lookup "missing" '((name . "missing"))))

     (system-timeout-error?
      (make-system-timeout-error 'wait "timed out" '((timeout . 10))))

     (system-exit-error?
      (make-system-exit-error 'process-run "failed" '((status . 1))))

     (system-unsupported-error?
      (make-system-unsupported-error 'feature "unsupported"))

     ;; Error case: malformed FFI result vectors should be rejected.
     (guard (c [else #t])
       (ffi-result-ref '#(bad value))
       #f))

(mat user-credentials

     (<= 0 (getuid))
     (<= 0 (getgid))
     (<= 0 (geteuid))
     (<= 0 (getegid)))

(mat current-ids

     (let ([uid (getuid)]
           [gid (getgid)]
           [euid (geteuid)]
           [egid (getegid)])
       (and (integer? uid)
            (integer? gid)
            (integer? euid)
            (integer? egid)
            (<= 0 uid)
            (<= 0 gid)
            (<= 0 euid)
            (<= 0 egid))))

(mat user-group-roundtrip

     (let* ([uid (getuid)]
            [name (uid->user uid)])
       (= uid (user->uid name)))

     (let* ([gid (getgid)]
            [name (gid->group gid)])
       (= gid (group->gid name))))

(mat process-ids

     (< 1 (getpid))
     (< 1 (getppid))
     (<= 0 (gettid)))

(mat system-info

     (string? (hostname))
     (string? (cpu-arch))
     (<= 1 (cpu-count))
     (string? (system-hostname))
     (symbol? (system-machine))
     (boolean? (unix?))
     (boolean? (windows?))
     (boolean? (darwin?))
     (begin (sleep-seconds 0) #t)
     (begin (sleep-milliseconds 0) #t)
     (begin (sleep-nanoseconds 0) #t))

(mat system-platform-modules

     (and (memq (system-platform) '(linux darwin windows unknown)) #t)
     (boolean? (linux?))
     (boolean? (darwin?))
     (boolean? (windows?)))

;; Error case: unimplemented platform stubs should raise unsupported.
(mat system-platform-stubs

     (if (linux?)
         (guard (c [(system-unsupported-error? c) #t] [else #f])
           (windows-system-version)
           #f)
         #t))

(mat system-linux-apis

     (if (linux?)
         (boolean? (linux-procfs-mounted?))
         #t)

     (if (linux?)
         (filesystem-info? (linux-filesystem-info "."))
         #t)

     (if (linux?)
         (list? (linux-mounted-filesystems))
         #t)

     (if (linux?)
         (exists (lambda (sig) (eq? 'term (signal-name sig))) (linux-signal-list))
         #t)

     (if (and (linux?) (linux-procfs-mounted?))
         (number? (linux-system-uptime))
         #t)

     (if (and (linux?) (linux-procfs-mounted?))
         (list? (linux-memory-info))
         #t)

     (if (and (linux?) (linux-procfs-mounted?))
         (let ([loads (linux-load-average)])
           (and (= 3 (length loads))
                (andmap number? loads)))
         #t)

     (if (linux?)
         (list? (linux-os-release))
         #t))
