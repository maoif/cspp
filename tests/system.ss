(import (cspp)
        (cspp system linux os-release)
        (only (cspp system process expert) make-process-exit-status)
        (only (cspp system user records) make-unix-user make-unix-group))

(mat system-argument-error-origins

     ;; Error case: argument rejection must identify the system API before reaching native code.
     (for-all
       (lambda (entry)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? (car entry) (condition-who condition)))])
           ((cdr entry)) #f))
       (list (cons 'process-pid (lambda () (process-pid #f)))
             (cons 'filesystem-info (lambda () (filesystem-info #f)))
             (cons 'unix-user-name (lambda () (unix-user-name #f)))
             (cons 'user-ref (lambda () (user-ref 1.0)))
             (cons 'user-ref/default (lambda () (user-ref/default #f 'missing)))
             (cons 'group-ref/default (lambda () (group-ref/default #f 'missing)))
             (cons 'user-exists? (lambda () (user-exists? #f)))
             (cons 'group-exists? (lambda () (group-exists? #f)))
             (cons 'signal (lambda () (signal #f)))
             (cons 'signal-name (lambda () (signal-name #f)))
             (cons 'send-signal (lambda () (send-signal 0 'term)))
             (cons 'sleep (lambda () (sleep 1.0)))
             (cons 'make-system-error (lambda () (make-system-error 'test #f #f '())))
             (cons 'raise-system-error (lambda () (raise-system-error 'test #f #f '())))
             (cons 'raise-system-unsupported (lambda () (raise-system-unsupported 'test #f)))
             (cons 'make-system-timeout-error
                   (lambda () (make-system-timeout-error 'test #f)))
             (cons 'process-capture (lambda () (process-capture #f)))
             (cons 'process-capture (lambda () (process-capture /bin/true :timeout -1)))
             (cons 'process-spawn-shell
                   (lambda () (process-spawn-shell "exit 0" '((cwd . ""))))))))

(mat cpu-count-affinity

     ;; Error case: an online CPU outside the caller's affinity mask is not available to it.
     (or (not (linux?))
         (let* ([get-affinity
                  (foreign-procedure __errno "sched_getaffinity" (int uptr u8*) int)]
                [set-affinity (foreign-procedure "sched_setaffinity" (int uptr u8*) int)]
                [original
                  (let loop ([size 128])
                    (let ([mask (make-bytevector size 0)])
                      (let-values ([(result code) (get-affinity 0 size mask)])
                        (cond [(= result 0) mask]
                              [(= code 22) (loop (* size 2))]
                              [else (error 'cpu-count-affinity "cannot read CPU affinity" code)]))))]
                [single (make-bytevector (bytevector-length original) 0)]
                [before (cpu-count)])
           (let find-byte ([index 0])
             (let ([byte (bytevector-u8-ref original index)])
               (if (= byte 0)
                   (find-byte (+ index 1))
                   (let find-bit ([bit 1])
                     (if (= (logand byte bit) 0)
                         (find-bit (* bit 2))
                         (bytevector-u8-set! single index bit))))))
           (and
             (dynamic-wind
               (lambda ()
                 (unless (= 0 (set-affinity 0 (bytevector-length single) single))
                   (error 'cpu-count-affinity "cannot restrict CPU affinity")))
               (lambda () (= 1 (cpu-count)))
               (lambda ()
                 (unless (= 0 (set-affinity 0 (bytevector-length original) original))
                   (error 'cpu-count-affinity "cannot restore CPU affinity"))))
             (= before (cpu-count))))))

(mat shared-object-unicode-path

     ;; Error case: native UTF-8 library paths must retain their Unicode characters.
     (let ([path (format "/tmp/cspp-shared-é中文-~a.so" (getpid))])
       (dynamic-wind
         (lambda () (file-copy "../libcspp.so" path))
         (lambda ()
           (load-shared-object path)
           (and (member path (shared-object-list)) #t))
         (lambda () (delete-file path)))))

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

(mat linux-os-release-repeated-keys

     ;; Error case: repeated assignments must return one entry per key with the latest value.
     (let ([path (format "/tmp/cspp-os-release-repeat-~a" (getpid))])
       (dynamic-wind
         (lambda ()
           (call-with-output-file path
             (lambda (port)
               (put-string port
                 "NAME=Old\nID=example\nNAME=New\nVERSION=first\nVERSION=\n"))))
         (lambda ()
           (equal? '((NAME . "New") (ID . "example") (VERSION . ""))
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

(mat system-standard-condition-fields

     ;; Error case: system errors must be readable by ordinary Chez condition handlers.
     (for-all
       (lambda (make-error)
         (let ([failure (make-error 'example "failure ~s" '((detail . 17)))])
           (and (system-error? failure) (error? failure)
                (who-condition? failure) (message-condition? failure)
                (eq? 'example (condition-who failure))
                (string=? "failure ~s" (condition-message failure))
                (equal? '((detail . 17)) (system-error-context failure)))))
       (list (lambda (operation message context)
               (make-system-error operation 5 message context))
             make-system-unsupported-error make-system-not-found-error
             make-system-permission-error make-system-timeout-error make-system-exit-error))

     ;; Unknown operations still expose a standard who component with the value #f.
     (let ([failure (make-system-error #f #f "unknown operation" '())])
       (and (who-condition? failure) (not (condition-who failure))
            (string=? "unknown operation" (condition-message failure))))

     ;; Native FFI errors and checked process failures must carry the same standard fields.
     (and
       (guard (failure
                [(system-permission-error? failure)
                 (and (eq? 'native-op (condition-who failure))
                      (string=? "denied" (condition-message failure)))])
         (ffi-result-ref '#(errno native-op 13 "denied" ())) #f)
       (guard (failure
                [(system-exit-error? failure)
                 (and (eq? 'process-run (condition-who failure))
                      (string? (condition-message failure)))])
         (process-run/check /bin/false) #f)))

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

(mat system-memory-info-field-names

     ;; Error case: punctuation in kernel field names must survive conversion into symbol keys.
     (if (and (linux?) (file-exists? "/proc/meminfo"))
         (let* ([info (linux-memory-info)]
                [names (map (lambda (line) (car (string-split line #\:)))
                            (read-lines "/proc/meminfo"))])
           (and (equal? names (map (lambda (entry) (symbol->string (car entry))) info))
                (for-all (lambda (entry) (natural? (cdr entry))) info)
                (pair? (assq (string->symbol "Active(anon)") info))
                (pair? (assq (string->symbol "Inactive(file)") info))))
         #t))

(mat system-procfs-query-allocation

     ;; Error case: uptime queries must not allocate mount records unrelated to the result.
     (if (and (linux?) (file-exists? "/proc/uptime"))
         (let* ([direct-query
                  (lambda ()
                    (call-with-input-file "/proc/uptime"
                      (lambda (port)
                        (string->number (car (string-split (get-line port) #\space))))))]
                [allocated
                  (lambda (query)
                    (query)
                    (collect)
                    (let ([before (+ (bytes-allocated) (bytes-deallocated))])
                      (do ([index 0 (+ index 1)]) ((= index 10)) (query))
                      (- (+ (bytes-allocated) (bytes-deallocated)) before)))])
           (< (allocated linux-system-uptime) (+ (allocated direct-query) 40960)))
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
