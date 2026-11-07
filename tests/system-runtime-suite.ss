(import (cspp))

(mat system-tilde-lookup-allows-collection

  ;; Error case: slow account services used by tilde expansion must not stall other threads' GC.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss tilde-lookup-gc"
                   "./cspp --script=tests/system-runtime-runner.ss tilde-lookup-gc"))))

(mat system-preexec-signal-dispositions

  ;; Error case: a child must not enter Chez's inherited handlers while preparing exec.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "-qq" "-e" "trace=close_range"
            "-e" "inject=close_range:delay_enter=800ms:when=1" "-o" "/dev/null"
            (begin launcher) (begin script) "preexec-signal" :timeout 5000)))
      #t))

(mat system-spawn-interrupts

  ;; Error case: exec setup must return to Scheme and retain ownership across handler escapes.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "-qq" "-e" "trace=close_range"
            "-e" "inject=close_range:delay_enter=800ms:when=1" "-o" "/dev/null"
            (begin launcher) (begin script) "spawn-interrupt" :timeout 5000)))
      #t))

(mat system-resource-acquisition-interrupts

  ;; Error case: resource handoff needs isolation because the old implementation leaves children.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss ownership-interrupts handoff-interrupt-depth"
                   "./cspp --script=tests/system-runtime-runner.ss ownership-interrupts handoff-interrupt-depth"))))

(mat system-live-wait-notification

  ;; Error case: wait notification must preserve timeout, concurrent reaping, and GC contracts.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss wait-notification wait-interrupt pending-wait-interrupt"
                   "./cspp --script=tests/system-runtime-runner.ss wait-notification wait-interrupt pending-wait-interrupt"))))

(mat system-native-operation-interrupts

  ;; Error case: synchronous native operations must retain cleanup while servicing Scheme handlers.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss native-interrupts native-continuation-lifetime timeout-grace"
                   "./cspp --script=tests/system-runtime-runner.ss native-interrupts native-continuation-lifetime timeout-grace"))))

(mat system-live-wait-fallback

  ;; Error case: unavailable pidfds must retain the previous wait behavior.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-qq" "-e" "trace=pidfd_open"
            "-e" "inject=pidfd_open:error=ENOSYS" "-o" "/dev/null"
            (begin launcher) (begin script) "live-wait" :timeout 5000)))
      #t))

(mat system-live-wait-efficiency

  ;; Error case: a supported native exit notification must not wake to reap every few milliseconds.
  (let ([available?
          (and (foreign-entry? "pidfd_open")
               (let ([fd ((foreign-procedure "pidfd_open" (int unsigned) int) (getpid) 0)])
                 (and (>= fd 0) (begin ((foreign-procedure "close" (int) int) fd) #t))))])
    (if (and available? (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1")))
        (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
               [launcher (string-append root "/cspp")]
               [script (string-append "--script=" root "/tests/system-native-faults.ss")]
               [trace (format "/tmp/cspp-live-wait-~a.trace" (getpid))])
          (dynamic-wind
            void
            (lambda ()
              (and
                (process-exit-success?
                  (process-run strace "-qq" "-e" "trace=wait4" "-o" (begin trace)
                    (begin launcher) (begin script) "live-wait" :timeout 5000))
                (< (length (filter (lambda (line) (string-startswith? line "wait4("))
                                   (read-lines trace))) 10)))
            (lambda () (when (file-exists? trace) (delete-file trace)))))
        #t)))

(mat system-native-error-details

  ;; Error case: native allocation and localized messages need isolation from the test runner.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss native-allocation-errors localized-errors legacy-localized-errors"
                   "./cspp --script=tests/system-runtime-runner.ss native-allocation-errors localized-errors legacy-localized-errors"))))

(mat system-pipeline-descriptor-limit

  ;; Error case: pipeline launch must use a bounded number of connection descriptors.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss pipeline-low-descriptor-limit"
                   "./cspp --script=tests/system-runtime-runner.ss pipeline-low-descriptor-limit"))))

(mat system-spawn-without-descriptor-enumeration

  ;; Error case: without reliable descriptor closure, launch must fail rather than inherit handles.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "-qq" "-e" "trace=close_range,openat"
            "-e" "inject=close_range:error=ENOSYS" "-e" "inject=openat:error=ENOENT:when=1"
            "-o" "/dev/null" (begin launcher) (begin script)
            "spawn-without-descriptor-enumeration" :timeout 5000)))
      #t))

(mat system-pipeline-io-failure-cleanup

  ;; Error case: an I/O error must not wait indefinitely for independent upstream work.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "-qq" "-e" "trace=poll"
            "-e" "inject=poll:error=EIO:when=1" "-o" "/dev/null"
            (begin launcher) (begin script) "pipeline-io-failure" :timeout 5000)))
      #t))

(mat system-pipeline-failure-allows-collection

  ;; Error case: reaping a partially launched pipeline must not stall unrelated collections.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (for-all
          (lambda (mode)
            (process-exit-success?
              (process-run strace "-f" "--quiet=path-resolution,attach,personality,exit"
                "-e" "trace=wait4" "-e" "inject=wait4:delay_enter=2s:when=1"
                "-o" "/dev/null" (begin launcher) (begin script) (begin mode) :timeout 5000)))
          '("pipeline-run-gc" "pipeline-capture-gc" "pipeline-spawn-gc")))
      #t))

(mat system-filesystem-query-allows-collection

  ;; Error case: a delayed statfs must allow another thread to complete garbage collection.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "--quiet=path-resolution,attach,personality,exit"
            "-e" "trace=statfs,fstatfs" "-e" "inject=statfs:delay_enter=2s:when=1"
            "-e" "inject=fstatfs:delay_enter=2s:when=1"
            "-o" "/dev/null" (begin launcher) (begin script) "filesystem-gc" :timeout 5000)))
      #t))

(mat system-filesystem-single-target

  ;; Error case: a symlink replacement must not combine two filesystems in one record.
  (if (and (file-directory? "/dev/shm")
           (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1")))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "--quiet=path-resolution,attach,personality,exit"
            "-e" "trace=statfs,fstatfs" "-e" "inject=statfs:delay_exit=1s:when=1"
            "-e" "inject=fstatfs:delay_exit=1s:when=1"
            "-o" "/dev/null" (begin launcher) (begin script) "filesystem-snapshot"
            :timeout 5000)))
      #t))

(mat system-procfs-without-mountinfo

  ;; Error case: all metric queries must work without opening the mount table.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "--quiet=path-resolution,attach,personality,exit"
            "-P" "/proc/self/mountinfo"
            "-e" "trace=openat" "-e" "inject=openat:error=EACCES"
            "-o" "/dev/null" (begin launcher) (begin script) "procfs-without-mountinfo"
            :timeout 5000)))
      #t))

(mat system-native-host-query-errors

  ;; Error case: unavailable host information must raise rather than return an error as data.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (process-exit-success?
          (process-run strace "-f" "-qq" "-e" "trace=uname" "-e" "inject=uname:error=EPERM"
            "-o" "/dev/null" (begin launcher) (begin script) "host-query" :timeout 5000)))
      #t))

(mat system-native-cleanup-errors

  ;; Error case: retrying an interrupted wait must preserve the original pipeline launch failure.
  ;; Syscall injection is optional on hosts without strace or permission to trace a child.
  (if (= 0 (system "strace -qq -o /dev/null true >/dev/null 2>&1"))
      (let* ([root (if (file-exists? "system-native-faults.ss") ".." ".")]
             [launcher (string-append root "/cspp")]
             [script (string-append "--script=" root "/tests/system-native-faults.ss")])
        (for-all
          (lambda (mode)
            (process-exit-success?
              (process-run strace "-f" "-qq" "-e" "trace=wait4"
                "-e" "inject=wait4:error=EINTR:when=1" "-o" "/dev/null"
                (begin launcher) (begin script) (begin mode) :timeout 5000)))
          '("pipeline-run" "pipeline-capture" "pipeline-spawn")))
      #t))

(mat system-runtime-isolation
  ;; Error case: native stalls, crashes, inherited input, and child cleanup need isolation.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss boundaries capture gc inherit wait ports environment-race signals pipeline-contracts vfork-rejected"
                   "./cspp --script=tests/system-runtime-runner.ss boundaries capture gc inherit wait ports environment-race signals pipeline-contracts vfork-rejected"))))

(mat system-loader-concurrency

  ;; Error case: loading new libraries while listing them must not deadlock Chez's runtime.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss loader-list-race"
                   "./cspp --script=tests/system-runtime-runner.ss loader-list-race"))))
