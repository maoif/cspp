(import (cspp))

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
