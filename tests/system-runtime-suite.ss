(import (cspp))

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
