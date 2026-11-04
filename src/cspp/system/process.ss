(library (cspp system process)
  ;; Public facade: command macros and completed results are independent of live process state.
  ;; See docs/system.md for usage, defaults, and stream ownership.
  (export
          ;; exit status records
          process-exit-status?
          process-exit-status-kind
          process-exit-status-code
          process-exit-success?

          ;; process records
          process?
          process-pid
          process-command
          process-arguments
          process-stdin
          process-stdout
          process-stderr
          process-status
          process-running?

          ;; expert process controls re-exported for convenience
          process-spawn
          process-spawn-shell
          process-wait
          process-wait/no-hang
          process-wait/timeout
          process-kill
          process-terminate
          process-interrupt
          process-close-ports!
          make-pipe
          process-spawn-pipeline
          process-run-pipeline
          fork
          vfork
          getpid
          gettid
          getppid

          ;; process result records
          process-result?
          process-result-status
          process-result-stdout
          process-result-stderr
          process-result-pid
          process-result-command

          ;; high-level process forms
          process-run
          process-run/check
          process-capture
          process-capture/check
          process-run-shell
          process-capture-shell
          process-capture-pipeline)
  (import (cspp system process expert)
          (cspp system process backend)
          (cspp system process macros)))
