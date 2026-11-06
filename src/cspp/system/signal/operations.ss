(library (cspp system signal operations)
  (export send-signal send-process-signal send-process-group-signal signal-mask
          signal-mask-set! signal-block! signal-unblock! wait-signal)
  (import (cspp chez) (cspp utils) (cspp system common) (cspp system private)
          (cspp system signal values) (cspp system process expert))

;;;;===----------------------------------------------------------------------===
;;;; signal operations
;;;;===----------------------------------------------------------------------===

  (define $send-signal-ffi
    (foreign-procedure "cspp_send_signal" (int int) ptr))

  #|proc:send-signal
  The `send-signal` procedure sends `sig` to process `pid`.
  The `pid` parameter is an exact positive signed 32-bit integer process ID.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  Sending a process-directed signal to this Scheme process requires blocking it in every
  eligible thread before creating those threads if it will be consumed with `wait-signal`.
  Returns unspecified values on success.
  |#
  (define-who send-signal
    (lambda (pid sig)
      (pcheck-who who ([$positive-pid? pid] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi pid (signal-number ($signal-ref who sig))))
              (void))))

  #|proc:send-process-signal
  The `send-process-signal` procedure sends `sig` to `process`.
  The `process` parameter is a CSPP process object.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  Process status synchronization prevents signaling a process after its exit has been reaped.
  Returns unspecified values on success.
  |#
  (define-who send-process-signal
    (lambda (process sig)
      (pcheck-who who ([process? process] [$signal-input? sig])
              (process-kill process (signal-number ($signal-ref who sig)))
              (void))))

  #|proc:send-process-group-signal
  The `send-process-group-signal` procedure sends `sig` to process group `process-group-id`.
  The `process-group-id` parameter is an exact signed 32-bit integer greater than 1.
  Group ID 1 is rejected because negating it selects the native broadcast operation.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  Returns unspecified values on success.
  |#
  (define-who send-process-group-signal
    (lambda (process-group-id sig)
      (pcheck-who who ([(lambda (value) (and ($positive-pid? value) (> value 1))) process-group-id]
               [$signal-input? sig])
              (ffi-result-ref
               ($send-signal-ffi (- process-group-id) (signal-number ($signal-ref who sig))))
              (void))))

  (define $mask-ffi
    (foreign-procedure "cspp_signal_mask" () ptr))

  (define $mask-update-ffi
    (foreign-procedure "cspp_signal_mask_update" (ptr int) ptr))

  (define $wait-signal-ffi
    (foreign-procedure "cspp_wait_signal" (ptr) ptr))

  (define $signal-numbers
    (lambda (who signals)
      (list->vector
       (map (lambda (spec) (signal-number ($signal-ref who spec))) signals))))

  #|proc:signal-mask
  The `signal-mask` procedure returns a list of signal records blocked in the calling native thread.
  This implementation supports Linux; other platforms raise a system unsupported condition.
  |#
  (define-who signal-mask
    (lambda ()
      (map (lambda (number) ($signal-ref who number)) (ffi-result-ref ($mask-ffi)))))

  #|proc:signal-mask-set!
  The `signal-mask-set!` procedure replaces the calling native thread's signal mask with `signals`.
  The `signals` parameter is a list of signal specs accepted by `signal`.
  SIGKILL and SIGSTOP are rejected. The mask state of synchronous runtime fault signals
  SIGSEGV, SIGBUS, SIGILL, and SIGFPE is preserved; newly blocking them is rejected.
  Passing a previous `signal-mask` result restores it. Returns unspecified values.
  Other threads' masks are unchanged. Platforms other than Linux raise unsupported conditions.
  |#
  (define-who signal-mask-set!
    (lambda (signals)
      (pcheck-who who ([list? signals])
              (ffi-result-ref ($mask-update-ffi ($signal-numbers who signals) 0))
              (void))))

  #|proc:signal-block!
  The `signal-block!` procedure adds each signal in `signals` to the calling native thread's mask.
  The `signals` parameter is a list of signal specs accepted by `signal`.
  SIGKILL, SIGSTOP, SIGSEGV, SIGBUS, SIGILL, and SIGFPE are rejected.
  Returns unspecified values; existing mask entries and other threads' masks are preserved.
  Before using a process-directed signal with `wait-signal`, block it in every eligible thread.
  Blocking it before creating threads lets those threads inherit the mask.
  |#
  (define-who signal-block!
    (lambda (signals)
      (pcheck-who who ([list? signals])
              (ffi-result-ref ($mask-update-ffi ($signal-numbers who signals) 1))
              (void))))

  #|proc:signal-unblock!
  The `signal-unblock!` procedure removes `signals` from the calling native thread's signal mask.
  The `signals` parameter is a list of signal specs accepted by `signal`.
  SIGKILL, SIGSTOP, SIGSEGV, SIGBUS, SIGILL, and SIGFPE are rejected.
  Returns unspecified values; other entries and other threads' masks are preserved.
  |#
  (define-who signal-unblock!
    (lambda (signals)
      (pcheck-who who ([list? signals])
              (ffi-result-ref ($mask-update-ffi ($signal-numbers who signals) 2))
              (void))))

  #|proc:wait-signal
  The `wait-signal` procedure waits until one of `signals` is delivered and returns that signal.
  The `signals` parameter is a nonempty list of signal specs accepted by `signal`.
  Every listed signal must already be blocked in the calling native thread.
  SIGKILL, SIGSTOP, SIGSEGV, SIGBUS, SIGILL, and SIGFPE are rejected.
  Waiting preserves the mask and permits other Scheme threads to run and collect.
  A process-directed signal can reach any eligible thread: block it in every such thread
  before sending it, or arrange thread-directed delivery. No Scheme signal handlers are installed.
  |#
  (define-who wait-signal
    (lambda (signals)
      (pcheck-who who ([list? signals])
              ($signal-ref who
                           (ffi-result-ref ($wait-signal-ffi ($signal-numbers who signals)))))))

  )
