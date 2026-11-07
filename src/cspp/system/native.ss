(library (cspp system native)
  (export $call-native-operation $call-process-operation)
  (import (cspp chez) (cspp utils) (cspp system common))

  #|proc:$call-native-operation
  The internal `$call-native-operation` procedure owns a resumable native operation.
  `who` is its caller symbol. `create` has signature `() -> native-address`; `prepare`
  initializes that address; `step` takes it and returns zero while work remains, nonzero
  when done. `result` takes it and returns a tagged FFI result. `dispose` takes it and frees
  all remaining native resources. `consume` takes the decoded result and returns the final
  values; it must publish newly transferred resources inside its own cleanup guard.
  Native steps must bound blocking waits and permit GC without accessing Scheme objects.
  Pending Scheme events run between steps. Resource publication runs with interrupts disabled.
  Leaving the scope disposes its state; replaying a continuation after disposal raises an error.
  A continuation may not enter the native operation from a different Scheme thread.
  |#
  (define $call-native-operation
    (lambda (who create prepare step result dispose consume)
      (pcheck-who who ([procedure? create prepare step result dispose consume])
        (let ([handle 0] [closed? #f] [owner (get-thread-id)])
          (dynamic-wind
            (lambda ()
              (unless (= owner (get-thread-id))
                (errorf who "cannot enter a native operation from another thread"))
              (when closed? (errorf who "cannot resume a disposed native operation")))
            (lambda ()
              ;; Acquire in the protected body and publish before enabling interrupts.
              ;; Native initialization owns copied inputs even if prepare reports failure.
              (with-interrupts-disabled
                (set! handle (create))
                (when (= handle 0)
                  (raise (make-system-error who 12 "native operation allocation failed" '())))
                (prepare handle))
              (let loop ()
                ;; Balanced scope forces a Chez event check and preserves caller disable depth.
                (with-interrupts-disabled (void))
                (if (= 0 (step handle))
                    (loop)
                    (with-interrupts-disabled
                      (consume (ffi-result-ref (result handle)))))))
            (lambda ()
              (with-interrupts-disabled
                (set! closed? #t)
                (unless (= handle 0)
                  (dispose handle)
                  (set! handle 0)))))))))

  (define $process-create
    (foreign-procedure "cspp_process_operation_create" () uptr))
  (define $process-step
    (foreign-procedure __collect_safe "cspp_process_operation_step" (uptr) int))
  (define $process-result
    (foreign-procedure "cspp_process_operation_result" (uptr) ptr))
  (define $process-dispose
    (foreign-procedure __collect_safe "cspp_process_operation_dispose" (uptr) void))

  #|proc:$call-process-operation
  The internal `$call-process-operation` procedure runs one resumable process operation.
  `who` is its caller symbol. `prepare` has signature `native-address -> unspecified` and
  initializes copied native inputs. `consume` receives the decoded result and returns its values,
  publishing transferred live resources inside a previously installed cleanup guard.
  Process steps and cleanup share `$call-native-operation`'s lifetime and interrupt contract.
  |#
  (define $call-process-operation
    (lambda (who prepare consume)
      ($call-native-operation who $process-create prepare $process-step $process-result
                              $process-dispose consume)))
)
