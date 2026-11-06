(library (cspp system)
  (export
          ;; raw process APIs retained at the facade
          fork
          vfork
          getpid
          gettid
          getppid

          ;; dynamic loader information
          shared-object-list

          ;; compatibility condition predicate
          os-error?)
  (import (cspp chez)
          (cspp system common)
          (except (cspp system darwin) darwin?)
          (cspp system filesystem)
          (cspp system info)
          (except (cspp system linux) linux?)
          (cspp system process)
          (cspp system signal)
          (cspp system user)
          (except (cspp system windows) windows?)
          (cspp utils))

  (export
          ;; focused system libraries re-exported by the facade
          (import (cspp system common)
                  (except (cspp system darwin) darwin?)
                  (cspp system filesystem)
                  (cspp system info)
                  (except (cspp system linux) linux?)
                  (cspp system process)
                  (cspp system signal)
                  (cspp system user)
                  (except (cspp system windows) windows?)))

;;;;===----------------------------------------------------------------------===
;;;; OS errors
;;;;===----------------------------------------------------------------------===

  #|proc:os-error?
  The `os-error?` procedure tests `condition` for a CSPP system error condition.
  It is a compatibility alias of `system-error?` and returns a boolean.
  |#
  (define os-error? system-error?)

;;;;===----------------------------------------------------------------------===
;;;; processes
;;;;===----------------------------------------------------------------------===

  #|proc:shared-object-list
  The `shared-object-list` procedure returns shared objects currently loaded by the process,
  in load order, as a list of strings. Concurrent loading and collection are supported;
  native snapshot failures raise a system condition.
  |#
  (define-who shared-object-list
    (let ([ffi (foreign-procedure "cspp_shared_object_list" () ptr)])
      (lambda ()
        (let* ([x (ffi-result-ref (ffi))] [rx (reverse x)]
               [res (if (and (pair? rx) (string=? "" (car rx))) (cdr rx) rx)])
          res))))

  )
