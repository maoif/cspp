(library (cspp system process status)
  (export process-exit-status? process-exit-status-kind process-exit-status-code
          process-exit-success? make-process-exit-status $wait-status->exit-status)
  (import (cspp chez) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; process status records
;;;;===----------------------------------------------------------------------===

  #|proc:process-exit-status?
The `process-exit-status?` procedure returns whether `object` is a status.
The `object` parameter is any Scheme object.
|#
  (define-record-type ($process-exit-status %make-process-exit-status
                                             process-exit-status?)
    (nongenerative)
    (fields (immutable kind $process-exit-status-kind)
            (immutable code $process-exit-status-code)))

  #|proc:process-exit-status-kind
  The `process-exit-status-kind` procedure returns the kind stored in `status`.
  The `status` parameter is a process exit-status record.
  |#
  (define process-exit-status-kind
    (lambda (status)
      (pcheck ([process-exit-status? status]) ($process-exit-status-kind status))))

  #|proc:process-exit-status-code
  The `process-exit-status-code` procedure returns the numeric code in `status`.
  The `status` parameter is a process exit-status record.
  |#
  (define process-exit-status-code
    (lambda (status)
      (pcheck ([process-exit-status? status]) ($process-exit-status-code status))))

  #|proc:make-process-exit-status
  The `make-process-exit-status` procedure returns an exit-status record.
  `kind` is `exit`, `signal`, or `stopped`; `code` is an exact nonnegative integer.
  For `exit` the code is an exit code; for the other kinds it is a signal number.
  |#
  (define make-process-exit-status
    (lambda (kind code)
      (pcheck ([(lambda (value) (memq value '(exit signal stopped))) kind] [natural? code])
        (%make-process-exit-status kind code))))

  #|proc:process-exit-success?
The `process-exit-success?` procedure returns whether `status` is exit code 0.
The `status` parameter is a process exit-status record.
|#
  (define process-exit-success?
    (lambda (status)
      (pcheck ([process-exit-status? status])
              (and (eq? 'exit (process-exit-status-kind status))
                   (= 0 (process-exit-status-code status))))))

  (define $wait-status->exit-status
    (lambda (raw)
      (cond [(fx= (fxand raw #x7f) 0)
             (make-process-exit-status 'exit (fxsrl raw 8))]
            [(fx= (fxand raw #x7f) #x7f)
             (make-process-exit-status 'stopped (fxsrl raw 8))]
            [else
             (make-process-exit-status 'signal (fxand raw #x7f))])))

)
