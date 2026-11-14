(library (cspp net transfer)
  (export transfer-policy?
          make-transfer-policy
          transfer-policy-resume
          transfer-policy-overwrite
          transfer-policy-chunk-size
          transfer-policy-progress
          default-transfer-policy
          transfer-report-progress!)
  (import (cspp chez)
          (cspp utils))

  (define valid-resume-mode?
    (lambda (value)
      (or (memq value '(never resume))
          (natural? value))))

  (define valid-overwrite-mode?
    (lambda (value)
      (and (symbol? value)
           (memq value '(error replace skip)))))

  (define transfer-positive-natural?
    (lambda (value)
      (and (natural? value) (positive? value))))

  #|record:transfer-policy
The `transfer-policy` record describes behavior shared by file transfer protocols.
The `resume` field is `never`, `resume`, or an exact nonnegative byte offset.
The `overwrite` field is `error`, `replace`, or `skip`.
The `chunk-size` field is the positive number of bytes processed by one transfer step.
The `progress` field is `#f` or a procedure with signature
`(protocol direction path completed-bytes total-bytes-or-#f) -> unspecified`.
|#
  (define-record-type (transfer-policy %make-transfer-policy transfer-policy?)
    (fields
     (immutable resume transfer-policy-resume)
     (immutable overwrite transfer-policy-overwrite)
     (immutable chunk-size transfer-policy-chunk-size)
     (immutable progress transfer-policy-progress)))

  #|proc:make-transfer-policy
The `make-transfer-policy` procedure constructs a file transfer policy.
The `resume` parameter is `never`, `resume`, or an exact nonnegative byte offset.
The `overwrite` parameter is `error`, `replace`, or `skip`.
The `chunk-size` parameter is the positive number of bytes processed by one transfer step.
The `progress` parameter is `#f` or a procedure with signature
`(protocol direction path completed-bytes total-bytes-or-#f) -> unspecified`.
The return value is a new immutable transfer policy.
|#
  (define-who make-transfer-policy
    (lambda (resume overwrite chunk-size progress)
      (pcheck ([valid-resume-mode? resume]
               [valid-overwrite-mode? overwrite]
               [transfer-positive-natural? chunk-size]
               [(lambda (value) (or (not value) (procedure? value))) progress])
              (%make-transfer-policy resume overwrite chunk-size progress))))

  #|value:default-transfer-policy
The `default-transfer-policy` value is the default file transfer policy.
The policy disables resume, rejects overwrites, uses 65536-byte chunks, and has no progress hook.
|#
  (define default-transfer-policy
    (make-transfer-policy 'never 'error 65536 #f))

  #|proc:transfer-report-progress!
The `transfer-report-progress!` procedure invokes the progress hook in `policy`, when present.
The `policy` parameter is a transfer policy.
The `protocol` parameter is a symbol naming the transfer protocol.
The `direction` parameter is `upload` or `download`.
The `path` parameter is the protocol-specific path being transferred.
The `completed-bytes` parameter is the exact nonnegative number of bytes transferred so far.
The `total-bytes-or-#f` parameter is the exact nonnegative total size or `#f` when unknown.
The return value is unspecified.
|#
  (define transfer-report-progress!
    (lambda (policy protocol direction path completed-bytes total-bytes-or-f)
      (pcheck ([transfer-policy? policy]
               [symbol? protocol]
               [(lambda (value) (memq value '(upload download))) direction]
               [string? path]
               [natural? completed-bytes]
               [(lambda (value) (or (not value) (natural? value))) total-bytes-or-f])
              (let ([progress (transfer-policy-progress policy)])
                (when progress
                  (progress protocol direction path completed-bytes total-bytes-or-f))))))
  )
