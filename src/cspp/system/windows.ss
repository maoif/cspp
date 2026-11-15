(library (cspp system windows)
  (export
          ;; platform predicate
          windows?

          ;; Windows system information stubs
          windows-system-version
          windows-memory-info
          windows-filesystem-info)
  (import (cspp chez)
          (cspp system private common)
          (only (cspp system info) system-platform)
          (cspp utils))

  #|proc:windows?
  The `windows?` procedure returns `#t` when the current platform is Windows, otherwise `#f`.
  |#
  (define windows?
    (lambda ()
      (eq? (system-platform) 'windows)))

  #|proc:windows-system-version
  The `windows-system-version` procedure raises a system unsupported error.
  Windows system version queries are unsupported; the procedure does not return a value.
  |#
  (define-who windows-system-version
    (lambda ()
      (raise-system-unsupported who "Windows system version is unsupported")))

  #|proc:windows-memory-info
  The `windows-memory-info` procedure raises a system unsupported error.
  Windows memory queries are unsupported; the procedure does not return a value.
  |#
  (define-who windows-memory-info
    (lambda ()
      (raise-system-unsupported who "Windows memory information is unsupported")))

  #|proc:windows-filesystem-info
  The `windows-filesystem-info` procedure validates `path` and raises a system unsupported error.
  The `path` parameter is a string naming a path on the filesystem to query.
  Windows filesystem queries are unsupported; the procedure does not return a value.
  |#
  (define-who windows-filesystem-info
    (lambda (path)
      (pcheck-who who ([string? path])
              (raise-system-unsupported who "Windows filesystem information is unsupported"))))

  )
