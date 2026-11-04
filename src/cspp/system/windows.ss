(library (cspp system windows)
  (export
          ;; platform predicate
          windows?

          ;; Windows system information stubs
          windows-system-version
          windows-memory-info
          windows-filesystem-info)
  (import (cspp chez)
          (cspp system common)
          (only (cspp system info) system-platform)
          (cspp utils))

  #|proc:windows?
  The `windows?` procedure returns `#t` when the current platform is Windows, otherwise `#f`.
  |#
  (define windows?
    (lambda ()
      (eq? (system-platform) 'windows)))

  #|proc:windows-system-version
  The `windows-system-version` procedure returns Windows system version information.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who windows-system-version
    (lambda ()
      (raise-system-unsupported who "Windows system version is unsupported")))

  #|proc:windows-memory-info
  The `windows-memory-info` procedure returns Windows memory information.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who windows-memory-info
    (lambda ()
      (raise-system-unsupported who "Windows memory information is unsupported")))

  #|proc:windows-filesystem-info
  The `windows-filesystem-info` procedure returns Windows filesystem information for `path`.
  The `path` parameter is a string naming a path on the filesystem to query.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who windows-filesystem-info
    (lambda (path)
      (pcheck ([string? path])
              (raise-system-unsupported who "Windows filesystem information is unsupported"))))

  )
