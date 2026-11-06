(library (cspp system darwin)
  (export
          ;; platform predicate
          darwin?

          ;; Darwin system information stubs
          darwin-system-version
          darwin-memory-info
          darwin-filesystem-info)
  (import (cspp chez)
          (cspp system common)
          (only (cspp system info) system-platform)
          (cspp utils))

  #|proc:darwin?
  The `darwin?` procedure returns `#t` when the current platform is Darwin, otherwise `#f`.
  |#
  (define darwin?
    (lambda ()
      (eq? (system-platform) 'darwin)))

  #|proc:darwin-system-version
  The `darwin-system-version` procedure returns Darwin system version information.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who darwin-system-version
    (lambda ()
      (raise-system-unsupported who "Darwin system version is unsupported")))

  #|proc:darwin-memory-info
  The `darwin-memory-info` procedure returns Darwin memory information.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who darwin-memory-info
    (lambda ()
      (raise-system-unsupported who "Darwin memory information is unsupported")))

  #|proc:darwin-filesystem-info
  The `darwin-filesystem-info` procedure returns Darwin filesystem information for `path`.
  The `path` parameter is a string naming a path on the filesystem to query.
  This procedure is currently a stub and raises a system unsupported error.
  |#
  (define-who darwin-filesystem-info
    (lambda (path)
      (pcheck-who who ([string? path])
              (raise-system-unsupported who "Darwin filesystem information is unsupported"))))

  )
