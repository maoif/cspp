(library (cspp system linux)
  (export
          ;; platform predicate
          linux?

          ;; Linux filesystem helpers
          linux-procfs-mounted?
          linux-mounted-filesystems
          linux-filesystem-info

          ;; Linux signal helpers
          linux-signal-list
          linux-send-signal

          ;; Linux system information
          linux-system-uptime
          linux-memory-info
          linux-load-average
          linux-os-release)
  (import (cspp chez)
          (cspp string)
          (only (cspp file) read-lines)
          (cspp system private)
          (cspp system linux os-release)
          (only (cspp system signal values) $signal-input?)
          (cspp system common)
          (cspp system filesystem)
          (only (cspp system info) system-platform)
          (cspp system signal)
          (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; helpers
;;;;===----------------------------------------------------------------------===

  (define $unsupported
    (lambda (who message)
      (raise-system-unsupported who message)))

  (define $ensure-linux
    (lambda (who)
      (unless (linux?)
        ($unsupported who "Linux system operation is unsupported on this platform"))))

  (define $ensure-linux-procfs
    (lambda (who)
      ($ensure-linux who)
      (unless (linux-procfs-mounted?)
        ($unsupported who "Linux procfs is not mounted"))))

  (define $read-first-line
    (lambda (path)
      (call-with-input-file path
        (lambda (port)
          (let ([line (get-line port)])
            (if (eof-object? line) "" line))))))

  (define $space-fields
    (let ([nonempty? (p/not string-empty?)])
      (lambda (text) (filter nonempty? (string-split text #\space)))))

  (define $line-key/value
    (lambda (line)
      (let ([colon (string-search line #\:)])
        (and colon
             (cons (substring line 0 colon)
                   (string-trim (substring line (fx+ colon 1) (string-length line))))))))

;;;;===----------------------------------------------------------------------===
;;;; platform
;;;;===----------------------------------------------------------------------===

  #|proc:linux?
  The `linux?` procedure returns `#t` when the current platform is Linux, otherwise `#f`.
  |#
  (define linux?
    (lambda ()
      (eq? (system-platform) 'linux)))

  #|proc:linux-procfs-mounted?
  The `linux-procfs-mounted?` procedure returns `#t` when Linux procfs appears to be mounted at
  `/proc`, otherwise `#f`.
  |#
  (define linux-procfs-mounted?
    (lambda ()
      (and (linux?)
           (file-exists? "/proc/self/mountinfo")
           (let loop ([mounts (mounted-filesystems)])
             (cond [(null? mounts) #f]
                   [(and (string=? "proc" (mounted-filesystem-type (car mounts)))
                         (string=? "/proc" (mounted-filesystem-target (car mounts))))
                    #t]
                   [else (loop (cdr mounts))])))))

;;;;===----------------------------------------------------------------------===
;;;; filesystem and signal wrappers
;;;;===----------------------------------------------------------------------===

  #|proc:linux-mounted-filesystems
  The `linux-mounted-filesystems` procedure returns the list of Linux mounted filesystem records for
  the current process mount namespace.
  |#
  (define-who linux-mounted-filesystems
    (lambda ()
      ($ensure-linux who)
      (mounted-filesystems)))

  #|proc:linux-filesystem-info
  The `linux-filesystem-info` procedure returns filesystem capacity and identity information for
  `path` on Linux.
  The `path` parameter is a string naming a path on the filesystem to query.
  |#
  (define-who linux-filesystem-info
    (lambda (path)
      (pcheck ([string? path])
              ($ensure-linux who)
              (filesystem-info path))))

  #|proc:linux-signal-list
  The `linux-signal-list` procedure returns the known Linux signal records.
  |#
  (define-who linux-signal-list
    (lambda ()
      ($ensure-linux who)
      (signal-list)))

  #|proc:linux-send-signal
  The `linux-send-signal` procedure sends `sig` to process `pid` on Linux.
  The `pid` parameter is an exact positive integer process ID.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  |#
  (define-who linux-send-signal
    (lambda (pid sig)
      (pcheck ([$positive-pid? pid] [$signal-input? sig])
              ($ensure-linux who)
              (send-signal pid sig))))

;;;;===----------------------------------------------------------------------===
;;;; procfs and os-release
;;;;===----------------------------------------------------------------------===

  #|proc:linux-system-uptime
  The `linux-system-uptime` procedure returns the system uptime in seconds from `/proc/uptime`.
  |#
  (define-who linux-system-uptime
    (lambda ()
      ($ensure-linux-procfs who)
      (let* ([fields ($space-fields ($read-first-line "/proc/uptime"))]
             [uptime (and (pair? fields) (string->number (car fields)))])
        (or uptime
            ($unsupported who "could not parse /proc/uptime")))))

  #|proc:linux-memory-info
  The `linux-memory-info` procedure returns memory information from `/proc/meminfo` as an
  association list.
  Each association has a symbol key from `/proc/meminfo` and a numeric value, usually measured in
  KiB by Linux.
  |#
  (define-who linux-memory-info
    (lambda ()
      ($ensure-linux-procfs who)
      (let loop ([lines (read-lines "/proc/meminfo")] [out '()])
        (cond [(null? lines) (reverse out)]
              [else
               (let* ([entry ($line-key/value (car lines))]
                      [fields (and entry ($space-fields (cdr entry)))]
                      [value (and (pair? fields) (string->number (car fields)))])
                 (loop (cdr lines)
                       (if (and entry value)
                           (cons (cons (string->symbol (car entry)) value) out)
                           out)))]))))

  #|proc:linux-load-average
  The `linux-load-average` procedure returns the one-, five-, and fifteen-minute load averages from
  `/proc/loadavg`.
  |#
  (define-who linux-load-average
    (lambda ()
      ($ensure-linux-procfs who)
      (let* ([fields ($space-fields ($read-first-line "/proc/loadavg"))]
             [one (and (pair? fields) (string->number (car fields)))]
             [fields (and (pair? fields) (cdr fields))]
             [five (and (pair? fields) (string->number (car fields)))]
             [fields (and (pair? fields) (cdr fields))]
             [fifteen (and (pair? fields) (string->number (car fields)))])
        (if (and one five fifteen)
            (list one five fifteen)
            ($unsupported who "could not parse /proc/loadavg")))))

  #|proc:linux-os-release
  The `linux-os-release` procedure returns operating-system release fields as an association list.
  Keys are symbols from `/etc/os-release` or `/usr/lib/os-release`, and values are strings.
  Shell-style quotes and backslash escapes are decoded without expanding variables or commands.
  |#
  (define-who linux-os-release
    (lambda ()
      ($ensure-linux who)
      (cond [(file-exists? "/etc/os-release")
             ($read-os-release "/etc/os-release")]
            [(file-exists? "/usr/lib/os-release")
             ($read-os-release "/usr/lib/os-release")]
            [else
             ($unsupported who "Linux os-release file was not found")])))

  )
