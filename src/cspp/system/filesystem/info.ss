(library (cspp system filesystem info)
  (export filesystem-info?
          filesystem-info
          filesystem-info-path
          filesystem-info-device
          filesystem-info-type
          filesystem-info-block-size
          filesystem-info-blocks
          filesystem-info-blocks-free
          filesystem-info-blocks-available
          filesystem-info-files
          filesystem-info-files-free
          filesystem-info-read-only?
          filesystem-total-bytes filesystem-free-bytes filesystem-available-bytes)
  (import (cspp chez) (cspp system private) (cspp system common) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; filesystem information
;;;;===----------------------------------------------------------------------===

  #|proc:filesystem-info?
  The `filesystem-info?` procedure returns `#t` when its argument is a filesystem information
  record, otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:$filesystem-info
  Filesystem information records hold the queried path, identity, and capacity snapshot.
  The exported `filesystem-info?` predicate accepts any object and returns a boolean.
  Block counts use the reported fragment size; device/type fields may be `#f`.
  |#
  (define-record-type ($filesystem-info make-filesystem-info filesystem-info?)
    (nongenerative)
    (fields (immutable path $filesystem-info-path)
            (immutable device $filesystem-info-device)
            (immutable type $filesystem-info-type)
            (immutable block-size $filesystem-info-block-size)
            (immutable blocks $filesystem-info-blocks)
            (immutable blocks-free $filesystem-info-blocks-free)
            (immutable blocks-available $filesystem-info-blocks-available)
            (immutable files $filesystem-info-files)
            (immutable files-free $filesystem-info-files-free)
            (immutable read-only? $filesystem-info-read-only?)))

  #|proc:filesystem-info-path
  The `filesystem-info-path` procedure returns the queried path stored in a filesystem information
  record.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-path
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-path info))))

  #|proc:filesystem-info-device
  The `filesystem-info-device` procedure returns the device number for the queried path, or `#f`
  when unavailable.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-device
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-device info))))

  #|proc:filesystem-info-type
  The `filesystem-info-type` procedure returns the filesystem type symbol for the queried path, or
  `#f` when unavailable.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-type
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-type info))))

  #|proc:filesystem-info-block-size
  The `filesystem-info-block-size` procedure returns the fragment size used for capacity counts.
  This is the `statvfs` `f_frsize` value in bytes.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-block-size
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-block-size info))))

  #|proc:filesystem-info-blocks
  The `filesystem-info-blocks` procedure returns the total number of filesystem blocks.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-blocks
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-blocks info))))

  #|proc:filesystem-info-blocks-free
  The `filesystem-info-blocks-free` procedure returns the number of free filesystem blocks.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-blocks-free
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-blocks-free info))))

  #|proc:filesystem-info-blocks-available
  The `filesystem-info-blocks-available` procedure returns the number of filesystem blocks
  available to unprivileged users.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-blocks-available
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-blocks-available info))))

  #|proc:filesystem-info-files
  The `filesystem-info-files` procedure returns the total number of file nodes in the filesystem.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-files
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-files info))))

  #|proc:filesystem-info-files-free
  The `filesystem-info-files-free` procedure returns the number of free file nodes in the
  filesystem.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-files-free
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-files-free info))))

  #|proc:filesystem-info-read-only?
  The `filesystem-info-read-only?` procedure returns `#t` when the filesystem is mounted
  read-only, otherwise `#f`.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define-who filesystem-info-read-only?
    (lambda (info)
      (pcheck-who who ([filesystem-info? info]) ($filesystem-info-read-only? info))))

  (define $filesystem-info-ffi
    (foreign-procedure "cspp_filesystem_info" (string) ptr))

  (define $vector->filesystem-info
    (lambda (v)
      (make-filesystem-info (vector-ref v 0)
                            (vector-ref v 1)
                            (vector-ref v 2)
                            (vector-ref v 3)
                            (vector-ref v 4)
                            (vector-ref v 5)
                            (vector-ref v 6)
                            (vector-ref v 7)
                            (vector-ref v 8)
                            (vector-ref v 9))))

  #|proc:filesystem-info
  The `filesystem-info` procedure returns filesystem capacity and identity information for `path`.
  The `path` parameter is a string without NUL characters naming the path to query.
  Identity and capacity describe one resolved target even if `path` is replaced during the query.
  On Linux, querying metadata requires no read permission and does not open device or FIFO streams.
  Native filesystem access permits other Scheme threads to run and collect while it blocks.
  |#
  (define-who filesystem-info
    (lambda (path)
      (pcheck-who who ([$os-string? path])
              ($vector->filesystem-info (ffi-result-ref ($filesystem-info-ffi path))))))

  (define $filesystem-bytes
    (lambda (path block-selector)
      (let ([info (filesystem-info path)])
        (* (filesystem-info-block-size info) (block-selector info)))))

  #|proc:filesystem-total-bytes
  The `filesystem-total-bytes` procedure returns the total filesystem size in bytes for `path`.
  The `path` parameter is a string without NUL characters naming the path to query.
  |#
  (define-who filesystem-total-bytes
    (lambda (path)
      (pcheck-who who ([$os-string? path])
              ($filesystem-bytes path filesystem-info-blocks))))

  #|proc:filesystem-free-bytes
  The `filesystem-free-bytes` procedure returns the free filesystem size in bytes for `path`.
  The `path` parameter is a string without NUL characters naming the path to query.
  |#
  (define-who filesystem-free-bytes
    (lambda (path)
      (pcheck-who who ([$os-string? path])
              ($filesystem-bytes path filesystem-info-blocks-free))))

  #|proc:filesystem-available-bytes
  The `filesystem-available-bytes` procedure returns the filesystem size in bytes available to
  unprivileged users for `path`.
  The `path` parameter is a string without NUL characters naming the path to query.
  |#
  (define-who filesystem-available-bytes
    (lambda (path)
      (pcheck-who who ([$os-string? path])
              ($filesystem-bytes path filesystem-info-blocks-available))))

)
