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
  (import (cspp chez) (cspp string) (cspp system common) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; filesystem information
;;;;===----------------------------------------------------------------------===

  #|proc:filesystem-info?
  The `filesystem-info?` procedure returns `#t` when its argument is a filesystem information
  record, otherwise `#f`.
  The `object` parameter is the object to test.
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
  (define filesystem-info-path
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-path info))))

  #|proc:filesystem-info-device
  The `filesystem-info-device` procedure returns the device number for the queried path, or `#f`
  when unavailable.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-device
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-device info))))

  #|proc:filesystem-info-type
  The `filesystem-info-type` procedure returns the filesystem type symbol for the queried path, or
  `#f` when unavailable.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-type
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-type info))))

  #|proc:filesystem-info-block-size
  The `filesystem-info-block-size` procedure returns the filesystem block size in bytes.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-block-size
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-block-size info))))

  #|proc:filesystem-info-blocks
  The `filesystem-info-blocks` procedure returns the total number of filesystem blocks.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-blocks
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-blocks info))))

  #|proc:filesystem-info-blocks-free
  The `filesystem-info-blocks-free` procedure returns the number of free filesystem blocks.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-blocks-free
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-blocks-free info))))

  #|proc:filesystem-info-blocks-available
  The `filesystem-info-blocks-available` procedure returns the number of filesystem blocks
  available to unprivileged users.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-blocks-available
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-blocks-available info))))

  #|proc:filesystem-info-files
  The `filesystem-info-files` procedure returns the total number of file nodes in the filesystem.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-files
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-files info))))

  #|proc:filesystem-info-files-free
  The `filesystem-info-files-free` procedure returns the number of free file nodes in the
  filesystem.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-files-free
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-files-free info))))

  #|proc:filesystem-info-read-only?
  The `filesystem-info-read-only?` procedure returns `#t` when the filesystem is mounted
  read-only, otherwise `#f`.
  The `info` parameter is a filesystem information record returned by `filesystem-info`.
  |#
  (define filesystem-info-read-only?
    (lambda (info)
      (pcheck ([filesystem-info? info]) ($filesystem-info-read-only? info))))

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
  The `path` parameter is a string naming a path on the filesystem to query.
  |#
  (define filesystem-info
    (lambda (path)
      (pcheck ([string? path])
              ($vector->filesystem-info (ffi-result-ref ($filesystem-info-ffi path))))))

  (define $filesystem-bytes
    (lambda (path block-selector)
      (let ([info (filesystem-info path)])
        (* (filesystem-info-block-size info) (block-selector info)))))

  #|proc:filesystem-total-bytes
  The `filesystem-total-bytes` procedure returns the total filesystem size in bytes for `path`.
  The `path` parameter is a string naming a path on the filesystem to query.
  |#
  (define filesystem-total-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks))))

  #|proc:filesystem-free-bytes
  The `filesystem-free-bytes` procedure returns the free filesystem size in bytes for `path`.
  The `path` parameter is a string naming a path on the filesystem to query.
  |#
  (define filesystem-free-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks-free))))

  #|proc:filesystem-available-bytes
  The `filesystem-available-bytes` procedure returns the filesystem size in bytes available to
  unprivileged users for `path`.
  The `path` parameter is a string naming a path on the filesystem to query.
  |#
  (define filesystem-available-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks-available))))

)
