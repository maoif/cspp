(library (cspp private file)
  (export $file-type $file-mount-id)
  (import (chezscheme) (cspp utils))

  #|proc:$file-type
  Inspect validated filesystem pathname `path`, following its final link when `follow-link?`
  is true. Return an `FT_*` type symbol, or `#f` for a missing path or nondirectory parent.
  Other lookup failures raise an I/O filename error attributed to symbol `who`.
  |#
  (define $file-type
    (let ([inspect (foreign-procedure "cspp_file_type" (string boolean) scheme-object)])
      (lambda (who path follow-link?)
        (pcheck ([symbol? who] [string? path] [boolean? follow-link?])
          (let ([result (inspect path follow-link?)])
            (if (string? result)
                (raise (condition (make-who-condition who) (make-i/o-filename-error path)
                                  (make-message-condition result)))
                (case result
                  [(#f) #f]
                  [(#o140000) 'FT_socket] [(#o120000) 'FT_symlink]
                  [(#o100000) 'FT_regular] [(#o60000) 'FT_block]
                  [(#o40000) 'FT_dir] [(#o20000) 'FT_chardev]
                  [(#o10000) 'FT_fifo] [else 'FT_unknown])))))))

  #|proc:$file-mount-id
  Return the Linux mount ID for validated pathname `path`, following links, or `#f` when
  unavailable. Lookup failures raise an I/O filename error attributed to symbol `who`.
  |#
  (define $file-mount-id
    (let ([inspect (foreign-procedure "cspp_file_mount_id" (string) scheme-object)])
      (lambda (who path)
        (pcheck ([symbol? who] [string? path])
          (let ([result (inspect path)])
            (if (string? result)
                (raise (condition (make-who-condition who) (make-i/o-filename-error path)
                                  (make-message-condition result)))
                result)))))))
