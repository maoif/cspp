(library (cspp private file)
  (export $file-type $file-mount-id $file-view-key $file-view-seen?)
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
                result))))))

  #|proc:$file-view-key
  Combine the mount ID of directory pathname `path` with its device/inode list `identity`.
  Return a key distinguishing mount views of the same directory, with `#f` as its first
  element when mount IDs are unavailable. Lookup errors are attributed to symbol `who`.
  |#
  (define $file-view-key
    (lambda (who path identity)
      (pcheck ([symbol? who] [string? path] [list? identity])
        (cons ($file-mount-id who path) identity))))

  #|proc:$file-view-seen?
  Return whether directory view key `view` occurs in ancestor key list `ancestors`.
  A repeated key without a mount ID cannot prove a cycle and raises an I/O filename error
  for pathname `path`, attributed to symbol `who`, instead of silently skipping a view.
  |#
  (define $file-view-seen?
    (lambda (who path view ancestors)
      (pcheck ([symbol? who] [string? path] [pair? view] [list? ancestors])
        (and (member view ancestors)
             (if (car view) #t
                 (raise (condition (make-who-condition who) (make-i/o-filename-error path)
                                   (make-message-condition
                                    "mount identity is unavailable for alias")))))))))
