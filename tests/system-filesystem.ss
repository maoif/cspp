(import (cspp))

(mat filesystem-path-contract

     ;; Error case: a NUL suffix must not query the filesystem of a different path.
     (guard (condition [else #t]) (filesystem-info "/\x0;ignored") #f)

     ;; Error case: native path conversion must preserve Unicode rather than truncate characters.
     (let ([path (format "/tmp/cspp-filesystem-~a-中文" (getpid))])
       (dynamic-wind
         (lambda () (mkdir path))
         (lambda () (string=? path (filesystem-info-path (filesystem-info path))))
         (lambda () (delete-directory path)))))

(mat filesystem-info-basic

     (let ([info (filesystem-info ".")])
       (and (filesystem-info? info)
            (string? (filesystem-info-path info))
            (integer? (filesystem-info-block-size info))
            (integer? (filesystem-info-blocks info))
            (integer? (filesystem-info-blocks-free info))
            (integer? (filesystem-info-blocks-available info))
            (or (not (filesystem-info-device info))
                (integer? (filesystem-info-device info)))
            (or (not (filesystem-info-type info))
                (symbol? (filesystem-info-type info)))
            (integer? (filesystem-info-files info))
            (integer? (filesystem-info-files-free info))
            (boolean? (filesystem-info-read-only? info))))

     (<= 0 (filesystem-total-bytes "."))
     (<= 0 (filesystem-free-bytes "."))
     (<= 0 (filesystem-available-bytes "."))

     (integer? (file-inode "."))

     ;; Error case: filesystem-info-inode is intentionally not exported.
     (guard (c [else #t])
       (eval 'filesystem-info-inode)
       #f))

(mat mounted-filesystems-basic

     (let ([mounts (mounted-filesystems)])
       (and (list? mounts)
            (exists (lambda (m)
                      (and (mounted-filesystem? m)
                           (string? (mounted-filesystem-source m))
                           (string? (mounted-filesystem-target m))
                           (string? (mounted-filesystem-type m))
                           (string? (mounted-filesystem-options m))
                           (string=? "/" (mounted-filesystem-target m))))
                    mounts))))
