(import (cspp))

;; The parent fixture has an inherited group outside this namespace's caller groups.
;; Run separately because dropping supplementary groups must not affect the test runner.
(define root (car (command-line-arguments)))
(unless (= 0 ((foreign-procedure "setgroups" (uptr uptr) int) 0 0))
  (error 'file-mode-test "cannot drop supplementary groups"))

(define source (path-build root "source"))
(define child (path-build source "child"))
(mkdir source #o755)
(mkdir child #o755)
(write-string (path-build child "payload") "source stays")
(unless (= #o2755 (file-mode child))
  (error 'file-mode-test "fixture did not inherit set-group-ID"))

(for-each
 (lambda (copy name)
   (let ([unchanged (path-build root (string-append name "-unchanged"))]
         [changed (path-build root (string-append name "-changed"))])
     (mkdir unchanged #o755)
     (mkdir changed #o700)
     (copy child unchanged)
     (unless (= #o2755 (file-mode unchanged))
       (error 'file-mode-test "redundant mode copy lost set-group-ID" name))
     ;; Error case: an unretainable mode must raise an I/O error rather than claim success.
     (let ([failed?
            (guard (failure
                    [(i/o-filename-error? failure)
                     (unless (string=? changed (i/o-error-filename failure))
                       (raise failure))
                     #t])
              (copy child changed)
              #f)])
       (unless (or failed? (= #o2755 (file-mode changed)))
         (error 'file-mode-test "mode copy silently lost set-group-ID" name)))))
 (list file-copymode file-copymeta) '("copymode" "copymeta"))

(let* ([dest (path-build root "destination")]
       [copied (path-build dest "child")])
  (mkdir dest #o755)
  ;; Error case: failed restoration must leave the complete source in place.
  (let ([failed? (guard (failure [(i/o-filename-error? failure) #t])
                   (file-move source dest)
                   #f)])
    (unless (if failed?
                (and (file-directory? source)
                     (= #o2755 (file-mode child))
                     (string=? "source stays" (read-string (path-build child "payload"))))
                (and (not (file-exists? source #f))
                     (= #o2755 (file-mode copied))
                     (string=? "source stays" (read-string (path-build copied "payload")))))
      (error 'file-mode-test "move lost metadata or removed its source after failure"))))
