(import (cspp))

;; Run in an isolated user/mount namespace. The caller owns fixture cleanup.
(define root (car (command-line-arguments)))
(define source (path-build root "source"))
(define alias (path-build root "alias"))
(define mount-bind (foreign-procedure "mount" (string string string unsigned-long void*) int))
(define unmount (foreign-procedure "umount2" (string int) int))
(mkdirs (path-build source "child"))
(mkdir alias)
(write-string (path-build source "child/payload") "source stays")

(for-each
 (lambda (mounted-source)
   (unless (= 0 (mount-bind mounted-source alias "" #x1000 0))
     (error 'file-bind-test "cannot mount alias"))
   (dynamic-wind
     void
     (lambda ()
       (for-each
        (lambda (operation)
          (let ([destination (path-build alias "nested")] [calls 0])
            ;; Error case: a source or descendant alias must reject before any changes.
            (let ([failed?
                   (guard (failure [(error? failure) #t])
                     (operation source destination
                                (lambda (from to)
                                  (set! calls (+ calls 1))
                                  ;; Bound accidental recursive copying if validation regresses.
                                  (when (> calls 3) (error 'file-bind-test "recursive copy"))
                                  (file-copy from to)))
                     #f)])
              (unless (and failed? (= calls 0) (not (file-exists? destination #f))
                           (string=? "source stays"
                                     (read-string (path-build source "child/payload"))))
                (error 'file-bind-test "source alias bypassed containment" mounted-source)))))
        (list file-copytree file-move)))
     (lambda ()
       (unless (= 0 (unmount alias 2)) (error 'file-bind-test "cannot unmount alias")))))
 (list source (path-build source "child")))

;; The same inode can have different children in two mount views. Exercise both view orders.
(define backing (path-build root "backing"))
(define target (path-build root "target"))
(define view-a (path-build source "view-a"))
(define view-b (path-build source "view-b"))
(mkdirs (path-build backing "x"))
(mkdir target)
(mkdir view-a)
(mkdir view-b)
(write-string (path-build target "payload") "keep")

(for-each
 (lambda (selected-view)
   (let ([mounts '()])
     (define bind
       (lambda (from to)
         (unless (= 0 (mount-bind from to "" #x1000 0))
           (error 'file-bind-test "cannot mount distinct view"))
         (set! mounts (cons to mounts))))
     (dynamic-wind
       void
       (lambda ()
         (bind backing view-a)
         (bind backing view-b)
         (bind target (path-build selected-view "x"))
         (bind target alias)
         (let ([destination (path-build alias "nested")] [calls 0])
           ;; Error case: a visited inode must not hide children unique to another mount view.
           (let ([failed?
                  (guard (failure [(error? failure) #t])
                    (file-copytree source destination
                      (lambda (from to)
                        (set! calls (+ calls 1))
                        (when (> calls 3) (error 'file-bind-test "recursive mount view"))
                        (file-copy from to)))
                    #f)])
             (unless (and failed? (= calls 0) (not (file-exists? destination #f)))
               (error 'file-bind-test "distinct mount view bypassed containment")))))
       (lambda ()
         (for-each
          (lambda (path)
            (unless (= 0 (unmount path 2)) (error 'file-bind-test "cannot unmount view")))
          mounts)))))
 (list view-a view-b))

;; Nonrecursive bind mounts expose finite, distinct views of the same backing directory.
(define finite (path-build root "finite"))
(define finite-backing (path-build finite "backing"))
(define finite-view (path-build finite "view"))
(define finite-child (path-build finite-view "child"))
(define finite-leaf (path-build finite-child "child"))
(mkdirs (path-build finite-backing "child"))
(mkdir finite-view)
(file-symlink "child/link" (path-build finite-backing "link"))
(write-string (path-build finite-backing "child/link") "finite payload")

(let ([mounts '()])
  (define bind
    (lambda (from to)
      (unless (= 0 (mount-bind from to "" #x1000 0))
        (error 'file-bind-test "cannot mount finite view"))
      (set! mounts (cons to mounts))))
  (dynamic-wind
    void
    (lambda ()
      (bind finite-backing finite-view)
      (bind finite-backing finite-child)
      (let ([link (path-build finite-view "link")]
            [leaf (path-build finite-leaf "link")]
            [copied (path-build finite "copy")]
            ;; Hard links must stay within one mount, even when devices match.
            [linked (path-build finite-leaf "hard-link")])
        (unless (and (same-file? finite-view finite-child)
                     (string=? "finite payload" (read-string link))
                     (same-file? leaf (readlink2 link #t)))
          (error 'file-bind-test "finite symlink chain was rejected"))
        (let-values ([(text parent) (readlink link #t)])
          (unless (same-file? leaf (path-build parent text))
            (error 'file-bind-test "recursive readlink lost its mount context")))
        (file-copy link copied)
        (file-link link linked)
        (unless (and (string=? "finite payload" (read-string copied))
                     (same-file? leaf linked))
          (error 'file-bind-test "finite link chain cannot be copied or linked")))
      (for-each
       (lambda (top-down?)
         (let ([visited (make-list-builder)])
           (walk-files (lambda (directory directories files) (visited directory) #t)
                       finite-view #t top-down?)
           (unless (equal? (sort string<? (visited))
                           (sort string<? (list finite-view finite-child finite-leaf)))
             (error 'file-bind-test "walk skipped a finite mount view")))
         (let ([iterator (fs->iter finite-view #t top-down?)])
           (dynamic-wind
             void
             (lambda ()
               (unless (member (path-build finite-leaf "link") (iter->list iterator))
                 (error 'file-bind-test "iterator skipped a finite mount view")))
             (lambda () (iter-finalize! iterator)))))
       '(#f #t))
      (for-each
       (lambda (flavor)
         (unless (equal? (glob* flavor (path-build finite-view "**") #f #t 'empty)
                         (glob* flavor (path-build finite-view "**") #t #t 'empty))
           (error 'file-bind-test "glob skipped a finite mount view" flavor)))
       '(unix windows)))
    (lambda ()
      (for-each
       (lambda (path)
         (unless (= 0 (unmount path 2)) (error 'file-bind-test "cannot unmount finite view")))
       mounts))))

;; A missing destination on another filesystem must use the metadata-preserving fallback.
(define move-source (path-build root "move-source"))
(define move-volume (path-build root "move-volume"))
(define move-target (path-build move-volume "moved"))
(define stamp (make-time 'time-utc 123000000 1000000000))
(mkdirs (path-build move-source "child/deeper"))
(write-string (path-build move-source "child/deeper/payload") "move payload")
(mkdir move-volume)
(for-each (lambda (path) (file-touch path stamp))
          (list move-source (path-build move-source "child")
                (path-build move-source "child/deeper")))
(unless (= 0 (mount-bind "" move-volume "tmpfs" 0 0))
  (error 'file-bind-test "cannot mount move destination"))
(dynamic-wind
  void
  (lambda ()
    (file-move move-source move-target)
    (unless (and (not (file-exists? move-source #f))
                 (for-all (lambda (path) (time=? stamp (file-access-time path)))
                          (list move-target (path-build move-target "child")
                                (path-build move-target "child/deeper")))
                 (string=? "move payload"
                           (read-string (path-build move-target "child/deeper/payload"))))
      (error 'file-bind-test "cross-device move lost original directory timestamps")))
  (lambda ()
    (unless (= 0 (unmount move-volume 2))
      (error 'file-bind-test "cannot unmount move destination"))))
