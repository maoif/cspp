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
