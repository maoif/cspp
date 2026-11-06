(library (cspp system filesystem mounts)
  (export mounted-filesystem?
          mounted-filesystems
          mounted-filesystem-source
          mounted-filesystem-target
          mounted-filesystem-type
          mounted-filesystem-options)
  (import (cspp chez) (cspp string) (cspp system common) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; mounted filesystems
;;;;===----------------------------------------------------------------------===

  #|proc:mounted-filesystem?
  The `mounted-filesystem?` procedure returns `#t` when its argument is a mounted filesystem
  record, otherwise `#f`.
  The `object` parameter is the object to test.
  |#
  #|record:$mounted-filesystem
  Mounted-filesystem records hold source, target, type, and option strings from Linux mountinfo.
  The exported `mounted-filesystem?` predicate accepts any object and returns a boolean.
  Strings describe a snapshot of the calling process's mount namespace.
  |#
  (define-record-type ($mounted-filesystem make-mounted-filesystem mounted-filesystem?)
    (nongenerative)
    (fields (immutable source $mounted-filesystem-source)
            (immutable target $mounted-filesystem-target)
            (immutable type $mounted-filesystem-type)
            (immutable options $mounted-filesystem-options)))

  #|proc:mounted-filesystem-source
  The `mounted-filesystem-source` procedure returns the source device or pseudo-device of a
  mounted filesystem record.
  The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
  |#
  (define-who mounted-filesystem-source
    (lambda (mount)
      (pcheck-who who ([mounted-filesystem? mount]) ($mounted-filesystem-source mount))))

  #|proc:mounted-filesystem-target
  The `mounted-filesystem-target` procedure returns the mount target path of a mounted filesystem
  record.
  The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
  |#
  (define-who mounted-filesystem-target
    (lambda (mount)
      (pcheck-who who ([mounted-filesystem? mount]) ($mounted-filesystem-target mount))))

  #|proc:mounted-filesystem-type
  The `mounted-filesystem-type` procedure returns the filesystem type string of a mounted
  filesystem record.
  The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
  |#
  (define-who mounted-filesystem-type
    (lambda (mount)
      (pcheck-who who ([mounted-filesystem? mount]) ($mounted-filesystem-type mount))))

  #|proc:mounted-filesystem-options
  The `mounted-filesystem-options` procedure returns mount and superblock options separated by
  a space, as reported by Linux mountinfo.
  The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
  |#
  (define-who mounted-filesystem-options
    (lambda (mount)
      (pcheck-who who ([mounted-filesystem? mount]) ($mounted-filesystem-options mount))))

  (define $mountinfo-unescape
    (lambda (field)
      (list->string
       (let ([n (string-length field)])
         (let loop ([i 0] [chars '()])
           (if (fx= i n)
               (reverse chars)
               (if (and (char=? (string-ref field i) #\\)
                        (fx<= (fx+ i 3) (fx- n 1)))
                   (let ([escape (substring field (fx+ i 1) (fx+ i 4))])
                     (cond [(string=? escape "040") (loop (fx+ i 4) (cons #\space chars))]
                           [(string=? escape "011") (loop (fx+ i 4) (cons #\tab chars))]
                           [(string=? escape "012") (loop (fx+ i 4) (cons #\newline chars))]
                           [(string=? escape "134") (loop (fx+ i 4) (cons #\\ chars))]
                           [else (loop (fx+ i 1) (cons (string-ref field i) chars))]))
                   (loop (fx+ i 1) (cons (string-ref field i) chars)))))))))

  (define $parse-mountinfo-line
    (lambda (line)
      (let* ([fields (string-split line #\space)]
             [tail (memp (lambda (field) (string=? field "-")) fields)])
        (and tail
             (let ([tail-count (length tail)])
               (and (fx<= 4 tail-count)
                    (fx<= 6 (fx- (length fields) tail-count))
                    (let* ([mount-options (list-ref fields 5)]
                           [super-options (cadddr tail)]
                           [options (if (string=? super-options "")
                                        mount-options
                                        (string-append mount-options " " super-options))])
                      (make-mounted-filesystem ($mountinfo-unescape (caddr tail))
                                               ($mountinfo-unescape (list-ref fields 4))
                                               (cadr tail)
                                               options))))))))

  (define $read-mounted-filesystems
    (lambda (path)
      (call-with-input-file path
        (lambda (port)
          (let loop ([mounts '()])
            (let ([line (get-line port)])
              (if (eof-object? line)
                  (reverse mounts)
                  (let ([mount ($parse-mountinfo-line line)])
                    (loop (if mount (cons mount mounts) mounts))))))))))

  #|proc:mounted-filesystems
  The `mounted-filesystems` procedure returns a list of mounted filesystem records for the current
  process mount namespace.
  It reads Linux `/proc/self/mountinfo`; an absent file yields an empty list.
  |#
  (define mounted-filesystems
    (lambda ()
      (let ([mountinfo "/proc/self/mountinfo"])
        (if (file-exists? mountinfo)
            ($read-mounted-filesystems mountinfo)
            '()))))

)
