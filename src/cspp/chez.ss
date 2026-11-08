;; This file is used to mask out those unwanted names in (chezscheme).
(library (cspp chez)
  (export (rename
           ;; os.ss
           (sleep $sleep)))
  (import (except (chezscheme) getenv putenv)
          (only (cspp utils) pcheck))

  (export getenv putenv)

  (export (import (except (chezscheme)
                          sleep getenv putenv
                          string-for-each
                          ;; file.ss
                          file-access-time file-change-time file-modification-time)))


  (define $environment-name?
    (lambda (name)
      (and (string? name) (> (string-length name) 0)
           (let loop ([index 0])
             (or (= index (string-length name))
                 (and (not (memv (string-ref name index) '(#\nul #\=)))
                      (loop (+ index 1))))))))

  (define $environment-string?
    (lambda (value)
      (and (string? value)
           (let loop ([index 0])
             (or (= index (string-length value))
                 (and (not (char=? (string-ref value index) #\nul))
                      (loop (+ index 1))))))))

  (define $environment-result
    (lambda (who result)
      (if (eq? (vector-ref result 0) 'ok)
          (vector-ref result 1)
          (errorf who "~a" (vector-ref result 3)))))

  #|proc:getenv
  The `getenv` procedure returns the string value of environment variable `name`, or `#f`.
  `name` is a string without NUL characters. The result is a fresh snapshot.
  CSPP environment reads, writes, and inherited process snapshots share a lock.
  Direct libc or Chez environment mutations outside CSPP require caller synchronization.
  |#
  (define getenv
    (lambda (name)
      (pcheck ([$environment-string? name])
        ($environment-result 'getenv
          ((foreign-procedure "cspp_environment_get" (utf-8) ptr) name)))))

  #|proc:putenv
  The `putenv` procedure sets environment variable `name` to string `value`.
  `name` is nonempty and contains neither NUL nor `=`; `value` contains no NUL.
  It returns unspecified values. Writes synchronize with CSPP readers and process snapshots.
  Direct libc or Chez environment mutations outside CSPP require caller synchronization.
  |#
  (define putenv
    (lambda (name value)
      (pcheck ([$environment-name? name] [$environment-string? value])
        ($environment-result 'putenv
          ((foreign-procedure "cspp_environment_set" (utf-8 utf-8) ptr) name value)))))
)
