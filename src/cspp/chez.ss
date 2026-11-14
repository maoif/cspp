;; The Chez gateway provides primitives without loading CSPP's native runtime.
(library (cspp chez)
  (export (rename (getenv $getenv)))
  (import (chezscheme))

  (export (import (except (chezscheme)
                          sleep getenv putenv
                          string-for-each
                          ;; file.ss
                          file-access-time file-change-time file-modification-time))))
