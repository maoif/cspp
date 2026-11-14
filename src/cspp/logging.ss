#!chezscheme
(library (cspp logging)
  (export)
  (import (cspp chez))

  (export (import (cspp logging level)
                  (cspp logging formatter)
                  (cspp logging sink)
                  (cspp logging logger)
                  (cspp logging macros))))
