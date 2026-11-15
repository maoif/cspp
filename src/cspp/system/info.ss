(library (cspp system info)
  (export
          ;; platform predicates
          unix?
          windows?
          darwin?
          linux?
          system-platform

          ;; host information
          hostname
          system-hostname
          cpu-arch
          cpu-count
          system-machine)
  (import (cspp system platform)))
