(library (cspp system info)
  (export
          ;; time
          sleep
          milisleep
          nanosleep
          sleep-seconds
          sleep-milliseconds
          sleep-nanoseconds

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
  (import (only (cspp utils) sleep milisleep nanosleep
                sleep-seconds sleep-milliseconds sleep-nanoseconds) (cspp system platform)))
