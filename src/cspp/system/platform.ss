(library (cspp system platform)
  (export unix? windows? darwin? linux? system-platform hostname system-hostname
          cpu-arch cpu-count system-machine)
  (import (cspp chez) (cspp private os) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; platform info
;;;;===----------------------------------------------------------------------===

  #|proc:linux?
  The `linux?` procedure returns `#t` when the current operating system is Linux,
  otherwise `#f`.
  |#
  (define linux?
    (let ([native (foreign-procedure "cspp_linux_p" () int)])
      (lambda () (= 1 (native)))))

  #|proc:hostname
  The `hostname` procedure returns the hostname of the current operating system.
  |#
  (define hostname
    (foreign-procedure "cspp_hostname" () ptr))

  #|proc:system-hostname
  The `system-hostname` procedure returns the hostname of the current operating system.
  |#
  (define system-hostname hostname)

  #|proc:cpu-arch
  The `cpu-arch` procedure returns the instruction set architecture name of the current processor.
  |#
  (define cpu-arch
    (foreign-procedure "cspp_cpu_arch" () ptr))

  #|proc:cpu-count
  The `cpu-count` procedure returns the number of available logical processors.
  |#
  (define cpu-count
    (foreign-procedure "cspp_cpu_count" () int))

  #|proc:system-machine
  The `system-machine` procedure returns the machine type symbol reported by Chez Scheme.
  |#
  (define system-machine
    (lambda ()
      (machine-type)))

  #|proc:system-platform
  The `system-platform` procedure returns a symbol naming the current platform family.
  |#
  (define system-platform
    (lambda ()
      (cond [(windows?) 'windows]
            [(darwin?) 'darwin]
            [(linux?) 'linux]
            [else 'unknown])))

  )
