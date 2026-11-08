(library (cspp system platform)
  (export unix? windows? darwin? linux? system-platform hostname system-hostname
          cpu-arch cpu-count system-machine)
  (import (cspp chez) (cspp private os) (cspp system common) (cspp utils))

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
  The `hostname` procedure returns the hostname of the current operating system as a string.
  Native query failures raise a system condition.
  |#
  (define hostname
    (let ([ffi (foreign-procedure "cspp_hostname" () ptr)])
      (lambda () (ffi-result-ref (ffi)))))

  #|proc:system-hostname
  The `system-hostname` procedure returns the hostname of the current operating system.
  It is an alias of `hostname`; native query failures raise a system condition.
  |#
  (define system-hostname hostname)

  #|proc:cpu-arch
  The `cpu-arch` procedure returns the instruction set architecture name of the current processor.
  The result is a string; native query failures raise a system condition.
  |#
  (define cpu-arch
    (let ([ffi (foreign-procedure "cspp_cpu_arch" () ptr)])
      (lambda () (ffi-result-ref (ffi)))))

  #|proc:cpu-count
  The `cpu-count` procedure returns the number of available logical processors.
  On Linux it counts CPUs allowed by the calling native thread's affinity mask.
  When affinity is unavailable, it uses the online CPU count, with a minimum fallback of one.
  This is a CPU count, not a measure of CPU-time quotas imposed by a container.
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
