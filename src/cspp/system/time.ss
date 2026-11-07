(library (cspp system time)
  (export sleep milisleep nanosleep sleep-seconds sleep-milliseconds sleep-nanoseconds)
  (import (cspp chez) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; time
;;;;===----------------------------------------------------------------------===

  (define $maximum-sleep-seconds
    ((foreign-procedure "cspp_sleep_max_seconds" () unsigned-64)))

  (define $sleep-duration?
    (lambda (duration units-per-second)
      (and (natural? duration)
           (<= (quotient duration units-per-second) $maximum-sleep-seconds))))

  #|proc:sleep
  The `sleep` procedure pauses the current thread for `seconds` seconds.
  `seconds` is an exact nonnegative integer within the native `time_t` range.
  Larger durations raise an argument error. The return value is unspecified.
  |#
  (define-who sleep
    (lambda (seconds)
      (pcheck-who who ([(lambda (value) ($sleep-duration? value 1)) seconds])
        ($sleep (make-time 'time-duration 0 seconds)))))

  #|proc:milisleep
  The `milisleep` procedure pauses the current thread for `milliseconds` milliseconds.
  `milliseconds` is an exact nonnegative integer whose whole seconds fit native `time_t`.
  Larger durations raise an argument error. The return value is unspecified.
  |#
  (define-who milisleep
    (lambda (milliseconds)
      (pcheck-who who ([(lambda (value) ($sleep-duration? value 1000)) milliseconds])
        ($sleep (make-time 'time-duration
                  (* 1000000 (modulo milliseconds 1000))
                  (quotient milliseconds 1000))))))

  #|proc:nanosleep
  The `nanosleep` procedure pauses the current thread for `nanoseconds` nanoseconds.
  `nanoseconds` is an exact nonnegative integer whose whole seconds fit native `time_t`.
  Larger durations raise an argument error. The return value is unspecified.
  |#
  (define-who nanosleep
    (lambda (nanoseconds)
      (pcheck-who who ([(lambda (value) ($sleep-duration? value 1000000000)) nanoseconds])
        ($sleep (make-time 'time-duration
                  (modulo nanoseconds 1000000000)
                  (quotient nanoseconds 1000000000))))))

  #|proc:sleep-seconds
  The `sleep-seconds` procedure pauses the current thread for `seconds` seconds.
  The `seconds` parameter is a natural number of seconds.
  It is an alias of `sleep`; the return value is unspecified.
  |#
  (define sleep-seconds sleep)

  #|proc:sleep-milliseconds
  The `sleep-milliseconds` procedure pauses the current thread for `milliseconds` milliseconds.
  The `milliseconds` parameter is a natural number of milliseconds.
  It is an alias of `milisleep`; the return value is unspecified.
  |#
  (define sleep-milliseconds milisleep)

  #|proc:sleep-nanoseconds
  The `sleep-nanoseconds` procedure pauses the current thread for `nanoseconds` nanoseconds.
  The `nanoseconds` parameter is a natural number of nanoseconds.
  It is an alias of `nanosleep`; the return value is unspecified.
  |#
  (define sleep-nanoseconds nanosleep)


)
