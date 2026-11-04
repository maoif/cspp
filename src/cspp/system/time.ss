(library (cspp system time)
  (export sleep milisleep nanosleep sleep-seconds sleep-milliseconds sleep-nanoseconds)
  (import (cspp chez) (cspp utils))

;;;;===----------------------------------------------------------------------===
;;;; time
;;;;===----------------------------------------------------------------------===

  #|proc:sleep
  The `sleep` procedure pauses the current thread for `seconds` seconds.
  `seconds` is an exact nonnegative integer. The return value is unspecified.
  |#
  (define sleep
    (lambda (seconds)
      (pcheck ([natural? seconds])
        ($sleep (make-time 'time-duration 0 seconds)))))

  #|proc:milisleep
  The `milisleep` procedure pauses the current thread for `milliseconds` milliseconds.
  `milliseconds` is an exact nonnegative integer. The return value is unspecified.
  |#
  (define milisleep
    (lambda (milliseconds)
      (pcheck ([natural? milliseconds])
        ($sleep (make-time 'time-duration
                  (* 1000000 (modulo milliseconds 1000))
                  (quotient milliseconds 1000))))))

  #|proc:nanosleep
  The `nanosleep` procedure pauses the current thread for `nanoseconds` nanoseconds.
  `nanoseconds` is an exact nonnegative integer. The return value is unspecified.
  |#
  (define nanosleep
    (lambda (nanoseconds)
      (pcheck ([natural? nanoseconds])
        ($sleep (make-time 'time-duration
                  (modulo nanoseconds 1000000000)
                  (quotient nanoseconds 1000000000))))))

  #|proc:sleep-seconds
  The `sleep-seconds` procedure pauses the current thread for `seconds` seconds.
  The `seconds` parameter is a natural number of seconds.
  |#
  (define sleep-seconds sleep)

  #|proc:sleep-milliseconds
  The `sleep-milliseconds` procedure pauses the current thread for `milliseconds` milliseconds.
  The `milliseconds` parameter is a natural number of milliseconds.
  |#
  (define sleep-milliseconds milisleep)

  #|proc:sleep-nanoseconds
  The `sleep-nanoseconds` procedure pauses the current thread for `nanoseconds` nanoseconds.
  The `nanoseconds` parameter is a natural number of nanoseconds.
  |#
  (define sleep-nanoseconds nanosleep)


)
