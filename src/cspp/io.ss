(library (cspp io)
  (export print println displayln
          fprint fprintln)
  (import (cspp chez)
          (cspp private common)
          (only (cspp utils) pcheck))



  #|doc
  Display a string either to the current output port
  or a given textual output port, with a newline.
  |#
  (define displayln
    (case-lambda
      [(obj) (display obj) (display "\n")]
      [(obj port)
       (pcheck ([port? port])
               (display obj port)
               (display "\n" port))]))


  #|doc
  Contruct a formatted string using the format string `fmt` and objects `args`,
  and print the string to the current output port, with a newline in the end.
  |#
  (define println
    (lambda (fmt . args)
      (pcheck ([string? fmt])
              (apply format #t fmt args)
              (format #t "\n"))))


  #|doc
  Contruct a formatted string using the format string `fmt` and objects `args`,
  and print the string to the current output port.
  |#
  (define print
    (lambda (fmt . args)
      (pcheck ([string? fmt])
              (apply format #t fmt args))))


  #|doc
  Contruct a formatted string using the format string `fmt` and objects `args`,
  and print the string to the given output port `port`, with a newline in the end.
  |#
  (define fprintln
    (lambda (port fmt . args)
      (pcheck ([port? port] [string? fmt])
              (apply format port fmt args)
              (format port "\n"))))


  #|doc
  Contruct a formatted string using the format string `fmt` and objects `args`,
  and print the string to the given output port `port`.
  |#
  (define fprint
    (lambda (port fmt . args)
      (pcheck ([port? port] [string? fmt])
              (apply format port fmt args))))

  )
