(library (cspp system linux os-release)
  (export $read-os-release)
  (import (cspp chez) (cspp utils) (cspp string) (cspp list)
          (cspp system private))

  ;; OS-release permits shell quoting and escaping, but forbids shell expansion.
  (define $decode-value
    (lambda (value)
      (let* ([length (string-length value)]
             [quote-character (and (fx>= length 2)
                         (let ([first (string-ref value 0)])
                           (and (memv first '(#\" #\'))
                                (char=? first (string-ref value (fx- length 1)))
                                first)))]
             [start (if quote-character 1 0)]
             [end (if quote-character (fx- length 1) length)])
        (if (or (eqv? quote-character #\') (not (string-search value #\\)))
            (if quote-character (substring value start end) value)
            (let ([result (make-string (fx- end start))])
              (let loop ([index start] [output 0])
                (if (fx= index end)
                    (substring result 0 output)
                    (let* ([character (string-ref value index)]
                           [next (fx+ index 1)]
                           [escaped? (and (char=? character #\\) (fx< next end)
                                          (or (not quote-character)
                                              (memv (string-ref value next)
                                                    '(#\" #\\ #\$ #\`))))])
                      (string-set! result output
                        (if escaped? (string-ref value next) character))
                      (loop (if escaped? (fx+ next 1) next) (fx+ output 1))))))))))

  #|proc:$read-os-release
  The internal `$read-os-release` procedure reads release fields from NUL-free file path `path`.
  It returns an association list of symbol keys and decoded string values in file order.
  Blank and comment lines are ignored. Single quotes preserve literal contents; double quotes
  decode escapes for quotes, backslashes, dollar signs, and backticks. Unquoted escapes quote the
  following character. No variable expansion or command substitution is performed.
  |#
  (define $read-os-release
    (lambda (path)
      (pcheck ([$os-string? path])
        (call-with-input-file path
          (lambda (port)
            (let ([builder (make-list-builder)])
              (let loop ()
                (let ([line (get-line port)])
                  (if (eof-object? line)
                      (builder)
                      (let ([line (string-trim line)])
                        (unless (or (string-empty? line) (char=? #\# (string-ref line 0)))
                          (let ([equals (string-search line #\=)])
                            (when equals
                              (builder
                                (cons (string->symbol (substring line 0 equals))
                                      ($decode-value
                                        (substring line (fx+ equals 1) (string-length line))))))))
                        (loop)))))))))))

)
