(library (cspp parser private)
  (export define-parser-record-writer define-checked-record-type
          unicode-scalar-value?
          parser-decode-bytes parser-read-text-file
          make-parser-digit-converter parser-decimal-source->flonum)
  (import (cspp chez) (cspp file) (cspp list) (cspp utils))

  #|macro:define-parser-record-writer
  The `define-parser-record-writer` macro registers a labeled writer for `record-type`. `tag` names
  the printed record, and each `label` and `accessor` pair prints one record field.
  |#
  (define-syntax define-parser-record-writer
    (syntax-rules ()
      [(_ record-type tag ([label accessor] ...))
       (define writer-registration
         (record-writer
          (type-descriptor record-type)
          (lambda (record port write-value)
            (display "#[" port)
            (display (quote tag) port)
            (for-each
             (lambda (field-label field-value)
               (display " " port)
               (display field-label port)
               (display ": " port)
               (write-value field-value port))
             (list (quote label) ...)
             (list (accessor record) ...))
            (display "]" port))))]))

  #|macro:define-checked-record-type
  Defines an immutable record named `internal-name`, its internal constructor `internal-maker`
  and predicate `internal-predicate`, a labeled writer using `tag`, and checked public bindings.
  `public-maker` checks each `field` with its `field-predicate` (`Any -> Boolean`).
  Optional `relationship-predicate` (`Vector -> Boolean`) checks the fields in declaration order
  after their individual checks. `public-predicate` recognizes the record; each `public-accessor`
  checks the record before calling its `internal-accessor`. Supplied field contents are retained.
  |#
  (define-syntax define-checked-record-type
    (syntax-rules ()
      [(_ internal-name public-maker public-predicate tag internal-maker internal-predicate
          relationship-predicate
          ([field public-accessor internal-accessor field-predicate] ...))
       (begin
         (define-record-type (internal-name internal-maker internal-predicate)
           (fields (immutable field internal-accessor) ...))
         (define-parser-record-writer internal-name tag
           ([field internal-accessor] ...))
         (define public-maker
           (lambda (field ...)
             (let ([relationship-fields (vector field ...)])
               (pcheck ([field-predicate field] ...
                        [relationship-predicate relationship-fields])
                       (internal-maker field ...)))))
         (define public-predicate
           (lambda (object)
             (pcheck () (internal-predicate object))))
         (define public-accessor
           (lambda (record)
             (pcheck ([internal-predicate record])
                     (internal-accessor record)))) ...)]
      [(_ internal-name public-maker public-predicate tag internal-maker internal-predicate
          ([field public-accessor internal-accessor field-predicate] ...))
       (begin
         (define-record-type (internal-name internal-maker internal-predicate)
           (fields (immutable field internal-accessor) ...))
         (define-parser-record-writer internal-name tag
           ([field internal-accessor] ...))
         (define public-maker
           (lambda (field ...)
             (pcheck ([field-predicate field] ...)
                     (internal-maker field ...))))
         (define public-predicate
           (lambda (object)
             (pcheck () (internal-predicate object))))
         (define public-accessor
           (lambda (record)
             (pcheck ([internal-predicate record])
                     (internal-accessor record)))) ...)]))

  #|proc:unicode-scalar-value?
  Returns whether arbitrary object `value` is an exact integer Unicode scalar value: from zero
  through U+10FFFF, excluding the surrogate range U+D800 through U+DFFF.
  |#
  (define unicode-scalar-value?
    (lambda (value)
      (pcheck ()
              (and (integer? value)
                   (exact? value)
                   (<= 0 value #x10ffff)
                   (not (<= #xd800 value #xdfff))))))

  #|proc:make-parser-digit-converter
  The `make-parser-digit-converter` procedure takes symbol `who` for diagnostics and exact
  integer `radix` of at least two. It returns a procedure `(List -> Integer)` accepting a proper
  list of exact integer digits from zero through `radix - 1`. It validates digits in list order
  and combines balanced halves into an exact integer; an empty list returns zero.
  |#
  (define (make-parser-digit-converter who radix)
    (pcheck ([symbol? who]
             [(lambda (value) (and (integer? value) (exact? value) (>= value 2))) radix])
            (lambda (d*)
              (pcheck ([list? d*])
                      ;; Combine balanced halves instead of copying a growing bignum per digit.
                      ;; Returning the unused tail avoids copying or repeatedly splitting lists.
                      (define convert
                        (lambda (digits count)
                          (if (fx<= count 14)
                              (let loop ([digits digits] [count count] [number 0])
                                (if (fxzero? count)
                                    (values number digits)
                                    (let ([digit (car digits)])
                                      (unless (and (integer? digit) (exact? digit)
                                                   (<= 0 digit) (< digit radix))
                                        (errorf who "bad digit ~a for base ~a" digit radix))
                                      (loop (cdr digits) (fx1- count) (+ digit (* number radix))))))
                              (let* ([left-count (fxdiv count 2)]
                                     [right-count (fx- count left-count)])
                                (let-values ([(left remaining) (convert digits left-count)])
                                  (let-values ([(right tail) (convert remaining right-count)])
                                    ;; Validate both halves before skipping a zero leading half.
                                    (values (if (zero? left) right
                                                (+ (* left (expt radix right-count)) right))
                                            tail)))))))
                      (let-values ([(number tail) (convert d* (length d*))]) number)))))

  (define decimal-digits->integer
    (make-parser-digit-converter 'parser-decimal-source->flonum 10))

  #|proc:parser-decimal-source->flonum
  The `parser-decimal-source->flonum` procedure converts string `source` to a binary64 flonum.
  The calling format grammar must validate a decimal spelling with optional sign, decimal point
  and exponent, without digit separators. It combines exact significand digits in balanced
  halves and rounds one exact value, preserving signed zero, overflow and underflow.
  |#
  (define parser-decimal-source->flonum
    (lambda (source)
      (pcheck ([string? source])
              (let* ([length (string-length source)]
                     [negative-source? (char=? (string-ref source 0) #\-)]
                     [start (if (memv (string-ref source 0) '(#\+ #\-)) 1 0)]
                     ;; All source digits together can cancel at most this exponent bound.
                     [limit (+ (* 8 length) 2048)]
                     [digits (make-list-builder)])
                (let scan ([index start] [fractional? #f] [fractional-count 0])
                  (if (or (fx= index length)
                          (memv (string-ref source index) '(#\e #\E)))
                      (let* ([exponent
                              (if (fx= index length)
                                  0
                                  (let* ([first (fx1+ index)]
                                         [negative-exponent?
                                          (char=? (string-ref source first) #\-)]
                                         [digit-start
                                          (if (memv (string-ref source first) '(#\+ #\-))
                                              (fx1+ first) first)])
                                    (let loop ([index digit-start] [value 0])
                                      (if (fx= index length)
                                          (if negative-exponent? (- value) value)
                                          (loop
                                           (fx1+ index)
                                           (if (> value limit)
                                               value
                                               (+ (* value 10)
                                                  (fx- (char->integer
                                                        (string-ref source index)) 48))))))))]
                             [significand (decimal-digits->integer (digits))]
                             [exponent (- exponent fractional-count)]
                             ;; Source length bounds decimal order even with leading zeros.
                             [magnitude
                              (cond [(zero? significand) 0.0]
                                    [(> exponent 309) +inf.0]
                                    [(< (+ exponent (fx- length start)) -324) 0.0]
                                    [else
                                     (inexact
                                      (if (negative? exponent)
                                          (/ significand (expt 10 (- exponent)))
                                          (* significand (expt 10 exponent))))])])
                        (if negative-source? (- magnitude) magnitude))
                      (let ([character (string-ref source index)])
                        (if (char=? character #\.)
                            (scan (fx1+ index) #t fractional-count)
                            (begin
                              (digits (fx- (char->integer character) 48))
                              (scan (fx1+ index) fractional?
                                    (if fractional? (fx1+ fractional-count)
                                        fractional-count)))))))))))

  #|proc:parser-decode-bytes
  The `parser-decode-bytes` procedure decodes bytevector `bytes` strictly using `encoding`,
  one of `utf-8`, `utf-16le`, or `utf-16be`. It returns a string without newline conversion.
  Every encoded scalar is content, including an initial U+FEFF. Invalid encodings raise an
  I/O decoding condition. Callers must remove any stream signature before calling this procedure.
  |#
  (define-who parser-decode-bytes
    (lambda (bytes encoding)
      (pcheck ([bytevector? bytes] [symbol? encoding])
              (case encoding
                [(utf-8)
                 (let ([text (bytevector->string
                              bytes (make-transcoder (utf-8-codec) (eol-style none)
                                                     (error-handling-mode raise)))])
                   ;; Chez consumes one UTF-8 signature even in a data string. Restore it.
                   (if (and (>= (bytevector-length bytes) 3)
                            (= #xef (bytevector-u8-ref bytes 0))
                            (= #xbb (bytevector-u8-ref bytes 1))
                            (= #xbf (bytevector-u8-ref bytes 2)))
                       (string-append (string #\xfeff) text)
                       text))]
                [(utf-16le utf-16be)
                 ;; Prefix a code unit so the codec cannot treat content as a signature or
                 ;; change endianness. Removing the decoded prefix preserves all content.
                 (let ([prefixed (make-bytevector (+ 2 (bytevector-length bytes)) 0)]
                       [endian (if (eq? encoding 'utf-16le)
                                   (endianness little) (endianness big))])
                   (bytevector-u16-set! prefixed 0 32 endian)
                   (bytevector-copy! bytes 0 prefixed 2 (bytevector-length bytes))
                   (let ([text (bytevector->string
                                prefixed
                                (make-transcoder (utf-16-codec endian) (eol-style none)
                                                 (error-handling-mode raise)))])
                     (substring text 1 (string-length text))))]
                [else (errorf who "unsupported parser text encoding: ~a" encoding)]))))

  #|proc:parser-read-text-file
  The `parser-read-text-file` procedure reads the UTF-8 file at string `path` and returns its
  text without newline conversion. One initial UTF-8 signature is ignored. File and malformed
  UTF-8 decoding conditions propagate; invalid bytes are never replaced with new characters.
  |#
  (define parser-read-text-file
    (lambda (path)
      (pcheck ([string? path])
              (bytevector->string
               (read-u8vec path)
               (make-transcoder (utf-8-codec) (eol-style none)
                                (error-handling-mode raise))))))

  )
