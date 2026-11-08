(library (cspp parser private)
  (export define-parser-record-writer parser-decode-bytes parser-read-text-file)
  (import (cspp chez) (cspp file) (cspp utils))

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
