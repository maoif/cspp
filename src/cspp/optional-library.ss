(library (cspp optional-library)
  (export optional-library-info?
          optional-library-info
          optional-library-name
          optional-library-available?
          optional-library-version
          optional-library-capabilities
          optional-library-error)
  (import (cspp chez)
          (cspp utils)
          (only (cspp private optional-library) ffi-optional-library-info))

  #|record:optional-library-info-record
The `optional-library-info-record` record is an immutable native dependency probe result.
Its name is a symbol, availability is a boolean, version is a string or `#f`, capabilities are
dependency-specific data, and error is a diagnostic string or `#f`.
|#
  (define-record-type (optional-library-info-record %make-optional-library-info
                                             optional-library-info?)
    (opaque #t)
    (fields (immutable name optional-library-name)
            (immutable available? optional-library-available?)
            (immutable version optional-library-version)
            (immutable capabilities optional-library-capabilities)
            (immutable error optional-library-error)))

  (define supported-optional-libraries
    '(openssl xxhash blake3 curl ssh websockets grpc zlib cares idn2 uuid))

  #|proc:optional-library-info
  The `optional-library-info` procedure reports the native dependency named by symbol `name`.
  It returns a record with build availability, version, capabilities, and an error diagnostic.
  Disabled libraries report no version or capabilities and identify build-time disablement.
  |#
  (define-who optional-library-info
    (lambda (name)
      (pcheck ([symbol? name])
              (unless (memq name supported-optional-libraries)
                (errorf who "supported optional libraries are ~s"
                        supported-optional-libraries))
              (let ([value (ffi-optional-library-info (symbol->string name))])
                (unless (and (vector? value) (= (vector-length value) 5))
                  (errorf who "optional library probe returned an invalid result"))
                (%make-optional-library-info
                 (vector-ref value 0)
                 (vector-ref value 1)
                 (vector-ref value 2)
                 (vector-ref value 3)
                 (vector-ref value 4))))))
  )
