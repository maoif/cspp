(library (cspp uuid)
  (export make-uuid make-uuid-from-time make-uuid-from-md5 make-uuid-from-sha1
          uuid?
          uuid->string uuid->string-upcase uuid->string-downcase
          string->uuid
          uuid->bytevector bytevector->uuid
          uuid-time
          uuid=? uuid<? uuid<=? uuid>? uuid>=?)
  (import (cspp private optional-library) (cspp chez)
          (cspp utils)
          (cspp private common))


  #|record:uuid
  The `uuid` record stores a UUID as a 16-byte bytevector in its `data` field.
  |#
  (define-record-type (uuid mk-uuid uuid?)
    (opaque #f)
    (sealed #t)
    (fields data))

  #|proc:ffi-generate-uuid
  The `ffi-generate-uuid` procedure calls the native uuid operation `cspp_generate_uuid`.
  It returns a new 16-byte UUID bytevector.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-generate-uuid
    (let ([native (foreign-procedure "cspp_generate_uuid" () ptr)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-generate-uuid 'uuid)
                (native )))))
  #|proc:ffi-generate-uuid-time
  The `ffi-generate-uuid-time` procedure calls the native uuid operation
  `cspp_generate_uuid_time`.
  It returns a vector containing a safe-generation boolean and the UUID bytevector.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-generate-uuid-time
    (let ([native (foreign-procedure "cspp_generate_uuid_time" () ptr)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-generate-uuid-time 'uuid)
                (native )))))
  #|proc:ffi-generate-uuid-md5
  The `ffi-generate-uuid-md5` procedure calls the native uuid operation
  `cspp_generate_uuid_md5`.
  Parameters `uuid-ns-bv`, `name` are passed to the native operation in that order.
  `uuid-ns-bv` is the UUID namespace bytevector.
  `name` is the name to hash in the UUID namespace.
  It returns the 16-byte UUID bytevector derived using MD5.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-generate-uuid-md5
    (let ([native (foreign-procedure "cspp_generate_uuid_md5" (ptr string) ptr)])
      (lambda (uuid-ns-bv name)
        (pcheck ([string? name])
                (require-optional-library 'ffi-generate-uuid-md5 'uuid)
                (native uuid-ns-bv name)))))
  #|proc:ffi-generate-uuid-sha1
  The `ffi-generate-uuid-sha1` procedure calls the native uuid operation
  `cspp_generate_uuid_sha1`.
  Parameters `uuid-ns-bv`, `name` are passed to the native operation in that order.
  `uuid-ns-bv` is the UUID namespace bytevector.
  `name` is the name to hash in the UUID namespace.
  It returns the 16-byte UUID bytevector derived using SHA1.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-generate-uuid-sha1
    (let ([native (foreign-procedure "cspp_generate_uuid_sha1" (ptr string) ptr)])
      (lambda (uuid-ns-bv name)
        (pcheck ([string? name])
                (require-optional-library 'ffi-generate-uuid-sha1 'uuid)
                (native uuid-ns-bv name)))))
  #|proc:ffi-uuid-to-string
  The `ffi-uuid-to-string` procedure calls the native uuid operation `cspp_uuid_to_string`.
  Parameters `uuid-bv` are passed to the native operation in that order.
  `uuid-bv` is the UUID bytevector.
  It returns the 36-character hexadecimal UUID representation.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-uuid-to-string
    (let ([native (foreign-procedure "cspp_uuid_to_string" (ptr) ptr)])
      (lambda (uuid-bv)
        (pcheck ()
                (require-optional-library 'ffi-uuid-to-string 'uuid)
                (native uuid-bv)))))
  #|proc:ffi-uuid-to-string-upcase
  The `ffi-uuid-to-string-upcase` procedure calls the native uuid operation
  `cspp_uuid_to_string_upcase`.
  Parameters `uuid-bv` are passed to the native operation in that order.
  `uuid-bv` is the UUID bytevector.
  It returns the 36-character uppercase hexadecimal UUID representation.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-uuid-to-string-upcase
    (let ([native (foreign-procedure "cspp_uuid_to_string_upcase" (ptr) ptr)])
      (lambda (uuid-bv)
        (pcheck ()
                (require-optional-library 'ffi-uuid-to-string-upcase 'uuid)
                (native uuid-bv)))))
  #|proc:ffi-uuid-to-string-downcase
  The `ffi-uuid-to-string-downcase` procedure calls the native uuid operation
  `cspp_uuid_to_string_downcase`.
  Parameters `uuid-bv` are passed to the native operation in that order.
  `uuid-bv` is the UUID bytevector.
  It returns the 36-character lowercase hexadecimal UUID representation.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-uuid-to-string-downcase
    (let ([native (foreign-procedure "cspp_uuid_to_string_downcase" (ptr) ptr)])
      (lambda (uuid-bv)
        (pcheck ()
                (require-optional-library 'ffi-uuid-to-string-downcase 'uuid)
                (native uuid-bv)))))
  #|proc:ffi-uuid-compare
  The `ffi-uuid-compare` procedure calls the native uuid operation `cspp_uuid_compare`.
  Parameters `uuid-bv1`, `uuid-bv2` are passed to the native operation in that order.
  `uuid-bv1` is a Scheme object.
  `uuid-bv2` is a Scheme object.
  It returns a negative integer, zero, or a positive integer according to UUID order.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-uuid-compare
    (let ([native (foreign-procedure "cspp_uuid_compare" (ptr ptr) int)])
      (lambda (uuid-bv1 uuid-bv2)
        (pcheck ()
                (require-optional-library 'ffi-uuid-compare 'uuid)
                (native uuid-bv1 uuid-bv2)))))
  #|proc:ffi-uuid-time
  The `ffi-uuid-time` procedure calls the native uuid operation `cspp_uuid_time`.
  Parameters `uuid-bv` are passed to the native operation in that order.
  `uuid-bv` is the UUID bytevector.
  It returns a bytevector containing native `timeval` seconds followed by microseconds.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-uuid-time
    (let ([native (foreign-procedure "cspp_uuid_time" (ptr) ptr)])
      (lambda (uuid-bv)
        (pcheck ()
                (require-optional-library 'ffi-uuid-time 'uuid)
                (native uuid-bv)))))
  #|proc:ffi-string-to-uuid
  The `ffi-string-to-uuid` procedure calls the native uuid operation `cspp_string_to_uuid`.
  Parameters `text` are passed to the native operation in that order.
  `text` is the input text.
  It returns the parsed UUID bytevector, or #f when the representation is invalid.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-string-to-uuid
    (let ([native (foreign-procedure "cspp_string_to_uuid" (string) ptr)])
      (lambda (text)
        (pcheck ([string? text])
                (require-optional-library 'ffi-string-to-uuid 'uuid)
                (native text)))))


  #|proc:make-uuid
  The `make-uuid` procedure takes no parameters and returns a random UUID.
  |#
  (define make-uuid
    (case-lambda
      [()
       (mk-uuid (ffi-generate-uuid))]))


  #|proc:make-uuid-from-time
  The `make-uuid-from-time` procedure takes no parameters and returns two values: a boolean
  indicating whether generation was safe, and a time-based UUID.
  |#
  (define make-uuid-from-time
    (case-lambda
      [()
       (let ([vec (ffi-generate-uuid-time)])
         (values (vector-ref vec 0) (mk-uuid (vector-ref vec 1))))]))


  #|proc:make-uuid-from-md5
  The `make-uuid-from-md5` procedure returns an MD5-based UUID using UUID `namespace-uuid`
  and string `name-string` as the name within that namespace.
  |#
  (define make-uuid-from-md5
    (lambda (namespace-uuid name-string)
      (pcheck ([uuid? namespace-uuid] [string? name-string])
              (mk-uuid (ffi-generate-uuid-md5 (uuid-data namespace-uuid) name-string)))))


  #|proc:make-uuid-from-sha1
  The `make-uuid-from-sha1` procedure returns a SHA1-based UUID using UUID `namespace-uuid`
  and string `name-string` as the name within that namespace.
  |#
  (define make-uuid-from-sha1
    (lambda (namespace-uuid name-string)
      (pcheck ([uuid? namespace-uuid] [string? name-string])
              (mk-uuid (ffi-generate-uuid-sha1 (uuid-data namespace-uuid) name-string)))))


  #|proc:uuid->string
  The `uuid->string` procedure returns the 36-character hexadecimal representation of UUID `uuid`.
  |#
  (define uuid->string
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string (uuid-data uuid)))))


  #|proc:uuid->string-upcase
  The `uuid->string-upcase` procedure returns the uppercase hexadecimal string for UUID `uuid`.
  |#
  (define uuid->string-upcase
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string-upcase (uuid-data uuid)))))


  #|proc:uuid->string-downcase
  The `uuid->string-downcase` procedure returns the lowercase hexadecimal string for UUID `uuid`.
  |#
  (define uuid->string-downcase
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string-downcase (uuid-data uuid)))))


  #|proc:string->uuid
  The `string->uuid` procedure parses UUID representation string `str` and returns a UUID,
  or `#f` when the representation is invalid.
  |#
  (define string->uuid
    (lambda (str)
      (pcheck ([string? str])
              (let ([res (ffi-string-to-uuid str)])
                (and res (mk-uuid res))))))


  #|proc:uuid->bytevector
  The `uuid->bytevector` procedure returns a copy of UUID `uuid`'s 16-byte representation.
  |#
  (define uuid->bytevector
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (bytevector-copy (uuid-data uuid)))))


  #|proc:bytevector->uuid
  Convert a 16-byte bytevector to a UUID.
  |#
  (define bytevector->uuid
    (lambda (bv)
      (pcheck ([bytevector? bv])
              (if (fx= 16 (bytevector-length bv))
                  (mk-uuid (bytevector-copy bv))
                  (errorf 'bytevector->uuid "expected 16-byte bytevector, got length ~a"
                          (bytevector-length bv))))))


  #|doc
  Make a copy of the given UUID object.
  |#
  (define uuid-copy
    (lambda (uuid)
      (mk-uuid (bytevector-copy (uuid-data uuid)))))


  #|proc:uuid-time
  The `uuid-time` procedure returns the creation time of time-based UUID `uuid` as a UTC time.
  |#
  (define uuid-time
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (let ([bv (ffi-uuid-time (uuid-data uuid))])
                (make-time 'time-utc
                           (fx* (bytevector-s64-native-ref bv 8) 1000)
                           (bytevector-s64-native-ref bv 0))))))


  #|proc:uuid=?
  The `uuid=?` procedure compares UUIDs `first-uuid` and `second-uuid` in native UUID order.
  It returns `#t` when the first UUID is equal to the second UUID, and `#f` otherwise.
  |#
  (define uuid=?
    (lambda (first-uuid second-uuid)
      (pcheck ([uuid? first-uuid second-uuid])
              (fx= 0 (ffi-uuid-compare (uuid-data first-uuid) (uuid-data second-uuid))))))


  #|proc:uuid<?
  The `uuid<?` procedure compares UUIDs `first-uuid` and `second-uuid` in native UUID order.
  It returns `#t` when the first UUID is less to the second UUID, and `#f` otherwise.
  |#
  (define uuid<?
    (lambda (first-uuid second-uuid)
      (pcheck ([uuid? first-uuid second-uuid])
              (fx< (ffi-uuid-compare (uuid-data first-uuid) (uuid-data second-uuid)) 0))))


  #|proc:uuid<=?
  The `uuid<=?` procedure compares UUIDs `first-uuid` and `second-uuid` in native UUID order.
  It returns `#t` when the first UUID is less or equal to the second UUID, and `#f` otherwise.
  |#
  (define uuid<=?
    (lambda (first-uuid second-uuid)
      (pcheck ([uuid? first-uuid second-uuid])
              (fx<= (ffi-uuid-compare (uuid-data first-uuid) (uuid-data second-uuid)) 0))))


  #|proc:uuid>?
  The `uuid>?` procedure compares UUIDs `first-uuid` and `second-uuid` in native UUID order.
  It returns `#t` when the first UUID is greater to the second UUID, and `#f` otherwise.
  |#
  (define uuid>?
    (lambda (first-uuid second-uuid)
      (pcheck ([uuid? first-uuid second-uuid])
              (fx> (ffi-uuid-compare (uuid-data first-uuid) (uuid-data second-uuid)) 0))))


  #|proc:uuid>=?
  The `uuid>=?` procedure compares UUIDs `first-uuid` and `second-uuid` in native UUID order.
  It returns `#t` when the first UUID is greater or equal to the second UUID, and `#f` otherwise.
  |#
  (define uuid>=?
    (lambda (first-uuid second-uuid)
      (pcheck ([uuid? first-uuid second-uuid])
              (fx>= (ffi-uuid-compare (uuid-data first-uuid) (uuid-data second-uuid)) 0))))


  (record-writer (type-descriptor uuid)
                 (lambda (r p wr)
                   (display "#[uuid " p)
                   (display (ffi-uuid-to-string (uuid-data r)) p)
                   (display "]" p)))

  (record-type-equal-procedure (type-descriptor uuid)
                               (lambda (x1 x2 =?)
                                 (uuid=? x1 x2)))
  )
