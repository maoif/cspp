(library (cspp net grpc reflection)
  (export grpc-reflection-registry? make-grpc-reflection-registry
          grpc-reflection-register-file! grpc-register-reflection!)
  (import (cspp chez)
          (only (cspp utils) pcheck)
          (cspp format protobuf)
          (cspp net grpc))

  (define grpc-reflection-method
    "/grpc.reflection.v1.ServerReflection/ServerReflectionInfo")

  #|record:grpc-reflection-registry
The `grpc-reflection-registry` record owns synchronized descriptor indexes used by reflection.
Its hash tables index immutable file bytes by file, dependency, symbol, service, and extension
names. Registration updates those indexes while holding the registry's mutex.
|#
  (define-record-type (grpc-reflection-registry %make-grpc-reflection-registry
                                                grpc-reflection-registry?)
    (sealed #t)
    (opaque #f)
    (fields (immutable files grpc-reflection-registry-files)
            (immutable dependencies grpc-reflection-registry-dependencies)
            (immutable symbols grpc-reflection-registry-symbols)
            (immutable services grpc-reflection-registry-services)
            (immutable extensions grpc-reflection-registry-extensions)
            (immutable mutex grpc-reflection-registry-mutex)))

  #|proc:make-grpc-reflection-registry
The `make-grpc-reflection-registry` procedure creates an empty synchronized descriptor registry.
The return value is a new gRPC reflection registry.
|#
  (define make-grpc-reflection-registry
    (lambda ()
      (%make-grpc-reflection-registry
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-mutex))))

  (define descriptor-string
    (lambda (field)
      (protobuf-decode-string (protobuf-wire-field-value field))))

  (define qualified-name
    (lambda (prefix name)
      (if (zero? (string-length prefix)) name (string-append prefix "." name))))

  (define extension-key
    (lambda (containing-type number)
      (let ([type (if (and (positive? (string-length containing-type))
                           (char=? (string-ref containing-type 0) #\.))
                      (substring containing-type 1 (string-length containing-type))
                      containing-type)])
        (string-append type ":" (number->string number)))))

  (define scan-extension
    (lambda (bytes filename extensions)
      (let ([decoder (make-protobuf-decoder bytes)] [containing-type #f] [number #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(2) (set! containing-type (descriptor-string field))]
                [(3) (set! number (protobuf-wire-field-value field))])
              (loop))))
        (when (and containing-type number)
          (hashtable-set! extensions (extension-key containing-type number) filename)))))

  (define scan-message
    (lambda (bytes prefix filename symbols extensions)
      (let ([decoder (make-protobuf-decoder bytes)] [name #f] [nested* '()] [enum* '()]
            [extension* '()])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (descriptor-string field))]
                [(3) (set! nested* (cons (protobuf-wire-field-value field) nested*))]
                [(4) (set! enum* (cons (protobuf-wire-field-value field) enum*))]
                [(6) (set! extension* (cons (protobuf-wire-field-value field) extension*))])
              (loop))))
        (when name
          (let ([full-name (qualified-name prefix name)])
            (hashtable-set! symbols full-name filename)
            (for-each
             (lambda (nested) (scan-message nested full-name filename symbols extensions))
             (reverse nested*))
            (for-each
             (lambda (enum)
               (let ([enum-decoder (make-protobuf-decoder enum)])
                 (let enum-loop ()
                   (let ([field (protobuf-decoder-next-field enum-decoder)])
                     (when field
                       (when (= (protobuf-wire-field-number field) 1)
                         (hashtable-set!
                          symbols (qualified-name full-name (descriptor-string field)) filename))
                       (enum-loop))))))
             (reverse enum*))
            (for-each
             (lambda (extension) (scan-extension extension filename extensions))
             (reverse extension*)))))))

  (define scan-file-descriptor!
    (lambda (bytes filename dependencies symbols services extensions)
      (let ([decoder (make-protobuf-decoder bytes)] [package ""] [dependency* '()]
            [message* '()] [enum* '()] [service* '()] [extension* '()])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(2) (set! package (descriptor-string field))]
                [(3) (set! dependency* (cons (descriptor-string field) dependency*))]
                [(4) (set! message* (cons (protobuf-wire-field-value field) message*))]
                [(5) (set! enum* (cons (protobuf-wire-field-value field) enum*))]
                [(6) (set! service* (cons (protobuf-wire-field-value field) service*))]
                [(7) (set! extension* (cons (protobuf-wire-field-value field) extension*))])
              (loop))))
        (hashtable-set! dependencies filename (reverse dependency*))
        (for-each
         (lambda (message) (scan-message message package filename symbols extensions))
         (reverse message*))
        (for-each
         (lambda (encoded)
           (let ([item-decoder (make-protobuf-decoder encoded)])
             (let item-loop ()
               (let ([field (protobuf-decoder-next-field item-decoder)])
                 (when field
                   (when (= (protobuf-wire-field-number field) 1)
                     (hashtable-set!
                      (if (memq encoded service*) services symbols)
                      (qualified-name package (descriptor-string field)) filename))
                   (item-loop))))))
         (append (reverse enum*) (reverse service*)))
        (for-each
         (lambda (extension) (scan-extension extension filename extensions))
         (reverse extension*)))))

  #|proc:grpc-reflection-register-file!
The `grpc-reflection-register-file!` procedure registers one encoded file descriptor.
`registry` receives `filename`, descriptor `bytes`, defined `symbol*`, and gRPC `service*` names.
The symbol and service parameters are lists of strings. The return value is `registry`.
|#
  (define grpc-reflection-register-file!
    (lambda (registry filename bytes symbol* service*)
      (pcheck ([grpc-reflection-registry? registry] [string? filename]
               [bytevector? bytes] [list? symbol* service*])
        (unless (and (andmap string? symbol*) (andmap string? service*))
          (errorf 'grpc-reflection-register-file!
                  "symbol and service names must be strings"))
        (with-mutex (grpc-reflection-registry-mutex registry)
          (hashtable-set! (grpc-reflection-registry-files registry) filename bytes)
          (scan-file-descriptor!
           bytes filename
           (grpc-reflection-registry-dependencies registry)
           (grpc-reflection-registry-symbols registry)
           (grpc-reflection-registry-services registry)
           (grpc-reflection-registry-extensions registry))
          (for-each
           (lambda (symbol)
             (hashtable-set! (grpc-reflection-registry-symbols registry) symbol filename))
           symbol*)
          (for-each
           (lambda (service)
             (hashtable-set! (grpc-reflection-registry-services registry) service filename))
           service*))
        registry)))

  (define reflection-error
    (lambda (request code message)
      (protobuf-encode-message
       (list (list 2 'message request)
             (list 7 'message
                   (protobuf-encode-message
                    (list (list 1 'uint32 code) (list 2 'string message))))))))

  (define reflection-file-response
    (lambda (request descriptor*)
      (protobuf-encode-message
       (list (list 2 'message request)
             (list 4 'message
                   (protobuf-encode-message
                    (map (lambda (descriptor) (list 1 'bytes descriptor)) descriptor*)))))))

  (define registry-file-closure
    (lambda (registry filename)
      (let ([seen (make-hashtable string-hash string=?)] [answer '()])
        (let visit ([name filename])
          (unless (hashtable-ref seen name #f)
            (hashtable-set! seen name #t)
            (let ([descriptor
                   (hashtable-ref (grpc-reflection-registry-files registry) name #f)])
              (when descriptor
                (set! answer (cons descriptor answer))
                (for-each visit
                          (hashtable-ref
                           (grpc-reflection-registry-dependencies registry) name '()))))))
        (reverse answer))))

  (define registry-service-name*
    (lambda (registry)
      (let-values ([(key* value*)
                    (hashtable-entries (grpc-reflection-registry-services registry))])
        (sort string<? (vector->list key*)))))

  (define reflection-list-response
    (lambda (registry request)
      (let ([service-field*
             (map (lambda (name)
                    (list 1 'message
                          (protobuf-encode-message (list (list 1 'string name)))))
                  (registry-service-name* registry))])
        (protobuf-encode-message
         (list (list 2 'message request)
               (list 6 'message (protobuf-encode-message service-field*)))))))

  (define reflection-request-kind
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [kind #f] [value #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(3) (set! kind 'filename) (set! value (protobuf-decode-string
                                                        (protobuf-wire-field-value field)))]
                [(4) (set! kind 'symbol) (set! value (protobuf-decode-string
                                                      (protobuf-wire-field-value field)))]
                [(5)
                 (let ([extension-decoder
                        (make-protobuf-decoder (protobuf-wire-field-value field))]
                       [containing-type #f] [number #f])
                   (let extension-loop ()
                     (let ([extension-field
                            (protobuf-decoder-next-field extension-decoder)])
                       (when extension-field
                         (case (protobuf-wire-field-number extension-field)
                           [(1) (set! containing-type (descriptor-string extension-field))]
                           [(2) (set! number (protobuf-wire-field-value extension-field))])
                         (extension-loop))))
                   (set! kind 'extension)
                   (set! value (and containing-type number
                                    (extension-key containing-type number))))]
                [(7) (set! kind 'list-services) (set! value #t)])
              (loop))))
        (values kind value))))

  (define reflection-answer
    (lambda (registry request)
      (with-mutex (grpc-reflection-registry-mutex registry)
        (let-values ([(kind value) (reflection-request-kind request)])
          (case kind
            [(list-services) (reflection-list-response registry request)]
            [(filename)
             (let ([descriptor* (registry-file-closure registry value)])
               (if (pair? descriptor*)
                   (reflection-file-response request descriptor*)
                   (reflection-error request 5 "file descriptor not found")))]
            [(symbol)
             (let* ([filename
                     (hashtable-ref (grpc-reflection-registry-symbols registry) value #f)]
                    [descriptor* (and filename (registry-file-closure registry filename))])
               (if (pair? descriptor*)
                   (reflection-file-response request descriptor*)
                   (reflection-error request 5 "symbol not found")))]
            [(extension)
             (let* ([filename
                     (and value
                          (hashtable-ref
                           (grpc-reflection-registry-extensions registry) value #f))]
                    [descriptor* (and filename (registry-file-closure registry filename))])
               (if (pair? descriptor*)
                   (reflection-file-response request descriptor*)
                   (reflection-error request 5 "extension not found")))]
            [else (reflection-error request 12 "reflection request is unsupported")])))))

  #|proc:grpc-register-reflection!
The `grpc-register-reflection!` procedure registers v1 reflection on server `channel`.
`registry` supplies descriptor bytes and service names. The return value is `channel`.
|#
  (define grpc-register-reflection!
    (lambda (channel registry)
      (pcheck ([grpc-channel? channel] [grpc-reflection-registry? registry])
        (grpc-register-service!
         channel grpc-reflection-method 'bidi
         (lambda (stream)
           (let loop ()
             (let ([request (grpc-stream-recv stream)])
               (unless (eof-object? request)
                 (grpc-stream-send stream (reflection-answer registry request))
                 (loop))))
           #f))
        channel)))
)
