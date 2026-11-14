(library (cspp format protobuf descriptor)
  (export protobuf-code-generator-request?
          protobuf-code-generator-request-file-to-generate
          protobuf-code-generator-request-parameter
          protobuf-code-generator-request-proto-files
          protobuf-file-descriptor? protobuf-file-descriptor-name
          protobuf-file-descriptor-package protobuf-file-descriptor-dependencies
          protobuf-file-descriptor-messages protobuf-file-descriptor-enums
          protobuf-file-descriptor-services protobuf-file-descriptor-syntax
          protobuf-file-descriptor-options protobuf-file-descriptor-raw
          protobuf-message-descriptor? protobuf-message-descriptor-name
          protobuf-message-descriptor-fields protobuf-message-descriptor-nested-messages
          protobuf-message-descriptor-enums protobuf-message-descriptor-oneofs
          protobuf-message-descriptor-options
          protobuf-field-descriptor? protobuf-field-descriptor-name
          protobuf-field-descriptor-number protobuf-field-descriptor-label
          protobuf-field-descriptor-type protobuf-field-descriptor-type-name
          protobuf-field-descriptor-oneof-index protobuf-field-descriptor-json-name
          protobuf-field-descriptor-proto3-optional? protobuf-field-descriptor-options
          protobuf-enum-descriptor? protobuf-enum-descriptor-name
          protobuf-enum-descriptor-values protobuf-enum-descriptor-options
          protobuf-service-descriptor? protobuf-service-descriptor-name
          protobuf-service-descriptor-methods protobuf-service-descriptor-options
          protobuf-method-descriptor? protobuf-method-descriptor-name
          protobuf-method-descriptor-input-type protobuf-method-descriptor-output-type
          protobuf-method-descriptor-client-streaming?
          protobuf-method-descriptor-server-streaming? protobuf-method-descriptor-options
          bytevector->protobuf-code-generator-request
          protobuf-code-generator-response)
  (import (cspp chez)
          (only (cspp utils) pcheck)
          (cspp format protobuf wire))

  #|record:protobuf-code-generator-request
The `protobuf-code-generator-request` record is an immutable decoded protoc plugin request.
File-to-generate is a vector of target names, parameter is a string or `#f`, and proto-files is a
vector of file descriptors containing the request's schema graph.
|#
  (define-record-type (protobuf-code-generator-request
                       %make-protobuf-code-generator-request
                       protobuf-code-generator-request?)
    (sealed #t)
    (opaque #f)
    (fields (immutable file-to-generate protobuf-code-generator-request-file-to-generate)
            (immutable parameter protobuf-code-generator-request-parameter)
            (immutable proto-files protobuf-code-generator-request-proto-files)))

  #|record:protobuf-file-descriptor
The `protobuf-file-descriptor` record is an immutable decoded FileDescriptorProto.
It stores name, package, dependency names, message, enum, and service vectors, syntax, raw options,
and the original encoded bytes. Nested descriptor values are immutable.
|#
  (define-record-type (protobuf-file-descriptor %make-protobuf-file-descriptor
                                                protobuf-file-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-file-descriptor-name)
            (immutable package protobuf-file-descriptor-package)
            (immutable dependencies protobuf-file-descriptor-dependencies)
            (immutable messages protobuf-file-descriptor-messages)
            (immutable enums protobuf-file-descriptor-enums)
            (immutable services protobuf-file-descriptor-services)
            (immutable syntax protobuf-file-descriptor-syntax)
            (immutable options protobuf-file-descriptor-options)
            (immutable raw protobuf-file-descriptor-raw)))

  #|record:protobuf-message-descriptor
The `protobuf-message-descriptor` record is an immutable decoded DescriptorProto.
It stores the message name and vectors of fields, nested messages, enums, and oneof names, plus
the raw encoded options payload or `#f`.
|#
  (define-record-type (protobuf-message-descriptor %make-protobuf-message-descriptor
                                                   protobuf-message-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-message-descriptor-name)
            (immutable fields protobuf-message-descriptor-fields)
            (immutable nested-messages protobuf-message-descriptor-nested-messages)
            (immutable enums protobuf-message-descriptor-enums)
            (immutable oneofs protobuf-message-descriptor-oneofs)
            (immutable options protobuf-message-descriptor-options)))

  #|record:protobuf-field-descriptor
The `protobuf-field-descriptor` record is an immutable decoded FieldDescriptorProto.
It stores name, number, numeric label and type, type name, optional oneof index, JSON name,
proto3-optional status, and the raw encoded options payload or `#f`.
|#
  (define-record-type (protobuf-field-descriptor %make-protobuf-field-descriptor
                                                 protobuf-field-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-field-descriptor-name)
            (immutable number protobuf-field-descriptor-number)
            (immutable label protobuf-field-descriptor-label)
            (immutable type protobuf-field-descriptor-type)
            (immutable type-name protobuf-field-descriptor-type-name)
            (immutable oneof-index protobuf-field-descriptor-oneof-index)
            (immutable json-name protobuf-field-descriptor-json-name)
            (immutable proto3-optional? protobuf-field-descriptor-proto3-optional?)
            (immutable options protobuf-field-descriptor-options)))

  #|record:protobuf-enum-descriptor
The `protobuf-enum-descriptor` record is an immutable decoded EnumDescriptorProto.
It stores the enum name, a vector of name, number, and options value vectors, and raw options.
|#
  (define-record-type (protobuf-enum-descriptor %make-protobuf-enum-descriptor
                                                protobuf-enum-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-enum-descriptor-name)
            (immutable values protobuf-enum-descriptor-values)
            (immutable options protobuf-enum-descriptor-options)))

  #|record:protobuf-service-descriptor
The `protobuf-service-descriptor` record is an immutable decoded ServiceDescriptorProto.
It stores the service name, a vector of method descriptors, and raw encoded options or `#f`.
|#
  (define-record-type (protobuf-service-descriptor %make-protobuf-service-descriptor
                                                   protobuf-service-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-service-descriptor-name)
            (immutable methods protobuf-service-descriptor-methods)
            (immutable options protobuf-service-descriptor-options)))

  #|record:protobuf-method-descriptor
The `protobuf-method-descriptor` record is an immutable decoded MethodDescriptorProto.
It stores name, input and output type names, client and server streaming flags, and raw options.
|#
  (define-record-type (protobuf-method-descriptor %make-protobuf-method-descriptor
                                                  protobuf-method-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name protobuf-method-descriptor-name)
            (immutable input-type protobuf-method-descriptor-input-type)
            (immutable output-type protobuf-method-descriptor-output-type)
            (immutable client-streaming? protobuf-method-descriptor-client-streaming?)
            (immutable server-streaming? protobuf-method-descriptor-server-streaming?)
            (immutable options protobuf-method-descriptor-options)))

  (define field-string
    (lambda (field)
      (protobuf-decode-string (protobuf-wire-field-value field))))

  (define decode-oneof
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (when (= (protobuf-wire-field-number field) 1)
                (set! name (field-string field)))
              (loop))))
        name)))

  (define decode-field
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)]
            [name ""] [number 0] [label 1] [type 0] [type-name ""]
            [oneof-index #f] [json-name ""] [proto3-optional? #f] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(3) (set! number (protobuf-wire-field-value field))]
                [(4) (set! label (protobuf-wire-field-value field))]
                [(5) (set! type (protobuf-wire-field-value field))]
                [(6) (set! type-name (field-string field))]
                [(8) (set! options (protobuf-wire-field-value field))]
                [(9) (set! oneof-index (protobuf-wire-field-value field))]
                [(10) (set! json-name (field-string field))]
                [(17) (set! proto3-optional? (not (zero? (protobuf-wire-field-value field))))])
              (loop))))
        (%make-protobuf-field-descriptor name number label type type-name oneof-index json-name
                                         proto3-optional? options))))

  (define decode-enum-value
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [number 0] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (let ([value (protobuf-wire-field-value field)])
                       (set! number (if (bitwise-bit-set? value 31)
                                        (- value #x100000000)
                                        value)))]
                [(3) (set! options (protobuf-wire-field-value field))])
              (loop))))
        (vector name number options))))

  (define decode-enum
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [values '()] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (set! values (cons (decode-enum-value (protobuf-wire-field-value field))
                                        values))]
                [(3) (set! options (protobuf-wire-field-value field))])
              (loop))))
        (%make-protobuf-enum-descriptor name (list->vector (reverse values)) options))))

  (define decode-message
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [fields '()] [nested '()]
            [enums '()] [oneofs '()] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (set! fields (cons (decode-field (protobuf-wire-field-value field)) fields))]
                [(3) (set! nested (cons (decode-message (protobuf-wire-field-value field)) nested))]
                [(4) (set! enums (cons (decode-enum (protobuf-wire-field-value field)) enums))]
                [(7) (set! options (protobuf-wire-field-value field))]
                [(8) (set! oneofs (cons (decode-oneof (protobuf-wire-field-value field)) oneofs))])
              (loop))))
        (%make-protobuf-message-descriptor
         name (list->vector (reverse fields)) (list->vector (reverse nested))
         (list->vector (reverse enums)) (list->vector (reverse oneofs)) options))))

  (define decode-method
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [input-type ""]
            [output-type ""] [client-streaming? #f] [server-streaming? #f] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (set! input-type (field-string field))]
                [(3) (set! output-type (field-string field))]
                [(4) (set! options (protobuf-wire-field-value field))]
                [(5) (set! client-streaming? (not (zero? (protobuf-wire-field-value field))))]
                [(6) (set! server-streaming? (not (zero? (protobuf-wire-field-value field))))])
              (loop))))
        (%make-protobuf-method-descriptor name input-type output-type client-streaming?
                                          server-streaming? options))))

  (define decode-service
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [methods '()] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (set! methods (cons (decode-method (protobuf-wire-field-value field)) methods))]
                [(3) (set! options (protobuf-wire-field-value field))])
              (loop))))
        (%make-protobuf-service-descriptor name (list->vector (reverse methods)) options))))

  (define decode-file
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [name ""] [package ""]
            [dependencies '()] [messages '()] [enums '()] [services '()]
            [syntax "proto2"] [options #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! name (field-string field))]
                [(2) (set! package (field-string field))]
                [(3) (set! dependencies (cons (field-string field) dependencies))]
                [(4) (set! messages (cons (decode-message (protobuf-wire-field-value field))
                                          messages))]
                [(5) (set! enums (cons (decode-enum (protobuf-wire-field-value field)) enums))]
                [(6) (set! services (cons (decode-service (protobuf-wire-field-value field))
                                          services))]
                [(8) (set! options (protobuf-wire-field-value field))]
                [(12) (set! syntax (field-string field))])
              (loop))))
        (%make-protobuf-file-descriptor
         name package (list->vector (reverse dependencies)) (list->vector (reverse messages))
         (list->vector (reverse enums)) (list->vector (reverse services)) syntax options bytes))))

  #|proc:bytevector->protobuf-code-generator-request
The `bytevector->protobuf-code-generator-request` procedure decodes plugin request `bytes`.
It returns a descriptor request containing target file names, parameters, and file descriptors.
|#
  (define bytevector->protobuf-code-generator-request
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([decoder (make-protobuf-decoder bytes)] [file-to-generate '()]
              [parameter #f] [proto-files '()])
          (let loop ()
            (let ([field (protobuf-decoder-next-field decoder)])
              (when field
                (case (protobuf-wire-field-number field)
                  [(1) (set! file-to-generate (cons (field-string field) file-to-generate))]
                  [(2) (set! parameter (field-string field))]
                  [(15) (set! proto-files
                              (cons (decode-file (protobuf-wire-field-value field)) proto-files))])
                (loop))))
          (%make-protobuf-code-generator-request
           (list->vector (reverse file-to-generate)) parameter
           (list->vector (reverse proto-files)))))))

  (define bytevector-join
    (lambda (part*)
      (let-values ([(port get) (open-bytevector-output-port)])
        (for-each (lambda (part) (put-bytevector port part)) part*)
        (get))))

  #|proc:protobuf-code-generator-response
The `protobuf-code-generator-response` procedure encodes generated `file*` entries.
Each entry is a pair whose car is an output file name and whose cdr is its textual content.
The return value is a protobuf `CodeGeneratorResponse` bytevector.
|#
  (define protobuf-code-generator-response
    (lambda (file*)
      (pcheck ([list? file*])
        (protobuf-encode-message
         (map (lambda (entry)
                (unless (and (pair? entry) (string? (car entry)) (string? (cdr entry)))
                  (errorf 'protobuf-code-generator-response
                          "expected (name . content) string pair: ~s" entry))
                (list 15 'message
                      (protobuf-encode-message
                       (list (list 1 'string (car entry))
                             (list 15 'string (cdr entry))))))
              file*)))))
)
