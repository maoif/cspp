(import (cspp)
        (cspp format protobuf))

(define append-map
  (lambda (proc value*)
    (fold-right (lambda (value answer) (append (proc value) answer)) '() value*)))

(define string-split-on
  (lambda (text separator)
    (let ([length (string-length text)])
      (let loop ([start 0] [index 0] [answer '()])
        (cond
         [(= index length)
          (reverse (cons (substring text start index) answer))]
         [(char=? (string-ref text index) separator)
          (loop (+ index 1) (+ index 1) (cons (substring text start index) answer))]
         [else (loop start (+ index 1) answer)])))))

(define identifier->kebab
  (lambda (name)
    (let-values ([(port get) (open-string-output-port)])
      (let loop ([index 0] [previous #f])
        (when (< index (string-length name))
          (let ([character (string-ref name index)])
            (cond
             [(or (char=? character #\_) (char=? character #\.))
              (unless (or (not previous) (char=? previous #\-)) (write-char #\- port))
              (loop (+ index 1) #\-)]
             [(char-upper-case? character)
              (when (and previous
                         (or (char-lower-case? previous) (char-numeric? previous)))
                (write-char #\- port))
              (write-char (char-downcase character) port)
              (loop (+ index 1) character)]
             [else
              (write-char (char-downcase character) port)
              (loop (+ index 1) character)]))))
      (get))))

(define path-base-name
  (lambda (name)
    (let* ([part* (string-split-on name #\/)]
           [base (car (reverse part*))]
           [length (string-length base)])
      (if (and (>= length 6) (string=? ".proto" (substring base (- length 6) length)))
          (substring base 0 (- length 6))
          base))))

(define library-name
  (lambda (file)
    (append (filter (lambda (part) (not (string=? part "")))
                    (map identifier->kebab
                         (string-split-on (protobuf-file-descriptor-package file) #\.)))
            (list (identifier->kebab (path-base-name (protobuf-file-descriptor-name file)))
                  "protobuf"))))

(define proto-output-name
  (lambda (name)
    (let ([length (string-length name)])
      (if (and (>= length 6) (string=? ".proto" (substring name (- length 6) length)))
          (string-append (substring name 0 (- length 6)) ".pb.ss")
          (string-append name ".pb.ss")))))

(define vector->list* vector->list)

(define option-bool
  (lambda (options number)
    (and options
         (let ([decoder (make-protobuf-decoder options)])
           (let loop ()
             (let ([field (protobuf-decoder-next-field decoder)])
               (cond
                [(not field) #f]
                [(= number (protobuf-wire-field-number field))
                 (not (zero? (protobuf-wire-field-value field)))]
                [else (loop)])))))))

(define map-entry-message?
  (lambda (message)
    (option-bool (protobuf-message-descriptor-options message) 7)))

(define collect-messages
  (lambda (file)
    (letrec ([walk
              (lambda (message parent-proto parent-scheme)
                (let* ([name (protobuf-message-descriptor-name message)]
                       [proto-name (if (string=? parent-proto "")
                                       name
                                       (string-append parent-proto "." name))]
                       [scheme-name (if (string=? parent-scheme "")
                                        (identifier->kebab name)
                                        (string-append parent-scheme "-"
                                                       (identifier->kebab name)))])
                  (cons (vector proto-name scheme-name message)
                        (append-map
                         (lambda (nested) (walk nested proto-name scheme-name))
                         (vector->list*
                          (protobuf-message-descriptor-nested-messages message))))))])
      (append-map (lambda (message) (walk message "" ""))
                  (vector->list* (protobuf-file-descriptor-messages file))))))

(define collect-enums
  (lambda (file)
    (letrec ([message-enums
              (lambda (entry)
                (let* ([prefix (vector-ref entry 1)]
                       [message (vector-ref entry 2)])
                  (map (lambda (enum)
                         (vector (string-append prefix "-"
                                                (identifier->kebab
                                                 (protobuf-enum-descriptor-name enum)))
                                 enum))
                       (vector->list* (protobuf-message-descriptor-enums message)))))])
      (append
       (map (lambda (enum)
              (vector (identifier->kebab (protobuf-enum-descriptor-name enum)) enum))
            (vector->list* (protobuf-file-descriptor-enums file)))
       (append-map message-enums (collect-messages file))))))

(define scalar-type?
  (lambda (type)
    (not (= type 11))))

(define field-type-symbol
  (lambda (field)
    (case (protobuf-field-descriptor-type field)
      [(1) 'double] [(2) 'float] [(3) 'int64] [(4) 'uint64] [(5) 'int32]
      [(6) 'fixed64] [(7) 'fixed32] [(8) 'bool] [(9) 'string] [(11) 'message]
      [(12) 'bytes] [(13) 'uint32] [(14) 'enum] [(15) 'sfixed32]
      [(16) 'sfixed64] [(17) 'sint32] [(18) 'sint64]
      [else (errorf 'protoc-gen-cspp "unsupported protobuf field type ~s"
                    (protobuf-field-descriptor-type field))])))

(define type-scheme-name
  (lambda (file type-name)
    (let* ([plain (if (and (positive? (string-length type-name))
                           (char=? (string-ref type-name 0) #\.))
                      (substring type-name 1 (string-length type-name))
                      type-name)]
           [package (protobuf-file-descriptor-package file)]
           [relative (if (and (positive? (string-length package))
                              (> (string-length plain) (string-length package))
                              (string=? package
                                        (substring plain 0 (string-length package))))
                         (substring plain (+ (string-length package) 1)
                                    (string-length plain))
                         plain)])
      (identifier->kebab relative))))

(define find-message-entry
  (lambda (file type-name)
    (let ([name (type-scheme-name file type-name)])
      (find (lambda (entry) (string=? name (vector-ref entry 1)))
            (collect-messages file)))))

(define map-field?
  (lambda (file field)
    (and (= (protobuf-field-descriptor-label field) 3)
         (= (protobuf-field-descriptor-type field) 11)
         (let ([entry (find-message-entry file
                                          (protobuf-field-descriptor-type-name field))])
           (and entry (map-entry-message? (vector-ref entry 2)))))))

(define field-oneof?
  (lambda (field)
    (and (protobuf-field-descriptor-oneof-index field)
         (not (protobuf-field-descriptor-proto3-optional? field)))))

(define field-presence?
  (lambda (file field)
    (and (scalar-type? (protobuf-field-descriptor-type field))
         (not (field-oneof? field))
         (or (protobuf-field-descriptor-proto3-optional? field)
             (and (string=? "proto2" (protobuf-file-descriptor-syntax file))
                  (= 1 (protobuf-field-descriptor-label field)))))))

(define field-name
  (lambda (field)
    (let ([base (identifier->kebab (protobuf-field-descriptor-name field))])
      (if (= 8 (protobuf-field-descriptor-type field))
          (string-append base "?")
          base))))

(define field-default
  (lambda (file field)
    (cond
     [(map-field? file field) '(make-hashtable equal-hash equal?)]
     [(= 3 (protobuf-field-descriptor-label field)) ''()]
     [else
      (case (protobuf-field-descriptor-type field)
        [(1 2) 0.0] [(3 4 5 6 7 13 14 15 16 17 18) 0]
        [(8) #f] [(9) ""] [(11) #f] [(12) '#vu8()]
        [else #f])])))

(define field-predicate
  (lambda (file field)
    (cond
     [(map-field? file field) 'hashtable?]
     [(= 3 (protobuf-field-descriptor-label field)) 'vector?]
     [else
      (case (protobuf-field-descriptor-type field)
        [(1 2) 'real?] [(3 5 15 16 17 18) 'integer?]
        [(4 6 7 13) 'natural?] [(8) 'boolean?] [(9) 'string?]
        [(11) `(lambda (value)
                 (or (not value)
                     (,(string->symbol
                        (string-append
                         (type-scheme-name file
                                           (protobuf-field-descriptor-type-name field))
                         "?"))
                      value)))]
        [(12) 'bytevector?] [(14) 'integer?]
        [else 'object?])])))

(define message-oneofs
  (lambda (message)
    (let ([field* (vector->list* (protobuf-message-descriptor-fields message))])
      (filter
       (lambda (entry)
         (exists (lambda (field)
                   (and (field-oneof? field)
                        (= (car entry) (protobuf-field-descriptor-oneof-index field))))
                 field*))
       (let loop ([index 0]
                  [name* (vector->list* (protobuf-message-descriptor-oneofs message))]
                  [answer '()])
         (if (null? name*)
             (reverse answer)
             (loop (+ index 1) (cdr name*)
                   (cons (cons index (identifier->kebab (car name*))) answer))))))))

(define message-components
  (lambda (file message)
    (append
     (append-map
      (lambda (field)
        (if (field-oneof? field)
            '()
            (if (field-presence? file field)
                (list (field-name field)
                      (string-append (field-name field) "-present?"))
                (list (field-name field)))))
      (vector->list* (protobuf-message-descriptor-fields message)))
     (map cdr (message-oneofs message)))))

(define write-doc
  (lambda (port kind name line*)
    (format port "#|~a:~a~%" kind name)
    (for-each (lambda (line) (format port "~a~%" line)) line*)
    (display "|#\n" port)))

(define write-form
  (lambda (port form)
    (parameterize ([pretty-line-length 96])
      (pretty-print form port))))

(define field-accessor
  (lambda (message-name field)
    (string->symbol (string-append message-name "-" (field-name field)))))

(define component-accessor
  (lambda (message-name component)
    (string->symbol (string-append message-name "-" component))))

(define encode-value
  (lambda (file field value)
    (if (= 11 (protobuf-field-descriptor-type field))
        (list (string->symbol
               (string-append (type-scheme-name file
                                                (protobuf-field-descriptor-type-name field))
                              "-encode"))
              value)
        value)))

(define non-default-test
  (lambda (file field value)
    (case (protobuf-field-descriptor-type field)
      [(1 2 3 4 5 6 7 13 14 15 16 17 18) `(not (zero? ,value))]
      [(8) value]
      [(9) `(not (string=? ,value ""))]
      [(11) value]
      [(12) `(positive? (bytevector-length ,value))]
      [else #t])))

(define normal-field-encode
  (lambda (file message-name field)
    (let* ([accessor (field-accessor message-name field)]
           [value `(,accessor message)]
           [encoded (encode-value file field value)]
           [item `(list ,(protobuf-field-descriptor-number field)
                        ',(field-type-symbol field) ,encoded)])
      (cond
       [(map-field? file field)
        (let* ([entry (vector-ref (find-message-entry
                                   file (protobuf-field-descriptor-type-name field)) 2)]
               [entry-name (type-scheme-name file
                                             (protobuf-field-descriptor-type-name field))])
          `(%protobuf-map-fields ,(protobuf-field-descriptor-number field)
                                 ,value ,(string->symbol
                                          (string-append "%" entry-name "-encode"))))]
       [(= 3 (protobuf-field-descriptor-label field))
        `(map (lambda (value)
                (list ,(protobuf-field-descriptor-number field)
                      ',(field-type-symbol field)
                      ,(encode-value file field 'value)))
              (vector->list ,value))]
       [(field-presence? file field)
        `(if (,(component-accessor message-name
                                   (string-append (field-name field) "-present?"))
              message)
             (list ,item)
             '())]
       [(= 2 (protobuf-field-descriptor-label field)) `(list ,item)]
       [else `(if ,(non-default-test file field value) (list ,item) '())]))))

(define oneof-encode
  (lambda (file message-name message oneof)
    (let* ([index (car oneof)] [name (cdr oneof)]
           [accessor (component-accessor message-name name)]
           [field* (filter
                    (lambda (field)
                      (and (field-oneof? field)
                           (= index (protobuf-field-descriptor-oneof-index field))))
                    (vector->list* (protobuf-message-descriptor-fields message)))])
      `(let ([choice (,accessor message)])
         (if (not choice)
             '()
             (case (car choice)
               ,@(map
                  (lambda (field)
                    `[(,(string->symbol (field-name field)))
                      (list
                       (list ,(protobuf-field-descriptor-number field)
                             ',(field-type-symbol field)
                             ,(encode-value file field '(cdr choice))))])
                  field*)
               [else (errorf ',accessor "invalid oneof value: ~s" choice)]))))))

(define decode-value
  (lambda (file field)
    (let ([value '(protobuf-wire-field-value field)])
      (case (protobuf-field-descriptor-type field)
        [(1) `(%protobuf-u64->double ,value)]
        [(2) `(%protobuf-u32->float ,value)]
        [(3) `(%protobuf-signed ,value 64)]
        [(5 14) `(%protobuf-signed ,value 32)]
        [(8) `(not (zero? ,value))]
        [(9) `(protobuf-decode-string ,value)]
        [(11) `(,(string->symbol
                  (string-append "bytevector->"
                                 (type-scheme-name
                                  file (protobuf-field-descriptor-type-name field))))
                ,value)]
        [(12) value]
        [(15) `(%protobuf-u32->signed ,value)]
        [(16) `(%protobuf-u64->signed ,value)]
        [(17 18) `(%protobuf-zigzag ,value)]
        [else value]))))

(define decode-clause
  (lambda (file message message-name field)
    (let* ([name (field-name field)]
           [variable (string->symbol name)]
           [value (decode-value file field)])
      `[(,(protobuf-field-descriptor-number field))
        ,(cond
          [(map-field? file field)
           (let ([entry-name (type-scheme-name file
                                               (protobuf-field-descriptor-type-name field))])
             `(let ([entry (,(string->symbol
                              (string-append "%bytevector->" entry-name))
                            (protobuf-wire-field-value field))])
                (hashtable-set! ,variable (car entry) (cdr entry))))]
          [(= 3 (protobuf-field-descriptor-label field))
           `(set! ,variable (cons ,value ,variable))]
          [(field-oneof? field)
           (let* ([oneof (vector-ref (protobuf-message-descriptor-oneofs message)
                                     (protobuf-field-descriptor-oneof-index field))]
                  [oneof-variable (string->symbol (identifier->kebab oneof))])
             `(set! ,oneof-variable (cons ',variable ,value)))]
          [(field-presence? file field)
           `(begin (set! ,variable ,value)
                   (set! ,(string->symbol (string-append name "-present?")) #t))]
          [else `(set! ,variable ,value)])])))

(define write-message
  (lambda (port file entry)
    (let* ([message-name (vector-ref entry 1)]
           [message (vector-ref entry 2)]
           [field* (vector->list* (protobuf-message-descriptor-fields message))]
           [component* (message-components file message)]
           [oneof* (message-oneofs message)]
           [internal (string->symbol (string-append "%make-" message-name))]
           [public (string->symbol (string-append "make-" message-name))]
           [predicate (string->symbol (string-append message-name "?"))]
           [unknown-accessor (string->symbol (string-append message-name "-unknown-fields"))])
      (write-form
       port
       `(define-record-type (,(string->symbol (string-append "%" message-name "-record-type"))
                             ,internal ,predicate)
          (sealed #t)
          (opaque #f)
          (fields
           ,@(map (lambda (component)
                    `(immutable ,(string->symbol component)
                                ,(component-accessor message-name component)))
                  component*)
           (immutable unknown-fields ,unknown-accessor))))
      (newline port)
      (write-doc port "proc" (symbol->string public)
                 (list (format "The `~a` procedure creates a protobuf `~a` message."
                               public (protobuf-message-descriptor-name message))
                       "Its parameters supply fields in schema order; presence flags mark optional values."
                       "The return value is a new message record with no unknown fields."))
      (let* ([argument* (map string->symbol component*)]
             [check*
              (append-map
               (lambda (field)
                 (if (field-oneof? field)
                     '()
                     (let ([name (string->symbol (field-name field))])
                       (append
                        (list (list (field-predicate file field) name))
                        (if (field-presence? file field)
                            (list (list 'boolean?
                                        (string->symbol
                                         (string-append (field-name field) "-present?"))))
                            '())))))
               field*)]
             [check* (append check*
                             (map (lambda (oneof)
                                    (list '(lambda (value) (or (not value) (pair? value)))
                                          (string->symbol (cdr oneof))))
                                  oneof*))])
        (write-form
         port
         `(define ,public
            (lambda ,argument*
              (pcheck ,check*
                (,internal ,@argument* '#()))))))
      (newline port)
      (write-doc port "proc" (string-append message-name "-encode")
                 (list (format "The `~a-encode` procedure encodes protobuf record `message`."
                               message-name)
                       "The return value is a newly allocated wire-format bytevector."))
      (write-form
       port
       `(define ,(string->symbol (string-append message-name "-encode"))
          (lambda (message)
            (pcheck ([,predicate message])
              (%protobuf-encode-with-unknown
               (append
                ,@(append
                   (map (lambda (field) (normal-field-encode file message-name field))
                        (filter (lambda (field) (not (field-oneof? field))) field*))
                   (map (lambda (oneof)
                          (oneof-encode file message-name message oneof))
                        oneof*)))
               (,unknown-accessor message))))))
      (newline port)
      (write-doc port "proc" (string-append message-name "-encoded-size")
                 (list (format "The `~a-encoded-size` procedure measures protobuf record `message`."
                               message-name)
                       "The return value is the encoded byte length."))
      (write-form
       port
       `(define ,(string->symbol (string-append message-name "-encoded-size"))
          (lambda (message)
            (pcheck ([,predicate message])
              (bytevector-length
               (,(string->symbol (string-append message-name "-encode")) message))))))
      (newline port)
      (write-doc port "proc" (string-append "bytevector->" message-name)
                 (list (format "The `bytevector->~a` procedure decodes protobuf bytevector `bytes`."
                               message-name)
                       "The return value is a message record that retains unknown fields."))
      (let* ([normal-field* (filter (lambda (field) (not (field-oneof? field))) field*)]
             [binding*
              (append
               (append-map
                (lambda (field)
                  (let ([name (string->symbol (field-name field))])
                    (append
                     (list (list name (field-default file field)))
                     (if (field-presence? file field)
                         (list (list (string->symbol
                                     (string-append (field-name field) "-present?")) #f))
                         '()))))
                normal-field*)
               (map (lambda (oneof) (list (string->symbol (cdr oneof)) #f)) oneof*))]
             [result-argument*
              (map (lambda (component)
                     (let ([field (find (lambda (candidate)
                                         (string=? component (field-name candidate)))
                                       normal-field*)])
                       (if (and field
                                (= 3 (protobuf-field-descriptor-label field))
                                (not (map-field? file field)))
                           `(list->vector (reverse ,(string->symbol component)))
                           (string->symbol component))))
                   component*)])
        (write-form
         port
         `(define ,(string->symbol (string-append "bytevector->" message-name))
            (lambda (bytes)
              (pcheck ([bytevector? bytes])
                (let ([decoder (make-protobuf-decoder bytes)] ,@binding*)
                  (let loop ()
                    (let ([field (protobuf-decoder-next-field decoder)])
                      (when field
                        (case (protobuf-wire-field-number field)
                          ,@(map (lambda (field)
                                   (decode-clause file message message-name field))
                                 field*)
                          [else (protobuf-decoder-preserve-field! decoder field)])
                        (loop))))
                  (,internal ,@result-argument*
                             (protobuf-decoder-unknown-fields decoder)))))))
      (newline port)))))

(define write-map-entry
  (lambda (port file entry)
    (let* ([name (vector-ref entry 1)]
           [message (vector-ref entry 2)]
           [field* (vector->list* (protobuf-message-descriptor-fields message))]
           [key (find (lambda (field) (= 1 (protobuf-field-descriptor-number field))) field*)]
           [value (find (lambda (field) (= 2 (protobuf-field-descriptor-number field))) field*)])
      (write-form
       port
       `(define ,(string->symbol (string-append "%" name "-encode"))
          (lambda (key value)
            (protobuf-encode-message
             (list (list 1 ',(field-type-symbol key) ,(encode-value file key 'key))
                   (list 2 ',(field-type-symbol value) ,(encode-value file value 'value)))))))
      (write-form
       port
       `(define ,(string->symbol (string-append "%bytevector->" name))
          (lambda (bytes)
            (let ([decoder (make-protobuf-decoder bytes)]
                  [key ,(field-default file key)] [value ,(field-default file value)])
              (let loop ()
                (let ([field (protobuf-decoder-next-field decoder)])
                  (when field
                    (case (protobuf-wire-field-number field)
                      [(1) (set! key ,(decode-value file key))]
                      [(2) (set! value ,(decode-value file value))])
                    (loop))))
              (cons key value)))))
      (newline port))))

(define message-exports
  (lambda (file entry)
    (let* ([name (vector-ref entry 1)] [message (vector-ref entry 2)])
      (append
       (list (string-append name "?") (string-append "make-" name))
       (map (lambda (component) (string-append name "-" component))
            (message-components file message))
       (list (string-append name "-unknown-fields")
             (string-append name "-encoded-size")
             (string-append name "-encode")
             (string-append "bytevector->" name))))))

(define enum-exports
  (lambda (entry)
    (let ([prefix (vector-ref entry 0)] [enum (vector-ref entry 1)])
      (map (lambda (value)
             (string-append prefix "-" (identifier->kebab (vector-ref value 0))))
           (vector->list* (protobuf-enum-descriptor-values enum))))))

(define service-exports
  (lambda (service)
    (let ([service-name (identifier->kebab (protobuf-service-descriptor-name service))])
      (append-map
       (lambda (method)
         (let ([method-name (identifier->kebab (protobuf-method-descriptor-name method))])
           (list (string-append service-name "-" method-name "-method")
                 (string-append service-name "-" method-name))))
       (vector->list* (protobuf-service-descriptor-methods service)))
      )))

(define descriptor-symbol-names
  (lambda (file)
    (let ([package (protobuf-file-descriptor-package file)])
      (map (lambda (entry)
             (let ([relative (vector-ref entry 0)])
               (if (string=? package "")
                   relative
                   (string-append package "." relative))))
           (filter (lambda (entry)
                     (not (map-entry-message? (vector-ref entry 2))))
                   (collect-messages file))))))

(define descriptor-service-names
  (lambda (file)
    (let ([package (protobuf-file-descriptor-package file)])
      (map (lambda (service)
             (if (string=? package "")
                 (protobuf-service-descriptor-name service)
                 (string-append package "."
                                (protobuf-service-descriptor-name service))))
           (vector->list* (protobuf-file-descriptor-services file))))))

(define method-shape
  (lambda (method)
    (cond
     [(and (protobuf-method-descriptor-client-streaming? method)
           (protobuf-method-descriptor-server-streaming? method)) 'bidi]
     [(protobuf-method-descriptor-client-streaming? method) 'client]
     [(protobuf-method-descriptor-server-streaming? method) 'server]
     [else 'unary])))

(define write-service
  (lambda (port file service)
    (let* ([service-name (identifier->kebab (protobuf-service-descriptor-name service))]
           [package (protobuf-file-descriptor-package file)]
           [qualified (if (string=? package "")
                          (protobuf-service-descriptor-name service)
                          (string-append package "."
                                         (protobuf-service-descriptor-name service)))])
      (for-each
       (lambda (method)
         (let* ([method-name (identifier->kebab (protobuf-method-descriptor-name method))]
                [base (string-append service-name "-" method-name)]
                [constant (string->symbol (string-append base "-method"))]
                [procedure (string->symbol base)]
                [input (type-scheme-name file (protobuf-method-descriptor-input-type method))]
                [output (type-scheme-name file (protobuf-method-descriptor-output-type method))]
                [shape (method-shape method)])
           (write-form port `(define ,constant
                               ,(format "/~a/~a" qualified
                                        (protobuf-method-descriptor-name method))))
           (newline port)
           (write-doc
            port "proc" base
            (list (format "The `~a` procedure starts the generated `~a` RPC."
                          base (protobuf-method-descriptor-name method))
                  "The `channel` parameter is a client gRPC channel; request values are encoded."
                  "The return value is a decoded response record or a gRPC stream."))
           (write-form
            port
            (case shape
              [(unary)
               `(define ,procedure
                  (lambda (channel request)
                    (pcheck ([grpc-channel? channel]
                             [,(string->symbol (string-append input "?")) request])
                      (,(string->symbol (string-append "bytevector->" output))
                       (grpc-response-payload
                        (grpc-call channel ,constant
                                   (,(string->symbol (string-append input "-encode"))
                                    request)))))))]
              [(server)
               `(define ,procedure
                  (lambda (channel request)
                    (pcheck ([grpc-channel? channel]
                             [,(string->symbol (string-append input "?")) request])
                      (grpc-call/server-stream
                       channel ,constant
                       (,(string->symbol (string-append input "-encode")) request)))))]
              [(client)
               `(define ,procedure
                  (lambda (channel message*)
                    (pcheck ([grpc-channel? channel] [vector? message*])
                      (let ([stream (grpc-call/client-stream channel ,constant)])
                        (dynamic-wind
                          void
                          (lambda ()
                            (vector-for-each
                             (lambda (message)
                               (grpc-stream-send
                                stream
                                (,(string->symbol (string-append input "-encode")) message)))
                             message*)
                            (grpc-stream-close-send stream)
                            (,(string->symbol (string-append "bytevector->" output))
                             (grpc-stream-recv stream)))
                          (lambda () (grpc-stream-close stream)))))))]
              [(bidi)
               `(define ,procedure
                  (lambda (channel)
                    (pcheck ([grpc-channel? channel])
                      (grpc-call/bidi-stream channel ,constant))))]))
           (newline port)))
       (vector->list* (protobuf-service-descriptor-methods service)))
      (let* ([register-name (string-append "register-" service-name "-service!")]
             [argument* (map (lambda (method)
                               (string->symbol
                                (string-append
                                 (identifier->kebab
                                  (protobuf-method-descriptor-name method))
                                 "-handler")))
                             (vector->list* (protobuf-service-descriptor-methods service)))])
        (write-doc port "proc" register-name
                   (list (format "The `~a` procedure registers generated handlers on `server`."
                                 register-name)
                         "Each handler corresponds to one schema method and must follow its RPC shape."
                         "The return value is `server`."))
        (write-form
         port
         `(define ,(string->symbol register-name)
            (lambda (server ,@argument*)
              (pcheck ([grpc-channel? server] [procedure? ,@argument*])
                ,@(map
                   (lambda (method argument)
                     `(grpc-register-service!
                       server
                       ,(string->symbol
                         (string-append service-name "-"
                                        (identifier->kebab
                                         (protobuf-method-descriptor-name method))
                                        "-method"))
                       ',(method-shape method) ,argument))
                   (vector->list* (protobuf-service-descriptor-methods service)) argument*)
                server))))
      (newline port)))))

(define write-runtime-helpers
  (lambda (port)
    (for-each
     (lambda (form) (write-form port form) (newline port))
     '((define %protobuf-encode-with-unknown
         (lambda (field* unknown*)
           (let-values ([(port get) (open-bytevector-output-port)])
             (put-bytevector port (protobuf-encode-message field*))
             (vector-for-each (lambda (raw) (put-bytevector port raw)) unknown*)
             (get))))
       (define %protobuf-signed
         (lambda (value bits)
           (if (bitwise-bit-set? value (- bits 1))
               (- value (bitwise-arithmetic-shift 1 bits))
               value)))
       (define %protobuf-zigzag
         (lambda (value)
           (bitwise-xor (bitwise-arithmetic-shift value -1)
                        (- (bitwise-and value 1)))))
       (define %protobuf-u32->signed (lambda (value) (%protobuf-signed value 32)))
       (define %protobuf-u64->signed (lambda (value) (%protobuf-signed value 64)))
       (define %protobuf-u32->float
         (lambda (value)
           (let ([bytes (make-bytevector 4)])
             (bytevector-u32-set! bytes 0 value (endianness little))
             (protobuf-decode-float bytes))))
       (define %protobuf-u64->double
         (lambda (value)
           (let ([bytes (make-bytevector 8)])
             (bytevector-u64-set! bytes 0 value (endianness little))
             (protobuf-decode-double bytes))))
       (define %protobuf-map-fields
         (lambda (number table encode-entry)
           (let-values ([(key* value*) (hashtable-entries table)])
             (let loop ([index 0] [answer '()])
               (if (= index (vector-length key*))
                   (reverse answer)
                   (loop (+ index 1)
                         (cons (list number 'message
                                     (encode-entry (vector-ref key* index)
                                                   (vector-ref value* index)))
                               answer)))))))))))

(define bytevector-literal
  (lambda (bytes)
    (format "~s" bytes)))

(define generate-file
  (lambda (file)
    (let* ([message-entry* (collect-messages file)]
           [public-message* (filter (lambda (entry)
                                      (not (map-entry-message? (vector-ref entry 2))))
                                    message-entry*)]
           [map-entry* (filter (lambda (entry)
                                 (map-entry-message? (vector-ref entry 2)))
                               message-entry*)]
           [enum* (collect-enums file)]
           [service* (vector->list* (protobuf-file-descriptor-services file))]
           [exports (append
                     (append-map (lambda (entry) (message-exports file entry)) public-message*)
                     (append-map enum-exports enum*)
                     (append-map service-exports service*)
                     (map (lambda (service)
                            (string-append "register-"
                                           (identifier->kebab
                                            (protobuf-service-descriptor-name service))
                                           "-service!"))
                          service*)
                     (list "protobuf-file-descriptor-bytes"
                           "register-protobuf-file-reflection!"))])
      (let-values ([(port get) (open-string-output-port)])
        (format port "(library ~s~%" (map string->symbol (library-name file)))
        (format port "  (export~%")
        (for-each (lambda (name) (format port "    ~a~%" name)) exports)
        (display
         (string-append
           "  )\n  (import (cspp chez)\n"
           "          (only (cspp utils) pcheck)\n"
           "          (cspp format protobuf)\n"
           "          (cspp net grpc)\n"
           "          (cspp net grpc reflection))\n\n")
         port)
        (format port "  (define protobuf-file-descriptor-bytes ~a)~%~%"
                (bytevector-literal (protobuf-file-descriptor-raw file)))
        (write-doc port "proc" "register-protobuf-file-reflection!"
                   (list
                    "The `register-protobuf-file-reflection!` procedure adds this file to `registry`."
                    "The return value is the supplied gRPC reflection registry."))
        (write-form
         port
         `(define register-protobuf-file-reflection!
            (lambda (registry)
              (pcheck ([grpc-reflection-registry? registry])
                (grpc-reflection-register-file!
                 registry ,(protobuf-file-descriptor-name file)
                 protobuf-file-descriptor-bytes
                 ',(descriptor-symbol-names file)
                 ',(descriptor-service-names file))))))
        (newline port)
        (write-runtime-helpers port)
        (for-each
         (lambda (entry)
           (let ([prefix (vector-ref entry 0)] [enum (vector-ref entry 1)])
             (vector-for-each
              (lambda (value)
                (write-form
                 port
                 `(define ,(string->symbol
                            (string-append prefix "-"
                                           (identifier->kebab (vector-ref value 0))))
                    ,(vector-ref value 1))))
              (protobuf-enum-descriptor-values enum))
             (newline port)))
         enum*)
        (for-each (lambda (entry) (write-map-entry port file entry)) map-entry*)
        (for-each (lambda (entry) (write-message port file entry)) public-message*)
        (for-each (lambda (service) (write-service port file service)) service*)
        (display ")\n" port)
        (cons (proto-output-name (protobuf-file-descriptor-name file)) (get))))))

(define find-file
  (lambda (request name)
    (find (lambda (file) (string=? name (protobuf-file-descriptor-name file)))
          (vector->list* (protobuf-code-generator-request-proto-files request)))))

(define main
  (lambda ()
    (let* ([input (get-bytevector-all (standard-input-port))]
           [request (bytevector->protobuf-code-generator-request input)]
           [file*
            (map (lambda (name)
                   (let ([file (find-file request name)])
                     (unless file
                       (errorf 'protoc-gen-cspp "missing descriptor for ~a" name))
                     (generate-file file)))
                 (vector->list*
                  (protobuf-code-generator-request-file-to-generate request)))])
      (put-bytevector (standard-output-port) (protobuf-code-generator-response file*))
      (flush-output-port (standard-output-port)))))

(main)
