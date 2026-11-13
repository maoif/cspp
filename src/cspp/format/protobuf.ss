(library (cspp format protobuf)
  (export protobuf-encode-varint protobuf-decode-varint
          protobuf-encode-signed-varint protobuf-decode-signed-varint
          protobuf-encode-zigzag protobuf-decode-zigzag
          protobuf-encode-fixed32 protobuf-decode-fixed32
          protobuf-encode-fixed64 protobuf-decode-fixed64
          protobuf-encode-sfixed32 protobuf-decode-sfixed32
          protobuf-encode-sfixed64 protobuf-decode-sfixed64
          protobuf-encode-float protobuf-decode-float
          protobuf-encode-double protobuf-decode-double
          protobuf-encode-bool protobuf-decode-bool
          protobuf-encode-enum protobuf-decode-enum
          protobuf-encode-bytes protobuf-decode-bytes
          protobuf-encode-string protobuf-decode-string
          protobuf-encode-embedded-message protobuf-decode-embedded-message
          protobuf-encode-tag protobuf-decode-tag
          protobuf-encode-field protobuf-encode-message
          protobuf-decoder? make-protobuf-decoder
          protobuf-decoder-eof? protobuf-decoder-index protobuf-decoder-limit
          protobuf-decoder-recursion-depth protobuf-decoder-recursion-limit
          protobuf-decoder-unknown-fields protobuf-decoder-next-field
          protobuf-decoder-preserve-field!
          protobuf-wire-field? protobuf-wire-field-number
          protobuf-wire-field-wire-type protobuf-wire-field-value protobuf-wire-field-raw
          protobuf-code-generator-request?
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
          bytevector->protobuf-code-generator-request protobuf-code-generator-response)
  (import (cspp format protobuf wire)
          (cspp format protobuf descriptor)))
