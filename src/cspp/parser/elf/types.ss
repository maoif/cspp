(library (cspp parser elf types)
  (export make-elf-file elf-file? elf-file-identification elf-file-header
          elf-file-program-headers elf-file-sections
          make-elf-identification elf-identification? elf-identification-class
          elf-identification-endianness elf-identification-version elf-identification-osabi
          elf-identification-abi-version
          make-elf-header elf-header? elf-header-type elf-header-machine elf-header-version
          elf-header-entry elf-header-program-header-offset elf-header-section-header-offset
          elf-header-flags elf-header-header-size elf-header-program-header-entry-size
          elf-header-program-header-count elf-header-section-header-entry-size
          elf-header-section-header-count elf-header-section-name-index
          make-elf-program-header elf-program-header? elf-program-header-type
          elf-program-header-flags elf-program-header-offset elf-program-header-virtual-address
          elf-program-header-physical-address elf-program-header-file-size
          elf-program-header-memory-size elf-program-header-alignment elf-program-header-data
          make-elf-section elf-section? elf-section-header elf-section-content
          make-elf-section-header elf-section-header? elf-section-header-name-index
          elf-section-header-name elf-section-header-type elf-section-header-flags
          elf-section-header-address elf-section-header-offset elf-section-header-size
          elf-section-header-link elf-section-header-info elf-section-header-address-alignment
          elf-section-header-entry-size
          make-elf-raw-section elf-raw-section? elf-raw-section-bytes
          make-elf-string-table elf-string-table? elf-string-table-bytes
          make-elf-symbol-table elf-symbol-table? elf-symbol-table-symbols
          make-elf-symbol elf-symbol? elf-symbol-name-index elf-symbol-name elf-symbol-info
          elf-symbol-other elf-symbol-section-index elf-symbol-value elf-symbol-size
          make-elf-relocation-table elf-relocation-table? elf-relocation-table-relocations
          elf-relocation-table-with-addends?
          make-elf-relocation elf-relocation? elf-relocation-offset elf-relocation-info
          elf-relocation-symbol-index elf-relocation-type elf-relocation-addend
          make-elf-relr-table elf-relr-table? elf-relr-table-entries
          make-elf-hash-table elf-hash-table? elf-hash-table-buckets elf-hash-table-chains
          make-elf-group-section elf-group-section? elf-group-section-flags
          elf-group-section-members
          make-elf-word-table elf-word-table? elf-word-table-entries
          make-elf-dynamic-table elf-dynamic-table? elf-dynamic-table-entries
          make-elf-dynamic-entry elf-dynamic-entry? elf-dynamic-entry-tag elf-dynamic-entry-value
          make-elf-note-table elf-note-table? elf-note-table-notes
          make-elf-note elf-note? elf-note-name elf-note-type elf-note-descriptor)
  (import (cspp chez)
          (cspp parser private)
          (cspp utils))

  (define elf-value? (lambda (value) #t))
  (define elf-optional-integer? (lambda (value) (or (not value) (integer? value))))


;;;;===----------------------------------------------------------------------===
;;;; ELF file identity and headers
;;;;===----------------------------------------------------------------------===

  ;; Constructors check the declared field predicates, not complete ELF relationships or vector
  ;; element types. Descriptions distinguish those accepted fields from parser-produced values.
  ;; Parsed numeric fields are exact integers. Records retain all supplied contents by reference;
  ;; immutable record fields do not imply immutable strings, vectors, or bytevectors.

  #|proc:make-elf-file
  Creates and returns an `elf-file` record. `identification` is an `elf-identification` record
  describing class and encoding. `header` is an `elf-header` record containing resolved extended
  counts. `program-headers` is a vector; parsers populate it with `elf-program-header` records in
  file order. `sections` is a vector; parsers populate it with `elf-section` records in
  section-index order, including section zero. Supplied strings, vectors, bytevectors, and nested
  records are retained without copying.
  |#
  #|proc:elf-file?
  The `elf-file?` procedure returns whether `object` is an ELF file record. The `object` parameter
  is the object to test.
  |#
  #|proc:elf-file-identification
  Returns the stored `identification` of `elf-file` record `record`. It is an `elf-identification`
  record describing class and encoding.
  |#
  #|proc:elf-file-header
  Returns the stored `header` of `elf-file` record `record`. It is an `elf-header` record
  containing resolved extended counts.
  |#
  #|proc:elf-file-program-headers
  Returns the stored `program-headers` of `elf-file` record `record`. It is a vector; parsers
  populate it with `elf-program-header` records in file order. Mutable contents are returned by
  reference.
  |#
  #|proc:elf-file-sections
  Returns the stored `sections` of `elf-file` record `record`. It is a vector; parsers populate it
  with `elf-section` records in section-index order, including section zero. Mutable contents are
  returned by reference.
  |#
  (define-checked-record-type $elf-file make-elf-file elf-file? elf-file
    $make-elf-file $elf-file?
    ([identification elf-file-identification $elf-file-identification
                     elf-identification?]
     [header elf-file-header $elf-file-header elf-header?]
     [program-headers elf-file-program-headers $elf-file-program-headers vector?]
     [sections elf-file-sections $elf-file-sections vector?]))
  #|proc:make-elf-identification
  Creates and returns an `elf-identification` record. `class` is a symbol; parsed files use
  `elf32` or `elf64`. `endianness` is a symbol; parsed files use `little` or `big`. `version` is
  the natural identification-version code, parsed as 1. `osabi` is the natural numeric
  operating-system/ABI code. `abi-version` is the natural ABI-version code. The constructor checks
  symbols and natural numbers without restricting them to parser-supported encoding codes.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-identification?
  The `elf-identification?` procedure returns whether `object` is an ELF identification record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-identification-class
  Returns the stored `class` of `elf-identification` record `record`. It is a symbol; parsed files
  use `elf32` or `elf64`.
  |#
  #|proc:elf-identification-endianness
  Returns the stored `endianness` of `elf-identification` record `record`. It is a symbol; parsed
  files use `little` or `big`.
  |#
  #|proc:elf-identification-version
  Returns the stored `version` of `elf-identification` record `record`. It is the natural
  identification-version code, parsed as 1.
  |#
  #|proc:elf-identification-osabi
  Returns the stored `osabi` of `elf-identification` record `record`. It is the natural numeric
  operating-system/ABI code.
  |#
  #|proc:elf-identification-abi-version
  Returns the stored `abi-version` of `elf-identification` record `record`. It is the natural
  ABI-version code.
  |#
  (define-checked-record-type $elf-identification
    make-elf-identification elf-identification? elf-identification
    $make-elf-identification $elf-identification?
    ([class elf-identification-class $elf-identification-class symbol?]
     [endianness elf-identification-endianness $elf-identification-endianness symbol?]
     [version elf-identification-version $elf-identification-version natural?]
     [osabi elf-identification-osabi $elf-identification-osabi natural?]
     [abi-version elf-identification-abi-version $elf-identification-abi-version natural?]))
  #|proc:make-elf-header
  Creates and returns an `elf-header` record. `type` is the natural numeric ELF object-type code.
  `machine` is the natural numeric target-machine code. `version` is the natural ELF
  format-version code. `entry` is the natural entry-point virtual address in bytes, or zero if
  absent. `program-header-offset` is the natural byte offset of the program-header table from the
  file start. `section-header-offset` is the natural byte offset of the section-header table from
  the file start. `flags` is the natural architecture-specific flags bitmask. `header-size` is the
  natural ELF-header size in bytes. `program-header-entry-size` is the natural program-header
  stride in bytes. `program-header-count` is the natural program-header count, with extended
  numbering resolved by parsers. `section-header-entry-size` is the natural section-header stride
  in bytes. `section-header-count` is the natural section count including section zero, with
  extended numbering resolved by parsers. `section-name-index` is the natural section index of the
  section-name string table, or zero if absent; parsers resolve extended numbering. Supplied
  strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-header?
  The `elf-header?` procedure returns whether `object` is an ELF header record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-header-type
  Returns the stored `type` of `elf-header` record `record`. It is the natural numeric ELF
  object-type code.
  |#
  #|proc:elf-header-machine
  Returns the stored `machine` of `elf-header` record `record`. It is the natural numeric
  target-machine code.
  |#
  #|proc:elf-header-version
  Returns the stored `version` of `elf-header` record `record`. It is the natural ELF
  format-version code.
  |#
  #|proc:elf-header-entry
  Returns the stored `entry` of `elf-header` record `record`. It is the natural entry-point
  virtual address in bytes, or zero if absent.
  |#
  #|proc:elf-header-program-header-offset
  Returns the stored `program-header-offset` of `elf-header` record `record`. It is the natural
  byte offset of the program-header table from the file start.
  |#
  #|proc:elf-header-section-header-offset
  Returns the stored `section-header-offset` of `elf-header` record `record`. It is the natural
  byte offset of the section-header table from the file start.
  |#
  #|proc:elf-header-flags
  Returns the stored `flags` of `elf-header` record `record`. It is the natural
  architecture-specific flags bitmask.
  |#
  #|proc:elf-header-header-size
  Returns the stored `header-size` of `elf-header` record `record`. It is the natural ELF-header
  size in bytes.
  |#
  #|proc:elf-header-program-header-entry-size
  Returns the stored `program-header-entry-size` of `elf-header` record `record`. It is the
  natural program-header stride in bytes.
  |#
  #|proc:elf-header-program-header-count
  Returns the stored `program-header-count` of `elf-header` record `record`. It is the natural
  program-header count, with extended numbering resolved by parsers.
  |#
  #|proc:elf-header-section-header-entry-size
  Returns the stored `section-header-entry-size` of `elf-header` record `record`. It is the
  natural section-header stride in bytes.
  |#
  #|proc:elf-header-section-header-count
  Returns the stored `section-header-count` of `elf-header` record `record`. It is the natural
  section count including section zero, with extended numbering resolved by parsers.
  |#
  #|proc:elf-header-section-name-index
  Returns the stored `section-name-index` of `elf-header` record `record`. It is the natural
  section index of the section-name string table, or zero if absent; parsers resolve extended
  numbering. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-header make-elf-header elf-header? elf-header
    $make-elf-header $elf-header?
    ([type elf-header-type $elf-header-type natural?]
     [machine elf-header-machine $elf-header-machine natural?]
     [version elf-header-version $elf-header-version natural?]
     [entry elf-header-entry $elf-header-entry natural?]
     [program-header-offset elf-header-program-header-offset
                            $elf-header-program-header-offset natural?]
     [section-header-offset elf-header-section-header-offset
                            $elf-header-section-header-offset natural?]
     [flags elf-header-flags $elf-header-flags natural?]
     [header-size elf-header-header-size $elf-header-header-size natural?]
     [program-header-entry-size elf-header-program-header-entry-size
                                $elf-header-program-header-entry-size natural?]
     [program-header-count elf-header-program-header-count
                           $elf-header-program-header-count natural?]
     [section-header-entry-size elf-header-section-header-entry-size
                                $elf-header-section-header-entry-size natural?]
     [section-header-count elf-header-section-header-count
                           $elf-header-section-header-count natural?]
     [section-name-index elf-header-section-name-index
                         $elf-header-section-name-index natural?]))
  #|proc:make-elf-program-header
  Creates and returns an `elf-program-header` record. `type` is the natural numeric segment-type
  code. `flags` is the natural segment-permission bitmask. `offset` is the natural byte offset of
  the segment from the file start. `virtual-address` is the natural segment virtual address in
  bytes. `physical-address` is the natural segment physical address in bytes. `file-size` is the
  natural segment size in file bytes. `memory-size` is the natural segment size in memory bytes.
  `alignment` is the natural segment alignment in bytes; zero or one imposes no alignment. `data`
  is any supplied value; parsers return a copied bytevector of segment bytes, empty for null
  segments. Supplied strings, vectors, bytevectors, and nested records are retained without
  copying.
  |#
  #|proc:elf-program-header?
  The `elf-program-header?` procedure returns whether `object` is an ELF program header record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-program-header-type
  Returns the stored `type` of `elf-program-header` record `record`. It is the natural numeric
  segment-type code.
  |#
  #|proc:elf-program-header-flags
  Returns the stored `flags` of `elf-program-header` record `record`. It is the natural
  segment-permission bitmask.
  |#
  #|proc:elf-program-header-offset
  Returns the stored `offset` of `elf-program-header` record `record`. It is the natural byte
  offset of the segment from the file start.
  |#
  #|proc:elf-program-header-virtual-address
  Returns the stored `virtual-address` of `elf-program-header` record `record`. It is the natural
  segment virtual address in bytes.
  |#
  #|proc:elf-program-header-physical-address
  Returns the stored `physical-address` of `elf-program-header` record `record`. It is the natural
  segment physical address in bytes.
  |#
  #|proc:elf-program-header-file-size
  Returns the stored `file-size` of `elf-program-header` record `record`. It is the natural
  segment size in file bytes.
  |#
  #|proc:elf-program-header-memory-size
  Returns the stored `memory-size` of `elf-program-header` record `record`. It is the natural
  segment size in memory bytes.
  |#
  #|proc:elf-program-header-alignment
  Returns the stored `alignment` of `elf-program-header` record `record`. It is the natural
  segment alignment in bytes; zero or one imposes no alignment.
  |#
  #|proc:elf-program-header-data
  Returns the stored `data` of `elf-program-header` record `record`. It is any supplied value;
  parsers return a copied bytevector of segment bytes, empty for null segments. Mutable contents
  are returned by reference.
  |#
  (define-checked-record-type $elf-program-header
    make-elf-program-header elf-program-header? elf-program-header
    $make-elf-program-header $elf-program-header?
    ([type elf-program-header-type $elf-program-header-type natural?]
     [flags elf-program-header-flags $elf-program-header-flags natural?]
     [offset elf-program-header-offset $elf-program-header-offset natural?]
     [virtual-address elf-program-header-virtual-address
                      $elf-program-header-virtual-address natural?]
     [physical-address elf-program-header-physical-address
                       $elf-program-header-physical-address natural?]
     [file-size elf-program-header-file-size $elf-program-header-file-size natural?]
     [memory-size elf-program-header-memory-size $elf-program-header-memory-size natural?]
     [alignment elf-program-header-alignment $elf-program-header-alignment natural?]
     [data elf-program-header-data $elf-program-header-data elf-value?]))
;;;;===----------------------------------------------------------------------===
;;;; Sections and typed content
;;;;===----------------------------------------------------------------------===

  #|proc:make-elf-section
  Creates and returns an `elf-section` record. `header` is an `elf-section-header` record.
  `content` is any supplied value; parsers return a typed content record as described below.
  Parsed content is an `elf-raw-section`, `elf-string-table`, `elf-symbol-table`,
  `elf-relocation-table`, `elf-relr-table`, `elf-hash-table`, `elf-group-section`,
  `elf-word-table`, `elf-dynamic-table`, or `elf-note-table`, according to the section type.
  Unsupported types remain raw. Compressed sections expose decoded content while the header
  retains physical metadata. Supplied strings, vectors, bytevectors, and nested records are
  retained without copying.
  |#
  #|proc:elf-section?
  The `elf-section?` procedure returns whether `object` is an ELF section record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-section-header
  Returns the stored `header` of `elf-section` record `record`. It is an `elf-section-header`
  record.
  |#
  #|proc:elf-section-content
  Returns the stored `content` of `elf-section` record `record`. It is any supplied value; parsers
  return a typed content record as described below. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-section make-elf-section elf-section? elf-section
    $make-elf-section $elf-section?
    ([header elf-section-header $elf-section-header elf-section-header?]
     [content elf-section-content $elf-section-content elf-value?]))
  #|proc:make-elf-section-header
  Creates and returns an `elf-section-header` record. `name-index` is the natural byte offset of
  the section name in the section-name string table. `name` is the decoded section-name string,
  empty when unnamed. `type` is the natural numeric section-type code. `flags` is the natural
  section-flags bitmask. `address` is the natural section virtual address in bytes, or zero if not
  allocated. `offset` is the natural physical byte offset from the file start. `size` is the
  natural physical section size in bytes, or memory size for a no-bits section. `link` is the
  natural section-type-dependent linked section index, or zero when unused. `info` is the natural
  section-type-dependent auxiliary value, such as a symbol or section index. `address-alignment`
  is the natural alignment in bytes; zero or one imposes no alignment. `entry-size` is the natural
  fixed entry stride in bytes, or zero for variable/no entries. For compressed sections, `offset`,
  `size`, and `flags` retain the physical file representation rather than the decompressed payload
  dimensions. Supplied strings, vectors, bytevectors, and nested records are retained without
  copying.
  |#
  #|proc:elf-section-header?
  The `elf-section-header?` procedure returns whether `object` is an ELF section header record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-section-header-name-index
  Returns the stored `name-index` of `elf-section-header` record `record`. It is the natural byte
  offset of the section name in the section-name string table. Mutable contents are returned by
  reference.
  |#
  #|proc:elf-section-header-name
  Returns the stored `name` of `elf-section-header` record `record`. It is the decoded
  section-name string, empty when unnamed. Mutable contents are returned by reference.
  |#
  #|proc:elf-section-header-type
  Returns the stored `type` of `elf-section-header` record `record`. It is the natural numeric
  section-type code.
  |#
  #|proc:elf-section-header-flags
  Returns the stored `flags` of `elf-section-header` record `record`. It is the natural
  section-flags bitmask.
  |#
  #|proc:elf-section-header-address
  Returns the stored `address` of `elf-section-header` record `record`. It is the natural section
  virtual address in bytes, or zero if not allocated.
  |#
  #|proc:elf-section-header-offset
  Returns the stored `offset` of `elf-section-header` record `record`. It is the natural physical
  byte offset from the file start.
  |#
  #|proc:elf-section-header-size
  Returns the stored `size` of `elf-section-header` record `record`. It is the natural physical
  section size in bytes, memory size for a no-bits section, or the extended section count in
  section zero.
  |#
  #|proc:elf-section-header-link
  Returns the stored `link` of `elf-section-header` record `record`. It is the natural
  section-type-dependent linked section index, or zero when unused.
  |#
  #|proc:elf-section-header-info
  Returns the stored `info` of `elf-section-header` record `record`. It is the natural
  section-type-dependent auxiliary value, such as a symbol or section index.
  |#
  #|proc:elf-section-header-address-alignment
  Returns the stored `address-alignment` of `elf-section-header` record `record`. It is the
  natural alignment in bytes; zero or one imposes no alignment.
  |#
  #|proc:elf-section-header-entry-size
  Returns the stored `entry-size` of `elf-section-header` record `record`. It is the natural fixed
  entry stride in bytes, or zero for variable/no entries.
  |#
  (define-checked-record-type $elf-section-header-record
    make-elf-section-header elf-section-header? elf-section-header
    $make-elf-section-header $elf-section-header?
    ([name-index elf-section-header-name-index $elf-section-header-name-index natural?]
     [name elf-section-header-name $elf-section-header-name string?]
     [type elf-section-header-type $elf-section-header-type natural?]
     [flags elf-section-header-flags $elf-section-header-flags natural?]
     [address elf-section-header-address $elf-section-header-address natural?]
     [offset elf-section-header-offset $elf-section-header-offset natural?]
     [size elf-section-header-size $elf-section-header-size natural?]
     [link elf-section-header-link $elf-section-header-link natural?]
     [info elf-section-header-info $elf-section-header-info natural?]
     [address-alignment elf-section-header-address-alignment
                        $elf-section-header-address-alignment natural?]
     [entry-size elf-section-header-entry-size $elf-section-header-entry-size natural?]))
  #|proc:make-elf-raw-section
  Creates and returns an `elf-raw-section` record. `bytes` is a bytevector of uninterpreted
  section bytes; parsed null/no-bits sections have no payload. Supplied strings, vectors,
  bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-raw-section?
  The `elf-raw-section?` procedure returns whether `object` is a raw section record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-raw-section-bytes
  Returns the stored `bytes` of `elf-raw-section` record `record`. It is a bytevector of
  uninterpreted section bytes; parsed null/no-bits sections have no payload. Mutable contents are
  returned by reference.
  |#
  (define-checked-record-type $elf-raw-section make-elf-raw-section elf-raw-section? elf-raw-section
    $make-elf-raw-section $elf-raw-section?
    ([bytes elf-raw-section-bytes $elf-raw-section-bytes bytevector?]))

  #|proc:make-elf-string-table
  Creates and returns an `elf-string-table` record. `bytes` is a bytevector of raw NUL-delimited
  string data, retaining original name encodings. Supplied strings, vectors, bytevectors, and
  nested records are retained without copying.
  |#
  #|proc:elf-string-table?
  The `elf-string-table?` procedure returns whether `object` is a string table record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-string-table-bytes
  Returns the stored `bytes` of `elf-string-table` record `record`. It is a bytevector of raw
  NUL-delimited string data, retaining original name encodings. Mutable contents are returned by
  reference.
  |#
  (define-checked-record-type $elf-string-table make-elf-string-table elf-string-table? elf-string-table
    $make-elf-string-table $elf-string-table?
    ([bytes elf-string-table-bytes $elf-string-table-bytes bytevector?]))

;;;;===----------------------------------------------------------------------===
;;;; Symbols and relocations
;;;;===----------------------------------------------------------------------===

  #|proc:make-elf-symbol-table
  Creates and returns an `elf-symbol-table` record. `symbols` is a vector; parsers populate it
  with `elf-symbol` records in symbol-index order. Supplied strings, vectors, bytevectors, and
  nested records are retained without copying.
  |#
  #|proc:elf-symbol-table?
  The `elf-symbol-table?` procedure returns whether `object` is a symbol table record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-symbol-table-symbols
  Returns the stored `symbols` of `elf-symbol-table` record `record`. It is a vector; parsers
  populate it with `elf-symbol` records in symbol-index order. Mutable contents are returned by
  reference.
  |#
  (define-checked-record-type $elf-symbol-table make-elf-symbol-table elf-symbol-table? elf-symbol-table
    $make-elf-symbol-table $elf-symbol-table?
    ([symbols elf-symbol-table-symbols $elf-symbol-table-symbols vector?]))
  #|proc:make-elf-symbol
  Creates and returns an `elf-symbol` record. `name-index` is the natural byte offset into the
  linked string table. `name` is the decoded symbol-name string, empty when unnamed. `info` is the
  natural raw binding/type byte (binding in the high nibble). `other` is the natural raw
  visibility/target-specific byte. `section-index` is the natural section index or reserved
  special index; parsers resolve SHN_XINDEX via its companion table. `value` is the natural symbol
  value, interpreted as an address or section offset according to ELF type. `size` is the natural
  symbol size in bytes, or zero when unspecified. Supplied strings, vectors, bytevectors, and
  nested records are retained without copying.
  |#
  #|proc:elf-symbol?
  The `elf-symbol?` procedure returns whether `object` is an ELF symbol record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-symbol-name-index
  Returns the stored `name-index` of `elf-symbol` record `record`. It is the natural byte offset
  into the linked string table. Mutable contents are returned by reference.
  |#
  #|proc:elf-symbol-name
  Returns the stored `name` of `elf-symbol` record `record`. It is the decoded symbol-name string,
  empty when unnamed. Mutable contents are returned by reference.
  |#
  #|proc:elf-symbol-info
  Returns the stored `info` of `elf-symbol` record `record`. It is the natural raw binding/type
  byte (binding in the high nibble).
  |#
  #|proc:elf-symbol-other
  Returns the stored `other` of `elf-symbol` record `record`. It is the natural raw
  visibility/target-specific byte.
  |#
  #|proc:elf-symbol-section-index
  Returns the stored `section-index` of `elf-symbol` record `record`. It is the natural section
  index or reserved special index; parsers resolve SHN_XINDEX via its companion table.
  |#
  #|proc:elf-symbol-value
  Returns the stored `value` of `elf-symbol` record `record`. It is the natural symbol value,
  interpreted as an address or section offset according to ELF type.
  |#
  #|proc:elf-symbol-size
  Returns the stored `size` of `elf-symbol` record `record`. It is the natural symbol size in
  bytes, or zero when unspecified.
  |#
  (define-checked-record-type $elf-symbol make-elf-symbol elf-symbol? elf-symbol
    $make-elf-symbol $elf-symbol?
    ([name-index elf-symbol-name-index $elf-symbol-name-index natural?]
     [name elf-symbol-name $elf-symbol-name string?]
     [info elf-symbol-info $elf-symbol-info natural?]
     [other elf-symbol-other $elf-symbol-other natural?]
     [section-index elf-symbol-section-index $elf-symbol-section-index natural?]
     [value elf-symbol-value $elf-symbol-value natural?]
     [size elf-symbol-size $elf-symbol-size natural?]))
  #|proc:make-elf-relocation-table
  Creates and returns an `elf-relocation-table` record. `relocations` is a vector; parsers
  populate it with `elf-relocation` records in table order. `with-addends?` is a boolean: `#t` for
  RELA entries with explicit addends and `#f` for REL entries. Supplied strings, vectors,
  bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-relocation-table?
  The `elf-relocation-table?` procedure returns whether `object` is an ELF relocation table
  record. The `object` parameter is the object to test.
  |#
  #|proc:elf-relocation-table-relocations
  Returns the stored `relocations` of `elf-relocation-table` record `record`. It is a vector;
  parsers populate it with `elf-relocation` records in table order. Mutable contents are returned
  by reference.
  |#
  #|proc:elf-relocation-table-with-addends?
  Returns the stored `with-addends?` of `elf-relocation-table` record `record`. It is a boolean:
  `#t` for RELA entries with explicit addends and `#f` for REL entries.
  |#
  (define-checked-record-type $elf-relocation-table
    make-elf-relocation-table elf-relocation-table? elf-relocation-table
    $make-elf-relocation-table $elf-relocation-table?
    ([relocations elf-relocation-table-relocations
                  $elf-relocation-table-relocations vector?]
     [with-addends? elf-relocation-table-with-addends?
                    $elf-relocation-table-with-addends? boolean?]))
  #|proc:make-elf-relocation
  Creates and returns an `elf-relocation` record. `offset` is the natural relocation target
  address or section-relative byte offset, according to ELF type. `info` is the natural raw
  relocation info word read using the file byte order. `symbol-index` is the natural decoded index
  in the linked symbol table. `type` is the natural decoded machine-specific relocation code.
  `addend` is an integer explicit RELA addend, or `#f` for REL without an explicit addend.
  Machine-specific layouts, including MIPS64EL, are decoded into `symbol-index` and `type`
  separately from the raw `info` word. Supplied strings, vectors, bytevectors, and nested records
  are retained without copying.
  |#
  #|proc:elf-relocation?
  The `elf-relocation?` procedure returns whether `object` is an ELF relocation record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-relocation-offset
  Returns the stored `offset` of `elf-relocation` record `record`. It is the natural relocation
  target address or section-relative byte offset, according to ELF type.
  |#
  #|proc:elf-relocation-info
  Returns the stored `info` of `elf-relocation` record `record`. It is the natural raw relocation
  info word read using the file byte order. Machine-specific layouts, including MIPS64EL, are
  decoded separately into the symbol-index and type fields.
  |#
  #|proc:elf-relocation-symbol-index
  Returns the stored `symbol-index` of `elf-relocation` record `record`. It is the natural decoded
  index in the linked symbol table.
  |#
  #|proc:elf-relocation-type
  Returns the stored `type` of `elf-relocation` record `record`. It is the natural decoded
  machine-specific relocation code.
  |#
  #|proc:elf-relocation-addend
  Returns the stored `addend` of `elf-relocation` record `record`. It is an integer explicit RELA
  addend, or `#f` for REL without an explicit addend.
  |#
  (define-checked-record-type $elf-relocation make-elf-relocation elf-relocation? elf-relocation
    $make-elf-relocation $elf-relocation?
    ([offset elf-relocation-offset $elf-relocation-offset natural?]
     [info elf-relocation-info $elf-relocation-info natural?]
     [symbol-index elf-relocation-symbol-index $elf-relocation-symbol-index natural?]
     [type elf-relocation-type $elf-relocation-type natural?]
     [addend elf-relocation-addend $elf-relocation-addend elf-optional-integer?]))
;;;;===----------------------------------------------------------------------===
;;;; Packed relocations and auxiliary tables
;;;;===----------------------------------------------------------------------===

  #|proc:make-elf-relr-table
  Creates and returns an `elf-relr-table` record. `entries` is a vector; parsers populate it with
  unsigned native-class words encoding RELR addresses/bitmaps, not expanded relocation records.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-relr-table?
  The `elf-relr-table?` procedure returns whether `object` is a RELR table record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-relr-table-entries
  Returns the stored `entries` of `elf-relr-table` record `record`. It is a vector; parsers
  populate it with unsigned native-class words encoding RELR addresses/bitmaps, not expanded
  relocation records. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-relr-table make-elf-relr-table elf-relr-table? elf-relr-table
    $make-elf-relr-table $elf-relr-table?
    ([entries elf-relr-table-entries $elf-relr-table-entries vector?]))

  #|proc:make-elf-hash-table
  Creates and returns an `elf-hash-table` record. `buckets` is a vector; parsers populate it with
  unsigned 32-bit symbol indexes for SysV hash buckets. `chains` is a vector; parsers populate it
  with unsigned 32-bit symbol indexes for SysV hash chains. Supplied strings, vectors,
  bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-hash-table?
  The `elf-hash-table?` procedure returns whether `object` is an ELF hash table record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-hash-table-buckets
  Returns the stored `buckets` of `elf-hash-table` record `record`. It is a vector; parsers
  populate it with unsigned 32-bit symbol indexes for SysV hash buckets. Mutable contents are
  returned by reference.
  |#
  #|proc:elf-hash-table-chains
  Returns the stored `chains` of `elf-hash-table` record `record`. It is a vector; parsers
  populate it with unsigned 32-bit symbol indexes for SysV hash chains. Mutable contents are
  returned by reference.
  |#
  (define-checked-record-type $elf-hash-table make-elf-hash-table elf-hash-table? elf-hash-table
    $make-elf-hash-table $elf-hash-table?
    ([buckets elf-hash-table-buckets $elf-hash-table-buckets vector?]
     [chains elf-hash-table-chains $elf-hash-table-chains vector?]))

  #|proc:make-elf-group-section
  Creates and returns an `elf-group-section` record. `flags` is the natural group-flags word,
  including bit 0 for COMDAT. `members` is a vector; parsers populate it with nonzero section
  indexes in group order. Supplied strings, vectors, bytevectors, and nested records are retained
  without copying.
  |#
  #|proc:elf-group-section?
  The `elf-group-section?` procedure returns whether `object` is a section group record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-group-section-flags
  Returns the stored `flags` of `elf-group-section` record `record`. It is the natural group-flags
  word, including bit 0 for COMDAT.
  |#
  #|proc:elf-group-section-members
  Returns the stored `members` of `elf-group-section` record `record`. It is a vector; parsers
  populate it with nonzero section indexes in group order. Mutable contents are returned by
  reference.
  |#
  (define-checked-record-type $elf-group-section
    make-elf-group-section elf-group-section? elf-group-section
    $make-elf-group-section $elf-group-section?
    ([flags elf-group-section-flags $elf-group-section-flags natural?]
     [members elf-group-section-members $elf-group-section-members vector?]))

  #|proc:make-elf-word-table
  Creates and returns an `elf-word-table` record. `entries` is a vector; parsers use unsigned
  address words for init/fini/preinit arrays or unsigned 32-bit section indexes for SHNDX tables.
  Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-word-table?
  The `elf-word-table?` procedure returns whether `object` is a word table record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-word-table-entries
  Returns the stored `entries` of `elf-word-table` record `record`. It is a vector; parsers use
  unsigned address words for init/fini/preinit arrays or unsigned 32-bit section indexes for SHNDX
  tables. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-word-table make-elf-word-table elf-word-table? elf-word-table
    $make-elf-word-table $elf-word-table?
    ([entries elf-word-table-entries $elf-word-table-entries vector?]))

  #|proc:make-elf-dynamic-table
  Creates and returns an `elf-dynamic-table` record. `entries` is a vector; parsers populate it
  with `elf-dynamic-entry` records in table order, retaining null entries. Supplied strings,
  vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-dynamic-table?
  The `elf-dynamic-table?` procedure returns whether `object` is a dynamic table record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-dynamic-table-entries
  Returns the stored `entries` of `elf-dynamic-table` record `record`. It is a vector; parsers
  populate it with `elf-dynamic-entry` records in table order, retaining null entries. Mutable
  contents are returned by reference.
  |#
  (define-checked-record-type $elf-dynamic-table
    make-elf-dynamic-table elf-dynamic-table? elf-dynamic-table
    $make-elf-dynamic-table $elf-dynamic-table?
    ([entries elf-dynamic-table-entries $elf-dynamic-table-entries vector?]))

  #|proc:make-elf-dynamic-entry
  Creates and returns an `elf-dynamic-entry` record. `tag` is an integer signed dynamic tag code.
  `value` is the natural unsigned dynamic value, interpreted by tag as an address, size, index, or
  flags. Supplied strings, vectors, bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-dynamic-entry?
  The `elf-dynamic-entry?` procedure returns whether `object` is a dynamic entry record. The
  `object` parameter is the object to test.
  |#
  #|proc:elf-dynamic-entry-tag
  Returns the stored `tag` of `elf-dynamic-entry` record `record`. It is an integer signed dynamic
  tag code.
  |#
  #|proc:elf-dynamic-entry-value
  Returns the stored `value` of `elf-dynamic-entry` record `record`. It is the natural unsigned
  dynamic value, interpreted by tag as an address, size, index, or flags.
  |#
  (define-checked-record-type $elf-dynamic-entry
    make-elf-dynamic-entry elf-dynamic-entry? elf-dynamic-entry
    $make-elf-dynamic-entry $elf-dynamic-entry?
    ([tag elf-dynamic-entry-tag $elf-dynamic-entry-tag integer?]
     [value elf-dynamic-entry-value $elf-dynamic-entry-value natural?]))

;;;;===----------------------------------------------------------------------===
;;;; ELF notes
;;;;===----------------------------------------------------------------------===

  #|proc:make-elf-note-table
  Creates and returns an `elf-note-table` record. `notes` is a vector; parsers populate it with
  `elf-note` records in source order. Supplied strings, vectors, bytevectors, and nested records
  are retained without copying.
  |#
  #|proc:elf-note-table?
  The `elf-note-table?` procedure returns whether `object` is a note table record. The `object`
  parameter is the object to test.
  |#
  #|proc:elf-note-table-notes
  Returns the stored `notes` of `elf-note-table` record `record`. It is a vector; parsers populate
  it with `elf-note` records in source order. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-note-table make-elf-note-table elf-note-table? elf-note-table
    $make-elf-note-table $elf-note-table?
    ([notes elf-note-table-notes $elf-note-table-notes vector?]))
  #|proc:make-elf-note
  Creates and returns an `elf-note` record. `name` is the decoded owner-name string without its
  final NUL terminator. `type` is the natural owner-specific numeric note-type code. `descriptor`
  is a bytevector of descriptor bytes excluding alignment padding. Supplied strings, vectors,
  bytevectors, and nested records are retained without copying.
  |#
  #|proc:elf-note?
  The `elf-note?` procedure returns whether `object` is an ELF note record. The `object` parameter
  is the object to test.
  |#
  #|proc:elf-note-name
  Returns the stored `name` of `elf-note` record `record`. It is the decoded owner-name string
  without its final NUL terminator. Mutable contents are returned by reference.
  |#
  #|proc:elf-note-type
  Returns the stored `type` of `elf-note` record `record`. It is the natural owner-specific
  numeric note-type code.
  |#
  #|proc:elf-note-descriptor
  Returns the stored `descriptor` of `elf-note` record `record`. It is a bytevector of descriptor
  bytes excluding alignment padding. Mutable contents are returned by reference.
  |#
  (define-checked-record-type $elf-note make-elf-note elf-note? elf-note
    $make-elf-note $elf-note?
    ([name elf-note-name $elf-note-name string?]
     [type elf-note-type $elf-note-type natural?]
     [descriptor elf-note-descriptor $elf-note-descriptor bytevector?]))
  )
