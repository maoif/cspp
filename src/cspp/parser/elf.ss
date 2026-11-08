(library (cspp parser elf)
  (export elf-file? elf-file-identification elf-file-header elf-file-program-headers
          elf-file-sections
          elf-identification? elf-identification-class elf-identification-endianness
          elf-identification-version elf-identification-osabi elf-identification-abi-version
          elf-header? elf-header-type elf-header-machine elf-header-version elf-header-entry
          elf-header-program-header-offset elf-header-section-header-offset elf-header-flags
          elf-header-header-size elf-header-program-header-entry-size
          elf-header-program-header-count elf-header-section-header-entry-size
          elf-header-section-header-count elf-header-section-name-index
          elf-program-header? elf-program-header-type elf-program-header-flags
          elf-program-header-offset elf-program-header-virtual-address
          elf-program-header-physical-address elf-program-header-file-size
          elf-program-header-memory-size elf-program-header-alignment elf-program-header-data
          elf-section? elf-section-header elf-section-content
          elf-section-header? elf-section-header-name-index elf-section-header-name
          elf-section-header-type elf-section-header-flags elf-section-header-address
          elf-section-header-offset elf-section-header-size elf-section-header-link
          elf-section-header-info elf-section-header-address-alignment elf-section-header-entry-size
          elf-raw-section? elf-raw-section-bytes elf-string-table? elf-string-table-bytes
          elf-symbol-table? elf-symbol-table-symbols
          elf-symbol? elf-symbol-name-index elf-symbol-name elf-symbol-info elf-symbol-other
          elf-symbol-section-index elf-symbol-value elf-symbol-size
          elf-relocation-table? elf-relocation-table-relocations
          elf-relocation-table-with-addends?
          elf-relocation? elf-relocation-offset elf-relocation-info elf-relocation-symbol-index
          elf-relocation-type elf-relocation-addend
          elf-relr-table? elf-relr-table-entries
          elf-hash-table? elf-hash-table-buckets elf-hash-table-chains
          elf-group-section? elf-group-section-flags elf-group-section-members
          elf-word-table? elf-word-table-entries
          elf-dynamic-table? elf-dynamic-table-entries
          elf-dynamic-entry? elf-dynamic-entry-tag elf-dynamic-entry-value
          elf-note-table? elf-note-table-notes elf-note? elf-note-name elf-note-type
          elf-note-descriptor
          elf-section-type-name elf-program-type-name elf-machine-name elf-dynamic-tag-name
          parse-elf parse-elf-file)
  (import (cspp chez)
          (cspp file)
          (cspp parser combinator)
          (cspp parser elf types)
          (cspp parser elf binary)
          (cspp parser elf validate)
          (cspp utils))

  #|proc:elf-section-type-name
  The `elf-section-type-name` procedure returns the symbolic name of numeric section `type`, or
  `unknown` when the code is not a standard ELF section type.
  |#
  (define elf-section-type-name
    (lambda (type)
      (pcheck ([natural? type])
              (case type
                [(0) 'null] [(1) 'progbits] [(2) 'symbol-table] [(3) 'string-table]
                [(4) 'relocation-with-addends] [(5) 'hash] [(6) 'dynamic] [(7) 'note]
                [(8) 'no-bits] [(9) 'relocation] [(11) 'dynamic-symbol-table]
                [(14) 'init-array] [(15) 'fini-array] [(16) 'preinit-array]
                [(17) 'group] [(18) 'symbol-table-section-indices] [(19) 'relr]
                [else 'unknown]))))

  #|proc:elf-program-type-name
  The `elf-program-type-name` procedure returns the symbolic name of numeric segment `type`, or
  `unknown` when the code is not a standard ELF program-header type.
  |#
  (define elf-program-type-name
    (lambda (type)
      (pcheck ([natural? type])
              (case type
                [(0) 'null] [(1) 'load] [(2) 'dynamic] [(3) 'interpreter] [(4) 'note]
                [(5) 'shared-library] [(6) 'program-header] [(7) 'tls]
                [else 'unknown]))))

  #|proc:elf-machine-name
  The `elf-machine-name` procedure returns the symbolic name of numeric machine code `machine`,
  or `unknown` when the machine is not known.
  |#
  (define elf-machine-name
    (lambda (machine)
      (pcheck ([natural? machine])
              (case machine
                [(0) 'none] [(2) 'sparc] [(3) 'i386] [(8) 'mips] [(20) 'powerpc]
                [(21) 'powerpc64] [(22) 's390] [(40) 'arm] [(50) 'ia64] [(62) 'x86-64]
                [(183) 'aarch64] [(243) 'riscv] [(258) 'loongarch]
                [else 'unknown]))))

  #|proc:elf-dynamic-tag-name
  The `elf-dynamic-tag-name` procedure returns the symbolic name of integer dynamic `tag`, or
  `unknown` when the tag is not a standard ELF dynamic tag.
  |#
  (define elf-dynamic-tag-name
    (lambda (tag)
      (pcheck ([integer? tag])
              (case tag
                [(0) 'null] [(1) 'needed] [(2) 'pltrelsz] [(3) 'pltgot] [(4) 'hash]
                [(5) 'strtab] [(6) 'symtab] [(7) 'rela] [(8) 'relasz] [(9) 'relaent]
                [(10) 'strsz] [(11) 'syment] [(12) 'init] [(13) 'fini] [(14) 'soname]
                [(15) 'rpath] [(16) 'symbolic] [(17) 'rel] [(18) 'relsz] [(19) 'relent]
                [else 'unknown]))))

  #|proc:parse-elf
  The `parse-elf` procedure decodes ELF bytevector `input` and returns an `elf-file` record.
  Names decode as strict UTF-8 with every scalar retained, including U+FEFF. If a name is
  invalid UTF-8, each byte maps to the character with the same code point. The string-table
  records retain the original bytes independently of this display-oriented name decoding.
  |#
  (define parse-elf
    (lambda (input)
      (pcheck ([bytevector? input])
              (run-binary-parser parser-elf input))))

  #|proc:parse-elf-file
  The `parse-elf-file` procedure decodes the ELF file at string `path` and returns an `elf-file`
  record. Name decoding follows `parse-elf`: UTF-8 where valid, otherwise one character per byte.
  |#
  (define parse-elf-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-binary-file parser-elf path))))
  )
