;; Included by parser-elf.ss. Fixtures are reproducible with data/elf-review/generate.py.
(define elf-review-file
  (lambda (name) (parse-elf-file (string-append "data/elf-review/" name ".elf"))))

(mat parser-elf-symbolless-relocations

     (for-all
      (lambda (tag)
        (for-all
         (lambda (kind)
           (let* ([table (elf-section-content
                          (vector-ref (elf-file-sections
                                       (elf-review-file (string-append "symbolless-" kind tag))) 1))]
                  [entry (vector-ref (elf-relocation-table-relocations table) 0)])
             (and (= 0 (elf-relocation-symbol-index entry))
                  (= 8 (elf-relocation-type entry))
                  (= #x1000 (elf-relocation-offset entry))
                  (equal? (if (string=? kind "rela-") -4 #f)
                          (elf-relocation-addend entry)))))
         '("rel-" "rela-")))
      '("32le" "32be" "64le" "64be"))

     ;; Error case: a nonzero symbol index still requires a linked symbol table.
     (error?
      (let ([bytes (read-u8vec "data/elf-review/symbolless-rela-64le.elf")])
        (bytevector-u64-set! bytes 200 #x100000008 (endianness little))
        (parse-elf bytes)))

     ;; Error case: a nonzero link cannot name a relocation section as a symbol table.
     (error?
      (let ([bytes (read-u8vec "data/elf-review/symbolless-rela-64le.elf")])
        (bytevector-u32-set! bytes (+ 128 40) 1 (endianness little))
        (parse-elf bytes))))

(mat parser-elf-name-review

     (let ([sections (elf-file-sections (elf-review-file "nonutf8-section"))])
       (string=? "\xff;" (elf-section-header-name
                           (elf-section-header (vector-ref sections 2)))))

     (let* ([sections (elf-file-sections (elf-review-file "nonutf8-symbol"))]
            [symbols (elf-symbol-table-symbols
                      (elf-section-content (vector-ref sections 2)))])
       (string=? "\xff;" (elf-symbol-name (vector-ref symbols 1))))

     (let ([sections (elf-file-sections (elf-review-file "bom-section"))])
       (string=? "\xfeff;A" (elf-section-header-name
                             (elf-section-header (vector-ref sections 2)))))

     (let ([sections (elf-file-sections (elf-review-file "utf8-section"))])
       (string=? "\xe9;" (elf-section-header-name
                           (elf-section-header (vector-ref sections 2)))))

     (elf-file? (elf-review-file "empty-symbol-strings"))

     (elf-file? (elf-review-file "empty-shstr"))

     (let ([program (vector-ref (elf-file-program-headers
                                (elf-review-file "null-segment")) 0)])
       (and (= 0 (elf-program-header-type program))
            (= 0 (bytevector-length (elf-program-header-data program)))))

     (let ([section (vector-ref (elf-file-sections
                                (elf-review-file "nonfirst-null")) 1)])
       (and (= 0 (elf-section-header-type (elf-section-header section)))
            (elf-raw-section? (elf-section-content section))))

     ;; Inactive-section flags and offsets remain undefined, including SHF_COMPRESSED.
     (let ([bytes (read-u8vec "data/elf-review/nonfirst-null.elf")])
       (bytevector-u64-set! bytes (+ 128 8) #x800 (endianness little))
       (elf-raw-section?
        (elf-section-content (vector-ref (elf-file-sections (parse-elf bytes)) 1))))

     (let* ([sections (elf-file-sections (elf-review-file "note-padding"))]
            [note (vector-ref (elf-note-table-notes
                              (elf-section-content (vector-ref sections 1))) 0)])
       (and (string=? "A" (elf-note-name note))
            (equal? #vu8(88) (elf-note-descriptor note))))

     )

(mat parser-elf-validation-review

     ;; Error case: SHN_XINDEX must resolve to an actual section, not a reserved 16-bit code.
     (error? (elf-review-file "reserved-xindex"))

     ;; Error case: duplicate companion sections cannot disable unused-index validation.
     (error? (elf-review-file "duplicate-shndx"))

     ;; Error case: group members must reference existing sections.
     (error? (elf-review-file "bad-group-index"))

     ;; Error case: hash buckets and chains must reference the linked symbol table.
     (error? (elf-review-file "bad-hash-index"))

     ;; Error case: the first-nonlocal symbol index cannot exceed the symbol count.
     (error? (elf-review-file "bad-symbol-info"))

     ;; Error case: no section-name table can be selected without a section table.
     (error? (elf-review-file "shstr-no-sections"))

     ;; Error case: every field of the undefined symbol zero must be zero.
     (error? (parse-elf (rich-elf-with-u32 208 1)))

     ;; Error case: a local symbol cannot appear after the first nonlocal symbol boundary.
     (error?
      (let ([bytes (rich-elf64le)])
        (bytevector-u8-set! bytes 236 2)
        (parse-elf bytes)))

     ;; Error case: group signatures must reference an existing linked symbol.
     (error?
      (let ([bytes (read-u8vec "data/elf-review/bad-group-index.elf")])
        (bytevector-u32-set! bytes (+ 64 (* 3 64) 44) 999 (endianness little))
        (parse-elf bytes)))

     )

(define elf-reserved-header
  (lambda (name-index?)
    (let* ([count (if name-index? #xff01 #xff00)]
           [bytes (make-bytevector (+ 64 (* count 64)) 0)]
           [little (endianness little)])
      (bytevector-copy! (minimal-elf64le) 0 bytes 0 64)
      (bytevector-u64-set! bytes 40 64 little)
      (bytevector-u16-set! bytes 60 (if name-index? 0 count) little)
      (when name-index?
        (bytevector-u64-set! bytes (+ 64 32) count little)
        (bytevector-u16-set! bytes 62 #xff00 little)
        (bytevector-u32-set! bytes (+ 64 (* #xff00 64) 4) 3 little))
      bytes)))

(mat parser-elf-section-relationships

     (elf-file? (elf-review-file "group-valid"))

     (elf-file? (elf-review-file "nobits-overlap"))

     ;; Error case: ordinary string tables must start with NUL.
     (error? (elf-review-file "string-table-missing-first-nul"))

     ;; Error case: ordinary string tables must end with NUL.
     (error? (elf-review-file "string-table-missing-last-nul"))

     ;; Error case: every section-group member must carry SHF_GROUP.
     (error? (elf-review-file "group-missing-member-flag"))

     ;; Error case: the same section cannot belong to two distinct groups.
     (error? (elf-review-file "group-multiple-membership"))

     ;; Error case: section groups occur only in relocatable objects.
     (error? (elf-review-file "group-in-executable"))

     ;; Error case: nonempty file-backed section ranges cannot overlap.
     (error? (elf-review-file "overlapping-sections"))

     ;; Error case: reserved section counts require extended encoding.
     (error? (parse-elf (elf-reserved-header #f)))

     ;; Error case: reserved section-name indices require SHN_XINDEX.
     (error? (parse-elf (elf-reserved-header #t))))

(mat parser-elf-group-order

     ;; Error case: a group section header must precede every member header.
     (error? (elf-review-file "group-after-member"))

     ;; Error case: a section marked SHF_GROUP must have a referring group.
     (error? (elf-review-file "group-orphan"))

     ;; Error case: SHF_GROUP is restricted to relocatable objects, even without a group table.
     (error? (elf-review-file "group-orphan-executable")))

(mat parser-elf-note-alignment

     (for-all
      (lambda (name)
        (let* ([sections (elf-file-sections (elf-review-file name))]
               [notes (elf-note-table-notes (elf-section-content (vector-ref sections 1)))])
          (and (= 1 (vector-length notes))
               (string=? "ABCD" (elf-note-name (vector-ref notes 0)))
               (equal? #vu8(88) (elf-note-descriptor (vector-ref notes 0))))))
      '("note-align4-32le" "note-align4-32be" "note-align4-64le" "note-align4-64be"
        "note-align8-64le" "note-align8-64be"))

     ;; Error case: an eight-aligned note must include all of its final padding.
     (error? (elf-review-file "note-short8-64le"))

     ;; Error case: big-endian notes obey the same eight-byte padding rule.
     (error? (elf-review-file "note-short8-64be")))

(mat parser-elf-compressed-sections

     (mat-requires (zlib)
       (for-all
        (lambda (name)
          (let* ([sections (elf-file-sections (elf-review-file name))]
                 [section (vector-ref sections 1)])
            (and (equal? #vu8(0 120 0) (elf-string-table-bytes (elf-section-content section)))
                 ;; Public headers retain the compressed flag and physical file size.
                 (logbit? 11 (elf-section-header-flags (elf-section-header section)))
                 (> (elf-section-header-size (elf-section-header section)) 3))))
        '("compressed-strings-32le" "compressed-strings-32be"
          "compressed-strings-64le" "compressed-strings-64be" "compressed-alloc")))

     (mat-requires (zlib)
       (let* ([sections (elf-file-sections (elf-review-file "compressed-symbol-strings"))]
              [symbols (elf-symbol-table-symbols (elf-section-content (vector-ref sections 2)))])
         (string=? "x" (elf-symbol-name (vector-ref symbols 1)))))

     (mat-requires (zlib)
       (let ([sections (elf-file-sections (elf-review-file "compressed-section-names"))])
         (string=? ".text" (elf-section-header-name
                            (elf-section-header (vector-ref sections 2))))))

     (mat-requires (zlib)
       (let ([sections (elf-file-sections (elf-review-file "compressed-empty"))])
         (equal? #vu8() (elf-string-table-bytes
                        (elf-section-content (vector-ref sections 1))))))

     (mat-requires (zlib)
       (let* ([sections (elf-file-sections (elf-review-file "compressed-note"))]
              [notes (elf-note-table-notes (elf-section-content (vector-ref sections 1)))])
         (and (= 1 (vector-length notes))
              (equal? #vu8(88) (elf-note-descriptor (vector-ref notes 0))))))

     ;; Error cases: invalid metadata and unsupported methods fail without needing zlib.
     (for-all
      (lambda (name)
        (guard (err [(parser-error? err) #t] [else #f])
          (elf-review-file name)
          #f))
      '("compressed-bad-method" "compressed-bad-alignment" "compressed-bad-header"
        "compressed-bad-huge-size" "compressed-bad-reserved"
        "compressed-alloc-exec" "compressed-alloc-shared" "compressed-nobits"))

     ;; Error cases: incomplete streams, trailing bytes, and a mismatched decoded size fail.
     (mat-requires (zlib)
       (for-all
        (lambda (name)
          (guard (err [(parser-error? err) #t] [else #f])
            (elf-review-file name)
            #f))
        '("compressed-bad-size" "compressed-bad-truncated" "compressed-bad-trailing"))))

(mat parser-elf-mips-relocations

     (for-all
      (lambda (tag)
        (for-all
         (lambda (kind)
           (for-all
            (lambda (case)
              (let* ([sections (elf-file-sections
                                (elf-review-file
                                 (string-append "mips-" kind (car case) tag)))]
                     [table (elf-section-content (vector-ref sections 3))]
                     [entry (vector-ref (elf-relocation-table-relocations table) 0)])
                (and (= 1 (elf-relocation-symbol-index entry))
                     (= (cadr case) (elf-relocation-type entry))
                     (= #x1000 (elf-relocation-offset entry))
                     (= (if (string=? tag "-64le") (caddr case) (cadddr case))
                        (elf-relocation-info entry))
                     (equal? (if (string=? kind "rela-") -4 #f)
                             (elf-relocation-addend entry)))))
            '(("ordinary" 18 #x1200000000000001 #x100000012)
              ("none" 0 1 #x100000000)
              ("compound" #x01020304 #x0403020100000001 #x101020304))))
         '("rel-" "rela-")))
      '("-64le" "-64be"))

     ;; Error case: the decoded MIPS symbol must still be inside its linked symbol table.
     (for-all
      (lambda (tag)
        (for-all (lambda (kind)
                   (guard (err [(parser-error? err) #t])
                     (elf-review-file (string-append "mips-" kind "bad-symbol" tag))
                     #f))
                 '("rel-" "rela-")))
      '("-64le" "-64be")))

(mat parser-elf-section-structure

     ;; Error cases: duplicate singleton tables and misaligned section addresses are rejected.
     (for-all
      (lambda (tag)
        (for-all
         (lambda (name)
           (guard (condition [(parser-error? condition) #t] [else #f])
             (elf-review-file (string-append name "-" tag))
             #f))
         '("duplicate-symtab" "duplicate-dynsym" "duplicate-hash" "duplicate-dynamic"
           "misaligned-address")))
      '("32le" "32be" "64le" "64be"))

     ;; One table of each kind, multiple string tables, and unconstrained addresses are valid.
     (for-all
      (lambda (tag)
        (for-all (lambda (name)
                   (elf-file? (elf-review-file (string-append name "-" tag))))
                 '("single-tables" "multiple-strtab" "aligned-address"
                   "unconstrained-address" "unit-alignment-address")))
      '("32le" "32be" "64le" "64be")))

(define elf-review-rejected?
  (lambda (bytes)
    (guard (condition [(parser-error? condition) #t] [else #f])
      (parse-elf bytes)
      #f)))

(mat parser-elf-flag-indices

     ;; Error cases: flag-declared section indices must name existing sections.
     (for-all
      (lambda (tag wide? endian)
        (let* ([original (read-u8vec (string-append "data/elf-review/aligned-address-" tag ".elf"))]
               [header (+ (if wide? 64 52) (if wide? 64 40))])
          (for-all
           (lambda (flag field)
             (for-all
              (lambda (index)
                (let ([bytes (bytevector-copy original)])
                  (if wide?
                      (bytevector-u64-set! bytes (+ header 8) flag endian)
                      (bytevector-u32-set! bytes (+ header 8) flag endian))
                  (bytevector-u32-set! bytes (+ header field) index endian)
                  (if (< index 2)
                      (elf-file? (parse-elf bytes))
                      (elf-review-rejected? bytes))))
              '(0 1 2 4294967295)))
           '(64 128) (if wide? '(44 40) '(28 24)))))
      '("32le" "32be" "64le" "64be") '(#f #f #t #t)
      (list (endianness little) (endianness big) (endianness little) (endianness big))))

(mat parser-elf-fixed-entry-sizes

     ;; Error cases: zero and incorrect entry sizes cannot describe fixed-size typed tables.
     (for-all
      (lambda (tag wide? endian)
        (for-all
         (lambda (name)
           (let* ([original (read-u8vec (string-append "data/elf-review/" name tag ".elf"))]
                  [file (parse-elf original)] [headers (vector-map elf-section-header (elf-file-sections file))]
                  [header-size (if wide? 64 52)] [section-size (if wide? 64 40)])
             (let loop ([index 1])
               (or (= index (vector-length headers))
                   (and
                    (let ([type (elf-section-header-type (vector-ref headers index))])
                      (or (not (memv type '(4 5 6 9 14 15 16 17 19)))
                          (for-all
                           (lambda (size)
                             (let ([bytes (bytevector-copy original)]
                                   [at (+ header-size (* section-size index)
                                          (if wide? 56 36))])
                               (if wide?
                                   (bytevector-u64-set! bytes at size endian)
                                   (bytevector-u32-set! bytes at size endian))
                               (elf-review-rejected? bytes)))
                           '(0 1))))
                    (loop (+ index 1)))))))
         '("fixed-tables-" "fixed-relr-")))
      '("32le" "32be" "64le" "64be") '(#f #f #t #t)
      (list (endianness little) (endianness big) (endianness little) (endianness big))))

(mat parser-elf-relr-file-types

     ;; Error cases: gABI permits RELR only in executable and shared-object files.
     (for-all
      (lambda (tag endian)
        (let ([original (read-u8vec (string-append "data/elf-review/fixed-relr-" tag ".elf"))])
          (for-all
           (lambda (type)
             (let ([bytes (bytevector-copy original)])
               (bytevector-u16-set! bytes 16 type endian)
               (if (memv type '(2 3))
                   (equal? '#(0)
                           (elf-relr-table-entries
                            (elf-section-content (vector-ref (elf-file-sections
                                                             (parse-elf bytes)) 1))))
                   (elf-review-rejected? bytes))))
           '(0 1 2 3 4))))
      '("32le" "32be" "64le" "64be")
      (list (endianness little) (endianness big) (endianness little) (endianness big)))

     ;; Multi-entry RELR content is decoded in the containing file's class and byte order.
     (for-all
      (lambda (tag wide? endian)
        (let* ([original (read-u8vec (string-append "data/elf-review/fixed-relr-" tag ".elf"))]
               [header (elf-section-header (vector-ref (elf-file-sections
                                                        (parse-elf original)) 1))]
               [offset (elf-section-header-offset header)] [word (if wide? 8 4)]
               [bytes (make-bytevector (+ (bytevector-length original) word) 0)]
               [set-word! (if wide? bytevector-u64-set! bytevector-u32-set!)])
          (bytevector-copy! original 0 bytes 0 (bytevector-length original))
          (set-word! bytes (+ (if wide? 64 52) (if wide? 64 40) (if wide? 32 20))
                     (* word 2) endian)
          (set-word! bytes offset #x1000 endian)
          (set-word! bytes (+ offset word) 3 endian)
          (equal? '#(#x1000 3)
                  (elf-relr-table-entries
                   (elf-section-content (vector-ref (elf-file-sections (parse-elf bytes)) 1))))))
      '("32le" "32be" "64le" "64be") '(#f #f #t #t)
      (list (endianness little) (endianness big) (endianness little) (endianness big))))

(mat parser-elf-group-header-flags

     ;; Error cases: group headers require zero sh_flags; GRP_COMDAT is payload data.
     (for-all
      (lambda (tag wide? endian)
        (let* ([original (read-u8vec (string-append "data/elf-review/fixed-tables-" tag ".elf"))]
               [sections (elf-file-sections (parse-elf original))])
          (let find ([index 0])
            (and (< index (vector-length sections))
                 (if (= 17 (elf-section-header-type
                            (elf-section-header (vector-ref sections index))))
                     (for-all
                      (lambda (flags)
                        (let ([bytes (bytevector-copy original)])
                          ((if wide? bytevector-u64-set! bytevector-u32-set!)
                           bytes (+ (if wide? 64 52) (* index (if wide? 64 40)) 8)
                           flags endian)
                          (elf-review-rejected? bytes)))
                      '(1 2 4 64 128))
                     (find (+ index 1)))))))
      '("32le" "32be" "64le" "64be") '(#f #f #t #t)
      (list (endianness little) (endianness big) (endianness little) (endianness big))))
