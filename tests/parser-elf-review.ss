;; Included by parser-elf.ss. Fixtures are reproducible with data/elf-review/generate.py.
(define elf-review-file
  (lambda (name) (parse-elf-file (string-append "data/elf-review/" name ".elf"))))

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
