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
