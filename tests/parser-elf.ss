(import (cspp)
        (cspp parser elf)
        (cspp parser elf types))

(include "parser-external-tools.ss")
(include "parser-test-support.ss")

(mat parser-elf-record-writers

     (string=?
      (string-append
       "#[elf-symbol name-index: 1 name: \"entry\" info: 2 other: 3 "
       "section-index: 4 value: 5 size: 6]")
      (format "~s" (make-elf-symbol 1 "entry" 2 3 4 5 6))))

(define minimal-elf64le
  (lambda ()
    (let ([bytes (make-bytevector 64 0)]
          [little (endianness little)])
      (bytevector-u8-set! bytes 0 #x7f)
      (bytevector-u8-set! bytes 1 (char->integer #\E))
      (bytevector-u8-set! bytes 2 (char->integer #\L))
      (bytevector-u8-set! bytes 3 (char->integer #\F))
      (bytevector-u8-set! bytes 4 2)
      (bytevector-u8-set! bytes 5 1)
      (bytevector-u8-set! bytes 6 1)
      (bytevector-u16-set! bytes 16 1 little)
      (bytevector-u16-set! bytes 18 62 little)
      (bytevector-u32-set! bytes 20 1 little)
      (bytevector-u16-set! bytes 52 64 little)
      (bytevector-u16-set! bytes 54 56 little)
      (bytevector-u16-set! bytes 58 64 little)
      bytes)))

(define minimal-elf
  (lambda (class endianness-name)
    (let* ([elf64? (eq? class 'elf64)]
           [size (if elf64? 64 52)]
           [bytes (make-bytevector size 0)]
           [endian (if (eq? endianness-name 'little)
                       (endianness little) (endianness big))]
           [sizes-at (if elf64? 52 40)])
      (bytevector-u8-set! bytes 0 #x7f)
      (bytevector-u8-set! bytes 1 (char->integer #\E))
      (bytevector-u8-set! bytes 2 (char->integer #\L))
      (bytevector-u8-set! bytes 3 (char->integer #\F))
      (bytevector-u8-set! bytes 4 (if elf64? 2 1))
      (bytevector-u8-set! bytes 5 (if (eq? endianness-name 'little) 1 2))
      (bytevector-u8-set! bytes 6 1)
      (bytevector-u16-set! bytes 16 2 endian)
      (bytevector-u16-set! bytes 18 (if elf64? 62 3) endian)
      (bytevector-u32-set! bytes 20 1 endian)
      (if elf64?
          (bytevector-u64-set! bytes 24 #x12345678 endian)
          (bytevector-u32-set! bytes 24 #x12345678 endian))
      (bytevector-u16-set! bytes sizes-at size endian)
      (bytevector-u16-set! bytes (+ sizes-at 2) (if elf64? 56 32) endian)
      (bytevector-u16-set! bytes (+ sizes-at 6) (if elf64? 64 40) endian)
      bytes)))

(define elf-with-u8
  (lambda (class endianness-name offset value)
    (let ([bytes (minimal-elf class endianness-name)])
      (bytevector-u8-set! bytes offset value)
      bytes)))

(define elf-with-u16
  (lambda (class endianness-name offset value)
    (let ([bytes (minimal-elf class endianness-name)])
      (bytevector-u16-set! bytes offset value
                           (if (eq? endianness-name 'little)
                               (endianness little) (endianness big)))
      bytes)))

(define truncated-elf-header
  (lambda ()
    (let ([bytes (make-bytevector 20)])
      (bytevector-copy! (minimal-elf 'elf64 'little) 0 bytes 0 20)
      bytes)))

(define rich-elf64le
  (lambda ()
    (let ([bytes (make-bytevector 960 0)]
          [little (endianness little)])
      (define set-section!
        (lambda (index name type offset size link info alignment entry-size)
          (let ([at (+ 512 (* index 64))])
            (bytevector-u32-set! bytes at name little)
            (bytevector-u32-set! bytes (+ at 4) type little)
            (bytevector-u64-set! bytes (+ at 24) offset little)
            (bytevector-u64-set! bytes (+ at 32) size little)
            (bytevector-u32-set! bytes (+ at 40) link little)
            (bytevector-u32-set! bytes (+ at 44) info little)
            (bytevector-u64-set! bytes (+ at 48) alignment little)
            (bytevector-u64-set! bytes (+ at 56) entry-size little))))
      (bytevector-copy! (minimal-elf64le) 0 bytes 0 64)
      (bytevector-u64-set! bytes 32 64 little)
      (bytevector-u64-set! bytes 40 512 little)
      (bytevector-u16-set! bytes 56 1 little)
      (bytevector-u16-set! bytes 60 7 little)
      (bytevector-u16-set! bytes 62 1 little)
      (bytevector-u32-set! bytes 64 1 little)
      (bytevector-u32-set! bytes 68 5 little)
      (bytevector-u64-set! bytes 72 120 little)
      (bytevector-u64-set! bytes 80 120 little)
      (bytevector-u64-set! bytes 96 4 little)
      (bytevector-u64-set! bytes 104 8 little)
      (bytevector-u64-set! bytes 112 1 little)
      (bytevector-copy! (string->utf8
                         "\x0;.shstrtab\x0;.strtab\x0;.symtab\x0;.note\x0;.rela\x0;.shndx\x0;")
                        0 bytes 128 46)
      (bytevector-copy! (string->utf8 "\x0;foo\x0;") 0 bytes 192 5)
      (bytevector-u32-set! bytes 232 1 little)
      (bytevector-u8-set! bytes 236 #x12)
      (bytevector-u16-set! bytes 238 #xffff little)
      (bytevector-u64-set! bytes 240 #x1234 little)
      (bytevector-u64-set! bytes 248 4 little)
      (bytevector-u32-set! bytes 272 4 little)
      (bytevector-u32-set! bytes 276 4 little)
      (bytevector-u32-set! bytes 280 3 little)
      (bytevector-copy! (string->utf8 "GNU\x0;") 0 bytes 284 4)
      (bytevector-copy! (bytevector 1 2 3 4) 0 bytes 288 4)
      (bytevector-u64-set! bytes 304 #x10 little)
      (bytevector-u64-set! bytes 312 (+ (ash 1 32) 7) little)
      (bytevector-s64-set! bytes 320 -4 little)
      (bytevector-u32-set! bytes 336 0 little)
      (bytevector-u32-set! bytes 340 1 little)
      (set-section! 1 1 3 128 46 0 0 1 0)
      (set-section! 2 11 3 192 5 0 0 1 0)
      (set-section! 3 19 2 208 48 2 1 8 24)
      (set-section! 4 27 7 272 20 0 0 4 0)
      (set-section! 5 33 4 304 24 3 1 8 24)
      (set-section! 6 39 18 336 8 3 0 4 4)
      bytes)))

(define rich-elf-with-u32
  (lambda (offset value)
    (let ([bytes (rich-elf64le)])
      (bytevector-u32-set! bytes offset value (endianness little))
      bytes)))

(define rich-elf-with-u64
  (lambda (offset value)
    (let ([bytes (rich-elf64le)])
      (bytevector-u64-set! bytes offset value (endianness little))
      bytes)))

(define rich-elf-without-section-names
  (lambda ()
    (let ([bytes (rich-elf64le)])
      (bytevector-u16-set! bytes 62 0 (endianness little))
      bytes)))

(define elf-endianness
  (lambda (endianness-name)
    (if (eq? endianness-name 'little) (endianness little) (endianness big))))

(define set-elf-word!
  (lambda (bytes offset size value endianness)
    (if (= size 4)
        (bytevector-u32-set! bytes offset value endianness)
        (bytevector-u64-set! bytes offset value endianness))))

(define set-elf-sword!
  (lambda (bytes offset size value endianness)
    (if (= size 4)
        (bytevector-s32-set! bytes offset value endianness)
        (bytevector-s64-set! bytes offset value endianness))))

(define set-elf-section!
  (lambda (bytes class endianness table-offset index type offset size link entry-size)
    (let ([at (+ table-offset (* index (if (eq? class 'elf32) 40 64)))])
      (bytevector-u32-set! bytes (+ at 4) type endianness)
      (if (eq? class 'elf32)
          (begin
            (bytevector-u32-set! bytes (+ at 16) offset endianness)
            (bytevector-u32-set! bytes (+ at 20) size endianness)
            (bytevector-u32-set! bytes (+ at 24) link endianness)
            (bytevector-u32-set! bytes (+ at 32) 1 endianness)
            (bytevector-u32-set! bytes (+ at 36) entry-size endianness))
          (begin
            (bytevector-u64-set! bytes (+ at 24) offset endianness)
            (bytevector-u64-set! bytes (+ at 32) size endianness)
            (bytevector-u32-set! bytes (+ at 40) link endianness)
            (bytevector-u64-set! bytes (+ at 48) 1 endianness)
            (bytevector-u64-set! bytes (+ at 56) entry-size endianness))))))

(define typed-elf
  (lambda (class endianness-name)
    (let* ([elf64? (eq? class 'elf64)]
           [word (if elf64? 8 4)]
           [symbol-size (if elf64? 24 16)]
           [table-offset 640]
           [endianness (elf-endianness endianness-name)]
           [bytes (make-bytevector 1400 0)]
           [header (minimal-elf class endianness-name)])
      (bytevector-copy! header 0 bytes 0 (bytevector-length header))
      ;; Section groups belong to ET_REL objects and their members carry SHF_GROUP.
      (bytevector-u16-set! bytes 16 1 endianness)
      (if elf64?
          (bytevector-u64-set! bytes 40 table-offset endianness)
          (bytevector-u32-set! bytes 32 table-offset endianness))
      (bytevector-u16-set! bytes (if elf64? 60 48) 11 endianness)
      (bytevector-u16-set! bytes (if elf64? 62 50) 0 endianness)

      (bytevector-copy! (string->utf8 "\x0;x\x0;") 0 bytes 128 3)
      (let ([symbol-at (+ 144 symbol-size)])
        (bytevector-u32-set! bytes symbol-at 1 endianness)
        (if elf64?
            (begin
              (bytevector-u8-set! bytes (+ symbol-at 4) #x12)
              (bytevector-u16-set! bytes (+ symbol-at 6) 1 endianness)
              (bytevector-u64-set! bytes (+ symbol-at 8) #x11223344 endianness)
              (bytevector-u64-set! bytes (+ symbol-at 16) 7 endianness))
            (begin
              (bytevector-u32-set! bytes (+ symbol-at 4) #x11223344 endianness)
              (bytevector-u32-set! bytes (+ symbol-at 8) 7 endianness)
              (bytevector-u8-set! bytes (+ symbol-at 12) #x12)
              (bytevector-u16-set! bytes (+ symbol-at 14) 1 endianness))))

      (set-elf-word! bytes 208 word #x1000 endianness)
      (set-elf-word! bytes (+ 208 word) word 3 endianness)
      (bytevector-u32-set! bytes 240 1 endianness)
      (bytevector-u32-set! bytes 244 2 endianness)
      (bytevector-u32-set! bytes 248 1 endianness)
      (bytevector-u32-set! bytes 252 0 endianness)
      (bytevector-u32-set! bytes 256 1 endianness)
      (bytevector-u32-set! bytes 272 1 endianness)
      (bytevector-u32-set! bytes 276 6 endianness)
      (for-each
       (lambda (offset)
         (set-elf-word! bytes offset word #x11 endianness)
         (set-elf-word! bytes (+ offset word) word #x22 endianness))
       '(288 320 352))
      (set-elf-sword! bytes 400 word 1 endianness)
      (set-elf-word! bytes (+ 400 word) word #x33 endianness)
      (set-elf-sword! bytes (+ 400 (* 2 word)) word 0 endianness)
      (set-elf-word! bytes (+ 400 (* 3 word)) word 0 endianness)

      (set-elf-section! bytes class endianness table-offset 1 3 128 3 0 0)
      (set-elf-section! bytes class endianness table-offset 2 2 144 (* 2 symbol-size)
                        1 symbol-size)
      ;; Symbol zero is local; symbol one is the first nonlocal entry.
      (bytevector-u32-set! bytes
                           (+ table-offset (* 2 (if elf64? 64 40)) (if elf64? 44 28))
                           1 endianness)
      ;; RELR cannot coexist with GROUP: the former requires ET_EXEC/ET_DYN,
      ;; while this fixture exercises GROUP in ET_REL. Keep these bytes untyped.
      (set-elf-section! bytes class endianness table-offset 3 1 208 (* 2 word) 0 0)
      (set-elf-section! bytes class endianness table-offset 4 5 240 20 2 4)
      (set-elf-section! bytes class endianness table-offset 5 17 272 8 2 4)
      (set-elf-section! bytes class endianness table-offset 6 14 288 (* 2 word) 0 word)
      (set-elf-section! bytes class endianness table-offset 7 15 320 (* 2 word) 0 word)
      (set-elf-section! bytes class endianness table-offset 8 16 352 (* 2 word) 0 word)
      (set-elf-section! bytes class endianness table-offset 9 6 400 (* 4 word) 1
                        (* 2 word))
      (set-elf-section! bytes class endianness table-offset 10 18 480 8 2 4)
      (set-elf-word! bytes (+ table-offset (* 6 (if elf64? 64 40)) 8)
                     word #x200 endianness)
      bytes)))

(define typed-elf-with-section-size
  (lambda (class endianness-name index size)
    (let* ([bytes (typed-elf class endianness-name)]
           [endianness (elf-endianness endianness-name)]
           [at (+ 640 (* index (if (eq? class 'elf32) 40 64))
                  (if (eq? class 'elf32) 20 32))])
      (if (eq? class 'elf32)
          (bytevector-u32-set! bytes at size endianness)
          (bytevector-u64-set! bytes at size endianness))
      bytes)))

(define extended-count-elf32be
  (lambda ()
    (let* ([header-size 52]
           [program-entry-size 32]
           [program-count #xffff]
           [section-entry-size 40]
           [section-count #xff01]
           [section-name-index #xff00]
           [section-offset (+ header-size (* program-entry-size program-count))]
           [name-offset (+ section-offset (* section-entry-size section-count))]
           [bytes (make-bytevector (+ name-offset 1) 0)]
           [big (endianness big)]
           [name-header (+ section-offset (* section-name-index section-entry-size))])
      (bytevector-copy! (minimal-elf 'elf32 'big) 0 bytes 0 header-size)
      (bytevector-u32-set! bytes 28 header-size big)
      (bytevector-u32-set! bytes 32 section-offset big)
      (bytevector-u16-set! bytes 44 #xffff big)
      (bytevector-u16-set! bytes 48 0 big)
      (bytevector-u16-set! bytes 50 #xffff big)
      (bytevector-u32-set! bytes (+ section-offset 20) section-count big)
      (bytevector-u32-set! bytes (+ section-offset 24) section-name-index big)
      (bytevector-u32-set! bytes (+ section-offset 28) program-count big)
      (bytevector-u32-set! bytes (+ name-header 4) 3 big)
      (bytevector-u32-set! bytes (+ name-header 16) name-offset big)
      (bytevector-u32-set! bytes (+ name-header 20) 1 big)
      (bytevector-u32-set! bytes (+ name-header 32) 1 big)
      bytes)))

(define elf32be-with-section-zero
  (lambda (program-count section-count section-name-index size link info)
    (let* ([section-offset 52]
           [bytes (make-bytevector 92 0)]
           [big (endianness big)])
      (bytevector-copy! (minimal-elf 'elf32 'big) 0 bytes 0 52)
      (bytevector-u32-set! bytes 32 section-offset big)
      (bytevector-u16-set! bytes 44 program-count big)
      (bytevector-u16-set! bytes 48 section-count big)
      (bytevector-u16-set! bytes 50 section-name-index big)
      (bytevector-u32-set! bytes (+ section-offset 20) size big)
      (bytevector-u32-set! bytes (+ section-offset 24) link big)
      (bytevector-u32-set! bytes (+ section-offset 28) info big)
      bytes)))

(mat parse-elf-records

     (let* ([file (parse-elf (minimal-elf64le))]
            [identification (elf-file-identification file)]
            [header (elf-file-header file)])
       (and (elf-file? file)
            (elf-identification? identification)
            (eq? 'elf64 (elf-identification-class identification))
            (eq? 'little (elf-identification-endianness identification))
            (= 62 (elf-header-machine header))
            (= 0 (vector-length (elf-file-program-headers file)))
            (= 0 (vector-length (elf-file-sections file)))))

     )

(mat elf-record-contracts

     ;; error: an ELF section-header name must be a string.
     (error? (make-elf-section-header 0 #f 0 0 0 0 0 0 0 0 0))

     )
(mat parse-elf-headers

     (for-all
      (lambda (configuration)
        (let* ([class (car configuration)]
               [endianness-name (cadr configuration)]
               [file (parse-elf (minimal-elf class endianness-name))]
               [identification (elf-file-identification file)]
               [header (elf-file-header file)])
          (and (eq? class (elf-identification-class identification))
               (eq? endianness-name (elf-identification-endianness identification))
               (= #x12345678 (elf-header-entry header))
               (= (if (eq? class 'elf64) 62 3) (elf-header-machine header)))))
      '((elf32 little) (elf32 big) (elf64 little) (elf64 big)))

     ;; error: the four-byte ELF magic must match exactly.
     (parser-error? (parser-error-from (lambda () (parse-elf (elf-with-u8 'elf64 'little 0 0)))))

     ;; error: the ELF class byte must identify ELF32 or ELF64.
     (parser-error? (parser-error-from (lambda () (parse-elf (elf-with-u8 'elf64 'little 4 3)))))

     ;; error: the ELF data byte must identify little- or big-endian encoding.
     (parser-error? (parser-error-from (lambda () (parse-elf (elf-with-u8 'elf64 'little 5 0)))))

     ;; error: the declared ELF header size must match the selected class.
     (parser-error? (parser-error-from (lambda () (parse-elf (elf-with-u16 'elf32 'big 40 51)))))

     ;; error: a complete class-specific ELF header is required.
     (parser-error? (parser-error-from (lambda () (parse-elf (truncated-elf-header)))))

     )

(define elf-find-section
  (lambda (sections name)
    (let loop ([index 0])
      (and (fx<? index (vector-length sections))
           (let ([section (vector-ref sections index)])
             (if (string=? name (elf-section-header-name (elf-section-header section)))
                 section
                 (loop (fx1+ index))))))))

(define readelf-header-matches?
  (lambda (file output)
    (let ([fields (map (lambda (line) (map string-trim (string-split line #\:)))
                       (string-split output #\newline))]
          [identification (elf-file-identification file)]
          [header (elf-file-header file)])
      (define field=?
        (lambda (label expected)
          (let ([matches (filter (lambda (field) (string=? label (car field))) fields)])
            (and (= 1 (length matches))
                 (= 2 (length (car matches)))
                 (string=? expected (cadar matches))))))
      (define count=?
        (lambda (label count sentinel threshold)
          (or (field=? label (number->string count))
              (and (>= count threshold)
                   (field=? label (format "~a (~a)" sentinel count))))))
      (and (field=? "Class" (if (eq? (elf-identification-class identification) 'elf64)
                                "ELF64" "ELF32"))
           (field=? "Data" (format "2's complement, ~a endian"
                                    (if (eq? (elf-identification-endianness identification)
                                             'little) "little" "big")))
           (count=? "Number of program headers"
                    (elf-header-program-header-count header) 65535 65535)
           (count=? "Number of section headers"
                    (elf-header-section-header-count header) 0 65280)
           (count=? "Section header string table index"
                    (elf-header-section-name-index header) 65535 65280)))))

(mat elf-readelf-comparison-controls

     (readelf-header-matches?
      (parse-elf (minimal-elf64le))
      (string-append "  Class:                             ELF64\n"
                     "  Data:                              2's complement, little endian\n"
                     "  Number of program headers:         0\n"
                     "  Number of section headers:         0\n"
                     "  Section header string table index: 0\n"))

     ;; Error controls: a numeric field cannot match just a decimal prefix.
     (for-all
      (lambda (counts)
        (not (readelf-header-matches?
              (parse-elf (minimal-elf64le))
              (format
               (string-append "  Class:                             ELF64\n"
                              "  Data:                              2's complement, little endian\n"
                              "  Number of program headers:         ~a\n"
                              "  Number of section headers:         ~a\n"
                              "  Section header string table index: ~a\n")
               (car counts) (cadr counts) (caddr counts)))))
      '(("01" "0" "0") ("0" "01" "0") ("0" "0" "01")
        ("0x" "0" "0") ("0 1" "0" "0"))))

(mat elf-readelf-extended-comparison

     (readelf-header-matches?
      (parse-elf (extended-count-elf32be))
      (string-append "Class: ELF32\nData: 2's complement, big endian\n"
                     "Number of program headers: 65535 (65535)\n"
                     "Number of section headers: 0 (65281)\n"
                     "Section header string table index: 65535 (65280)\n"))

     ;; Error controls: only the proper sentinel and one complete decimal count are allowed.
     (let ([file (parse-elf (extended-count-elf32be))])
       (for-all
        (lambda (count)
          (not (readelf-header-matches?
                file
                (string-append "Class: ELF32\nData: 2's complement, big endian\n"
                               "Number of program headers: 65535 (65535)\n"
                               "Number of section headers: " count "\n"
                               "Section header string table index: 65535 (65280)\n"))))
        '("1 (65281)" "0 (652810)" "0 (065281)" "0 (65281)x" "0 (65281) (65281)"
          "0(65281)" "0 (65281" "0 (65281.0)"
          "0 (65281)\nNumber of section headers: 0 (65281)"))))

(mat parse-elf-file

     (let* ([file (parse-elf-file "../libcspp.so")]
            [identification (elf-file-identification file)]
            [header (elf-file-header file)]
            [program-headers (elf-file-program-headers file)]
            [sections (elf-file-sections file)]
            [text-section (elf-find-section sections ".text")])
       (and (memq (elf-identification-class identification) '(elf32 elf64))
            (memq (elf-identification-endianness identification) '(little big))
            (= (elf-header-program-header-count header) (vector-length program-headers))
            (= (elf-header-section-header-count header) (vector-length sections))
            (fxpositive? (vector-length program-headers))
            (fxpositive? (vector-length sections))
            (fxpositive? (elf-header-section-name-index header))
            text-section
            (positive? (elf-section-header-size (elf-section-header text-section)))
            (elf-raw-section? (elf-section-content text-section))
            (positive?
             (bytevector-length
              (elf-raw-section-bytes (elf-section-content text-section))))))

     (or (not (external-tool-available? "readelf"))
         (let* ([file (parse-elf-file "../libcspp.so")]
                [result
                 (process-capture "/bin/sh" "-c"
                                  "LC_ALL=C exec readelf -h -W \"$1\""
                                  "sh" "../libcspp.so"
                   :stdout capture
                   :stderr capture
                   :timeout 10000)]
                [output (successful-process-output result)])
           (and output (readelf-header-matches? file output))))

     )

(mat parse-elf-content

     (let* ([file (parse-elf (rich-elf64le))]
            [program (vector-ref (elf-file-program-headers file) 0)]
            [sections (elf-file-sections file)]
            [symbols (elf-symbol-table-symbols
                      (elf-section-content (vector-ref sections 3)))]
            [symbol (vector-ref symbols 1)]
            [notes (elf-note-table-notes
                    (elf-section-content (vector-ref sections 4)))]
            [relocations (elf-relocation-table-relocations
                          (elf-section-content (vector-ref sections 5)))])
       (and (= 1 (elf-program-header-type program))
            (equal? (bytevector 0 0 0 0) (elf-program-header-data program))
            (equal? ".symtab" (elf-section-header-name
                                (elf-section-header (vector-ref sections 3))))
            (equal? "foo" (elf-symbol-name symbol))
            (= 1 (elf-symbol-section-index symbol))
            (= #x1234 (elf-symbol-value symbol))
            (equal? "GNU" (elf-note-name (vector-ref notes 0)))
            (equal? (bytevector 1 2 3 4)
                    (elf-note-descriptor (vector-ref notes 0)))
            (= 1 (elf-relocation-symbol-index (vector-ref relocations 0)))
            (= 7 (elf-relocation-type (vector-ref relocations 0)))
            (= -4 (elf-relocation-addend (vector-ref relocations 0)))))

     ;; error: a symbol table must link to a string table.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (rich-elf-with-u32 (+ 512 (* 3 64) 40) 4)))))

     ;; error: a symbol name index must identify a terminated linked-table string.
     (parser-error? (parser-error-from (lambda () (parse-elf (rich-elf-with-u32 232 99)))))

     ;; error: a relocation table must link to a symbol table.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (rich-elf-with-u32 (+ 512 (* 5 64) 40) 2)))))

     ;; Note padding aligns fields but has no required byte value.
     (elf-file?
      (let ([bytes (rich-elf-with-u32 272 2)])
        (bytevector-u8-set! bytes 284 (char->integer #\G))
        (bytevector-u8-set! bytes 285 0)
        (bytevector-u8-set! bytes 286 1)
        (parse-elf bytes)))

     (let* ([bytes (rich-elf64le)]
            [unused (bytevector-u8-set! bytes 284 #xff)]
            [file (parse-elf bytes)]
            [notes (elf-note-table-notes
                    (elf-section-content (vector-ref (elf-file-sections file) 4)))])
       (string=? "\xff;NU" (elf-note-name (vector-ref notes 0))))

     ;; Error case: a nonempty note name must end in NUL, reported as malformed content.
     (parser-error?
      (parser-error-from
       (lambda ()
         (let ([bytes (rich-elf64le)])
           (bytevector-u8-set! bytes 287 65)
           (parse-elf bytes)))))

     ;; error: the section-header table must fit in the input.
     (parser-error? (parser-error-from (lambda () (parse-elf (rich-elf-with-u64 40 800)))))

     )

(mat parse-elf-without-section-names

     (let* ([file (parse-elf (rich-elf-without-section-names))]
            [header (elf-file-header file)]
            [sections (elf-file-sections file)])
       (and (= 0 (elf-header-section-name-index header))
            (for-all
             (lambda (section)
               (string=? "" (elf-section-header-name (elf-section-header section))))
             (vector->list sections))))

     )

(mat parse-elf-typed-sections

     (for-all
      (lambda (configuration)
        (let* ([class (car configuration)]
               [endianness-name (cadr configuration)]
               [file (parse-elf (typed-elf class endianness-name))]
               [sections (elf-file-sections file)]
               [hash (elf-section-content (vector-ref sections 4))]
               [group (elf-section-content (vector-ref sections 5))]
               [init (elf-section-content (vector-ref sections 6))]
               [fini (elf-section-content (vector-ref sections 7))]
               [preinit (elf-section-content (vector-ref sections 8))]
               [dynamic (elf-section-content (vector-ref sections 9))]
               [dynamic* (elf-dynamic-table-entries dynamic)]
               [xindex (elf-section-content (vector-ref sections 10))])
          (and (equal? '#(1) (elf-hash-table-buckets hash))
               (equal? '#(0 1) (elf-hash-table-chains hash))
               (= 1 (elf-group-section-flags group))
               (equal? '#(6) (elf-group-section-members group))
               (equal? '#(#x11 #x22) (elf-word-table-entries init))
               (equal? '#(#x11 #x22) (elf-word-table-entries fini))
               (equal? '#(#x11 #x22) (elf-word-table-entries preinit))
               (= 1 (elf-dynamic-entry-tag (vector-ref dynamic* 0)))
               (= #x33 (elf-dynamic-entry-value (vector-ref dynamic* 0)))
               (= 0 (elf-dynamic-entry-tag (vector-ref dynamic* 1)))
               (equal? '#(0 0) (elf-word-table-entries xindex)))))
      '((elf32 little) (elf32 big) (elf64 little) (elf64 big)))

     ;; error: an SHT_RELR payload must contain complete class-sized words.
     (parser-error?
      (parser-error-from
       (lambda ()
         (let ([bytes (read-u8vec "data/elf-review/fixed-relr-32le.elf")])
           (bytevector-u32-set! bytes (+ 52 40 20) 3 (endianness little))
           (parse-elf bytes)))))

     ;; error: an SHT_HASH payload must match its declared bucket and chain counts.
     (parser-error?
      (parser-error-from
       (lambda ()
         (let ([bytes (typed-elf 'elf64 'big)])
           (bytevector-u32-set! bytes 240 2 (endianness big))
           (parse-elf bytes)))))

     ;; error: an SHT_GROUP payload must contain a flags word and complete members.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (typed-elf-with-section-size 'elf64 'little 5 6)))))

     ;; error: an address-array payload must contain complete class-sized words.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (typed-elf-with-section-size 'elf32 'big 6 5)))))

     ;; error: an SHT_DYNAMIC payload must contain complete tag/value pairs.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (typed-elf-with-section-size 'elf64 'big 9 17)))))

     ;; error: unused SHT_SYMTAB_SHNDX entries must be zero.
     (parser-error?
      (parser-error-from
       (lambda ()
         (let ([bytes (typed-elf 'elf32 'little)])
           (bytevector-u32-set! bytes 480 1 (endianness little))
           (parse-elf bytes)))))

     )

(mat parse-elf-extended-counts

     (let* ([file (parse-elf (extended-count-elf32be))]
            [header (elf-file-header file)]
            [programs (elf-file-program-headers file)]
            [sections (elf-file-sections file)]
            [zero (elf-section-content (vector-ref sections 0))])
       (and (= #xffff (elf-header-program-header-count header))
            (= #xff01 (elf-header-section-header-count header))
            (= #xff00 (elf-header-section-name-index header))
            (= #xffff (vector-length programs))
            (= #xff01 (vector-length sections))
            (elf-raw-section? zero)
            (= 0 (bytevector-length (elf-raw-section-bytes zero)))))

     ;; error: an extended section count must be in the reserved-index range.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero 0 0 0 2 0 0)))))

     ;; error: an extended program-header count must be at least PN_XNUM.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero #xffff 1 0 0 0 2)))))

     ;; error: an extended section-name index must be in the reserved-index range.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero 0 1 #xffff 0 2 0)))))

     ;; error: section zero must not carry an extended count when e_shnum is ordinary.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero 0 1 0 2 0 0)))))

     ;; error: section zero must not carry PN_XNUM data when e_phnum is ordinary.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero 0 1 0 0 0 2)))))

     ;; error: section zero must not carry SHN_XINDEX data when e_shstrndx is ordinary.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-elf (elf32be-with-section-zero 0 1 0 0 2 0)))))

     )

(include "parser-elf-regressions.ss")

(mat elf-standard-dynamic-tag-names

     (for-all (lambda (entry) (eq? (cdr entry) (elf-dynamic-tag-name (car entry))))
              '((20 . pltrel) (21 . debug) (22 . textrel) (23 . jmprel) (24 . bind-now)
                (25 . init-array) (26 . fini-array) (27 . init-arraysz) (28 . fini-arraysz)
                (29 . runpath) (30 . flags) (32 . preinit-array) (33 . preinit-arraysz)
                (34 . symtab-shndx) (35 . relrsz) (36 . relr) (37 . relrent)))

     (for-all (lambda (tag) (eq? 'unknown (elf-dynamic-tag-name tag))) '(-1 31 38)))
