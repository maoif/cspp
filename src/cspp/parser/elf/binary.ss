(library (cspp parser elf binary)
  (export parser-elf)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser private)
          (cspp optional-library-check)
          (cspp parser elf types)
          (cspp parser elf validate)
          (only (cspp vector) vfor-all))

  (define issue (lambda (offset message) (make-elf-issue offset message)))

  (define-record-type decoded-elf-header
    (fields class endianness-name endianness type machine version entry phoff shoff flags
            header-size phentsize raw-phnum shentsize raw-shnum raw-shstrndx sizes-at))

  (define copy-range
    (lambda (bytes offset size)
      (let ([result (make-bytevector size)])
        (bytevector-copy! bytes offset result 0 size)
        result)))

  ;; Reuse the native zlib stream without making the ELF parser depend on networking.
  (define zlib-stream-open
    (foreign-procedure "cspp_zlib_stream_open" (int int) uptr))
  (define zlib-stream-process
    (foreign-procedure "cspp_zlib_stream_process"
                       (uptr scheme-object int int int int) scheme-object))
  (define zlib-stream-close
    (foreign-procedure "cspp_zlib_stream_close" (uptr) void))

  (define inflate-section
    (lambda (raw start size)
      (require-optional-library 'parse-elf 'zlib)
      (let ([handle (zlib-stream-open 0 0)])
        (and (not (zero? handle))
             (dynamic-wind
               void
               (lambda ()
                 ;; A spare byte distinguishes a full output buffer from exact stream completion.
                 (let ([answer (zlib-stream-process handle raw start (bytevector-length raw)
                                                    1 (+ size 1))])
                   (and (vector? answer) (= 2 (vector-length answer))
                        (bytevector? (vector-ref answer 0)) (vector-ref answer 1)
                        (= size (bytevector-length (vector-ref answer 0)))
                        (vector-ref answer 0))))
               (lambda () (zlib-stream-close handle)))))))

  (define decompress-section
    (lambda (raw class endianness-name type header)
      (let* ([offset (elf-section-header-offset header)]
             [elf64? (eq? class 'elf64)]
             [header-size (if elf64? 24 12)]
             [endian (if (eq? endianness-name 'little)
                         (endianness little) (endianness big))]
             [reject (lambda (message) (values #f (issue offset message)))])
        (cond
          [(= 8 (elf-section-header-type header))
           (reject "NOBITS sections cannot be compressed")]
          [(and (memv type '(2 3)) (logbit? 1 (elf-section-header-flags header)))
           (reject "allocated sections cannot be compressed in executable or shared files")]
          [(< (bytevector-length raw) header-size)
           (reject "truncated ELF compression header")]
          [else
           (let* ([ref (if elf64? bytevector-u64-ref bytevector-u32-ref)]
                  [method (bytevector-u32-ref raw 0 endian)]
                  [size (ref raw (if elf64? 8 4) endian)]
                  [alignment (ref raw (if elf64? 16 8) endian)])
             (cond
               [(and elf64? (not (zero? (bytevector-u32-ref raw 4 endian))))
                (reject "nonzero reserved ELF compression field")]
               [(not (= method 1))
                (reject (format "unsupported ELF compression method ~a" method))]
               [(not (elf-power-of-two? alignment))
                (reject "invalid decompressed ELF section alignment")]
               ;; The existing native stream interface uses signed 32-bit lengths.
               [(or (> size #x7ffffffe) (> (bytevector-length raw) #x7fffffff))
                (reject "compressed ELF section exceeds the zlib decoder size limit")]
               [else
                (let ([decoded (inflate-section raw header-size size)])
                  (if decoded (values decoded #f)
                      (reject "invalid ELF zlib stream or decompressed size")))]))]))))

  (define section-content-alignment
    (lambda (bytes class endianness-name header)
      (if (and (not (zero? (elf-section-header-type header)))
               (logbit? 11 (elf-section-header-flags header)))
          ;; The compression header was already validated while decoding the section bytes.
          (let ([endian (if (eq? endianness-name 'little)
                            (endianness little) (endianness big))]
                [offset (elf-section-header-offset header)])
            (if (eq? class 'elf64)
                (bytevector-u64-ref bytes (+ offset 16) endian)
                (bytevector-u32-ref bytes (+ offset 8) endian)))
          (elf-section-header-address-alignment header))))

  ;; ELF names are byte sequences. Preserve UTF-8 where valid and map invalid bytes
  ;; one-to-one into characters; the original string-table bytes remain available.
  (define decode-elf-name
    (lambda (bytes)
      (guard (condition
              [else
               (let* ([length (bytevector-length bytes)] [text (make-string length)])
                 (do ([i 0 (fx1+ i)]) ((fx= i length) text)
                   (string-set! text i (integer->char (bytevector-u8-ref bytes i)))))])
        (parser-decode-bytes bytes 'utf-8))))

  (define valid-string-table?
    (lambda (bytes)
      (let ([length (bytevector-length bytes)])
        (or (zero? length)
            (and (zero? (bytevector-u8-ref bytes 0))
                 (zero? (bytevector-u8-ref bytes (fx1- length))))))))

  (define nul-string
    (lambda (bytes index)
      (if (and (zero? index) (zero? (bytevector-length bytes))) ""
          (and (< index (bytevector-length bytes))
               (let loop ([end index])
                 (cond [(= end (bytevector-length bytes)) #f]
                       [(zero? (bytevector-u8-ref bytes end))
                        (decode-elf-name (copy-range bytes index (- end index)))]
                       [else (loop (fx1+ end))]))))))

  (define rebuild-section-header
    (lambda (header name)
      (make-elf-section-header
       (elf-section-header-name-index header) name (elf-section-header-type header)
       (elf-section-header-flags header) (elf-section-header-address header)
       (elf-section-header-offset header) (elf-section-header-size header)
       (elf-section-header-link header) (elf-section-header-info header)
       (elf-section-header-address-alignment header) (elf-section-header-entry-size header))))

  (define unsigned-parser
    (lambda (size endianness-name)
      (case (cons size endianness-name)
        [((2 . little)) <u16le>] [((4 . little)) <u32le>]
        [((8 . little)) <u64le>] [((2 . big)) <u16be>]
        [((4 . big)) <u32be>] [((8 . big)) <u64be>])))

  (define signed-parser
    (lambda (size endianness-name)
      (case (cons size endianness-name)
        [((4 . little)) <s32le>] [((8 . little)) <s64le>]
        [((4 . big)) <s32be>] [((8 . big)) <s64be>])))

  (define parse-bounded-content
    (lambda (bytes parser)
      (guard (condition [else #f])
        (run-binary-parser (<bounded> (bytevector-length bytes) parser) bytes))))

  (define vector-parser
    (lambda (parser count)
      (<map> list->vector (<rep> parser count))))

  (define parse-word-vector
    (lambda (bytes size endianness-name)
      (let ([length (bytevector-length bytes)])
        (and (zero? (modulo length size))
             (parse-bounded-content
              bytes
              (vector-parser (unsigned-parser size endianness-name) (/ length size)))))))

  (define-record-type parsed-symbol
    (fields name-index info other raw-index value size))

  (define symbol-entry-parser
    (lambda (class endianness-name)
      (let ([<half> (unsigned-parser 2 endianness-name)]
            [<word> (unsigned-parser 4 endianness-name)])
        (if (eq? class 'elf32)
            (<map>
             (lambda (field*)
               (make-parsed-symbol
                (list-ref field* 0) (list-ref field* 3) (list-ref field* 4)
                (list-ref field* 5) (list-ref field* 1) (list-ref field* 2)))
             (<~> <word> <word> <word> <u8> <u8> <half>))
            (let ([<xword> (unsigned-parser 8 endianness-name)])
              (<map>
               (lambda (field*)
                 (make-parsed-symbol
                  (list-ref field* 0) (list-ref field* 1) (list-ref field* 2)
                  (list-ref field* 3) (list-ref field* 4) (list-ref field* 5)))
               (<~> <word> <u8> <u8> <half> <xword> <xword>)))))))

  (define decode-symbols
    (lambda (bytes class endianness-name header string-bytes xindices section-count)
      (let* ([size (bytevector-length bytes)]
             [standard (if (eq? class 'elf32) 16 24)]
             [entry-size (elf-section-header-entry-size header)]
             [entry-size (if (zero? entry-size) standard entry-size)])
        (and string-bytes (= entry-size standard) (zero? (modulo size entry-size))
             (let* ([count (/ size entry-size)]
                    [parsed (parse-bounded-content
                             bytes
                             (vector-parser
                              (<bounded> entry-size
                                         (symbol-entry-parser class endianness-name))
                              count))]
                    [result (make-vector count)])
               (and parsed (> count 0)
                    (<= 1 (elf-section-header-info header) count)
                    (or (not xindices)
                        (and (vector? xindices) (= count (vector-length xindices))))
                    (let loop ([i 0])
                      (if (= i count)
                          (make-elf-symbol-table result)
                 (let* ([symbol (vector-ref parsed i)]
                        [name-index (parsed-symbol-name-index symbol)]
                        [name (nul-string string-bytes name-index)]
                        [raw-index (parsed-symbol-raw-index symbol)]
                        [extended? (= raw-index #xffff)]
                        [section-index (if extended?
                                           (and xindices (vector-ref xindices i))
                                           raw-index)]
                        [unused-extension?
                         (and xindices (not extended?)
                              (not (zero? (vector-ref xindices i))))]
                        [valid-index?
                         (and section-index
                              (or (< section-index section-count)
                                  (and (not extended?)
                                       (<= #xff00 section-index #xfffe))))]
                        [local? (zero? (ash (parsed-symbol-info symbol) -4))]
                        [first-valid?
                         (or (not (zero? i))
                             (and (zero? name-index) (zero? raw-index)
                                  (zero? (parsed-symbol-info symbol))
                                  (zero? (parsed-symbol-other symbol))
                                  (zero? (parsed-symbol-value symbol))
                                  (zero? (parsed-symbol-size symbol))))])
                   (and name valid-index? (not unused-extension?) first-valid?
                        (eq? local? (< i (elf-section-header-info header)))
                        (begin
                          (vector-set!
                           result i
                           (make-elf-symbol
                            name-index name (parsed-symbol-info symbol)
                            (parsed-symbol-other symbol) section-index
                            (parsed-symbol-value symbol) (parsed-symbol-size symbol)))
                          (loop (+ i 1)))))))))))))

  (define relocation-entry-parser
    (lambda (bytes class endianness-name machine addends?)
      (let* ([word (if (eq? class 'elf32) 4 8)]
             [<word> (unsigned-parser word endianness-name)]
             [<sword> (signed-parser word endianness-name)]
             [mips64el? (and (eq? class 'elf64) (eq? endianness-name 'little)
                             (= machine 8))])
        (<map>
         (lambda (field*)
           (let ([position (car field*)] [offset (cadr field*)] [info (caddr field*)])
             ;; MIPS64EL stores a little-endian symbol followed by big-endian type bytes.
             ;; Preserve the raw info word and decode its fields with the ABI's byte orders.
             (make-elf-relocation
              offset info
              (if mips64el?
                  (bytevector-u32-ref bytes (+ position 8) (endianness little))
                  (if (eq? class 'elf32) (ash info -8) (ash info -32)))
              (if mips64el?
                  (bytevector-u32-ref bytes (+ position 12) (endianness big))
                  (if (eq? class 'elf32) (logand info #xff) (logand info #xffffffff)))
              (and addends? (list-ref field* 3)))))
         (if addends? (<~> <pos> <word> <word> <sword>) (<~> <pos> <word> <word>))))))

  (define decode-relocations
    (lambda (bytes class endianness-name machine header addends?)
      (let* ([standard (case class [(elf32) (if addends? 12 8)]
                                   [else (if addends? 24 16)])]
             [entry-size (elf-section-header-entry-size header)]
             [size (bytevector-length bytes)])
        (and (= entry-size standard) (zero? (modulo size entry-size))
             (let ([result
                    (parse-bounded-content
                     bytes
                     (vector-parser
                      (<bounded> entry-size
                                 (relocation-entry-parser bytes class endianness-name machine addends?))
                      (/ size entry-size)))])
               (and result (make-elf-relocation-table result addends?)))))))

  (define note-parser
    (lambda (endianness-name alignment)
      (let ([<word> (unsigned-parser 4 endianness-name)])
        (<bind>
         (<~> <word> <word> <word>)
         (lambda (header*)
           (let* ([name-size (list-ref header* 0)]
                  [descriptor-size (list-ref header* 1)]
                  [type (list-ref header* 2)]
                  [name-padding (modulo (- (+ 12 name-size)) alignment)]
                  [descriptor-padding (modulo (- descriptor-size) alignment)])
             (<map>
              (lambda (field*)
                (let* ([raw-name (list-ref field* 0)]
                       [terminated? (and (> name-size 0)
                                         (zero? (bytevector-u8-ref raw-name
                                                                   (- name-size 1))))]
                       [name-length (if terminated? (- name-size 1) name-size)])
                  (unless (or (zero? name-size) terminated?)
                    (errorf 'note-parser "unterminated ELF note name"))
                  (make-elf-note
                   (decode-elf-name (copy-range raw-name 0 name-length)) type
                   (list-ref field* 2))))
              (<~> (<u8vec> name-size)
                   (<skip> <u8> name-padding)
                   (<u8vec> descriptor-size)
                   (<skip> <u8> descriptor-padding)))))))))

  (define decode-notes
    (lambda (bytes endianness-name alignment)
      (let ([note* (parse-bounded-content
                    bytes (<map> list->vector
                                (<many> (note-parser endianness-name (max 4 alignment)))))])
        (and note* (make-elf-note-table note*)))))

  (define linked-section-type?
    (lambda (headers index type*)
      (and (< index (vector-length headers))
           (memv (elf-section-header-type (vector-ref headers index)) type*))))

  (define symbol-table-entry-count
    (lambda (class header raw)
      (let ([standard (if (eq? class 'elf32) 16 24)]
            [entry-size (elf-section-header-entry-size header)])
        (and (= entry-size standard)
             (zero? (modulo (bytevector-length raw) entry-size))
             (/ (bytevector-length raw) entry-size)))))

  (define symbol-xindices
    (lambda (symbol-index headers raw-sections endianness-name expected-count)
      ;; Absent and invalid companions must remain distinct: invalid cannot bypass checks.
      (let loop ([i 0] [found #f])
        (if (= i (vector-length headers)) found
            (let ([header (vector-ref headers i)])
              (if (and (= 18 (elf-section-header-type header))
                       (= symbol-index (elf-section-header-link header)))
                  (if (or found
                          (not (= 4 (elf-section-header-entry-size header)))
                          (not (= (* expected-count 4)
                                  (bytevector-length (vector-ref raw-sections i)))))
                      'invalid
                      (let ([indices (parse-word-vector
                                      (vector-ref raw-sections i) 4 endianness-name)])
                        (if indices (loop (fx1+ i) indices) 'invalid)))
                  (loop (fx1+ i) found)))))))

  (define hash-table-parser
    (lambda (endianness-name)
      (let ([<word> (unsigned-parser 4 endianness-name)])
        (<bind>
         (<~> <word> <word>)
         (lambda (count*)
           (<map>
            (lambda (table*)
              (make-elf-hash-table (list->vector (list-ref table* 0))
                                   (list->vector (list-ref table* 1))))
            (<~> (<rep> <word> (list-ref count* 0))
                 (<rep> <word> (list-ref count* 1)))))))))

  (define dynamic-entry-parser
    (lambda (word endianness-name)
      (<map>
       (lambda (field*) (make-elf-dynamic-entry (car field*) (cadr field*)))
       (<~> (signed-parser word endianness-name)
            (unsigned-parser word endianness-name)))))

  (define relocation-symbols-valid?
    (lambda (relocations symbol-count)
      (let ([relocation* (elf-relocation-table-relocations relocations)])
        (let loop ([i 0])
          (or (= i (vector-length relocation*))
              (and (< (elf-relocation-symbol-index (vector-ref relocation* i))
                      symbol-count)
                   (loop (+ i 1))))))))

  (define decode-section-content
    (lambda (raw class endianness-name machine section-index header all-headers raw-sections
             alignment)
      (let* ([type (elf-section-header-type header)]
             [size (bytevector-length raw)]
             [word (if (eq? class 'elf32) 4 8)]
             [link (elf-section-header-link header)]
             [linked (and (< link (vector-length all-headers))
                          (vector-ref raw-sections link))])
        (case type
          [(3) (and (valid-string-table? raw) (make-elf-string-table raw))]
          [(2 11)
           (and (linked-section-type? all-headers link '(3))
                (let ([count (symbol-table-entry-count class header raw)])
                  (and count
                       (decode-symbols
                        raw class endianness-name header linked
                        (symbol-xindices section-index all-headers raw-sections
                                         endianness-name count)
                        (vector-length all-headers)))))]
          [(4 9)
           (and (or (zero? link) (linked-section-type? all-headers link '(2 11)))
                (< (elf-section-header-info header) (vector-length all-headers))
                (let* ([symbol-count
                        ;; STN_UNDEF relocations need no symbol table when sh_link is zero.
                        (if (zero? link) 1
                            (symbol-table-entry-count
                             class (vector-ref all-headers link) (vector-ref raw-sections link)))]
                       [relocations (and symbol-count
                                         (decode-relocations
                                          raw class endianness-name machine header (= type 4)))])
                  (and relocations
                       (relocation-symbols-valid? relocations symbol-count)
                       relocations)))]
          [(19) (and (= word (elf-section-header-entry-size header))
                     (let ([entry* (parse-word-vector raw word endianness-name)])
                       (and entry* (make-elf-relr-table entry*))))]
          [(5)
           (and (linked-section-type? all-headers link '(2 11))
                (= 4 (elf-section-header-entry-size header))
                (let* ([count (symbol-table-entry-count
                               class (vector-ref all-headers link) linked)]
                       [table (parse-bounded-content raw (hash-table-parser endianness-name))])
                  (and count table (= count (vector-length (elf-hash-table-chains table)))
                       (vfor-all (lambda (index) (< index count))
                                 (elf-hash-table-buckets table))
                       (vfor-all (lambda (index) (< index count))
                                 (elf-hash-table-chains table))
                       table)))]
          [(17)
           (and (linked-section-type? all-headers link '(2 11))
                (>= size 4) (zero? (modulo size 4))
                (= 4 (elf-section-header-entry-size header))
                (let* ([count (symbol-table-entry-count
                               class (vector-ref all-headers link) linked)]
                       [table (parse-bounded-content
                               raw
                               (<map> (lambda (fields)
                                        (make-elf-group-section
                                         (car fields) (list->vector (cadr fields))))
                                      (<~> (unsigned-parser 4 endianness-name)
                                           (<rep> (unsigned-parser 4 endianness-name)
                                                  (fx1- (/ size 4))))))])
                  (and count (< (elf-section-header-info header) count) table
                       (vfor-all (lambda (index) (< 0 index (vector-length all-headers)))
                                 (elf-group-section-members table))
                       table)))]
          [(18) (and (linked-section-type? all-headers link '(2 11))
                     (= 4 (elf-section-header-entry-size header))
                     (let ([symbol-count
                            (symbol-table-entry-count
                             class (vector-ref all-headers link)
                             (vector-ref raw-sections link))])
                       (and symbol-count (= size (* symbol-count 4))))
                     (let ([entry* (parse-word-vector raw 4 endianness-name)])
                       (and entry* (make-elf-word-table entry*))))]
          [(14 15 16) (and (= word (elf-section-header-entry-size header))
                           (let ([entry* (parse-word-vector raw word endianness-name)])
                             (and entry* (make-elf-word-table entry*))))]
          [(6) (and (linked-section-type? all-headers link '(3))
                    (zero? (modulo size (* 2 word)))
                    (= (* 2 word) (elf-section-header-entry-size header))
                    (let ([entry*
                           (parse-bounded-content
                            raw
                            (vector-parser
                             (<bounded> (* 2 word)
                                        (dynamic-entry-parser word endianness-name))
                             (/ size (* 2 word))))])
                      (and entry* (make-elf-dynamic-table entry*))))]
          [(7) (decode-notes raw endianness-name alignment)]
          [else (make-elf-raw-section raw)]))))

  (define <elf-class>
    (</> (<as> 'elf32 (<uimm8> 1)) (<as> 'elf64 (<uimm8> 2))))

  (define <elf-endianness>
    (</> (<as> 'little (<uimm8> 1)) (<as> 'big (<uimm8> 2))))

  (define <elf-identification>
    (<~> (<u8*> #x7f #x45 #x4c #x46)
         <elf-class>
         <elf-endianness>
         (<uimm8> 1)
         <u8>
         <u8>
         (<u8vec> 7)))

  (define <elf-header>
    (<bind>
     <elf-identification>
     (lambda (identification)
       (let* ([class (list-ref identification 1)]
              [endianness-name (list-ref identification 2)]
              [endianness (if (eq? endianness-name 'little)
                              (endianness little) (endianness big))]
              [word-size (if (eq? class 'elf32) 4 8)]
              [sizes-at (if (eq? class 'elf32) 40 52)]
              [<half> (unsigned-parser 2 endianness-name)]
              [<word> (unsigned-parser 4 endianness-name)]
              [<address> (unsigned-parser word-size endianness-name)])
         (<map>
          (lambda (fields)
            (make-decoded-elf-header
             class endianness-name endianness
             (list-ref fields 0) (list-ref fields 1) (list-ref fields 2)
             (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
             (list-ref fields 6) (list-ref fields 7) (list-ref fields 8)
             (list-ref fields 9) (list-ref fields 10) (list-ref fields 11)
             (list-ref fields 12) sizes-at))
          (<~> <half> <half> <word> <address> <address> <address> <word>
               <half> <half> <half> <half> <half> <half>))))))

  (define program-header-parser
    (lambda (class endianness-name)
      (let* ([word-size (if (eq? class 'elf32) 4 8)]
             [<word> (unsigned-parser 4 endianness-name)]
             [<address> (unsigned-parser word-size endianness-name)])
        (<map>
         (lambda (fields)
           (if (eq? class 'elf32)
               (make-elf-program-header
                (list-ref fields 0) (list-ref fields 6) (list-ref fields 1)
                (list-ref fields 2) (list-ref fields 3) (list-ref fields 4)
                (list-ref fields 5) (list-ref fields 7) #f)
               (make-elf-program-header
                (list-ref fields 0) (list-ref fields 1) (list-ref fields 2)
                (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
                (list-ref fields 6) (list-ref fields 7) #f)))
         (if (eq? class 'elf32)
             (<~> <word> <address> <address> <address> <address> <address> <word> <address>)
             (<~> <word> <word> <address> <address> <address> <address> <address>
                  <address>))))))

  (define section-header-parser
    (lambda (class endianness-name)
      (let* ([word-size (if (eq? class 'elf32) 4 8)]
             [<word> (unsigned-parser 4 endianness-name)]
             [<address> (unsigned-parser word-size endianness-name)])
        (<map>
         (lambda (fields)
           (make-elf-section-header
            (list-ref fields 0) "" (list-ref fields 1) (list-ref fields 2)
            (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
            (list-ref fields 6) (list-ref fields 7) (list-ref fields 8)
            (list-ref fields 9)))
         (<~> <word> <word> <address> <address> <address> <address>
              <word> <word> <address> <address>)))))

  (define problem-parser
    (lambda (problem length)
      (<pos-at> (min (elf-issue-offset problem) length)
                (<fail-with> (elf-issue-message problem)))))

  (define header-problem
    (lambda (header)
      (let* ([class (decoded-elf-header-class header)]
             [standard-header (if (eq? class 'elf32) 52 64)]
             [standard-program (if (eq? class 'elf32) 32 56)]
             [standard-section (if (eq? class 'elf32) 40 64)]
             [sizes-at (decoded-elf-header-sizes-at header)])
        (cond
          [(not (= 1 (decoded-elf-header-version header)))
           (issue 20 "invalid ELF version")]
          [(not (= standard-header (decoded-elf-header-header-size header)))
           (issue sizes-at "invalid ELF header size")]
          [(and (not (zero? (decoded-elf-header-raw-phnum header)))
                (not (= standard-program (decoded-elf-header-phentsize header))))
           (issue (+ sizes-at 2) "invalid program-header entry size")]
          [(and (or (not (zero? (decoded-elf-header-raw-shnum header)))
                    (not (zero? (decoded-elf-header-shoff header))))
                (not (= standard-section (decoded-elf-header-shentsize header))))
           (issue (+ sizes-at 6) "invalid section-header entry size")]
          [(>= (decoded-elf-header-raw-shnum header) #xff00)
           (issue (+ sizes-at 8) "reserved direct section count")]
          [(<= #xff00 (decoded-elf-header-raw-shstrndx header) #xfffe)
           (issue (+ sizes-at 10) "reserved direct section-name index")]
          [else #f]))))

  (define table-parser
    (lambda (offset entry-size count entry-parser)
      (<pos-at> offset
                (<bounded> (* entry-size count)
                           (<map> list->vector
                                  (<rep> (<bounded> entry-size entry-parser) count))))))

  (define build-programs
    (lambda (bytes header raw-programs)
      (let* ([length (bytevector-length bytes)]
             [count (vector-length raw-programs)]
             [programs (make-vector count)]
             [table-offset (decoded-elf-header-phoff header)]
             [entry-size (decoded-elf-header-phentsize header)])
        (let loop ([index 0])
          (if (= index count)
              (values programs #f)
              (let* ([raw (vector-ref raw-programs index)]
                     [at (+ table-offset (* index entry-size))]
                     [type (elf-program-header-type raw)]
                     [file-offset (elf-program-header-offset raw)]
                     [virtual-address (elf-program-header-virtual-address raw)]
                     [file-size (elf-program-header-file-size raw)]
                     [memory-size (elf-program-header-memory-size raw)]
                     [alignment (elf-program-header-alignment raw)])
                (cond
                  [(and (not (zero? type)) (not (elf-range-valid? file-offset file-size length)))
                   (values #f (issue file-offset "segment exceeds input"))]
                  [(and (= type 1) (> file-size memory-size))
                   (values #f (issue at "load segment file size exceeds memory size"))]
                  [(and (not (zero? type)) (not (elf-power-of-two? alignment)))
                   (values #f (issue at "invalid segment alignment"))]
                  [(and (= type 1) (> alignment 1)
                        (not (= (modulo file-offset alignment)
                                (modulo virtual-address alignment))))
                   (values #f (issue at "misaligned load segment"))]
                  [else
                   (vector-set!
                    programs index
                    (make-elf-program-header
                     type (elf-program-header-flags raw) file-offset virtual-address
                     (elf-program-header-physical-address raw) file-size memory-size alignment
                     (if (zero? type) (make-bytevector 0)
                         (copy-range bytes file-offset file-size))))
                   (loop (+ index 1))])))))))

  ;; Section ranges are independent of program segments, which may legitimately overlap.
  (define section-overlap-problem
    (lambda (headers)
      (let ([backed
             (list-sort
              (lambda (left right)
                (< (elf-section-header-offset left) (elf-section-header-offset right)))
              (filter
               (lambda (header)
                 (and (not (memv (elf-section-header-type header) '(0 8)))
                      (positive? (elf-section-header-size header))))
               (vector->list headers)))])
        (let loop ([remaining backed] [end 0])
          (and (pair? remaining)
               (let* ([header (car remaining)] [start (elf-section-header-offset header)])
                 (if (< start end)
                     (issue start "overlapping file-backed sections")
                     (loop (cdr remaining) (+ start (elf-section-header-size header))))))))))

  (define section-group-problem
    (lambda (type sections)
      (let ([owners (make-vector (vector-length sections) #f)])
        (or
         (let loop ([index 0])
           (and (fx< index (vector-length sections))
                (let* ([section (vector-ref sections index)]
                       [header (elf-section-header section)]
                       [content (elf-section-content section)])
                  (if (elf-group-section? content)
                      (cond
                        [(not (= type 1))
                         (issue (elf-section-header-offset header)
                                "section groups require a relocatable object")]
                        [(not (zero? (elf-section-header-flags header)))
                         (issue (elf-section-header-offset header)
                                "section-group header flags must be zero")]
                        [else
                         (let ([members (elf-group-section-members content)])
                            (or
                             (let member-loop ([member-index 0])
                               (and (fx< member-index (vector-length members))
                                    (let* ([member (vector-ref members member-index)]
                                           [owner (vector-ref owners member)]
                                           [member-header
                                            (elf-section-header (vector-ref sections member))])
                                      (cond
                                        [(<= member index)
                                         (issue (elf-section-header-offset header)
                                                "section-group header must precede its members")]
                                        [(not (logbit? 9 (elf-section-header-flags member-header)))
                                         (issue (elf-section-header-offset header)
                                                "section-group member lacks SHF_GROUP")]
                                        [(and owner (not (= owner index)))
                                         (issue (elf-section-header-offset header)
                                                "section belongs to multiple groups")]
                                        [else
                                         (vector-set! owners member index)
                                         (member-loop (fx1+ member-index))]))))
                             (loop (fx1+ index))))])
                      (loop (fx1+ index))))))
         (let loop ([index 0])
           (and (fx< index (vector-length sections))
                (let ([header (elf-section-header (vector-ref sections index))])
                  (if (and (not (zero? (elf-section-header-type header)))
                           (logbit? 9 (elf-section-header-flags header)))
                      (cond [(not (= type 1))
                             (issue (elf-section-header-offset header)
                                    "SHF_GROUP requires a relocatable object")]
                            [(not (vector-ref owners index))
                             (issue (elf-section-header-offset header)
                                    "SHF_GROUP section has no group")]
                            [else (loop (fx1+ index))])
                      (loop (fx1+ index))))))))))

  (define build-sections
    (lambda (bytes header headers)
      (let* ([length (bytevector-length bytes)]
             [count (vector-length headers)]
             [raw-sections (make-vector count)]
             [table-offset (decoded-elf-header-shoff header)]
             [entry-size (decoded-elf-header-shentsize header)])
        (let loop ([index 0] [seen-singletons 0])
          (if (= index count)
              (values raw-sections (section-overlap-problem headers))
              (let* ([section (vector-ref headers index)]
                     [at (+ table-offset (* index entry-size))]
                     [file-offset (elf-section-header-offset section)]
                     [size (elf-section-header-size section)]
                     [alignment (elf-section-header-address-alignment section)]
                     [singleton (case (elf-section-header-type section)
                                  [(2) 1] [(11) 2] [(5) 4] [(6) 8] [else 0])])
                (cond
                  [(zero? (elf-section-header-type section))
                   (vector-set! raw-sections index (make-bytevector 0))
                   (loop (fx1+ index) seen-singletons)]
                  [(not (fxzero? (fxlogand singleton seen-singletons)))
                   (values #f (issue at "duplicate singleton section type"))]
                  [(and (= 19 (elf-section-header-type section))
                        (not (memv (decoded-elf-header-type header) '(2 3))))
                   (values #f (issue at "RELR sections require an executable or shared file"))]
                  [(and (logbit? 6 (elf-section-header-flags section))
                        (>= (elf-section-header-info section) count))
                   (values #f (issue at "section info index is out of range"))]
                  [(and (logbit? 7 (elf-section-header-flags section))
                        (>= (elf-section-header-link section) count))
                   (values #f (issue at "section link index is out of range"))]
                  [(not (elf-power-of-two? alignment))
                   (values #f (issue at "invalid section alignment"))]
                  [(and (> alignment 1)
                        (not (zero? (mod (elf-section-header-address section) alignment))))
                   (values #f (issue at "section address violates alignment"))]
                  [(and (not (= 8 (elf-section-header-type section)))
                        (not (elf-range-valid? file-offset size length)))
                   (values #f (issue file-offset "section exceeds input"))]
                  [else
                   (let* ([raw (if (= 8 (elf-section-header-type section))
                                   (make-bytevector 0)
                                   (copy-range bytes file-offset size))])
                     (let-values ([(content problem)
                                   (if (logbit? 11 (elf-section-header-flags section))
                                       (decompress-section
                                        raw (decoded-elf-header-class header)
                                        (decoded-elf-header-endianness-name header)
                                        (decoded-elf-header-type header) section)
                                       (values raw #f))])
                       (if problem (values #f problem)
                           (begin
                             (vector-set! raw-sections index content)
                             (loop (fx1+ index)
                                   (fxlogor seen-singletons singleton))))))])))))))

  (define elf-layout-parser
    (lambda (bytes length header zero)
      (let* ([class (decoded-elf-header-class header)]
             [endianness-name (decoded-elf-header-endianness-name header)]
             [raw-phnum (decoded-elf-header-raw-phnum header)]
             [raw-shnum (decoded-elf-header-raw-shnum header)]
             [raw-shstrndx (decoded-elf-header-raw-shstrndx header)]
             [phnum (if (= raw-phnum #xffff) (elf-section-header-info zero) raw-phnum)]
             [shnum (if (= raw-shnum 0)
                        (if (zero? (decoded-elf-header-shoff header))
                            0
                            (elf-section-header-size zero))
                        raw-shnum)]
             [shstrndx (if (= raw-shstrndx #xffff)
                           (elf-section-header-link zero)
                           raw-shstrndx)])
        (if (and (not (zero? shstrndx)) (>= shstrndx shnum))
            (problem-parser
             (issue (+ (decoded-elf-header-sizes-at header) 10)
                    "invalid section-name table index")
             length)
            (<bind>
             (table-parser
              (decoded-elf-header-phoff header)
              (decoded-elf-header-phentsize header)
              phnum
              (program-header-parser class endianness-name))
             (lambda (raw-programs)
               (<bind>
                (table-parser
                 (decoded-elf-header-shoff header)
                 (decoded-elf-header-shentsize header)
                 shnum
                 (section-header-parser class endianness-name))
                (lambda (headers)
                  (let-values ([(programs program-problem)
                                (build-programs bytes header raw-programs)]
                               [(raw-sections section-problem)
                                (build-sections bytes header headers)])
                    (cond
                      [program-problem (problem-parser program-problem length)]
                      [section-problem (problem-parser section-problem length)]
                      [else
                       (let-values ([(file finish-problem)
                                     (finish-elf
                                      bytes class endianness-name
                                      (decoded-elf-header-type header)
                                      (decoded-elf-header-machine header)
                                      (decoded-elf-header-version header)
                                      (decoded-elf-header-entry header)
                                      (decoded-elf-header-phoff header)
                                      (decoded-elf-header-shoff header)
                                      (decoded-elf-header-flags header)
                                      (decoded-elf-header-header-size header)
                                      (decoded-elf-header-phentsize header) phnum
                                      (decoded-elf-header-shentsize header) shnum shstrndx
                                      programs headers raw-sections)])
                         (if finish-problem
                             (problem-parser finish-problem length)
                             (complete-file-parser file length)))]
                      ))))))))))

  ;; All file ranges and typed contents have already been checked through random access.
  (define complete-file-parser
    (lambda (file length)
      (define-parser complete-parser
        (begin
          (input-pos-set! inp length)
          (values #t file inp)))
      complete-parser))

  (define section-zero-problem
    (lambda (header zero)
      (let ([raw-phnum (decoded-elf-header-raw-phnum header)]
            [raw-shnum (decoded-elf-header-raw-shnum header)]
            [raw-shstrndx (decoded-elf-header-raw-shstrndx header)]
            [shoff (decoded-elf-header-shoff header)])
        (cond
          [(and zero
                (or (not (zero? (elf-section-header-name-index zero)))
                    (not (zero? (elf-section-header-type zero)))
                    (not (zero? (elf-section-header-flags zero)))
                    (not (zero? (elf-section-header-address zero)))
                    (not (zero? (elf-section-header-offset zero)))
                    (not (zero? (elf-section-header-address-alignment zero)))
                    (not (zero? (elf-section-header-entry-size zero)))))
           (issue shoff "invalid extended-count section zero")]
          [(and (= raw-phnum #xffff)
                (< (elf-section-header-info zero) #xffff))
           (issue shoff "invalid extended program-header count")]
          [(and (not (= raw-phnum #xffff)) zero
                (not (zero? (elf-section-header-info zero))))
           (issue shoff "unexpected extended program-header count")]
          [(and (= raw-shnum 0) (not (zero? shoff))
                (< (elf-section-header-size zero) #xff00))
           (issue shoff "invalid extended section count")]
          [(and (not (= raw-shnum 0)) zero
                (not (zero? (elf-section-header-size zero))))
           (issue shoff "unexpected extended section count")]
          [(and (= raw-shstrndx #xffff)
                (< (elf-section-header-link zero) #xff00))
           (issue shoff "invalid extended section-name index")]
          [(and (not (= raw-shstrndx #xffff)) zero
                (not (zero? (elf-section-header-link zero))))
           (issue shoff "unexpected extended section-name index")]
          [else #f]))))

  (define elf-body-parser
    (lambda (bytes length header)
      (let* ([problem (header-problem header)]
             [class (decoded-elf-header-class header)]
             [standard-section (if (eq? class 'elf32) 40 64)]
             [raw-phnum (decoded-elf-header-raw-phnum header)]
             [raw-shnum (decoded-elf-header-raw-shnum header)]
             [raw-shstrndx (decoded-elf-header-raw-shstrndx header)]
             [shoff (decoded-elf-header-shoff header)]
             [ordinary-empty? (and (= raw-shnum 0) (zero? shoff)
                                   (not (= raw-phnum #xffff))
                                   (not (= raw-shstrndx #xffff)))]
             [parse-zero? (not ordinary-empty?)])
        (cond
          [problem (problem-parser problem length)]
          [(and parse-zero? (zero? shoff))
           (problem-parser (issue shoff "missing section zero for extended counts") length)]
          [else
           (<bind>
            (if parse-zero?
                (<pos-at> shoff
                          (<bounded> standard-section
                                     (section-header-parser
                                      class
                                      (decoded-elf-header-endianness-name header))))
                (<result> #f))
            (lambda (zero)
              (let ([zero-problem (section-zero-problem header zero)])
                (if zero-problem
                    (problem-parser zero-problem length)
                    (elf-layout-parser bytes length header zero)))))]))))

  (define finish-elf
    (lambda (bytes class endianness-name type machine version entry phoff shoff flags
                   header-size phentsize phnum shentsize shnum shstrndx programs headers
                   raw-sections)
      (let ([name-bytes (and (> shnum 0) (not (zero? shstrndx))
                             (vector-ref raw-sections shstrndx))])
        (cond
          [(and name-bytes
                (not (= 3 (elf-section-header-type (vector-ref headers shstrndx)))))
           (values #f (issue shoff "section-name table is not a string table"))]
          [(and name-bytes (not (valid-string-table? name-bytes)))
           (values #f (issue (elf-section-header-offset (vector-ref headers shstrndx))
                             "invalid section-name string table"))]
          [else
           (let ([sections (make-vector shnum)])
             (let loop ([i 0])
               (if (= i shnum)
                   (let ([problem (section-group-problem type sections)])
                     (values
                      (and (not problem)
                           (make-elf-file
                            (make-elf-identification
                             class endianness-name 1 (bytevector-u8-ref bytes 7)
                             (bytevector-u8-ref bytes 8))
                            (make-elf-header
                             type machine version entry phoff shoff flags header-size
                             phentsize phnum shentsize shnum shstrndx)
                            programs sections))
                      problem))
                   (let* ([old-header (vector-ref headers i)]
                          [name-index (elf-section-header-name-index old-header)]
                          [name (if (and name-bytes
                                         (not (zero? (elf-section-header-type old-header))))
                                    (nul-string name-bytes name-index) "")])
                     (if (not name)
                         (values #f
                                 (issue (elf-section-header-offset
                                         (vector-ref headers shstrndx))
                                        "invalid section name index or terminator"))
                         (let* ([header (rebuild-section-header old-header name)]
                                [content
                                 (decode-section-content
                                  (vector-ref raw-sections i) class endianness-name machine i header headers
                                  raw-sections
                                  (section-content-alignment bytes class endianness-name header))])
                           (vector-set! headers i header)
                           (if content
                               (begin
                                 (vector-set! sections i (make-elf-section header content))
                                 (loop (+ i 1)))
                               (values
                                #f
                                (issue (elf-section-header-offset header)
                                       "malformed typed section")))))))))]))))

  (define-parser parser-elf
    (let ([bytes (binary-input-data inp)]
          [length (input-len inp)])
      (parser-call
       (<bind> <elf-header>
               (lambda (header)
                 (elf-body-parser bytes length header)))
       inp state (fx1+ lvl))))
  )
