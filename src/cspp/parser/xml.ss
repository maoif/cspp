(library (cspp parser xml)
  (export xml-document? xml-document-declaration xml-document-before-root
          xml-document-root xml-document-after-root
          xml-declaration? xml-declaration-version xml-declaration-encoding
          xml-declaration-standalone
          xml-element? xml-element-name xml-element-attributes xml-element-children
          xml-attribute? xml-attribute-name xml-attribute-value
          xml-text? xml-text-value
          xml-cdata? xml-cdata-value
          xml-comment? xml-comment-value
          xml-processing-instruction? xml-processing-instruction-target
          xml-processing-instruction-data
          parse-xml parse-xml-file)
  (import (cspp chez)
          (cspp parser private)
          (cspp parser combinator)
          (cspp file)
          (cspp list)
          (cspp utils))

  (define-record-type xml-document
    (fields (immutable declaration)
            (immutable before-root)
            (immutable root)
            (immutable after-root)))

  (define-record-type xml-declaration
    (fields (immutable version)
            (immutable encoding)
            (immutable standalone)))

  (define-record-type xml-element
    (fields (immutable name)
            (immutable attributes)
            (immutable children)))

  (define-record-type xml-attribute
    (fields (immutable name)
            (immutable value)))

  (define-record-type xml-text
    (fields (immutable value)))

  (define-record-type xml-cdata
    (fields (immutable value)))

  (define-record-type xml-comment
    (fields (immutable value)))

  (define-record-type xml-processing-instruction
    (fields (immutable target)
            (immutable data)))

  (define-parser-record-writer xml-document xml-document
    ([declaration xml-document-declaration]
     [before-root xml-document-before-root]
     [root xml-document-root]
     [after-root xml-document-after-root]))
  (define-parser-record-writer xml-declaration xml-declaration
    ([version xml-declaration-version]
     [encoding xml-declaration-encoding]
     [standalone xml-declaration-standalone]))
  (define-parser-record-writer xml-element xml-element
    ([name xml-element-name]
     [attributes xml-element-attributes]
     [children xml-element-children]))
  (define-parser-record-writer xml-attribute xml-attribute
    ([name xml-attribute-name]
     [value xml-attribute-value]))
  (define-parser-record-writer xml-text xml-text
    ([value xml-text-value]))
  (define-parser-record-writer xml-cdata xml-cdata
    ([value xml-cdata-value]))
  (define-parser-record-writer xml-comment xml-comment
    ([value xml-comment-value]))
  (define-parser-record-writer xml-processing-instruction xml-processing-instruction
    ([target xml-processing-instruction-target]
     [data xml-processing-instruction-data]))

;;;;===----------------------------------------------------------------------===
;;;; XML character and name classes
;;;;===----------------------------------------------------------------------===

  (define xml-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (or (= value #x9)
            (= value #xa)
            (= value #xd)
            (<= #x20 value #xd7ff)
            (<= #xe000 value #xfffd)
            (<= #x10000 value #x10ffff)))))

  (define xml-name-start-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (or (char=? character #\:)
            (char<=? #\A character #\Z)
            (char=? character #\_)
            (char<=? #\a character #\z)
            (<= #xc0 value #xd6)
            (<= #xd8 value #xf6)
            (<= #xf8 value #x2ff)
            (<= #x370 value #x37d)
            (<= #x37f value #x1fff)
            (<= #x200c value #x200d)
            (<= #x2070 value #x218f)
            (<= #x2c00 value #x2fef)
            (<= #x3001 value #xd7ff)
            (<= #xf900 value #xfdcf)
            (<= #xfdf0 value #xfffd)
            (<= #x10000 value #xeffff)))))

  (define xml-name-character?
    (lambda (character)
      (let ([value (char->integer character)])
        (or (xml-name-start-character? character)
            (char=? character #\-)
            (char=? character #\.)
            (char<=? #\0 character #\9)
            (= value #xb7)
            (<= #x300 value #x36f)
            (<= #x203f value #x2040)))))

  (define <xml-character>
    (<satisfy-char> xml-character? "not an XML character"))

  (define <name-start-character>
    (<satisfy-char> xml-name-start-character? "invalid XML name start"))

  (define <name-character>
    (<satisfy-char> xml-name-character? "invalid XML name character"))

  (define <name>
    (<map> (lambda (value)
             (list->string (cons (car value) (cadr value))))
           (<~> <name-start-character> (<many> <name-character>))))

  (define <whitespace-character>
    (<one-of> "\x20;\x9;\xA;"))

  (define <S> (<some> <whitespace-character>))
  (define <S?> (<many> <whitespace-character>))

;;;;===----------------------------------------------------------------------===
;;;; References and quoted values
;;;;===----------------------------------------------------------------------===

  (define (<validated-character-reference> parser radix)
    (<bind> parser
            (lambda (digits)
              (let ([value (string->number (list->string digits) radix)])
                (if (and value
                         (<= value #x10ffff)
                         (not (<= #xd800 value #xdfff))
                         (xml-character? (integer->char value)))
                    (<result> (integer->char value))
                    (<fail-with> "numeric reference is not an XML character"))))))

  (define <decimal-character-reference>
    (<~1> (<string> "&#")
          (<validated-character-reference> (<some> <digit>) 10)
          (<char> #\;)))

  (define <hexadecimal-character-reference>
    (<~1> (<string> "&#x")
          (<validated-character-reference> (<some> <hexdigit>) 16)
          (<char> #\;)))

  (define <predefined-reference>
    (</> (<as> #\< (<string> "&lt;"))
         (<as> #\> (<string> "&gt;"))
         (<as> #\& (<string> "&amp;"))
         (<as> #\' (<string> "&apos;"))
         (<as> #\" (<string> "&quot;"))))

  (define <reference>
    (</> <hexadecimal-character-reference>
         <decimal-character-reference>
         <predefined-reference>))

  (define (attribute-value-parser quote)
    (<map> list->string
           (<~1> (<char> quote)
                 (<many>
                  (</> <reference>
                       (<as> #\space (<one-of> "\t\n\r"))
                       (<satisfy-char>
                        (lambda (character)
                          (and (xml-character? character)
                               (not (char=? character #\<))
                               (not (char=? character #\&))
                               (not (char=? character quote))))
                        "invalid XML attribute character")))
                 (<char> quote))))

  (define <attribute-value>
    (</> (attribute-value-parser #\")
         (attribute-value-parser #\')))

;;;;===----------------------------------------------------------------------===
;;;; Comments, CDATA, and processing instructions
;;;;===----------------------------------------------------------------------===

  (define <not-comment-hyphen>
    (<satisfy-char>
     (lambda (character)
       (and (xml-character? character)
            (not (char=? character #\-))))
     "invalid XML comment character"))

  (define <comment-piece>
    (</> (<map> list <not-comment-hyphen>)
         (<map> (lambda (value) (list #\- (cadr value)))
                (<~> (<char> #\-) <not-comment-hyphen>))))

  (define <comment>
    (<map> (lambda (pieces)
             (make-xml-comment (list->string (apply append pieces))))
           (<~1> (<string> "<!--")
                 (<many> <comment-piece>)
                 (<string> "-->"))))

  (define <not-cdata-end>
    (<~1> (<not-followed-by> (<result> #t) (<string> "]]>") )
          <xml-character>))

  (define <cdata>
    (<map> (lambda (characters)
             (make-xml-cdata (list->string characters)))
           (<~1> (<string> "<![CDATA[")
                 (<many> <not-cdata-end>)
                 (<string> "]]>"))))

  (define <not-processing-instruction-end>
    (<~1> (<not-followed-by> (<result> #t) (<string> "?>"))
          <xml-character>))

  (define <processing-instruction>
    (<~1> (<string> "<?")
          (<bind> <name>
                  (lambda (target)
                    (if (string-ci=? target "xml")
                        (<fail-with> "processing-instruction target cannot be XML")
                        (<map> (lambda (value)
                                 (make-xml-processing-instruction
                                  target
                                  (if (string? (car value)) (car value) #f)))
                               (<~> (<optional>
                                     (~> <S>
                                         (<as-string>
                                          (<many>
                                           <not-processing-instruction-end>))))
                                    (<string> "?>"))))))))

;;;;===----------------------------------------------------------------------===
;;;; XML declaration
;;;;===----------------------------------------------------------------------===

  (define <equals>
    (<~1> <S?> (<char> #\=) <S?>))

  (define (<quoted-text> parser)
    (</> (<~1> (<char> #\") parser (<char> #\"))
         (<~1> (<char> #\') parser (<char> #\'))))

  (define <version-value>
    (<bind> (<quoted-text>
             (<map> (lambda (value)
                      (string-append "1." (cadr value)))
                    (<~> (<string> "1.") (<as-string> (<some> <digit>)))))
            (lambda (version)
              (if (string=? version "1.0")
                  (<result> version)
                  (<fail-with> "only XML version 1.0 is supported")))))

  (define <encoding-name>
    (<map> (lambda (value)
             (list->string (cons (car value) (cadr value))))
           (<~> (<satisfy-char> (lambda (character)
                                  (or (char<=? #\a character #\z)
                                      (char<=? #\A character #\Z))))
                (<many> (<satisfy-char>
                         (lambda (character)
                           (or (char<=? #\a character #\z)
                               (char<=? #\A character #\Z)
                               (char<=? #\0 character #\9)
                               (memv character '(#\. #\_ #\-)))))))))

  (define <encoding-info>
    (<~2> (<string> "encoding") <equals> (<quoted-text> <encoding-name>)))

  (define <standalone-info>
    (<map> string->symbol
           (<~2> (<string> "standalone")
                 <equals>
                 (<quoted-text> (</> (<string> "yes") (<string> "no"))))))

  (define <xml-declaration>
    (<map> (lambda (value)
             (make-xml-declaration
              (caddr value)
              (let ([encoding (list-ref value 3)])
                (if (string? encoding) encoding #f))
              (let ([standalone (list-ref value 4)])
                (if (symbol? standalone) standalone #f))))
           (<~> (<string> "<?xml")
                <S>
                (<~2> (<string> "version") <equals> <version-value>)
                (<optional> (~> <S> <encoding-info>))
                (<optional> (~> <S> <standalone-info>))
                <S?>
                (<string> "?>"))))

;;;;===----------------------------------------------------------------------===
;;;; Elements and documents
;;;;===----------------------------------------------------------------------===

  (define <attribute>
    (<map> (lambda (value)
             (make-xml-attribute (car value) (caddr value)))
           (<~> <name> <equals> <attribute-value>)))

  (define duplicate-attribute?
    (lambda (attributes)
      (let ([seen (make-hashtable string-hash string=?)])
        (let loop ([attributes attributes])
          (and (pair? attributes)
               (let ([name (xml-attribute-name (car attributes))])
                 (if (hashtable-contains? seen name) #t
                     (begin (hashtable-set! seen name #t)
                            (loop (cdr attributes))))))))))

  (define merge-adjacent-text
    (lambda (nodes)
      (let ([builder (make-list-builder)])
        (let loop ([nodes nodes])
          (cond [(null? nodes) (builder)]
                [(xml-text? (car nodes))
                 ;; Size each text run first, then copy each fragment exactly once.
                 (let scan ([end (cdr nodes)]
                            [length (string-length (xml-text-value (car nodes)))])
                   (if (and (pair? end) (xml-text? (car end)))
                       (scan (cdr end)
                             (fx+ length (string-length (xml-text-value (car end)))))
                       (begin
                         (if (eq? end (cdr nodes))
                             (builder (car nodes))
                             (let ([text (make-string length)])
                               (let copy ([parts nodes] [offset 0])
                                 (unless (eq? parts end)
                                   (let* ([part (xml-text-value (car parts))]
                                          [size (string-length part)])
                                     (string-copy! part 0 text offset size)
                                     (copy (cdr parts) (fx+ offset size)))))
                               (builder (make-xml-text text))))
                         (loop end))))]
                [else (builder (car nodes)) (loop (cdr nodes))])))))

  (define <reference-text>
    (<map> (lambda (character)
             (make-xml-text (string character)))
           <reference>))

  (define <character-data-character>
    (<~1> (<not-followed-by> (<result> #t) (<string> "]]>") )
          (<satisfy-char>
           (lambda (character)
             (and (xml-character? character)
                  (not (char=? character #\<))
                  (not (char=? character #\&))))
           "invalid XML character data")))

  (define <character-data>
    (<map> (lambda (characters)
             (make-xml-text (list->string characters)))
           (<some> <character-data-character>)))

  (declare-lazy-parser <element>)

  (define <content-node>
    (</> <cdata>
         <comment>
         <processing-instruction>
         <element>
         <reference-text>
         <character-data>))

  (define <end-tag>
    (<~1> (<string> "</") <name> <S?> (<char> #\>)))

  (define <element-parser>
    (<bind> (<~1> (<char> #\<) <name>)
            (lambda (name)
              (<bind> (<~> (<many> (~> <S> <attribute>)) <S?>)
                      (lambda (value)
                        (let ([attributes (car value)])
                          (if (duplicate-attribute? attributes)
                              (<fail-with> "duplicate XML attribute")
                              (</> (<as> (make-xml-element
                                          name
                                          (list->vector attributes)
                                          '#())
                                        (<string> "/>"))
                                   (~> (<char> #\>)
                                       (<bind> (<many> <content-node>)
                                               (lambda (children)
                                                 (<bind> <end-tag>
                                                         (lambda (end-name)
                                                           (if (string=? name end-name)
                                                               (<result>
                                                                (make-xml-element
                                                                 name
                                                                 (list->vector attributes)
                                                                 (list->vector
                                                                  (merge-adjacent-text
                                                                   children))))
                                                               (<fail-with>
                                                                (string-append
                                                                 "end tag does not match "
                                                                 "open tag"))))))
                                                         ))))))))))

  (define <misc-node>
    (</> <comment>
         <processing-instruction>
         (<as> #f <S>)))

  (define xml-misc-node?
    (lambda (value)
      (or (xml-comment? value)
          (xml-processing-instruction? value))))

  (define parser-xml
    (begin
      (install-lazy-parser! <element> <element-parser>)
      (<map> (lambda (value)
               (make-xml-document
                (let ([declaration (car value)])
                  (if (xml-declaration? declaration) declaration #f))
                (list->vector (filter xml-misc-node? (cadr value)))
                (caddr value)
                (list->vector (filter xml-misc-node? (list-ref value 3)))))
             (<~> (<optional> <xml-declaration>)
                  (<many> <misc-node>)
                  <element>
                  (<many> <misc-node>)
                  <eof>))))

  (define normalize-xml-newlines
    (lambda (text)
      (let* ([length (string-length text)]
             [normalized (make-string length)])
        (let loop ([source-index 0] [target-index 0])
          (if (= source-index length)
              (substring normalized 0 target-index)
              (let ([character (string-ref text source-index)])
                (if (char=? character #\return)
                    (begin
                      (string-set! normalized target-index #\newline)
                      (loop (if (and (< (fx1+ source-index) length)
                                     (char=? (string-ref text (fx1+ source-index))
                                             #\newline))
                                (fx+ source-index 2)
                                (fx1+ source-index))
                            (fx1+ target-index)))
                    (begin
                      (string-set! normalized target-index character)
                      (loop (fx1+ source-index) (fx1+ target-index))))))))))

;;;;===----------------------------------------------------------------------===
;;;; File decoding
;;;;===----------------------------------------------------------------------===

  (define bytevector-slice
    (lambda (bytevector start)
      (let* ([length (- (bytevector-length bytevector) start)]
             [result (make-bytevector length 0)])
        (bytevector-copy! bytevector start result 0 length)
        result)))

  (define detect-xml-encoding
    (lambda (bytevector)
      (let ([length (bytevector-length bytevector)])
        (cond [(and (>= length 3)
                    (= #xef (bytevector-u8-ref bytevector 0))
                    (= #xbb (bytevector-u8-ref bytevector 1))
                    (= #xbf (bytevector-u8-ref bytevector 2)))
               (values 'utf-8 3)]
              [(and (>= length 2)
                    (= #xfe (bytevector-u8-ref bytevector 0))
                    (= #xff (bytevector-u8-ref bytevector 1)))
               (values 'utf-16be 2)]
              [(and (>= length 2)
                    (= #xff (bytevector-u8-ref bytevector 0))
                    (= #xfe (bytevector-u8-ref bytevector 1)))
               (values 'utf-16le 2)]
              [(and (>= length 4)
                    (= 0 (bytevector-u8-ref bytevector 0))
                    (= #x3c (bytevector-u8-ref bytevector 1))
                    (= 0 (bytevector-u8-ref bytevector 2)))
               (values 'utf-16be 0)]
              [(and (>= length 4)
                    (= #x3c (bytevector-u8-ref bytevector 0))
                    (= 0 (bytevector-u8-ref bytevector 1))
                    (= 0 (bytevector-u8-ref bytevector 3)))
               (values 'utf-16le 0)]
              [else (values 'utf-8 0)]))))

  (define decode-xml-bytevector
    (lambda (bytevector)
      (let-values ([(encoding signature-length)
                    (detect-xml-encoding bytevector)])
        (values
         (parser-decode-bytes (bytevector-slice bytevector signature-length) encoding)
         encoding))))

  (define encoding-declaration-compatible?
    (lambda (detected declared)
      (or (not declared)
          (case detected
            [(utf-8) (string-ci=? declared "UTF-8")]
            [(utf-16le)
             (or (string-ci=? declared "UTF-16")
                 (string-ci=? declared "UTF-16LE"))]
            [(utf-16be)
             (or (string-ci=? declared "UTF-16")
                 (string-ci=? declared "UTF-16BE"))]
            [else #f]))))

;;;;===----------------------------------------------------------------------===
;;;; Public API
;;;;===----------------------------------------------------------------------===

  #|proc:parse-xml
  The `parse-xml` procedure parses XML 1.0 string `text` and returns an `xml-document`.
  Document type declarations and declared general entities are not supported. Error offsets
  refer to text after CR and CRLF are normalized to LF; lines and columns are zero-based.
  |#
  (define parse-xml
    (lambda (text)
      (pcheck ([string? text])
              (run-textual-parser parser-xml (normalize-xml-newlines text)))))

  #|proc:parse-xml-file
  The `parse-xml-file` procedure parses the regular XML file at string `path` and returns an
  `xml-document`. Document type declarations and declared general entities are not supported.
  Syntax failures raise `&parser-error` with `path` as the source and offsets in newline-normalized
  text. File, decoding, and encoding-declaration conflicts propagate as non-parser exceptions.
  |#
  (define parse-xml-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (let-values ([(text encoding)
                            (decode-xml-bytevector (read-u8vec path))])
                (let* ([document (run-textual-parser/source
                                  parser-xml (normalize-xml-newlines text) path)]
                       [declaration (xml-document-declaration document)]
                       [declared (and declaration
                                      (xml-declaration-encoding declaration))])
                  (if (encoding-declaration-compatible? encoding declared)
                      document
                      (errorf 'parse-xml-file
                              "declared encoding ~a conflicts with detected ~a"
                              declared encoding)))))))

  )
