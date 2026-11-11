(import (cspp)
        (cspp parser xml))

(include "parser-external-tools.ss")
(include "parser-test-support.ss")

(define xml-child-elements
  (lambda (element)
    (filter xml-element? (vector->list (xml-element-children element)))))

(define xml-child-element-ref
  (lambda (element name)
    (let loop ([children (xml-child-elements element)])
      (and (pair? children)
           (let ([child (car children)])
             (if (string=? name (xml-element-name child))
                 child
                 (loop (cdr children))))))))

(define xml-element-text
  (lambda (element)
    (xml-text-value (vector-ref (xml-element-children element) 0))))

(define bytevector-drop
  (lambda (bytevector count)
    (let* ([length (- (bytevector-length bytevector) count)]
           [result (make-bytevector length 0)])
      (bytevector-copy! bytevector count result 0 length)
      result)))

(mat parse-xml-records

     (string=? "#[xml-attribute name: \"id\" value: \"1\"]"
               (format "~s"
                       (vector-ref
                        (xml-element-attributes
                         (xml-document-root (parse-xml "<x id='1'/>")))
                        0)))

     (let* ([document (parse-xml "<root a=\"1\">x<![CDATA[y]]><child/>z</root>")]
            [root (xml-document-root document)]
            [children (vector->list (xml-element-children root))])
       (and (xml-document? document)
            (xml-element? root)
            (string=? "root" (xml-element-name root))
            (= 1 (vector-length (xml-element-attributes root)))
            (xml-text? (list-ref children 0))
            (xml-cdata? (list-ref children 1))
            (xml-element? (list-ref children 2))
            (xml-text? (list-ref children 3))))

     )

(mat parse-xml-lexical

     (let ([root (xml-document-root
                  (parse-xml "<\x3B1;_1>\x41;&#65;&#x41;</\x3B1;_1>"))])
       (and (string=? "\x3B1;_1" (xml-element-name root))
            (string=? "AAA"
                      (xml-text-value
                       (vector-ref (xml-element-children root) 0)))))

     ;; error: a name cannot start with an ASCII digit.
     (parser-error? (parser-error-from (lambda () (parse-xml "<1root/>"))))

     ;; error: a numeric reference must denote an XML Char.
     (parser-error? (parser-error-from (lambda () (parse-xml "<root>&#0;</root>"))))

     ;; error: undeclared general entities are unavailable when DTDs are unsupported.
     (parser-error? (parser-error-from (lambda () (parse-xml "<root>&custom;</root>"))))

     (let ([document
            (parse-xml "<?build release?><root><!--ok--><![CDATA[a<b&c]]></root>")])
       (and (xml-processing-instruction?
             (vector-ref (xml-document-before-root document) 0))
            (xml-comment?
             (vector-ref (xml-element-children (xml-document-root document)) 0))
            (xml-cdata?
             (vector-ref (xml-element-children (xml-document-root document)) 1))))

     ;; error: XML comments cannot contain a double hyphen.
     (parser-error?
      (parser-error-from
       (lambda ()
         (parse-xml "<root><!-- bad -- comment --></root>"))))

     ;; error: CDATA closing delimiters cannot occur in character data.
     (parser-error? (parser-error-from (lambda () (parse-xml "<root><![CDATA[a]]>b]]></root>"))))

     ;; error: processing-instruction targets cannot case-insensitively equal xml.
     (parser-error? (parser-error-from (lambda () (parse-xml "<?XML data?><root/>"))))

     )

(mat parse-xml-structure

     (let ([declaration
            (xml-document-declaration
             (parse-xml
              "<?xml version='1.0' encoding='UTF-8' standalone='yes'?><root/>"))])
       (and (xml-declaration? declaration)
            (string=? "1.0" (xml-declaration-version declaration))
            (string-ci=? "UTF-8" (xml-declaration-encoding declaration))
            (eq? 'yes (xml-declaration-standalone declaration))))

     ;; error: an end tag must match the open element name.
     (parser-error? (parser-error-from (lambda () (parse-xml "<a></b>"))))

     ;; error: an element cannot repeat an attribute name.
     (parser-error? (parser-error-from (lambda () (parse-xml "<a x='1' x='2'/>"))))

     ;; error: an XML document must contain exactly one root element.
     (parser-error? (parser-error-from (lambda () (parse-xml "<a/><b/>"))))

     ;; error: document type declarations are not supported.
     (parser-error? (parser-error-from (lambda () (parse-xml "<!DOCTYPE root><root/>"))))

     (string=? "a\nb\nc"
               (xml-text-value
                (vector-ref
                 (xml-element-children
                  (xml-document-root (parse-xml "<r>a\r\nb\rc</r>")))
                 0)))

     )

(mat parse-xml-files

     (let* ([document (parse-xml-file "data/xml-basic.xml")]
            [declaration (xml-document-declaration document)]
            [root (xml-document-root document)]
            [item (car (xml-child-elements root))]
            [attribute (vector-ref (xml-element-attributes item) 0)])
       (and (xml-document? document)
            (xml-declaration? declaration)
            (string=? "1.0" (xml-declaration-version declaration))
            (string-ci=? "UTF-8" (xml-declaration-encoding declaration))
            (string=? "root" (xml-element-name root))
            (string=? "item" (xml-element-name item))
            (string=? "id" (xml-attribute-name attribute))
            (string=? "1" (xml-attribute-value attribute))
            (string=? "alpha" (xml-element-text item))))

     (let* ([root (xml-document-root (parse-xml-file "data/large-dataset.xml"))]
            [employees (xml-child-elements root)]
            [first (car employees)]
            [last (list-ref employees 12499)])
       (and (string=? "employees" (xml-element-name root))
            (= 12500 (length employees))
            (string=? "employee" (xml-element-name first))
            (string=? "1" (xml-element-text (xml-child-element-ref first "id")))
            (string=? "FirstName1"
                      (xml-element-text (xml-child-element-ref first "firstName")))
            (string=? "12500"
                      (xml-element-text (xml-child-element-ref last "id")))))

     (or (not (external-tool-available? "xmllint"))
         (let* ([path "data/large-dataset.xml"]
                [root (xml-document-root (parse-xml-file path))]
                [employees (xml-child-elements root)]
                [first (car employees)]
                [last (list-ref employees (fx1- (length employees)))]
                [summary
                 (format "~a|~a|~a|~a|~a\n"
                         (xml-element-name root)
                         (length employees)
                         (xml-element-text (xml-child-element-ref first "id"))
                         (xml-element-text (xml-child-element-ref first "firstName"))
                         (xml-element-text (xml-child-element-ref last "id")))]
                [result
                 (process-capture
                  "xmllint" "--xpath"
                  (string-append
                   "concat(name(/*),'|',count(/*/*),'|',"
                   "string(/*/employee[1]/id),'|',"
                   "string(/*/employee[1]/firstName),'|',"
                   "string(/*/employee[last()]/id))")
                  (begin path)
                  :stdout capture
                  :stderr capture
                  :timeout 10000)])
           (equal? summary (successful-process-output result))))

     (xml-document?
      (with-parser-file
       (parser-test-bytes (bytevector #xef #xbb #xbf)
                               (string->utf8 "<root/>"))
       parse-xml-file))

     (and
      (xml-document?
       (with-parser-file
        (string->bytevector
         "<?xml version='1.0' encoding='UTF-16'?><root/>"
         (make-transcoder (utf-16-codec (endianness little))
                          (eol-style none)
                          (error-handling-mode raise)))
        parse-xml-file))
      (xml-document?
       (with-parser-file
        (string->bytevector
         "<?xml version='1.0' encoding='UTF-16'?><root/>"
         (make-transcoder (utf-16-codec (endianness big))
                          (eol-style none)
                          (error-handling-mode raise)))
        parse-xml-file)))

     (and
      (xml-document?
       (with-parser-file
        (bytevector-drop
         (string->bytevector
          "<?xml version='1.0' encoding='UTF-16'?><root/>"
          (make-transcoder (utf-16-codec (endianness little))
                           (eol-style none)
                           (error-handling-mode raise)))
         2)
        parse-xml-file))
      (xml-document?
       (with-parser-file
        (bytevector-drop
         (string->bytevector
          "<?xml version='1.0' encoding='UTF-16'?><root/>"
          (make-transcoder (utf-16-codec (endianness big))
                           (eol-style none)
                           (error-handling-mode raise)))
         2)
        parse-xml-file)))

     ;; error: malformed UTF-8 must not be replaced during decoding.
     (error?
      (with-parser-file (bytevector #xc3 #x28) parse-xml-file))

     ;; error: UTF-16 input must contain complete code units.
     (error?
      (with-parser-file (bytevector #xff #xfe #x3c) parse-xml-file))

     ;; error: the declaration encoding must agree with the detected bytes.
     (error?
      (with-parser-file
       (string->utf8 "<?xml version='1.0' encoding='UTF-16'?><root/>")
       parse-xml-file))

     ;; error: UTF-16 input cannot contain an unpaired high surrogate.
     (error?
      (with-parser-file
       (bytevector #xff #xfe
                   #x3c #x00 #x72 #x00 #x3e #x00
                   #x00 #xd8
                   #x3c #x00 #x2f #x00 #x72 #x00 #x3e #x00)
       parse-xml-file))

     )

(include "parser-xml-regressions.ss")
