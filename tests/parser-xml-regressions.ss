;; Included by the registered xml parser suite; format-specific regressions.

(mat parser-xml-regressions

     (let* ([document (parse-xml "<a x='a\tb\r\nc&#9;d&#10;e&#13;f'/>")]
            [attribute (vector-ref (xml-element-attributes (xml-document-root document)) 0)])
       (string=? "a b c\td\ne\rf" (xml-attribute-value attribute)))

     (let* ([document (parse-xml "<a>one&amp;two&#65;<![CDATA[raw]]>three&amp;<!--c-->end</a>")]
            [children (xml-element-children (xml-document-root document))])
       (and (= 5 (vector-length children))
            (string=? "one&twoA" (xml-text-value (vector-ref children 0)))
            (xml-cdata? (vector-ref children 1))
            (string=? "three&" (xml-text-value (vector-ref children 2)))
            (xml-comment? (vector-ref children 3))
            (string=? "end" (xml-text-value (vector-ref children 4)))))

     (let* ([count 16000]
            [text (string-append "<a>" (apply string-append (make-list count "&amp;")) "</a>")]
            [before (statistics)]
            [root (xml-document-root (parse-xml text))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? (make-string count #\&)
                      (xml-text-value (vector-ref (xml-element-children root) 0)))
            ;; A generous allocation bound catches repeated copies of accumulated text.
            (< allocated 100000000)))

     )

(mat parser-xml-comment-copy

     (let* ([body (string-append (make-string 250000 #\a) "-b")]
            [text (string-append "<r><!--" body "--></r>")]
            [before (statistics)] [document (parse-xml text)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body (xml-comment-value
                           (vector-ref (xml-element-children
                                        (xml-document-root document)) 0)))
            (< allocated 68000000)))

     ;; Error case: a double hyphen inside comment text is still forbidden.
     (and (parser-error-from (lambda () (parse-xml "<r><!--a--b--></r>"))) #t)

     ;; Error case: a trailing hyphen is not part of a valid comment terminator.
     (and (parser-error-from (lambda () (parse-xml "<r><!--a---></r>"))) #t))

(mat parser-xml-newline-copy

     ;; Newline normalization must keep literal and referenced whitespace distinct.
     (for-all
      (lambda (newline)
        (let* ([document (parse-xml
                          (string-append "<r a='x" newline "y&#13;'><!--a" newline
                                         "b--><![CDATA[c" newline "d]]>e" newline "f</r>"))]
               [root (xml-document-root document)] [children (xml-element-children root)])
          (and (string=? "x y\r" (xml-attribute-value
                                  (vector-ref (xml-element-attributes root) 0)))
               (string=? "a\nb" (xml-comment-value (vector-ref children 0)))
               (string=? "c\nd" (xml-cdata-value (vector-ref children 1)))
               (string=? "e\nf" (xml-text-value (vector-ref children 2))))))
      '("\n" "\r" "\r\n")))

(mat parser-xml-bounded-references

     ;; Error cases: rejecting oversized references must not construct oversized integers.
     (for-all
      (lambda (prefix digit)
        (let* ([text (string-append "<r>" prefix (make-string 16000 digit) ";</r>")]
               [before (statistics)]
               [failure (parser-error-from (lambda () (parse-xml text)))]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and failure (< allocated 10000000))))
      '("&#" "&#x") '(#\9 #\f))

     (let* ([zeros (make-string 16000 #\0)]
            [document (parse-xml (string-append "<r a='&#x" zeros "10ffff;'>&#"
                                               zeros "65;</r>"))]
            [root (xml-document-root document)])
       (and (string=? (string #\x10ffff)
                      (xml-attribute-value (vector-ref (xml-element-attributes root) 0)))
            (string=? "A" (xml-text-value (vector-ref (xml-element-children root) 0))))))

(mat parser-xml-ordinary-characters

     (for-all
      (lambda (prefix suffix)
        (let* ([text (string-append prefix (make-string 100000 #\a) suffix)]
               [before (statistics)] [document (parse-xml text)]
               [allocated (sstats-bytes (sstats-difference (statistics) before))])
          (and (xml-document? document) (< allocated 22000000))))
      '("<r>" "<r><![CDATA[" "<?p ") '("</r>" "]]></r>" "?><r/>"))

     ;; Error case: ]]> remains forbidden in ordinary character data.
     (and (parser-error-from (lambda () (parse-xml "<r>a]]>b</r>"))) #t)

     ;; Error case: forbidden XML scalars cannot pass the ordinary-character shortcut.
     (and (parser-error-from (lambda () (parse-xml "<r><![CDATA[a\x0;b]]></r>"))) #t)

     (let* ([root (xml-document-root (parse-xml "<r>a]b<![CDATA[c]d]]><?p e?f?></r>"))]
            [children (xml-element-children root)])
       (and (string=? "a]b" (xml-text-value (vector-ref children 0)))
            (string=? "c]d" (xml-cdata-value (vector-ref children 1)))
            (string=? "e?f" (xml-processing-instruction-data (vector-ref children 2))))))

(mat parser-xml-ordinary-attribute-characters

     (let* ([body (make-string 100000 #\a)] [source (string-append "<r a='" body "'/>")]
            [before (statistics)] [document (parse-xml source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body (xml-attribute-value
                           (vector-ref (xml-element-attributes (xml-document-root document)) 0)))
            (< allocated 25000000)))

     ;; Literal whitespace normalizes to spaces; a referenced carriage return stays content.
     (let* ([document (parse-xml "<r a='x\t\r\ny&#xD;&amp;&quot;'/>")]
            [attribute (vector-ref (xml-element-attributes (xml-document-root document)) 0)])
       (string=? "x  y\r&\"" (xml-attribute-value attribute)))

     ;; Error cases: direct success cannot bypass forbidden characters or reference checking.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-xml source))) #t))
              '("<r a='a<b'/>" "<r a='a&no;'/>" "<r a='x&#0;'/>" "<r a='x\x0;'/>")))

(mat parser-xml-cr-normalization-cost

     (let* ([count 1000000] [source (string-append "<r>" (make-string count #\return) "</r>")]
            [before (statistics)] [document (parse-xml source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))]
            [text (xml-text-value (vector-ref
                                   (xml-element-children (xml-document-root document)) 0))])
       (and (string=? text (make-string count #\newline))
            (char=? #\return (string-ref source 3))
            (< allocated 154000000))))

(mat xml-processing-instruction-optional-data

     (equal? '(#f "" "x")
             (map (lambda (source)
                    (xml-processing-instruction-data
                     (vector-ref (xml-document-before-root (parse-xml source)) 0)))
                  '("<?p?><a/>" "<?p ?><a/>" "<?p x?><a/>"))))
