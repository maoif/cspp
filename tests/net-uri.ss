(import (cspp) (cspp net))

(mat net-uri-raw-components
     (let ([u (string->uri "http://example.test/a%2Fb?q=%2F#frag%2F")])
       (and (string=? "/a%2Fb" (uri-raw-path u))
            (string=? "/a/b" (uri-path u))
            (string=? "q=%2F" (uri-raw-query u))
            (string=? "q=/" (uri-query u))
            (string=? "frag/" (uri-fragment u))
            (string=? "http://example.test/a%2Fb?q=%2F#frag%2F" (uri->string u))
            (let ([updated (uri-update u 'path "/a%2Fc")])
              (and (string=? "/a/c" (uri-path updated))
                   (string=? "/a%2Fc" (uri-raw-path updated))
                   (string=? "http://example.test/a%2Fc?q=%2F#frag%2F"
                             (uri->string updated)))))))

(mat net-uri-constructor
     (string=? "https://example.test/a%2Fb"
               (uri->string (make-uri "https" #f "example.test" #f "/a%2Fb" #f #f)))
     (string=? "https://example.test/next?q=1#part"
               (uri->string
                (uri-with-fragment
                 (uri-with-query
                  (uri-with-path
                   (make-uri "https" #f "example.test" #f "/" #f #f)
                   "/next")
                  "q=1")
                 "part"))))

(define rfc3986-resolution-examples
  '(("g:h" . "g:h")
    ("g" . "http://a/b/c/g")
    ("./g" . "http://a/b/c/g")
    ("g/" . "http://a/b/c/g/")
    ("/g" . "http://a/g")
    ("//g" . "http://g")
    ("?y" . "http://a/b/c/d;p?y")
    ("g?y" . "http://a/b/c/g?y")
    ("#s" . "http://a/b/c/d;p?q#s")
    ("g#s" . "http://a/b/c/g#s")
    ("g?y#s" . "http://a/b/c/g?y#s")
    (";x" . "http://a/b/c/;x")
    ("g;x" . "http://a/b/c/g;x")
    ("g;x?y#s" . "http://a/b/c/g;x?y#s")
    ("" . "http://a/b/c/d;p?q")
    ("." . "http://a/b/c/")
    ("./" . "http://a/b/c/")
    (".." . "http://a/b/")
    ("../" . "http://a/b/")
    ("../g" . "http://a/b/g")
    ("../.." . "http://a/")
    ("../../" . "http://a/")
    ("../../g" . "http://a/g")
    ("../../../g" . "http://a/g")
    ("../../../../g" . "http://a/g")
    ("/./g" . "http://a/g")
    ("/../g" . "http://a/g")
    ("g." . "http://a/b/c/g.")
    (".g" . "http://a/b/c/.g")
    ("g.." . "http://a/b/c/g..")
    ("..g" . "http://a/b/c/..g")
    ("./../g" . "http://a/b/g")
    ("./g/." . "http://a/b/c/g/")
    ("g/./h" . "http://a/b/c/g/h")
    ("g/../h" . "http://a/b/c/h")
    ("g;x=1/./y" . "http://a/b/c/g;x=1/y")
    ("g;x=1/../y" . "http://a/b/c/y")
    ("g?y/./x" . "http://a/b/c/g?y/./x")
    ("g?y/../x" . "http://a/b/c/g?y/../x")
    ("g#s/./x" . "http://a/b/c/g#s/./x")
    ("g#s/../x" . "http://a/b/c/g#s/../x")
    ("http:g" . "http:g")))

(mat net-uri-rfc3986-resolution
     (let ([base (string->uri "http://a/b/c/d;p?q")])
       (andmap
        (lambda (example)
          (string=? (cdr example)
                    (uri->string (uri-resolve base (string->uri (car example))))))
        rfc3986-resolution-examples)))

(define idna-error?
  (lambda (domain)
    (guard (condition
            [(net-error? condition)
             (and (eq? 'uri (net-error-kind condition))
                  (eq? 'idna (net-error-operation condition)))]
            [else #f])
      (idna->ascii domain)
      #f)))

(mat net-uri-idna
     (mat-requires (idn2)
       (string=? "xn--bcher-kva.example" (idna->ascii "bücher.example")))
     (mat-requires (idn2)
       (string=? "bücher.example" (idna->unicode "xn--bcher-kva.example")))
     (mat-requires (idn2)
       (string=? "xn--bcher-kva.example" (idna->ascii "BÜCHER.EXAMPLE")))

     ;; IDNA labels may not contain control characters.
     (idna-error? (string-append "bad" (string (integer->char 1)) ".example"))

     ;; A label mixing left-to-right Latin and right-to-left Hebrew violates bidi rules.
     (mat-requires (idn2)
       (idna-error? "aא.example")))
