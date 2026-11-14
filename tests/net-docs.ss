(import (cspp)
        (cspp path))

(define read-text-file
  (lambda (path)
    (call-with-port
     (open-file-input-port path)
     (lambda (port) (utf8->string (get-bytevector-all port))))))

(mat net-public-api-documentation
     (= 0
        (system
         "../cspp --script=../tools/check-public-api-docs.ss \
../src/cspp/net ../src/cspp/format/protobuf ../src/cspp/format/protobuf.ss ../src/cspp/optional-library.ss >/dev/null 2>&1")))

(mat reviewed-data-structure-documentation
     (= 0
        (system
         "../cspp --script=../tools/check-public-api-docs.ss \
../src/cspp/array.ss ../src/cspp/bittree.ss ../src/cspp/bitvec.ss ../src/cspp/dlist.ss \
../src/cspp/dset.ss ../src/cspp/hashset.ss ../src/cspp/heap.ss ../src/cspp/list.ss \
../src/cspp/queue.ss ../src/cspp/stack.ss ../src/cspp/treeset.ss ../src/cspp/treemap.ss \
../src/cspp/vector.ss ../src/cspp/string.ss >/dev/null 2>&1")))

(mat net-public-api-documentation-errors
     ;; Error cases: detached documentation, an overlong line, and vague return wording are rejected.
     (let* ([stem (format "/tmp/cspp-net-docs-~a" (get-process-id))]
            [fixture (string-append stem ".ss")]
            [output (string-append stem ".out")]
            [source
             (string-append
              "(library (net-docs-fixture)\n"
              "  (export detached long-doc vague)\n"
              "  (import (chezscheme))\n"
              "  #|proc:detached\nDetached documentation.\n|#\n"
              "  (define ignored 1)\n"
              "  (define detached (lambda () #t))\n"
              "  #|proc:long-doc\n"
              "This documentation line is deliberately longer than one hundred characters so the checker rejects the fixture reliably.\n"
              "|#\n  (define long-doc (lambda () #t))\n"
              "  #|proc:vague\nThis procedure returns the result.\n|#\n"
              "  (define vague (lambda () #t)))\n")])
       (dynamic-wind
         (lambda ()
           (call-with-output-file fixture
             (lambda (port) (put-string port source))
             'replace))
         (lambda ()
           (let* ([status
                   (system
                    (format
                     "../cspp --script=../tools/check-public-api-docs.ss ~a >~a 2>&1"
                     fixture output))]
                  [diagnostics (read-text-file output)])
             (and (not (= status 0))
                  (string-contains? diagnostics "missing proc documentation for detached")
                  (string-contains? diagnostics "documentation line exceeds 100 characters")
                  (string-contains? diagnostics "vague return phrase: returns the result"))))
         (lambda ()
           (when (file-exists? fixture) (delete-file fixture))
           (when (file-exists? output) (delete-file output))))))
