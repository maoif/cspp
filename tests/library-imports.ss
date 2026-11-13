(import (cspp))

(mat default-library-environment

     (let ([environment (environment '(cspp))])
       (= 42 (eval '(let ([values (for/list ([number :list '(20 21)]) (add1 number))])
                      (match values [(,first ,second) (+ first second -1)]))
                   environment)))

     (let ([environment (environment '(cspp))])
       (eval '(begin (sleep 0) (sleep-milliseconds 0) (sleep-nanoseconds 0) #t)
             environment))

     ;; Error case: optional subsystem names must be unbound until explicitly imported.
     (let ([environment (environment '(cspp))])
       (for-all (lambda (name) (not (top-level-bound? name environment)))
                '(displayln make-queue path-join glob nav-select into
                  process-capture make-uuid digest parse-csv parse-toml
                  protobuf-encode-varint make-logger make-command rich-text
                  test-run benchmark net-operation?))))

(mat explicit-library-environment

     (let ([environment
             (environment '(cspp) '(cspp format csv) '(cspp format protobuf))])
       (eval '(and (equal? #vu8(150 1) (protobuf-encode-varint 150))
                   (equal? '#("a" "b")
                           (csv-record-fields
                             (vector-ref (csv-document-records (parse-csv "a,b")) 0))))
             environment))

     (let ([environment (environment '(cspp) '(cspp io))])
       (string=? "explicit\n"
                 (eval '(with-output-to-string (lambda () (displayln "explicit")))
                       environment))))

(mat launcher-default-environment

     ;; Error case: launcher dependencies must not become implicit application imports.
     (= 0 (system
             "../cspp -q -c='(exit (if (or (top-level-bound? (quote rich-text)) (top-level-bound? (quote make-command)) (top-level-bound? (quote displayln)) (top-level-bound? (quote process-capture))) 1 0))'")))
