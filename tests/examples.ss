(import (cspp))

(define example-entry-point*
  '("examples/parser/xml.ss"
    "examples/parser/json.ss"
    "examples/parser/toml.ss"
    "examples/parser/scheme.ss"
    "examples/parser/elf.ss"
    "examples/navigator/traversal.ss"
    "examples/transducer/pipeline.ss"
    "examples/iter/operations.ss"
    "examples/glob/files.ss"
    "examples/containers/treeset.ss"
    "examples/containers/treemap.ss"
    "examples/containers/heap.ss"
    "examples/containers/dlist.ss"
    "examples/containers/array.ss"
    "examples/net/http-file-server.ss"
    "examples/net/http2-file-server.ss"
    "examples/net/https-file-server.ss"
    "examples/net/wget.ss"
    "examples/net/ssh-open-repl.ss"
    "examples/net/ssh-run-cmd.ss"
    "examples/net/ssh-port-forward.ss"
    "examples/net/scp.ss"
    "examples/crypto/encrypt-file.ss"
    "examples/crypto/hash-file.ss"
    "benchmarks/treeset.ss"
    "benchmarks/treemap.ss"
    "benchmarks/vectors.ss"
    "benchmarks/arrays.ss"))

(define static-entry-point*
  '("examples/net/ssh-open-repl.ss"
    "examples/net/ssh-run-cmd.ss"
    "examples/net/http-download.ss"
    "examples/net/echo-client-grpc.ss"
    "examples/net/echo-server-grpc.ss"))

(define legacy-net-entry-point*
  '("examples/net/ssh-open-repl.ss"
    "examples/net/ssh-run-cmd.ss"
    "examples/net/http-download.ss"
    "examples/net/echo-client-grpc.ss"
    "examples/net/echo-server-grpc.ss"))

(define example-contains?
  (lambda (text fragment)
    (and (string-contains? text fragment) #t)))

(define read-script
  (lambda (path)
    (call-with-input-file path
      (lambda (port)
        (let loop ([datum* '()])
          (let ([datum (read port)])
            (if (eof-object? datum)
                (reverse datum*)
                (loop (cons datum datum*)))))))))

(mat example-entry-points-exist
     (parameterize ([current-directory ".."])
       (andmap (lambda (path) (file-exists? path)) example-entry-point*)))

(mat examples-declare-cli-help
     (parameterize ([current-directory ".."])
       (andmap
        (lambda (path)
          (and (file-exists? path)
               (let ([text (call-with-input-file path get-string-all)])
                 (and (or (example-contains? text "(cspp cli)")
                          (example-contains? text "examples/common.ss")
                          (example-contains? text "benchmarks/common.ss")
                          (example-contains? text "http-file-server-common.ss"))
                      (or (example-contains? text "help")
                          (example-contains? text "examples/common.ss")
                          (example-contains? text "benchmarks/common.ss")
                          (example-contains? text "http-file-server-common.ss"))
                      (pair? (read-script path))))))
        example-entry-point*)))

(mat example-script-reader-pass
     (parameterize ([current-directory ".."])
       (andmap (lambda (path) (pair? (read-script path))) example-entry-point*)))

(mat example-core-and-benchmark-integration
     (= 0 (system "cd .. && python3 tests/integration/examples.py help core benchmarks")))

(mat example-crypto-integration
     (mat-requires (openssl)
       (= 0 (system "cd .. && python3 tests/integration/examples.py crypto"))))

(mat rich-examples-terminal-sizing
     (or (eq? (system-platform) 'windows)
         (= 0 (system "cd .. && python3 tests/integration/rich-examples.py"))))
