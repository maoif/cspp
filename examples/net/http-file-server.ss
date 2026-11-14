(import (cspp)
        (cspp net http)
        (cspp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'http-file-server
 "Serve regular files from a local directory over HTTP/1."
 '((host . "Listener address (default: 127.0.0.1).")
   (port . "Listener port, 1 through 65535 (default: 8080).")
   (dir . "Required: local directory to serve; symlink paths are rejected."))
 (lambda (ref)
   (run-http-file-server! 'http1 ref))
 (command-line-arguments))
