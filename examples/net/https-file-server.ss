(import (cspp)
        (cspp net http)
        (cspp net tls)
        (cspp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'https-file-server
 "Serve files from a local directory over TLS."
 '((host . "Listener address (default: 127.0.0.1).")
   (port . "Listener port, 1 through 65535 (default: 8080).")
   (dir . "Required: local directory to serve; symlink paths are rejected.")
   (cert . "Required: path to the server PEM certificate.")
   (key . "Required: path to the matching PEM private key."))
 (lambda (ref)
   (run-http-file-server! 'https ref))
 (command-line-arguments))
