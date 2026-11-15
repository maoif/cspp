(import (cspp)
        (cspp file))

(let ([arguments (command-line-arguments)])
  (file-copytree (car arguments) (cadr arguments)))
