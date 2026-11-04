(import (cspp))

(let ([arguments (command-line-arguments)])
  (file-copytree (car arguments) (cadr arguments)))
