#!chezscheme
(library (cspp rich box)
  (export rich-box-style?
          rich-box-chars)
  (import (cspp chez)
          (cspp utils))

  (define $rich-box-styles
    `((ascii . #("+" "+" "+" "+" "-" "|" "+" "+" "+" "+" "+"))
      (square . #("┌" "┐" "└" "┘" "─" "│" "├" "┤" "┬" "┴" "┼"))
      (rounded . #("╭" "╮" "╰" "╯" "─" "│" "├" "┤" "┬" "┴" "┼"))
      (heavy . #("┏" "┓" "┗" "┛" "━" "┃" "┣" "┫" "┳" "┻" "╋"))
      (double . #("╔" "╗" "╚" "╝" "═" "║" "╠" "╣" "╦" "╩" "╬"))))

  #|proc:rich-box-style?
  The `rich-box-style?` procedure returns `#t` when `style` names a supported
  rich box style, and `#f` otherwise.
  |#
  (define rich-box-style?
    (lambda (style)
      (and (assq style $rich-box-styles) #t)))

  #|proc:rich-box-chars
  The `rich-box-chars` procedure returns a fresh vector of copied strings for `style`.
  The vector order is top-left, top-right, bottom-left, bottom-right,
  horizontal, vertical, left-tee, right-tee, top-tee, bottom-tee, and cross.
  |#
  (define rich-box-chars
    (lambda (style)
      (pcheck ([rich-box-style? style])
              (vector-map string-copy (cdr (assq style $rich-box-styles)))))))
