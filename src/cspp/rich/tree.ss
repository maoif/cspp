#!chezscheme
(library (cspp rich tree)
  (export make-rich-tree
          rich-tree
          rich-tree?
          rich-tree-label
          rich-tree-label-set!
          rich-tree-children
          rich-tree-add!
          rich-tree-render
          rich-tree-render/ascii)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich private lines)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich pretty))

  (define-record-type (rich-tree-record $make-rich-tree-record $rich-tree?)
    (fields (mutable label $rich-tree-label $rich-tree-label-set!)
            (mutable children $rich-tree-children $rich-tree-children-set!)))

  (define $rich-tree-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-tree? x))))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $rich-segment-line-list?
    (lambda (x)
      (and (list? x) (rich-list-every? $rich-segment-list? x))))

  (define $append-one
    (lambda (ls value)
      (let loop ([ls ls] [out '()])
        (if (null? ls)
            (rich-reverse-append out (list value))
            (loop (cdr ls) (cons (car ls) out))))))

  (define $string-split-lines
    (lambda (text)
      (let ([len (string-length text)])
        (let loop ([start 0] [i 0] [out '()])
          (cond [(= i len)
                 (reverse (cons (substring text start i) out))]
                [(char=? (string-ref text i) #\newline)
                 (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
                [else
                 (loop start (+ i 1) out)])))))

  (define $string->segment-lines
    (lambda (text)
      (map (lambda (line) (list (rich-segment line)))
           ($string-split-lines text))))

  (define $segment-copy
    (lambda (segment text)
      (rich-segment text
                    (rich-segment-style segment)
                    (rich-segment-control? segment))))

  (define $normalize-segment-line
    (lambda (line)
      (let loop-segments ([segments line] [line '()] [lines '()])
        (if (null? segments)
            (reverse (cons (reverse line) lines))
            (let loop-pieces ([pieces ($string-split-lines
                                       (rich-segment-text (car segments)))]
                              [line line]
                              [lines lines])
              (cond [(null? pieces)
                     (loop-segments (cdr segments) line lines)]
                    [(null? (cdr pieces))
                     (loop-segments (cdr segments)
                                    (cons ($segment-copy (car segments)
                                                         (car pieces))
                                          line)
                                    lines)]
                    [else
                     (loop-pieces (cdr pieces)
                                  '()
                                  (cons (reverse
                                         (cons ($segment-copy (car segments)
                                                              (car pieces))
                                               line))
                                        lines))]))))))

  (define $normalize-segment-lines
    (lambda (lines)
      (apply append (map $normalize-segment-line lines))))

  (define $rendered->segment-lines
    (lambda (who value)
      (cond [(string? value) ($string->segment-lines value)]
            [($rich-segment-line-list? value) ($normalize-segment-lines value)]
            [else (errorf who "renderer returned invalid value: ~a" value)])))

  (define $value->segment-lines
    (lambda (value)
      (cond [(rich-renderer-for value) =>
             (lambda (renderer)
               ($rendered->segment-lines 'rich-tree-render (renderer value)))]
            [(string? value)
             ($string->segment-lines value)]
            [(or (char? value) (number? value) (symbol? value))
             ($string->segment-lines
              (rich-string-output
               (lambda (port)
                 (if (char? value)
                     (write-char value port)
                     (display value port)))))]
            [else
             (rich-pretty-render value)])))

  (define $unicode-guides
    '#("├── " "└── " "│   " "    "))

  (define $ascii-guides
    '#("|-- " "`-- " "|   " "    "))

  (define $guide-branch
    (lambda (guides last?)
      (vector-ref guides (if last? 1 0))))

  (define $guide-child-prefix
    (lambda (guides last?)
      (vector-ref guides (if last? 3 2))))

  (define $prefix-label-lines
    (lambda (label first-prefix rest-prefix)
      (let* ([available (rich-render-width)]
             [prefix-width (min (rich-segment-width (rich-segment first-prefix))
                                (- available 1))]
             [width (- available prefix-width)]
             [measure-width (max width (min 2 available))]
             [raw (parameterize ([rich-render-width measure-width])
                    ($value->segment-lines label))]
             [prefix-width (min prefix-width (max 0 (- available (rich-lines-min-width raw))))]
             [child-width (- available prefix-width)]
             [raw (if (= child-width measure-width) raw
                      (parameterize ([rich-render-width child-width])
                        ($value->segment-lines label)))]
             [prefix (lambda (text)
                       (rich-segments->plain
                        (rich-segment-crop (list (rich-segment text)) prefix-width)))]
             [lines (rich-lines-wrap raw child-width)])
        (if (null? lines)
            (list (list (rich-segment (prefix first-prefix))))
            (let loop ([lines lines] [first? #t] [out '()])
              (if (null? lines)
                  (reverse out)
                  (loop (cdr lines) #f
                        (cons (cons (rich-segment (prefix (if first? first-prefix rest-prefix)))
                                    (car lines)) out))))))))

  (define $render-node-children
    (lambda (children prefix guides)
      (let loop ([children children] [out '()])
        (if (null? children)
            (reverse out)
            (let* ([child (car children)]
                   [last? (null? (cdr children))]
                   [child-prefix (string-append
                                  prefix
                                  ($guide-child-prefix guides last?))]
                   [label-lines ($prefix-label-lines
                                 (rich-tree-label child)
                                 (string-append prefix
                                                ($guide-branch guides last?))
                                 child-prefix)]
                   [child-lines ($render-node-children
                                 (rich-tree-children child)
                                 child-prefix
                                 guides)])
              (loop (cdr children)
                    (rich-reverse-append child-lines
                                         (rich-reverse-append label-lines out))))))))

  (define $rich-tree-render/guides
    (lambda (tree guides)
      (parameterize ([rich-layout-render-cache
                      (or (rich-layout-render-cache) (make-eq-hashtable))])
        (append ($prefix-label-lines (rich-tree-label tree) "" "")
                ($render-node-children (rich-tree-children tree) "" guides)))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Tree
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-tree?
  The `rich-tree?` procedure returns `#t` when its argument is a rich tree
  object, and `#f` otherwise.
  |#
  (define rich-tree?
    (lambda (x)
      ($rich-tree? x)))

  #|proc:make-rich-tree
  The `make-rich-tree` procedure constructs a mutable tree node with `label`
  and an initially empty child list.
  |#
  (define make-rich-tree
    (case-lambda
      [(label)
       ($make-rich-tree-record label '())]
      [(label children)
       (pcheck ([$rich-tree-list? children])
               ($make-rich-tree-record label children))]))

  #|proc:rich-tree-label
  The `rich-tree-label` procedure returns the label for `tree`.
  |#
  (define rich-tree-label
    (lambda (tree)
      (pcheck ([rich-tree? tree])
              ($rich-tree-label tree))))

  #|proc:rich-tree-label-set!
  The `rich-tree-label-set!` procedure sets the label for `tree`.
  |#
  (define rich-tree-label-set!
    (lambda (tree label)
      (pcheck ([rich-tree? tree])
              ($rich-tree-label-set! tree label))))

  #|proc:rich-tree-children
  The `rich-tree-children` procedure returns the child nodes for `tree`.
  |#
  (define rich-tree-children
    (lambda (tree)
      (pcheck ([rich-tree? tree])
              ($rich-tree-children tree))))

  #|proc:rich-tree-add!
  The `rich-tree-add!` procedure appends a child node with `label` to `tree` and
  returns the new child node.
  |#
  (define rich-tree-add!
    (lambda (tree label)
      (pcheck ([rich-tree? tree])
              (let ([child (make-rich-tree label)])
                ($rich-tree-children-set!
                 tree
                 ($append-one ($rich-tree-children tree) child))
                child))))

  #|proc:rich-tree-render
  The `rich-tree-render` procedure returns segment lines for `tree` with Unicode guides.
  Labels wrap after guide indentation; deep guides shrink to leave room for complete glyphs.
  |#
  (define rich-tree-render
    (lambda (tree)
      (pcheck ([rich-tree? tree])
              ($rich-tree-render/guides tree $unicode-guides))))

  #|proc:rich-tree-render/ascii
  The `rich-tree-render/ascii` procedure returns segment lines for `tree` with ASCII guides.
  Labels wrap after guide indentation; deep guides shrink to leave room for complete glyphs.
  |#
  (define rich-tree-render/ascii
    (lambda (tree)
      (pcheck ([rich-tree? tree])
              ($rich-tree-render/guides tree $ascii-guides))))

  #|macro:rich-tree
  The `rich-tree` macro constructs and returns a tree node. The `:label` field
  is required.
  |#
  (define-syntax rich-tree
    (lambda (stx)
      (define label-value
        (lambda (clause*)
          (let loop ([clause* clause*])
            (cond [(null? clause*) #f]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-tree form")]
                  [(eq? (syntax->datum (car clause*)) ':label)
                   (cadr clause*)]
                  [else (loop (cddr clause*))]))))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [action* '()])
            (cond [(null? clause*) (reverse action*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-tree form")]
                  [else
                   (let ([field (car clause*)]
                         [value (cadr clause*)])
                     (case (syntax->datum field)
                       [(:label)
                        (loop (cddr clause*) action*)]
                       [else
                        (syntax-error field "invalid rich-tree field")]))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (let ([label (label-value #'(clause ...))])
           (unless label
             (syntax-error stx "rich-tree requires :label"))
           (with-syntax ([tmp (car (generate-temporaries #'(rich-tree)))]
                         [label-value label])
             (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
               #'(let ([tmp (make-rich-tree label-value)])
                   action ...
                   tmp))))]
        [_ (syntax-error stx "invalid rich-tree form")])))

  (rich-register-container-renderer! rich-tree? rich-tree-render))
