#!chezscheme
(library (cspp rich table)
  (export make-rich-table
          rich-table
          rich-table?
          rich-table-title
          rich-table-title-set!
          rich-table-caption
          rich-table-caption-set!
          rich-table-box
          rich-table-box-set!
          rich-table-show-header?
          rich-table-show-header?-set!
          rich-table-show-lines?
          rich-table-show-lines?-set!
          rich-table-padding
          rich-table-padding-set!
          rich-table-add-column!
          rich-table-add-row!
          rich-table-render)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich box)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich pretty))

  (define-record-type (rich-table-record $make-rich-table-record $rich-table?)
    (fields (mutable title $rich-table-title $rich-table-title-set!)
            (mutable caption $rich-table-caption $rich-table-caption-set!)
            (mutable box $rich-table-box $rich-table-box-set!)
            (mutable show-header? $rich-table-show-header? $rich-table-show-header?-set!)
            (mutable show-lines? $rich-table-show-lines? $rich-table-show-lines?-set!)
            (mutable padding $rich-table-padding $rich-table-padding-set!)
            (mutable columns $rich-table-columns $rich-table-columns-set!)
            (mutable rows $rich-table-rows $rich-table-rows-set!)))

  (define-record-type (rich-table-column-record $make-rich-table-column-record $rich-table-column?)
    (fields (mutable title $rich-table-column-title $rich-table-column-title-set!)
            (mutable justify $rich-table-column-justify $rich-table-column-justify-set!)))

  (define $rich-table-title?
    (lambda (x)
      (or (not x) (string? x))))

  (define $rich-table-justify?
    (lambda (x)
      (memq x '(left center right))))

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

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $rich-segment-line-list?
    (lambda (x)
      (and (list? x) (rich-list-every? $rich-segment-list? x))))

  (define $rendered->segment-lines
    (lambda (who value)
      (cond [(string? value) ($string->segment-lines value)]
            [($rich-segment-line-list? value) value]
            [else (errorf who "renderer returned invalid value: ~a" value)])))

  (define $value->segment-lines
    (lambda (value)
      (cond [(rich-renderer-for value) =>
             (lambda (renderer)
               ($rendered->segment-lines 'rich-table-render (renderer value)))]
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

  (define $string-repeat
    (lambda (text count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            (apply string-append (reverse out))
            (loop (- count 1) (cons text out))))))

  (define $segment-lines-width
    (lambda (lines)
      (let loop ([lines lines] [width 0])
        (if (null? lines)
            width
            (loop (cdr lines) (max width (rich-segments-width (car lines))))))))

  (define $line-at
    (lambda (lines index)
      (if (< index (length lines))
          (list-ref lines index)
          '())))

  (define $line-pad-to
    (lambda (line width justify)
      (let ([current (rich-segments-width line)])
        (if (>= current width)
            line
            (let* ([space (- width current)]
                   [left (case justify
                           [(right) space]
                           [(center) (quotient space 2)]
                           [else 0])]
                   [right (- space left)])
              (append (if (zero? left)
                          '()
                          (list (rich-segment (make-string left #\space))))
                      line
                      (if (zero? right)
                          '()
                          (list (rich-segment (make-string right #\space))))))))))

  (define $row-height
    (lambda (blocks)
      (let loop ([blocks blocks] [height 0])
        (if (null? blocks)
            height
            (loop (cdr blocks) (max height (length (car blocks))))))))

  (define $cell-segments
    (lambda (line width padding justify)
      (append (if (zero? padding)
                  '()
                  (list (rich-segment (make-string padding #\space))))
              ($line-pad-to line width justify)
              (if (zero? padding)
                  '()
                  (list (rich-segment (make-string padding #\space)))))))

  (define $column-width
    (lambda (index header-blocks row-blocks)
      (let ([header-width (if header-blocks
                              ($segment-lines-width (list-ref header-blocks index))
                              0)])
        (let loop ([rows row-blocks] [width header-width])
          (if (null? rows)
              width
              (loop (cdr rows)
                    (max width
                         ($segment-lines-width (list-ref (car rows) index)))))))))

  (define $column-widths
    (lambda (columns header-blocks row-blocks)
      (let loop ([index 0] [columns columns] [out '()])
        (if (null? columns)
            (reverse out)
            (loop (+ index 1)
                  (cdr columns)
                  (cons ($column-width index header-blocks row-blocks) out))))))

  (define $border-line
    (lambda (chars left join right widths padding)
      (let ([h (vector-ref chars 4)]
            [cell-extra (* padding 2)])
        (let loop ([widths widths] [first? #t] [pieces (list left)])
          (if (null? widths)
              (list (rich-segment (apply string-append (reverse (cons right pieces)))))
              (let ([piece ($string-repeat h (+ (car widths) cell-extra))])
                (loop (cdr widths)
                      #f
                      (cons piece (if first?
                                      pieces
                                      (cons join pieces))))))))))

  (define $top-border
    (lambda (chars widths padding)
      ($border-line chars
                    (vector-ref chars 0)
                    (vector-ref chars 8)
                    (vector-ref chars 1)
                    widths
                    padding)))

  (define $middle-border
    (lambda (chars widths padding)
      ($border-line chars
                    (vector-ref chars 6)
                    (vector-ref chars 10)
                    (vector-ref chars 7)
                    widths
                    padding)))

  (define $bottom-border
    (lambda (chars widths padding)
      ($border-line chars
                    (vector-ref chars 2)
                    (vector-ref chars 9)
                    (vector-ref chars 3)
                    widths
                    padding)))

  (define $render-table-row
    (lambda (blocks columns widths padding chars header?)
      (let ([height ($row-height blocks)]
            [v (vector-ref chars 5)])
        (let loop-lines ([index 0] [out '()])
          (if (= index height)
              (reverse out)
              (let loop-cells ([blocks blocks]
                               [columns columns]
                               [widths widths]
                               [pieces (list (rich-segment v))])
                (if (null? blocks)
                    (loop-lines (+ index 1) (cons (reverse pieces) out))
                    (let* ([justify (if header?
                                        'left
                                        ($rich-table-column-justify (car columns)))]
                           [line ($line-at (car blocks) index)]
                           [cell ($cell-segments line (car widths) padding justify)])
                      (loop-cells (cdr blocks)
                                  (cdr columns)
                                  (cdr widths)
                                  (cons (rich-segment v)
                                        (rich-reverse-append cell pieces)))))))))))

  (define $render-data-rows
    (lambda (row-blocks columns widths padding chars show-lines?)
      (let loop ([rows row-blocks] [out '()])
        (cond [(null? rows) (reverse out)]
              [(and show-lines? (not (null? (cdr rows))))
               (loop (cdr rows)
                     (cons ($middle-border chars widths padding)
                           (rich-reverse-append
                            ($render-table-row (car rows) columns widths padding chars #f)
                            out)))]
              [else
               (loop (cdr rows)
                     (rich-reverse-append
                      ($render-table-row (car rows) columns widths padding chars #f)
                      out))]))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Table
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-table?
  The `rich-table?` procedure returns `#t` when its argument is a rich table
  object, and `#f` otherwise.
  |#
  (define rich-table?
    (lambda (x)
      ($rich-table? x)))

  #|proc:make-rich-table
  The `make-rich-table` procedure constructs a mutable table with ASCII box
  drawing, one space of cell padding, visible headers, and row lines.
  |#
  (define make-rich-table
    (case-lambda
      [()
       ($make-rich-table-record #f #f 'ascii #t #t 1 '() '())]))

  #|proc:rich-table-title
  The `rich-table-title` procedure returns the optional title for `table`.
  |#
  (define rich-table-title
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-title table))))

  #|proc:rich-table-title-set!
  The `rich-table-title-set!` procedure sets the optional title for `table`.
  |#
  (define rich-table-title-set!
    (lambda (table title)
      (pcheck ([rich-table? table] [$rich-table-title? title])
              ($rich-table-title-set! table title))))

  #|proc:rich-table-caption
  The `rich-table-caption` procedure returns the optional caption for `table`.
  |#
  (define rich-table-caption
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-caption table))))

  #|proc:rich-table-caption-set!
  The `rich-table-caption-set!` procedure sets the optional caption for `table`.
  |#
  (define rich-table-caption-set!
    (lambda (table caption)
      (pcheck ([rich-table? table] [$rich-table-title? caption])
              ($rich-table-caption-set! table caption))))

  #|proc:rich-table-box
  The `rich-table-box` procedure returns the box style for `table`.
  |#
  (define rich-table-box
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-box table))))

  #|proc:rich-table-box-set!
  The `rich-table-box-set!` procedure sets the box style for `table`.
  |#
  (define rich-table-box-set!
    (lambda (table box)
      (pcheck ([rich-table? table] [rich-box-style? box])
              ($rich-table-box-set! table box))))

  #|proc:rich-table-show-header?
  The `rich-table-show-header?` procedure returns whether column headers are
  rendered for `table`.
  |#
  (define rich-table-show-header?
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-show-header? table))))

  #|proc:rich-table-show-header?-set!
  The `rich-table-show-header?-set!` procedure sets whether column headers are
  rendered for `table`.
  |#
  (define rich-table-show-header?-set!
    (lambda (table show-header?)
      (pcheck ([rich-table? table] [boolean? show-header?])
              ($rich-table-show-header?-set! table show-header?))))

  #|proc:rich-table-show-lines?
  The `rich-table-show-lines?` procedure returns whether horizontal separators
  are rendered inside `table`.
  |#
  (define rich-table-show-lines?
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-show-lines? table))))

  #|proc:rich-table-show-lines?-set!
  The `rich-table-show-lines?-set!` procedure sets whether horizontal
  separators are rendered inside `table`.
  |#
  (define rich-table-show-lines?-set!
    (lambda (table show-lines?)
      (pcheck ([rich-table? table] [boolean? show-lines?])
              ($rich-table-show-lines?-set! table show-lines?))))

  #|proc:rich-table-padding
  The `rich-table-padding` procedure returns the number of spaces around cell
  content in `table`.
  |#
  (define rich-table-padding
    (lambda (table)
      (pcheck ([rich-table? table])
              ($rich-table-padding table))))

  #|proc:rich-table-padding-set!
  The `rich-table-padding-set!` procedure sets the non-negative cell padding
  for `table`.
  |#
  (define rich-table-padding-set!
    (lambda (table padding)
      (pcheck ([rich-table? table] [rich-nonnegative-integer? padding])
              ($rich-table-padding-set! table padding))))

  #|proc:rich-table-add-column!
  The `rich-table-add-column!` procedure appends a column with `title` to
  `table` and returns `table`. The optional `justify` argument controls data
  alignment and may be `left`, `center`, or `right`.
  |#
  (define rich-table-add-column!
    (case-lambda
      [(table title)
       (rich-table-add-column! table title 'left)]
      [(table title justify)
       (pcheck ([rich-table? table]
                [string? title]
                [$rich-table-justify? justify])
               (unless (null? ($rich-table-rows table))
                 (errorf 'rich-table-add-column!
                         "cannot add a column after rows have been added"))
               ($rich-table-columns-set!
                table
                ($append-one ($rich-table-columns table)
                             ($make-rich-table-column-record title justify)))
               table)]))

  #|proc:rich-table-add-row!
  The `rich-table-add-row!` procedure appends a row of cell values to `table`
  and returns `table`. The row length must match the current column count.
  |#
  (define rich-table-add-row!
    (lambda (table . values)
      (pcheck ([rich-table? table])
              (unless (= (length values) (length ($rich-table-columns table)))
                (errorf 'rich-table-add-row!
                        "row has ~a cells, but table has ~a columns"
                        (length values)
                        (length ($rich-table-columns table))))
              ($rich-table-rows-set! table ($append-one ($rich-table-rows table) values))
              table)))

  #|proc:rich-table-render
  The `rich-table-render` procedure renders `table` as segment lines.
  |#
  (define rich-table-render
    (lambda (table)
      (pcheck ([rich-table? table])
              (let* ([columns ($rich-table-columns table)]
                     [rows ($rich-table-rows table)]
                     [chars (rich-box-chars (rich-table-box table))]
                     [padding (rich-table-padding table)]
                     [cell-width (if (null? columns) (rich-render-width)
                                     (max 1 (quotient
                                             (- (rich-render-width) (+ (length columns) 1)
                                                (* 2 padding (length columns)))
                                             (length columns))))]
                     [render-cell (lambda (value)
                                    (parameterize ([rich-render-width cell-width])
                                      ($value->segment-lines value)))]
                     [header-blocks (and (rich-table-show-header? table)
                                         (map (lambda (column)
                                                (render-cell
                                                 ($rich-table-column-title column)))
                                              columns))]
                     [row-blocks (map (lambda (row)
                                        (map render-cell row))
                                      rows)]
                     [widths ($column-widths columns header-blocks row-blocks)]
                     [title (rich-table-title table)]
                     [caption (rich-table-caption table)]
                     [body (append
                            (list ($top-border chars widths padding))
                            (if header-blocks
                                (append ($render-table-row header-blocks
                                                           columns
                                                           widths
                                                           padding
                                                           chars
                                                           #t)
                                        (if (rich-table-show-lines? table)
                                            (list ($middle-border chars widths padding))
                                            '()))
                                '())
                            ($render-data-rows row-blocks
                                               columns
                                               widths
                                               padding
                                               chars
                                               (rich-table-show-lines? table))
                            (list ($bottom-border chars widths padding)))])
                (append (if title
                            (list (list (rich-segment title)))
                            '())
                        body
                        (if caption
                            (list (list (rich-segment caption)))
                            '()))))))

  #|macro:rich-table
  The `rich-table` macro constructs and returns a table. The `:columns` and
  `:rows` fields accept ordinary list expressions or parenthesized literal
  lists.
  |#
  (define-syntax rich-table
    (lambda (stx)
      (define expression-head?
        (lambda (datum)
          (symbol? datum)))
      (define setter-action
        (lambda (name setter value)
          (with-syntax ([table-name name]
                        [set-table-field! setter]
                        [field-value value])
            #'(set-table-field! table-name field-value))))
      (define columns-expression-action
        (lambda (name value)
          (with-syntax ([table-name name]
                        [columns-value value])
            #'(for-each (lambda (column)
                          (rich-table-add-column! table-name column))
                        columns-value))))
      (define rows-expression-action
        (lambda (name value)
          (with-syntax ([table-name name]
                        [rows-value value])
            #'(for-each (lambda (row)
                          (apply rich-table-add-row! table-name row))
                        rows-value))))
      (define column-actions
        (lambda (name value)
          (syntax-case value ()
            [()
             '()]
            [(head column ...)
             (let ([head-datum (syntax->datum #'head)])
               (if (expression-head? head-datum)
                   (list (columns-expression-action name value))
                   (with-syntax ([table-name name])
                     (syntax->list
                      #'((rich-table-add-column! table-name head)
                         (rich-table-add-column! table-name column) ...)))))]
            [_
             (list (columns-expression-action name value))])))
      (define row-actions
        (lambda (name value)
          (syntax-case value ()
            [()
             '()]
            [(head row ...)
             (let ([head-datum (syntax->datum #'head)])
               (cond [(expression-head? head-datum)
                      (list (rows-expression-action name value))]
                     [else
                      (syntax-case value ()
                        [((cell ...) ...)
                         (with-syntax ([table-name name])
                           (syntax->list
                            #'((rich-table-add-row! table-name cell ...) ...)))]
                        [_
                         (list (rows-expression-action name #'(list head row ...)))])]))]
            [_
             (list (rows-expression-action name value))])))
      (define field-actions
        (lambda (name field value)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:title) (list (setter-action name #'rich-table-title-set! value))]
              [(:caption) (list (setter-action name #'rich-table-caption-set! value))]
              [(:box) (list (setter-action name #'rich-table-box-set! value))]
              [(:show-header?) (list (setter-action name #'rich-table-show-header?-set! value))]
              [(:show-lines?) (list (setter-action name #'rich-table-show-lines?-set! value))]
              [(:padding) (list (setter-action name #'rich-table-padding-set! value))]
              [(:columns) (column-actions name value)]
              [(:rows) (row-actions name value)]
              [else (syntax-error field "invalid rich-table field")]))))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [action* '()])
            (cond [(null? clause*) (reverse action*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-table form")]
                  [else
                   (let ([actions (field-actions name (car clause*) (cadr clause*))])
                     (loop (cddr clause*) (rich-reverse-append actions action*)))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-table)))])
           (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-table)])
                 action ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-table form")])))

  (rich-register-renderer! rich-table? rich-table-render))
