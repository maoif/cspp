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
          (only (cspp list) make-list-builder)
          (cspp rich private common)
          (cspp rich private lines)
          (cspp rich box)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich private render))

  (define-record-type (rich-table-record $make-rich-table-record $rich-table?)
    (fields (mutable title $rich-table-title $rich-table-title-set!)
            (mutable caption $rich-table-caption $rich-table-caption-set!)
            (mutable box $rich-table-box $rich-table-box-set!)
            (mutable show-header? $rich-table-show-header? $rich-table-show-header?-set!)
            (mutable show-lines? $rich-table-show-lines? $rich-table-show-lines?-set!)
            (mutable padding $rich-table-padding $rich-table-padding-set!)
            (immutable columns $rich-table-column-builder)
            (immutable rows $rich-table-row-builder)))

  (define $rich-table-rows
    (lambda (table) (($rich-table-row-builder table))))

  (define $rich-table-columns
    (lambda (table) (($rich-table-column-builder table))))

  (define-record-type (rich-table-column-record $make-rich-table-column-record $rich-table-column?)
    (fields (mutable title $rich-table-column-title $rich-table-column-title-set!)
            (mutable justify $rich-table-column-justify $rich-table-column-justify-set!)))

  (define $rich-table-title?
    (lambda (x)
      (or (not x) (string? x))))

  (define $rich-table-justify?
    (lambda (x)
      (memq x '(left center right))))

  (define $string->segment-lines rich-string->lines)

  (define $value->segment-lines
    (lambda (value) (rich-value->lines 'rich-table-render value)))

  (define $string-repeat
    (lambda (text count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            (apply string-append (reverse out))
            (loop (- count 1) (cons text out))))))

  (define $segment-lines-width rich-lines-width)

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
            (loop (cdr blocks) (max height (vector-length (car blocks))))))))

  (define $cell-segments
    (lambda (line width padding justify)
      (append (if (zero? padding)
                  '()
                  (list (rich-segment (make-string padding #\space))))
              ($line-pad-to line width justify)
              (if (zero? padding)
                  '()
                  (list (rich-segment (make-string padding #\space)))))))

  ;; Traverse cells in row order; repeated list-ref made wide tables quadratic.
  (define $measure-columns
    (lambda (count headers rows measure initial)
      (let ([widths (make-vector count initial)])
        (define measure-row!
          (lambda (row)
            (let loop ([blocks row] [index 0])
              (unless (null? blocks)
                (vector-set! widths index (max (vector-ref widths index) (measure (car blocks))))
                (loop (cdr blocks) (fx1+ index))))))
        (when headers (measure-row! headers))
        (for-each measure-row! rows)
        (vector->list widths))))

  (define $column-widths
    (lambda (columns header-blocks row-blocks)
      ($measure-columns (length columns) header-blocks row-blocks $segment-lines-width 0)))

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
      ;; Index each cell's lines once, avoiding repeated list traversal for tall rows.
      (let* ([blocks (map list->vector blocks)]
             [height ($row-height blocks)]
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
                           [line (if (< index (vector-length (car blocks)))
                                     (vector-ref (car blocks) index) '())]
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
  drawing, one space of cell padding, visible headers, and row lines. Its rendered width
  fits the content and wraps at the current terminal or parent content width.
  |#
  (define make-rich-table
    (case-lambda
      [()
       ($make-rich-table-record #f #f 'ascii #t #t 1 (make-list-builder) (make-list-builder))]))

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
               (($rich-table-column-builder table)
                ($make-rich-table-column-record title justify))
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
              (($rich-table-row-builder table) values)
              table)))

  ;; Cap the widest columns first so a short ID or status does not waste a long column's space.
  (define $fit-column-widths
    (lambda (natural minimum budget)
      (if (<= (apply + natural) budget)
          natural
          (let search ([low 0] [high (apply max natural)])
            (if (< low high)
                (let* ([mid (quotient (+ low high 1) 2)]
                       [used (apply + (map (lambda (width floor)
                                             (max floor (min width mid))) natural minimum))])
                  (if (<= used budget) (search mid high) (search low (- mid 1))))
                (let* ([widths (map (lambda (width floor) (max floor (min width low)))
                                    natural minimum)]
                       [left (- budget (apply + widths))])
                  (map (lambda (width target)
                         (if (and (> width target) (positive? left))
                             (begin (set! left (- left 1)) (+ target 1)) target))
                       natural widths)))))))

  (define $column-minimums
    (lambda (count headers rows)
      ($measure-columns count headers rows rich-lines-min-width 1)))

  ;; When borders alone cannot fit, keep fields readable by stacking them without a box.
  (define $stack-table-blocks
    (lambda (headers rows width)
      (if (null? rows)
          (if headers (apply append (map (lambda (block) (rich-lines-wrap block width)) headers))
              '())
          (apply append
                 (map (lambda (row)
                        (apply append
                               (map (lambda (header block)
                                      (append (if header (rich-lines-wrap header width) '())
                                              (rich-lines-wrap block width)))
                                    (or headers (make-list (length row) #f)) row)))
                      rows)))))

  (define $table-render
    (lambda (table)
      (let* ([columns ($rich-table-columns table)] [count (length columns)]
             [rows ($rich-table-rows table)] [available (rich-render-width)]
             [chars (rich-box-chars (rich-table-box table))]
             ;; Measure each cell with room for its siblings' minimum content and borders.
             [measure-width (max 1 (min 2 available) (- available (* 2 count)))]
             [render-cell (lambda (value width)
                            (parameterize ([rich-render-width width])
                              ($value->segment-lines value)))]
             [header-values (and (rich-table-show-header? table)
                                 (map $rich-table-column-title columns))]
             [headers (and header-values
                           (map (lambda (value) (render-cell value measure-width)) header-values))]
             [blocks (map (lambda (row)
                            (map (lambda (value) (render-cell value measure-width)) row)) rows)]
             [minimum ($column-minimums count headers blocks)]
             [padding (if (zero? count) 0
                          (min (rich-table-padding table)
                               (max 0 (quotient (- available (+ count 1) (apply + minimum))
                                                (* 2 count)))))]
             [budget (max 0 (- available (+ count 1) (* 2 padding count)))]
             [natural (map max minimum ($column-widths columns headers blocks))]
             [stack? (< available (max 2 (+ count 1 (apply + minimum))))]
             [widths (if (or stack? (zero? count)) '()
                         ($fit-column-widths natural minimum budget))]
             [render-row (lambda (values widths)
                           (map (lambda (value width)
                                  (rich-lines-wrap (render-cell value width) width)) values widths))]
             [body (if stack?
                       ($stack-table-blocks
                        (and header-values
                             (map (lambda (value) (render-cell value available)) header-values))
                        (map (lambda (row)
                               (map (lambda (value) (render-cell value available)) row)) rows)
                        available)
                       (let* ([headers (and header-values (render-row header-values widths))]
                              [wrapped (map (lambda (row) (render-row row widths)) rows)]
                              [widths (map max minimum ($column-widths columns headers wrapped))])
                         (append
                          (list ($top-border chars widths padding))
                          (if headers
                              (append ($render-table-row headers columns widths padding chars #t)
                                      (if (and (rich-table-show-lines? table) (pair? wrapped))
                                          (list ($middle-border chars widths padding)) '()))
                              '())
                          ($render-data-rows wrapped columns widths padding chars
                                             (rich-table-show-lines? table))
                          (list ($bottom-border chars widths padding)))))])
        (append (if (rich-table-title table)
                    (rich-lines-wrap ($string->segment-lines (rich-table-title table)) available)
                    '())
                body
                (if (rich-table-caption table)
                    (rich-lines-wrap ($string->segment-lines (rich-table-caption table)) available)
                    '())))))

  #|proc:rich-table-render
  The `rich-table-render` procedure returns segment lines for `table`. Columns fit their
  content when space permits; long cells and headers wrap within the available parent or
  terminal width, preserving styles. Padding shrinks in narrow contexts; when complete
  glyphs and borders cannot fit, fields stack without borders. Titles and captions wrap.
  Built-in containers reflow to assigned column widths; custom cell renderers run once.
  Tabs expand at eight-cell content stops; CRLF and CR become line breaks.
  |#
  (define rich-table-render
    (lambda (table)
      (pcheck ([rich-table? table])
        (parameterize ([rich-layout-render-cache
                        (or (rich-layout-render-cache) (make-eq-hashtable))])
          ($table-render table)))))

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

  (rich-register-container-renderer! rich-table? rich-table-render))
