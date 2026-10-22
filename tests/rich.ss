#!chezscheme
(import (cspp)
        (cspp rich private common))

(mat rich-import

     (procedure? rich-style)
     (procedure? reset-style)
     (procedure? rich-print)
     (procedure? rich-println)
     (procedure? make-rich-console)

     )

(mat rich-print-smoke

     (equal? "123 abc"
             (with-output-to-string
               (lambda ()
                 (rich-print 123 " " "abc"))))

     (equal? "abc\n"
             (with-output-to-string
               (lambda ()
                 (rich-println "abc"))))

     )

(mat rich-style

     (rich-style? (rich-style 'bold 'red))
     (rich-reset? (reset-style))

     (equal? 'red (rich-style-fg (rich-style 'red)))
     (equal? 'blue (rich-style-fg (rich-style 'red 'blue)))
     (equal? 'on-red (rich-style-bg (rich-style 'on-red)))
     (equal? #xffaa00 (rich-style-fg (rich-style #xffaa00)))
     (equal? #x202020 (rich-style-bg (rich-style '(bg #x202020))))

     (equal? "\033[1m\033[31m"
             (rich-style->ansi #f (rich-style 'bold 'red)))

     ;; Later foreground colors override earlier foreground colors.
     (equal? "\033[34m"
             (rich-style->ansi #f (rich-style 'red 'blue)))

     ;; Later attributes override earlier conflicting attributes.
     (equal? ""
             (rich-style->ansi #f (rich-style 'underline 'no-underline)))

     (equal? "\033[38;2;255;170;0m"
             (rich-style->ansi #f (rich-style #xffaa00)))

     (equal? "\033[48;2;32;32;32m"
             (rich-style->ansi #f (rich-style '(bg #x202020))))

     (equal? "x"
             (with-output-to-string
               (lambda ()
                 (rich-print (rich-style 'red) "x" (reset-style)))))

     )

(mat rich-style-errors

     ;; Unknown style symbols are rejected.
     (error? (rich-style 'not-a-style))

     ;; Truecolor integers must be in RGB range.
     (error? (rich-style #x1000000))

     ;; Background color forms must have exactly one color value.
     (error? (rich-style '(bg)))

     )

(mat rich-theme

     (rich-color-system? 'auto)

     (let ([theme (rich-theme :error (rich-style 'red) :ok (rich-style 'green))])
       (and (rich-theme? theme)
            (equal? "\033[31m" (rich-style->ansi #f (rich-theme-ref theme 'error)))
            (equal? "\033[32m" (rich-style->ansi #f (rich-theme-ref theme 'ok)))))

     (rich-theme? (rich-theme))

     (let ([theme (rich-theme :error (rich-style 'red) :ok (rich-style 'green))])
       (and (equal? "\033[31m" (rich-style->ansi #f (rich-theme-ref theme 'error)))
            (equal? "\033[32m" (rich-style->ansi #f (rich-theme-ref theme 'ok)))))

     (let ([theme (make-rich-theme (list (cons 'error (rich-style 'red))))])
       (rich-style? (rich-theme-ref theme 'error)))

     (rich-output-target? (current-output-port))
     (rich-output-target? (make-rich-console))

     )

(mat rich-constructor-macros

     (let ([p (open-output-string)])
       (let ([c (rich-console :output-port p :color-system 'none)])
         (and (rich-console? c)
              (begin
                (rich-print c (rich-style 'red) "x" (reset-style))
                (equal? "x" (get-output-string p))))))

     (equal? "--- Build ----"
             (rich-export-text
              (rich-rule :title "Build" :width 14)))

     (equal? "  x  "
             (rich-export-text
              (rich-padding :body "x" :left 2 :right 2)))

     (equal? "  x   "
             (rich-export-text
              (rich-align :body "x" :width 6 :align 'center)))

     (equal? "a  bb\nccc"
             (rich-export-text
              (rich-columns :items ("a" "bb" "ccc") :width 5 :gap 2)))

     (string-contains?
      (rich-export-text
       (rich-layout :direction 'row :items ("top" "bottom")))
      "top"
      "bottom")

     (equal? "+-------+\n| hello |\n+-------+"
             (rich-export-text
              (rich-panel :body "hello")))

     (let ([tr (rich-tree :label "root")])
       (rich-tree-add! tr "child")
       (equal? "root\n└── child" (rich-export-text tr)))

     (let ([live (rich-live :renderable "one" :refresh-rate 0)])
       (and (rich-live? live)
            (equal? "one" (rich-live-renderable live))))

     (parameterize ([rich-current-time (lambda () 0)])
       (let ([status (rich-status :message "Working" :frames '("a" "b") :interval 1)])
         (equal? "a Working" (rich-export-text status))))

     (let ([progress (rich-progress :width 10 :tasks (("build" 4)))])
       (string-contains? (rich-export-text progress) "build" "0%"))

     (let ([table (rich-table
                    :title "Build"
                    :box 'rounded
                    :columns ("Name" "Status")
                    :rows (("compile" "ok")
                           ("test" "ok")))])
       (and (rich-table? table)
            (string-contains? (rich-export-text table) "Build" "compile" "test")))

     )

(mat rich-theme-errors

     ;; Theme alist values must be rich style objects.
     (error? (make-rich-theme (list (cons 'error "red"))))

     ;; Theme mutation values must be rich style objects.
     (error? (let ([theme (make-rich-theme)])
               (rich-theme-set! theme 'error "red")))

     )

(mat rich-segment

     (let ([s (rich-segment "abc")])
       (and (rich-segment? s)
            (= 3 (rich-segment-width s))
            (equal? "abc" (rich-segment-text s))))

     (equal? 5
             (rich-segments-width
              (list (rich-segment "ab")
                    (rich-segment "cde"))))

     (equal? "abcdef"
             (rich-segments->plain
              (list (rich-segment "abc")
                    (rich-segment "def"))))

     (equal? "red"
             (rich-strip-ansi "\033[31mred\033[0m"))

     (equal? (list "ab" "cd" "e")
             (map rich-segments->plain
                  (rich-segment-wrap (list (rich-segment "abcde")) 2)))

     (equal? "abc"
             (rich-segments->plain
              (rich-segment-crop (list (rich-segment "abcdef")) 3)))

     (let* ([style (rich-style 'red)]
            [lines (rich-segment-wrap (list (rich-segment "abcd" style)) 2)]
            [first (car (car lines))]
            [second (car (cadr lines))])
       (and (eq? style (rich-segment-style first))
            (eq? style (rich-segment-style second))
            (equal? "ab" (rich-segment-text first))
            (equal? "cd" (rich-segment-text second))))

     (let* ([control (rich-segment "\033[31m" #f #t)]
            [text (rich-segment "abcd")]
            [wrapped (rich-segment-wrap (list control text) 2)]
            [cropped (rich-segment-crop (list control text) 2)])
       (and (= 0 (rich-segment-width control))
            (rich-segment-control? (car (car wrapped)))
            (equal? "\033[31mab" (rich-segments->plain (car wrapped)))
            (equal? "cd" (rich-segments->plain (cadr wrapped)))
            (rich-segment-control? (car cropped))
            (equal? "\033[31mab" (rich-segments->plain cropped))))

     (begin
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-segment-lines-test))
        (lambda (value)
          (list (list (rich-segment "ab"))
                (list (rich-segment "cd")))))
       (equal? "ab\ncd"
               (with-output-to-string
                 (lambda ()
                   (rich-print 'rich-render-segment-lines-test)))))

     )

(mat rich-segment-errors

     ;; Segment text must be a string.
     (error? (rich-segment 123))

     ;; Wrap width must be positive.
     (error? (rich-segment-wrap (list (rich-segment "x")) 0))

     ;; Renderer results must be strings or segment-line lists.
     (error? (begin
               (rich-register-renderer!
                (lambda (value) (eq? value 'rich-render-invalid-test))
                (lambda (value) 123))
               (rich-print 'rich-render-invalid-test)))

     ;; rich-render rejects invalid renderer results too.
     (error? (begin
               (rich-register-renderer!
                (lambda (value) (eq? value 'rich-render-invalid-render-test))
                (lambda (value) 123))
               (rich-render (make-rich-console) 'rich-render-invalid-render-test)))

     )

(mat rich-text

     (let ([t (rich-text "hello" (rich-style 'green))])
       (and (rich-text? t)
            (equal? "hello" (rich-text-plain t))))

     (let ([t (make-rich-text)])
       (rich-text-append! t "a" (rich-style 'red))
       (rich-text-append! t "b")
       (equal? "ab" (rich-text-plain t)))

     (let* ([red (rich-style 'red)]
            [blue (rich-style 'blue)]
            [t (make-rich-text)])
       (rich-text-append! t "a" red)
       (rich-text-append! t "b" blue)
       (let ([segments (car (rich-text-render t))])
         (and (equal? "ab" (rich-segments->plain segments))
              (eq? red (rich-segment-style (car segments)))
              (eq? blue (rich-segment-style (cadr segments))))))

     (let ([p (open-output-string)])
       (let ([c (rich-console :output-port p :color-system 'standard)])
         (rich-print c (rich-text "ok" (rich-style 'green)))
         (equal? "\033[32mok\033[0m" (get-output-string p))))

     (equal? "ok" (rich-export-text (rich-text "ok" (rich-style 'green))))

     )

(mat rich-pretty

     (equal? "(1 2 3)" (rich-export-text '(1 2 3)))
     (equal? "#(a b)" (rich-export-text '#(a b)))
     (equal? "\"x\"" (rich-export-text "x"))
     (equal? "x" (with-output-to-string (lambda () (rich-print "x"))))
     (equal? "(1 2 3)" (with-output-to-string
                         (lambda () (rich-print '(1 2 3)))))

     (let ([cycle (list 'a)])
       (set-cdr! cycle cycle)
       (equal? "#0=(a . #0#)" (rich-export-text cycle)))

     )

(mat rich-basic-renderables

     (equal? "--- Build ----"
             (let ([r (rich-rule :title "Build" :width 14)])
               (rich-export-text r)))

     (= 14
        (string-length
         (rich-export-text
          (rich-rule :title "Build" :width 14))))

     (equal? "  x  "
             (let ([p (rich-padding :body "x" :left 2 :right 2)])
               (rich-export-text p)))

     (equal? "  x   "
             (let ([a (rich-align :body "x" :width 6 :align 'center)])
               (rich-export-text a)))

     (equal? "a  bb\nccc"
             (let ([c (rich-columns :items ("a" "bb" "ccc") :width 5 :gap 2)])
               (rich-export-text c)))

     (equal? "a  b"
             (let ([c (rich-columns :items (list "a" "b") :width 10 :gap 2)])
               (rich-export-text c)))

     (equal? "\033[31mx\033[0m  y"
             (let ([c (rich-columns
                        :items ((rich-text "x" (rich-style 'red)) "y")
                        :width 10
                        :gap 2)])
               (rich-export-ansi c)))

     (let ([root (rich-layout :direction 'row :items ("top" "bottom"))])
       (string-contains? (rich-export-text root) "top" "bottom"))

     (equal? "top bottom"
             (let ([root (rich-layout :direction 'row :items (list "top" "bottom"))])
               (rich-export-text root)))

     (equal? "\033[31mtop\033[0m bottom"
             (let ([root (rich-layout
                           :direction 'row
                           :items ((rich-text "top" (rich-style 'red)) "bottom"))])
               (rich-export-ansi root)))

     (let ([chars (rich-box-chars 'ascii)])
       (vector-set! chars 0 "!")
       (equal? "+" (vector-ref (rich-box-chars 'ascii) 0)))

     )

(mat rich-panel-tree

     (equal? (string-append
              "+-------+\n"
             "| hello |\n"
              "+-------+")
             (let ([p (rich-panel :body "hello")])
               (rich-export-text p)))

     (equal? (string-append
              "+---+\n"
              "| \033[31ma\033[0m |\n"
              "| \033[31mb\033[0m |\n"
              "+---+")
             (let ([p (rich-panel :body (rich-text "a\nb" (rich-style 'red)))])
               (rich-export-ansi p)))

     (let ([tr (rich-tree :label "root")])
       (let ([src (rich-tree-add! tr "src")])
         (rich-tree-add! src "main.ss")
         (rich-tree-add! tr "tests")
         (equal? (string-append
                  "root\n"
                  "├── src\n"
                  "│   └── main.ss\n"
                  "└── tests")
                 (rich-export-text tr))))

     (let ([tr (rich-tree :label "root")])
       (rich-tree-add! tr "line1\nline2")
       (equal? (string-append
                "root\n"
                "└── line1\n"
                "    line2")
               (rich-export-text tr)))

     (let ([tr (rich-tree :label (rich-text "root" (rich-style 'red)))])
       (equal? "\033[31mroot\033[0m" (rich-export-ansi tr)))

     (let ([tr (rich-tree :label "root")])
       (rich-tree-add! tr (rich-text "child1\nchild2" (rich-style 'red)))
       (equal? (string-append
                "root\n"
                "└── \033[31mchild1\033[0m\n"
                "    \033[31mchild2\033[0m")
               (rich-export-ansi tr)))

     (let ([p (open-output-string)]
           [tr (rich-tree :label "root")])
       (let ([c (rich-console
                  :output-port p
                  :color-system 'none
                  :ascii-only? #t)])
       (rich-tree-add! tr "child")
       (rich-print c tr)
       (equal? (string-append
                "root\n"
                "`-- child")
               (get-output-string p))))

     )

(mat rich-panel-tree-errors

     ;; Panel body is required.
     (error? (eval '(rich-panel)))

     ;; Tree label is required.
     (error? (eval '(rich-tree)))

     ;; Tree child insertion requires a tree receiver.
     (error? (rich-tree-add! "not-a-tree" "x"))

     )

(mat rich-live-status

     (let ([p (open-output-string)])
       (let* ([c (rich-console :output-port p :color-system 'none)]
              [live (rich-live :console c :renderable "one")])
         (begin
           (rich-live-refresh! live)
           (rich-live-renderable-set! live "two")
           (rich-live-refresh! live)
           (equal? "one\r\033[2Ktwo" (get-output-string p)))))

     (parameterize ([rich-current-time (lambda () 100)])
       (let ([st (rich-status :message "Working")])
         (equal? "⠋ Working" (rich-export-text st))))

     (equal? '("a Working" "b Working" "c Working")
             (let ([now 0])
               (parameterize ([rich-current-time (lambda () now)])
                 (let ([status (make-rich-status "Working"
                                                 (make-rich-console)
                                                 '("a" "b" "c")
                                                 1)])
                   (let ([first (rich-export-text status)])
                     (set! now 1)
                     (let ([second (rich-export-text status)])
                       (set! now 2)
                       (list first second (rich-export-text status))))))))

     (equal? "a Working\r\033[2Kb Working\r\033[2Kc Working"
             (let ([now 0]
                   [p (open-output-string)])
               (parameterize ([rich-current-time (lambda () now)])
                 (let* ([console (make-rich-console)]
                        [status (make-rich-status "Working"
                                                  console
                                                  '("a" "b" "c")
                                                  1)]
                        [live (make-rich-live status console 0 #f)])
                   (rich-console-output-port-set! console p)
                   (rich-console-color-system-set! console 'none)
                   (rich-live-refresh! live)
                   (set! now 1)
                   (rich-live-refresh! live)
                   (set! now 2)
                   (rich-live-refresh! live)
                   (get-output-string p)))))

     )

(mat rich-live-errors

     ;; Live refresh requires a rich-live object.
     (error? (rich-live-refresh! "not-live"))

     ;; Status message must be printable text.
     (error? (rich-status :message #f))

     )

(mat rich-progress

     (let ([pr (rich-progress :width 10)])
       (let ([id (rich-progress-add-task! pr "download" 10)])
         (rich-progress-update! pr id 4)
         (string-contains? (rich-export-text pr) "download" "40%")))

     (let ([pr (rich-progress :width 10)])
       (let ([id (rich-progress-add-task! pr "build" #f)])
         (rich-progress-advance! pr id 3)
         (string-contains? (rich-export-text pr) "build" "3")))

     )

(mat rich-progress-empty-task
     (let ([progress (make-rich-progress 4 '(description bar percentage completed))])
       (rich-progress-add-task! progress "empty" 0)
       (string=? "empty [####] 100% 0/0" (rich-export-text progress)))

     (let* ([progress (make-rich-progress 4 '(description bar percentage completed))]
            [id (rich-progress-add-task! progress "empty" #f)])
       (rich-progress-task-total-set! progress id 0)
       (rich-progress-complete! progress id)
       (string=? "empty [####] 100% 0/0" (rich-export-text progress))))

(mat rich-progress-errors

     ;; Progress width must be positive.
     (error? (rich-progress :width 0))

     ;; Progress updates require a known task id.
     (error? (let ([p (make-rich-progress)])
               (rich-progress-update! p 999 1)))

     ;; Error case: task totals must be nonnegative or #f.
     (error? (let ([p (make-rich-progress)])
               (rich-progress-add-task! p "bad" -1)))

     )

(mat rich-prompt

     (let ([in (open-input-string "maoif\n")]
           [out (open-output-string)])
       (define c (rich-console :input-port in :output-port out :color-system 'none))
       (equal? "maoif" (rich-prompt c "Name")))

     (let ([in (open-input-string "\n")]
           [out (open-output-string)])
       (define c (rich-console :input-port in :output-port out :color-system 'none))
       (equal? "default" (rich-prompt c "Name" "default")))

     (let ([in (open-input-string "y\n")]
           [out (open-output-string)])
       (define c (rich-console :input-port in :output-port out :color-system 'none))
       (rich-confirm c "Continue?"))

     )

(mat rich-prompt-errors

     ;; Prompt choices must include the entered value.
     (error? (let ([in (open-input-string "bad\n")]
                   [out (open-output-string)])
               (define c (rich-console :input-port in :output-port out :color-system 'none))
               (rich-prompt c "Mode" #f '("fast" "slow"))))

     )

(mat rich-table

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (rich-table-add-column! t "Status")
       (rich-table-add-row! t "build" "ok")
       (equal? (string-append
                "+-------+--------+\n"
                "| Name  | Status |\n"
                "+-------+--------+\n"
                "| build | ok     |\n"
                "+-------+--------+")
               (rich-export-text t)))

     (let ([t (rich-table
                :title "Build"
                :box 'rounded
                :columns ("Name" "Status")
                :rows (("compile" "ok")
                       ("test" "ok")))])
       (and (rich-table? t)
            (string-contains? (rich-export-text t) "Build" "compile" "test")))

     (begin
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-table-symbol-render-test))
        (lambda (value) (list (list (rich-segment "X")))))
       (let ([t (make-rich-table)])
         (rich-table-add-column! t "Name")
         (rich-table-add-row! t 'rich-table-symbol-render-test)
         (string-contains? (rich-export-text t) "X")))

     (let ([columns (vector "Name" "Status")]
           [rows '(("build" "ok"))])
       (define t
         (rich-table
           :columns (vector->list columns)
           :rows (map (lambda (row) row) rows)))
       (equal? (string-append
                "+-------+--------+\n"
                "| Name  | Status |\n"
                "+-------+--------+\n"
                "| build | ok     |\n"
                "+-------+--------+")
               (rich-export-text t)))

     )

(mat rich-table-errors

     ;; Table rows must match the number of columns.
     (error? (let ([t (make-rich-table)])
               (rich-table-add-column! t "Name")
               (rich-table-add-row! t "a" "b")))

     ;; Table box style must be known.
     (error? (eval '(rich-table :box 'unknown)))

     )

(mat rich-basic-errors

     ;; Rule width must be positive.
     (error? (eval '(rich-rule :width 0)))

     ;; Padding values must be non-negative.
     (error? (eval '(rich-padding :body "x" :left -1)))

     ;; Alignment must be left, center, or right.
     (error? (eval '(rich-align :body "x" :align 'middle)))

     )

(mat rich-console

     (let ([p (open-output-string)])
       (let ([c (make-rich-console)])
         (rich-console-output-port-set! c p)
         (rich-console-width-set! c 12)
         (rich-print c "ok")
         (equal? "ok" (get-output-string p))))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :width 20
           :color-system 'none))
       (and (rich-console? c)
            (= 20 (rich-console-width c))
            (begin
              (rich-print c (rich-style 'red) "x" (reset-style))
              (equal? "x" (get-output-string p)))))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :color-system 'standard))
       (rich-print c (rich-style 'red) "x" (reset-style))
       (equal? "\033[31mx\033[0m" (get-output-string p)))

     (let ()
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-styled-segment-test))
        (lambda (value)
          (list (list (rich-segment "x" (rich-style 'red))))))
       (and (let ([p (open-output-string)])
              (define c
                (rich-console
                  :output-port p
                  :color-system 'standard))
              (rich-print c 'rich-render-styled-segment-test)
              (equal? "\033[31mx\033[0m" (get-output-string p)))
            (let ([p (open-output-string)])
              (define c
                (rich-console
                  :output-port p
                  :color-system 'none))
              (rich-print c 'rich-render-styled-segment-test)
              (equal? "x" (get-output-string p)))
            (equal? "x"
                    (with-output-to-string
                      (lambda ()
                        (rich-print 'rich-render-styled-segment-test))))
            (equal? "\033[31mx\033[0m"
                    (rich-export-ansi 'rich-render-styled-segment-test))))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :color-system 'standard))
       (rich-print c (rich-style) (reset-style) "x")
       (equal? "x" (get-output-string p)))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :color-system 'standard))
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-outer-style-test))
        (lambda (value)
          (list (list (rich-segment "S" (rich-style 'blue))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-outer-style-test
                   "B"
                   (reset-style))
       (equal? "\033[31mA\033[34mS\033[0m\033[31mB\033[0m"
               (get-output-string p)))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :color-system 'standard))
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-segment-reset-test))
        (lambda (value)
          (list (list (rich-segment "S" (reset-style))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-segment-reset-test
                   "B"
                   (reset-style))
       (equal? "\033[31mA\033[0mS\033[31mB\033[0m"
               (get-output-string p)))

     (let ([p (open-output-string)])
       (define c
         (rich-console
           :output-port p
           :color-system 'standard))
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-empty-segment-style-test))
        (lambda (value)
          (list (list (rich-segment "S" (rich-style))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-empty-segment-style-test
                   "B"
                   (reset-style))
       (equal? "\033[31mASB\033[0m" (get-output-string p)))

     )

(mat rich-integration

     (let ([out (open-output-string)])
       (define c (rich-console :output-port out :color-system 'standard))
       (define p (rich-panel :body "done" :box 'ascii))
       (rich-print c
                   (rich-style 'bold 'green)
                   "pass"
                   (reset-style)
                   " "
                   p)
       (string-contains? (get-output-string out) "\033[1m" "pass" "+------+" "done"))

     (let ([t (rich-table
                :columns ("Name" "Status")
                :rows (("compile" "ok")))])
       (string-contains? (rich-export-ansi t) "compile" "ok"))

     )

(mat rich-console-errors

     ;; Console width must be positive.
     (error? (rich-console :width 0))

     ;; Console output port must be an output port.
     (error? (rich-console :output-port "not-a-port"))

     ;; Unknown constructor macro fields are rejected.
     (error? (eval '(rich-console :unknown 1)))

     ;; Console input port must be an input port.
     (error? (rich-console :input-port "not-a-port"))

     ;; Console color system must be supported.
     (error? (rich-console :color-system 'unsupported))

     )

(mat rich-console-auto-color

     (equal? "x"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port)
                            (rich-text "x" (rich-style 'red))))))

     (equal? "x"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :color-system 'auto)
                            (rich-style 'red) "x" (reset-style)))))

     (equal? "\033[31mx\033[0m"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :force-terminal? #t)
                            (rich-text "x" (rich-style 'red))))))

     (equal? "x"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :color-system 'none
                                          :force-terminal? #t)
                            (rich-text "x" (rich-style 'red))))))

     )

(mat rich-console-theme-styles

     (let* ([theme (rich-theme :accent (rich-style 'red))]
            [text (rich-text "x" 'accent)]
            [render (lambda ()
                      (rich-string-output
                       (lambda (port)
                         (rich-print (rich-console :output-port port :theme theme
                                                   :color-system 'standard)
                                     (rich-panel :body text)))))])
       (let ([first (render)])
         (rich-theme-set! theme 'accent (rich-style 'blue))
         (and (equal? "+---+\n| \033[31mx\033[0m |\n+---+" first)
              (equal? "+---+\n| \033[34mx\033[0m |\n+---+" (render)))))

     (equal? "x"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :color-system 'none)
                            (rich-text "x" 'accent)))))

     (equal? "x" (rich-export-text (rich-text "x" 'accent)))

     (equal? "\033[31mA\033[34mS\033[0m\033[31mB\033[0m"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :color-system 'standard
                                          :theme (rich-theme :accent (rich-style 'blue)))
                            (rich-style 'red) "A" (rich-text "S" 'accent)
                            "B" (reset-style)))))

     ;; Error case: a named style needs a binding when ANSI output is enabled.
     (error? (rich-export-ansi (rich-text "x" 'missing)))

     )

(mat rich-progress-selected-columns

     (let ([progress (make-rich-progress 4 '(completed description percentage bar))])
       (let ([id (rich-progress-add-task! progress "build" 8)])
         (rich-progress-update! progress id 2)
         (equal? "2/8 build 25% [#---]" (rich-export-text progress))))

     (let ([progress (rich-progress :width 4 :columns '(description))])
       (rich-progress-add-task! progress "build" #f)
       (equal? "build" (rich-export-text progress)))

     (let ([progress (make-rich-progress 4 '())])
       (rich-progress-add-task! progress "build" 8)
       (equal? "" (rich-export-text progress)))

     (parameterize ([rich-current-time (lambda () 0)])
       (let ([progress (make-rich-progress 4 '(percentage completed remaining speed))])
         (rich-progress-add-task! progress "unknown" #f)
         (equal? "? 0 --:--:-- 0.0/s" (rich-export-text progress))))

     (let ([now 0])
       (parameterize ([rich-current-time (lambda () now)])
         (let ([progress (make-rich-progress 4 '(spinner))])
           (rich-progress-add-task! progress "spin" #f)
           (set! now 1/10)
           (equal? "/" (rich-export-text progress)))))

     (let ([now 0])
       (parameterize ([rich-current-time (lambda () now)])
         (let* ([progress (make-rich-progress 4 '(elapsed remaining speed spinner))]
                [id (rich-progress-add-task! progress "build" 8)])
           (set! now 2)
           (rich-progress-update! progress id 4)
           (let ([running (rich-export-text progress)])
             (rich-progress-complete! progress id)
             (set! now 10)
             (and (equal? "00:00:02 00:00:02 2.0/s |" running)
                  (equal? "00:00:02 00:00:00 4.0/s |"
                          (rich-export-text progress)))))))

     ;; Error case: column descriptors must be supported names.
     (error? (make-rich-progress 4 '(unknown)))

     )

(mat rich-render-width-context

     (begin
       (rich-register-renderer!
        (lambda (value) (eq? value 'width-context-test))
        (lambda (value) (make-string (rich-render-width) #\-)))
       (let ([console (rich-console :width 17)])
         (and (equal? (make-string 17 #\-) (rich-render console 'width-context-test))
              (equal? (make-string 17 #\-)
                      (rich-string-output
                       (lambda (port)
                         (rich-console-output-port-set! console port)
                         (rich-print console 'width-context-test)))))))

     (let ([console (make-rich-console)])
       (rich-console-width-set! console 17)
       (rich-console-width-set! console #f)
       (rich-string-output
        (lambda (port)
          (rich-console-output-port-set! console port)
          (unless (= 80 (rich-console-width console))
            (error 'width-context-test "wrong fallback width"))))
       (= 80 (rich-console-width console)))

     (= 80 (string-length (rich-export-text 'width-context-test)))

     )

(mat rich-rule-auto-width

     (equal? (make-string 80 #\-) (rich-export-text (make-rich-rule)))

     (equal? (make-string 23 #\-)
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :width 23) (make-rich-rule)))))

     (equal? (make-string 7 #\-)
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :width 23)
                            (rich-rule :width 7)))))

     (let ([rule (rich-rule :width 7)])
       (rich-rule-width-set! rule #f)
       (equal? (make-string 80 #\-) (rich-export-text rule)))

     (equal? "  ---------------   "
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :width 20)
                            (rich-padding :body (make-rich-rule) :left 2 :right 3)))))

     (equal? "+----------+\n| -------- |\n+----------+"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :width 12)
                            (rich-panel :body (make-rich-rule))))))

     (equal? "------"
             (rich-export-text (rich-align :body (make-rich-rule) :width 6)))

     (equal? "+-------+\n|-------|\n+-------+"
             (rich-string-output
              (lambda (port)
                (rich-print (rich-console :output-port port :width 30)
                            (rich-panel :body (make-rich-rule) :width 9 :padding 0)))))

     )

(mat rich-rule-container-width

     (equal? "------- -------"
             (parameterize ([rich-render-width 15])
               (rich-export-text (make-rich-layout 'row
                                                   (list (make-rich-rule) (make-rich-rule))))))

     (equal? "root\n└── ------"
             (parameterize ([rich-render-width 10])
               (let ([tree (make-rich-tree "root")])
                 (rich-tree-add! tree (make-rich-rule))
                 (rich-export-text tree))))

     (equal? "+-----+-----+\n| --- | --- |\n+-----+-----+"
             (parameterize ([rich-render-width 13])
               (let ([table (rich-table :show-header? #f :columns ("a" "b"))])
                 (rich-table-add-row! table (make-rich-rule) (make-rich-rule #f 3))
                 (rich-export-text table))))

     )

(mat rich-progress-auto-width

     (parameterize ([rich-render-width 30] [rich-current-time (lambda () 0)])
       (let ([progress (make-rich-progress #f '(description bar percentage completed))])
         (let ([id (rich-progress-add-task! progress "a" 10)])
           (rich-progress-update! progress id 5))
         (rich-progress-add-task! progress "long" 10)
         (equal? "a    [#######-------] 50% 5/10\nlong [--------------]  0% 0/10"
                 (rich-export-text progress))))

     (parameterize ([rich-render-width 10])
       (let ([progress (make-rich-progress #f '(description bar percentage))])
         (rich-progress-add-task! progress "long-description" 10)
         (equal? "long [] 0%" (rich-export-text progress))))

     (parameterize ([rich-render-width 3])
       (let ([progress (make-rich-progress #f '(description bar percentage))])
         (rich-progress-add-task! progress "long-description" 10)
         (= 3 (string-length (rich-export-text progress)))))

     (parameterize ([rich-render-width 15])
       (let ([progress (make-rich-progress #f '(bar bar))])
         (rich-progress-add-task! progress "x" 10)
         (equal? "[-----] [-----]" (rich-export-text progress))))

     (parameterize ([rich-render-width 30])
       (let ([progress (make-rich-progress 4 '(description bar))])
         (rich-progress-add-task! progress "x" 10)
         (equal? "x [----]" (rich-export-text progress))))

     (let ([progress (rich-progress :columns '(bar))])
       (rich-progress-add-task! progress "x" 10)
       (= 80 (string-length (rich-export-text progress))))

     )

(mat rich-live-single-frame

     (let ([calls 0])
       (rich-register-renderer!
        (lambda (value) (eq? value 'live-single-frame-test))
        (lambda (value)
          (set! calls (+ calls 1))
          (if (odd? calls) "abcdefghij" "x")))
       (let ([output
              (rich-string-output
               (lambda (port)
                 (let ([live (make-rich-live 'live-single-frame-test
                                            (rich-console :output-port port :width 5) 0 #f)])
                   (rich-live-refresh! live)
                   (rich-live-refresh! live))))])
         (and (= calls 2)
              (equal? "abcdefghij\r\033[2K\033[1A\r\033[2Kx" output))))

     (equal? "abcde\r\033[2Kx"
             (rich-string-output
              (lambda (port)
                (let ([live (make-rich-live "abcde"
                                           (rich-console :output-port port :width 5) 0 #f)])
                  (rich-live-refresh! live)
                  (rich-live-renderable-set! live "x")
                  (rich-live-refresh! live)))))

     )

(mat rich-terminal-sizing

     (or (eq? (system-platform) 'windows)
         (= 0 (system "cd .. && python3 tests/integration/rich-terminal.py")))

     )

(mat rich-live-control-width

     (equal? "abcde\rx\r\033[2Ky"
             (rich-string-output
              (lambda (port)
                (let ([live (make-rich-live "abcde\rx"
                                           (rich-console :output-port port :width 5) 0 #f)])
                  (rich-live-refresh! live)
                  (rich-live-renderable-set! live "y")
                  (rich-live-refresh! live)))))

     (equal? "a\tbc\r\033[2K\033[1A\r\033[2Ky"
             (rich-string-output
              (lambda (port)
                (let ([live (make-rich-live "a\tbc"
                                           (rich-console :output-port port :width 8) 0 #f)])
                  (rich-live-refresh! live)
                  (rich-live-renderable-set! live "y")
                  (rich-live-refresh! live)))))

     (equal? "abcd\bbx\r\033[2Ky"
             (rich-string-output
              (lambda (port)
                (let ([live (make-rich-live "abcd\bbx"
                                           (rich-console :output-port port :width 5) 0 #f)])
                  (rich-live-refresh! live)
                  (rich-live-renderable-set! live "y")
                  (rich-live-refresh! live)))))

     )
