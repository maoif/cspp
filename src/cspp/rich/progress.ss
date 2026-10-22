#!chezscheme
(library (cspp rich progress)
  (export make-rich-progress
          rich-progress
          rich-progress?
          rich-progress-add-task!
          rich-progress-update!
          rich-progress-advance!
          rich-progress-complete!
          rich-progress-render
          rich-progress-default-columns
          rich-progress-description-column
          rich-progress-bar-column
          rich-progress-percentage-column
          rich-progress-completed-column
          rich-progress-elapsed-column
          rich-progress-remaining-column
          rich-progress-speed-column
          rich-progress-spinner-column
          rich-progress-task-description-set!
          rich-progress-task-total-set!
          rich-progress-task-visible?-set!)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich live))

  (define-record-type (rich-progress-record $make-rich-progress-record $rich-progress?)
    (fields (mutable next-id $rich-progress-next-id $rich-progress-next-id-set!)
            (mutable width $rich-progress-width $rich-progress-width-set!)
            (mutable columns $rich-progress-columns $rich-progress-columns-set!)
            (mutable tasks $rich-progress-tasks $rich-progress-tasks-set!)
            (mutable live $rich-progress-live $rich-progress-live-set!)))

  (define-record-type (rich-progress-task-record $make-rich-progress-task-record $rich-progress-task?)
    (fields (immutable id $rich-progress-task-id)
            (mutable description $rich-progress-task-description $rich-progress-task-description-set!)
            (mutable total $rich-progress-task-total $rich-progress-task-total-set!)
            (mutable completed $rich-progress-task-completed $rich-progress-task-completed-set!)
            (mutable visible? $rich-progress-task-visible? $rich-progress-task-visible?-set!)
            (mutable start-time $rich-progress-task-start-time $rich-progress-task-start-time-set!)
            (mutable stop-time $rich-progress-task-stop-time $rich-progress-task-stop-time-set!)))

  (define $default-columns
    '(description bar percentage completed elapsed remaining speed))

  (define $total?
    (lambda (x)
      (or (not x) (rich-nonnegative-integer? x))))

  (define $nonnegative-real?
    (lambda (x)
      (and (real? x) (not (negative? x)))))

  (define $columns?
    (lambda (x)
      (and (list? x)
           (rich-list-every?
            (lambda (column)
              (and (memq column '(description bar percentage completed elapsed remaining speed
                                  spinner)) #t))
            x))))

  (define $append-one
    (lambda (ls value)
      (let loop ([ls ls] [out '()])
        (if (null? ls)
            (rich-reverse-append out (list value))
            (loop (cdr ls) (cons (car ls) out))))))

  (define $progress-task-ref
    (lambda (progress id who)
      (let loop ([tasks ($rich-progress-tasks progress)])
        (cond [(null? tasks)
               (errorf who "unknown progress task id: ~a" id)]
              [(= id ($rich-progress-task-id (car tasks)))
               (car tasks)]
              [else
               (loop (cdr tasks))]))))

  (define $string-repeat
    (lambda (text count)
      (let loop ([count count] [out '()])
        (if (zero? count)
            (apply string-append (reverse out))
            (loop (- count 1) (cons text out))))))

  (define $rounded-exact
    (lambda (value)
      (let ([rounded (round value)])
        (if (exact? rounded)
            rounded
            (inexact->exact rounded)))))

  (define $percent
    (lambda (completed total)
      (if (zero? total)
          100
          (min 100 ($rounded-exact (* 100 (/ completed total)))))))

  (define $task-bar
    (lambda (task width)
      (let* ([total ($rich-progress-task-total task)]
             [completed ($rich-progress-task-completed task)]
             [filled (cond [(not total) 0]
                           [(zero? total) width]
                           [else (min width ($rounded-exact (* width (/ completed total))))])])
        (string-append "["
                       ($string-repeat "#" filled)
                       ($string-repeat "-" (- width filled))
                       "]"))))

  (define $duration-string
    (lambda (seconds)
      (let* ([seconds (inexact->exact (floor seconds))]
             [hours (quotient seconds 3600)]
             [minutes (quotient (remainder seconds 3600) 60)]
             [seconds (remainder seconds 60)]
             [two-digits (lambda (n)
                           (string-append (if (< n 10) "0" "") (number->string n)))])
        (string-append (two-digits hours) ":" (two-digits minutes) ":" (two-digits seconds)))))

  (define $task-column
    (lambda (task width column now)
      (let* ([completed ($rich-progress-task-completed task)]
             [total ($rich-progress-task-total task)]
             [elapsed (max 0 (- (or ($rich-progress-task-stop-time task) now)
                                ($rich-progress-task-start-time task)))]
             [speed (if (positive? elapsed) (/ completed elapsed) 0)])
        (case column
          [(description) ($rich-progress-task-description task)]
          [(bar) ($task-bar task width)]
          [(percentage) (if total
                            (string-append (number->string ($percent completed total)) "%")
                            "?")]
          [(completed) (if total
                           (string-append (number->string completed) "/" (number->string total))
                           (number->string completed))]
          [(elapsed) ($duration-string elapsed)]
          [(remaining) (cond [(and total (>= completed total)) "00:00:00"]
                             [(and total (positive? speed))
                              ($duration-string (ceiling (/ (- total completed) speed)))]
                             [else "--:--:--"])]
          [(speed) (string-append
                    (number->string (exact->inexact (/ (round (* speed 100)) 100))) "/s")]
          [(spinner) (vector-ref '#("|" "/" "-" "\\")
                                 (modulo (inexact->exact (floor (* elapsed 10))) 4))]))))

  (define $task-line
    (lambda (task width columns now)
      (let loop ([columns columns] [out '()] [first? #t])
        (if (null? columns)
            (apply string-append (reverse out))
            (loop (cdr columns)
                  (cons ($task-column task width (car columns) now)
                        (if first? out (cons " " out)))
                  #f)))))

  (define $visible-tasks
    (lambda (progress)
      (let loop ([tasks ($rich-progress-tasks progress)] [out '()])
        (cond [(null? tasks) (reverse out)]
              [($rich-progress-task-visible? (car tasks))
               (loop (cdr tasks) (cons (car tasks) out))]
              [else
               (loop (cdr tasks) out)]))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Progress Columns
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-progress-default-columns
  The `rich-progress-default-columns` procedure returns the default progress
  column descriptors.
  |#
  (define rich-progress-default-columns
    (lambda ()
      (map (lambda (column) column) $default-columns)))

  #|proc:rich-progress-description-column
  The `rich-progress-description-column` procedure returns a description column
  descriptor.
  |#
  (define rich-progress-description-column
    (lambda ()
      'description))

  #|proc:rich-progress-bar-column
  The `rich-progress-bar-column` procedure returns a bar column descriptor.
  |#
  (define rich-progress-bar-column
    (lambda ()
      'bar))

  #|proc:rich-progress-percentage-column
  The `rich-progress-percentage-column` procedure returns a percentage column
  descriptor.
  |#
  (define rich-progress-percentage-column
    (lambda ()
      'percentage))

  #|proc:rich-progress-completed-column
  The `rich-progress-completed-column` procedure returns a completed-count
  column descriptor.
  |#
  (define rich-progress-completed-column
    (lambda ()
      'completed))

  #|proc:rich-progress-elapsed-column
  The `rich-progress-elapsed-column` procedure returns an elapsed-time column
  descriptor.
  |#
  (define rich-progress-elapsed-column
    (lambda ()
      'elapsed))

  #|proc:rich-progress-remaining-column
  The `rich-progress-remaining-column` procedure returns a remaining-time
  column descriptor.
  |#
  (define rich-progress-remaining-column
    (lambda ()
      'remaining))

  #|proc:rich-progress-speed-column
  The `rich-progress-speed-column` procedure returns a speed column descriptor.
  |#
  (define rich-progress-speed-column
    (lambda ()
      'speed))

  #|proc:rich-progress-spinner-column
  The `rich-progress-spinner-column` procedure returns a spinner column
  descriptor.
  |#
  (define rich-progress-spinner-column
    (lambda ()
      'spinner))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Progress
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-progress?
  The `rich-progress?` procedure returns `#t` when its argument is a rich
  progress object, and `#f` otherwise.
  |#
  (define rich-progress?
    (lambda (x)
      ($rich-progress? x)))

  #|proc:make-rich-progress
  The `make-rich-progress` procedure constructs a progress display with a
  positive integer bar `width` (default 40) and no tasks. The list `columns` selects
  output columns in order; its default is `rich-progress-default-columns`. Supported
  symbols are `description`, `bar`, `percentage`, `completed`, `elapsed`, `remaining`,
  `speed`, and `spinner`. An empty list produces empty task lines. Returns the display.
  |#
  (define make-rich-progress
    (case-lambda
      [()
       ($make-rich-progress-record 0 40 (rich-progress-default-columns) '() #f)]
      [(width columns)
       (pcheck ([rich-positive-integer? width] [$columns? columns])
               ($make-rich-progress-record 0 width columns '() #f))]))

  (define $rich-progress-width-set!/checked
    (lambda (progress width)
      (pcheck ([rich-progress? progress] [rich-positive-integer? width])
              ($rich-progress-width-set! progress width))))

  (define $rich-progress-columns-set!/checked
    (lambda (progress columns)
      (pcheck ([rich-progress? progress] [$columns? columns])
              ($rich-progress-columns-set! progress columns))))

  #|proc:rich-progress-add-task!
  The `rich-progress-add-task!` procedure adds a task to the progress display `progress`.
  The string `description` labels the task. The `total` is an exact nonnegative count or `#f`
  when unknown. A zero total renders as complete (100%). Returns the numeric task id.
  |#
  (define rich-progress-add-task!
    (lambda (progress description total)
      (pcheck ([rich-progress? progress]
               [string? description]
               [$total? total])
              (let* ([id ($rich-progress-next-id progress)]
                     [task ($make-rich-progress-task-record
                            id description total 0 #t ((rich-current-time)) #f)])
                ($rich-progress-next-id-set! progress (+ id 1))
                ($rich-progress-tasks-set!
                 progress
                 ($append-one ($rich-progress-tasks progress) task))
                id))))

  #|proc:rich-progress-update!
  The `rich-progress-update!` procedure sets the completed count for a progress
  task and returns `progress`.
  |#
  (define rich-progress-update!
    (lambda (progress id completed)
      (pcheck ([rich-progress? progress]
               [rich-nonnegative-integer? id]
               [$nonnegative-real? completed])
              (let ([task ($progress-task-ref progress id 'rich-progress-update!)])
                ($rich-progress-task-completed-set! task completed)
                progress))))

  #|proc:rich-progress-advance!
  The `rich-progress-advance!` procedure increments the completed count for a
  progress task and returns `progress`.
  |#
  (define rich-progress-advance!
    (case-lambda
      [(progress id)
       (rich-progress-advance! progress id 1)]
      [(progress id amount)
       (pcheck ([rich-progress? progress]
                [rich-nonnegative-integer? id]
                [$nonnegative-real? amount])
               (let ([task ($progress-task-ref progress id 'rich-progress-advance!)])
                 ($rich-progress-task-completed-set!
                  task
                  (+ ($rich-progress-task-completed task) amount))
                 progress))]))

  #|proc:rich-progress-complete!
  The `rich-progress-complete!` procedure marks a progress task complete and
  records its stop time.
  |#
  (define rich-progress-complete!
    (lambda (progress id)
      (pcheck ([rich-progress? progress] [rich-nonnegative-integer? id])
              (let ([task ($progress-task-ref progress id 'rich-progress-complete!)])
                (when ($rich-progress-task-total task)
                  ($rich-progress-task-completed-set!
                   task
                   ($rich-progress-task-total task)))
                ($rich-progress-task-stop-time-set! task ((rich-current-time)))
                progress))))

  #|proc:rich-progress-task-description-set!
  The `rich-progress-task-description-set!` procedure sets a task description.
  |#
  (define rich-progress-task-description-set!
    (lambda (progress id description)
      (pcheck ([rich-progress? progress]
               [rich-nonnegative-integer? id]
               [string? description])
              ($rich-progress-task-description-set!
               ($progress-task-ref progress id 'rich-progress-task-description-set!)
               description))))

  #|proc:rich-progress-task-total-set!
  The `rich-progress-task-total-set!` procedure sets the total for task `id` in `progress`.
  The `total` is an exact nonnegative count or `#f` when unknown. A zero total renders as
  complete (100%). Returns an unspecified value.
  |#
  (define rich-progress-task-total-set!
    (lambda (progress id total)
      (pcheck ([rich-progress? progress]
               [rich-nonnegative-integer? id]
               [$total? total])
              ($rich-progress-task-total-set!
               ($progress-task-ref progress id 'rich-progress-task-total-set!)
               total))))

  #|proc:rich-progress-task-visible?-set!
  The `rich-progress-task-visible?-set!` procedure sets whether a task is
  rendered.
  |#
  (define rich-progress-task-visible?-set!
    (lambda (progress id visible?)
      (pcheck ([rich-progress? progress]
               [rich-nonnegative-integer? id]
               [boolean? visible?])
              ($rich-progress-task-visible?-set!
               ($progress-task-ref progress id 'rich-progress-task-visible?-set!)
               visible?))))

  #|proc:rich-progress-render
  The `rich-progress-render` procedure renders visible progress tasks as segment
  lines and returns them in task order using the configured columns of `progress`.
  Elapsed and estimated remaining durations use `HH:MM:SS`; unavailable estimates
  use `--:--:--`. Speed is the average completed units per second, rounded to two
  decimal places. Unknown totals show `?` for percentage and a bare completed count.
  Timing uses `rich-current-time` and freezes when the task is explicitly completed.
  Spinner frames cycle through `|`, `/`, `-`, and `\` every tenth of a second.
  |#
  (define rich-progress-render
    (lambda (progress)
      (pcheck ([rich-progress? progress])
              (let ([now ((rich-current-time))])
                (map (lambda (task)
                       (list (rich-segment
                              ($task-line task ($rich-progress-width progress)
                                          ($rich-progress-columns progress) now))))
                     ($visible-tasks progress))))))

  #|macro:rich-progress
  The `rich-progress` macro constructs and returns a progress display.
  |#
  (define-syntax rich-progress
    (lambda (stx)
      (define field->action
        (lambda (name field value)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:width)
               (with-syntax ([progress-name name] [field-value value])
                 #'($rich-progress-width-set!/checked progress-name field-value))]
              [(:columns)
               (with-syntax ([progress-name name] [field-value value])
                 #'($rich-progress-columns-set!/checked progress-name field-value))]
              [(:tasks)
               (with-syntax ([progress-name name] [task-list value])
                 #'(for-each
                    (lambda (task)
                      (rich-progress-add-task! progress-name (car task) (cadr task)))
                    task-list))]
              [else (syntax-error field "invalid rich-progress field")]))))
      (define task-list-value
        (lambda (value)
          (syntax-case value ()
            [()
             #''()]
            [((description total) ...)
             #'(list (list description total) ...)]
            [_
             value])))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [action* '()])
            (cond [(null? clause*) (reverse action*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-progress form")]
                  [else
                   (loop (cddr clause*)
                         (cons (field->action name
                                              (car clause*)
                                              (if (eq? (syntax->datum (car clause*)) ':tasks)
                                                  (task-list-value (cadr clause*))
                                                  (cadr clause*)))
                               action*))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([tmp (car (generate-temporaries #'(rich-progress)))])
           (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
             #'(let ([tmp (make-rich-progress)])
                 action ...
                 tmp)))]
        [_ (syntax-error stx "invalid rich-progress form")])))

  (rich-register-renderer! rich-progress? rich-progress-render))
