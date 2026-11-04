#!chezscheme
(library (cspp glob)
  (export make-glob glob? glob-match? glob glob* glob->iter)
  (import (cspp chez)
          (cspp path)
          (cspp file)
          (cspp iter)
          (cspp regex)
          (cspp utils)
          (cspp list)
          (cspp system platform))

  #|record:$glob
  An opaque compiled glob. `flavor` is the path flavor used to parse and match
  every branch; `patterns` is the immutable ordered list of compiled branches.
  |#
  (define-record-type ($glob %make-glob %glob?)
    (opaque #t) (sealed #t)
    (fields (immutable flavor glob-flavor) (immutable patterns glob-patterns)))

  #|record:$component
  An internal path-component matcher. `regex` matches one complete component;
  `literal` is the original text when the component has no wildcard syntax.
  |#
  (define-record-type ($component %component component?)
    (fields (immutable regex component-regex) (immutable literal component-literal)))

  #|record:$branch
  An internal expanded pattern branch. `path` stores its parsed root and
  components; `components` stores matchers; `separator` preserves output spelling.
  |#
  (define-record-type ($branch %branch branch?)
    (fields (immutable path branch-path) (immutable components branch-components)
            (immutable separator branch-separator) (immutable directory? branch-directory?)))

  ;; Path syntax follows the host OS when callers omit an explicit flavor.
  (define $default-flavor
    (if (eq? (system-platform) 'windows) 'windows 'unix))
  (define $error (lambda (message . args) (apply errorf 'make-glob message args)))
  (define $prefix?
    (lambda (prefix s)
      (let ([n (string-length prefix)])
        (and (fx<= n (string-length s)) (string=? prefix (substring s 0 n))))))
  (define $brace-open
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0] [class-start #f])
          (cond [(fx= i n) #f]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) class-start)
                     ($error "trailing escape"))]
                [(and class-start (char=? (string-ref s i) #\])
                      (fx> i (fx1+ class-start)))
                 (loop (fx1+ i) #f)]
                [(and (not class-start) (char=? (string-ref s i) #\[))
                 (loop (fx1+ i) i)]
                [class-start (loop (fx1+ i) class-start)]
                [(char=? (string-ref s i) #\}) ($error "unmatched closing brace")]
                [(char=? (string-ref s i) #\{) i]
                [else (loop (fx1+ i) #f)])))))
  (define $brace-close
    (lambda (s open)
      (let ([n (string-length s)])
        (let loop ([i (fx1+ open)] [depth 1] [class-start #f])
          (cond [(fx= i n) ($error "unmatched opening brace")]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) depth class-start)
                     ($error "trailing escape"))]
                [(and class-start (char=? (string-ref s i) #\])
                      (fx> i (fx1+ class-start)))
                 (loop (fx1+ i) depth #f)]
                [(and (not class-start) (char=? (string-ref s i) #\[))
                 (loop (fx1+ i) depth i)]
                [class-start (loop (fx1+ i) depth class-start)]
                [(char=? (string-ref s i) #\{)
                 (loop (fx1+ i) (fx1+ depth) #f)]
                [(char=? (string-ref s i) #\})
                 (if (fx= depth 1) i (loop (fx1+ i) (fx1- depth) #f))]
                [else (loop (fx1+ i) depth #f)])))))
  (define $split
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0] [start 0] [depth 0] [out '()] [class-start #f])
          (cond [(fx= i n) (reverse (cons (substring s start n) out))]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) start depth out class-start)
                     ($error "trailing escape in brace"))]
                [(and class-start (char=? (string-ref s i) #\])
                      (fx> i (fx1+ class-start)))
                 (loop (fx1+ i) start depth out #f)]
                [(and (not class-start) (char=? (string-ref s i) #\[))
                 (loop (fx1+ i) start depth out i)]
                [class-start (loop (fx1+ i) start depth out class-start)]
                [(char=? (string-ref s i) #\{)
                 (loop (fx1+ i) start (fx1+ depth) out #f)]
                [(char=? (string-ref s i) #\})
                 (loop (fx1+ i) start (fx1- depth) out #f)]
                [(and (char=? (string-ref s i) #\,) (fx= depth 0))
                 (loop (fx1+ i) (fx1+ i) depth (cons (substring s start i) out) #f)]
                [else (loop (fx1+ i) start depth out #f)])))))
  (define $integer?
    (lambda (s)
      (let ([n (string-length s)])
        (and (> n 0)
             (let ([start (if (memv (string-ref s 0) '(#\+ #\-)) 1 0)])
               (and (< start n)
                    (let loop ([i start])
                      (or (= i n)
                          (and (char<=? #\0 (string-ref s i) #\9)
                               (loop (+ i 1)))))))))))
  (define $contains-dots?
    (lambda (s)
      (let loop ([i 0])
        (and (< (+ i 1) (string-length s))
             (or (and (char=? (string-ref s i) #\.)
                      (char=? (string-ref s (+ i 1)) #\.))
                 (loop (+ i 1)))))))
  (define $range
    (lambda (s)
      (let ([dots (let loop ([i 0] [out '()])
                    (if (fx= i (string-length s))
                        (reverse out)
                        (if (and (fx< (fx1+ i) (string-length s))
                                 (char=? (string-ref s i) #\.)
                                 (char=? (string-ref s (fx1+ i)) #\.))
                            (loop (fx+ i 2) (cons i out))
                            (loop (fx1+ i) out))))])
        (and (or (fx= (length dots) 1) (fx= (length dots) 2))
             (let* ([a (car dots)]
                    [b (if (fx= (length dots) 2) (cadr dots) (string-length s))]
                    [x (substring s 0 a)]
                    [y (substring s (fx+ a 2) b)]
                    [z (and (fx= (length dots) 2)
                            (substring s (fx+ b 2) (string-length s)))])
               (and ($integer? x) ($integer? y) (or (not z) ($integer? z))
                    (let* ([start (string->number x)] [end (string->number y)]
                           [step (if z (string->number z) (if (<= start end) 1 -1))])
                      (when (or (= step 0)
                                (and (< start end) (< step 0))
                                (and (> start end) (> step 0)))
                        ($error "invalid numeric range step"))
                      (let loop ([v start] [out '()])
                        (if (if (> step 0) (> v end) (< v end)) (reverse out)
                            (loop (+ v step) (cons (format "~a" v) out)))))))))))
  (define $expand-braces
    (lambda (s)
      (let ([open ($brace-open s)])
        (if (not open) (list s)
            (let* ([close ($brace-close s open)] [body (substring s (fx1+ open) close)]
                   [prefix (substring s 0 open)]
                   [suffix (substring s (fx1+ close) (string-length s))]
                   [range ($range body)]
                   [values (or range ($split body))])
              (when (and (not range) (= (length values) 1)
                         (not ($brace-open body)) ($contains-dots? body))
                ($error "malformed numeric range"))
              (when (and (not range) (= (length values) 1)
                         (not ($brace-open body)))
                ($error "brace requires alternatives or a numeric range"))
              (when (exists (lambda (x) (string=? x "")) values)
                ($error "empty brace alternative"))
              (apply append (map (lambda (x) ($expand-braces (string-append prefix x suffix))) values)))))))
  ;; A component is compiled to SRE atoms; the regex engine never sees a path.
  (define $class
    (lambda (s start)
      (let* ([n (string-length s)]
             [negative? (and (< start n) (memv (string-ref s start) '(#\! #\^)))]
             [start (if negative? (+ start 1) start)])
        (let scan ([i start] [members '()])
          (cond [(= i n) ($error "unterminated bracket class")]
                [(and (char=? (string-ref s i) #\]) (pair? members))
                 (let ranges ([xs (reverse members)] [atoms '()])
                   (if (null? xs)
                       (values (+ i 1)
                               (let ([set (cons 'or (reverse atoms))])
                                 (if negative? (list '~ set) set)))
                       (if (and (pair? (cdr xs)) (pair? (cddr xs))
                                (eqv? (cadr xs) #\-))
                           (begin
                             (when (char>? (if (char? (car xs)) (car xs) (string-ref (car xs) 0))
                                           (if (char? (caddr xs)) (caddr xs) (string-ref (caddr xs) 0)))
                               ($error "descending bracket range"))
                             (ranges (cdddr xs)
                                     (cons (list '/ (if (char? (car xs)) (car xs) (string-ref (car xs) 0))
                                                      (if (char? (caddr xs)) (caddr xs) (string-ref (caddr xs) 0))) atoms)))
                           (ranges (cdr xs) (cons (car xs) atoms)))))]
                [(char=? (string-ref s i) #\\)
                 (if (< (+ i 1) n)
                     (scan (+ i 2) (cons (string (string-ref s (+ i 1))) members))
                     ($error "trailing escape in class"))]
                [else (scan (+ i 1) (cons (string-ref s i) members))])))))

  (define $component-sre
    (lambda (atoms)
      (let ([out (make-list-builder)])
        (let loop ([atoms atoms] [run '()])
          (cond [(null? atoms)
                 (unless (null? run) (out (apply string-append (reverse run))))
                 (cons 'seq (out))]
                [(string? (car atoms)) (loop (cdr atoms) (cons (car atoms) run))]
                [else
                 (unless (null? run) (out (apply string-append (reverse run))))
                 (out (car atoms)) (loop (cdr atoms) '())])))))

  (define $component-compile
    (lambda (flavor s)
      (if (string=? s "**") #f
          (let ([n (string-length s)])
            (let loop ([i 0] [atoms '()] [literal '()] [magic? #f])
              (if (= i n)
                  (%component (sre->regex ($component-sre (reverse atoms))
                                         (if (eq? flavor 'windows) '(i) '()))
                              (and (not magic?) (list->string (reverse literal))))
                  (let ([c (string-ref s i)])
                    (cond [(char=? c #\\)
                           (when (= (+ i 1) n) ($error "trailing escape"))
                           (let ([next (string-ref s (+ i 1))])
                             (loop (+ i 2) (cons (string next) atoms)
                                   (cons next literal) magic?))]
                          [(char=? c #\*)
                           (loop (+ i 1) (cons '(* any) atoms) literal #t)]
                          [(char=? c #\?)
                           (loop (+ i 1) (cons 'any atoms) literal #t)]
                          [(char=? c #\[)
                           (let-values ([(next atom) ($class s (+ i 1))])
                             (loop next (cons atom atoms) literal #t))]
                          [else (loop (+ i 1) (cons (string c) atoms)
                                      (cons c literal) magic?)]))))))))

  (define $compile
    (lambda (flavor s separator)
      (let* ([source
              (if (eq? flavor 'windows) s
                  (let ([n (string-length s)])
                    (let loop ([i 0] [chars '()])
                      (cond [(fx= i n) (list->string (reverse chars))]
                            [(and (char=? (string-ref s i) #\\)
                                  (fx< (fx1+ i) n)
                                  (char=? (string-ref s (fx1+ i)) #\/))
                             (loop (fx+ i 2) (cons #\/ chars))]
                            [else (loop (fx1+ i) (cons (string-ref s i) chars))]))))]
             [parsed (path-parse flavor source)])
        (%branch parsed (map (lambda (c) ($component-compile flavor c))
                             (path-components parsed))
                 separator
                 (and (> (string-length source) 0)
                      (let ([last (string-ref source (fx1- (string-length source)))])
                        (if (eq? flavor 'unix) (char=? last #\/)
                            (or (char=? last #\/) (char=? last #\\)))))))))

  (define $match?
    (lambda (components names)
      (let* ([ps (list->vector components)] [vs (list->vector names)]
             [pn (vector-length ps)] [vn (vector-length vs)]
             [memo (make-hashtable equal-hash equal?)])
        (let loop ([i 0] [j 0])
          (let* ([key (cons i j)] [saved (hashtable-ref memo key 'absent)])
            (if (not (eq? saved 'absent)) saved
                (let ([answer
                       (cond [(= i pn) (= j vn)]
                             [(not (vector-ref ps i))
                              (or (loop (+ i 1) j)
                                  (and (< j vn) (loop i (+ j 1))))]
                             [else
                              (and (< j vn)
                                   (regex-matches? (component-regex (vector-ref ps i))
                                                   (vector-ref vs j))
                                   (loop (+ i 1) (+ j 1)))])])
                  (hashtable-set! memo key answer)
                  answer)))))))

  (define $expand-tilde
    (lambda (flavor s)
      (let ([n (string-length s)])
        (cond [(or (string=? s "~")
                   (and (> n 1) (char=? (string-ref s 0) #\~)
                        (or (char=? (string-ref s 1) #\/)
                            (and (eq? flavor 'windows) (char=? (string-ref s 1) #\\)))))
               (let* ([separator (if (eq? flavor 'unix) #\/ #\\)]
                      [home (path-render (path-expand-user (path-parse flavor "~"))
                                         separator)])
                 (if (fx= n 1) home
                     (string-append home (string separator)
                                    (substring s 2 n))))]
              [(and (> n 0) (char=? (string-ref s 0) #\~))
               ($error "named-user tilde is unsupported")]
              [else s]))))

  (define $root=?
    (lambda (flavor a b)
      (if (eq? flavor 'unix) (equal? a b)
          (and (= (length a) (length b))
               (for-all (lambda (x y)
                          (if (and (string? x) (string? y)) (string-ci=? x y) (equal? x y)))
                        a b)))))

  #|proc:make-glob
  Compile string `pattern` and return an immutable, opaque glob object.
  Optional `flavor` follows the host OS: `'windows` on Windows, `'unix`
  elsewhere. Brace alternatives form an ordered union; numeric ranges and a leading
  current-user tilde expand before component compilation. Backslash escapes characters
  in Unix patterns; escaped `/` remains a separator. Windows treats both slash forms as
  separators, so bracket classes quote wildcard characters such as `[*]`. Malformed
  syntax raises an error.
  |#
  (define make-glob
    (case-lambda
      [(pattern) (make-glob $default-flavor pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               (let ([separator (if (or (eq? flavor 'unix)
                                        (exists (lambda (c) (char=? c #\/)) (string->list pattern)))
                                    #\/ #\\)]
                     [source (if (eq? flavor 'windows)
                                 (list->string (map (lambda (c) (if (char=? c #\\) #\/ c))
                                                    (string->list pattern)))
                                 pattern)])
                 (%make-glob flavor
                             (map (lambda (s) ($compile flavor ($expand-tilde flavor s) separator))
                                  ($expand-braces source)))))]))

  #|proc:glob?
  Return `#t` when arbitrary `object` is a compiled glob object, otherwise `#f`.
  |#
  (define glob? (lambda (object) (%glob? object)))

  (define $physical
    (lambda (p)
      (let ([s (path-render p #\/)])
        ;; Chez pathname expansion treats a leading `~` specially. A literal
        ;; escaped tilde must be made explicitly relative for filesystem calls.
        (cond [(string=? s "") "."]
              [(char=? (string-ref s 0) #\~) (string-append (current-directory) "/" s)]
              [else s]))))

  #|proc:$expansion-iterator
  Build the lazy traversal iterator shared by `glob`, `glob*`, and `glob->iter`.
  `compiled` supplies expanded branches; `follow-link?` controls symlink descent;
  `include-directories?` controls whether matching directories are yielded.

  The iterator's `stack` contains task vectors. A `node` task is
  `(kind logical-path physical-path remaining-components ancestors separator)`:
  `logical-path` is the spelling returned to the caller, `physical-path` is the
  case-correct path used for filesystem calls, `remaining-components` is the
  matcher suffix, `ancestors` records followed directory identities, and
  `separator` preserves the branch's slash style. A `scan` task replaces the
  physical path with an open directory stream and reads one entry per advance.
  Tasks are pushed and popped depth-first, so no complete subtree is collected.
  |#
  (define $expansion-iterator
    (lambda (compiled follow-link? include-directories?)
      (let ([branches (glob-patterns compiled)] [stack '()] [active-branch #f]
            [seen (make-hashtable string-hash string=?)] [finished? #f])
        (define close-all
          (lambda ()
            (let ([tasks stack] [failure #f])
              (set! stack '())
              (for-each (lambda (task)
                            (when (eq? (task-kind task) 'scan)
                            (guard (condition
                                    [else (unless failure (set! failure condition))])
                              (fs-close-directory (vector-ref task 5))))) tasks)
              (when failure (raise failure)))))
        ;; Tasks are six/seven-slot vectors kept private to this iterator. These
        ;; accessors make the two task layouts explicit at their use sites.
        (define task-kind (lambda (task) (vector-ref task 0)))
        (define task-logical (lambda (task) (vector-ref task 1)))
        (define task-physical (lambda (task) (vector-ref task 2)))
        (define task-components (lambda (task) (vector-ref task 3)))
        (define task-ancestors (lambda (task) (vector-ref task 4)))
        (define task-separator (lambda (task) (vector-ref task 5)))
        (define scan-directory (lambda (task) (vector-ref task 5)))
        (define scan-separator (lambda (task) (vector-ref task 6)))
        (define begin-branch
          (lambda (branch)
            (set! active-branch branch)
            (let* ([parsed (branch-path branch)]
                   [root (let loop ([p parsed] [cs (path-components parsed)])
                           (if (null? cs) (path-with-trailing-directory p #f)
                               (loop (path-drop-basename p) (cdr cs))))])
              ;; Keep logical and physical spellings separate. Literal components
              ;; are resolved lazily against the parent directory below.
              (set! stack (list (vector 'node root root
                                        (branch-components branch) '()
                                        (branch-separator branch)))))))
        (define $entry-match?
          (lambda (flavor wanted actual)
            (if (eq? flavor 'windows) (string-ci=? wanted actual)
                (string=? wanted actual))))
        (define find-literal-entry
          (lambda (flavor parent wanted)
            (if (not (file-directory? ($physical parent) follow-link?)) #f
                (let ([directory (fs-open-directory ($physical parent))]
                      [found #f])
                  (dynamic-wind
                    void
                    (lambda ()
                      (let loop ()
                        (unless found
                          (let ([entry (fs-read-directory directory)])
                            (when entry
                              (unless (member (car entry) '("." ".."))
                                (when ($entry-match? flavor wanted (car entry))
                                  (set! found entry)))
                              (unless found (loop)))))))
                    (lambda () (fs-close-directory directory)))
                  found))))
        (define identity
          (lambda (name)
            (list (file-dev-major name) (file-dev-minor name) (file-inode name))))
        (define open-scan
          (lambda (logical physical cs ancestors separator known-type)
            (let ([name ($physical physical)])
              (when (or (eq? known-type 'FT_dir)
                        (and (eq? known-type 'FT_symlink) follow-link?)
                        (and (not known-type) (file-directory? name follow-link?)))
                (let ([id (and follow-link? (identity name))])
                  (unless (and id (member id ancestors))
                    (let ([directory (fs-open-directory name)])
                      (set! stack
                            (cons (vector 'scan logical physical cs
                                          (if id (cons id ancestors) ancestors)
                                          directory separator) stack)))))))))
        (define eligible?
          (lambda (name)
            (and (file-exists? name #f)
                 (or include-directories? (file-symbolic-link? name)
                     (not (file-directory? name #f))))))
        (make-iter
         (lambda ()
           (guard (condition
                   [else (set! finished? #t)
                         (guard (cleanup-condition [else (void)]) (close-all))
                         (raise condition)])
             (let loop ()
               (cond [finished? iter-end]
                     [(null? stack)
                      (if (null? branches) (begin (set! finished? #t) iter-end)
                          (let ([branch (car branches)])
                            (set! branches (cdr branches)) (begin-branch branch) (loop)))]
                     [else
                        (let* ([task (car stack)] [kind (task-kind task)]
                               [p (task-logical task)] [physical (task-physical task)]
                               [cs (task-components task)]
                               [ancestors (task-ancestors task)])
                        (case kind
                          [(node)
                           (set! stack (cdr stack))
                           (let ([separator (task-separator task)] [name ($physical physical)])
                             (cond [(null? cs)
                                    (let* ([result-path
                                            (if (and (branch-directory? active-branch)
                                                     (pair? (path-components p)))
                                                (path-with-trailing-directory p #t) p)]
                                           [rendered (path-render result-path separator)]
                                           [result (if (string=? rendered "") "." rendered)])
                                      (if (and (eligible? name)
                                               (or (not (branch-directory? active-branch))
                                                   (file-directory? name follow-link?))
                                               (not (hashtable-contains? seen result)))
                                          (begin (hashtable-set! seen result #t) result) (loop)))]
                                   [(not (car cs))
                                    (open-scan p physical cs ancestors separator #f)
                                    (set! stack (cons (vector 'node p physical (cdr cs) ancestors separator) stack))
                                    (loop)]
                                   [(member (component-literal (car cs)) '("." ".."))
                                    (let* ([component (component-literal (car cs))]
                                           [logical-child (path-add-component p component)]
                                           [physical-child (path-add-component physical component)]
                                           [next (cdr cs)])
                                      (when (or (null? next)
                                                (file-directory? ($physical physical-child) follow-link?))
                                        (set! stack (cons (vector 'node logical-child physical-child
                                                                   next ancestors separator) stack))))
                                    (loop)]
                                   [(component-literal (car cs))
                                    (let ([entry (find-literal-entry (glob-flavor compiled)
                                                                      physical
                                                                      (component-literal (car cs)))])
                                      (when entry
                                        (let* ([actual (car entry)]
                                               [logical-child (path-add-component p (component-literal (car cs)))]
                                               [physical-child (path-add-component physical actual)]
                                               [next (cdr cs)]
                                               [child-name ($physical physical-child)])
                                          (when (or (null? next)
                                                    (or (eq? (cdr entry) 'FT_dir)
                                                        (and (eq? (cdr entry) 'FT_symlink) follow-link?)))
                                            (when (pair? next)
                                              (let ([probe (fs-open-directory child-name)])
                                                (fs-close-directory probe)))
                                            (set! stack (cons (vector 'node logical-child physical-child
                                                                       next ancestors separator) stack))))))
                                    (loop)]
                                   [else (open-scan p physical cs ancestors separator #f) (loop)]))]
                          [(scan)
                           (let* ([physical (task-physical task)]
                                  [cs (task-components task)]
                                  [directory (scan-directory task)]
                                  [separator (scan-separator task)]
                                  [entry (fs-read-directory directory)])
                             (cond [(not entry)
                                    (fs-close-directory directory)
                                    (set! stack (cdr stack)) (loop)]
                                   [(member (car entry) '("." "..")) (loop)]
                                   [(or (not (car cs))
                                        (regex-matches? (component-regex (car cs)) (car entry)))
                                    (set! stack
                                          (cons (vector 'node (path-add-component p (car entry))
                                                        (path-add-component physical (car entry))
                                                        (if (car cs) (cdr cs) cs) ancestors separator) stack))
                                    (loop)]
                                   [else (loop)]))]))]))))
         (lambda ()
           (close-all) (set! branches (glob-patterns compiled))
           (set! seen (make-hashtable string-hash string=?)) (set! finished? #f))
         (lambda () (close-all) (set! finished? #t))))))

  (define $path<?
    (lambda (flavor a b)
      (let loop ([xs (path-components (path-parse flavor a))]
                 [ys (path-components (path-parse flavor b))])
        (cond [(null? xs) (pair? ys)] [(null? ys) #f]
              [else
               (let ([x (if (eq? flavor 'windows) (string-foldcase (car xs)) (car xs))]
                     [y (if (eq? flavor 'windows) (string-foldcase (car ys)) (car ys))])
                 (if (string=? x y) (loop (cdr xs) (cdr ys)) (string<? x y)))]))))

  (define $glob-expand
    (lambda (flavor pattern follow-link? include-directories? unmatched)
      (let ([iterator ($expansion-iterator (make-glob flavor pattern) follow-link? include-directories?)]
            [results (make-list-builder)])
        (dynamic-wind
          void
          (lambda ()
            (let loop ([value (iter-next! iterator)])
              (unless (eq? value iter-end) (results value) (loop (iter-next! iterator)))))
          (lambda () (iter-finalize! iterator)))
        (let ([values (sort (lambda (a b) ($path<? flavor a b)) (results))])
          (if (and (null? values) (eq? unmatched 'literal)) (list pattern) values)))))

  #|proc:glob
  Expand string `pattern` against the filesystem and return duplicate-free path
  strings sorted by component order. Optional `flavor` follows the host OS:
  `'windows` on Windows, `'unix` elsewhere. Traversal starts at the longest literal
  prefix and preserves the root spelling. Symlinked directories are not descended into;
  matching directories are excluded. An unmatched pattern returns the empty list.
  |#
  (define glob
    (case-lambda
      [(pattern) (glob $default-flavor pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               ($glob-expand flavor pattern #f #f 'empty))]))

  #|proc:glob*
  Expand string `pattern` and return sorted, duplicate-free matching path
  strings. Optional `flavor` follows the host OS: `'windows` on Windows, `'unix`
  elsewhere.
  Boolean `follow-link?` permits descending into symlinked directories with
  cycle detection. Boolean `include-directories?` includes matching directories;
  files and symlinks are always eligible. `unmatched` is `'empty` to return `()`
  when nothing matches, or `'literal` to return a list of the original pattern.
  |#
  (define glob*
    (case-lambda
      [(pattern follow-link? include-directories? unmatched)
       (glob* $default-flavor pattern follow-link? include-directories? unmatched)]
      [(flavor pattern follow-link? include-directories? unmatched)
       (pcheck ([path-flavor? flavor] [string? pattern]
                [boolean? follow-link? include-directories?]
                [(lambda (x) (memq x '(empty literal))) unmatched])
               ($glob-expand flavor pattern follow-link? include-directories? unmatched))]))

  #|proc:glob->iter
  Expand string `pattern` lazily and return an iterator of matching path strings
  in filesystem order. Optional `flavor` follows the host OS: `'windows` on
  Windows, `'unix` elsewhere.
  The iterator uses the default `glob` policies and yields `iter-end` when
  exhausted. Streams open on advancement and close on exhaustion, reset,
  finalization, or error. `iter-reset!` begins a new pass over the current tree;
  `iter-finalize!` releases active streams without collecting the remaining tree.
  |#
  (define glob->iter
    (case-lambda
      [(pattern) (glob->iter $default-flavor pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               ($expansion-iterator (make-glob flavor pattern) #f #f))]))

  #|proc:glob-match?
  Return a boolean indicating whether string `path` matches any alternative of
  compiled `glob`, using that object's flavor. The three-argument form compiles
  string `pattern` for explicit `flavor` (`'unix` or `'windows`) before matching
  `path`. Matching is lexical, case-insensitive for Windows, and performs no
  filesystem access or dot-component normalization.
  |#
  (define glob-match?
    (case-lambda
      [(glob path)
       (pcheck ([glob? glob] [string? path])
               (let* ([p (path-parse (glob-flavor glob) path)] [root (path-root-info p)]
                      [values (path-components p)])
                 (exists (lambda (x) (and ($root=? (glob-flavor glob) root (path-root-info (branch-path x)))
                                         ($match? (branch-components x) values)))
                         (glob-patterns glob))))]
      [(flavor pattern path)
       (pcheck ([path-flavor? flavor] [string? pattern path])
               (glob-match? (make-glob flavor pattern) path))]))
)
