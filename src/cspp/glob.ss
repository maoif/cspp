#!chezscheme
(library (cspp glob)
  (export make-glob glob? glob-match? glob glob* glob->iter)
  (import (cspp chez)
          (cspp path)
          (cspp file)
          (cspp iter)
          (cspp regex)
          (only (cspp string) string-search string-map)
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
  An internal path-component matcher. `literal` holds original text without wildcard syntax;
  otherwise `regex` matches one complete component. The unused field is `#f`.
  |#
  (define-record-type ($component %component component?)
    (fields (immutable regex component-regex) (immutable literal component-literal)
            (immutable fold-case? component-fold-case?)))

  #|record:$branch
  An internal expanded pattern branch. `root` is a path object without components;
  `components` stores matchers; `separator` preserves output spelling.
  |#
  (define-record-type ($branch %branch branch?)
    (fields (immutable root branch-root)
            (immutable components branch-components)
            (immutable separator branch-separator) (immutable directory? branch-directory?)))

  ;; Path syntax follows the host OS when callers omit an explicit flavor.
  (define $default-flavor
    (if (eq? (system-platform) 'windows) 'windows 'unix))
  (define $error (lambda (message . args) (apply errorf 'make-glob message args)))

  ;; Fixed C/POSIX classes, independent of locale and Unicode category updates.
  ;; Use the existing regex SRE compiler for unions, ranges, and negation.
  (define $posix-classes
    '(("alnum" or (/ #\a #\z) (/ #\A #\Z) (/ #\0 #\9))
      ("alpha" or (/ #\a #\z) (/ #\A #\Z))
      ("blank" or #\space #\tab)
      ("cntrl" or (/ #\nul #\x1f) #\x7f)
      ("digit" / #\0 #\9)
      ("graph" / #\! #\~)
      ("lower" / #\a #\z)
      ("print" / #\space #\~)
      ("punct" or (/ #\! #\/) (/ #\: #\@) (/ #\[ #\`) (/ #\{ #\~))
      ("space" or #\space (/ #\tab #\return))
      ("upper" / #\A #\Z)
      ("xdigit" or (/ #\0 #\9) (/ #\a #\f) (/ #\A #\F))))

  (define $named-class-start?
    (lambda (s i)
      (and (fx< (fx1+ i) (string-length s))
           (char=? (string-ref s i) #\[)
           (char=? (string-ref s (fx1+ i)) #\:))))

  (define $named-class
    (lambda (s open)
      (let ([start (fx+ open 2)] [n (string-length s)])
        (let loop ([i start])
          (cond [(fx>= (fx1+ i) n) ($error "unterminated POSIX character class")]
                [(and (char=? (string-ref s i) #\:)
                      (char=? (string-ref s (fx1+ i)) #\]))
                 (let ([entry (assoc (substring s start i) $posix-classes)])
                   (unless entry ($error "unknown POSIX character class"))
                   (values (fx+ i 2) (cdr entry)))]
                [(char=? (string-ref s i) #\])
                 ($error "unterminated POSIX character class")]
                [else (loop (fx1+ i))])))))

  ;; The first member may be a literal closing bracket, after optional negation.
  ;; All brace scans share these class boundaries with the component grammar.
  (define $class-end
    (lambda (s open)
      (let* ([n (string-length s)] [start (fx1+ open)]
             [start (if (and (fx< start n) (memv (string-ref s start) '(#\! #\^)))
                        (fx1+ start) start)])
        (let loop ([i start])
          (cond [(fx= i n) ($error "unterminated bracket class")]
                [($named-class-start? s i)
                 (let-values ([(next atom) ($named-class s i)]) (loop next))]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2))
                     ($error "trailing escape in class"))]
                [(and (char=? (string-ref s i) #\]) (fx> i start)) (fx1+ i)]
                [else (loop (fx1+ i))])))))

  (define $brace-open
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0])
          (cond [(fx= i n) #f]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2))
                     ($error "trailing escape"))]
                [(char=? (string-ref s i) #\[) (loop ($class-end s i))]
                [(char=? (string-ref s i) #\}) ($error "unmatched closing brace")]
                [(char=? (string-ref s i) #\{) i]
                [else (loop (fx1+ i))])))))
  (define $brace-close
    (lambda (s open)
      (let ([n (string-length s)])
        (let loop ([i (fx1+ open)] [depth 1])
          (cond [(fx= i n) ($error "unmatched opening brace")]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) depth)
                     ($error "trailing escape"))]
                [(char=? (string-ref s i) #\[) (loop ($class-end s i) depth)]
                [(char=? (string-ref s i) #\{)
                 (loop (fx1+ i) (fx1+ depth))]
                [(char=? (string-ref s i) #\})
                 (if (fx= depth 1) i (loop (fx1+ i) (fx1- depth)))]
                [else (loop (fx1+ i) depth)])))))
  (define $split
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0] [start 0] [depth 0] [out '()])
          (cond [(fx= i n) (reverse (cons (substring s start n) out))]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) start depth out)
                     ($error "trailing escape in brace"))]
                [(char=? (string-ref s i) #\[) (loop ($class-end s i) start depth out)]
                [(char=? (string-ref s i) #\{)
                 (loop (fx1+ i) start (fx1+ depth) out)]
                [(char=? (string-ref s i) #\})
                 (loop (fx1+ i) start (fx1- depth) out)]
                [(and (char=? (string-ref s i) #\,) (fx= depth 0))
                 (loop (fx1+ i) (fx1+ i) depth (cons (substring s start i) out))]
                [else (loop (fx1+ i) start depth out)])))))
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

  (define $zero-padded?
    (lambda (text)
      (let ([start (if (memv (string-ref text 0) '(#\+ #\-)) 1 0)])
        (and (fx> (string-length text) (fx1+ start))
             (char=? (string-ref text start) #\0)))))

  (define $range-number
    (lambda (value width)
      (let* ([text (format "~a" value)] [length (string-length text)])
        (if (fx>= length width) text
            (let ([result (make-string width #\0)] [negative? (< value 0)])
              (when negative? (string-set! result 0 #\-))
              (let ([start (if negative? 1 0)])
                (string-copy! text start result (fx+ (fx- width length) start)
                              (fx- length start)))
              result)))))
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
                           [step (if z (string->number z) (if (<= start end) 1 -1))]
                           [width (if (or ($zero-padded? x) ($zero-padded? y))
                                      (fxmax (string-length x) (string-length y)) 0)])
                      (when (or (= step 0)
                                (and (< start end) (< step 0))
                                (and (> start end) (> step 0)))
                        ($error "invalid numeric range step"))
                      (let loop ([v start] [out '()])
                        (if (if (> step 0) (> v end) (< v end)) (reverse out)
                            (loop (+ v step) (cons ($range-number v width) out)))))))))))
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
                             (unless (and (or (char? (car xs)) (string? (car xs)))
                                          (or (char? (caddr xs)) (string? (caddr xs))))
                               ($error "POSIX character class cannot be a range endpoint"))
                             (when (char>? (if (char? (car xs)) (car xs) (string-ref (car xs) 0))
                                           (if (char? (caddr xs)) (caddr xs) (string-ref (caddr xs) 0)))
                               ($error "descending bracket range"))
                             (ranges (cdddr xs)
                                     (cons (list '/ (if (char? (car xs)) (car xs) (string-ref (car xs) 0))
                                                      (if (char? (caddr xs)) (caddr xs) (string-ref (caddr xs) 0))) atoms)))
                           (ranges (cdr xs) (cons (car xs) atoms)))))]
                [($named-class-start? s i)
                 (let-values ([(next atom) ($named-class s i)])
                   (scan next (cons atom members)))]
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
                 (unless (null? run) (out (list->string (reverse run))))
                 (cons 'seq (out))]
                [(char? (car atoms)) (loop (cdr atoms) (cons (car atoms) run))]
                [else
                 (unless (null? run) (out (list->string (reverse run))))
                 (out (car atoms)) (loop (cdr atoms) '())])))))

  (define $fold-string (lambda (text) (string-map char-foldcase text)))

  (define $fold-range
    (lambda (first last)
      ;; Folding a range's endpoints is insufficient: case mappings are not monotone.
      ;; Compress consecutive folded values into ranges as we enumerate Unicode scalars.
      (let ([out (make-list-builder)] [end (char->integer last)])
        (define emit
          (lambda (first last)
            (when first (out (integer->char first)) (out (integer->char last)))))
        (let loop ([index (char->integer first)] [first #f] [last #f])
          (cond [(fx> index end)
                 (emit first last)
                 ;; Folding can produce overlapping, out-of-order ranges. Reuse
                 ;; the regex charset union before passing ranges to its compiler.
                 (let ([ranges (regex-sre->cset (cons '/ (out)))]
                       [normalized (make-list-builder)])
                   (vector-for-each (lambda (range)
                                      (normalized (car range)) (normalized (cdr range))) ranges)
                   (cons '/ (normalized)))]
                [(fx<= #xd800 index #xdfff) (loop #xe000 first last)]
                [else
                 (let ([folded (char->integer (char-foldcase (integer->char index)))])
                   (if (and last (fx= folded (fx1+ last)))
                       (loop (fx1+ index) first folded)
                       (begin (emit first last) (loop (fx1+ index) folded folded))))])))))

  (define $fold-sre
    (lambda (sre)
      (cond [(string? sre) ($fold-string sre)]
            [(char? sre) (char-foldcase sre)]
            [(pair? sre)
             (if (eq? (car sre) '/)
                 ($fold-range (cadr sre) (caddr sre))
                 (cons (car sre) (map $fold-sre (cdr sre))))]
            [else sre])))

  (define $make-component
    (lambda (flavor sre literal)
      (let ([fold? (eq? flavor 'windows)])
        (%component (and (not literal) (sre->regex (if fold? ($fold-sre sre) sre)))
                    literal fold?))))

  (define $component-matches?
    (lambda (component name)
      (let ([literal (component-literal component)])
        (if literal
            (let ([length (string-length literal)]
                  [same-char? (if (component-fold-case? component) char-ci=? char=?)])
              (and (fx= length (string-length name))
                   (let loop ([index 0])
                     (or (fx= index length)
                         (and (same-char? (string-ref literal index) (string-ref name index))
                              (loop (fx1+ index)))))))
            (regex-matches? (component-regex component)
                            (if (component-fold-case? component) ($fold-string name) name))))))

  (define $component-compile
    (lambda (flavor s)
      (if (string=? s "**") #f
          (let ([n (string-length s)])
            (let loop ([i 0] [atoms '()] [magic? #f])
              (if (= i n)
                  (let ([atoms (reverse atoms)])
                    (if magic?
                        ($make-component flavor ($component-sre atoms) #f)
                        ($make-component flavor #f (list->string atoms))))
                  (let ([c (string-ref s i)])
                    (cond [(char=? c #\\)
                           (when (= (+ i 1) n) ($error "trailing escape"))
                           (let ([next (string-ref s (+ i 1))])
                             (loop (+ i 2) (cons next atoms) magic?))]
                          [(char=? c #\*)
                           (loop (+ i 1) (cons '(* any) atoms) #t)]
                          [(char=? c #\?)
                           (loop (+ i 1) (cons 'any atoms) #t)]
                          [(char=? c #\[)
                           (let-values ([(next atom) ($class s (+ i 1))])
                             (loop next (cons atom atoms) #t))]
                          [else (loop (+ i 1) (cons c atoms) magic?)]))))))))

  (define $normalize-unix-slashes
    (lambda (s)
      (if (not (string-search s "\\/")) s
          (let* ([n (string-length s)] [out (make-string n)])
            (let loop ([i 0] [j 0])
              (cond [(fx= i n) (substring out 0 j)]
                    [(and (char=? (string-ref s i) #\\)
                          (fx< (fx1+ i) n)
                          (char=? (string-ref s (fx1+ i)) #\/))
                     (string-set! out j #\/)
                     (loop (fx+ i 2) (fx1+ j))]
                    [(and (char=? (string-ref s i) #\\) (fx< (fx1+ i) n))
                     ;; Preserve an entire escape pair so its second backslash
                     ;; cannot be mistaken for a slash escape on the next pass.
                     (string-set! out j (string-ref s i))
                     (string-set! out (fx1+ j) (string-ref s (fx1+ i)))
                     (loop (fx+ i 2) (fx+ j 2))]
                    [else
                     (string-set! out j (string-ref s i))
                     (loop (fx1+ i) (fx1+ j))]))))))

  (define $normalize-windows-slashes
    (lambda (s)
      (let ([first (string-search s #\\)])
        (if (not first) s
            (let ([out (string-copy s)] [n (string-length s)])
              (do ([i first (fx1+ i)])
                  ((fx= i n) out)
                (when (char=? (string-ref out i) #\\)
                  (string-set! out i #\/))))))))

  (define $compile
    (lambda (flavor s separator)
      (let* ([source (if (eq? flavor 'windows) s ($normalize-unix-slashes s))]
             [home ($tilde-home flavor source)]
             [expanded (if home
                           (string-append (path-render home #\/)
                                          (substring source 1 (string-length source)))
                           source)]
             [parsed (path-parse flavor expanded)]
             [home-count (if home (length (path-components home)) 0)])
        (%branch ($path-root parsed)
                 (let loop ([components (path-components parsed)] [literal-count home-count])
                   (if (null? components) '()
                       (let ([component
                              (if (fx> literal-count 0)
                                  ($make-component flavor #f (car components))
                                  ($component-compile flavor (car components)))]
                             [rest (loop (cdr components) (fx1- literal-count))])
                         ;; Adjacent globstars describe the same recursive search.
                         (if (and (not component) (pair? rest) (not (car rest)))
                             rest
                             (cons component rest)))))
                 separator
                 (and (> (string-length expanded) 0)
                      (let ([last (string-ref expanded (fx1- (string-length expanded)))])
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
                                   ($component-matches? (vector-ref ps i) (vector-ref vs j))
                                   (loop (+ i 1) (+ j 1)))])])
                  (hashtable-set! memo key answer)
                  answer)))))))

  (define $path-root
    (lambda (parsed)
      ;; Reconstruct only the root; dropping every basename would copy lists quadratically.
      (let* ([root (path-root-info parsed)] [flavor (car root)])
        (case (cadr root)
          [(relative) (path flavor)]
          [(absolute rooted) (path-rooted flavor)]
          [(drive-relative) (path-drive (caddr root))]
          [(drive-absolute) (path-drive-absolute (caddr root))]
          [(unc) (path-unc (caddr root) (cadddr root))]
          [(special) (path-parse flavor (caddr root))]))))

  (define $trailing-separator?
    (lambda (flavor path)
      (let ([n (string-length path)])
        (and (> n 0)
             (let ([last (string-ref path (- n 1))])
               (if (eq? flavor 'unix)
                   (char=? last #\/)
                   (or (char=? last #\/) (char=? last #\\))))))))

  (define $tilde-home
    (lambda (flavor s)
      (let ([n (string-length s)])
        (cond [(or (string=? s "~")
                   (and (> n 1) (char=? (string-ref s 0) #\~)
                        (or (char=? (string-ref s 1) #\/)
                            (and (eq? flavor 'windows) (char=? (string-ref s 1) #\\)))))
               (path-expand-user (path-parse flavor "~"))]
              [(and (> n 0) (char=? (string-ref s 0) #\~))
               ($error "named-user tilde is unsupported")]
              [else #f]))))

  #|proc:make-glob
  Compile string `pattern` and return an immutable, opaque glob object.
  Optional `flavor` follows the host OS: `'windows` on Windows, `'unix`
  elsewhere. Brace alternatives form an ordered union; numeric ranges and a leading
  current-user tilde expand before component compilation. Home-directory names stay literal.
  Numeric ranges preserve zero padding when either endpoint has a leading zero. The width
  is the longer endpoint spelling, including any sign; nonnegative results omit `+`.
  Signed steps are supported and must move toward the endpoint.
  Bracket expressions support the C/POSIX ASCII classes `alnum`, `alpha`, `blank`, `cntrl`,
  `digit`, `graph`, `lower`, `print`, `punct`, `space`, `upper`, and `xdigit`, as in
  `[[:alpha:]]`. Classes can be mixed with literals, ranges, and other named classes.
  Backslash escapes characters
  in Unix patterns; escaped `/` remains a separator. Windows treats both slash forms as
  separators, so bracket classes quote wildcard characters such as `[*]`. Windows component
  matching uses Unicode simple case folding for literals, classes, and ranges, as in path equality.
  `*` and `?` match within one component; `**` matches zero or more directory levels.
  Dotfiles are eligible, and explicit `.` and `..` components retain their lexical meaning.
  Malformed syntax raises an error.
  |#
  (define make-glob
    (case-lambda
      [(pattern) (make-glob $default-flavor pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               (let ([separator (if (or (eq? flavor 'unix)
                                        (string-search pattern #\/))
                                    #\/ #\\)]
                     [source (if (eq? flavor 'windows)
                                 ($normalize-windows-slashes pattern) pattern)])
                 (%make-glob flavor
                             (map (lambda (s) ($compile flavor s separator))
                                  ($expand-braces source)))))]))

  #|proc:glob?
  Return `#t` when arbitrary `object` is a compiled glob object, otherwise `#f`.
  |#
  (define glob? (lambda (object) (%glob? object)))

  (define $filesystem-spelling
    (lambda (p separator)
      (let ([s (path-render p separator)])
        ;; Chez pathname expansion treats a leading `~` specially. A literal
        ;; escaped tilde must be made explicitly relative for filesystem calls.
        (cond [(string=? s "") "."]
              [(char=? (string-ref s 0) #\~) (string-append "." (string separator) s)]
              [else s]))))

  (define $physical (lambda (p) ($filesystem-spelling p #\/)))

  #|proc:$expansion-iterator
  Build the lazy traversal iterator shared by `glob`, `glob*`, and `glob->iter`.
  `compiled` supplies expanded branches; `follow-link?` controls symlink descent;
  `include-directories?` controls whether matching directories are yielded.

  The iterator's `stack` contains task vectors. A `node` task is
  `(kind path remaining-components ancestors separator)`: `path` uses the actual
  entry names for both results and filesystem calls, `remaining-components` is the
  matcher suffix, `ancestors` records followed directory identities, and
  `separator` preserves the branch's slash style. A `scan` task inserts an open
  directory stream before the separator and reads one entry per advance.
  Tasks are pushed and popped depth-first, so no complete subtree is collected.
  |#
  (define $expansion-iterator
    (lambda (compiled follow-link? include-directories?)
      (let ([branches (glob-patterns compiled)] [stack '()] [active-branch #f]
            [seen (make-hashtable string-hash string=?)] [states #f] [finished? #f])
        (define close-all
          (lambda ()
            (let ([tasks stack] [failure #f])
              (set! stack '())
              (for-each (lambda (task)
                          (when (eq? (task-kind task) 'scan)
                            (guard (condition
                                    [else (unless failure (set! failure condition))])
                              (fs-close-directory (scan-directory task))))) tasks)
              (when failure (raise failure)))))
        ;; Tasks are five/six-slot vectors kept private to this iterator. These
        ;; accessors make the two task layouts explicit at their use sites.
        (define task-kind (lambda (task) (vector-ref task 0)))
        (define task-path (lambda (task) (vector-ref task 1)))
        (define task-components (lambda (task) (vector-ref task 2)))
        (define task-ancestors (lambda (task) (vector-ref task 3)))
        (define task-separator (lambda (task) (vector-ref task 4)))
        (define scan-directory (lambda (task) (vector-ref task 4)))
        (define scan-separator (lambda (task) (vector-ref task 5)))
        (define begin-branch
          (lambda (branch)
            (set! active-branch branch)
            ;; Multiple recursive components can reach the same matcher suffix at
            ;; the same spelling many times. A single globstar cannot do so.
            (set! states (and (fx> (length (filter not (branch-components branch))) 1)
                               (make-eq-hashtable)))
            (set! stack (list (vector 'node (branch-root branch)
                                      (branch-components branch) '()
                                      (branch-separator branch))))))
        (define repeated-state?
          (lambda (path cs ancestors separator)
            (and states
                 (let* ([paths (or (hashtable-ref states cs #f)
                                   (let ([paths (make-hashtable string-hash string=?)])
                                     (hashtable-set! states cs paths)
                                     paths))]
                        [name (path-render path separator)]
                        [previous (hashtable-ref paths name '())]
                        [context (and follow-link? ancestors)])
                   ;; Preserve distinct spellings and cycle contexts; caching by
                   ;; inode would lose directory aliases and explicit dot paths.
                   (if (member context previous) #t
                       (begin (hashtable-set! paths name (cons context previous)) #f))))))
        (define identity
          (lambda (name)
            (let ([stat (file-stat name)])
              (list (file-stat-dev-major stat) (file-stat-dev-minor stat)
                    (file-stat-inode stat)))))
        (define open-scan
          (lambda (path cs ancestors separator)
            (let ([name ($physical path)])
              (if (not (file-directory? name follow-link?)) #t
                (let ([id (and follow-link? (identity name))]
                      [literal? (and (car cs) (component-literal (car cs)))])
                  ;; A finite literal prefix may explicitly revisit a directory.
                  ;; Its ancestors still constrain any later wildcard descent.
                  (if (and id (not literal?) (member id ancestors)) #f
                    (let ([directory (fs-open-directory name)])
                      (set! stack
                            (cons (vector 'scan path cs
                                          (if id (cons id ancestors) ancestors)
                                          directory separator) stack))
                      #t)))))))
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
                               [p (task-path task)]
                               [cs (task-components task)]
                               [ancestors (task-ancestors task)])
                        (case kind
                          [(node)
                           (set! stack (cdr stack))
                           (let ([separator (task-separator task)] [name ($physical p)])
                             (cond [(repeated-state? p cs ancestors separator) (loop)]
                                   [(null? cs)
                                    (let* ([result-path
                                            (if (and (branch-directory? active-branch)
                                                     (pair? (path-components p)))
                                                (path-with-trailing-directory p #t) p)]
                                           [result ($filesystem-spelling result-path separator)])
                                      (if (and (eligible? name)
                                               (or (not (branch-directory? active-branch))
                                                   (file-directory? name follow-link?))
                                               (not (hashtable-contains? seen result)))
                                          (begin
                                            ;; Caller-owned results may be mutated after yielding.
                                            (hashtable-set! seen (string-copy result) #t)
                                            result)
                                          (loop)))]
                                   [(not (car cs))
                                    ;; A recursive ancestor revisit must not consume zero
                                    ;; components and then enter a literal suffix either.
                                    (when (open-scan p cs ancestors separator)
                                      (set! stack
                                            (cons (vector 'node p (cdr cs) ancestors separator)
                                                  stack)))
                                    (loop)]
                                   [(member (component-literal (car cs)) '("." ".."))
                                    (let* ([component (component-literal (car cs))]
                                           [child (path-add-component p component)]
                                           [next (cdr cs)])
                                      ;; Dot traversal still descends through the parent.
                                      (when (and (file-directory? name follow-link?)
                                                 (or (null? next)
                                                     (file-directory? ($physical child)
                                                                      follow-link?)))
                                        ;; An explicit parent consumes a pattern component,
                                        ;; so the old descent chain is no longer a cycle guard.
                                        ;; Subsequent recursive scans build a fresh chain.
                                        (set! stack (cons (vector 'node child
                                                                   next (if (string=? component "..")
                                                                            '() ancestors)
                                                                   separator) stack))))
                                    (loop)]
                                   [(and (eq? (glob-flavor compiled) 'unix)
                                         (component-literal (car cs)))
                                    ;; Unix literals need search permission, but no directory scan.
                                    (when (file-directory? name follow-link?)
                                      (get-mode (string-append name "/."))
                                      (let* ([child (path-add-component p (component-literal (car cs)))]
                                             [next (cdr cs)])
                                        (when (or (null? next)
                                                  (file-directory? ($physical child) follow-link?))
                                          (set! stack (cons (vector 'node child next
                                                                     (if (and follow-link? (pair? next))
                                                                         (cons (identity name) ancestors)
                                                                         ancestors)
                                                                     separator) stack)))))
                                    (loop)]
                                   ;; Windows literals scan too: all case-equivalent names are eligible.
                                   [else (open-scan p cs ancestors separator) (loop)]))]
                          [(scan)
                           (let* ([cs (task-components task)]
                                  [directory (scan-directory task)]
                                  [separator (scan-separator task)]
                                  [entry (fs-read-directory directory)])
                             (cond [(not entry)
                                    (fs-close-directory directory)
                                    (set! stack (cdr stack)) (loop)]
                                   [(member (car entry) '("." "..")) (loop)]
                                   ;; A Unix name containing backslash cannot be one Windows component.
                                   [(and (eq? (glob-flavor compiled) 'windows)
                                         (string-search (car entry) #\\)) (loop)]
                                   [(or (not (car cs))
                                        ($component-matches? (car cs) (car entry)))
                                    (set! stack
                                          (cons (vector 'node (path-add-component p (car entry))
                                                        (if (car cs) (cdr cs) cs) ancestors separator) stack))
                                    (loop)]
                                   [else (loop)]))]))]))))
         (lambda ()
           (close-all) (set! branches (glob-patterns compiled))
           (set! seen (make-hashtable string-hash string=?)) (set! states #f)
           (set! finished? #f))
         (lambda () (close-all) (set! finished? #t))))))

  (define $components<?
    (lambda (a b)
      (let loop ([xs a] [ys b])
        (cond [(null? xs) (pair? ys)] [(null? ys) #f]
              [else
               (if (string=? (car xs) (car ys))
                   (loop (cdr xs) (cdr ys))
                   (string<? (car xs) (car ys)))]))))
  (define $path-sort-key
    (lambda (flavor path)
      (let ([components (path-components (path-parse flavor path))])
        (if (eq? flavor 'windows)
            (map $fold-string components)
            components))))

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
        (let* ([entries (map (lambda (path) (cons ($path-sort-key flavor path) path))
                             (results))]
               [sorted (sort (lambda (a b)
                               (or ($components<? (car a) (car b))
                                   (and (equal? (car a) (car b))
                                        (string<? (cdr a) (cdr b))))) entries)]
               [values (map cdr sorted)])
          (if (and (null? values) (eq? unmatched 'literal)) (list pattern) values)))))

  #|proc:glob
  Expand string `pattern` against the filesystem and return duplicate-free path
  strings sorted by component order, using simple case folding for Windows sorting.
  Equal folded keys are ordered by the rendered strings. Results use on-disk entry names,
  including every case-equivalent Windows match, and retain the pattern's separator style.
  Literal leading tildes receive a `./` prefix (or `.\` with backslash separators),
  so returned paths can be passed to filesystem APIs without home expansion.
  Optional `flavor` follows the host OS:
  `'windows` on Windows, `'unix` elsewhere. Unix literals use direct lookups; Windows
  components scan case-insensitively. Root spelling is preserved. Symlinked directories
  are not descended into; matching directories are excluded. An unmatched pattern returns `()`.
  Windows expansion skips Unix entries containing backslash, which is a Windows separator.
  |#
  (define glob
    (case-lambda
      [(pattern) (glob $default-flavor pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               ($glob-expand flavor pattern #f #f 'empty))]))

  #|proc:glob*
  Expand string `pattern` and return sorted, duplicate-free matching path
  strings, using the same entry spelling and sorting as `glob`.
  Optional `flavor` follows the host OS: `'windows` on Windows, `'unix`
  elsewhere.
  Boolean `follow-link?` permits descending into symlinked directories with
  cycle detection that counts literal and wildcard directory ancestors. An explicit
  parent component starts a new descent chain. Boolean `include-directories?` includes directories;
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
  `path`. Matching is lexical, uses Unicode simple case folding for Windows, and performs no
  filesystem access or dot-component normalization. A trailing separator on either
  pattern or candidate requires a trailing separator on both.
  Simple folding is symmetric and preserves character count: `straße` differs from `STRASSE`.
  Bracket classes, including negated classes and ranges, use the same rule as literals.
  |#
  (define glob-match?
    (case-lambda
      [(glob path)
       (pcheck ([glob? glob] [string? path])
               (let* ([p (path-parse (glob-flavor glob) path)]
                      [values (path-components p)])
                 (exists (lambda (x) (and (path-same-root? (branch-root x) p)
                                         (eq? (branch-directory? x)
                                              ($trailing-separator? (glob-flavor glob) path))
                                         ($match? (branch-components x) values)))
                         (glob-patterns glob))))]
      [(flavor pattern path)
       (pcheck ([path-flavor? flavor] [string? pattern path])
               (glob-match? (make-glob flavor pattern) path))]))
)
