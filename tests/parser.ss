(import (cspp chez)
        (cspp file)
        (cspp list)
        (cspp string)
        (cspp system process)
        (cspp utils)
        (cspp parser combinator)
        (cspp parser wasm)
        (cspp parser scheme))

(include "parser-external-tools.ss")
(include "parser-test-support.ss")

(mat parser-external-tools

     (external-tool-available? "sh")

     (not (external-tool-available? "cspp-intentionally-missing-command"))

     )

(define runT
  (case-lambda
    [(p in)    (runT p in #f)]
    [(p in st) (run-textual-parser p in st)]))

(define runB
  (case-lambda
    [(p in)    (runB p in #f)]
    [(p in st) (run-binary-parser p in st)]))

(define u8vec bytevector)

(define-parser <defined-item>
  (parser-call <item> inp state lvl))

(declare-lazy-parser <recursive-as>)
(install-lazy-parser!
 <recursive-as>
 (</> (<as> '() <eof>)
      (<map> (lambda (value) (cons (car value) (cadr value)))
             (<~> (<char> #\a) <recursive-as>))))

(declare-lazy-parser <installable>)

(define-parser <installed-a>
  (values #t 'a inp))

(define-parser <installed-b>
  (values #t 'b inp))

(mat parser-definitions

     (equal? #\a (runT <defined-item> "a"))

     (equal? '(#\a #\a #\a) (runT <recursive-as> "aaa"))

     (parser? <installable>)

     ;; error: a declared lazy parser cannot run before its body is installed.
     (error? (runT <installable> ""))

     (begin
       (install-lazy-parser! <installable> <installed-a>)
       (eq? 'a (runT <installable> "")))

     (begin
       (install-lazy-parser! <installable> <installed-b>)
       (eq? 'b (runT <installable> "")))

     (let ()
       (declare-lazy-parser <scoped>)
       (install-lazy-parser! <scoped> (<result> 'outer))
       (and (eq? 'outer (runT <scoped> ""))
            (let ()
              (declare-lazy-parser <scoped>)
              (install-lazy-parser! <scoped> (<result> 'inner))
              (eq? 'inner (runT <scoped> "")))))

     ;; error: an ordinary parser cannot be an installation target.
     (error? (install-lazy-parser! <item> <installed-a>))

     ;; error: the installed source must be a parser.
     (error? (install-lazy-parser! <installable> 'not-a-parser))

     ;; error: define-parser requires an ordinary parser body.
     (error?
      (eval '(let () (define-parser invalid) #t)
            (environment '(cspp chez) '(cspp parser combinator))))

     )

(mat parser-types

     (and (parser? <item>)
          (parser? (<result> 'value))
          (parser? (<many> <item>)))

     ;; error: parser runners reject a raw three-argument procedure.
     (error? (runT (lambda (inp state lvl) (values #t 'raw inp)) ""))

     ;; error: parser combinators reject a raw three-argument procedure.
     (error? (<many> (lambda (inp state lvl) (values #t 'raw inp))))

     ;; error: parser-call type-checks its parser argument through pcheck.
     (guard (err [(and (who-condition? err)
                       (eq? 'pcheck (condition-who err))) #t]
                 [else #f])
       (parser-call 'not-a-parser #f #f #f)
       #f)

     )

(mat parser-consumers

     (with-parser-file #vu8(0 97 115 109 1 0 0 0)
       (lambda (path) (wasm-module? (parse-wasm-binary-module path))))

     )

(mat parser-contracts

     ;; error: <satisfy> requires a parser as its first parameter.
     (error? (<satisfy> 'not-a-parser char?))

     ;; error: <satisfy> requires a predicate procedure.
     (error? (<satisfy> <item> 'not-a-procedure))

     ;; error: <satisfy-char> requires a predicate procedure.
     (error? (<satisfy-char> 'not-a-procedure))

     ;; error: <one-of> requires a string.
     (error? (<one-of> 'not-a-string))

     ;; error: <none-of> requires a string.
     (error? (<none-of> 'not-a-string))

     ;; error: <optional> requires a parser.
     (error? (<optional> 'not-a-parser))

     ;; error: sequence aliases require parser arguments.
     (error? (~> <item> 'not-a-parser))

     ;; error: <as-integer> requires a parser.
     (error? (<as-integer> 'not-a-parser))

     ;; error: <bind> callbacks must return parsers.
     (error? (runT (<bind> <item> (lambda (value) 'not-a-parser)) "a"))

     ;; error: <bind-st> callbacks must return parsers.
     (error? (runT (<bind-st> <item>
                             (lambda (value state) 'not-a-parser))
                   "a"))

     )

(mat parser-errors

     ;; error: textual parser failure exposes string source and zero-based position fields.
     (let ([err (parser-error-from (lambda () (runT (<char> #\a) "xb")))])
       (and (parser-error? err)
            (eq? 'run-textual-parser (parser-error-who err))
            (eq? 'expected (parser-error-kind err))
            (equal? "<string>" (parser-error-source err))
            (= 0 (parser-error-offset err))
            (= 0 (parser-error-line err))
            (= 0 (parser-error-column err))
            (equal? '(#\a) (parser-error-expected err))
            (char=? #\x (parser-error-found err))
            (equal? "<string>:1:1: expected #\\a, got #\\x"
                    (parser-error->string err))))

     ;; error: newline tracking reports the failure line and column.
     (let ([err (parser-error-from
                 (lambda () (runT (<~> (<string> "aa\nbb\n") (<char> #\c))
                                  "aa\nbb\nx")))])
       (and (parser-error? err)
            (= 6 (parser-error-offset err))
            (= 2 (parser-error-line err))
            (= 0 (parser-error-column err))
            (equal? "<string>:3:1: expected #\\c, got #\\x"
                    (parser-error->string err))))

     ;; error: source-aware textual runner uses the supplied source.
     (let ([err (parser-error-from
                 (lambda () (run-textual-parser/source (<char> #\a) "z" "input.ss")))])
       (and (parser-error? err)
            (equal? "input.ss" (parser-error-source err))
            (equal? "input.ss:1:1: expected #\\a, got #\\z"
                    (parser-error->string err))))

     ;; error: textual file parser reports the source path.
     (with-parser-file
      (string->utf8 "z")
      (lambda (path)
        (let ([err (parser-error-from
                    (lambda () (parse-textual-file (<char> #\a) path)))])
          (and (parser-error? err)
               (equal? path (parser-error-source err))
               (equal? (format "~a:1:1: expected #\\a, got #\\z" path)
                       (parser-error->string err))))))

     ;; error: binary parser failure reports byte offset and found byte.
     (let ([err (parser-error-from
                 (lambda () (runB (<uimm8> #x0b) (u8vec #x20))))])
       (and (parser-error? err)
            (equal? "<bytevector>" (parser-error-source err))
            (= 0 (parser-error-offset err))
            (not (parser-error-line err))
            (not (parser-error-column err))
            (= #x0b (car (parser-error-expected err)))
            (= #x20 (parser-error-found err))
            (equal? "<bytevector>:byte 0: expected #x0b, got #x20"
                    (parser-error->string err))))

     ;; error: binary file parser reports the source path and byte offset.
     (with-parser-file
      (u8vec #x20)
      (lambda (path)
        (let ([err (parser-error-from
                    (lambda () (parse-binary-file (<uimm8> #x0b) path)))])
          (and (parser-error? err)
               (equal? path (parser-error-source err))
               (equal? (format "~a:byte 0: expected #x0b, got #x20" path)
                       (parser-error->string err))))))

     ;; error: choice merges expected alternatives at the farthest offset.
     (let ([err (parser-error-from
                 (lambda () (runT (</> (<string> "abx") (<string> "aby")) "abz")))])
       (and (parser-error? err)
            (= 2 (parser-error-offset err))
            (equal? '("abx" "aby") (parser-error-expected err))
            (= 2 (length (parser-error-causes err)))
            (equal? "<string>:1:3: expected \"abx\" or \"aby\", got #\\z"
                    (parser-error->string err))))

     ;; error: sequence propagates the child failure without nested failed prefixes.
     (let ([err (parser-error-from
                 (lambda () (runT (<~> (<char> #\a) (<char> #\b)) "ac")))])
       (and (parser-error? err)
            (not (string-contains? (parser-error->string err) "failed <~>"))
            (equal? "<string>:1:2: expected #\\b, got #\\c"
                    (parser-error->string err))))

     ;; error: <pos-at> recomputes text line and column for random access.
     (let ([err (parser-error-from
                 (lambda () (runT (<pos-at> 3 (<char> #\z)) "a\nbx")))])
       (and (parser-error? err)
            (= 3 (parser-error-offset err))
            (= 1 (parser-error-line err))
            (= 1 (parser-error-column err))
            (equal? "<string>:2:2: expected #\\z, got #\\x"
                    (parser-error->string err))))

     ;; error: <u8vec> short input is a parser error, not a generic API error.
     (let ([err (parser-error-from (lambda () (runB (<u8vec> 3) (u8vec 1 2))))])
       (and (parser-error? err)
            (eq? 'unexpected-eof (parser-error-kind err))
            (equal? "<bytevector>:byte 0: unexpected EOF while reading u8vec"
                    (parser-error->string err))))

     )


(mat common-prims

     (equal? #\a (runT (<result> #\a) "a"))
     (equal? #\a (runT (<result> #\a) ""))

     (equal? #\a (runT <item> "a"))
     (equal? #\a (runT <item> "ab"))
     ;; Error case: an item is required at EOF.
     (parser-error? (parser-error-from (lambda () (runT <item> ""))))

     ;; TODO <fail> <pos> <msg> ...

     )


(mat text-prims

     (equal? #\a (runT (<char> #\a) "a"))
     ;; Error case: the next character must match a.
     (parser-error? (parser-error-from (lambda () (runT (<char> #\a) "b"))))

     ;; Error case: a letter is required at EOF.
     (parser-error? (parser-error-from (lambda () (runT <letter> ""))))

     (equal? #\a (runT <letter> "a"))

     ;; Error case: a lowercase character is not uppercase.
     (parser-error? (parser-error-from (lambda () (runT <upper> "a"))))

     (equal? #\A (runT <upper> "A"))

     (equal? #\0 (runT <digit> "0"))
     ;; Error case: a letter is not an ASCII decimal digit.
     (parser-error? (parser-error-from (lambda () (runT <digit> "a"))))

     (begin (define 0-9  (iota 10))
            (define 0-7  (iota 8))
            (define 0-15 (iota 16))

            (define 0-9-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 48 58)))
            (define 0-7-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 48 56)))
            (define a-f-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 97 103)))
            (define A-F-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 65 71)))
            #t)

     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <digit> s)))
             0-9-str)
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <bindigit> s)))
             '("0" "1"))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <octdigit> s)))
             0-7-str)
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <hexdigit> s)))
             (append 0-9-str a-f-str A-F-str))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <lower-hexdigit> s)))
             (append 0-9-str a-f-str))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <upper-hexdigit> s)))
             (append 0-9-str A-F-str))

     (andmap (lambda (n s)
               (equal? n (runT <digit2> s)))
             '(0 1) '("0" "1"))
     (andmap (lambda (n s)
               (equal? n (runT <digit8> s)))
             0-7 0-7-str)
     (andmap (lambda (n s)
               (equal? n (runT <digit10> s)))
             0-9 0-9-str)
     (andmap (lambda (n s)
               (equal? n (runT <digit16> s)))
             (append 0-15 (nums 10 16)) (append 0-9-str a-f-str A-F-str))
     (andmap (lambda (n s)
               (equal? n (runT <lower-digit16> s)))
             0-15 (append 0-9-str a-f-str))
     (andmap (lambda (n s)
               (equal? n (runT <upper-digit16> s)))
             0-15 (append 0-9-str A-F-str))

     ;; Error case: 3 is not a binary digit.
     (parser-error? (parser-error-from (lambda () (runT <bindigit> "3"))))

     ;; Error case: 8 is not an octal digit.
     (parser-error? (parser-error-from (lambda () (runT <octdigit> "8"))))

     ;; Error case: g is not a hexadecimal digit.
     (parser-error? (parser-error-from (lambda () (runT <hexdigit> "g"))))

     ;; Error case: a is not an uppercase hexadecimal digit.
     (parser-error? (parser-error-from (lambda () (runT <upper-hexdigit> "a"))))

     ;; Error case: A is not a lowercase hexadecimal digit.
     (parser-error? (parser-error-from (lambda () (runT <lower-hexdigit> "A"))))

     ;; Error case: 3 is not a base-two digit.
     (parser-error? (parser-error-from (lambda () (runT <digit2> "3"))))

     ;; Error case: 8 is not a base-eight digit.
     (parser-error? (parser-error-from (lambda () (runT <digit8> "8"))))

     ;; Error case: a is not a decimal digit.
     (parser-error? (parser-error-from (lambda () (runT <digit10> "a"))))

     ;; Error case: a is not an uppercase base-sixteen digit.
     (parser-error? (parser-error-from (lambda () (runT <upper-digit16> "a"))))

     ;; Error case: A is not a lowercase base-sixteen digit.
     (parser-error? (parser-error-from (lambda () (runT <lower-digit16> "A"))))


     (equal? #\a (runT (<one-of> "abc") "a"))
     (equal? #\b (runT (<one-of> "abc") "b"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<one-of> "abc") "d"))))

     (equal? #\d (runT (<none-of> "abc") "d"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<none-of> "abc") "b"))))


     )


(mat bin-prims

     (= #xff (runB (<uimm8> #xff) (u8vec #xff)))
     (= #xfeef (runB (<uimm16> #xfeef) (u8vec #xef #xfe)))

     (= 1 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 1)))
     (= 2 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 2)))
     (= 3 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 3)))
     ;; Error case: none of the immediate-byte alternatives matches 4.
     (parser-error?
      (parser-error-from
       (lambda ()
         (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 4)))))

     (equal? '(1 64)
             (runB (</> (<~> (<uimm8> 1) <u8>)
                        (<~> (<uimm8> 2) <u8>)
                        (<~> (<uimm8> 3) <u8>))
                   (u8vec 1 64)))
     ;; error
     (parser-error?
      (parser-error-from
       (lambda ()
         (runB (</> (<~> (<uimm8> 1) <u8>)
                 (<~> (<uimm8> 2) <u8>)
                 (<~> (<uimm8> 3) <u8>))
            (u8vec 4 64)))))

     (equal? '(1 2 3)
             (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
                   (u8vec 1 2 3)))
     ;; error
     (parser-error?
      (parser-error-from
       (lambda ()
         (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
            (u8vec 1 3)))))

     ;; Error case: the first byte violates the immediate sequence.
     (parser-error?
      (parser-error-from
       (lambda ()
         (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
            (u8vec 2 2 3)))))

     (equal? '((3 33) (1 11) (2 22))
             (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                               (<~> (<uimm8> 2) <u8>)
                               (<~> (<uimm8> 3) <u8>))
                          3)
                   (u8vec 3 33 1 11 2 22)))
     (equal? '((3 33) (1 11))
             (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                               (<~> (<uimm8> 2) <u8>)
                               (<~> (<uimm8> 3) <u8>))
                          2)
                   (u8vec 3 33 1 11 2 22)))
     ;; error
     (parser-error?
      (parser-error-from
       (lambda ()
         (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                    (<~> (<uimm8> 2) <u8>)
                    (<~> (<uimm8> 3) <u8>))
               4)
        (u8vec 3 33 1 11 2 22)))))

     (equal? '(6 6 6 6)
             (runB (<bind> (<uimm8> 4)
                           (lambda (n)
                             (<rep> (<uimm8> 6) n)))
                   (u8vec 4 6 6 6 6)))

     ;; m:u8 n:u8 m*{n}
     (equal? '(1)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 1 1 1)))
     (equal? '(1 1 1)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 1 3 1 1 1)))
     (equal? '(42 42 42 42 42 42)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 42 6 42 42 42 42 42 42)))


     (= #xfeca (runB <u16>   (u8vec #xca #xfe)))
     (= #xfeca (runB <u16le> (u8vec #xca #xfe)))
     (= #xcafe (runB <u16be> (u8vec #xca #xfe)))

     (= #xbebafeca (runB <u32>   (u8vec #xca #xfe #xba #xbe)))
     (= #xbebafeca (runB <u32le> (u8vec #xca #xfe #xba #xbe)))
     (= #xcafebabe (runB <u32be> (u8vec #xca #xfe #xba #xbe)))

     (= #xefbeaddebebafeca (runB <u64>   (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))
     (= #xefbeaddebebafeca (runB <u64le> (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))
     (= #xcafebabedeadbeef (runB <u64be> (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))

     ;; TODO signed
     ;; TODO imm

     (begin
       ;; ChezScheme mat.ss
       (define *fuzz* 1e-6)
       (define fl~=
         (lambda (x y)
           (cond
            [(and (fl>= (flabs x) 2.0) (fl>= (flabs y) 2.0))
             (fl~= (fl/ x 2.0) (fl/ y 2.0))]
            [(and (fl< 0.0 (flabs x) 1.0) (fl< 0.0 (flabs y) 1.0))
             (fl~= (fl* x 2.0) (fl* y 2.0))]
            [else (let ([d (flabs (fl- x y))])
                    (or (fl<= d *fuzz*)
                        (begin (printf "fl~~=: ~s~%" d) #f)))])))
       #t)

     ;; https://www.h-schmidt.net/FloatConverter/IEEE754.html
;;;; f32
     (= +0.0   (runB <f32le> (u8vec 0 0 0 0)))
     (= -0.0   (runB <f32le> (u8vec 0 0 0 #x80)))
     (= +inf.0 (runB <f32le> (u8vec 0 0 #x80 #x7f)))
     (= -inf.0 (runB <f32le> (u8vec 0 0 #x80 #xff)))
     (nan?     (runB <f32le> (u8vec 0 0 #xc0 #x7f)))

     (fl~= 3.14  (runB <f32le> (u8vec #xc3 #xf5 #x48 #x40)))
     (fl~= 2.718 (runB <f32le> (u8vec #xb6 #xf3 #x2d #x40)))
     (fl~= 1.618 (runB <f32le> (u8vec #xa0 #x1a #xcf #x3f)))
     (fl~= 1.414 (runB <f32le> (u8vec #xf4 #xfd #xb4 #x3f)))  ;; sqrt(2)
     (fl~= 0.69  (runB <f32le> (u8vec #xd7 #xa3 #x30 #x3f)))  ;; ln(2)
     (fl~= 2.3   (runB <f32le> (u8vec #x33 #x33 #x13 #x40)))  ;; ln(10)
     ;; precise nums
     (= 0.125 (runB <f32le> (u8vec #x00 #x00 #x00 #x3e)))
     (= 0.5   (runB <f32le> (u8vec #x00 #x00 #x00 #x3f)))
     (= 0.75  (runB <f32le> (u8vec #x00 #x00 #x40 #x3f)))
     (= 1.0   (runB <f32le> (u8vec #x00 #x00 #x80 #x3f)))
     (= 2.25  (runB <f32le> (u8vec #x00 #x00 #x10 #x40)))
     (= 3.75  (runB <f32le> (u8vec #x00 #x00 #x70 #x40)))

     (= +0.0   (runB <f32be> (u8vec 0 0 0 0)))
     (= -0.0   (runB <f32be> (u8vec #x80 0 0 0)))
     (= +inf.0 (runB <f32be> (u8vec #x7f #x80 0 0)))
     (= -inf.0 (runB <f32be> (u8vec #xff #x80 0 0)))
     (nan?     (runB <f32be> (u8vec #x7f #xc0 0 0)))

     (fl~= 3.14  (runB <f32be> (u8vec #x40 #x48 #xf5 #xc3)))
     (fl~= 2.718 (runB <f32be> (u8vec #x40 #x2d #xf3 #xb6)))
     (fl~= 1.618 (runB <f32be> (u8vec #x3f #xcf #x1a #xa0)))
     (fl~= 1.414 (runB <f32be> (u8vec #x3f #xb4 #xfd #xf4)))  ;; sqrt(2)
     (fl~= 0.69  (runB <f32be> (u8vec #x3f #x30 #xa3 #xd7)))  ;; ln(2)
     (fl~= 2.3   (runB <f32be> (u8vec #x40 #x13 #x33 #x33)))  ;; ln(10)
     ;; precise nums
     (= 0.125 (runB <f32be> (u8vec #x3e #x00 #x00 #x00)))
     (= 0.5   (runB <f32be> (u8vec #x3f #x00 #x00 #x00)))
     (= 0.75  (runB <f32be> (u8vec #x3f #x40 #x00 #x00)))
     (= 1.0   (runB <f32be> (u8vec #x3f #x80 #x00 #x00)))
     (= 2.25  (runB <f32be> (u8vec #x40 #x10 #x00 #x00)))
     (= 3.75  (runB <f32be> (u8vec #x40 #x70 #x00 #x00)))

;;;; f64
     (= +0.0   (runB <f64le> (u8vec 0 0 0 0 0 0 0 0)))
     (= -0.0   (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)))
     (= +inf.0 (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x7F)))
     (= -inf.0 (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #xFF)))
     (nan?     (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x7F)))

     (= 3.14  (runB <f64le> (u8vec #x1F #x85 #xEB #x51 #xB8 #x1E #x09 #x40)))
     (= 2.718 (runB <f64le> (u8vec #x58 #x39 #xB4 #xC8 #x76 #xBE #x05 #x40)))
     (= 1.618 (runB <f64le> (u8vec #x17 #xD9 #xCE #xF7 #x53 #xE3 #xF9 #x3F)))
     (= 1.414 (runB <f64le> (u8vec #x39 #xB4 #xC8 #x76 #xBE #x9F #xF6 #x3F)))  ;; sqrt(2)
     (= 0.69  (runB <f64le> (u8vec #x14 #xAE #x47 #xE1 #x7A #x14 #xE6 #x3F)))  ;; ln(2)
     (= 2.3   (runB <f64le> (u8vec #x66 #x66 #x66 #x66 #x66 #x66 #x02 #x40)))  ;; ln(10)

     (= +0.0   (runB <f64be> (u8vec 0 0 0 0 0 0 0 0)))
     (= -0.0   (runB <f64be> (u8vec #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
     (= +inf.0 (runB <f64be> (u8vec #x7F #xF0 #x00 #x00 #x00 #x00 #x00 #x00)))
     (= -inf.0 (runB <f64be> (u8vec #xFF #xF0 #x00 #x00 #x00 #x00 #x00 #x00)))
     (nan?     (runB <f64be> (u8vec #x7F #xF8 #x00 #x00 #x00 #x00 #x00 #x00)))

     (= 3.14  (runB <f64be> (u8vec #x40 #x09 #x1E #xB8 #x51 #xEB #x85 #x1F)))
     (= 2.718 (runB <f64be> (u8vec #x40 #x05 #xBE #x76 #xC8 #xB4 #x39 #x58)))
     (= 1.618 (runB <f64be> (u8vec #x3F #xF9 #xE3 #x53 #xF7 #xCE #xD9 #x17)))
     (= 1.414 (runB <f64be> (u8vec #x3F #xF6 #x9F #xBE #x76 #xC8 #xB4 #x39)))  ;; sqrt(2)
     (= 0.69  (runB <f64be> (u8vec #x3F #xE6 #x14 #x7A #xE1 #x47 #xAE #x14)))  ;; ln(2)
     (= 2.3   (runB <f64be> (u8vec #x40 #x02 #x66 #x66 #x66 #x66 #x66 #x66)))  ;; ln(10)

;;;; f32imm
     (bool (runB (<fimm32le> 0.125) (u8vec #x00 #x00 #x00 #x3e)))
     (bool (runB (<fimm32le> 0.5)   (u8vec #x00 #x00 #x00 #x3f)))
     (bool (runB (<fimm32le> 0.75)  (u8vec #x00 #x00 #x40 #x3f)))
     (bool (runB (<fimm32le> 1.0)   (u8vec #x00 #x00 #x80 #x3f)))
     (bool (runB (<fimm32le> 2.25)  (u8vec #x00 #x00 #x10 #x40)))
     (bool (runB (<fimm32le> 3.75)  (u8vec #x00 #x00 #x70 #x40)))

     (bool (runB (<fimm32be> 0.125) (u8vec #x3e #x00 #x00 #x00)))
     (bool (runB (<fimm32be> 0.5)   (u8vec #x3f #x00 #x00 #x00)))
     (bool (runB (<fimm32be> 0.75)  (u8vec #x3f #x40 #x00 #x00)))
     (bool (runB (<fimm32be> 1.0)   (u8vec #x3f #x80 #x00 #x00)))
     (bool (runB (<fimm32be> 2.25)  (u8vec #x40 #x10 #x00 #x00)))
     (bool (runB (<fimm32be> 3.75)  (u8vec #x40 #x70 #x00 #x00)))

;;;; f64imm
     (bool (runB (<fimm64le> 3.14)  (u8vec #x1F #x85 #xEB #x51 #xB8 #x1E #x09 #x40)))
     (bool (runB (<fimm64le> 2.718) (u8vec #x58 #x39 #xB4 #xC8 #x76 #xBE #x05 #x40)))
     (bool (runB (<fimm64le> 1.618) (u8vec #x17 #xD9 #xCE #xF7 #x53 #xE3 #xF9 #x3F)))
     (bool (runB (<fimm64le> 1.414) (u8vec #x39 #xB4 #xC8 #x76 #xBE #x9F #xF6 #x3F)))  ;; sqrt(2)
     (bool (runB (<fimm64le> 0.69)  (u8vec #x14 #xAE #x47 #xE1 #x7A #x14 #xE6 #x3F)))  ;; ln(2)
     (bool (runB (<fimm64le> 2.3)   (u8vec #x66 #x66 #x66 #x66 #x66 #x66 #x02 #x40)))  ;; ln(10)

     (bool (runB (<fimm64be> 3.14)  (u8vec #x40 #x09 #x1E #xB8 #x51 #xEB #x85 #x1F)))
     (bool (runB (<fimm64be> 2.718) (u8vec #x40 #x05 #xBE #x76 #xC8 #xB4 #x39 #x58)))
     (bool (runB (<fimm64be> 1.618) (u8vec #x3F #xF9 #xE3 #x53 #xF7 #xCE #xD9 #x17)))
     (bool (runB (<fimm64be> 1.414) (u8vec #x3F #xF6 #x9F #xBE #x76 #xC8 #xB4 #x39)))  ;; sqrt(2)
     (bool (runB (<fimm64be> 0.69)  (u8vec #x3F #xE6 #x14 #x7A #xE1 #x47 #xAE #x14)))  ;; ln(2)
     (bool (runB (<fimm64be> 2.3)   (u8vec #x40 #x02 #x66 #x66 #x66 #x66 #x66 #x66)))  ;; ln(10)

     ;; LEB128
     (begin (define encode-uleb128
              (lambda (n)
                (let loop ([res '()] [shift 0] [n n])
                  (let ([bits (logand n #x7f)]
                        [val  (ash n -7)])
                    (if (= val 0)
                        (reverse (cons (logbit0 7 bits) res))
                        (loop (cons (logbit1 7 bits) res) (fx+ shift 7) val))))))

            (define encode-sleb128
              (lambda (len n)
                (let loop ([res '()] [shift 0] [n n])
                  (let ([bits (logand n #x7f)]
                        [val  (ash n -7)])
                    (if (or (and (= val 0) (not (logbit? 6 bits)))
                            (and (= val -1) (logbit? 6 bits)))
                        (reverse (cons bits res))
                        (loop (cons (logbit1 7 bits) res)
                              (fx+ shift 7)
                              val))))))

            (define decode-uleb128
              (lambda (b*)
                (let loop ([b* b*] [shift 0] [n 0])
                  (let* ([b (car b*)]
                         [bits (fxlogand b #x7f)]
                         [cont (fxsrl (logand b #x80) 7)]
                         [n (+ n (ash bits shift))])
                    (if (fx= cont 0)
                        n
                        (loop (cdr b*) (fx+ shift 7) n))))))

            (define decode-sleb128
              (lambda (len b*)
                (let loop ([b* b*] [shift 0] [n 0])
                  (let* ([b (car b*)]
                         [bits (fxlogand b #x7f)]
                         [cont (fxsrl (logand b #x80) 7)]
                         [n (+ n (ash bits shift))])
                    (if (fx= cont 0)
                        (if (and (fx< shift len) (logbit? 6 b))
                            (logor n (ash -1 (fx+ 7 shift)))
                            n)
                        (loop (cdr b*) (fx+ shift 7) n))))))
            #t)

     (begin
       (define test-leb128
         (lambda (bits)
           (and (andmap (lambda (n)
                          (= n (decode-uleb128 (encode-uleb128 n))))
                        (random-list (lambda () (random (expt 2 bits))) 20 21))
                (andmap (lambda (n)
                          (= n (decode-sleb128 bits (encode-sleb128 bits n))))
                        (random-list (lambda () (random (expt 2 bits))) 20 21))
                (andmap (lambda (n)
                          (let ([n (- n)])
                            (= n (decode-sleb128 bits (encode-sleb128 bits n)))))
                        (random-list (lambda () (random (expt 2 bits))) 20 21)))))
       #t)

     (test-leb128 32)
     (test-leb128 64)
     (test-leb128 128)

     (begin
       (define test-<leb128>
         (lambda (bits)
           (and (let* ([n* (random-list (lambda () (random (expt 2 bits))) 20 21)]
                       [bv (apply bytevector
                                  (apply append
                                         (map (lambda (n) (encode-uleb128 n)) n*)))])
                  (equal? n* (runB (<rep> <uleb128> 20) bv)))
                (let* ([n* (random-list (lambda () (random (expt 2 (fx1- bits)))) 20 21)]
                       [bv (apply bytevector
                                  (apply append
                                         (map (lambda (n) (encode-sleb128 bits n)) n*)))])
                  (equal? n* (runB (<rep> (<sleb128> bits) 20) bv)))
                (let* ([n* (random-list (lambda () (- (random (expt 2 (fx1- bits))))) 20 21)]
                       [bv (apply bytevector
                                  (apply append
                                         (map (lambda (n) (encode-sleb128 bits n)) n*)))])
                  (equal? n* (runB (<rep> (<sleb128> bits) 20) bv))))))
       #t)

     (test-<leb128> 32)
     (test-<leb128> 64)
     (test-<leb128> 128)

     ;; u8*
     (equal? '() (runB (<u8*>) (u8vec 1 2 3)))
     (equal? '(1 2)
             (runB (<u8*> 1 2) (u8vec 1 2 3)))
     (equal? '(1 2 3)
             (runB (<u8*> 1 2 3) (u8vec 1 2 3)))
     ;; Error case: the byte sequence differs at its second byte.
     (parser-error? (parser-error-from (lambda () (runB (<u8*> 1 2 3) (u8vec 1 3 3)))))

     ;; Large 64-bit immediate values are valid even when they are not fixnums.
     (= #xffffffffffffffff
        (runB (<uimm64> #xffffffffffffffff)
              (u8vec #xff #xff #xff #xff #xff #xff #xff #xff)))

     ;; Inexact integral integer immediates are rejected before matching.
     (error? (runB (<uimm8> 1.0) (u8vec 1)))
     (error? (runB (<uimm64> 1.0) (u8vec 1 0 0 0 0 0 0 0)))

     ;; Float immediates require flonum arguments and support infinities.
     (error? (runB (<fimm32le> 1) (u8vec #x00 #x00 #x80 #x3f)))
     (bool (runB (<fimm32le> +inf.0) (u8vec #x00 #x00 #x80 #x7f)))
     (bool (runB (<fimm64be> -inf.0) (u8vec #xff #xf0 0 0 0 0 0 0)))

     ;; NaN float immediates are rejected because numeric equality cannot match NaN.
     (error? (runB (<fimm32le> +nan.0) (u8vec 0 0 0 0)))

     ;; TODO errors

     )


(mat parser-bounded-binary

     (equal? '(1 2)
             (runB (<~1> (<uimm8> 9)
                          (<bounded> 2 (<~> <u8> <u8>))
                          (<uimm8> 8)
                          <eof>)
                   (u8vec 9 1 2 8)))

     ;; error: the bounded parser must consume every declared byte.
     (parser-error? (parser-error-from (lambda () (runB (<bounded> 2 <u8>) (u8vec 1 2)))))

     ;; error: the declared range cannot extend beyond the binary input.
     (parser-error? (parser-error-from (lambda () (runB (<bounded> 3 (<rep> <u8> 3)) (u8vec 1 2)))))

     ;; error: failures inside a bounded parser retain the absolute byte offset.
     (let ([err (parser-error-from
                 (lambda ()
                   (runB (<~> (<uimm8> 9)
                              (<bounded> 2 (<~> (<uimm8> 1) (<uimm8> 2))))
                         (u8vec 9 1 3))))])
       (and (parser-error? err)
            (= 2 (parser-error-offset err))))

     )


(mat parser-combinators

     (equal? '()
             (runT (<many> (<char> #\a)) ""))
     (equal? '(#\a #\a #\a)
             (runT (<many> (<char> #\a)) "aaa"))
     (equal? '(#\a #\a #\a)
             (runT (<many> (<char> #\a)) "aaabbc"))

     (equal? '()
             (runT (<many> <item>) ""))
     (equal? '(#\a #\b #\c)
             (runT (<many> <item>) "abc"))

     (equal? '(#\a #\x)
             (runT (<~0> (<many-until> <item>
                                       (<~> (<char> #\a) (<char> #\b)))
                          (<char> #\a)
                          (<char> #\b)
                          <eof>)
                   "axab"))

     (equal? '()
             (runT (<~0> (<many-until> (<char> #\a) (<char> #\x3b))
                          (<char> #\x3b)
                          <eof>)
                   ";"))

     (equal? '(#\a #\a #\a)
             (runT (<~0> (<many-until> (<char> #\a) (<char> #\x3b))
                          (<char> #\x3b)
                          <eof>)
                   "aaa;"))

     (equal? '()
             (runB (<~0> (<many-until> <u8> (<uimm8> #xff))
                          (<uimm8> #xff)
                          <eof>)
                   (u8vec #xff)))

     (equal? '(1 2)
             (runB (<~0> (<many-until> <u8> (<uimm8> #xff))
                          (<uimm8> #xff)
                          <eof>)
                   (u8vec 1 2 #xff)))

     ;; error: a partially consumed item reports its own exact failure.
     (let ([err
            (parser-error-from
             (lambda ()
               (runT (<many-until> (<~> (<char> #\a) (<char> #\b))
                                   (<char> #\x3b))
                     "ax;")))])
       (and (parser-error? err)
            (eq? 'expected (parser-error-kind err))
            (= 1 (parser-error-offset err))
            (equal? '(#\b) (parser-error-expected err))
            (string=? "expected #\\b, got #\\x" (parser-error-message err))
            (char=? #\x (parser-error-found err))))

     ;; error: EOF without a terminator propagates the item parser's EOF failure.
     (let ([err
            (parser-error-from
             (lambda ()
               (runT (<many-until> (<char> #\a) (<char> #\x3b)) "aa")))])
       (and (parser-error? err)
            (eq? 'expected (parser-error-kind err))
            (= 2 (parser-error-offset err))
            (equal? '(#\a) (parser-error-expected err))
            (string=? "unexpected EOF, expected #\\a" (parser-error-message err))
            (eq? 'eof (parser-error-found err))))

     ;; error: a successful item parser must consume input.
     (error? (runT (<many-until> (<result> 'item) (<char> #\x3b)) "a"))

     ;; error: the repeated item must be a parser.
     (error? (<many-until> 'not-a-parser (<char> #\x3b)))

     ;; error: the terminator lookahead must be a parser.
     (error? (<many-until> (<char> #\a) 'not-a-parser))

     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<some> (<char> #\a)) ""))))

     (equal? '(#\a)
             (runT (<some> (<char> #\a)) "a"))
     (equal? '(#\a #\a #\a)
             (runT (<some> (<char> #\a)) "aaa"))
     (equal? '(#\a #\a #\a)
             (runT (<some> (<char> #\a)) "aaabbc"))

     ;; Error case: one or more items are required at EOF.
     (parser-error? (parser-error-from (lambda () (runT (<some> <item>) ""))))

     (equal? '(#\a #\b #\c)
             (runT (<some> <item>) "abc"))


     (equal? #\A (runT (</> <letter> <digit> <whitespace>) "A"))
     (equal? #\0 (runT (</> <letter> <digit> <whitespace>) "0"))
     (equal? #\space (runT (</> <letter> <digit> <whitespace>) " "))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (</> <letter> <digit> <whitespace>) "!"))))

     (equal? '(#\d #\e #\f #\1 #\2 #\3 #\a #\b #\c)
             (runT (<many> (</> <digit> <letter>)) "def123abc"))
     (equal? '(#\d #\e #\f #\1 #\2 #\3 #\a #\b #\c)
             (runT (<many> (</> <letter> <digit>)) "def123abc"))
     (equal? '((#\d #\e #\f) (#\1 #\2 #\3) (#\a #\b #\c))
             (runT (<many> (</> (<some> <digit>) (<some> <letter>))) "def123abc"))

     (equal? '(#\A #\9 #\space)
             (runT (<~> <letter> <digit> <whitespace>) "A9 "))
     ;; error
     (parser-error?
      (parser-error-from
       (lambda ()
         (runT (<~> <letter> <digit> <whitespace>) " A9 "))))

     (equal? '(#\space #\A #\9 #\space)
             (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
                   " A9 "))
     (equal? '(#\space #\A #\U #\space)
             (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
                   " AU "))
     ;; error
     (parser-error?
      (parser-error-from
       (lambda ()
         (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
               " Aa "))))

     (equal? '((#\a #\b #\c) (#\space #\space))
             (runT (<~> (<some> <letter>) (<many> <whitespace>)) "abc  "))

     (equal? '(#\( #\( #\() (runT (<rep> (<char> #\() 3) "((("))
     (equal? '(#\) #\) #\)) (runT (<rep> (<char> #\)) 3) ")))"))
     (equal? '(#\a #\a #\a) (runT (<rep> (<char> #\a) 3) "aaaa"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<rep> (<char> #\a) 3) "aa"))))

     (equal? #\a (runT (<~n> 0 <letter> <digit> <letter>) "a1c"))
     (equal? #\1 (runT (<~n> 1 <letter> <digit> <letter>) "a1c"))
     (equal? #\c (runT (<~n> 2 <letter> <digit> <letter>) "a1c"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<~n> 0 <letter> <digit> <letter>) "a12"))))

     ;; Error case: the indexed letter parser cannot accept a digit.
     (parser-error? (parser-error-from (lambda () (runT (<~n> 2 <letter> <digit> <letter>) "a12"))))

     (error? (runT (<~n> -1 <letter> <digit> <letter>) "a1c"))
     (error? (runT (<~n> 3 <letter> <digit> <letter>) "a1c"))

     (equal? #\c (runT (</> (<~n> 2 <letter> <digit> <letter>) (<~n> 2 <letter> <digit> <digit>)) "a1c"))
     (equal? #\2 (runT (</> (<~n> 2 <letter> <digit> <letter>) (<~n> 2 <letter> <digit> <digit>)) "a12"))

     ;; TODO <map>


     (begin (define <word>
              (<~> (<map>
                    (lambda (val) (apply string val))
                    (<some> <letter>))
                   (<many> <whitespace>)))
            (define <words>
              (<~> (<many> <whitespace>) (<many> <word>)))
            #t)

     (equal? '(() (("this" (#\space))
                   ("is" (#\space))
                   ("a" (#\space))
                   ("sentence" ())))
             (runT <words> "this is a sentence"))
     (equal? '(() (("this" (#\space))
                   ("is" (#\space))
                   ("a" (#\space))
                   ("sentence" (#\space #\space))))
             (runT <words> "this is a sentence  "))
     (equal? '((#\space #\space)
               (("this" (#\space))
                ("is" (#\space))
                ("a" (#\space))
                ("sentence" ())))
             (runT <words> "  this is a sentence"))
     (equal? '((#\space #\space)
               (("this" (#\space #\space))
                ("is" (#\space #\space #\space))
                ("a" (#\space #\space #\space))
                ("sentence" (#\space #\space #\space))))
             (runT <words> "  this  is   a   sentence   "))

     (begin (define <anbncn>
              (<bind> (<some> (<char> #\a))
                      (lambda (val)
                        (let ([count (length val)])
                          (<~> (<result> val)
                               (<rep> (<char> #\b) count)
                               (<rep> (<char> #\c) count))))))
            #t)
     (equal? '((#\a) (#\b) (#\c))
             (runT <anbncn> "abc"))
     (equal? '((#\a #\a) (#\b #\b) (#\c #\c))
             (runT <anbncn> "aabbcc"))
     (equal? '((#\a #\a #\a) (#\b #\b #\b) (#\c #\c #\c))
             (runT <anbncn> "aaabbbccc"))
     ;; still ok
     (equal? '((#\a) (#\b) (#\c))
             (runT <anbncn> "abcc"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT <anbncn> "aabc"))))

     ;; Error case: the grammar requires equal runs of a, b, and c.
     (parser-error? (parser-error-from (lambda () (runT <anbncn> "abbc"))))

     (begin (define (<onion> c)
              (<bind> (<some> (<char> #\())
                      (lambda (val)
                        (let ([count (length val)])
                          (<~n> 1
                                (<result> val)
                                (<char> c)
                                (<rep> (<char> #\)) count))))))
            #t)

     (equal? #\x1f496 (runT (<onion> #\x1f496) "(((\x1f496;)))"))
     (equal? #\a (runT (<onion> #\a) "(((a)))"))
     ;; still ok
     (equal? #\a (runT (<onion> #\a) "((a)))"))
     ;; error
     (parser-error? (parser-error-from (lambda () (runT (<~> (<onion> #\a) <eof>) "((a)))"))))

     ;; Error case: the nested form is missing a closing parenthesis.
     (parser-error? (parser-error-from (lambda () (runT (<onion> #\a) "(((a))"))))


     (begin (define <lines>
              (<map> (lambda (val)
                       (map (lambda (c*) (apply string c*)) val))
                     (<sep-by1> (<some> <letter>) (<char> #\newline))))
            #t)

     (equal? '("aa" "bb")
             (runT <lines> "aa\nbb"))
     (equal? '("aa" "bb")
             (runT <lines> "aa\nbb\n"))
     (equal? '("aa" "bb" "cc" "dd" "ff")
             (runT <lines> "aa\nbb\ncc\ndd\nff"))
     ;; error: no content
     (parser-error? (parser-error-from (lambda () (runT <lines> ""))))

     ;; Error case: a blank line cannot supply the required word.
     (parser-error? (parser-error-from (lambda () (runT <lines> "\n"))))

     ;; Error case: multiple blank lines cannot supply the required word.
     (parser-error? (parser-error-from (lambda () (runT <lines> "\n\n\n"))))

     ;; TODO states <map-st> <bind-st>

     ;; TODO [not]-followed-by

     ;; Empty string parser should succeed without advancing or raising EOF.
     (equal? "" (runT (<string> "") ""))
     (equal? '("" #\a) (runT (<~> (<string> "") <item>) "a"))

     ;; Empty choice should fail rather than act as a zero-width successful parser.
     (parser-error? (parser-error-from (lambda () (runT (</>) "abc"))))

     (begin (define arabic-indic-one (string (integer->char #x0661))) #t)

     ;; error: digit parser rejects non-ASCII numeric characters.
     (parser-error? (parser-error-from (lambda () (runT <digit> arabic-indic-one))))

     ;; error: numeric digit parser rejects non-ASCII numeric characters.
     (parser-error? (parser-error-from (lambda () (runT <digit10> arabic-indic-one))))

     ;; Repetition over zero-width success is an invalid parser shape.
     (error? (runT (<many> (<result> 1)) ""))
     (error? (runT (<some> (<result> 1)) ""))

     )

(include "parser-scheme-regressions.ss")

(mat parser-error-capture-contract

     (not (parser-error-from (lambda () (parse-scheme-datum "1"))))

     ;; Error case: malformed syntax returns its structured condition.
     (parser-error? (parser-error-from (lambda () (parse-scheme-datum "("))))

     ;; Error control: unrelated implementation conditions must propagate by identity.
     (let ([sentinel (make-assertion-violation)])
       (guard (condition [else (eq? condition sentinel)])
         (parser-error-from (lambda () (raise sentinel)))
         #f)))
