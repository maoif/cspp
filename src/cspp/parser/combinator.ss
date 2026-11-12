(library (cspp parser combinator)
  (export parser-error?
          parser-error-who parser-error-kind parser-error-message parser-error-source
          parser-error-offset parser-error-line parser-error-column parser-error-expected
          parser-error-found parser-error-context parser-error-causes parser-error->string
          parser? define-parser declare-lazy-parser install-lazy-parser!
          run-textual-parser run-binary-parser
          run-textual-parser/source run-binary-parser/source
          parse-textual-file parse-binary-file
          ;; TODO remove these
          parser-call input-pos input-pos-set! input-len save-input binary-input-data
          parser-discard-many parser-counted-vector
          bindigits->num octdigits->num digits->num hexdigits->num

          <fail> <fail-with> <eof> <result> <satisfy>
          <pos> <pos-at> <bounded> <msg-t> <msg-f>

          <satisfy-char>
          <item> <char> <string> <whitespace>
          <letter> <upper> <lower>
          <digit>
          <bindigit> <octdigit> <hexdigit> <lower-hexdigit> <upper-hexdigit>
          <digit2> <digit8> <digit10> <digit16> <lower-digit16> <upper-digit16>
          <one-of> <none-of>

          <u8> <u16> <u32> <u64>
          <s8> <s16> <s32> <s64>
          <f32> <f64>

          <u16le> <u32le> <u64le>
          <s16le> <s32le> <s64le>
          <f32le> <f64le>

          <u16be> <u32be> <u64be>
          <s16be> <s32be> <s64be>
          <f32be> <f64be>

          <uimm8> <uimm16> <uimm32> <uimm64>
          <simm8> <simm16> <simm32> <simm64>
          <fimm32> <fimm64>

          <uimm16le> <uimm32le> <uimm64le>
          <simm16le> <simm32le> <simm64le>
          <fimm32le> <fimm64le>

          <uimm16be> <uimm32be> <uimm64be>
          <simm16be> <simm32be> <simm64be>
          <fimm32be> <fimm64be>

          <u8*> <bytes> <u8vec>
          <uleb128> <sleb128>

          <many> <many-until> <some> <optional>
          <rep> <skip> <sep-by> <sep-by1>
          <~> <~n> <~ ~> <~0> <~1> <~2> <~3> <~4> <~5>
          </>
          <map> <map-st>
          <bind> <bind-st>
          <followed-by> <not-followed-by>
          <as> <as-string> <as-symbol> <as-integer>
          <token> <fully>)
  (import (cspp chez)
          (cspp internal)
          (cspp string)
          (cspp list)
          (cspp vector)
          (cspp io)
          (cspp file)
          (cspp parser private)
          (cspp utils))


#|
Independent parses may share a parser across threads. Each invocation owns its input cursor and
result builders. Callers must synchronize mutation of shared input, callback state, and values
returned by `<result>` or `<as>`. Complete lazy-parser installation before concurrent parsing;
synchronize any later replacement with all parses using that lazy parser.

Fiber safety is not guaranteed. Resuming a continuation captured in a parser callback has
undefined behavior. Backtracking and lookahead restore the input cursor only: mutations by
callbacks to caller-owned state are not rolled back. Callbacks may run in unsuccessful branches.

Text positions use zero-based character offsets, lines, and columns. CR, LF, U+2028, and U+2029
start a new line; CRLF counts as one line break, even across primitive parser boundaries.
Binary positions use zero-based byte offsets and have no line or column.

For simplicity, "PC" in the following documentation means "parser combinator".
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   infrastructure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|proc:parser-error?
  Returns #t when object has the parser-error condition type, and #f otherwise. The object
  parameter may be any Scheme value.
  |#
  #|proc:parser-error-who
  Returns the symbol naming the runner that raised err. The err parameter must satisfy
  parser-error?.
  |#
  #|proc:parser-error-kind
  Returns the failure-category symbol of err, such as expected, unexpected-eof, custom, or
  predicate. The err parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-message
  Returns the diagnostic message string of err, without a source/position prefix. The err
  parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-source
  Returns the source-label string of err; default labels are <string> and <bytevector>. The err
  parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-offset
  Returns the zero-based absolute character offset for text or byte offset for binary input in
  err. The err parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-line
  Returns the zero-based line number of textual err, or #f for a binary error. The err parameter
  must satisfy parser-error?.
  |#
  #|proc:parser-error-column
  Returns the zero-based character column of textual err, or #f for a binary error. The err
  parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-expected
  Returns the list of expected token descriptions of err, in first-occurrence order; it may be
  empty. The err parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-found
  Returns the found value in err, usually a character or byte. The symbol eof means end of input;
  #f means unspecified or an actual parsed #f. Predicate failures may report any parsed value.
  The err parameter must satisfy parser-error?.
  |#
  #|proc:parser-error-context
  Returns the list of parser-context labels of err; it may be empty. The err parameter must
  satisfy parser-error?.
  |#
  #|proc:parser-error-causes
  Returns the list of opaque internal failure records contributing to err; it may be empty.
  These records are not public parser-error conditions and cannot be inspected with these
  accessors. The err parameter must satisfy parser-error?.
  |#

  (define-condition-type &parser-error &error
    %make-parser-error
    parser-error?
    (who parser-error-who)
    (kind parser-error-kind)
    (message parser-error-message)
    (source parser-error-source)
    (offset parser-error-offset)
    (line parser-error-line)
    (column parser-error-column)
    (expected parser-error-expected)
    (found parser-error-found)
    (context parser-error-context)
    (causes parser-error-causes))

  (define-record-type parser-position
    (fields (immutable source)
            (immutable offset)
            (immutable line)
            (immutable column)
            (immutable kind)))

  (define-record-type parser-failure
    (fields (immutable kind)
            (immutable message)
            (immutable position)
            (immutable expected)
            (immutable found)
            (immutable context)
            (immutable causes)))

  ;; Keep failed alternatives as a tree; summarize their diagnostics only at the runner.
  (define-record-type parser-choice-failure
    (parent parser-failure)
    (fields))

  (define parser-failure-offset
    (lambda (failure)
      (parser-position-offset (parser-failure-position failure))))

  (define exact-natural?
    (lambda (value) (and (natural? value) (exact? value))))

  (define byte?
    (lambda (x)
      (and (integer? x) (exact? x) (<= 0 x #xff))))

  ;; Small failures avoid a hash table; wide choices switch to indexed deduplication.
  (define make-unique-builder
    (lambda ()
      (let ([seen #f] [count 0] [builder (make-list-builder)])
        (case-lambda
          [() (builder)]
          [(value)
           (unless (if seen
                       (hashtable-contains? seen value)
                       (member value (builder)))
             (builder value)
             (if seen
                 (hashtable-set! seen value #t)
                 (begin
                   (set! count (fx1+ count))
                   (when (fx= count 16)
                     (let ([table (make-hashtable equal-hash equal?)])
                       (for-each (lambda (item) (hashtable-set! table item #t)) (builder))
                       (set! seen table))))))]))))

  (define write-to-string
    (lambda (x)
      (let ([p (open-output-string)])
        (write x p)
        (let ([s (get-output-string p)])
          (close-output-port p)
          s))))

  (define format-byte
    (lambda (b)
      (let ([s (string-downcase (number->string b 16))])
        (if (< b 16)
            (format "#x0~a" s)
            (format "#x~a" s)))))

  (define format-parser-item
    (lambda (x)
      (cond [(char? x) (write-to-string x)]
            [(string? x) (write-to-string x)]
            [(eq? x 'eof) "EOF"]
            [(symbol? x) (symbol->string x)]
            [(byte? x) (format-byte x)]
            [else (write-to-string x)])))

  (define format-parser-item-list
    (lambda (x*)
      (cond [(null? x*) ""]
            [(null? (cdr x*)) (format-parser-item (car x*))]
            [(null? (cddr x*))
             (format "~a or ~a" (format-parser-item (car x*))
                     (format-parser-item (cadr x*)))]
            [else
             (call-with-port (open-output-string)
               (lambda (port)
                 (let loop ([items x*])
                   (if (null? (cdr items))
                       (begin (display "or " port)
                              (display (format-parser-item (car items)) port))
                       (begin (display (format-parser-item (car items)) port)
                              (display ", " port)
                              (loop (cdr items)))))
                 (get-output-string port)))])))

  (define parser-failure-message*
    (lambda (failure)
      (let ([message (parser-failure-message failure)]
            [expected (parser-failure-expected failure)]
            [found (parser-failure-found failure)])
        (cond [message message]
              [(and (pair? expected) found)
               (if (eq? found 'eof)
                   (format "unexpected EOF, expected ~a"
                           (format-parser-item-list expected))
                   (format "expected ~a, got ~a"
                           (format-parser-item-list expected)
                           (format-parser-item found)))]
              [(pair? expected)
               (format "expected ~a" (format-parser-item-list expected))]
              [found
               (if (eq? found 'eof)
                   "unexpected EOF"
                   (format "unexpected ~a" (format-parser-item found)))]
              [else "parser failed"]))))

  #|proc:parser-error->string
  The `parser-error->string` procedure formats parser error condition `err` as a
  message with source and position. Textual line/column displays are one-based; binary offsets
  stay zero-based. Source labels and custom messages are not escaped, so they can contain newlines.
  |#
  (define parser-error->string
    (lambda (err)
      (pcheck ([parser-error? err])
              (let ([source (parser-error-source err)]
                    [line (parser-error-line err)]
                    [column (parser-error-column err)]
                    [offset (parser-error-offset err)]
                    [message (parser-error-message err)])
                (if (and line column)
                    (format "~a:~a:~a: ~a" source (fx1+ line) (fx1+ column) message)
                    (format "~a:byte ~a: ~a" source offset message))))))

  (define parser-failure->condition
    (lambda (who failure)
      (let* ([failure (summarize-parser-failure (ensure-parser-failure failure #f))]
             [position (parser-failure-position failure)]
             [message (parser-failure-message* failure)])
        (%make-parser-error who
                            (parser-failure-kind failure)
                            message
                            (parser-position-source position)
                            (parser-position-offset position)
                            (parser-position-line position)
                            (parser-position-column position)
                            (parser-failure-expected failure)
                            (parser-failure-found failure)
                            (parser-failure-context failure)
                            (parser-failure-causes failure)))))

  (define raise-parser-error
    (lambda (who perr)
      (let ([err (parser-failure->condition who perr)])
        (raise (condition err
                          (make-who-condition who)
                          (make-message-condition (parser-error->string err)))))))

  (define parser-source?
    (lambda (source)
      (or (not source) (string? source))))


  (define $run-textual-parser
    (lambda (who p in source state)
      (pcheck ([parser? p] [string? in])
              (let ([inp (make-textual-input (string-length in) source 0 in 0 0)])
                (let-values ([(stt val inp/err) (parser-call p inp state 0)])
                  (if stt val
                      (raise-parser-error who (ensure-parser-failure inp/err inp))))))))


  (define $run-binary-parser
    (lambda (who p in source state)
      (pcheck ([parser? p] [bytevector? in])
              (let ([inp (make-binary-input (bytevector-length in) source 0 in 0)])
                (let-values ([(stt val inp/err) (parser-call p inp state 0)])
                  (if stt val
                      (raise-parser-error who (ensure-parser-failure inp/err inp))))))))

  #|proc:run-textual-parser/source
  The `run-textual-parser/source` procedure runs textual `parser` over string `input`.
  The `source` parameter is `#f` or a string used in displayed parser errors. The
  optional `state` parameter is passed through to state-aware parser combinators.
  Returns the parse value, or raises `&parser-error` on syntax failure. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who run-textual-parser/source
    (case-lambda
      [(parser input source) (run-textual-parser/source parser input source #f)]
      [(parser input source state)
       (pcheck ([parser? parser] [string? input] [parser-source? source])
               ($run-textual-parser who parser input (or source "<string>") state))]))

  #|proc:run-binary-parser/source
  The `run-binary-parser/source` procedure runs binary `parser` over bytevector `input`.
  The `source` parameter is `#f` or a string used in displayed parser errors. The
  optional `state` parameter is passed through to state-aware parser combinators.
  Returns the parse value, or raises `&parser-error` on syntax failure. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who run-binary-parser/source
    (case-lambda
      [(parser input source) (run-binary-parser/source parser input source #f)]
      [(parser input source state)
       (pcheck ([parser? parser] [bytevector? input] [parser-source? source])
               ($run-binary-parser who parser input
                                   (or source "<bytevector>") state))]))

  #|proc:run-textual-parser
  The `run-textual-parser` procedure runs textual `parser` over string `input` and
  returns the parse value. Optional `state` is passed to state-aware combinators and
  defaults to `#f`.
  If parsing fails, an error with condition type `&parser-error` is raised. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who run-textual-parser
    (case-lambda
      [(parser input) (run-textual-parser parser input #f)]
      [(parser input state)
       (pcheck ([parser? parser] [string? input])
               ($run-textual-parser who parser input "<string>" state))]))


  #|proc:run-binary-parser
  The `run-binary-parser` procedure runs binary `parser` over bytevector `input` and
  returns the parse value. Optional `state` is passed to state-aware combinators and
  defaults to `#f`.
  If parsing fails, an error with condition type `&parser-error` is raised. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who run-binary-parser
    (case-lambda
      [(parser input) (run-binary-parser parser input #f)]
      [(parser input state)
       (pcheck ([parser? parser] [bytevector? input])
               ($run-binary-parser who parser input "<bytevector>" state))]))


  #|proc:parse-textual-file
  The `parse-textual-file` procedure runs textual `parser` over the regular UTF-8 file at
  string `path` and returns the parse value. Malformed UTF-8 raises a decoding condition.
  Optional `state` is passed to state-aware
  combinators and defaults to `#f`.
  If parsing fails, an error with condition type `&parser-error` is raised. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who parse-textual-file
    (case-lambda
      [(parser path) (parse-textual-file parser path #f)]
      [(parser path state)
       (pcheck ([parser? parser] [file-regular? path])
               (let ([input (parser-read-text-file path)])
                 ($run-textual-parser who parser input path state)))]))


  #|proc:parse-binary-file
  The `parse-binary-file` procedure runs binary `parser` over the regular binary file at
  string `path` and returns the parse value. Optional `state` is passed to state-aware
  combinators and defaults to `#f`.
  If parsing fails, an error with condition type `&parser-error` is raised. Independent parses may
  share `parser` across threads. Fiber safety is not guaranteed; callback continuation resumption
  is undefined. Shared mutable input, state, and callback results require caller synchronization.
  Successful prefix parsing is allowed; the runner does not require EOF. Use an explicit <eof>
  sequence or the appropriate complete grammar when trailing input must be rejected.
  |#
  (define-who parse-binary-file
    (case-lambda
      [(parser path) (parse-binary-file parser path #f)]
      [(parser path state)
       (pcheck ([parser? parser] [file-regular? path])
               (let ([input (read-u8vec path)])
                 ($run-binary-parser who parser input path state)))]))


  #|proc:bindigits->num
  Converts proper list digits, in most-significant-first order, to an exact integer in radix 2.
  Each digit must be an exact integer from zero through 1. An empty list returns zero; invalid
  digits raise an argument error in list order.
  |#
  (define-who bindigits->num (make-parser-digit-converter who 2))
  #|proc:octdigits->num
  Converts proper list digits, in most-significant-first order, to an exact integer in radix 8.
  Each digit must be an exact integer from zero through 7. An empty list returns zero; invalid
  digits raise an argument error in list order.
  |#
  (define-who octdigits->num (make-parser-digit-converter who 8))
  #|proc:digits->num
  Converts proper list digits, in most-significant-first order, to an exact integer in radix 10.
  Each digit must be an exact integer from zero through 9. An empty list returns zero; invalid
  digits raise an argument error in list order.
  |#
  (define-who digits->num    (make-parser-digit-converter who 10))
  #|proc:hexdigits->num
  Converts proper list digits, in most-significant-first order, to an exact integer in radix 16.
  Each digit must be an exact integer from zero through 15. An empty list returns zero; invalid
  digits raise an argument error in list order.
  |#
  (define-who hexdigits->num (make-parser-digit-converter who 16))



  (define-record-type (parser make-parser $parser?)
    (fields (immutable body parser-body)))

  #|proc:parser?
  The `parser?` procedure returns `#t` when `value` is a parser record and `#f` otherwise.
  |#
  (define parser?
    (lambda (value)
      ($parser? value)))

  (define-record-type (lazy-parser make-lazy-parser lazy-parser?)
    (parent parser)
    (fields))

  #|macro:declare-lazy-parser
  The `declare-lazy-parser` macro defines `name` as a lazy parser whose body can be
  installed or replaced with `install-lazy-parser!`. Install before sharing the parser across
  threads; synchronize later replacements with all parses that use it.
  |#
  (define-syntax declare-lazy-parser
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define name
             (make-lazy-parser
              (let ([body (lambda (inp state lvl)
                            (errorf 'declare-lazy-parser
                                    "parser body not installed"))])
                (case-lambda
                  [(true-body) (set! body true-body)]
                  [(inp state lvl) (body inp state lvl)]))))])))

  #|proc:install-lazy-parser!
  The `install-lazy-parser!` procedure installs the body of `parser` in lazy parser
  `target` and returns unspecified values. A later call can replace the installed body. Complete
  installation before concurrent parsing, or synchronize replacement with all affected parses.
  |#
  (define-who (install-lazy-parser! target parser)
    (pcheck ([lazy-parser? target] [parser? parser])
            ((parser-body target) (parser-body parser))))

  #|macro:define-parser
  The `define-parser` macro defines parser `name` with body expression `body`. Within
  `body`, `inp`, `state`, and `lvl` are the current input, caller-supplied state, and nesting level.
  The body must return three values: `#t`, the parse value, and the resulting input on success;
  or `#f`, `#f`, and a failure on failure. Delegate to `parser-call` and existing combinators to
  preserve this protocol. Legacy string or `#f` failures use the available caller cursor;
  a private cursor discarded by the body cannot be recovered. Input cursors must belong to the
  invocation. Fiber safety is not guaranteed; callback continuation resumption is undefined.
  |#
  (define-syntax define-parser
    (lambda (stx)
      (syntax-case stx ()
        [(_ name body)
         (identifier? #'name)
         (with-syntax ([inp (datum->syntax #'name 'inp)]
                       [state (datum->syntax #'name 'state)]
                       [lvl (datum->syntax #'name 'lvl)])
           #'(define name
               (make-parser (lambda (inp state lvl) body))))])))

  #|macro:parser-call
  The `parser-call` macro invokes `parser` with its input cursor `input`, caller-supplied `state`,
  and fixnum nesting `level`. It returns the three values described by `define-parser`.
  |#
  (define-syntax parser-call
    (lambda (stx)
      (syntax-case stx ()
        [(k parser input state level)
         #'(let ([parser-value parser])
             (pcheck ([parser? parser-value])
                     ((parser-body parser-value) input state level)))])))
;;;; input logic

  #|proc:input-len
  Internal cursor accessor: returns the exclusive input limit of cursor inp as an exact integer.
  Units are characters for text and bytes for binary input; bounded binary limits are absolute.
  |#
  #|proc:input-pos
  Internal cursor accessor: returns cursor inp's zero-based absolute next-input position.
  Units are characters for text and bytes for binary input.
  |#
  #|proc:input-pos-set!
  Internal raw setter: changes cursor inp's absolute position to position and returns unspecified
  values. It does not update textual line/column tracking; use <pos-at> for textual random access.
  The caller is responsible for keeping position within the cursor's valid input bounds.
  |#
  (define-record-type input
    (fields (immutable len)
            (immutable source)
            ;; position of next symbol to read
            (mutable pos)))

  (define-record-type textual-input
    (parent input)
    (fields (immutable str) (mutable line) (mutable col)))

  #|proc:binary-input-data
  Internal cursor accessor: returns the backing bytevector of binary cursor inp without copying.
  A bounded cursor shares that bytevector; its legal positions still follow the cursor bounds.
  |#
  (define-record-type binary-input
    (parent input)
    ;; bytevector
    (fields (immutable data) (immutable start)))

  #|proc:save-input
  Internal cursor helper: returns a fresh snapshot of cursor inp's position and bounds, retaining
  the same source and input string/bytevector. Text snapshots also retain line/column state.
  Mutating the snapshot cursor does not move inp. Caller-owned data and callback state are shared.
  |#
  (define save-input
    (lambda (inp)
      (cond
       [(textual-input? inp)
        (make-textual-input
         (input-len inp)
         (input-source inp)
         (input-pos inp)
         (textual-input-str inp)
         (textual-input-line inp)
         (textual-input-col inp))]
       [(binary-input? inp)
        (make-binary-input
         (input-len inp)
         (input-source inp)
         (input-pos inp)
         (binary-input-data inp)
         (binary-input-start inp))]
       [else (assert-unreachable)])))

  ;; The index addresses the character being consumed in the original string. Looking
  ;; behind it makes CRLF handling independent of parser boundaries and backtracking.
  (define text-line-break?
    (lambda (text index character)
      (or (char=? character #\return)
          (char=? character #\x2028)
          (char=? character #\x2029)
          (and (char=? character #\newline)
               (or (fxzero? index)
                   (not (char=? (string-ref text (fx1- index)) #\return)))))))

  (define scan-text-position
    (lambda (str start target line col)
      (let loop ([i start] [line line] [col col])
        (if (fx= i target)
            (values line col)
            (let ([c (string-ref str i)])
              (cond [(text-line-break? str i c)
                     (loop (fx1+ i) (fx1+ line) 0)]
                    [(char=? c #\newline) (loop (fx1+ i) line 0)]
                    [else (loop (fx1+ i) line (fx1+ col))]))))))

  (define recompute-text-position
    (lambda (str target)
      (scan-text-position str 0 target 0 0)))

  (define-who set-input-position!
    (lambda (inp pos)
      (pcheck ([exact-natural? pos])
              (when (> pos (input-len inp))
                (errorf who "position ~a is past input length ~a" pos (input-len inp)))
              (cond
               [(textual-input? inp)
                (let* ([current-pos (input-pos inp)]
                       [str (textual-input-str inp)])
                  (let-values ([(line col)
                                (if (fx<= current-pos pos)
                                    (scan-text-position
                                     str current-pos pos
                                     (textual-input-line inp)
                                     (textual-input-col inp))
                                    (recompute-text-position str pos))])
                    (input-pos-set! inp pos)
                    (textual-input-line-set! inp line)
                    (textual-input-col-set! inp col)))]
               [(binary-input? inp)
                (input-pos-set! inp pos)]
               [else (assert-unreachable)]))))

  (define input-position-at
    (lambda (inp pos)
      (let ([inp1 (save-input inp)])
        (set-input-position! inp1 pos)
        (input->parser-position inp1))))

  (define input->parser-position
    (lambda (inp)
      (cond
       [(textual-input? inp)
        (make-parser-position (or (input-source inp) "<string>")
                              (input-pos inp)
                              (textual-input-line inp)
                              (textual-input-col inp)
                              'text)]
       [(binary-input? inp)
        (make-parser-position (or (input-source inp) "<bytevector>")
                              (input-pos inp)
                              #f
                              #f
                              'binary)]
       [else
        (make-parser-position "<unknown>" 0 #f #f 'binary)])))

  (define parser-failure-at
    (lambda (inp kind message expected found)
      (make-parser-failure kind message (input->parser-position inp)
                           expected found '() '())))

  (define parser-failure-expected-at
    (lambda (inp expected found)
      (parser-failure-at inp 'expected #f expected found)))

  (define parser-failure-eof
    (lambda (inp expected message)
      (parser-failure-at inp 'unexpected-eof message expected 'eof)))

  (define parser-failure-custom
    (lambda (inp message)
      (parser-failure-at inp 'custom message '() #f)))

  (define parser-failure-add-context
    (lambda (failure context)
      (let ([failure (ensure-parser-failure failure #f)])
        ((if (parser-choice-failure? failure) make-parser-choice-failure make-parser-failure)
                             (parser-failure-kind failure)
                             (parser-failure-message failure)
                             (parser-failure-position failure)
                             (parser-failure-expected failure)
                             (parser-failure-found failure)
                             (cons context (parser-failure-context failure))
                             (parser-failure-causes failure)))))

  (define parser-failure-merge-choice
    (lambda (failure*)
      (cond [(null? failure*)
             (make-parser-failure 'empty-choice "empty choice"
                                  (make-parser-position "<unknown>" 0 #f #f 'binary)
                                  '() #f '() '())]
            [(null? (cdr failure*))
             (parser-failure-add-context (car failure*) '</>)]
            [else
             (let ([first (car failure*)])
               (make-parser-choice-failure
                'expected #f (parser-failure-position first) '()
                (parser-failure-found first) '(</>) failure*))])))

  (define summarize-parser-failure
    (lambda (failure)
      (if (not (parser-choice-failure? failure))
          failure
          (let ([expected (make-unique-builder)] [messages (make-unique-builder)])
            ;; Each choice and leaf is visited once, in original alternative order.
            (let loop ([pending (list failure)])
              (unless (null? pending)
                (let ([current (car pending)])
                  (if (parser-choice-failure? current)
                      (loop (append (parser-failure-causes current) (cdr pending)))
                      (begin
                        (for-each expected (parser-failure-expected current))
                        (let ([message (parser-failure-message current)])
                          (when message (messages message)))
                        (loop (cdr pending)))))))
            (let ([messages (messages)])
              (make-parser-failure
               (parser-failure-kind failure)
               (and (pair? messages)
                    (apply string-append
                           (cons (car messages)
                                 (map (lambda (message) (string-append "; " message))
                                      (cdr messages)))))
               (parser-failure-position failure) (expected)
               (parser-failure-found failure) (parser-failure-context failure)
               (parser-failure-causes failure)))))))

  (define ensure-parser-failure
    (case-lambda
      [(err inp)
       (cond [(parser-failure? err) err]
             [(input? err)
              (parser-failure-custom err "parser failed")]
             [(string? err)
              (make-parser-failure 'custom err
                                   (if inp
                                       (input->parser-position inp)
                                       (make-parser-position "<unknown>" 0 #f #f 'binary))
                                   '() #f '() '())]
             [else
              (make-parser-failure 'custom (format "~a" err)
                                   (if inp
                                       (input->parser-position inp)
                                       (make-parser-position "<unknown>" 0 #f #f 'binary))
                                   '() #f '() '())])]
      [(err) (ensure-parser-failure err #f)]))

  (define current-found
    (lambda (inp)
      (if (end-of-input? inp)
          'eof
          (cond [(textual-input? inp)
                 (string-ref (textual-input-str inp) (input-pos inp))]
                [(binary-input? inp)
                 (bytevector-u8-ref (binary-input-data inp) (input-pos inp))]
                [else #f]))))

  (define failure-at-position
    (lambda (inp pos expected found)
      (make-parser-failure 'expected #f (input-position-at inp pos)
                           expected found '() '())))

  (define-who advance!
    (case-lambda
      [(inp) (advance! inp 1)]
      [(inp n)
       (pcheck ([exact-natural? n])
               (let ([len (input-len inp)] [pos (input-pos inp)])
                 (cond [(zero? n) (void)]
                       [(= len pos) (errorf who "already at eof")]
                       [else (input-pos-set! inp (min len (+ pos n)))])))]))

  (define update-line/col!
    (lambda (inp character)
      (let ([line (textual-input-line inp)]
            [column (textual-input-col inp)]
            [index (fx1- (input-pos inp))])
        (cond [(text-line-break? (textual-input-str inp) index character)
               (textual-input-line-set! inp (fx1+ line))
               (textual-input-col-set! inp 0)]
              [(char=? character #\newline) (textual-input-col-set! inp 0)]
              [else (textual-input-col-set! inp (fx1+ column))]))))

  (define end-of-input?
    (lambda (inp)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (= len pos))))

  ;; return the next available character in the input
  ;; and advance the position,
  ;; or eof
  (define get-next!
    (lambda (inp)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (= len pos)
            (eof-object)
            (let ([c (string-ref (textual-input-str inp) pos)])
              (advance! inp)
              (update-line/col! inp c)
              c)))))

  ;; check whether the next input character is `c`,
  ;; if so, advance the position by 1,
  ;; otherwise return #f
  (define peek-char!
    (lambda (inp c)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (< pos len)
            (let ([cc (string-ref (textual-input-str inp) pos)])
              (if (char=? cc c)
                  (begin (advance! inp) (update-line/col! inp cc) #t)
                  #f))
            #f))))

  ;; check whether the input contains string `str`,
  ;; if so, advance the position by the length of `str`,
  ;; otherwise return #f
  (define peek-string!
    (lambda (inp str)
      ;; check whether the substring starting at `i` in `str` is `substr`,
      ;; also maintain line and col
      (define str=?
        (lambda (str substr i line col)
          (let ([end (+ i (string-length substr))])
            (let loop ([i i] [j 0] [line line] [col col])
              (if (= i end)
                  (values #t line col)
                  (let ([c1 (string-ref str i)] [c2 (string-ref substr j)])
                    (if (char=? c1 c2)
                        (cond [(text-line-break? str i c1)
                               (loop (fx1+ i) (fx1+ j) (fx1+ line) 0)]
                              [(char=? c1 #\newline)
                               (loop (fx1+ i) (fx1+ j) line 0)]
                              [else (loop (fx1+ i) (fx1+ j) line (fx1+ col))])
                        (values #f #f #f))))))))
      (let ([len (input-len inp)] [pos (input-pos inp)] [strlen (string-length str)])
        (if (< (+ pos strlen -1) len)
            (let-values ([(?eq line col) (str=? (textual-input-str  inp) str pos
                                                (textual-input-line inp)
                                                (textual-input-col  inp))])
              (if ?eq
                  (begin (advance! inp strlen)
                         (textual-input-line-set! inp line)
                         (textual-input-col-set!  inp col)
                         #t)
                  #f))
            #f))))

  (define string-mismatch
    (lambda (inp str)
      (let ([len (input-len inp)]
            [pos (input-pos inp)]
            [strlen (string-length str)]
            [data (textual-input-str inp)])
        (let loop ([i 0])
          (cond [(fx= i strlen) (values pos #f)]
                [(>= (+ pos i) len) (values (+ pos i) 'eof)]
                [(char=? (string-ref data (+ pos i)) (string-ref str i))
                 (loop (fx1+ i))]
                [else (values (+ pos i) (string-ref data (+ pos i)))])))))


  ;; TODO report EOF in bin peeks

;;;; peek for given value
  (define-syntax gen-peek8!
    (syntax-rules ()
      [(_ name ref step)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos)])
                   (if (fx=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (gen-peek8! peek-u8! bytevector-u8-ref 1)
  (gen-peek8! peek-s8! bytevector-s8-ref 1)

  (define-syntax gen-peek!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (fx=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (define-syntax gen-peek/generic!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (= x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (define-syntax gen-fpeek!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (fl=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (gen-peek! peek-u16le! bytevector-u16-ref 2 (endianness little))
  (gen-peek! peek-u32le! bytevector-u32-ref 4 (endianness little))
  (gen-peek/generic! peek-u64le! bytevector-u64-ref 8 (endianness little))
  (gen-peek! peek-s16le! bytevector-s16-ref 2 (endianness little))
  (gen-peek! peek-s32le! bytevector-s32-ref 4 (endianness little))
  (gen-peek/generic! peek-s64le! bytevector-s64-ref 8 (endianness little))
  (gen-fpeek! peek-f32le! bytevector-ieee-single-ref 4 (endianness little))
  (gen-fpeek! peek-f64le! bytevector-ieee-double-ref 8 (endianness little))

  (gen-peek! peek-u16be! bytevector-u16-ref 2 (endianness big))
  (gen-peek! peek-u32be! bytevector-u32-ref 4 (endianness big))
  (gen-peek/generic! peek-u64be! bytevector-u64-ref 8 (endianness big))
  (gen-peek! peek-s16be! bytevector-s16-ref 2 (endianness big))
  (gen-peek! peek-s32be! bytevector-s32-ref 4 (endianness big))
  (gen-peek/generic! peek-s64be! bytevector-s64-ref 8 (endianness big))
  (gen-fpeek! peek-f32be! bytevector-ieee-single-ref 4 (endianness big))
  (gen-fpeek! peek-f64be! bytevector-ieee-double-ref 8 (endianness big))

;;;; peek for arbitrary value
  (define-syntax gen-peek8
    (syntax-rules ()
      [(_ name ref step)
       (define name
         (lambda (inp)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos)])
                   (advance! inp step)
                   y)
                 #f))))]))
  (gen-peek8 peek-u8 bytevector-u8-ref 1)
  (gen-peek8 peek-s8 bytevector-s8-ref 1)

  (define-syntax gen-peek
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (advance! inp step)
                   y)
                 #f))))]))
  (gen-peek peek-u16le bytevector-u16-ref 2 (endianness little))
  (gen-peek peek-u32le bytevector-u32-ref 4 (endianness little))
  (gen-peek peek-u64le bytevector-u64-ref 8 (endianness little))
  (gen-peek peek-s16le bytevector-s16-ref 2 (endianness little))
  (gen-peek peek-s32le bytevector-s32-ref 4 (endianness little))
  (gen-peek peek-s64le bytevector-s64-ref 8 (endianness little))
  (gen-peek peek-f32le bytevector-ieee-single-ref 4 (endianness little))
  (gen-peek peek-f64le bytevector-ieee-double-ref 8 (endianness little))

  (gen-peek peek-u16be bytevector-u16-ref 2 (endianness big))
  (gen-peek peek-u32be bytevector-u32-ref 4 (endianness big))
  (gen-peek peek-u64be bytevector-u64-ref 8 (endianness big))
  (gen-peek peek-s16be bytevector-s16-ref 2 (endianness big))
  (gen-peek peek-s32be bytevector-s32-ref 4 (endianness big))
  (gen-peek peek-s64be bytevector-s64-ref 8 (endianness big))
  (gen-peek peek-f32be bytevector-ieee-single-ref 4 (endianness big))
  (gen-peek peek-f64be bytevector-ieee-double-ref 8 (endianness big))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   common primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|proc:<result>
  The `<result>` procedure takes arbitrary `value` and returns a parser that always
  succeeds with `value` without consuming input.
  |#
  (define (<result> value)
    (define-parser result-parser
      (values #t value inp))
    result-parser)


  #|parser:<fail>
  `<fail>` is a parser that always fails when invoked.
  |#
  (define-who <fail>
    (let ()
      (define-parser fail-parser
        (values #f #f (parser-failure-custom inp (format "~a: failed" who))))
      fail-parser))


  #|proc:<fail-with>
  The `<fail-with>` procedure takes printable `message` and returns a parser that always
  fails with that message.
  |#
  (define-who (<fail-with> message)
    (define-parser fail-with-parser
      (values #f #f (parser-failure-custom inp (format "~a: ~a" who message))))
    fail-with-parser)


  #|parser:<eof>
  `<eof>` is a parser that tries to match the end of the input, be it textual or binary.
  |#
  (define-parser <eof>
    (let ([len (input-len inp)] [pos (input-pos inp)])
      (if (= len pos)
          (values #t #t inp)
          (values #f #f (parser-failure-expected-at inp '(eof) (current-found inp))))))


  #|proc:<satisfy>
  The `<satisfy>` procedure takes parser `parser`, predicate `predicate`, and optional
  string `message`. The `predicate` procedure must have signature `(Any -> Boolean)`.
  The returned parser succeeds with the parsed value when `parser` succeeds and
  `predicate` returns true. It fails otherwise.
  |#
  (define <satisfy>
    (case-lambda
      [(parser predicate)
       (<satisfy> parser predicate "failed predicate")]
      [(parser predicate message)
       (pcheck ([parser? parser] [procedure? predicate] [string? message])
               (define-parser satisfy-parser
                 (let ([start-input (save-input inp)])
                   (let-values ([(status value next-input)
                                 (parser-call parser inp state lvl)])
                     (if status
                         (if (predicate value)
                             (values #t value next-input)
                             (values #f #f
                                     (parser-failure-at start-input 'predicate message
                                                        '() value)))
                         (values #f #f (ensure-parser-failure next-input inp))))))
               satisfy-parser)]))


  #|parser:<pos>
  Return the current parse position in the input.
  |#
  (define-parser <pos>
    (values #t (input-pos inp) inp))


  #|proc:<pos-at>
  The `<pos-at>` procedure takes exact nonnegative integer `position` and `parser`. The returned
  parser runs `parser` at `position` and restores the original input position on success.
  It is an error when `position` is greater than the input length at parse time.
  |#
  (define-who (<pos-at> position parser)
    (pcheck ([exact-natural? position] [parser? parser])
            (define-parser pos-at-parser
              ;; TODO error report
              (if (or (> position (input-len inp))
                      (and (binary-input? inp) (< position (binary-input-start inp))))
                  (values #f #f
                          (parser-failure-custom
                           inp
                           (format "invalid position ~a, should be between ~a and ~a"
                                   position (if (binary-input? inp) (binary-input-start inp) 0)
                                   (input-len inp))))
                  (let ([new-inp (save-input inp)])
                (set-input-position! new-inp position)
                (let-values ([(stt val inp1)
                              (parser-call parser new-inp state (fx1+ lvl))])
                  (if stt
                      (values #t val inp)
                      (values #f #f (ensure-parser-failure inp1 new-inp)))))))
            pos-at-parser))


  #|proc:<bounded>
  The `<bounded>` procedure takes exact nonnegative integer `count` and binary `parser`. It returns
  a parser that runs `parser` over exactly the next `count` bytes.
  Random access must stay within that range.
  The returned parser fails if the range exceeds the input, if `parser` fails, or if `parser`
  does not consume the complete range.
  |#
  (define-who (<bounded> count parser)
    (pcheck ([exact-natural? count] [parser? parser])
            (define-parser bounded-parser
              (if (not (binary-input? inp))
                  (values #f #f
                          (parser-failure-custom inp "<bounded> requires binary input"))
                  (let* ([start (input-pos inp)]
                         [end (+ start count)])
                    (if (> end (input-len inp))
                        (values #f #f
                                (parser-failure-eof inp '(bounded-range)
                                                    "bounded range exceeds input"))
                        (let ([limited (make-binary-input
                                        end
                                        (input-source inp)
                                        start
                                        (binary-input-data inp) start)])
                          (let-values ([(status value next-input)
                                        (parser-call parser limited state (fx1+ lvl))])
                            (cond
                             [(not status)
                              (values #f #f (ensure-parser-failure next-input limited))]
                             [(not (= end (input-pos next-input)))
                              (values #f #f
                                      (parser-failure-custom
                                       next-input
                                       (format "bounded parser left ~a byte(s)"
                                               (- end (input-pos next-input)))))]
                             [else
                              (input-pos-set! inp end)
                              (values #t value inp)])))))))
            bounded-parser))


  #|proc:<msg-t>
  Takes printable message msg and optional printable label who, which defaults to <msg-t>.
  Returns a parser that prints the label, message, and current offset/input length, then
  succeeds with the formatted message string without consuming input. Repeating this parser
  alone with <many>, <some>, or <many-until> raises a progress error; it cannot supply input
  progress for an enclosing sequence.
  |#
  (define-who <msg-t>
    (case-lambda
      [(msg) (<msg-t> who msg)]
      [(who msg)
       (define-parser msg-t-parser
         (let ([msg (format "~a: ~a (~a/~a)" who msg (input-pos inp) (input-len inp))])
           (println msg)
           (values #t msg inp)))
       msg-t-parser]))


  #|proc:<msg-f>
  Takes printable message msg and optional printable label who, which defaults to <msg-f>.
  Returns a parser that prints the label, message, and current offset/input length, then fails
  with that formatted message without consuming input. This parser is intended for tracing
  failed grammar branches.
  |#
  (define-who <msg-f>
    (case-lambda
      [(msg) (<msg-f> who msg)]
      [(who msg)
       (define-parser msg-f-parser
         (let ([msg (format "~a: ~a (~a/~a)" who msg (input-pos inp) (input-len inp))])
           (println msg)
           (values #f msg (parser-failure-custom inp msg))))
       msg-f-parser]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   textual primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define ascii-digit?
    (lambda (c) (char<=? #\0 c #\9)))

  (define ascii-bindigit?
    (lambda (c) (or (char=? c #\0) (char=? c #\1))))

  (define ascii-octdigit?
    (lambda (c) (char<=? #\0 c #\7)))

  (define ascii-hexdigit?
    (lambda (c)
      (or (ascii-digit? c)
          (char<=? #\a c #\f)
          (char<=? #\A c #\F))))

  (define-who char->num
    (lambda (c)
      (if (ascii-digit? c)
          (fx- (char->integer c) 48)
          (errorf who "not a digit: ~a" c))))

  #|parser:<item>
  Parse and return the current character unconditionally.
  `<item>` fails only if EOF is reached.
  |#
  (define-parser <item>
    (if (end-of-input? inp)
        (values #f #f (parser-failure-eof inp '(item) #f))
        (values #t (get-next! inp) inp)))

  ;; Delay message construction until a predicate fails; live sets use their current value.
  (define character-parser
    (lambda (predicate message)
      (define-parser satisfy-character-parser
        (if (end-of-input? inp)
            (values #f #f (parser-failure-eof inp '(item) #f))
            ;; Keep the starting scalars; successful reads need no cursor snapshot.
            (let* ([start (input-pos inp)]
                   [line (textual-input-line inp)]
                   [column (textual-input-col inp)]
                   [character (get-next! inp)])
              (if (predicate character)
                  (values #t character inp)
                  (values #f #f
                          (make-parser-failure
                           'predicate (message)
                           (make-parser-position
                            (or (input-source inp) "<string>") start line column 'text)
                           '() character '() '()))))))
      satisfy-character-parser))

  #|proc:<satisfy-char>
  The `<satisfy-char>` procedure takes character predicate `predicate` and optional
  string `message`. The predicate must have signature `(Char -> Boolean)`. It returns a
  parser that succeeds with the current character when the predicate returns true.
  |#
  (define <satisfy-char>
    (case-lambda
      [(predicate) (<satisfy-char> predicate "failed predicate")]
      [(predicate message)
       (pcheck ([procedure? predicate] [string? message])
               (character-parser predicate (lambda () message)))]))

  #|proc:<char>
  The `<char>` procedure takes `character` and returns a textual parser. The parser
  matches and returns `character`, and fails when the current character differs.
  |#
  (define-who (<char> character)
    (pcheck ([char? character])
            (define-parser char-parser
              (if (peek-char! inp character)
                  (values #t character inp)
                  (values #f #f
                          (parser-failure-expected-at
                           inp (list character) (current-found inp)))))
            char-parser))

  #|proc:<string>
  The `<string>` procedure takes string `text` and returns a textual parser.
  The parser matches exactly `text` at the current input position and returns a fresh
  string containing the matched characters.
  |#
  (define-who (<string> text)
    (pcheck ([string? text])
            (define-parser string-parser
              (if (peek-string! inp text)
                  (values #t (string-copy text) inp)
                  (let-values ([(pos found) (string-mismatch inp text)])
                    (values #f #f
                            (failure-at-position inp pos (list text) found)))))
            string-parser))

  #|parser:<letter>
  Parse and return an arbitrary letter character.
  |#
  (define <letter>
    (<satisfy-char> char-alphabetic? "not a letter"))

  #|parser:<upper>
  Parse and return an arbitrary upper-case letter character.
  |#
  (define <upper>
    (<satisfy-char> char-upper-case? "not uppercase latter"))

  #|parser:<lower>
  Parse and return an arbitrary lower-case letter character.
  |#
  (define <lower>
    (<satisfy-char> char-lower-case? "not lowercase latter"))

  #|parser:<whitespace>
  Parse and return an arbitrary whitespace character.
  |#
  (define <whitespace>
    (<satisfy-char> char-whitespace? "not a whitespace"))

  #|parser:<digit>
  Parse a decimal digit and return the corresponding character.
  |#
  (define <digit>
    (<satisfy-char> ascii-digit? "not a digit"))

  #|parser:<bindigit>
  Parse a binary digit and return the corresponding character.
  |#
  (define <bindigit>
    (<satisfy-char> ascii-bindigit? "not a binary digit"))

  #|parser:<hexdigit>
  Parse a hex digit and return the corresponding character.
  |#
  (define <hexdigit>
    (<satisfy-char> ascii-hexdigit? "not a hexadecimal digit"))

  #|parser:<lower-hexdigit>
  Parse a lower hex digit and return the corresponding character.
  |#
  (define <lower-hexdigit>
    (<satisfy-char> (lambda (x) (or (ascii-digit? x)
                                    (char<=? #\a x #\f)))))

  #|parser:<upper-hexdigit>
  Parse an upper hex digit and return the corresponding character.
  |#
  (define <upper-hexdigit>
    (<satisfy-char> (lambda (x) (or (ascii-digit? x)
                                    (char<=? #\A x #\F)))))

  #|parser:<octdigit>
  Parse a octal digit and return the corresponding character.
  |#
  (define <octdigit>
    (<satisfy-char> ascii-octdigit? "not an octal digit"))

  ;; Compare directly against the live set string without allocating a search pattern.
  (define character-in-string?
    (lambda (character characters)
      (let loop ([index 0])
        (and (fx< index (string-length characters))
             (or (char=? character (string-ref characters index))
                 (loop (fx1+ index)))))))

  #|proc:<one-of>
  The `<one-of>` procedure takes string `characters` and returns a parser. The parser
  succeeds with the current character when it occurs in `characters` and fails otherwise.
  |#
  (define (<one-of> characters)
    (pcheck ([string? characters])
            (character-parser (lambda (character)
                                (character-in-string? character characters))
                              (lambda () (format "not one of \"~a\"" characters)))))

  #|proc:<none-of>
  The `<none-of>` procedure takes string `characters` and returns a parser. The parser
  succeeds with the current character when it does not occur in `characters`.
  |#
  (define (<none-of> characters)
    (pcheck ([string? characters])
            (character-parser (lambda (character)
                                (not (character-in-string? character characters)))
                              (lambda () (format "should not be one of \"~a\"" characters)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   binary primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define enough-bytes?
    (lambda (inp n)
      (<= (+ (input-pos inp) n) (input-len inp))))

  (define binary-short?
    (lambda (inp n)
      (not (enough-bytes? inp n))))


  (define-syntax gen-bin-prim
    (syntax-rules ()
      [(_ name peek expected step)
       (define-who name
         (let ()
           (define-parser binary-parser
             (let ([v (peek inp)])
               (if v
                   (values #t v inp)
                   (values #f #f
                           (if (binary-short? inp step)
                               (parser-failure-eof inp (list expected) #f)
                               (parser-failure-expected-at inp
                                                          (list expected)
                                                          (current-found inp)))))))
           binary-parser))]))
  (define-syntax gen-bin-imm-prim
    (syntax-rules ()
      [(_ name peek! valid-x? step)
       (define-who (name value)
         (pcheck ([valid-x? value])
                 (define-parser binary-immediate-parser
                   (let ([v (peek! inp value)])
                     (if v
                         (values #t value inp)
                         (values #f #f
                                 (if (binary-short? inp step)
                                     (parser-failure-eof inp (list value) #f)
                                     (parser-failure-expected-at
                                      inp (list value) (current-found inp)))))))
                 binary-immediate-parser))]))
  (define int-in-range?
    (lambda (x lo hi)
      (and (integer? x) (exact? x) (<= lo x hi))))
  (define u8?  (lambda (x) (int-in-range? x 0 (sub1 (expt 2 8)))))
  (define u16? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 16)))))
  (define u32? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 32)))))
  (define u64? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 64)))))
  (define s8?  (lambda (x) (int-in-range? x (- (expt 2 7))  (sub1 (expt 2 7)))))
  (define s16? (lambda (x) (int-in-range? x (- (expt 2 15)) (sub1 (expt 2 15)))))
  (define s32? (lambda (x) (int-in-range? x (- (expt 2 31)) (sub1 (expt 2 31)))))
  (define s64? (lambda (x) (int-in-range? x (- (expt 2 63)) (sub1 (expt 2 63)))))
  (define f32? (lambda (x) (and (flonum? x) (not (nan? x)))))
  (define f64? (lambda (x) (and (flonum? x) (not (nan? x)))))
  (define sleb128-len? (lambda (x) (and (fixnum? x) (fx> x 0))))
  (define all-u8? (lambda (x*) (andmap u8? x*)))
  (define all-parsers? (lambda (x*) (andmap parser? x*)))

  ;; TODO maybe merge the two gen macros
  ;; TODO rename these since they also change the inp state
  ;; try match and return an arbitrary value
  #|parser:<u8>
  The `<u8>` parser reads a 8-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u8>  peek-u8 'u8 1)
  #|parser:<u16>
  The `<u16>` parser reads a 16-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u16> peek-u16le 'u16 2)
  #|parser:<u32>
  The `<u32>` parser reads a 32-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u32> peek-u32le 'u32 4)
  #|parser:<u64>
  The `<u64>` parser reads a 64-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u64> peek-u64le 'u64 8)
  #|parser:<s8>
  The `<s8>` parser reads a 8-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s8>  peek-s8 's8 1)
  #|parser:<s16>
  The `<s16>` parser reads a 16-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s16> peek-s16le 's16 2)
  #|parser:<s32>
  The `<s32>` parser reads a 32-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s32> peek-s32le 's32 4)
  #|parser:<s64>
  The `<s64>` parser reads a 64-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s64> peek-s64le 's64 8)
  #|parser:<f32>
  The `<f32>` parser reads a 32-bit IEEE-754 floating-point number in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f32> peek-f32le 'f32 4)
  #|parser:<f64>
  The `<f64>` parser reads a 64-bit IEEE-754 floating-point number in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f64> peek-f64le 'f64 8)

  #|parser:<u16le>
  The `<u16le>` parser reads a 16-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u16le> peek-u16le 'u16le 2)
  #|parser:<u32le>
  The `<u32le>` parser reads a 32-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u32le> peek-u32le 'u32le 4)
  #|parser:<u64le>
  The `<u64le>` parser reads a 64-bit unsigned integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u64le> peek-u64le 'u64le 8)
  #|parser:<s16le>
  The `<s16le>` parser reads a 16-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s16le> peek-s16le 's16le 2)
  #|parser:<s32le>
  The `<s32le>` parser reads a 32-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s32le> peek-s32le 's32le 4)
  #|parser:<s64le>
  The `<s64le>` parser reads a 64-bit signed integer in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s64le> peek-s64le 's64le 8)
  #|parser:<f32le>
  The `<f32le>` parser reads a 32-bit IEEE-754 floating-point number in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f32le> peek-f32le 'f32le 4)
  #|parser:<f64le>
  The `<f64le>` parser reads a 64-bit IEEE-754 floating-point number in little-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f64le> peek-f64le 'f64le 8)

  #|parser:<u16be>
  The `<u16be>` parser reads a 16-bit unsigned integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u16be> peek-u16be 'u16be 2)
  #|parser:<u32be>
  The `<u32be>` parser reads a 32-bit unsigned integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u32be> peek-u32be 'u32be 4)
  #|parser:<u64be>
  The `<u64be>` parser reads a 64-bit unsigned integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <u64be> peek-u64be 'u64be 8)
  #|parser:<s16be>
  The `<s16be>` parser reads a 16-bit signed integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s16be> peek-s16be 's16be 2)
  #|parser:<s32be>
  The `<s32be>` parser reads a 32-bit signed integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s32be> peek-s32be 's32be 4)
  #|parser:<s64be>
  The `<s64be>` parser reads a 64-bit signed integer in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <s64be> peek-s64be 's64be 8)
  #|parser:<f32be>
  The `<f32be>` parser reads a 32-bit IEEE-754 floating-point number in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f32be> peek-f32be 'f32be 4)
  #|parser:<f64be>
  The `<f64be>` parser reads a 64-bit IEEE-754 floating-point number in big-endian byte order
  and returns its value. It fails with `&parser-error` through a runner on insufficient input.
  |#
  (gen-bin-prim <f64be> peek-f64be 'f64be 8)

  ;; try match a given immediate value
  #|proc:<uimm8>
  The `<uimm8>` procedure takes 8-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm8>  peek-u8!    u8? 1)
  #|proc:<uimm16>
  The `<uimm16>` procedure takes 16-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm16> peek-u16le! u16? 2)
  #|proc:<uimm32>
  The `<uimm32>` procedure takes 32-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm32> peek-u32le! u32? 4)
  #|proc:<uimm64>
  The `<uimm64>` procedure takes 64-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm64> peek-u64le! u64? 8)
  #|proc:<simm8>
  The `<simm8>` procedure takes 8-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm8>  peek-s8!    s8? 1)
  #|proc:<simm16>
  The `<simm16>` procedure takes 16-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm16> peek-s16le! s16? 2)
  #|proc:<simm32>
  The `<simm32>` procedure takes 32-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm32> peek-s32le! s32? 4)
  #|proc:<simm64>
  The `<simm64>` procedure takes 64-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm64> peek-s64le! s64? 8)
  #|proc:<fimm32>
  The `<fimm32>` procedure takes 32-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm32> peek-f32le! f32? 4)
  #|proc:<fimm64>
  The `<fimm64>` procedure takes 64-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm64> peek-f64le! f64? 8)

  #|proc:<uimm16le>
  The `<uimm16le>` procedure takes 16-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm16le> peek-u16le! u16? 2)
  #|proc:<uimm32le>
  The `<uimm32le>` procedure takes 32-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm32le> peek-u32le! u32? 4)
  #|proc:<uimm64le>
  The `<uimm64le>` procedure takes 64-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm64le> peek-u64le! u64? 8)
  #|proc:<simm16le>
  The `<simm16le>` procedure takes 16-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm16le> peek-s16le! s16? 2)
  #|proc:<simm32le>
  The `<simm32le>` procedure takes 32-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm32le> peek-s32le! s32? 4)
  #|proc:<simm64le>
  The `<simm64le>` procedure takes 64-bit signed integer `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm64le> peek-s64le! s64? 8)
  #|proc:<fimm32le>
  The `<fimm32le>` procedure takes 32-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm32le> peek-f32le! f32? 4)
  #|proc:<fimm64le>
  The `<fimm64le>` procedure takes 64-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in little-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm64le> peek-f64le! f64? 8)

  #|proc:<uimm16be>
  The `<uimm16be>` procedure takes 16-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm16be> peek-u16be! u16? 2)
  #|proc:<uimm32be>
  The `<uimm32be>` procedure takes 32-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm32be> peek-u32be! u32? 4)
  #|proc:<uimm64be>
  The `<uimm64be>` procedure takes 64-bit unsigned integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <uimm64be> peek-u64be! u64? 8)
  #|proc:<simm16be>
  The `<simm16be>` procedure takes 16-bit signed integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm16be> peek-s16be! s16? 2)
  #|proc:<simm32be>
  The `<simm32be>` procedure takes 32-bit signed integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm32be> peek-s32be! s32? 4)
  #|proc:<simm64be>
  The `<simm64be>` procedure takes 64-bit signed integer `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <simm64be> peek-s64be! s64? 8)
  #|proc:<fimm32be>
  The `<fimm32be>` procedure takes 32-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm32be> peek-f32be! f32? 4)
  #|proc:<fimm64be>
  The `<fimm64be>` procedure takes 64-bit flonum other than NaN `value`. It returns a binary parser
  that matches `value` in big-endian byte order and returns it, or fails on a mismatch or EOF.
  |#
  (gen-bin-imm-prim <fimm64be> peek-f64be! f64? 8)


  ;; return a copy of the list of bytes if successful
  (define ($<u8*> who)
    (lambda byte*
      (pcheck ([all-u8? byte*])
              (define-parser u8-list-parser
                (let loop ([u8* byte*])
                  (if (null? u8*)
                      (values #t (list-copy byte*) inp)
                      (if (peek-u8! inp (car u8*))
                          (loop (cdr u8*))
                          (let ([failure (parser-failure-expected-at
                                          inp
                                          (list (car u8*))
                                          (current-found inp))])
                            (values #f #f failure))))))
              u8-list-parser)))

  #|proc:<u8*>
  The `<u8*>` procedure takes `byte*`, each an integer from 0 to 255, and returns a
  binary parser. The parser matches exactly those bytes in order and returns a fresh
  list containing the matched bytes.
  |#
  (define-who <u8*> ($<u8*> who))

  #|proc:<bytes>
  The `<bytes>` procedure takes `byte*`, each an integer from 0 to 255, and
  returns a binary parser. The parser matches exactly those bytes in order and returns a
  fresh list containing the matched bytes.
  |#
  (define-who <bytes> ($<u8*> who))


  #|proc:<u8vec>
  The `<u8vec>` procedure takes exact nonnegative integer `count` and returns a binary parser.
  The parser consumes the next `count` bytes and returns them in a fresh bytevector.
  |#
  (define-who (<u8vec> count)
    (pcheck ([exact-natural? count])
            (define-parser u8vec-parser
              (let ([len (input-len inp)] [pos (input-pos inp)])
                ;; TODO how to report error?
                (if (> (+ pos count) len)
                    (values #f #f
                            (parser-failure-eof
                             inp
                             '(u8vec)
                             "unexpected EOF while reading u8vec"))
                    (let ([bv (make-bytevector count 0)] [data (binary-input-data inp)])
                      (bytevector-copy! data pos bv 0 count)
                      (input-pos-set! inp (+ pos count))
                      (values #t bv inp)))))
            u8vec-parser))


  ;; Decode little-endian base-128 digits in balanced halves. Use this only for long
  ;; encodings; the streaming accumulators below keep short integers inexpensive.
  (define leb128-bytes->integer
    (lambda (bytes start count)
      (if (fx<= count 8)
          (let loop ([index (fx1- (fx+ start count))] [number 0])
            (if (fx< index start)
                number
                (loop (fx1- index)
                      (+ (ash number 7)
                         (fxlogand (bytevector-u8-ref bytes index) #x7f)))))
          (let* ([low-count (fxdiv count 2)]
                 [low (leb128-bytes->integer bytes start low-count)]
                 [high (leb128-bytes->integer bytes (fx+ start low-count)
                                             (fx- count low-count))])
            (if (zero? high) low (+ low (ash high (* low-count 7))))))))

  #|parser:<uleb128>
  Parse an unsigned LEB128-encoded number.
  See https://en.wikipedia.org/wiki/LEB128.
  |#
  (define-parser <uleb128>
    (let ([start (input-pos inp)])
      (let loop ([shift 0] [number 0])
        (let ([byte (peek-u8 inp)])
          (if byte
              (let ([number (if (fx< shift 56)
                                (+ number (ash (fxlogand byte #x7f) shift))
                                number)])
                (if (fx>= byte #x80)
                    (loop (fx+ shift 7) number)
                    (values #t
                            (if (fx< shift 56) number
                                (leb128-bytes->integer
                                 (binary-input-data inp) start (fx- (input-pos inp) start)))
                            inp)))
              (values #f #f (parser-failure-eof inp '(uleb128) #f)))))))


  #|proc:<sleb128>
  The `<sleb128>` procedure takes positive fixnum `bit-length` and returns a binary parser
  for signed LEB128 integers in that width. At most `ceiling(bit-length / 7)` bytes are
  accepted, and unused high bits in the final byte must match the sign bit. Sign padding
  within this byte limit is permitted. The parser returns the decoded signed integer.
  |#
  (define-who (<sleb128> bit-length)
    (pcheck ([sleb128-len? bit-length])
            (define-parser sleb128-parser
              (let ([start (input-pos inp)])
                (let loop ([shift 0] [n 0])
                  (let ([b (peek-u8 inp)])
                    (if b
                        (let* ([bits (fxlogand b #x7f)]
                               [continued? (logbit? 7 b)]
                               [used-bits (min 7 (- bit-length shift))]
                               [unused-mask (fxlogxor #x7f (fx1- (fxsll 1 used-bits)))]
                               [negative? (logbit? (fx1- used-bits) bits)]
                               [n (if (< shift 56) (+ n (ash bits shift)) n)])
                          (cond
                            [(and continued? (<= (- bit-length shift) 7))
                             (values #f #f
                                     (parser-failure-custom inp "signed LEB128 is too long"))]
                            [(not (= (fxlogand bits unused-mask)
                                     (if negative? unused-mask 0)))
                             (values #f #f
                                     (parser-failure-custom
                                      inp "signed LEB128 has inconsistent unused bits"))]
                            [continued? (loop (+ shift 7) n)]
                            [else
                             (let ([n (if (< shift 56) n
                                          (leb128-bytes->integer
                                           (binary-input-data inp) start
                                           (fx- (input-pos inp) start)))])
                               (values #t (if (logbit? 6 b) (- n (ash 1 (+ shift 7))) n) inp))]))
                        (values #f #f (parser-failure-eof inp '(sleb128) #f)))))))
            sleb128-parser))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   combinators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define ensure-progress
    (lambda (who inp0 inp1)
      (when (= (input-pos inp0) (input-pos inp1))
        (errorf who "parser succeeded without consuming input"))))


  #|proc:<many>
  The `<many>` procedure takes `parser` and returns a parser that runs it repeatedly.
  It returns the successful values and raises an error if a success consumes no input.
  If `parser` fails immediately, the returned parser succeeds with `'()`.
  A final failed attempt restores its starting cursor and ends repetition successfully.
  |#
  (define-who (<many> parser)
    (pcheck ([parser? parser])
            (let ([parser-body (parser-body parser)])
              (define-parser many-parser
                (let ([lb (make-list-builder)])
                  (let loop ([inp inp] [old-inp (save-input inp)])
                    (let-values ([(stt val inp1)
                                  (parser-body inp state (fx1+ lvl))])
                      (if stt
                          (begin (ensure-progress who old-inp inp1)
                                 (lb val)
                                 (loop inp1 (save-input inp1)))
                          (values #t (lb) old-inp))))))
              many-parser)))


  #|proc:<many-until>
  The `<many-until>` procedure returns a parser that repeatedly runs `item-parser`, whose
  behavior is `(Input -> Any)`. At each position, `terminator-parser` is checked with lookahead
  behavior `(Input -> Any)` and is not consumed. The result is the list of item values. When the
  terminator does not match, an item failure is propagated. An item success that consumes no input
  raises a progress error.
  |#
  (define-who (<many-until> item-parser terminator-parser)
    (pcheck ([parser? item-parser terminator-parser])
            (let ([item-body (parser-body item-parser)]
                  [terminator-body (parser-body terminator-parser)])
              (define-parser many-until-parser
                (let ([lb (make-list-builder)])
                  (let loop ([inp1 inp])
                    (let ([current-input (save-input inp1)])
                      (let-values ([(terminator-status terminator-value terminator-input)
                                    (terminator-body (save-input current-input)
                                                     state
                                                     (fx1+ lvl))])
                        (if terminator-status
                            (values #t (lb) current-input)
                            (let-values ([(item-status item-value item-input)
                                          (item-body inp1 state (fx1+ lvl))])
                              (if item-status
                                  (begin
                                    (ensure-progress who current-input item-input)
                                    (lb item-value)
                                    (loop item-input))
                                  (values #f #f
                                          (ensure-parser-failure
                                           item-input inp1))))))))))
              many-until-parser)))


  #|proc:<some>
  The `<some>` procedure takes `parser` and returns a parser that runs it one or more
  times. It returns the successful values and raises an error if a success consumes no input.
  After the first success, a failed attempt restores its starting cursor and ends repetition.
  The initial failure is propagated.
  |#
  (define-who (<some> parser)
    (pcheck ([parser? parser])
            (let ([parser-body (parser-body parser)])
              (define-parser some-parser
                (let ([lb (make-list-builder)])
                  ;; 1st
                  (let ([old-inp (save-input inp)])
                    (let-values ([(stt val inp1)
                                  (parser-body inp state (fx1+ lvl))])
                      (if stt
                          (begin (ensure-progress who old-inp inp1)
                                 (lb val)
                                 (let loop ([inp inp1] [old-inp (save-input inp1)])
                                   (let-values ([(stt val inp2)
                                                 (parser-body inp state (fx1+ lvl))])
                                     (if stt
                                         (begin (ensure-progress who old-inp inp2)
                                                (lb val)
                                                (loop inp2 (save-input inp2)))
                                         ;; need to backtrack when the last `p` fails
                                         (values #t (lb) old-inp)))))
                          (values #f #f (ensure-parser-failure inp1 inp)))))))
              some-parser)))


  #|proc:<optional>
  The `<optional>` procedure takes `parser` and returns a parser that runs it once.
  The returned parser returns the parsed value on success and `'()` on failure.
  Failure restores the starting input cursor; caller-owned state mutations are not undone.
  |#
  (define-who (<optional> parser)
    (pcheck ([parser? parser])
            (let ([parser-body (parser-body parser)])
              (define-parser optional-parser
                (let ([old-inp (save-input inp)])
                  (let-values ([(stt val inp1)
                                (parser-body inp state (fx1+ lvl))])
                    (if stt
                        (values #t val inp1)
                        (values #t '() old-inp)))))
              optional-parser)))


  #|proc:<rep>
  The `<rep>` procedure takes `parser` and exact nonnegative integer `count`. The returned parser
  runs `parser` exactly `count` times and returns the values, or fails when any run fails.
  |#
  (define-who (<rep> parser count)
    (pcheck ([parser? parser] [exact-natural? count])
            (let ([parser-body (parser-body parser)])
              (define-parser rep-parser
                (let ([lb (make-list-builder)])
                  (let loop ([i 0] [inp1 inp])
                    (if (= i count)
                        (values #t (lb) inp1)
                        (let-values ([(stt val inp2)
                                      (parser-body inp1 state (fx1+ lvl))])
                          (if stt
                              (begin (lb val)
                                     (loop (+ i 1) inp2))
                              (values #f #f (ensure-parser-failure inp2 inp1))))))))
              rep-parser)))


  #|proc:parser-counted-vector
  The `parser-counted-vector` procedure takes `element-parser` and exact nonnegative `count`.
  It returns a binary parser that runs `element-parser` exactly `count` times and returns a new
  vector of the results, or fails when an element fails. The element parser has behavior
  `(BinaryInput -> Any)` and may consume zero bytes. Counts larger than the remaining input use
  counted list repetition to avoid preallocating a vector for a potentially malformed count.
  This helper is shared internally by format grammars and is excluded from the parser umbrella.
  |#
  (define (parser-counted-vector element-parser count)
    (pcheck ([parser? element-parser] [exact-natural? count])
            (let ([checked-element (<map> values element-parser)]
                  [fallback (<map> list->vector (<rep> element-parser count))])
              (define-parser vector-parser
                (if (> count (- (input-len inp) (input-pos inp)))
                    (parser-call fallback inp state lvl)
                    (let ([result (make-vector count)])
                      (let loop ([index 0] [current inp])
                        (if (fx= index count)
                            (values #t result current)
                            (let-values ([(status value next)
                                          (parser-call checked-element current state lvl)])
                              (if status
                                  (begin (vector-set! result index value)
                                         (loop (fx1+ index) next))
                                  (values #f #f next))))))))
              vector-parser)))


  #|proc:<skip>
  The `<skip>` procedure takes `parser` and exact nonnegative integer `count`. The returned parser
  runs `parser` exactly `count` times, ignores the values, and returns `'()`.
  It fails when any run of `parser` fails.
  |#
  (define-who (<skip> parser count)
    (pcheck ([parser? parser] [exact-natural? count])
            (let ([parser-body (parser-body parser)])
              (define-parser skip-parser
                (let loop ([i 0] [inp1 inp])
                  (if (= i count)
                      (values #t '() inp1)
                      (let-values ([(stt val inp2)
                                    (parser-body inp1 state (fx1+ lvl))])
                        (if stt
                            (loop (+ i 1) inp2)
                            (values #f #f (ensure-parser-failure inp2 inp1)))))))
              skip-parser)))


  #|proc:</>
  The `</>` procedure takes zero or more parsers and returns a left-biased choice parser.
  The returned parser tries each parser from left to right. If no parser is supplied, the
  returned parser always fails. Failures at the farthest position combine expected tokens and
  custom messages in first-occurrence order, including across nested choices.
  |#
  (define (</> . parser*)
    (pcheck ([all-parsers? parser*])
            (let ([body* (map parser-body parser*)])
              (define-parser choice-parser
              (if (null? parser*)
                  (values #f #f (parser-failure-custom inp "empty choice"))
                  (let ([old-inp (save-input inp)])
                    (let loop ([body* body*] [failure* '()])
                      (if (null? body*)
                          (let* ([max-offset (fold-left
                                              (lambda (offset failure)
                                                (max offset (parser-failure-offset failure)))
                                              -1
                                              failure*)]
                                 [farthest (filter
                                            (lambda (failure)
                                              (= max-offset (parser-failure-offset failure)))
                                            failure*)])
                            (values #f #f (parser-failure-merge-choice (reverse farthest))))
                          (let ([branch-input (save-input old-inp)])
                            (let-values ([(stt val inp1)
                                          ((car body*) branch-input state (fx1+ lvl))])
                              (if stt
                                  (values #t val inp1)
                                  (loop (cdr body*)
                                        (cons (ensure-parser-failure inp1 branch-input)
                                              failure*))))))))))
              choice-parser)))


  #|proc:<~>
  The `<~>` procedure takes zero or more parsers `parser*` and returns a sequence parser.
  The returned parser runs each parser in order, returns a list of their values when all
  parsers succeed, and fails when any parser fails.
  |#
  (define-who (<~> . parser*)
    (pcheck ([all-parsers? parser*])
            (let ([body* (map parser-body parser*)])
              (define-parser sequence-parser
                (let ([lb (make-list-builder)])
                  (let loop ([inp1 inp] [body* body*])
                    (if (null? body*)
                        (values #t (lb) inp1)
                        (let-values ([(stt val inp2)
                                      ((car body*) inp1 state (fx1+ lvl))])
                          (if stt
                              (begin (lb val)
                                     (loop inp2 (cdr body*)))
                              (values #f #f
                                      (ensure-parser-failure inp2 inp1))))))))
              sequence-parser)))


  #|proc:<~n>
  The `<~n>` procedure takes exact nonnegative integer `index` and zero or more parsers `parser*`.
  It returns a sequence parser like `<~>`, except success returns only the value from
  `index`. It is an error when `index` is outside the supplied parser range.
  The index is zero-based. Its upper bound is checked when the returned parser runs, before any
  supplied parser runs.
  |#
  (define-who (<~n> index . parser*)
    (pcheck ([exact-natural? index] [all-parsers? parser*])
            (let ([body* (map parser-body parser*)])
              (define-parser indexed-sequence-parser
                (let* ([body* body*] [len (length body*)] [v #f])
                  (if (<= 0 index (fx1- len))
                      (let loop ([i 0] [body* body*] [inp inp])
                        (if (null? body*)
                            (values #t v inp)
                            (let-values ([(stt val inp1)
                                          ((car body*) inp state (fx1+ lvl))])
                              (if stt
                                  (begin (when (fx= i index) (set! v val))
                                         (loop (fx1+ i) (cdr body*) inp1))
                                  (values #f #f
                                          (ensure-parser-failure inp1 inp))))))
                      (errorf who "bad parser index ~a (must be between 0 and ~a)"
                              index (fx1- len)))))
              indexed-sequence-parser)))


  #|proc:<map>
  The `<map>` procedure takes `mapper` and `parser`. The `mapper` procedure must have
  signature `(Any -> Any)`. When `parser` succeeds, the returned parser applies `mapper`.
  |#
  (define-who (<map> mapper parser)
    (pcheck ([procedure? mapper] [parser? parser])
            (let ([parser-body (parser-body parser)])
              (define-parser map-parser
                (let-values ([(stt val inp1) (parser-body inp state (fx1+ lvl))])
                  (if stt
                      (values #t (mapper val) inp1)
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              map-parser)))


  #|proc:<map-st>
  The `<map-st>` procedure takes `mapper` and `parser`. The `mapper` procedure must have
  signature `(Any Any -> Any)` for a parse value and parser state. When `parser` succeeds, the
  returned parser applies `mapper` and returns its value. State mutations are not rolled back
  when an enclosing parser backtracks or performs lookahead.
  |#
  (define-who (<map-st> mapper parser)
    (pcheck ([procedure? mapper] [parser? parser])
            (let ([parser-body (parser-body parser)])
              (define-parser map-state-parser
                (let-values ([(stt val inp1) (parser-body inp state (fx1+ lvl))])
                  (if stt
                      (values #t (mapper val state) inp1)
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              map-state-parser)))


  #|proc:<bind>
  The `<bind>` procedure takes `parser` and `binder`. The `binder` procedure must have
  signature `(Any -> Parser)`. When `parser` succeeds, the returned parser applies
  `binder` to the value and runs the resulting parser.
  |#
  (define-who (<bind> parser binder)
    (pcheck ([parser? parser] [procedure? binder])
            (let ([initial-body (parser-body parser)])
              (define-parser bind-parser
                (let-values ([(stt val inp1) (initial-body inp state (fx1+ lvl))])
                  (if stt
                      (let ([next-parser (binder val)])
                        (pcheck ([parser? next-parser])
                                (let-values ([(status value next-input)
                                              ((parser-body next-parser) inp1 state (fx1+ lvl))])
                                  (if status (values #t value next-input)
                                      (values #f #f
                                              (ensure-parser-failure next-input inp1))))))
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              bind-parser)))


  #|proc:<bind-st>
  The `<bind-st>` procedure takes `parser` and `binder`. The `binder` procedure must have
  signature `(Any Any -> Parser)` for a parse value and parser state. When `parser`
  succeeds, the returned parser applies `binder` and returns the value from the resulting parser.
  State mutations are not rolled back by backtracking or lookahead.
  |#
  (define-who (<bind-st> parser binder)
    (pcheck ([parser? parser] [procedure? binder])
            (let ([initial-body (parser-body parser)])
              (define-parser bind-state-parser
                (let-values ([(stt val inp1) (initial-body inp state (fx1+ lvl))])
                  (if stt
                      (let ([next-parser (binder val state)])
                        (pcheck ([parser? next-parser])
                                (let-values ([(status value next-input)
                                              ((parser-body next-parser) inp1 state (fx1+ lvl))])
                                  (if status (values #t value next-input)
                                      (values #f #f
                                              (ensure-parser-failure next-input inp1))))))
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              bind-state-parser)))


  #|proc:<followed-by>
  The `<followed-by>` procedure takes `parser` and `following-parser`. The returned
  parser returns the first value only when both parsers succeed, without consuming input
  from `following-parser`.
  |#
  (define-who (<followed-by> parser following-parser)
    (pcheck ([parser? parser following-parser])
            (let ([parser-body (parser-body parser)]
                  [following-body (parser-body following-parser)])
              (define-parser followed-by-parser
                (let-values ([(stt1 val1 inp1)
                              (parser-body inp state (fx1+ lvl))])
                  (if stt1
                      (let ([old-input (save-input inp1)])
                        (let-values ([(stt2 val2 inp2)
                                      (following-body inp1 state (fx1+ lvl))])
                          (if stt2
                              (values #t val1 old-input)
                              (values #f #f (ensure-parser-failure inp2 inp1)))))
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              followed-by-parser)))


  #|proc:<not-followed-by>
  The `<not-followed-by>` procedure takes `parser` and `following-parser`. The returned
  parser returns the first value only when `parser` succeeds and `following-parser` fails,
  without consuming input from `following-parser`.
  |#
  (define-who (<not-followed-by> parser following-parser)
    (pcheck ([parser? parser following-parser])
            (let ([parser-body (parser-body parser)]
                  [following-body (parser-body following-parser)])
              (define-parser not-followed-by-parser
                (let-values ([(stt1 val1 inp1)
                              (parser-body inp state (fx1+ lvl))])
                  (if stt1
                      (let ([old-input (save-input inp1)])
                        (let-values ([(stt2 val2 inp2)
                                      (following-body inp1 state (fx1+ lvl))])
                          (if stt2
                              (values #f #f
                                      (parser-failure-custom old-input
                                                             "input was not expected"))
                              (values #t val1 old-input))))
                      (values #f #f (ensure-parser-failure inp1 inp)))))
              not-followed-by-parser)))


  #|proc:~>
  The `~>` procedure takes parsers `left-parser` and `right-parser`. It returns a parser
  that runs them sequentially and returns the value from `right-parser`.
  |#
  (define (~> left-parser right-parser)
    (pcheck ([parser? left-parser right-parser])
            (<~n> 1 left-parser right-parser)))


  #|proc:<~
  The `<~` procedure takes parsers `left-parser` and `right-parser`. It returns a parser
  that runs them sequentially and returns the value from `left-parser`.
  |#
  (define (<~ left-parser right-parser)
    (pcheck ([parser? left-parser right-parser])
            (<~n> 0 left-parser right-parser)))


  #|proc:<~0>
  The `<~0>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 0. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~0>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 0 parser*))))

  #|proc:<~1>
  The `<~1>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 1. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~1>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 1 parser*))))

  #|proc:<~2>
  The `<~2>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 2. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~2>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 2 parser*))))

  #|proc:<~3>
  The `<~3>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 3. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~3>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 3 parser*))))

  #|proc:<~4>
  The `<~4>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 4. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~4>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 4 parser*))))

  #|proc:<~5>
  The `<~5>` procedure takes parsers `parser*`, runs them in order, and returns the value at
  zero-based index 5. It is `<~n>` with that index; too few parsers raise an error at parse time.
  |#
  (define <~5>
    (lambda parser*
      (pcheck ([all-parsers? parser*])
              (apply <~n> 5 parser*))))


  #|proc:<as>
  The `<as>` procedure takes value `value` and parser `parser`. It returns a parser that
  returns `value` whenever `parser` succeeds.
  |#
  (define (<as> value parser)
    (pcheck ([parser? parser])
            (<map> (lambda (parsed-value) value) parser)))


  #|proc:<as-string>
  The `<as-string>` procedure takes `parser` and returns a parser that converts its
  parsed character list to a string.
  |#
  (define (<as-string> parser)
    (pcheck ([parser? parser])
            (<map> list->string parser)))


  #|proc:<as-symbol>
  The `<as-symbol>` procedure takes `parser` and returns a parser that converts its
  parsed character list to a symbol.
  |#
  (define (<as-symbol> parser)
    (pcheck ([parser? parser])
            (<map> (lambda (value) (string->symbol (list->string value))) parser)))

  #|proc:<as-integer>
  The `<as-integer>` procedure takes parser `parser` and returns a parser that converts
  its parsed decimal character list to an integer, allowing an optional leading sign.
  An empty list returns `#f`. A sign without digits fails with a parser error through a runner.
  |#
  (define (<as-integer> parser)
    (pcheck ([parser? parser])
            (<bind> parser
                    (lambda (value)
                      (if (null? value)
                          (<result> #f)
                          (let-values ([(sign digit*)
                                        (cond [(eq? #\+ (car value))
                                               (values + (cdr value))]
                                              [(eq? #\- (car value))
                                               (values - (cdr value))]
                                              [else (values + value)])])
                            (if (null? digit*)
                                (<fail-with> "integer sign must be followed by digits")
                                (let ([digits (make-list-builder)])
                                  ;; Validate left to right; map callback order is unspecified.
                                  (for-each (lambda (character) (digits (char->num character))) digit*)
                                  (<result> (sign (digits->num (digits))))))))))))

  #|proc:<sep-by>
  The `<sep-by>` procedure takes `parser` and `separator`. It returns a parser for zero
  or more `parser` values separated by `separator` values, which are ignored.
  A failed separator/item attempt restores the cursor before that separator; a trailing
  separator is left unread. If the first item fails, the cursor is restored and the result is an
  empty list.
  |#
  (define (<sep-by> parser separator)
    (pcheck ([parser? parser separator])
            (<map> (lambda (val) (if (null? val) val (cons (car val) (cadr val))))
                   (<optional> (<~> parser (<many> (~> separator parser)))))))

  #|proc:<sep-by1>
  The `<sep-by1>` procedure takes `parser` and `separator`. It returns a parser for one
  or more `parser` values separated by `separator` values, which are ignored.
  A failed separator/item attempt restores the cursor before that separator; a trailing
  separator is left unread. The first item must succeed.
  |#
  (define (<sep-by1> parser separator)
    (pcheck ([parser? parser separator])
            (<map> (lambda (val) (cons (car val) (cadr val)))
                   (<~> parser (<many> (~> separator parser))))))


  #|proc:parser-discard-many
  The `parser-discard-many` procedure takes `parser` and returns a parser that repeats it,
  ignores its values, and returns the empty list. The argument `parser` has behavior
  `(Input -> Any)` and must consume input on success. The final failed attempt restores its
  starting cursor. A success without input progress raises an error. Callback state is not
  rolled back.
  This helper is shared internally by format grammars and is excluded from the parser umbrella.
  |#
  (define (parser-discard-many parser)
    (pcheck ([parser? parser])
            (define-parser discard-parser
              (let loop ([current inp])
                (let ([snapshot (save-input current)])
                  (let-values ([(status value next-input)
                                (parser-call parser current state (fx1+ lvl))])
                    (if status
                        (begin (ensure-progress '<many> snapshot next-input)
                               (loop next-input))
                        (values #t '() snapshot))))))
            discard-parser))

  #|proc:<token>
  The `<token>` procedure takes `parser` and returns a parser that runs it, consumes any
  following whitespace, and returns its value.
  |#
  (define (<token> parser)
    (pcheck ([parser? parser])
            (<~ parser (parser-discard-many <whitespace>))))


  #|proc:<fully>
  The `<fully>` procedure takes textual `parser`. The returned parser consumes leading whitespace,
  runs `parser`, consumes trailing whitespace, requires EOF, and returns the value of `parser`.
  |#
  (define (<fully> parser)
    (pcheck ([parser? parser])
            (<~n> 1 (parser-discard-many <whitespace>) parser (parser-discard-many <whitespace>) <eof>)))


  ;; TODO These can be placed in the front at o=3, but not at o=2.
  (define c->n
    (lambda (c)
      (if (ascii-digit? c)
          (fx- (char->integer c) 48)
          (errorf 'c->n "not an ASCII digit: ~a" c))))
  (define hexc->n
    (lambda (c)
      (cond [(char<=? #\a c #\f) (fx- (char->integer c) 87)]
            [(char<=? #\A c #\F) (fx- (char->integer c) 55)]
            [else (c->n c)])))

  #|parser:<digit2>
  Parse a binary digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1.
  |#
  (define <digit2> (<map> c->n <bindigit>))

  #|parser:<digit8>
  Parse a octal digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\7 -> 7.
  |#
  (define <digit8> (<map> c->n <octdigit>))

  #|parser:<digit10>
  Parse a decimal digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\9 -> 9.
  |#
  (define <digit10> (<map> c->n <digit>))

  #|parser:<digit16>
  Parse a hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\a -> 10, ..., #\f -> 15, #\A -> 10, ..., #\F -> 15.
  |#
  (define <digit16> (<map> hexc->n <hexdigit>))

  #|parser:<upper-digit16>
  Parse an upper hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\A -> 10, ..., #\F -> 15.
  |#
  (define <upper-digit16> (<map> hexc->n <upper-hexdigit>))

  #|parser:<lower-digit16>
  Parse a lower hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\a -> 10, ..., #\f -> 15.
  |#
  (define <lower-digit16> (<map> hexc->n <lower-hexdigit>))



  )
