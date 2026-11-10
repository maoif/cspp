(import (cspp)
        (cspp parser toml))

(include "parser-external-tools.ss")

(define toml-root
  (lambda (text)
    (toml-document-root (parse-toml text))))

(define toml-value
  (lambda (text key)
    (toml-table-ref (toml-root text) key)))

(define with-temporary-toml-bytes
  (lambda (bytes procedure)
    (let ([path (format "parser-toml-~a.toml" (random 999999))]
          [port #f])
      (dynamic-wind
        (lambda ()
          (set! port
                (open-file-output-port path
                                       (file-options no-fail replace)
                                       (buffer-mode block)
                                       #f))
          (put-bytevector port bytes)
          (flush-output-port port))
        (lambda () (procedure path))
        (lambda ()
          (when port (close-port port))
          (when (file-exists? path) (delete-file path)))))))

(mat parse-toml-records

     (string=? "#[toml-local-date year: 2026 month: 9 day: 4]"
               (format "~s"
                       (toml-table-ref
                        (toml-document-root (parse-toml "day = 2026-09-04"))
                        "day")))

     (let* ([document (parse-toml "answer = 42")]
            [root (toml-document-root document)])
       (and (toml-document? document)
            (toml-table? root)
            (= 42 (toml-table-ref root "answer"))))

     (let* ([root (toml-document-root
                   (parse-toml "[owner]\nname = 'Tom'"))]
            [owner (toml-table-ref root "owner")])
       (and (toml-table? owner)
            (string=? "Tom" (toml-table-ref owner "name"))))

     )

(mat parse-toml-scalars

     (and (string=? "basic" (toml-value "s = \"basic\"" "s"))
          (string=? "literal" (toml-value "s = 'literal'" "s"))
          (string=? "line 1\nline 2"
                    (toml-value "s = \"\"\"\nline 1\nline 2\"\"\"" "s"))
          (string=? "line 1\nline 2"
                    (toml-value "s = '''\nline 1\nline 2'''" "s"))
          (string=? "a\"\"b" (toml-value "s = \"\"\"a\"\"b\"\"\"" "s"))
          (string=? "a''b" (toml-value "s = '''a''b'''" "s"))
          (string=? "a\"\"" (toml-value "s = \"\"\"a\"\"\"\"\"" "s"))
          (string=? "a''" (toml-value "s = '''a'''''" "s")))

     (string=? "hello world"
               (toml-value "s = \"\"\"\nhello \\\n                 world\"\"\"" "s"))

     (string=? "A\x1B;\xE9;"
               (toml-value "s = \"A\\e\\xE9\"" "s"))

     ;; error: unrecognized escape sequences are reserved.
     (error? (parse-toml "s = \"\\q\""))

     ;; error: a basic string cannot contain an unescaped control character.
     (error? (parse-toml (string #\s #\space #\= #\space #\" #\x7F #\")))

     (and (= 0 (toml-value "x=0" "x"))
          (= 0 (toml-value "x=-0" "x"))
          (= -123456789 (toml-value "x=-123_456_789" "x"))
          (= #xdeadbeef (toml-value "x=0xde_ad_be_ef" "x"))
          (= #o456 (toml-value "x=0o4_56" "x"))
          (= #b11011 (toml-value "x=0b1_10_11" "x"))
          (= 224617.445991228 (toml-value "x=224_617.445_991_228" "x"))
          (= 5e22 (toml-value "x=5e+22" "x"))
          (infinite? (toml-value "x=-inf" "x"))
          (nan? (toml-value "x=nan" "x")))

     ;; error: decimal integers cannot contain leading zeros.
     (error? (parse-toml "x=0123"))

     ;; error: underscores must be surrounded by digits.
     (error? (parse-toml "x=1__0"))

     ;; error: TOML special floats are lowercase.
     (error? (parse-toml "x=Infinity"))

     ;; error: ordinary key/value pairs cannot share a line.
     (error? (parse-toml "a=1 b=2"))

     ;; error: a bare carriage return is not a TOML newline.
     (error? (parse-toml "a=1\rb=2"))

     (and (= 1 (toml-table-ref (toml-root "a=1\r\n") "a"))
          (= 2 (toml-table-ref (toml-root "a=1\r\nb=2") "b")))

     ;; error: comments cannot contain forbidden control characters.
     (error? (parse-toml (string-append "a=1 #" (string #\x7f))))

     )

(mat parse-toml-temporal-and-collections

     (let ([value (toml-value "t=1979-05-27T07:32-07:00" "t")])
       (and (toml-offset-date-time? value)
            (= 1979 (toml-offset-date-time-year value))
            (= 5 (toml-offset-date-time-month value))
            (= 27 (toml-offset-date-time-day value))
            (= 7 (toml-offset-date-time-hour value))
            (= 32 (toml-offset-date-time-minute value))
            (= 0 (toml-offset-date-time-second value))
            (= 0 (toml-offset-date-time-nanosecond value))
            (= -25200 (toml-offset-date-time-offset-seconds value))))

     (let ([value (toml-value "t=1979-05-27 07:32:01.1234567899" "t")])
       (and (toml-local-date-time? value)
            (= 1 (toml-local-date-time-second value))
            (= 123456789 (toml-local-date-time-nanosecond value))))

     (let ([date (toml-value "d=2000-02-29" "d")]
           [time (toml-value "t=23:59" "t")])
       (and (toml-local-date? date)
            (= 2000 (toml-local-date-year date))
            (= 2 (toml-local-date-month date))
            (= 29 (toml-local-date-day date))
            (toml-local-time? time)
            (= 0 (toml-local-time-second time))))

     ;; error: February 29 requires a leap year.
     (error? (parse-toml "d=1900-02-29"))

     ;; error: a month must be between 1 and 12.
     (error? (parse-toml "d=2000-13-01"))

     ;; error: an hour must be between 0 and 23.
     (error? (parse-toml "t=24:00"))

     ;; error: a minute must be between 0 and 59.
     (error? (parse-toml "t=12:60"))

     ;; error: a second cannot exceed the leap-second value 60.
     (error? (parse-toml "t=12:00:61"))

     ;; error: an offset hour must be between 0 and 23.
     (error? (parse-toml "t=1979-05-27T07:32+24:00"))

     (let ([array (toml-value
                   "a = [1, 'two', [3], # comment\n true,]"
                   "a")])
       (and (toml-array? array)
            (= 4 (vector-length (toml-array-elements array)))
            (toml-array? (vector-ref (toml-array-elements array) 2))))

     (let ([table (toml-value
                   "point = {\n x = 1,\n nested.value = 2,\n }"
                   "point")])
       (and (toml-table? table)
            (toml-table-inline? table)
            (= 1 (toml-table-ref table "x"))
            (= 2 (toml-table-ref (toml-table-ref table "nested") "value"))))

     )

(mat parse-toml-tables

     (let* ([root (toml-root "database.server.port = 5432")]
            [database (toml-table-ref root "database")]
            [server (toml-table-ref database "server")])
       (= 5432 (toml-table-ref server "port")))

     (let* ([root (toml-root "[a.b]\nx=1\n[a]\ny=2")]
            [a (toml-table-ref root "a")]
            [b (toml-table-ref a "b")])
       (and (= 2 (toml-table-ref a "y"))
            (= 1 (toml-table-ref b "x"))))

     (let* ([root (toml-root
                   "[[products]]\nname='one'\n[[products]]\nname='two'\n[products.meta]\nx=1")]
            [products (toml-table-ref root "products")]
            [elements (toml-array-elements products)]
            [latest (vector-ref elements 1)])
       (and (toml-array? products)
            (= 2 (vector-length elements))
            (string=? "two" (toml-table-ref latest "name"))
            (= 1 (toml-table-ref (toml-table-ref latest "meta") "x"))))

     (let* ([root (toml-root
                   "[[fruits]]\nname='apple'\n[[fruits.varieties]]\nname='red'")]
            [fruits (toml-table-ref root "fruits")]
            [fruit (vector-ref (toml-array-elements fruits) 0)]
            [varieties (toml-table-ref fruit "varieties")])
       (and (toml-array? varieties)
            (string=? "red"
                      (toml-table-ref
                       (vector-ref (toml-array-elements varieties) 0)
                       "name"))))

     ;; error: a key cannot be defined twice.
     (error? (parse-toml "a=1\na=2"))

     ;; error: bare and quoted spellings define the same key.
     (error? (parse-toml "a=1\n'a'=2"))

     ;; error: a scalar cannot be extended as a table.
     (error? (parse-toml "a=1\n[a.b]"))

     ;; error: an explicitly defined table cannot be redefined.
     (error? (parse-toml "[a]\n[a]"))

     ;; error: a dotted-key table cannot later be explicitly defined.
     (error? (parse-toml "a.b=1\n[a]"))

     ;; error: inline tables cannot be extended later.
     (error? (parse-toml "a={b=1}\na.c=2"))

     ;; error: a static array cannot become an array of tables.
     (error? (parse-toml "a=[]\n[[a]]"))

     ;; error: a table cannot become an array of tables.
     (error? (parse-toml "[a]\n[[a]]"))

     ;; An ordinary table parent is created implicitly for a nested array of tables.
     (let* ([root (toml-document-root (parse-toml "[[a.b]]\nx=1"))]
            [array (toml-table-ref (toml-table-ref root "a") "b")])
       (= 1 (toml-table-ref (vector-ref (toml-array-elements array) 0) "x")))

     )

(mat parse-toml-files

     (let* ([document (parse-toml-file "data/pyproj.toml")]
            [root (toml-document-root document)]
            [build-system (toml-table-ref root "build-system")]
            [project (toml-table-ref root "project")]
            [dependencies (toml-table-ref project "dependencies")]
            [urls (toml-table-ref project "urls")]
            [tool (toml-table-ref root "tool")]
            [scikit-build (toml-table-ref tool "scikit-build")]
            [cmake (toml-table-ref scikit-build "cmake")]
            [define (toml-table-ref cmake "define")]
            [overrides (toml-table-ref scikit-build "overrides")]
            [override (vector-ref (toml-array-elements overrides) 0)]
            [condition (toml-table-ref override "if")])
       (and (toml-document? document)
            (string=? "scikit_build_core.build"
                      (toml-table-ref build-system "build-backend"))
            (string=? "halide" (toml-table-ref project "name"))
            (string=? ">=3.8" (toml-table-ref project "requires-python"))
            (= 2 (vector-length (toml-array-elements dependencies)))
            (string=? "numpy" (vector-ref (toml-array-elements dependencies) 0))
            (string=? "imageio" (vector-ref (toml-array-elements dependencies) 1))
            (string=? "https://halide-lang.org" (toml-table-ref urls "Homepage"))
            (string=? ">=3.28" (toml-table-ref cmake "version"))
            (string=? "wabt" (toml-table-ref define "Halide_WASM_BACKEND"))
            (string=? "^win32" (toml-table-ref condition "platform-system"))))

     (or (not (external-tool-available? "tomlq"))
         (let* ([path "data/pyproj.toml"]
                [root (toml-document-root (parse-toml-file path))]
                [build-system (toml-table-ref root "build-system")]
                [project (toml-table-ref root "project")]
                [dependencies (toml-table-ref project "dependencies")]
                [tool (toml-table-ref root "tool")]
                [scikit-build (toml-table-ref tool "scikit-build")]
                [cmake (toml-table-ref scikit-build "cmake")]
                [define (toml-table-ref cmake "define")]
                [summary
                 (format "~a\n~a\n~a\n~a\n~a\n"
                         (toml-table-ref project "name")
                         (toml-table-ref project "requires-python")
                         (vector-length (toml-array-elements dependencies))
                         (toml-table-ref build-system "build-backend")
                         (toml-table-ref define "Halide_WASM_BACKEND"))]
                [result
                 (process-capture
                  "tomlq" "-r"
                  (string-append
                   ".project.name, .project[\"requires-python\"], "
                   "(.project.dependencies | length), "
                   ".[\"build-system\"][\"build-backend\"], "
                   ".tool[\"scikit-build\"].cmake.define.Halide_WASM_BACKEND")
                  (begin path)
                  :stdout capture
                  :stderr capture
                  :timeout 10000)])
           (equal? summary (successful-process-output result))))

     ;; error: malformed UTF-8 must not be replaced while reading TOML files.
     (error?
      (with-temporary-toml-bytes (bytevector #xc3 #x28) parse-toml-file))

     )

(mat parse-toml-leap-seconds

     (let ([value (toml-value "t=2016-12-31T23:59:60Z" "t")])
       (and (= 60 (toml-offset-date-time-second value))
            (= 0 (toml-offset-date-time-offset-seconds value))))

     (let ([value (toml-value "t=2017-01-01T00:59:60.125+01:00" "t")])
       (and (= 60 (toml-offset-date-time-second value))
            (= 125000000 (toml-offset-date-time-nanosecond value))
            (= 3600 (toml-offset-date-time-offset-seconds value))))

     (= 60 (toml-local-date-time-second
            (toml-value "t=2016-12-31T23:59:60" "t")))

     (= 60 (toml-local-time-second (toml-value "t=23:59:60" "t"))))

(mat parse-toml-inline-error-positions

     ;; Error cases: duplicate and conflicting inline keys point to the offending key,
     ;; including nested tables/arrays and multiline TOML 1.1 inline tables.
     (for-all
      (lambda (source offset line column)
        (guard (condition
                [(parser-error? condition)
                 (and (= offset (parser-error-offset condition))
                      (= line (parser-error-line condition))
                      (= column (parser-error-column condition)))])
          (parse-toml source)
          #f))
      '("a={x=1,x=2}" "a={x=1,x.y=2}" "a={x.y=1,x=2}"
        "a={\n x=1,\n x=2\n}" "a=[{x=1,x=2}]" "a={outer={x=1,x=2}}"
        "x=1\nx=2")
      '(7 7 9 11 8 14 4) '(0 0 0 2 0 0 1) '(7 7 9 1 8 14 0)))
