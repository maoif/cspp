(library (cspp net uri)
  (export string->uri
          make-uri
          uri-update
          uri-with-scheme
          uri-with-authority
          uri-with-path
          uri-with-query
          uri-with-fragment
          uri->string
          uri?
          uri-scheme
          uri-userinfo
          uri-host
          uri-port
          uri-path
          uri-query
          uri-fragment
          uri-raw-userinfo
          uri-raw-host
          uri-raw-path
          uri-raw-query
          uri-raw-fragment
          uri-authority
          uri-path-segments
          uri-query-alist
          uri-resolve
          uri-normalize
          uri-encode
          uri-decode
          form-urlencode
          form-urldecode
          idna->ascii
          idna->unicode
          idna-domain->ascii
          idna-domain->unicode)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp string)
          (cspp net private ffi)
          (cspp net errors))

  #|record:uri
The `uri` record is an immutable parsed or constructed URI.
Scheme and port are normalized values. Userinfo, host, path, query, and fragment are decoded
strings or `#f`; their raw accessors preserve original percent escapes for lossless rendering.
|#
  (define-record-type (uri %make-uri/raw uri?)
    (sealed #t)
    (opaque #f)
    (fields (immutable scheme uri-scheme)
            (immutable userinfo uri-userinfo)
            (immutable host uri-host)
            (immutable port uri-port)
            (immutable path uri-path)
            (immutable query uri-query)
            (immutable fragment uri-fragment)
            (immutable raw-userinfo uri-raw-userinfo)
            (immutable raw-host uri-raw-host)
            (immutable raw-path uri-raw-path)
            (immutable raw-query uri-raw-query)
            (immutable raw-fragment uri-raw-fragment)))

  (define make-uri-from-raw
    (lambda (who scheme userinfo host port path query fragment)
      (%make-uri/raw scheme
                     (and userinfo (percent-decode who userinfo #f))
                     (and host (percent-decode who host #f))
                     port
                     (percent-decode who path #f)
                     (and query (percent-decode who query #f))
                     (and fragment (percent-decode who fragment #f))
                     userinfo host path query fragment)))

  #|proc:make-uri
The `make-uri` procedure constructs an immutable URI from optional component strings.
`scheme`, `userinfo`, `host`, `path`, `query`, and `fragment` are strings or `#f`; `port` is
an integer or `#f`. The return value is a URI object.
|#
  (define-who make-uri
    (lambda (scheme userinfo host port path query fragment)
      (pcheck ([string? path])
        (unless (or (not scheme) (string? scheme)) (errorf who "invalid URI scheme"))
        (unless (or (not userinfo) (string? userinfo)) (errorf who "invalid URI userinfo"))
        (unless (or (not host) (string? host)) (errorf who "invalid URI host"))
        (unless (or (not port) (and (fixnum? port) (fx<= 0 port) (fx<= port 65535)))
          (errorf who "invalid URI port"))
        (unless (or (not query) (string? query)) (errorf who "invalid URI query"))
        (unless (or (not fragment) (string? fragment)) (errorf who "invalid URI fragment"))
        (make-uri-from-raw who scheme userinfo host port path query fragment))))

  #|proc:uri-update
The `uri-update` procedure returns a URI copied from `u` with one `field` replaced by `value`.
`field` is one of `scheme`, `userinfo`, `host`, `port`, `path`, `query`, or `fragment`.
|#
  (define-who uri-update
    (lambda (u field value)
      (pcheck ([uri? u] [symbol? field])
        (case field
          [(scheme) (uri-with-scheme u value)]
          [(userinfo) (uri-with-authority u value (uri-raw-host u) (uri-port u))]
          [(host) (uri-with-authority u (uri-raw-userinfo u) value (uri-port u))]
          [(port) (uri-with-authority u (uri-raw-userinfo u) (uri-raw-host u) value)]
          [(path) (uri-with-path u value)]
          [(query) (uri-with-query u value)]
          [(fragment) (uri-with-fragment u value)]
          [else (errorf who "unknown URI field ~s" field)]))))

  #|proc:uri-with-scheme
The `uri-with-scheme` procedure returns `u` with its scheme replaced by `scheme`.
The `u` parameter is a URI, and `scheme` is a string or `#f`.
|#
  (define-who uri-with-scheme
    (lambda (u scheme)
      (pcheck ([uri? u])
        (unless (or (not scheme) (string? scheme))
          (errorf who "invalid URI scheme"))
        (make-uri-from-raw who scheme (uri-raw-userinfo u) (uri-raw-host u) (uri-port u)
                           (uri-raw-path u) (uri-raw-query u) (uri-raw-fragment u)))))

  #|proc:uri-with-authority
The `uri-with-authority` procedure returns `u` with new authority components.
The `userinfo` and `host` parameters are strings or `#f`; `port` is a valid port or `#f`.
|#
  (define-who uri-with-authority
    (lambda (u userinfo host port)
      (pcheck ([uri? u])
        (unless (or (not userinfo) (string? userinfo))
          (errorf who "invalid URI userinfo"))
        (unless (or (not host) (string? host))
          (errorf who "invalid URI host"))
        (unless (or (not port) (and (fixnum? port) (fx<= 0 port 65535)))
          (errorf who "invalid URI port"))
        (make-uri-from-raw who (uri-scheme u) userinfo host port (uri-raw-path u)
                           (uri-raw-query u) (uri-raw-fragment u)))))

  #|proc:uri-with-path
The `uri-with-path` procedure returns `u` with its path replaced by string `path`.
|#
  (define-who uri-with-path
    (lambda (u path)
      (pcheck ([uri? u] [string? path])
        (make-uri-from-raw who (uri-scheme u) (uri-raw-userinfo u) (uri-raw-host u)
                           (uri-port u) path (uri-raw-query u) (uri-raw-fragment u)))))

  #|proc:uri-with-query
The `uri-with-query` procedure returns `u` with its query replaced by string `query` or `#f`.
|#
  (define-who uri-with-query
    (lambda (u query)
      (pcheck ([uri? u])
        (unless (or (not query) (string? query))
          (errorf who "invalid URI query"))
        (make-uri-from-raw who (uri-scheme u) (uri-raw-userinfo u) (uri-raw-host u)
                           (uri-port u) (uri-raw-path u) query (uri-raw-fragment u)))))

  #|proc:uri-with-fragment
The `uri-with-fragment` procedure returns `u` with fragment string `fragment` or `#f`.
|#
  (define-who uri-with-fragment
    (lambda (u fragment)
      (pcheck ([uri? u])
        (unless (or (not fragment) (string? fragment))
          (errorf who "invalid URI fragment"))
        (make-uri-from-raw who (uri-scheme u) (uri-raw-userinfo u) (uri-raw-host u)
                           (uri-port u) (uri-raw-path u) (uri-raw-query u) fragment))))

  #|proc:uri-raw-userinfo
The `uri-raw-userinfo` procedure returns the URI userinfo exactly as parsed, or `#f`.
|#
  #|proc:uri-raw-host
The `uri-raw-host` procedure returns the URI host exactly as parsed, or `#f`.
|#
  #|proc:uri-raw-path
The `uri-raw-path` procedure returns the URI path exactly as parsed.
|#
  #|proc:uri-raw-query
The `uri-raw-query` procedure returns the URI query exactly as parsed, or `#f`.
|#
  #|proc:uri-raw-fragment
The `uri-raw-fragment` procedure returns the URI fragment exactly as parsed, or `#f`.
|#

  (define join-strings
    (lambda (parts sep)
      (if (null? parts)
          ""
          (let loop ([parts (cdr parts)] [out (car parts)])
            (if (null? parts)
                out
                (loop (cdr parts) (string-append out sep (car parts))))))))

  (define string-index
    (lambda (str ch)
      (let ([len (string-length str)])
        (let loop ([i 0])
          (cond
           [(fx= i len) #f]
           [(char=? (string-ref str i) ch) i]
           [else (loop (fx1+ i))])))))

  (define string-rindex
    (lambda (str ch)
      (let loop ([i (fx1- (string-length str))])
        (cond
         [(fx< i 0) #f]
         [(char=? (string-ref str i) ch) i]
         [else (loop (fx1- i))]))))

  (define split-first
    (lambda (str ch)
      (let ([i (string-index str ch)])
        (if i
            (values (substring str 0 i)
                    (substring str (fx1+ i) (string-length str)))
            (values str #f)))))

  (define split-scheme
    (lambda (str)
      (let ([len (string-length str)])
        (if (fx= len 0)
            (values #f str)
            (let ([ch0 (string-ref str 0)])
              (if (or (char<=? #\a ch0 #\z)
                      (char<=? #\A ch0 #\Z))
                  (let loop ([i 1])
                    (cond
                     [(fx= i len) (values #f str)]
                     [(char=? (string-ref str i) #\:)
                      (values (string-downcase (substring str 0 i))
                              (substring str (fx1+ i) len))]
                     [(or (char<=? #\a (string-ref str i) #\z)
                          (char<=? #\A (string-ref str i) #\Z)
                          (char<=? #\0 (string-ref str i) #\9)
                          (char=? (string-ref str i) #\+)
                          (char=? (string-ref str i) #\.)
                          (char=? (string-ref str i) #\-))
                      (loop (fx1+ i))]
                     [else (values #f str)]))
                  (values #f str)))))))

  (define parse-authority
    (lambda (authority)
      (let* ((at (string-rindex authority #\@))
             (userinfo (and at (substring authority 0 at)))
             (hostport (if at
                           (substring authority (fx1+ at) (string-length authority))
                           authority)))
        (cond
         ((string=? hostport "")
          (values userinfo #f #f))
         ((char=? (string-ref hostport 0) #\[)
          (let ((close (string-index hostport #\])))
            (and close
                 (let ((host (substring hostport 1 close))
                       (rest (substring hostport (fx1+ close) (string-length hostport))))
                   (if (string=? rest "")
                       (values userinfo host #f)
                       (if (and (fx>= (string-length rest) 2)
                                (char=? (string-ref rest 0) #\:))
                           (let ((port (string->number (substring rest 1 (string-length rest)) 10)))
                             (and port
                                  (integer? port)
                                  (exact? port)
                                  (values userinfo host port)))
                           #f))))))
         (else
          (let ((colon* (string-search-all hostport #\:)))
            (if (and colon* (null? (cdr colon*)))
                (let ((pos (car colon*)))
                  (let ((host (substring hostport 0 pos))
                        (port-text (substring hostport (fx1+ pos) (string-length hostport))))
                    (let ((port (string->number port-text 10)))
                      (and port
                           (integer? port)
                           (exact? port)
                           (values userinfo host port)))))
                (values userinfo hostport #f))))))))

  (define parse-uri-reference
    (lambda (str)
      (let-values ([(before-fragment fragment) (split-first str #\#)])
        (let-values ([(before-query query) (split-first before-fragment #\?)])
          (let-values ([(scheme rest0) (split-scheme before-query)])
            (if (and (fx>= (string-length rest0) 2)
                     (string=? (substring rest0 0 2) "//"))
                (let* ([rest (substring rest0 2 (string-length rest0))]
                       [slash (string-index rest #\/)]
                       [authority (if slash (substring rest 0 slash) rest)]
                       [path (if slash
                                 (substring rest slash (string-length rest))
                                 "")])
                  (call-with-values
                      (lambda () (parse-authority authority))
                    (lambda (userinfo host port)
                      (and (or host (string=? authority ""))
                           (make-uri-from-raw
                            'string->uri scheme userinfo host port path query fragment)))))
                (make-uri-from-raw 'string->uri scheme #f #f #f rest0 query fragment)))))))

  (define unreserved-byte?
    (lambda (u8)
      (or (and (fx<= 65 u8) (fx<= u8 90))
          (and (fx<= 97 u8) (fx<= u8 122))
          (and (fx<= 48 u8) (fx<= u8 57))
          (memv u8 '(45 46 95 126)))))

  (define byte->hex
    (lambda (u8)
      (let ([s (number->string u8 16)])
        (if (fx= (string-length s) 1)
            (string-append "0" (string-upcase s))
            (string-upcase s)))))

  (define hex-value
    (lambda (ch)
      (cond
       [(char<=? #\0 ch #\9) (fx- (char->integer ch) (char->integer #\0))]
       [(char<=? #\a ch #\f) (fx+ 10 (fx- (char->integer ch) (char->integer #\a)))]
       [(char<=? #\A ch #\F) (fx+ 10 (fx- (char->integer ch) (char->integer #\A)))]
       [else #f])))

  (define percent-decode
    (lambda (who str plus->space?)
      (let ([len (string-length str)])
        (let-values ([(port get) (open-bytevector-output-port)])
          (let loop ([i 0])
            (if (fx= i len)
                (utf8->string (get))
                (let ([ch (string-ref str i)])
                  (cond
                   [(and plus->space? (char=? ch #\+))
                    (put-u8 port 32)
                    (loop (fx1+ i))]
                   [(char=? ch #\%)
                    (if (fx<= (fx+ i 3) len)
                        (let ([a (hex-value (string-ref str (fx1+ i)))]
                              [b (hex-value (string-ref str (fx+ i 2)))])
                          (if (and a b)
                              (begin
                                (put-u8 port (fx+ (fxsll a 4) b))
                                (loop (fx+ i 3)))
                              (errorf who "invalid percent escape in ~s" str)))
                        (errorf who "truncated percent escape in ~s" str))]
                   [else
                    (put-bytevector port (string->utf8 (string ch)))
                    (loop (fx1+ i))]))))))))

  (define normalize-path
    (lambda (path)
      (let* ([absolute? (and (fx> (string-length path) 0)
                             (char=? (string-ref path 0) #\/))]
             [length (string-length path)]
             [trailing?
              (or (and (fx> length 0) (char=? (string-ref path (fx1- length)) #\/))
                  (and (fx>= length 2)
                       (string=? (substring path (fx- length 2) length) "/."))
                  (and (fx>= length 3)
                       (string=? (substring path (fx- length 3) length) "/..")))]
             [segments (string-split path #\/)])
        (let loop ([segments segments] [stack '()])
          (if (null? segments)
              (let* ([body (join-strings (reverse stack) "/")]
                     [base (cond
                            [(and absolute? (string=? body "")) "/"]
                            [absolute? (string-append "/" body)]
                            [else body])])
                (if (and trailing?
                         (not (string=? base ""))
                         (not (char=? (string-ref base (fx1- (string-length base))) #\/)))
                    (string-append base "/")
                    base))
              (let ([segment (car segments)])
                (cond
                 [(or (string=? segment "") (string=? segment "."))
                  (loop (cdr segments) stack)]
                 [(string=? segment "..")
                  (loop (cdr segments) (if (null? stack) stack (cdr stack)))]
                 [else
                  (loop (cdr segments) (cons segment stack))])))))))

  (define merge-path
    (lambda (base relative)
      (cond
       [(and (uri-raw-host base) (string=? (uri-raw-path base) ""))
        (string-append "/" relative)]
       [else
        (let* ([path (uri-raw-path base)]
               [slash (string-rindex path #\/)])
          (if slash
              (string-append (substring path 0 (fx1+ slash)) relative)
              relative))])))

  (define default-port-for-scheme
    (lambda (scheme)
      (cond
       [(or (string=? scheme "http") (string=? scheme "ws")) 80]
       [(or (string=? scheme "https") (string=? scheme "wss")) 443]
       [(string=? scheme "ftp") 21]
       [else #f])))

  #|proc:string->uri
The `string->uri` procedure parses a URI or URI reference string and returns a URI object, or `#f`
on failure.
|#
  (define-who string->uri
    (lambda (str)
      (pcheck ([string? str])
              (parse-uri-reference str))))

  #|proc:uri-authority
The `uri-authority` procedure renders the authority component of a URI, or `#f` when there is none.
|#
  (define-who uri-authority
    (lambda (u)
      (pcheck ([uri? u])
              (and (uri-raw-host u)
                   (let ([host (if (string-contains? (uri-raw-host u) ":")
                                   (string-append "[" (uri-raw-host u) "]")
                                   (uri-raw-host u))])
                     (string-append
                      (if (uri-raw-userinfo u)
                          (string-append (uri-raw-userinfo u) "@")
                          "")
                      host
                      (if (uri-port u)
                          (string-append ":" (number->string (uri-port u)))
                          "")))))))

  #|proc:uri->string
The `uri->string` procedure renders a URI object to text.
|#
  (define-who uri->string
    (lambda (u)
      (pcheck ([uri? u])
              (string-append
               (if (uri-scheme u)
                   (string-append (uri-scheme u) ":")
                   "")
               (if (uri-raw-host u)
                   (string-append "//" (uri-authority u))
                   "")
               (uri-raw-path u)
               (if (uri-raw-query u)
                   (string-append "?" (uri-raw-query u))
                   "")
               (if (uri-raw-fragment u)
                   (string-append "#" (uri-raw-fragment u))
                   "")))))

  #|proc:uri-path-segments
The `uri-path-segments` procedure splits a URI path into slash-separated segments.
|#
  (define-who uri-path-segments
    (lambda (u)
      (pcheck ([uri? u])
              (let ([parts (string-split (uri-path u) #\/)])
                (if (and (pair? parts) (string=? (car parts) ""))
                    (cdr parts)
                    parts)))))

  #|proc:uri-query-alist
The `uri-query-alist` procedure parses the query component of a URI into decoded key/value pairs.
|#
  (define-who uri-query-alist
    (lambda (u)
      (pcheck ([uri? u])
              (if (or (not (uri-raw-query u)) (string=? (uri-raw-query u) ""))
                  '()
                  (map (lambda (piece)
                         (let-values ([(key value) (split-first piece #\=)])
                           (cons (percent-decode who key #t)
                                 (percent-decode who (or value "") #t))))
                       (string-split (uri-raw-query u) #\&))))))

  #|proc:uri-resolve
The `uri-resolve` procedure resolves a URI reference against a base URI.
|#
  (define-who uri-resolve
    (lambda (base ref)
      (pcheck ([uri? base] [uri? ref])
              (cond
               [(uri-scheme ref)
                (make-uri-from-raw who (uri-scheme ref) (uri-raw-userinfo ref)
                                   (uri-raw-host ref) (uri-port ref)
                                   (normalize-path (uri-raw-path ref))
                                   (uri-raw-query ref) (uri-raw-fragment ref))]
               [(uri-raw-host ref)
                (make-uri-from-raw who (uri-scheme base) (uri-raw-userinfo ref)
                                   (uri-raw-host ref) (uri-port ref)
                                   (normalize-path (uri-raw-path ref))
                                   (uri-raw-query ref) (uri-raw-fragment ref))]
               [(string=? (uri-raw-path ref) "")
                (make-uri-from-raw who (uri-scheme base) (uri-raw-userinfo base)
                                   (uri-raw-host base) (uri-port base) (uri-raw-path base)
                                   (or (uri-raw-query ref) (uri-raw-query base))
                                   (uri-raw-fragment ref))]
               [(and (fx> (string-length (uri-raw-path ref)) 0)
                     (char=? (string-ref (uri-raw-path ref) 0) #\/))
                (make-uri-from-raw who (uri-scheme base) (uri-raw-userinfo base)
                                   (uri-raw-host base) (uri-port base)
                                   (normalize-path (uri-raw-path ref))
                                   (uri-raw-query ref) (uri-raw-fragment ref))]
               [else
                (make-uri-from-raw who (uri-scheme base) (uri-raw-userinfo base)
                                   (uri-raw-host base) (uri-port base)
                                   (normalize-path (merge-path base (uri-raw-path ref)))
                                   (uri-raw-query ref) (uri-raw-fragment ref))]))))

  #|proc:uri-normalize
The `uri-normalize` procedure normalizes URI casing, dot segments, and default ports.
|#
  (define-who uri-normalize
    (lambda (u)
      (pcheck ([uri? u])
              (let* ([scheme (and (uri-scheme u) (string-downcase (uri-scheme u)))]
                     [host (and (uri-host u) (string-downcase (uri-host u)))]
                     [port (uri-port u)]
                     [default-port (and scheme (default-port-for-scheme scheme))])
                (make-uri-from-raw who scheme
                                   (uri-raw-userinfo u)
                                   host
                                   (if (and default-port port (fx= default-port port))
                                       #f
                                       port)
                                   (normalize-path (uri-raw-path u))
                                   (uri-raw-query u)
                                   (uri-raw-fragment u))))))

  #|proc:uri-encode
The `uri-encode` procedure percent-encodes a string for use in URI components.
|#
  (define-who uri-encode
    (lambda (str)
      (pcheck ([string? str])
              (let ([bv (string->utf8 str)])
                (let loop ([i 0] [out '()])
                  (if (fx= i (bytevector-length bv))
                      (apply string-append (reverse out))
                      (let ([u8 (bytevector-u8-ref bv i)])
                        (loop (fx1+ i)
                              (cons (if (unreserved-byte? u8)
                                        (string (integer->char u8))
                                        (string-append "%" (byte->hex u8)))
                                    out)))))))))

  #|proc:uri-decode
The `uri-decode` procedure decodes percent escapes in a URI component string.
|#
  (define-who uri-decode
    (lambda (str)
      (pcheck ([string? str])
              (percent-decode who str #f))))

  (define idna-convert
    (lambda (who domain convert)
      (define control-character?
        (lambda (character)
          (let ([codepoint (char->integer character)])
            (or (fx< codepoint #x20)
                (and (fx<= #x7f codepoint) (fx<= codepoint #x9f))))))
      (when (exists control-character? (string->list domain))
        (raise-net-error 'uri 'idna "domain contains a control character"
                         'invalid-domain #f domain #f #f #f))
      (let ([answer (convert domain)])
        (if (string? answer)
            answer
            (if (and (vector? answer) (fx= (vector-length answer) 2)
                     (eq? (vector-ref answer 0) 'error)
                     (string? (vector-ref answer 1)))
                (raise-net-error 'uri 'idna (vector-ref answer 1)
                                 'invalid-domain #f domain #f #f answer)
                (raise-net-error who 'internal-ffi "malformed libidn2 result" answer))))))

  #|proc:idna->ascii
The `idna->ascii` procedure converts Unicode `domain` to lowercase IDNA ASCII form.
The return value is an ASCII domain string; invalid labels raise a structured URI error.
|#
  (define-who idna->ascii
    (lambda (domain)
      (pcheck ([string? domain])
        (string-downcase (idna-convert who domain ffi-net-idna->ascii)))))

  #|proc:idna->unicode
The `idna->unicode` procedure converts IDNA ASCII `domain` to normalized Unicode form.
The return value is a Unicode domain string; invalid labels raise a structured URI error.
|#
  (define-who idna->unicode
    (lambda (domain)
      (pcheck ([string? domain])
        (string-downcase (idna-convert who domain ffi-net-idna->unicode)))))

  #|proc:idna-domain->ascii
The `idna-domain->ascii` procedure converts Unicode `domain` to lowercase IDNA ASCII form.
It is an alias for `idna->ascii` and returns an ASCII domain string.
|#
  (define idna-domain->ascii idna->ascii)

  #|proc:idna-domain->unicode
The `idna-domain->unicode` procedure converts IDNA ASCII `domain` to normalized Unicode form.
It is an alias for `idna->unicode` and returns a Unicode domain string.
|#
  (define idna-domain->unicode idna->unicode)

  #|proc:form-urlencode
The `form-urlencode` procedure encodes an association list into an
`application/x-www-form-urlencoded` string.
|#
  (define-who form-urlencode
    (lambda (alist)
      (unless (list? alist)
        (errorf who "expected association list, given ~s" alist))
      (let ([encode-component
             (lambda (x)
               (let ([bv (string->utf8 x)])
                 (let loop ([i 0] [out '()])
                   (if (fx= i (bytevector-length bv))
                       (apply string-append (reverse out))
                       (let ([u8 (bytevector-u8-ref bv i)])
                         (loop (fx1+ i)
                               (cons (cond
                                      [(fx= u8 32) "+"]
                                      [(unreserved-byte? u8) (string (integer->char u8))]
                                      [else (string-append "%" (byte->hex u8))])
                                     out)))))))])
        (join-strings
         (map (lambda (entry)
                (unless (pair? entry)
                  (errorf who "expected association list entry, given ~s" entry))
                (let ([key (car entry)] [value (cdr entry)])
                  (unless (string? key)
                    (errorf who "expected string form key, given ~s" key))
                  (unless (string? value)
                    (errorf who "expected string form value, given ~s" value))
                  (string-append (encode-component key)
                                 "="
                                 (encode-component value))))
              alist)
         "&"))))

  #|proc:form-urldecode
The `form-urldecode` procedure decodes an `application/x-www-form-urlencoded` string into an
association list.
|#
  (define-who form-urldecode
    (lambda (str)
      (pcheck ([string? str])
              (if (string=? str "")
                  '()
                  (map (lambda (piece)
                         (let-values ([(key value) (split-first piece #\=)])
                           (cons (percent-decode who key #t)
                                 (percent-decode who (or value "") #t))))
                       (string-split str #\&))))))
  )
