(library (cspp regex)
  #|
  Literal patterns can be parsed and validated during Scheme macro expansion.
  General compile-time construction is different: Irregex uses serializable DFA
  data for regular patterns, but backreferences can produce closure-based matchers.
  On Chez 10.5, fasl-write accepts the compiled DFA for "abc" and rejects the
  procedure-containing matcher for a capture followed by a backreference.
  A general ahead-of-time facade would need matcher code generation or an explicit
  DFA-only contract. This facade instead compiles patterns through string->regex
  or sre->regex at runtime; a library-level binding compiles once per instantiation.
  |#
  (export regex? make-regex string->regex sre->regex regex->irregex
          regex-flags regex-num-submatches regex-names
          regex-match? regex-match-pattern regex-new-match regex-reset-match!
          regex-search regex-search/matches regex-match regex-matches?
          regex-match-substring regex-match-subchunk regex-match-start-index regex-match-end-index
          regex-match-start-chunk regex-match-end-chunk regex-match-num-submatches
          regex-match-names regex-match-valid-index?
          regex-chunker? make-regex-chunker regex-search/chunked regex-match/chunked
          regex-fold regex-fold/chunked regex-extract regex-split regex-replace regex-replace/all
          regex-quote regex-opt regex-sre->string regex-sre->cset)
  (import (cspp chez)
          (cspp private irregex)
          (cspp utils))

  ;; Raw engine objects remain private except through the explicit escape hatch.
  (define-record-type ($regex %make-regex %regex?)
    (opaque #t) (sealed #t)
    (fields (immutable engine regex-engine)))
  (define-record-type ($regex-match %make-match %match?)
    (opaque #t) (sealed #t)
    (fields (immutable pattern match-pattern)
            (mutable data match-data match-data-set!)))
  (define-record-type ($regex-chunker %make-chunker %chunker?)
    (opaque #t) (sealed #t)
    (fields (immutable engine chunker-engine)))

  (define sre?
    (lambda (value)
      (or (string? value) (symbol? value) (char? value) (list? value))))
  (define capture?
    (lambda (value)
      (or (symbol? value) (and (fixnum? value) (fx>= value 0)))))
  (define flags?
    (lambda (flags)
      (and (list? flags)
           (for-all (lambda (flag)
                      (memq flag '(i ci case-insensitive m multi-line s single-line
                                   x ignore-space u utf8)))
                    flags))))
  (define check-range
    (lambda (text start end)
      (pcheck ([string? text] [fixnum? start end])
              (unless (fx<= 0 start end (string-length text))
                (error 'regex "invalid string bounds" start end)))))
  (define wrap-match
    (lambda (pattern data)
      (and data (%make-match pattern data))))

  #|proc:regex?
  Return whether `object` is a regex record. Accepts any object.
  |#
  (define regex?
    (lambda (object)
      (%regex? object)))

  #|proc:regex-match?
  Return whether `object` is a regex-match record. Accepts any object.
  |#
  (define regex-match?
    (lambda (object)
      (%match? object)))

  #|proc:regex-chunker?
  Return whether `object` is a regex-chunker record. Accepts any object.
  |#
  (define regex-chunker?
    (lambda (object)
      (%chunker? object)))

  #|proc:make-regex
  Wrap compiled Irregex object `engine` and return a regex record.
  |#
  (define make-regex
    (lambda (engine)
      (pcheck ([irregex? engine])
              (%make-regex engine))))

  #|proc:string->regex
  Compile `pattern` (a pattern string) and return a regex record.
  Optional `flags` is a list of Irregex flags: i, m, s, x, u, utf8 and their long aliases; default
  is empty.
  Compilation happens when this procedure runs, not during Scheme expansion.
  |#
  (define string->regex
    (case-lambda
      [(pattern) (string->regex pattern '())]
      [(pattern flags)
       (pcheck ([string? pattern] [flags? flags])
               (%make-regex (apply string->irregex pattern flags)))]))

  #|proc:sre->regex
  Compile `pattern` (an S-expression pattern) and return a regex record.
  Optional `flags` is a list of Irregex flags: i, m, s, x, u, utf8 and their long aliases; default
  is empty.
  Compilation happens when this procedure runs, not during Scheme expansion.
  |#
  (define sre->regex
    (case-lambda
      [(pattern) (sre->regex pattern '())]
      [(pattern flags)
       (pcheck ([sre? pattern] [flags? flags])
               (%make-regex (apply sre->irregex pattern flags)))]))

  #|proc:regex->irregex
  Return the underlying engine of `pattern`; mutating it affects this wrapper.
  |#
  (define regex->irregex
    (lambda (pattern)
      (pcheck ([regex? pattern])
              (regex-engine pattern))))

  #|proc:regex-flags
  Return the engine's numeric flags for regex record `pattern`.
  |#
  (define regex-flags
    (lambda (pattern)
      (pcheck ([regex? pattern])
              ((lambda (p) (irregex-flags (regex-engine p))) pattern))))

  #|proc:regex-num-submatches
  Return the number of capturing groups in regex record `pattern`, excluding group zero.
  |#
  (define regex-num-submatches
    (lambda (pattern)
      (pcheck ([regex? pattern])
              ((lambda (p) (irregex-num-submatches (regex-engine p))) pattern))))

  #|proc:regex-names
  Return a fresh name/index association list for regex record `pattern`.
  |#
  (define regex-names
    (lambda (pattern)
      (pcheck ([regex? pattern])
              ((lambda (p)
                 (map (lambda (entry) (cons (car entry) (cdr entry)))
                      (irregex-names (regex-engine p))))
               pattern))))

  #|proc:regex-match-pattern
  Return the regex record associated with match record `match`.
  |#
  (define regex-match-pattern
    (lambda (match)
      (pcheck ([regex-match? match])
              (match-pattern match))))

  #|proc:regex-new-match
  Return an empty reusable match record for regex record `pattern`.
  |#
  (define regex-new-match
    (lambda (pattern)
      (pcheck ([regex? pattern])
              (%make-match pattern (irregex-new-matches (regex-engine pattern))))))

  #|proc:regex-reset-match!
  Clear captures in match record `match` and return `match`.
  |#
  (define regex-reset-match!
    (lambda (match)
      (pcheck ([regex-match? match])
              (irregex-reset-matches! (match-data match))
              match)))

  #|proc:regex-search
  Find the first match of regex `pattern` in string `text`.
  Optional `start` and `end` are character offsets with 0 <= start <= end <= length.
  Defaults cover the whole string. Return a match record or #f.
  |#
  (define regex-search
    (case-lambda
      [(pattern text) (regex-search pattern text 0 (string-length text))]
      [(pattern text start) (regex-search pattern text start (string-length text))]
      [(pattern text start end)
       (pcheck ([regex? pattern] [string? text] [fixnum? start end])
               (check-range text start end)
               (wrap-match pattern (irregex-search (regex-engine pattern) text start end)))]))

  #|proc:regex-match
  Match the entire selected substring of regex `pattern` in string `text`.
  Optional `start` and `end` are character offsets with 0 <= start <= end <= length.
  Defaults cover the whole string. Return a match record or #f.
  |#
  (define regex-match
    (case-lambda
      [(pattern text) (regex-match pattern text 0 (string-length text))]
      [(pattern text start) (regex-match pattern text start (string-length text))]
      [(pattern text start end)
       (pcheck ([regex? pattern] [string? text] [fixnum? start end])
               (check-range text start end)
               (wrap-match pattern (irregex-match (regex-engine pattern) text start end)))]))

  #|proc:regex-matches?
  Return whether regex `pattern` matches the entire string `text`.
  |#
  (define regex-matches?
    (lambda (pattern text)
      (pcheck ([regex? pattern] [string? text])
              (and (regex-match pattern text) #t))))

  #|proc:regex-search/matches
  Search string `text` with regex `pattern`, storing captures in `match`.
  The reusable match must belong to that exact pattern. Optional bounds select characters.
  Return `match` on success, or #f with its old captures cleared on failure.
  |#
  (define regex-search/matches
    (case-lambda
      [(pattern match text)
       (regex-search/matches pattern match text 0 (string-length text))]
      [(pattern match text start end)
       (pcheck ([regex? pattern] [regex-match? match] [string? text] [fixnum? start end])
               (unless (eq? pattern (match-pattern match))
                 (error 'regex-search/matches "match belongs to another pattern"))
               (check-range text start end)
               (let ([result (regex-search pattern text start end)])
                 (regex-reset-match! match)
                 (and result
                      (begin
                        (match-data-set! match (match-data result))
                        match))))]))

  #|proc:regex-match-substring
  Return the matched string for capture `index` in `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-substring
    (case-lambda
      [(match) (regex-match-substring match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-substring (match-data match) index))]))

  #|proc:regex-match-start-index
  Return the start character offset within its chunk for capture `index` in `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-start-index
    (case-lambda
      [(match) (regex-match-start-index match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-start-index (match-data match) index))]))

  #|proc:regex-match-end-index
  Return the exclusive end character offset within its chunk for capture `index` in `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-end-index
    (case-lambda
      [(match) (regex-match-end-index match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-end-index (match-data match) index))]))

  #|proc:regex-match-start-chunk
  Return the start chunk for capture `index` in `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-start-chunk
    (case-lambda
      [(match) (regex-match-start-chunk match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-start-chunk (match-data match) index))]))

  #|proc:regex-match-end-chunk
  Return the end chunk for capture `index` in `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-end-chunk
    (case-lambda
      [(match) (regex-match-end-chunk match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-end-chunk (match-data match) index))]))

  #|proc:regex-match-subchunk
  Return the subchunk returned by the chunker's extraction callback for capture `index` in
  `match`.
  The optional index is a nonnegative integer or group-name symbol; default is zero.
  Return #f for an unmatched capture; raise for an unknown name or out-of-range index.
  |#
  (define regex-match-subchunk
    (case-lambda
      [(match) (regex-match-subchunk match 0)]
      [(match index)
       (pcheck ([regex-match? match] [capture? index])
               (irregex-match-subchunk (match-data match) index))]))

  #|proc:regex-match-num-submatches
  Return the number of capturing groups, excluding group zero of match record `match`.
  |#
  (define regex-match-num-submatches
    (lambda (match)
      (pcheck ([regex-match? match])
              (irregex-match-num-submatches (match-data match)))))

  #|proc:regex-match-names
  Return the name/index association list of match record `match`.
  |#
  (define regex-match-names
    (lambda (match)
      (pcheck ([regex-match? match])
              (irregex-match-names (match-data match)))))

  #|proc:regex-match-valid-index?
  Return whether capture `index` (nonnegative integer or symbol) exists in `match`.
  A valid capture may still be unmatched.
  |#
  (define regex-match-valid-index?
    (lambda (match index)
      (pcheck ([regex-match? match] [capture? index])
              (and (irregex-match-valid-index? (match-data match) index) #t))))

  #|proc:make-regex-chunker
  Return a typed chunker. `next` maps a chunk to the next chunk or #f; `text` maps it to a string.
  Optional `start` and `end` map a chunk to character bounds; defaults are zero and string length.
  Optional `substring` and `subchunk` take (first-chunk start last-chunk end) and return a string
  and an application-defined subchunk, respectively. Omitted extraction uses Irregex defaults.
  |#
  (define make-regex-chunker
    (case-lambda
      [(next text) (make-regex-chunker next text #f #f #f #f)]
      [(next text start end) (make-regex-chunker next text start end #f #f)]
      [(next text start end substring subchunk)
       (pcheck ([procedure? next text]
                [(lambda (v) (or (not v) (procedure? v))) start end substring subchunk])
               (%make-chunker (make-irregex-chunker next text start end substring subchunk)))]))

  #|proc:regex-search/chunked
  Apply regex `pattern` to `source` using typed `chunker`.
  Source is a caller-defined first chunk. Return a match record or #f.
  Search for the first occurrence.
  |#
  (define regex-search/chunked
    (lambda (pattern chunker source)
      (pcheck ([regex? pattern] [regex-chunker? chunker])
              (wrap-match pattern
                          (irregex-search/chunked (regex-engine pattern)
                                                  (chunker-engine chunker) source)))))

  #|proc:regex-match/chunked
  Apply regex `pattern` to `source` using typed `chunker`.
  Source is a caller-defined first chunk. Return a match record or #f.
  Match all chunks in their entirety.
  |#
  (define regex-match/chunked
    (lambda (pattern chunker source)
      (pcheck ([regex? pattern] [regex-chunker? chunker])
              (wrap-match pattern
                          (irregex-match/chunked (regex-engine pattern)
                                                 (chunker-engine chunker) source)))))

  #|proc:regex-fold
  Fold matches of regex `pattern` in string `text`, starting from `seed`.
  `proc` has signature (previous-end match accumulator) -> accumulator.
  Optional `finish` has signature (previous-end accumulator) -> result; default returns
  accumulator.
  Optional `start` and `end` delimit the string. Match records retained by callbacks remain
  usable.
  Empty-match advancement follows Irregex. Return the finalizer result.
  |#
  (define regex-fold
    (case-lambda
      [(pattern proc seed text)
       (regex-fold pattern proc seed text (lambda (end acc) acc) 0 (string-length text))]
      [(pattern proc seed text finish)
       (regex-fold pattern proc seed text finish 0 (string-length text))]
      [(pattern proc seed text finish start end)
       (pcheck ([regex? pattern] [procedure? proc finish] [string? text] [fixnum? start end])
               (check-range text start end)
               (irregex-fold (regex-engine pattern)
                             (lambda (previous raw acc)
                               (proc previous (%make-match pattern raw) acc))
                             seed text finish start end))]))

  #|proc:regex-fold/chunked
  Fold matches of `pattern` in `source` through typed `chunker`, starting with `seed`.
  `proc` takes (previous-chunk previous-index match accumulator) and returns the next accumulator.
  Match records are stable across callbacks. Return the final accumulator.
  |#
  (define regex-fold/chunked
    (lambda (pattern proc seed chunker source)
      (pcheck ([regex? pattern] [procedure? proc] [regex-chunker? chunker])
              (irregex-fold/chunked (regex-engine pattern)
                                    (lambda (chunk index raw acc)
                                      (proc chunk index (%make-match pattern raw) acc))
                                    seed (chunker-engine chunker) source))))

  #|proc:regex-extract
  Return a list of matching substrings of regex `pattern` in string `text`.
  Optional `start` and `end` are character offsets, defaulting to the entire string.
  |#
  (define regex-extract
    (case-lambda
      [(pattern text) (regex-extract pattern text 0 (string-length text))]
      [(pattern text start end)
       (pcheck ([regex? pattern] [string? text] [fixnum? start end])
               (check-range text start end)
               (irregex-extract (regex-engine pattern) text start end))]))

  #|proc:regex-split
  Return a list of nonempty pieces between matches (Irregex splitting semantics) of regex
  `pattern` in string `text`.
  Optional `start` and `end` are character offsets, defaulting to the entire string.
  |#
  (define regex-split
    (case-lambda
      [(pattern text) (regex-split pattern text 0 (string-length text))]
      [(pattern text start end)
       (pcheck ([regex? pattern] [string? text] [fixnum? start end])
               (check-range text start end)
               (irregex-split (regex-engine pattern) text start end))]))

  (define replacement?
    (lambda (value)
      (or (string? value) (capture? value) (procedure? value))))
  (define adapt-replacement
    (lambda (pattern replacement)
      (if (procedure? replacement)
          (lambda (raw)
            ;; The fast Irregex replacement path reuses its match vector.
            (let ([result (replacement (%make-match pattern (vector-copy raw)))])
              (pcheck ([string? result]) result)))
          replacement)))

  #|proc:regex-replace
  Return string `text` with the first occurrence of regex `pattern` replaced.
  Each replacement part is a literal string, capture index/name, pre/post symbol, or callback.
  Callbacks have signature (match) -> string and receive stable typed match records.
  No replacement parts deletes the matched text. Unmatched captures contribute empty strings.
  |#
  (define regex-replace
    (lambda (pattern text . replacements)
      (pcheck ([regex? pattern] [string? text])
              (for-each (lambda (part)
                          (pcheck ([replacement? part]) (void)))
                        replacements)
              (apply irregex-replace (regex-engine pattern) text
                     (map (lambda (part) (adapt-replacement pattern part)) replacements)))))

  #|proc:regex-replace/all
  Return string `text` with all occurrences of regex `pattern` replaced.
  Each replacement part is a literal string, capture index/name, pre/post symbol, or callback.
  Callbacks have signature (match) -> string and receive stable typed match records.
  No replacement parts deletes the matched text. Unmatched captures contribute empty strings.
  |#
  (define regex-replace/all
    (lambda (pattern text . replacements)
      (pcheck ([regex? pattern] [string? text])
              (for-each (lambda (part)
                          (pcheck ([replacement? part]) (void)))
                        replacements)
              (apply irregex-replace/all (regex-engine pattern) text
                     (map (lambda (part) (adapt-replacement pattern part)) replacements)))))

  #|proc:regex-quote
  Return string `text` escaped for literal use inside a regex pattern.
  |#
  (define regex-quote
    (lambda (text)
      (pcheck ([string? text])
              (irregex-quote text))))

  #|proc:regex-opt
  Return an optimized S-expression pattern matching any member of list `strings`.
  Each member must be a string. Irregex's optimization utility supports byte-range characters.
  |#
  (define regex-opt
    (lambda (strings)
      (pcheck ([(lambda (v) (and (list? v) (for-all string? v))) strings])
              (irregex-opt strings))))

  #|proc:regex-sre->string
  Return a pattern string representing S-expression pattern `sre`, using Irregex conversion.
  |#
  (define regex-sre->string
    (lambda (sre)
      (pcheck ([sre? sre])
              (sre->string sre))))

  #|proc:regex-sre->cset
  Return Irregex's character-set vector for S-expression character-set `sre`.
  Optional boolean `ignore-case?` defaults to #f. This conversion returns engine data, not a
  regex.
  |#
  (define regex-sre->cset
    (case-lambda
      [(sre) (regex-sre->cset sre #f)]
      [(sre ignore-case?)
       (pcheck ([sre? sre] [boolean? ignore-case?])
               (sre->cset sre ignore-case?))]))
)
