(library (cspp parser wasm binary)
  (export <wasm-binary-module>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm types)
          (cspp parser wasm validate)
          (cspp parser wasm binary values)
          (cspp parser wasm binary types)
          (cspp parser wasm binary instructions))

  (define list->vector*
    (lambda (value*)
      (let* ([length (length value*)]
             [result (make-vector length)])
        (let loop ([index 0] [value* value*])
          (if (null? value*)
              result
              (begin
                (vector-set! result index (car value*))
                (loop (fx1+ index) (cdr value*))))))))

  (define-record-type parsed-section
    (fields offset id value))

  (define issue-parser
    (lambda (issue)
      (<pos-at> (wasm-issue-offset issue)
                (<fail-with> (wasm-issue-message issue)))))

  (define expression-instruction
    (lambda (mnemonic index)
      (make-wasm-instruction mnemonic (vector index) '#() '#())))

  (define function-index-initializers
    (lambda (indices)
      (let ([result (make-vector (vector-length indices))])
        (let loop ([index 0])
          (if (= index (vector-length indices))
              result
              (begin
                (vector-set! result index
                             (vector (expression-instruction
                                      'ref.func (vector-ref indices index))))
                (loop (fx1+ index))))))))

  (define table-parser
    (</> (<map> (lambda (fields)
                  (make-wasm-table (car fields) (cadr fields)))
                (<~1> (<u8*> #x40 #x00)
                       (<~> <wasm-table-type> <wasm-expression>)))
         (<map> (lambda (type) (make-wasm-table type #f)) <wasm-table-type>)))

  (define memory-parser
    (<map> (lambda (type) (make-wasm-memory type)) <wasm-memory-type>))

  (define global-parser
    (<map> (lambda (fields)
             (make-wasm-global (car fields) (cadr fields)))
           (<~> <wasm-global-type> <wasm-expression>)))

  (define export-parser
    (<map> (lambda (fields)
             (let ([descriptor (cadr fields)])
               (make-wasm-export (car fields)
                                 (car descriptor)
                                 (cdr descriptor))))
           (<~> <wasm-name>
                (<bind> <u8>
                        (lambda (kind)
                          (case kind
                            [(0) (<map> (lambda (index) (cons 'function index))
                                        <wasm-u32>)]
                            [(1) (<map> (lambda (index) (cons 'table index))
                                        <wasm-u32>)]
                            [(2) (<map> (lambda (index) (cons 'memory index))
                                        <wasm-u32>)]
                            [(3) (<map> (lambda (index) (cons 'global index))
                                        <wasm-u32>)]
                            [(4) (<map> (lambda (index) (cons 'tag index))
                                        <wasm-u32>)]
                            [else (<fail-with> "invalid WebAssembly export kind")]))))))

  (define import-parser
    (<map> (lambda (fields)
             (make-wasm-import (car fields) (cadr fields) (caddr fields)))
           (<~> <wasm-name> <wasm-name> <wasm-external-type>)))

  (define local-declaration-parser
    (<~> <wasm-u32> <wasm-value-type>))

  ;; Code bodies carry their own size so locals and the expression are bounded together.
  (define code-parser
    (<bind> <wasm-u32>
            (lambda (size)
              (<bounded>
               size
               (<bind>
                (<~> (<wasm-vector> local-declaration-parser) <wasm-expression>)
                (lambda (fields)
                  (let* ([declarations (car fields)]
                         [body (cadr fields)]
                         [count (let loop ([index 0] [count 0])
                                  (if (= index (vector-length declarations)) count
                                      (loop (fx1+ index)
                                            (+ count (car (vector-ref declarations index))))))])
                    (if (>= count (ash 1 32))
                        (<fail-with> "WebAssembly function has too many locals")
                        (let ([locals (make-vector count)])
                          (let loop ([index 0] [next 0])
                            (if (= index (vector-length declarations))
                                (<result> (vector locals body))
                                (let* ([declaration (vector-ref declarations index)]
                                       [count (car declaration)]
                                       [type (cadr declaration)])
                                  (let fill ([local-index 0])
                                    (if (= local-index count)
                                        (loop (fx1+ index) (+ next count))
                                        (begin
                                          (vector-set! locals (+ next local-index) type)
                                          (fill (fx1+ local-index)))))))))))))))))

  (define element-kind-parser (<uimm8> 0))

  (define function-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'active
                                (make-wasm-reference-type #t 'func)
                                0
                                (cadr fields)
                                (function-index-initializers (caddr fields))))
           (<~> (<result> #f) <wasm-expression> (<wasm-vector> <wasm-u32>))))

  (define passive-function-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'passive
                                (make-wasm-reference-type #t 'func)
                                #f #f
                                (function-index-initializers (cadr fields))))
           (<~> element-kind-parser (<wasm-vector> <wasm-u32>))))

  (define explicit-function-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'active
                                (make-wasm-reference-type #t 'func)
                                (car fields) (cadr fields)
                                (function-index-initializers (cadddr fields))))
           (<~> <wasm-u32> <wasm-expression> element-kind-parser
                (<wasm-vector> <wasm-u32>))))

  (define declarative-function-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'declarative
                                (make-wasm-reference-type #t 'func)
                                #f #f
                                (function-index-initializers (cadr fields))))
           (<~> element-kind-parser (<wasm-vector> <wasm-u32>))))

  (define active-expression-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'active
                                (make-wasm-reference-type #t 'func)
                                0 (car fields) (cadr fields)))
           (<~> <wasm-expression> (<wasm-vector> <wasm-expression>))))

  (define passive-expression-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'passive (car fields) #f #f (cadr fields)))
           (<~> <wasm-reference-type> (<wasm-vector> <wasm-expression>))))

  (define explicit-expression-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'active (caddr fields) (car fields)
                                (cadr fields) (cadddr fields)))
           (<~> <wasm-u32> <wasm-expression> <wasm-reference-type>
                (<wasm-vector> <wasm-expression>))))

  (define declarative-expression-element-parser
    (<map> (lambda (fields)
             (make-wasm-element 'declarative (car fields) #f #f (cadr fields)))
           (<~> <wasm-reference-type> (<wasm-vector> <wasm-expression>))))

  (define element-parser
    (<bind> <wasm-u32>
            (lambda (flags)
              (case flags
                [(0) function-element-parser]
                [(1) passive-function-element-parser]
                [(2) explicit-function-element-parser]
                [(3) declarative-function-element-parser]
                [(4) active-expression-element-parser]
                [(5) passive-expression-element-parser]
                [(6) explicit-expression-element-parser]
                [(7) declarative-expression-element-parser]
                [else (<fail-with> "invalid WebAssembly element segment flags")]))))

  (define data-parser
    (<bind> <wasm-u32>
            (lambda (flags)
              (case flags
                [(0) (<map> (lambda (fields)
                              (make-wasm-data 'active 0 (car fields) (cadr fields)))
                            (<~> <wasm-expression> <wasm-byte-vector>))]
                [(1) (<map> (lambda (bytes)
                              (make-wasm-data 'passive #f #f bytes))
                            <wasm-byte-vector>)]
                [(2) (<map> (lambda (fields)
                              (make-wasm-data 'active (car fields) (cadr fields)
                                              (caddr fields)))
                            (<~> <wasm-u32> <wasm-expression> <wasm-byte-vector>))]
                [else (<fail-with> "invalid WebAssembly data segment flags")]))))

  (define custom-section-parser
    (lambda (size)
      (<bind> <pos>
              (lambda (start)
                (<bind> <wasm-name>
                        (lambda (name)
                          (<bind> <pos>
                                  (lambda (end)
                                    (<map>
                                     (lambda (bytes) (cons name bytes))
                                     (<u8vec> (- size (- end start))))))))))))

  (define section-parser
    (lambda (id size)
      (<bounded>
       size
       (case id
         [(0) (custom-section-parser size)]
         [(1) (<wasm-vector> <wasm-recursive-type>)]
         [(2) (<wasm-vector> import-parser)]
         [(3) (<wasm-vector> <wasm-u32>)]
         [(4) (<wasm-vector> table-parser)]
         [(5) (<wasm-vector> memory-parser)]
         [(6) (<wasm-vector> global-parser)]
         [(7) (<wasm-vector> export-parser)]
         [(8) <wasm-u32>]
         [(9) (<wasm-vector> element-parser)]
         [(10) (<wasm-vector> code-parser)]
         [(11) (<wasm-vector> data-parser)]
         [(12) <wasm-u32>]
         [(13) (<wasm-vector>
                (<map> (lambda (type) (make-wasm-tag type))
                       <wasm-tag-type>))]
         [else (<fail-with> "unknown standard WebAssembly section")]))))

  (define section
    (<bind> <pos>
            (lambda (offset)
              (<bind> <u8>
                      (lambda (id)
                        (<bind> <wasm-u32>
                                (lambda (size)
                                  (<map>
                                   (lambda (value)
                                     (make-parsed-section offset id value))
                                   (section-parser id size)))))))))

  (define make-functions
    (lambda (type-indices code-bodies)
      (let ([result (make-vector (vector-length type-indices))])
        (let loop ([index 0])
          (if (= index (vector-length result))
              result
              (let ([code (vector-ref code-bodies index)])
                (vector-set! result index
                             (make-wasm-function
                              (vector-ref type-indices index)
                              (vector-ref code 0)
                              (vector-ref code 1)))
                (loop (fx1+ index))))))))

  (define vector-any?
    (lambda (predicate values)
      (let loop ([index 0])
        (and (fx< index (vector-length values))
             (or (predicate (vector-ref values index)) (loop (fx1+ index)))))))

  (define expression-uses-data-index?
    (lambda (instructions)
      (vector-any?
       (lambda (instruction)
         (let ([mnemonic (wasm-instruction-mnemonic instruction)])
           (or (memq mnemonic '(memory.init data.drop array.new-data array.init-data))
               (expression-uses-data-index? (wasm-instruction-body instruction))
               ;; Other structured alternates contain catch clauses, not instructions.
               (and (eq? mnemonic 'if)
                    (expression-uses-data-index? (wasm-instruction-alternate instruction))))))
       instructions)))

  (define code-uses-data-index?
    (lambda (code)
      (vector-any? (lambda (body) (expression-uses-data-index? (vector-ref body 1))) code)))

  (define assemble-module
    (lambda (section*)
      (let ([section-offset* (make-vector 14 #f)])
        (let loop ([section* section*]
                   [previous-rank 0]
                   [types '#()] [imports '#()] [type-indices '#()]
                   [tables '#()] [memories '#()] [globals '#()] [tags '#()]
                   [exports '#()] [start #f] [elements '#()] [data '#()]
                   [data-count #f] [code '#()] [custom* '()] [after #f])
        (if (null? section*)
            (cond
             [(not (wasm-function-code-count-valid? type-indices code))
              (issue-parser
               (make-wasm-issue
                (or (vector-ref section-offset* 10)
                    (vector-ref section-offset* 3)
                    8)
                "WebAssembly function and code section lengths differ"))]
             [(not (wasm-data-count-valid? data-count data))
              (issue-parser
               (make-wasm-issue
                (or (vector-ref section-offset* 12)
                    (vector-ref section-offset* 11)
                    8)
                "WebAssembly data count does not match data section"))]
             [(and (not data-count) (code-uses-data-index? code))
              (issue-parser
               (make-wasm-issue (or (vector-ref section-offset* 10) 8)
                                "WebAssembly data-index instructions require a data-count section"))]
             [else
              (<result>
               (make-wasm-module
                types imports (make-functions type-indices code)
                tables memories globals tags exports start elements data
                (list->vector* (reverse custom*))))])
            (let* ([parsed (car section*)]
                   [offset (parsed-section-offset parsed)]
                   [id (parsed-section-id parsed)]
                   [value (parsed-section-value parsed)])
              (if (= id 0)
                  (loop (cdr section*) previous-rank types imports type-indices
                        tables memories globals tags exports start elements data
                        data-count code
                        (cons (make-wasm-custom-section
                               (car value) (cdr value) after)
                              custom*)
                        after)
                  (begin
                    (vector-set! section-offset* id offset)
                    (if (not (wasm-section-order-valid? previous-rank id))
                        (issue-parser
                         (make-wasm-issue
                          offset
                          "duplicate or out-of-order WebAssembly section"))
                        (case id
                        [(1) (loop (cdr section*) (wasm-section-rank id) value imports
                                   type-indices tables memories globals tags exports start
                                   elements data data-count code custom* 'type)]
                        [(2) (loop (cdr section*) (wasm-section-rank id) types value
                                   type-indices tables memories globals tags exports start
                                   elements data data-count code custom* 'import)]
                        [(3) (loop (cdr section*) (wasm-section-rank id) types imports value
                                   tables memories globals tags exports start elements data
                                   data-count code custom* 'function)]
                        [(4) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices value memories globals tags exports start
                                   elements data data-count code custom* 'table)]
                        [(5) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices tables value globals tags exports start
                                   elements data data-count code custom* 'memory)]
                        [(6) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices tables memories value tags exports start
                                   elements data data-count code custom* 'global)]
                        [(7) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices tables memories globals tags value start
                                   elements data data-count code custom* 'export)]
                        [(8) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices tables memories globals tags exports value
                                   elements data data-count code custom* 'start)]
                        [(9) (loop (cdr section*) (wasm-section-rank id) types imports
                                   type-indices tables memories globals tags exports start
                                   value data data-count code custom* 'element)]
                        [(10) (loop (cdr section*) (wasm-section-rank id) types imports
                                    type-indices tables memories globals tags exports start
                                    elements data data-count value custom* 'code)]
                        [(11) (loop (cdr section*) (wasm-section-rank id) types imports
                                    type-indices tables memories globals tags exports start
                                    elements value data-count code custom* 'data)]
                        [(12) (loop (cdr section*) (wasm-section-rank id) types imports
                                    type-indices tables memories globals tags exports start
                                    elements data value code custom* 'data-count)]
                          [(13) (loop (cdr section*) (wasm-section-rank id) types imports
                                      type-indices tables memories globals value exports start
                                      elements data data-count code custom* 'tag)]))))))))))

  (define <wasm-binary-module>
    (<bind> (<~> (<u8*> #x00 #x61 #x73 #x6d)
                 (<u8*> 1 0 0 0)
                 (<many-until> section <eof>))
            (lambda (fields) (assemble-module (caddr fields)))))
  )
