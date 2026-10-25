(import (cspp))


(mat simple-datatype

     (begin (datatype Term
                      [Var   var]
                      [Const const]
                      [Abs   binder body]
                      [App   rator rand])
            #t)

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (procedure? Term-Var-var)
     (procedure? Term-Const-const)
     (procedure? Term-Abs-binder)
     (procedure? Term-Abs-body)
     (procedure? Term-App-rator)
     (procedure? Term-App-rand)


     (Term? (Var 'v))
     (Term? (Const 42))
     (Term? (Abs 'x 'body))
     (Term? (App 'f 'x))
     (Term? (Abs 'x (Abs 'y (Const 'f))))
     (Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (Term? 42))
     (not (Term? (list)))
     (not (Term? (vector)))


     ;; singleton
     (begin (datatype Color [R] [G] [B])
            #t)

     (Color? R)
     (Color? G)
     (Color? B)
     (not (Color? 42))
     (not (procedure? R))
     (not (procedure? G))
     (not (procedure? B))


     ;; mixed
     (begin (datatype Direction
                      [X length]
                      [Y length]
                      [Still])
            #t)

     (procedure? X)
     (procedure? Y)
     (not (procedure? Still))
     (Direction? (X 10))
     (Direction? (Y 10))
     (Direction? Still)

     )


(mat datatype-with-custom-type-predicate

     (begin (datatype Term A-Term?
                      [Var   var]
                      [Const const]
                      [Abs   binder body]
                      [App   rator rand])
            #t)

     (A-Term? (Var 'v))
     (A-Term? (Const 42))
     (A-Term? (Abs 'x 'body))
     (A-Term? (App 'f 'x))
     (A-Term? (Abs 'x (Abs 'y (Const 'f))))
     (A-Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (A-Term? 42))
     (not (A-Term? (list)))
     (not (A-Term? (vector)))


     (begin (datatype Color Prim-Color? [R] [G] [B])
            #t)

     (Prim-Color? R)
     (Prim-Color? G)
     (Prim-Color? B)
     (not (Prim-Color? 42))

     )


(mat datatype-with-variant-field-predicate

     (begin (datatype Term Term?
                      [Var
                       (var :predicate symbol?)]
                      [Const
                       (const :predicate number?)]
                      [Abs
                       (binder :predicate symbol?)
                       (body :predicate Term?)]
                      [App
                       (rator :predicate Term?)
                       (rand :predicate Term?)])
            #t)
     (error? (Var 42))
     (error? (Const 'bla))
     (error? (Abs 'v 'bla))
     (error? (Abs 42 (Var 'v)))
     (error? (App 'v (Const 42)))
     (error? (App (Const 42) 'v))

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (Term? (Var 'v))
     (Term? (Const 42))
     (Term? (Abs 'x (Abs 'y (Const 42))))
     (Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (Term? 42))
     (not (Term? (list)))
     (not (Term? (vector)))

     )


(mat datatype-with-mutability

     (begin (datatype Term Term?
                      [Var
                       (var :mutable :predicate symbol?)]
                      [Const
                       (const :mutable :predicate number?)]
                      [Abs
                       (binder :immutable :predicate symbol?)
                       (body :immutable :predicate Term?)]
                      [App
                       (rator :immutable :predicate Term?)
                       (rand :immutable :predicate Term?)])
            #t)
     (error? (Var 42))
     (error? (Const 'bla))
     (error? (Abs 'v 'bla))
     (error? (Abs 42 (Var 'v)))
     (error? (App 'v (Const 42)))
     (error? (App (Const 42) 'v))

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (procedure? Term-Var-var-set!)
     (procedure? Term-Const-const-set!)

     (error? (let ([t (Var 'v)])
               (Term-Var-var-set! t 42)))
     (error? (let ([t (Const 42)])
               (Term-Const-const-set! t 'bla)))

     (let ([t (Var 'v)])
       (Term-Var-var-set! t 'vvv)
       (eq? 'vvv
            (Term-Var-var t)))
     (let ([t (Const 42)])
       (Term-Const-const-set! t 24)
       (= 24
          (Term-Const-const t)))

     )


(mat datatype-with-keyword-field-options

     (begin (datatype KeywordTerm KeywordTerm?
                      [KVar
                       (name :predicate symbol? :mutable)]
                      [KConst
                       (value :mutable :predicate number?)]
                      [KAbs
                       (binder :immutable :predicate symbol?)
                       (body :predicate KeywordTerm?)]
                      [KCell
                       (value :mutable)])
            #t)

     (KeywordTerm? (KVar 'x))
     (KeywordTerm? (KConst 42))

     ;; The name predicate rejects non-symbol values.
     (error? (KVar 42))

     ;; The value predicate rejects non-numeric values.
     (error? (KConst 'bad))

     ;; The binder predicate rejects non-symbol values.
     (error? (KAbs 42 (KVar 'x)))

     ;; The body predicate rejects values outside the datatype.
     (error? (KAbs 'x 'bad))

     (let ([v (KVar 'x)])
       (KeywordTerm-KVar-name-set! v 'y)
       (eq? 'y (KeywordTerm-KVar-name v)))

     ;; The mutable name setter applies its predicate.
     (error? (let ([v (KVar 'x)])
               (KeywordTerm-KVar-name-set! v 42)))

     (let ([v (KConst 42)])
       (KeywordTerm-KConst-value-set! v 24)
       (= 24 (KeywordTerm-KConst-value v)))

     (let ([v (KCell 'first)])
       (KeywordTerm-KCell-value-set! v 42)
       (= 42 (KeywordTerm-KCell-value v)))

     ;; Explicitly immutable fields do not generate setters.
     (error? (eval 'KeywordTerm-KAbs-binder-set!))

     ;; Fields with no mutability option are immutable by default.
     (error? (eval 'KeywordTerm-KAbs-body-set!))

     ;; Duplicate :predicate option is invalid.
     (error? (eval '(datatype BadDuplicatePredicate
                      [Bad (value :predicate number? :predicate integer?)])))

     ;; Duplicate :immutable option is invalid.
     (error? (eval '(datatype BadDuplicateImmutable
                      [Bad (value :immutable :immutable)])))

     ;; :mutable and :immutable conflict.
     (error? (eval '(datatype BadConflictingMutability
                      [Bad (value :mutable :immutable)])))

     ;; :predicate requires a predicate identifier.
     (error? (eval '(datatype BadMissingPredicate
                      [Bad (value :predicate)])))

     ;; Unknown field options are invalid.
     (error? (eval '(datatype BadUnknownOption
                      [Bad (value :unknown)])))

     ;; Positional predicate syntax is no longer supported.
     (error? (eval '(datatype BadPositionalPredicate
                      [Bad (value number?)])))

     ;; Prefix mutable syntax is no longer supported.
     (error? (eval '(datatype BadPrefixMutable
                      [Bad (mutable value number?)])))

     ;; Prefix immutable syntax is no longer supported.
     (error? (eval '(datatype BadPrefixImmutable
                      [Bad (immutable value number?)])))

     )


(mat explicit-datatype-patterns
     (let ()
       (datatype MatchModes [NoFields] [One value] [Two left right] [KeywordField unquote])
       (and (= 42 (match (One 42) [,(datatype MatchModes One :values ,x) x]))
            (= 42 (match (One 42) [,(datatype MatchModes One :fields (value ,x)) x]))
            (equal? '(20 10)
                    (match (Two 10 20)
                      [,(datatype MatchModes Two :fields (right ,r) (left ,l)) (list r l)]))
            (match (Two 10 20) [,(datatype MatchModes Two :fields) #t])
            (match NoFields [,(datatype MatchModes NoFields) #t])
            (match NoFields [,(datatype MatchModes NoFields :values) #t])
            (match NoFields [,(datatype MatchModes NoFields :fields) #t])
            (= 30 (match (KeywordField 30)
                    [,(datatype MatchModes KeywordField :values ,x) x]))
            (= 30 (match (KeywordField 30)
                    [,(datatype MatchModes KeywordField :fields (unquote ,x)) x]))
            (equal? '(30 40)
                    (match (One '(value 30 40))
                      [,(datatype MatchModes One :values (value ,x ...)) x]))))

     ;; Pattern captures may shadow the declared type, variant value, and compiler helper names.
     (let ()
       (datatype ShadowType [ShadowEmpty] [ShadowItem value])
       (and (match (list 99 ShadowEmpty)
              [(,ShadowEmpty ,(datatype ShadowType ShadowEmpty)) #t] [else #f])
            (match (list 99 ShadowEmpty)
              [(,ShadowType ,(datatype ShadowType ShadowEmpty :values)) #t] [else #f])
            (equal? '(10 20)
                    (match (list (ShadowItem 10) (ShadowItem 20))
                      [(,(datatype ShadowType ShadowItem :values ,rtd) ...) rtd]))
            (equal? '(10 20)
                    (match (vector (ShadowItem 10) (ShadowItem 20))
                      [#(,(datatype ShadowType ShadowItem :fields (value ,fields)) ...) fields]))))

     ;; Both wrapper and mode tags are recognized by spelling even when locally bound.
     (let ()
       (datatype TagType [TagItem value])
       (let ([datatype #f] [:fields #f] [:values #f])
         (= 42 (match (TagItem 42) [,(datatype TagType TagItem :values ,x) x])))))

(mat explicit-datatype-pattern-errors
     ;; Missing modes, invalid selectors, duplicate fields, and wrong arities are syntax errors.
     (for-all
      (lambda (pattern)
        (guard (condition [(syntax-violation? condition) #t] [else #f])
          (expand `(let ()
                     (datatype InvalidModes [Empty] [Item value])
                     (match #f [,(list 'unquote pattern) #t] [else #f])))
          #f))
      '((datatype InvalidModes Item)
        (datatype InvalidModes Item ,x)
        (datatype InvalidModes Item :unknown ,x)
        (datatype InvalidModes Item :fields ,x)
        (datatype InvalidModes Item :fields (value ,x extra))
        (datatype InvalidModes Item :fields (value ,x) (value ,y))
        (datatype InvalidModes Item :fields (missing ,x))
        (datatype InvalidModes Item :values)
        (datatype InvalidModes Empty :values ,x)
        (datatype InvalidModes Missing :values)
        (InvalidModes Item ,x)
        (InvalidModes Empty))))

(mat match-datatype-singleton

     (begin (datatype Color [R] [G] [B])
            #t)

     (= 1
        (match R
          [,(datatype Color R) 1]
          [,(datatype Color G) 2]
          [,(datatype Color B) 3]))
     (= 2
        (match G
          [,(datatype Color R) 1]
          [,(datatype Color G) 2]
          [,(datatype Color B) 3]))
     (= 3
        (match B
          [,(datatype Color R) 1]
          [,(datatype Color G) 2]
          [,(datatype Color B) 3]))

     (error? (match 'bla
               [,(datatype Color R) 1]
               [,(datatype Color G) 2]
               [,(datatype Color B) 3]))
     (error? (match 42
               [,(datatype Color R) 1]
               [,(datatype Color G) 2]
               [,(datatype Color B) 3]))

     )


(mat match-datatype

     (begin
       (datatype Tree
         [Nil]
         [Node val (left :predicate Tree?) (right :predicate Tree?)])
       #t)

     ;; match by position
     (eq? 'val
          (match (Node 'val Nil Nil)
            [,(datatype Tree Nil) 'nil]
            [,(datatype Tree Node :values ,x _ _) x])) ;; FIXME

     ;; match by name
     (equal? 2
             (match (Node '(1 2 3) Nil Nil)
               [,(datatype Tree Nil) 'nil]
               [,(datatype Tree Node :fields (val (_ ,x _))) x]))

     (eq? 42
          (match (Node 42 Nil Nil)
            [,(datatype Tree Nil) 'nil]
            [,(datatype Tree Node :fields (val ,v)) v]))
     (eq? Nil
          (match (Node 42 Nil Nil)
            [,(datatype Tree Nil) 'nil]
            [,(datatype Tree Node :fields (right ,v)) v]))
     (equal? (list 42 Nil)
             (match (Node 42 Nil Nil)
               [,(datatype Tree Nil) 'nil]
               [,(datatype Tree Node :values ,v _ ,y) (list v y)]))


     ;; nested patterns

     (begin (datatype Json
                      [JNull]
                      [JBool   b]
                      [JNumber num]
                      [JString str]
                      [JArray  arr]
                      [JObject obj])
            #t)

     (match JNull
       [,(datatype Json JNull) #t])

     (match (JBool #t)
       [,(datatype Json JBool :values ,x) x])

     (match (JBool #f)
       [,(datatype Json JBool :values ,x) (not x)])

     (match (JNumber 42)
       [,(datatype Json JNumber :values ,x)
        (= x 42)])

     (match (JString "jstr")
       [,(datatype Json JString :values ,x)
        (string=? x "jstr")])

     (match (JArray (list JNull (JBool #t) (JBool #f) (JNumber 42) (JString "jstr")))
       [,(datatype Json JArray :values (,(datatype Json JNull)
                       ,(datatype Json JBool :values ,x0)
                       ,(datatype Json JBool :values ,x1)
                       ,(datatype Json JNumber :values ,x2)
                       ,(datatype Json JString :values ,x3)))
        (equal? (list x0 x1 x2 x3)
                (list #t #f 42 "jstr"))])

     (match (JObject (list
                      (cons "Null"   JNull)
                      (cons "True"   (JBool   #t))
                      (cons "False"  (JBool   #f))
                      (cons "Number" (JNumber 42))
                      (cons "String" (JString "jstr"))))
       [,(datatype Json JObject :values ((,n0 . ,(datatype Json JNull))
                        (,n1 . ,(datatype Json JBool :values ,x0))
                        (,n2 . ,(datatype Json JBool :values ,x1))
                        (,n3 . ,(datatype Json JNumber :values ,x2))
                        (,n4 . ,(datatype Json JString :values ,x3))))
        (equal? (list n0 n1 n2 n3 n4)
                (list "Null" "True" "False" "Number" "String"))])

     )



;; good pred name


;; good variant name


;; ensure names are unique


;; variant field mutability


;; vfield predicate


;; (de)serialization
