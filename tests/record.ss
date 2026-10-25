(import (cspp))


(mat simple-record

     (begin (record Person (name age sex))
            #t)
     (procedure? Person)
     (procedure? Person-age)
     (procedure? Person-name)
     (procedure? Person-sex)
     (procedure? Person?)

     (Person? (Person "J" 14 'male))
     (not (Person? (list)))

     (= (Person-age (Person "J" 14 'male))
        14)
     (equal? (Person-name (Person "J" 14 'male))
             "J")
     (eq? (Person-sex (Person "J" 14 'male))
          'male)

     )

(mat record-with-custom-type-predicate

     (begin (record Dog A-Dog? (name age sex))
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     (begin (record Dog A-Dog? ([name :predicate string?]
                                [age :predicate natural?]
                                sex))
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     )


(mat record-with-predicate

     (begin (define sex? (lambda (x) (or (eq? x 'male) (eq? x 'female))))
            (record Person
                    ([name :predicate string?]
                     [height :predicate natural?]
                     [sex :predicate sex?]
                     [age :predicate natural?]))
            #t)
     (error? (Person 'Jack 170 'male 19))
     (error? (Person "Jack" -170 'male 19))
     (error? (Person "Jack" 170 'mmmale 19))
     (error? (Person "Jack" 170 'male -19))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person? p))

     )


(mat record-with-mutability

     (begin (define sex? (lambda (x) (or (eq? x 'male) (eq? x 'female))))
            (record Person
                    ([name :mutable :predicate string?]
                     [height :mutable :predicate natural?]
                     [sex :immutable :predicate sex?]
                     [age :immutable :predicate natural?]))
            #t)
     (error? (Person 'Jack 170 'male 19))
     (error? (Person "Jack" -170 'male 19))
     (error? (Person "Jack" 170 'mmmale 19))
     (error? (Person "Jack" 170 'male -19))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person? p))

     (error? (let ([p (Person "Jack" 170 'male 19)])
               (Person-name-set! p 'sym)))
     (error? (let ([p (Person "Jack" 170 'male 19)])
               (Person-height-set! p -60)))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person-name-set! p "New Jack")
       (equal? (Person-name p) "New Jack"))
     (let ([p (Person "Jack" 170 'male 19)])
       (Person-height-set! p 60)
       (= (Person-height p) 60))

     )


(mat record-with-keyword-field-options

     (begin (define sex? (lambda (x) (memq x '(male female))))
            (record KeywordPerson
                     ([name :predicate string?]
                      [age :predicate natural? :mutable]
                      [sex :immutable :predicate sex?]
                      [tag :mutable]
                      id))
            #t)

     (KeywordPerson? (KeywordPerson "Jack" 19 'male 'new 42))

     ;; The name predicate rejects non-strings.
     (error? (KeywordPerson 42 19 'male 'new 42))

     ;; The age predicate rejects negative integers.
     (error? (KeywordPerson "Jack" -19 'male 'new 42))

     ;; The sex predicate rejects values outside its accepted set.
     (error? (KeywordPerson "Jack" 19 'other 'new 42))

     (let ([p (KeywordPerson "Jack" 19 'male 'new 42)])
       (KeywordPerson-age-set! p 20)
       (KeywordPerson-tag-set! p 'updated)
       (and (= 20 (KeywordPerson-age p))
            (eq? 'updated (KeywordPerson-tag p))))

     ;; The mutable age setter applies its predicate.
     (error? (let ([p (KeywordPerson "Jack" 19 'male 'new 42)])
               (KeywordPerson-age-set! p -1)))

     ;; Default-immutable fields do not generate setters.
     (error? (eval 'KeywordPerson-name-set!))

     ;; Explicitly immutable fields do not generate setters.
     (error? (eval 'KeywordPerson-sex-set!))

     ;; Bare fields remain immutable by default.
     (error? (eval 'KeywordPerson-id-set!))

     ;; Duplicate :predicate option is invalid.
     (error? (eval '(record BadDuplicatePredicate
                           ([value :predicate number? :predicate integer?]))))

     ;; Duplicate :mutable option is invalid.
     (error? (eval '(record BadDuplicateMutable
                           ([value :mutable :mutable]))))

     ;; :mutable and :immutable conflict.
     (error? (eval '(record BadConflictingMutability
                           ([value :mutable :immutable]))))

     ;; :predicate requires a predicate identifier.
     (error? (eval '(record BadMissingPredicate
                           ([value :predicate]))))

     ;; Unknown field options are invalid.
     (error? (eval '(record BadUnknownOption
                           ([value :wat]))))

     ;; Positional predicate syntax is no longer supported.
     (error? (eval '(record BadPositionalPredicate
                           ([value number?]))))

     ;; Prefix mutable syntax is no longer supported.
     (error? (eval '(record BadPrefixMutable
                           ([mutable value number?]))))

     ;; Prefix immutable syntax is no longer supported.
     (error? (eval '(record BadPrefixImmutable
                           ([immutable value number?]))))

     )


(mat record-match

     (begin
       (record Person (name age sex))
       #t)

     (Person? (Person "Jack" 16 'male))
     (not (Person? 1))

     (equal? "Jack"
             (match (Person "Jack" 16 'male)
               [,(record Person :fields (name ,x)) x]))

     (equal? (list "Jack" 16 'male)
             (match (Person "Jack" 16 'male)
               [,(record Person :values ,name ,age ,sex) (list name age sex)]))
     (equal? (list "Jack" 'male)
             (match (Person "Jack" 16 'male)
               [,(record Person :values ,name _ ,sex) (list name sex)]))
     (equal? (list 16 "Jack")
             (match (Person "Jack" 16 'male)
               [,(record Person :values ,name ,age _) (list age name)]))

     (equal? 'small
             (match (Person "Jack" 23 'male)
               [,(record Person :values ,name ,age _)
                (guard (< age 24))
                'small]
               [,(record Person :values ,name ,age _)
                (guard (< age 50))
                'big]
               [else 'old]))
     (equal? 'old
             (match (Person "Jack" 51 'male)
               [,(record Person :values ,name ,age _)
                (guard (< age 24))
                'small]
               [,(record Person :values ,name ,age _)
                (guard (< age 50))
                'big]
               [else 'old]))

     (equal? (list "Jack" 16 'male)
             (match (Person "Jack" 16 'male)
               [,(record Person :values ,name ,age ,sex) (list name age sex)]))

     (error? (match 42
               [,(record Person :values ,a ,b ,c) a]))
     (error? (match '(1 2 3)
               [,(record Person :values ,name ,age _)
                (guard (< age 24))
                'small]
               [,(record Person :values ,name ,age _)
                (guard (< age 50))
                'big]))

     ;; nested patterns
     (begin
       (record Triple (a b c))
       #t)

     (equal? '(2 3 42)
             (match (Triple (Triple 1 1 1)
                            (Triple (Triple 2 2 2)
                                    (Triple 3 3 3)
                                    (Triple (Triple 1 1 1)
                                            (Triple 1 1 1)
                                            (Triple 1 1 42)))
                            (Triple 1 1 1))
               [,(record Triple :values ,(record Triple :values 1 1 1)
                         ,(record Triple :values ,(record Triple :values 2 ,x0 2)
                                  ,(record Triple :values 3 ,x1 3)
                                  ,(record Triple :values ,p3
                                           ,p4
                                           ,(record Triple :values _ _ ,x2)))
                         ,(record Triple :values 1 1 1))
                (list x0 x1 x2)]))

     (equal? (list "Jack0" "Jack1" "Jack00" "Jack11")
             (match (Triple (Person "Jack0" 16 'male)
                            (Person "Jack1" 17 'male)
                            (Triple (Person "Jack00" 18 'male)
                                    (Person "Jack11" 19 'male)
                                    (Person "Jack21" 20 'male)))
               [,(record Triple :values ,(record Person :values ,n0 _ _)
                         ,(record Person :values ,n1 _ _)
                         ,(record Triple :values ,(record Person :values ,n2 _ _)
                                  ,(record Person :values ,n3 _ _)
                                  _))
                (list n0 n1 n2 n3)]))

     )



;; good pred name


;; ensure names are unique


;; (de)serialization
