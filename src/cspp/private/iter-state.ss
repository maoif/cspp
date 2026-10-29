(library (cspp private iter-state)
  (export mk-$iter $iter? $iter-next!-proc $iter-reset!-proc $iter-fini-proc
          $iter-children $iter-ops $iter-finalized? $iter-finalized?-set!
          $iter-resource-capable? iter-resource? finalize-iter-chain!
          finalize-iter-chains! reset-iter-chain!)
  (import (chezscheme) (cspp list) (cspp utils))

  ;; A chain is an iterator and the upstream iterators reachable through children.
  ;; Ownership means responsibility for an actual resource's finalizer. Ordinary
  ;; cursors have no finalizer. Child links carry cleanup responsibility without
  ;; requiring exclusive handles or retaining an ownership graph of user data.
  ;; Restart dependencies are the cursors a wrapper rewinds before resetting its
  ;; local state. They normally equal cleanup children; sequences retain historical
  ;; nested resources for cleanup but restart only their upstream source. Private
  ;; reset callbacks reset local state only; public custom producers keep their own
  ;; reset callbacks, which may manage opaque upstream state.
  ;; Resource capability is immutable conservative metadata: #f proves the graph
  ;; permanently resource-free; #t means it may reach a resource now or later. It
  ;; is not ownership and does not disable a cursor. Fixed child lists combine
  ;; direct children's capability once; callbacks for dynamic child links remain
  ;; capable even when currently empty. Avoid per-pass retention of free wrappers
  ;; without repeating full graph classification or missing later acquisitions.
  #|proc:mk-$iter
  Construct private iterator state. `next!-proc` has signature `() -> value-or-iter-end`;
  `reset!-proc` has signature `() -> unspecified` and resets local state. Optional
  `fini-proc` has signature `() -> unspecified`; omit it or use `void` for a free leaf.
  With a fourth argument, `fini-proc` is a finalizer or `#f`, and `children` is either a
  fixed iterator list or `() -> iterator-list` for dynamic cleanup/restart dependencies.
  A fixed list must stay unchanged and permits permanent resource-free classification.
  The five-argument form takes separate dynamic cleanup `children` and `reset-children`
  callbacks, both `() -> iterator-list`. Dynamic graphs remain resource-capable even
  when currently empty. Return a cursor; callbacks may suspend, and each cursor owns
  its own pipeline state. Kind: private state constructor.
  |#
  (define-record-type ($iter mk-$iter $iter?)
    (fields (immutable next!-proc) (immutable reset!-proc) (immutable fini-proc) (immutable children)
            (immutable reset-children) (immutable ops) (mutable finalized?)
            (immutable resource-capable?))
    (opaque #t)
    (sealed #t)
    (protocol (lambda (p)
                (case-lambda
                  [(next!-proc reset!-proc fini-proc)
                   (pcheck ([procedure? next!-proc reset!-proc fini-proc])
                           (let ([fini-proc (if (eq? fini-proc void) #f fini-proc)])
                             (p next!-proc reset!-proc fini-proc
                                (lambda () '()) (lambda () '()) (make-list-builder) #f
                                (bool fini-proc))))]
                  [(next!-proc reset!-proc fini-proc children)
                   (if (list? children)
                       (let ([child-proc (lambda () children)])
                         (p next!-proc reset!-proc fini-proc child-proc child-proc
                            (make-list-builder) #f
                            (or (bool fini-proc) (bool (exists $iter-resource-capable? children)))))
                       (p next!-proc reset!-proc fini-proc children children
                          (make-list-builder) #f #t))]
                  [(next!-proc reset!-proc fini-proc children reset-children)
                   (p next!-proc reset!-proc fini-proc children reset-children
                      (make-list-builder) #f #t)]
                  [(next!-proc reset!-proc)
                   (pcheck ([procedure? next!-proc reset!-proc])
                           (p next!-proc reset!-proc #f (lambda () '()) (lambda () '())
                              (make-list-builder) #f #f))]))))

  ;; One explicit identity set belongs to each reset operation, including when
  ;; callbacks suspend/migrate. Postorder resets sources before local rebuilding
  ;; (e.g. sorted snapshots). Shared nodes reset once, not once per incoming path.
  ;; No ambient handler can leak between independent operations or continuations.
  (define reset-iter-chain!
    (lambda (iterator)
      (define reset-local!
        (lambda (iterator)
          (($iter-reset!-proc iterator))
          (for-each (lambda (op) (vector-set! op 3 (vector-ref op 2)))
                    (($iter-ops iterator)))))
      (define check-live!
        (lambda (iterator)
          (when ($iter-finalized? iterator)
            (errorf 'iter-reset! "finalized iterator cannot be reset!"))))
      (check-live! iterator)
      (let ([children (($iter-reset-children iterator))])
        (if (null? children)
            (reset-local! iterator)
            (let ([seen (make-eq-hashtable)])
              (let visit ([iterator iterator])
                (unless (hashtable-contains? seen iterator)
                  (hashtable-set! seen iterator #t)
                  (check-live! iterator)
                  (for-each visit (($iter-reset-children iterator)))
                  (reset-local! iterator))))))))

  ;; Repeated handles form a DAG (a directed acyclic graph), not necessarily a tree.
  ;; Memoize negative answers too: nested (iter-concat previous previous) has only
  ;; linear unique handles but exponentially many paths to a resource-free leaf.
  (define iter-resource?
    (lambda (iterator)
      (if ($iter-fini-proc iterator)
          #t
          (let ([children (($iter-children iterator))])
            (and (pair? children)
                 (let ([seen (make-eq-hashtable)])
                   (let visit ([iterator iterator])
                     (cond [(hashtable-contains? seen iterator)
                            (hashtable-ref seen iterator #f)]
                           [else
                            (hashtable-set! seen iterator #f)
                            (let ([resource? (or (and ($iter-fini-proc iterator) #t)
                                                 (exists visit (($iter-children iterator))))])
                              (hashtable-set! seen iterator resource?)
                              resource?)]))))))))

  ;; A forest is the collection of root cursors and their reachable child graphs.
  ;; One forest walk shares identity memoization across every retained root.
  ;; A cleanup candidate is a cursor capable of reaching a resource, including a
  ;; currently empty dynamic source; it does not imply present resource ownership.
  ;; Dynamic children are read during this walk, so earlier resource-free state
  ;; cannot hide later acquisition. Permanently free graphs need no walk.
  #|proc:finalize-iter-chains!
  Finalize the resources reachable from the list of iterator roots `iterators`, sharing
  identity memoization across all roots. Return an unspecified value. Resource-free
  cursors remain reusable; finalizer exceptions leave partial cleanup state undefined.
  Kind: private terminal cleanup helper; candidates may include resource-free wrappers.
  |#
  (define finalize-iter-chains!
    (lambda (iterators)
      (unless (null? iterators)
        (let ([seen (make-eq-hashtable)])
          (define visit
            (lambda (iterator)
              (cond [(not ($iter-resource-capable? iterator)) #f]
                    [(hashtable-contains? seen iterator) (hashtable-ref seen iterator #f)]
                    [else
                     (hashtable-set! seen iterator #f)
                     (let* ([children (($iter-children iterator))]
                            ;; Visit every child even if an earlier one has resources.
                            [child-resources? (fold-left
                                               (lambda (found? child) (or (visit child) found?))
                                               #f children)]
                            [resource? (or ($iter-fini-proc iterator) child-resources?)])
                       (unless ($iter-finalized? iterator)
                         ;; Mark before user cleanup; errors leave state undefined.
                         (when resource? ($iter-finalized?-set! iterator #t))
                         (when ($iter-fini-proc iterator) (($iter-fini-proc iterator))))
                       (hashtable-set! seen iterator resource?)
                       resource?)])))
          (for-each visit iterators)))))

  (define finalize-iter-chain!
    (lambda (iterator)
      (when ($iter-resource-capable? iterator)
        (finalize-iter-chains! (list iterator)))))
)
