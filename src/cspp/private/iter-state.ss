(library (cspp private iter-state)
  (export mk-$iter $iter? $iter-next!-proc $iter-reset!-proc $iter-fini-proc
          $iter-children $iter-ops $iter-finalized? $iter-finalized?-set!
          iter-resource? finalize-iter-chain! finalize-iter-chains! reset-iter-chain!)
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
  (define-record-type ($iter mk-$iter $iter?)
    (fields (immutable next!-proc) (immutable reset!-proc) (immutable fini-proc) (immutable children)
            (immutable reset-children) (immutable ops) (mutable finalized?))
    (opaque #t)
    (sealed #t)
    (protocol (lambda (p)
                (case-lambda
                  [(next!-proc reset!-proc fini-proc)
                   (pcheck ([procedure? next!-proc reset!-proc fini-proc])
                           (p next!-proc reset!-proc (if (eq? fini-proc void) #f fini-proc)
                              (lambda () '()) (lambda () '()) (make-list-builder) #f))]
                  [(next!-proc reset!-proc fini-proc children)
                   (p next!-proc reset!-proc fini-proc children children (make-list-builder) #f)]
                  [(next!-proc reset!-proc fini-proc children reset-children)
                   (p next!-proc reset!-proc fini-proc children reset-children
                      (make-list-builder) #f)]
                  [(next!-proc reset!-proc)
                   (pcheck ([procedure? next!-proc reset!-proc])
                           (p next!-proc reset!-proc #f (lambda () '()) (lambda () '())
                              (make-list-builder) #f))]))))

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
  ;; A cleanup candidate is a resource cursor or a wrapper with child links; it
  ;; does not imply resource ownership. Dynamic children are read during this walk,
  ;; so a lazy source's earlier resource-free state cannot hide later acquisition.
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
              (if (hashtable-contains? seen iterator)
                  (hashtable-ref seen iterator #f)
                  (begin
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
                      resource?)))))
          (for-each visit iterators)))))

  (define finalize-iter-chain!
    (lambda (iterator)
      (unless (and (not ($iter-fini-proc iterator))
                   (null? (($iter-children iterator))))
        (finalize-iter-chains! (list iterator)))))
)
