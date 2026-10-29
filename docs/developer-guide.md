# CSPP Developer Guide

## Safety contracts

Fiber safety, thread safety, and continuation safety describe different ways execution can
overlap or revisit earlier work. Review each property separately: satisfying one does not
establish the others. The examples below describe required behavior, rather than asserting
that every existing implementation already satisfies these contracts.

### Fiber safety

An operation is **fiber-safe** when suspending a fiber, interleaving other fibers, and resuming
the suspended fiber preserve the operation's invariants and documented behavior. In CSPP,
suspension can result from an explicit yield, a blocking operation, or timer preemption.
A fiber can resume on a different worker thread.

Even with one worker, another fiber can observe shared state while an operation is suspended.
Treat calls to user procedures as possible suspension points. Migration also means that a
worker's identity or thread-local scheduler state must not be mistaken for a fiber's identity.

For example:

```scheme
(import (cspp) (cspp concurrency fiber))

(run-fibers
 (lambda ()
   (for/vector ([i :fxiota 3])
     (fiber-yield)
     (match (vector i)
       [#(,x) (+ x 10)])))
 2)
;; => #(10 11 12)
```

For `match`, suspension must preserve the current candidate, captures, and matching progress.
For `for` and its variants, it must preserve source positions, counters, accumulators, and
collection state without losing or duplicating results. Independent invocations must keep
their internal state separate.

Shared lazy captures require caller synchronization. Suppose one fiber starts materializing a
capture, suspends, and another assigns that capture. The initializer can overwrite that
assignment when it resumes. This unsynchronized use is a caller bug, even with one worker.
Serialize the binding's first reads and assignments, or materialize it before publishing it
for read-only access. Independent invocations do not share capture bindings.

### Thread safety

An operation is **thread-safe** when simultaneous execution on multiple OS threads preserves
its invariants and documented behavior under the API's ownership and synchronization rules.
This includes correct publication and visibility of shared internal state.

For example, separate invocations must not share traversal or collection bookkeeping:

```scheme
(import (cspp))

(define (collect base)
  (for/list ([i :fxiota 3])
    (+ base i)))

(run-threads
 2
 (lambda () (collect 0))
 (lambda () (collect 10)))
;; => #((0 1 2) (10 11 12))
```

Concurrent `match` invocations must likewise keep their captures and regex match results
independent. Sharing one capture binding through closures follows the caller synchronization
rules documented by `match`. A lazy first read mutates internal state, so even concurrent
first reads require synchronization. Without it, readers can obtain different list objects,
and initialization can overwrite an assignment. These outcomes are allowed for unsupported,
unsynchronized sharing; they are not library safety defects.

Thread safety does not make a whole loop or match atomic. It also does not automatically
permit concurrent mutation of caller-owned inputs. Sharing a destructive iterator or mutable
container requires the ownership or synchronization rules documented by that object's API.

### Continuation safety

An operation is **continuation-safe** when capturing and invoking a continuation preserve its
intended behavior. Support for general continuations includes invoking a saved continuation
more than once, including after the original traversal has completed.

Ordinary fiber suspension normally resumes a saved continuation once. Replaying a general
continuation revisits earlier execution, so code can be fiber-safe while failing this stronger
condition. Chez Scheme's `call/cc` captures a general continuation. `call/1cc` captures a
one-shot continuation; an active one-shot continuation is promoted to a general continuation
when `call/cc` captures it as part of its continuation.

For example:

```scheme
(import (cspp))

(let ([resume #f] [replayed? #f])
  (let ([result
         (for/list ([i :fxiota 3])
           (if (= i 1)
               ;; Save the continuation before collecting this result.
               (call/cc
                (lambda (continuation)
                  (set! resume continuation)
                  i))
               i))])
    (if replayed?
        result
        (begin
          (set! replayed? #t)
          (resume 9)))))
;; => (0 9 2)
```

Re-entry restores the traversal point at item `1`. The second traversal collects `9` and
continues with `2`. Loop parameters restore positions, indexes, accumulator bindings, list
tails, and vector block cursors. A `:length` limit remains effective on each replayed path,
and vector writes stay within bounds. These guarantees do not restore mutable storage.

Prefer loop parameters for internal counters and cursors. If a counter must be mutable,
document where it lives, what advances it, and that continuation re-entry does not reset it.

List cells, output heads, vector buffers, and cached block links remain shared by saved
continuations. Replay may overwrite earlier results or detach list prefixes; earlier returned
containers are not guaranteed to remain unchanged. An unbounded vector collector copies its
current buffered prefix at finalization, while a preallocated vector is reused directly.
Callers who retain results across replay must account for this sharing and any intervening
mutation, just as with a raw named loop that writes to outer mutable storage.

`match` must preserve matching progress and accumulated captures across re-entry without
accidentally duplicating captures or internally modifying an earlier returned capture list.
Compiler-generated loop bookkeeping should travel with the resumable computation rather than
remain in mutable locations that later traversal has already advanced.

Continuation safety does not mean rolling back caller assignments, I/O, or consumption of a
destructive iterator. For a `:iter` source, an already-prefetched current item should survive
re-entry, but future reads use the iterator's current state; the loop does not implicitly
rewind the iterator. Explicit user mutation of a capture also follows ordinary Scheme binding
semantics rather than being undone automatically.

### Review and verification

Exercise suspension in source guards, callbacks, conditions, and bodies. Check both one-worker
interleaving and execution on multiple workers. Test independent calls separately from shared
bindings or shared caller-owned objects.

For continuation tests, capture in the middle of a traversal, allow it to finish, then replay
the saved continuation repeatedly. Verify restored positions, counters, cursors, and length
limits separately from the documented effects on shared result objects. Use nondestructive
sources to isolate bookkeeping defects from consumption of an external iterator.

Chez Scheme documents continuation semantics in
[Control Structures](https://cisco.github.io/ChezScheme/csug/control.html) and primitive thread
safety in [Thread System](https://cisco.github.io/ChezScheme/csug/threads.html).
