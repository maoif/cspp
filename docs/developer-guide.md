# CSPP Developer Guide

## API design considerations

Support concurrent independent operations in both threads and fibers, including fiber
suspension and worker migration. Minimize shared internal state: keep traversal progress,
counters, accumulators, and scratch storage local to each invocation or active pass.
Synchronize unavoidable internal sharing, such as registry updates. Library-created sharing
must not make otherwise independent calls interfere.

### Independent and dependent operations

Operations are **independent** when their accesses to mutable state do not require an ordering
between them. They may share immutable descriptions, pure callbacks, and unchanged inputs.
Operations are **dependent** when they require ordering, such as two calls advancing one
iterator, or one call reading data another modifies. Distinct roots can still share a mutable
child, and callbacks can introduce dependencies through captured data.

| Example | Relationship |
| --- | --- |
| Separate iterators over the same unchanged list | Independent |
| The same reusable transducer with separate per-run state and accumulators | Independent |
| The same pure navigator path selecting from unchanged inputs | Independent |
| Mutating separate object graphs with independent callbacks | Independent |
| Advancing, resetting, or finalizing the same active iterator | Dependent |
| Callbacks updating the same counter or mutable container | Dependent |
| Mutating a child shared by otherwise separate inputs | Dependent |

Callers must order or synchronize conflicting dependent operations. Unsynchronized sharing of
caller-owned mutable state is an allowed caller bug, even with one fiber worker; APIs need not
automatically serialize or detect it. Sharing read-only data alone is not a bug. Hidden global
traversal counters, shared scratch buffers, and races in internal registries are library bugs.

### Reusable descriptions and pure navigator paths

A **pure navigator path** is a fully constructed path with stable configuration and captured
dependencies, side-effect-free navigation callbacks, and no shared mutable traversal progress.
Traversal state belongs to each invocation. Custom steps and predicates must satisfy the same
conditions; purity is a semantic contract, not something `nav-path?` verifies.

The same pure path must allow concurrent use. A reusable transducer description must likewise
allocate its state separately for each application, even for stateful transformations such as
`ttake`.

```scheme
(define positive-items (nav/path nav/all (nav/pred positive?)))
(define first-two (ttake 2))

(run-threads
 2
 (lambda () (nav-select positive-items '(1 -2 3)))
 (lambda () (nav-select positive-items '(-4 5 6))))
;; => #((1 3) (5 6))

(run-threads
 2
 (lambda () (into 'list first-two '(1 2 3)))
 (lambda () (into 'list first-two '(10 20 30))))
;; => #((1 2) (10 20))
```

A pure path does not make `nav-transform!` pure: concurrent mutations still require independent
targets or caller synchronization. A predicate that updates a captured counter introduces
caller-owned shared state, even when the inputs differ.

### Iterator resources and terminal operations

An **iterator** is a destructive cursor. A **pass** runs from construction or reset until
exhaustion or terminal completion. An **intermediate combinator** configures or wraps a cursor;
a **terminal operation** consumes it and returns a result or performs effects.
A **chain** includes a cursor and all upstream iterators it reaches through combinators.

A **resource** is a handle needing explicit cleanup, such as an opened file or port.
**Ownership** means responsibility for finalizing that resource. Resource-free iterators such
as `list->iter` and `vector->iter` need no ownership or finalization: finalization is a no-op,
and reset makes them reusable. Reset retains the configured pipeline and restarts its counters;
it does not reset mutable state captured by user callbacks.

Intermediate exhaustion and source switching keep resources open. Only successful terminal
completion, including early termination, finalizes the entire chain; explicit `iter-finalize!`
is also available. Repeated references to one resource handle close it once. Borrowed ports
remain the caller's responsibility. Errors and nonlocal exits leave chain state undefined;
automatic cleanup, rollback, and recovery are not promised.

Sequential concatenation supports repeated iterator handles. A first occurrence starts at its
current position; later occurrences reset it before use. For example, `(iter-concat iter iter
iter)` traverses a list cursor three times. With `(iter-concat lines lines)`, a file cursor reads
its remaining lines, seeks back, then reads the whole file; the consuming terminal closes it
once. Nonseekable streams cannot restart. Concurrent operations on one chain, shared source,
or aliased resource are dependent and require caller ordering or synchronization.

Document each API's kind and behavior: constructors versus terminals, in-place pipeline
configuration versus wrappers, lazy versus eager buffering, per-pass state, reset behavior,
and resource responsibility. A reusable description does not make a captured destructive
source reusable or independent.

### Continuation resumption and support boundaries

Continuation resumption restores control and lexical bindings, not snapshots of mutable
storage. An API's mutable counters, buffers, caches, heap updates, callback effects, I/O, and
destructive iterator consumption need not be fully restored on continuation resumption. Prefer
loop parameters for traversal bookkeeping, and document mutable state and its behavior on
continuation resumption. Ordinary fiber suspension must still preserve a live operation's
invariants and documented results.

Do not reject general continuation resumption merely because restoring all heap state would be
difficult. Conversely, continuation resumption does not reopen finalized resources or undo reset,
mutation, or other dependent operations. Document restrictions for such lifecycle interactions.

Keep support bounded. Do not require automatic rollback, deep alias tracking, or restoration of
every container that shares a key. Require hash, equivalence, and ordering dependencies to stay
stable while keys or members are indexed, unless an API explicitly promises otherwise. This
includes mutation through aliases or captured comparator state; identity hashing can remain
stable when object contents change. Violating a documented stability requirement is a caller
bug. Behavior explicitly supported by an API remains the library's responsibility. Document
partial updates on failure and unsupported edge cases instead of adding complex recovery
machinery by default.

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

Ordinary fiber suspension normally resumes a saved continuation once. General continuation
resumption can revisit earlier execution repeatedly, so code can be fiber-safe while failing this
stronger condition. Chez Scheme's `call/cc` captures a general continuation. `call/1cc` captures a
one-shot continuation; an active one-shot continuation is promoted to a general continuation
when `call/cc` captures it as part of its continuation.

For example:

```scheme
(import (cspp))

(let ([resume #f] [resumed? #f])
  (let ([result
         (for/list ([i :fxiota 3])
           (if (= i 1)
               ;; Save the continuation before collecting this result.
               (call/cc
                (lambda (continuation)
                  (set! resume continuation)
                  i))
               i))])
    (if resumed?
        result
        (begin
          (set! resumed? #t)
          (resume 9)))))
;; => (0 9 2)
```

Continuation resumption restores the traversal point at item `1`. The second traversal
collects `9` and continues with `2`. Loop parameters restore positions, indexes, accumulator
bindings, list tails, and vector block cursors. A `:length` limit remains effective on each
resumed path, and vector writes stay within bounds. These guarantees do not restore mutable
storage.

Prefer loop parameters for internal counters and cursors. If a counter must be mutable,
document where it lives, what advances it, and that continuation resumption does not reset it.

List cells, output heads, vector buffers, and cached block links remain shared by saved
continuations. Continuation resumption may overwrite earlier results or detach list
prefixes; earlier returned containers are not guaranteed to remain unchanged. An unbounded
vector collector copies its current buffered prefix at finalization, while a preallocated
vector is reused directly. Callers who retain results across continuation resumption must
account for this sharing and any intervening mutation, just as with a raw named loop that
writes to outer mutable storage.

`match` must preserve matching progress and accumulated captures across continuation
resumption without accidentally duplicating captures or internally modifying an earlier
returned capture list. Compiler-generated loop bookkeeping should travel with the resumable
computation rather than remain in mutable locations that later traversal has already
advanced.

Continuation safety does not mean rolling back caller assignments, I/O, or consumption of a
destructive iterator. For a `:iter` source, an already-prefetched current item should
survive continuation resumption, but future reads use the iterator's current state; the loop
does not implicitly rewind the iterator. Explicit user mutation of a capture also follows
ordinary Scheme binding semantics rather than being undone automatically.

### Review and verification

Exercise suspension in source guards, callbacks, conditions, and bodies. Check both one-worker
interleaving and execution on multiple workers. Test independent calls separately from shared
bindings or shared caller-owned objects.

For continuation tests, capture in the middle of a traversal, allow it to finish, then resume
the saved continuation repeatedly. Verify restored positions, counters, cursors, and length
limits separately from the documented effects on shared result objects. Use nondestructive
sources to isolate bookkeeping defects from consumption of an external iterator.

Chez Scheme documents continuation semantics in
[Control Structures](https://cisco.github.io/ChezScheme/csug/control.html) and primitive thread
safety in [Thread System](https://cisco.github.io/ChezScheme/csug/threads.html).
