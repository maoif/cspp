# Test and benchmark usage requirements

`(cspp test)` and `(cspp benchmark)` coordinate synchronous execution from one controlling
OS thread, outside a fiber scheduler. Here, “top-level” describes execution ownership;
calls may appear in ordinary helper procedures and documented nested suite forms.
Standalone assertions and the other exceptions listed below may run in workers.

## Which APIs may run in workers?

| APIs | Thread and fiber requirements |
| --- | --- |
| `test-assert`, `test-assert-equal`, `test-assert-raises`, `test-assert-not-raises`, `test-true`, `test-false`, `test-eq`, `test-eqv`, `test-equal`, `test-=`, `test-pred`, `test-raises`, `test-not-raises`, `test-fail` | Independent calls may run concurrently in any OS thread or fiber, including while a controlling test run is active. |
| `make-test-failure`, `test-failure?`, `test-failure-message`, `test-failure-expected`, `test-failure-actual` | May construct and inspect failures in any thread or fiber. |
| Record predicates and read-only descriptor, config, concrete-case, result, sample, comparison, and summary accessors | May inspect already-published values from any thread or fiber. Referenced lists, statistics, and other mutable payloads must remain stable while read. |
| `chez-version` and the `chez-version` comparison procedures; `benchmark-absolute-difference`, `benchmark-percent-difference`, `benchmark-do-not-optimize`, `benchmark-clobber-memory` | May run independently in any thread or fiber. Optimization barriers do not synchronize shared memory. |
| All remaining APIs | Use from the controlling thread. This includes definition macros, registries and registry reads, runners, selection and expansion, configuration construction, capture, reporting, baselines, summarization and result comparison, and benchmark-state APIs. |

Worker-safe assertions do not synchronize their arguments or user callbacks. For example,
`test-equal` requires objects that can safely be compared concurrently, and `test-pred`
requires a predicate safe for the caller's execution context. Synchronize shared mutable
inputs and any shared state accessed by predicates or thunks.

Coordinator APIs have undefined behavior when used concurrently, from worker threads or
fibers, through overlapping independent runs or reentrant reporting, or through continuations
that escape and later reenter their execution. Ordinary return and exception propagation
are supported. These restrictions do not prohibit ordinary fiber suspension inside a
standalone assertion's predicate or thunk; that assertion has no runner-owned resources.

## Worker assertions and failure propagation

Assertions return `#t` on success and raise a `test-failure` condition in the calling
thread or fiber on failure. `test-fail` always raises. They do not register a result,
write a report, or send a failure to another thread. Invalid arguments and exceptions
raised by user predicates can instead raise ordinary error conditions.

The controlling body must wait for its workers and propagate their failures before
returning. `thread-join` only waits; it does not relay a worker's exception. CSPP's
`run-threads` waits and stores either each thunk's return value or its raised object in
the returned vector. For workers consisting of successful assertions, `#t` is the success
value, so any other outcome must be raised in the controlling body:

```scheme
(import (cspp))

(test-case parallel-addition
  (let ([outcomes
         (run-threads 2
           (lambda () (test-= 2 (+ 1 1)))
           (lambda () (test-= 4 (+ 2 2))))])
    (vector-for-each
      (lambda (outcome)
        (unless (eq? outcome #t)
          (raise outcome)))
      outcomes)))
```

`run-fibers` may likewise be called by a controlling body, with assertions inside its
workers. An uncaught fiber failure is raised to the caller after the scheduler's workers
join. The init fiber must wait for the work it intends to test: init completion stops
pending child work. Call the test or benchmark runner outside `run-fibers`.

## Callbacks, fixtures, and captured output

Bodies may test concurrent code, but every worker must finish before the body returns
or propagates a failure. Workers may use the worker-safe APIs above. They must not
register cases, start runners, operate on benchmark state, or report results. Copy needed
benchmark-state values into ordinary local bindings before starting workers; collect
worker measurements and update counters in the controlling body after joining them.

Test fixture initializers, `:before`, `:after`, case policy predicates, and `:condition-handler`
run synchronously in the controlling thread. The custom condition handler applies only to
conditions from the tested runtime, expansion, or compilation body. A supplied `:raises`
predicate controls exception matching; missing or rejected conditions and predicate errors
bypass the custom handler. Preparation, fixture,
`:before`, and `:after` failures use the normal failure policy and cannot become handled passes.
Allowed lifecycle warnings resume their hooks independently of the body handler.
An unhandled body failure takes precedence over a cleanup failure. When the custom handler
accepts the body condition, a separate cleanup failure still determines the case outcome.
Test cleanup receives successfully acquired fixtures even when a later initializer or `:before`
fails. A benchmark sample's teardown runs after the body, even without setup. If a supplied setup
raises, setup must release its own partial resources and sample teardown is skipped.
Benchmark suite teardown also runs after failed suite setup, receiving `#f` as its setup
value. Cleanup must follow these different lifecycle contracts.

Case output capture surrounds the body and runtime fixture hooks; skip and expected-failure
rules and subsequent condition classification run outside capture. Standalone
`test-capture-ports` surrounds only its thunk. Capture redirects the controlling thread's
current output ports. Native child threads can inherit those ports; they must finish before capture closes them. Synchronize
concurrent writes to a shared port, or give workers separate ports and combine their
output after joining. Capture does not provide atomic writes or automatically collect
output from arbitrary workers. Independently created workers and fiber migration must
not be assumed to preserve the controlling thread's current-port bindings.

Built-in reporters require open textual output ports. A custom reporter may use a binary
output port if its callbacks support it. Reporter callbacks execute synchronously and must
return normally or raise; do not reenter reporting from a callback or a custom port writer.

## Benchmark measurement scope

CPU time measures the controlling OS thread. Elapsed time includes time spent waiting
for workers. Allocation and garbage-collection statistics are process-wide, so worker
and unrelated background allocations can contribute to the sample. Pausing timing
subtracts the corresponding statistics interval with these same scopes; it does not
isolate a thread or process. CPU time does not aggregate the CPU consumed by workers.

Benchmark state belongs to its controlling sample. Counter updates, user-data mutation,
and timing pause/resume are unsynchronized and must stay in the controlling thread.
Result counter lists are snapshots of completed samples; arbitrary user data and other
mutable payloads are not deeply copied.
