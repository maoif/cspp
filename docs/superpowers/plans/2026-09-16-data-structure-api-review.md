# Data-Structure API Review Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make CSPP custom data structures consistently sized, searchable, documented,
iterable, transducible, and navigable while completing the reviewed string and bit-set APIs.

## Current Status (2026-09-18)

Tasks 1 through 7 are implemented in this worktree through the integration commits and
the follow-up navigator deletion-callback fix. The focused reviewed tests, clean build,
documentation checker, Scheme balance checks, and ChezScheme header comparison pass.
The aggregate test suite retains one unrelated, timing-dependent
`net-websocket-phase4-features` failure (`websocket pong timed out`); focused
`net-websocket` passes and this branch has no networking changes.

The implementation review is now recorded in `docs/data-structure-api-review.md`.
The remaining iterator follow-ups are zero/negative-step validation, finalized-iterator
guards, the unfinished hashtable path, source-conversion naming, port ownership, adapter
snapshot semantics, and duplicate-registration policy.

**Architecture:** Expose extension-registration APIs from `(cspp iter)` and
`(cspp navigator)`. Each traversable data-structure library imports those APIs and
registers its own iter and navigator adapters during its own library initialization, with a
distinct navigator set protocol for set-like structures. Transducers consume the same iter
registry as their fallback path.

**Tech Stack:** ChezScheme libraries, `pcheck`, `mat` tests, `make clean && make`, and the
existing public-documentation checker in `tools/check-public-api-docs.ss`.

**Spec:** `docs/data-structure-api-review.md`

## Global Constraints

- Use `X-size` for every custom data structure; `array-length` and `dlist-length` are not
  public APIs after migration.
- Use only `lambda` and `case-lambda` in procedure definitions; do not add keyword arguments.
- Every exported procedure and macro has adjacent `#|proc:name` or `#|macro:name` Markdown
  documentation.
- Every exported public API validates arguments with `pcheck` (or the project’s `pcheck-*` helper).
- Keep extension registries and public registration APIs in `iter` and `navigator`; keep each
  data-structure adapter and its registration call in the data-structure's own library.
- Group each library's executable extension-registration calls under a section comment using
  the `;;;;===----------------------------------------------------------------------===` convention.
- ChezScheme has no built-in `string-for-each` or `string-map`; provide and export both from
  `(cspp string)`.
- `dset` remains outside navigator traversal because it has no collection-value traversal contract.
- Use ChezScheme bytevector accessors directly; do not add custom bytevector access helpers.
- Close every port opened by new code.
- Keep every documentation line at or below 100 characters.
- Run `make clean && make` from the project root of the implementation worktree.
- Run focused tests with `make -C tests test-some TEST='iter transducer navigator array dlist
  queue stack heap hashset treemap treeset bittree bitvec dset string'`.

---

### Task 1: Normalize size, membership, and deletion contracts

**Files:**
- Modify: `src/cspp/array.ss`
- Modify: `src/cspp/dlist.ss`
- Modify: `src/cspp/queue.ss`
- Modify: `src/cspp/heap.ss`
- Modify: `src/cspp/hashset.ss`
- Modify: `src/cspp/treeset.ss`
- Modify: `src/cspp/treemap.ss`
- Modify: `src/cspp/bitvec.ss`
- Modify: `src/cspp/bittree.ss`
- Modify: `src/cspp/concurrency/fiber.ss`
- Modify: `tests/array.ss`
- Modify: `tests/dlist.ss`
- Modify: `tests/queue.ss`
- Modify: `tests/heap.ss`
- Modify: `tests/hashset.ss`
- Modify: `tests/treeset.ss`
- Modify: `tests/treemap.ss`
- Modify: `tests/bitvec.ss`
- Modify: `tests/bittree.ss`

**Interfaces:**
- Produces `array-size`, `fxarray-size`, `u8array-size`, and `dlist-size` as the
  public size observers, with matching private record accessors and generated macro
  bindings.
- Produces boolean `array-contains?`, `fxarray-contains?`, `u8array-contains?`,
  `dlist-contains?`, and predicate variants; adds corresponding
  `X-index-of` and `X-find-index` APIs for indexed sequences.
- Establishes these absent-item policies: set/map delete is idempotent, indexed
  deletion reports an out-of-range index, and empty queue/stack/heap pop/peek reports
  an error.

- [ ] **Step 1: Write failing contract tests.** Add assertions such as:

```scheme
(mat array-membership
     (not (array-contains? (array 10 20) 30))
     (= 1 (array-index-of (array 10 20) 20))
     (= 1 (array-find-index (array 10 20) (lambda (x) (= x 20)))))

(mat dlist-membership
     (not (dlist-contains? (dlist 10 20) 30))
     (= 0 (dlist-index-of (dlist 10 20) 10)))

(mat deletion-policy
     (begin (hashset-delete! (hashset 1) 2) #t)
     (error? (array-delete! (array 1) 3 1)))
```

Update all existing `array-length`/`dlist-length` assertions to the new names in the
same test files and in `src/cspp/concurrency/fiber.ss`.

- [ ] **Step 2: Run the focused tests and verify the old API failures are visible.**

Run: `make -C tests test-some TEST='array dlist queue heap hashset treemap treeset bittree
bitvec'`

Expected: FAIL because the new size/index APIs are not defined and the old contains
procedures still return indexes.

- [ ] **Step 3: Rename the size observers and update generated array bindings.** Change
  the array record field/accessor from `length`/`array-length` to `size`/`array-size`,
  update `define-array-procedure` aliases, export the typed `X-size` names, and replace
  every internal `array-length`/`dlist-length` reference with the matching `X-size` name.
  Do not export compatibility aliases named `array-length` or `dlist-length`.

- [ ] **Step 4: Split boolean membership from index lookup.** Make each `X-contains?`
  return only `#t` or `#f`; implement `X-index-of` using `equal?` and `X-find-index`
  using a unary `pred` that returns true for a matching element. Preserve `X-search`
  as the value-returning operation and update parameter names and docs from `=?` to
  `pred` where the argument is unary.

- [ ] **Step 5: Fix the reviewed contract defects.** Add `pcheck ([queue? q]
  [procedure? proc])` to `queue-pop-all!`’s callback branch; rename its local binding
  from `procedure?`; correct `heap-contains/p?` documentation to say it returns a
  boolean; and make `treemap-max` documentation state only its chosen empty-map
  behavior (return `#f`, matching `treemap-min`).

- [ ] **Step 6: Implement and document the deletion policy.** Ensure hashset, treeset,
  and treemap deletion of an absent key/value leaves the structure unchanged; retain
  explicit range errors for array/dlist indexed deletion and empty errors for adapter
  pop/peek operations. Add negative tests with comments identifying each invalid case.

- [ ] **Step 7: Run tests and inspect the diff.**

Run: `make -C tests test-some TEST='array dlist queue heap hashset treemap treeset bittree bitvec'`

Expected: PASS with no stdout/stderr diagnostics, and `rg -n 'array-length|dlist-length'`
shows no public or implementation references except historical review text.

- [ ] **Step 8: Commit the independently reviewable API-contract change.**

```bash
git add cspp tests
git commit -m "data-structure: normalize sizes and membership contracts"
```

### Task 2: Add and consume the iter source-adapter registry

**Files:**
- Modify: `src/cspp/iter.ss`
- Modify: `src/cspp/transducer.ss`
- Modify: `src/cspp/array.ss`
- Modify: `src/cspp/dlist.ss`
- Modify: `src/cspp/queue.ss`
- Modify: `src/cspp/stack.ss`
- Modify: `src/cspp/heap.ss`
- Modify: `src/cspp/hashset.ss`
- Modify: `src/cspp/treeset.ss`
- Modify: `src/cspp/treemap.ss`
- Modify: `src/cspp/bitvec.ss`
- Modify: `src/cspp/bittree.ss`
- Modify: `src/cspp/dset.ss`
- Modify: `tests/iter.ss`
- Modify: `tests/transducer.ss`

**Interfaces:**
- Adds exported `(iter-register-source! predicate iterator-maker)` to `(cspp iter)`.
  `iterator-maker` receives one source and returns an iterator made with `make-iter`.
- `source->iter` and `transducible?` recognize registered sources after built-ins.
- Registered sources use their library’s documented traversal order; hashset order is
  unspecified, treeset/treemap/bit sets are ordered, queue is oldest-first, stack is
  newest-first, heap uses repeated priority-pop order, and arrays/dlists use index order.

- [ ] **Step 1: Add registry tests before implementation.** Extend `tests/iter.ss` with:

```scheme
(mat registered-array-iter
     (equal? '(1 2 3) (iter->list (source->iter (array 1 2 3)))))

(mat registered-queue-iter
     (equal? '(1 2 3) (iter->list (source->iter (queue 1 2 3)))))
```

Extend `tests/transducer.ss` with:

```scheme
(mat registered-source-transduction
     (= 12 (transduce (tcompose (tfilter even?) (tmap add1))
                      (rffxsum)
                      (array 1 2 3 4 5 6))))
```

- [ ] **Step 2: Run the tests to confirm registry behavior is absent.**

Run: `make -C tests test-some TEST='iter transducer'`

Expected: FAIL because `source->iter` and `transducible?` reject custom sources.

- [ ] **Step 3: Implement the registry in `src/cspp/iter.ss`.** Store newest registrations
  first, validate both procedures with `pcheck`, provide a private lookup procedure, and
  document the public registration procedure. Keep built-in conversion procedures
  unchanged and make `source->iter` call the registered maker only after built-in cases.

- [ ] **Step 4: Register each traversable data structure in its own library.** Add one
  initialization call in each of `array.ss`, `dlist.ss`, `queue.ss`, `stack.ss`,
  `heap.ss`, `hashset.ss`, `treeset.ss`, `treemap.ss`, `bitvec.ss`, and `bittree.ss`.
  Also register `dset.ss` as a read-only iterator over natural indexes from `0` through
  `(dset-size dset - 1)`; this does not make `dset` a navigator collection.
  Use existing `X->list`/ordered operations where they preserve the documented order;
  otherwise construct a stateful iterator that avoids mutating the source. Import only the
  required extension bindings from `(cspp iter)` in each library and place the executable
  registration call under a `;;;;===----------------------------------------------------------------------===` section heading near the end of
  the library.

- [ ] **Step 5: Update transducer dispatch.** Make `transducible?` include registered
  sources and make `source->iter` consult the registry. In `run-source`, retain direct
  runners for built-ins and route registered custom sources through `run-iter`; preserve
  `current-transducer-source-mode` semantics and `eduction` handling.

- [ ] **Step 6: Add order and reset tests for every adapter family.** Cover dlist, queue,
  stack, heap, treeset, treemap, bitvec, bittree, and both mutable array subtypes in
  `tests/iter.ss`; cover dset’s `0..n-1` index traversal; reset each iterator and assert
  the same sequence is produced twice.
  For hashsets, compare sorted output rather than relying on hash-table order.

- [ ] **Step 7: Run the focused tests and build.**

Run:

```bash
make -C tests test-some \
  TEST='iter transducer array dlist queue stack heap hashset treemap treeset bittree bitvec'
```

Expected: PASS with empty stderr/stdout apart from the test runner’s normal progress.

- [ ] **Step 8: Commit the registry change.**

```bash
git add cspp tests
git commit -m "iter: register custom data-structure sources"
```

### Task 3: Expose navigator extension APIs and self-register adapters

**Files:**
- Modify: `src/cspp/navigator/data.ss`
- Modify: `src/cspp/navigator/basic.ss`
- Modify: `src/cspp/navigator.ss`
- Modify: `src/cspp/array.ss`
- Modify: `src/cspp/dlist.ss`
- Modify: `src/cspp/hashset.ss`
- Modify: `src/cspp/treeset.ss`
- Modify: `src/cspp/treemap.ss`
- Modify: `src/cspp/bitvec.ss`
- Modify: `src/cspp/bittree.ss`
- Modify: `tests/navigator.ss`

**Interfaces:**
- Keeps `nav-register-indexed!` and `nav-register-keyed!` public and adds the exported
  `nav-register-set!` extension API to `(cspp navigator)`.
- The set protocol supplies `predicate`, `values-proc`, `replace-proc`, `replace!-proc`,
  `delete-proc`, and `delete!-proc`; it never exposes set members as fake map keys.
- Each supported data-structure library imports the required extension APIs from
  `(cspp navigator)` and registers its own protocol callbacks when that library is
  initialized; navigator does not import concrete data-structure libraries.
- `nav/all` and `nav/values` traverse registered sets; `nav/key`, `nav/nth`, `nav/keys`,
  and `nav/entries` reject them with navigator errors. Set clear/transform operations
  use explicit replace/delete callbacks.

- [ ] **Step 1: Add failing navigator tests.** Add cases in `tests/navigator.ss`:

```scheme
(mat navigator-data-structures
     (equal? '(1 2 3) (nav-select nav/all (array 1 2 3)))
     (let ([ts (make-fixnum-treeset fx= fx<)])
       (treeset-add! ts 1)
       (treeset-add! ts 2)
       (treeset-add! ts 3)
       (equal? '(1 2 3) (nav-select nav/all ts)))
     (let ([tm (make-fixnum-treemap fx= fx<)])
       (treemap-set! tm 1 'a)
       (treemap-set! tm 2 'b)
       (equal? '(a b) (nav-select nav/values tm)))
     (let ([ts (make-fixnum-treeset fx= fx<)])
       (treeset-add! ts 1)
       (error? (nav-select (nav/key 'x) ts))))
```

Add mutable tests asserting `nav-transform!` changes an array, a dlist, a treemap,
and a registered set while returning the original object where the API is mutating.

- [ ] **Step 2: Run the navigator tests before implementation.**

Run: `make -C tests test-some TEST='navigator'`

Expected: FAIL because the data structures are not registered and no set protocol exists.

- [ ] **Step 3: Extend `navigator/data.ss`.** Add the sealed set-protocol record,
  registry, lookup, validation, and helpers for values, replacement, and deletion. Keep
  indexed and keyed behavior unchanged. Export the existing indexed/keyed registration
  procedures and the new `nav-register-set!` as extension APIs. Document all three with
  exact callback signatures and return-value behavior.

- [ ] **Step 4: Teach `basic.ss` about sets.** Update collection selection and transformation
  branches so `nav/all`/`nav/values` use set values and set replacement callbacks, while
  `nav/keys`, `nav/entries`, `nav/key`, and `nav/nth` report unsupported operations for
  sets. Ensure `nav-clearval` removes a selected set member through delete callbacks.

- [ ] **Step 5: Register each navigator adapter in its data-structure library.** Import only
  the required registration procedures from `(cspp navigator)` in `array.ss`, `dlist.ss`,
  `treemap.ss`, `treeset.ss`, `hashset.ss`, `bitvec.ss`, and `bittree.ss`, then register:

  - arrays and dlists as indexed containers;
  - treemaps as keyed containers using in-order `treemap->list` entries;
  - treesets, hashsets, bitvecs, and bittrees through the set protocol;
  - no dset adapter.

  Use pure callbacks that copy/rebuild and mutating callbacks that preserve object identity.
  Keep each adapter's callbacks and registration form in the library that owns the type.
  Place the executable calls near the end of each library under a section comment:

```scheme
;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===
(nav-register-indexed!
 array? array-size array-ref
 (lambda (arr i value)
   (let ([copy (array-copy arr)])
     (array-set! copy i value)
     copy))
 array-set!)
```

- [ ] **Step 6: Re-export the navigator registration APIs.** Export
  `nav-register-indexed!`, `nav-register-keyed!`, and `nav-register-set!` from
  `(cspp navigator)`. Keep this library independent of concrete data-structure libraries;
  importing a data-structure library installs that type's registrations, including when the
  umbrella `(cspp)` library imports both sides.

- [ ] **Step 7: Run navigator tests and the complete build.**

Run: `make -C tests test-some TEST='navigator array dlist hashset treeset treemap bitvec bittree'`

Expected: PASS and no unsupported-operation errors for the registered operations.

- [ ] **Step 8: Commit the navigator protocol and registrations.**

```bash
git add cspp tests
git commit -m "navigator: register data structures with set protocol"
```

### Task 4: Complete bit-set algebra and bounds APIs

**Files:**
- Modify: `src/cspp/bitvec.ss`
- Modify: `src/cspp/bittree.ss`
- Modify: `tests/bitvec.ss`
- Modify: `tests/bittree.ss`

**Interfaces:**
- Exports `bitvec-bound`, returning the maximum exclusive bit index accepted by `bv`.
- Adds `bittree-or`, `bittree-and`, and `bittree-xor`; keeps `bittree-merge` as the
  documented union operation or compatibility alias. `bittree-not` is not added because
  an unbounded sparse set has no finite complement.

- [ ] **Step 1: Add failing tests.**

```scheme
(mat bitvec-bound
     (= 64 (bitvec-bound (make-bitvec 64)))
     (error? (bitvec-set! (make-bitvec 4) 4)))

(mat bittree-algebra
     (equal? '(1 2 3) (bittree->list (bittree-or (bittree 1 2) (bittree 2 3))))
     (equal? '(2) (bittree->list (bittree-and (bittree 1 2) (bittree 2 3))))
     (equal? '(1 3) (bittree->list (bittree-xor (bittree 1 2) (bittree 2 3)))))
```

- [ ] **Step 2: Run bit-set tests and verify the new names fail.**

Run: `make -C tests test-some TEST='bitvec bittree'`

Expected: FAIL because `bitvec-bound` and the binary bittree algebra procedures are not
currently exported.

- [ ] **Step 3: Export and document `bitvec-bound`.** Add `pcheck` validation and use the
  existing immutable bound field; ensure all bitvec mutators reject indexes at or above
  the bound with the existing error style.

- [ ] **Step 4: Implement sparse bittree algebra.** Build new bittrees by traversing set
  bits and applying union/intersection/symmetric-difference operations without converting
  to an unbounded complement. Preserve comparator/storage invariants and document traversal
  order and non-mutation of inputs.

- [ ] **Step 5: Run focused tests and the build.**

Run: `make -C tests test-some TEST='bitvec bittree navigator' && cd .. && make clean && make`

Expected: PASS and a successful CSPP build.

- [ ] **Step 6: Commit the bit-set APIs.**

```bash
git add cspp tests
git commit -m "bitset: expose bounds and align sparse algebra"
```

### Task 5: Review and complete the string library

**Files:**
- Modify: `src/cspp/string.ss`
- Modify: `tests/string.ss`

**Interfaces:**
- Exports and implements `edit-distance` as Levenshtein distance over two strings.
- Exports and implements `string-for-each` and `string-map` because ChezScheme does not
  provide them, and completes the indexed surface with `string-for-each/i` and
  `string-map/i`.
- Exports `string-slice` with the same `(str end)`, `(str start end)`, and
  `(str start end step)` conventions as `list:slice`, `vslice`, and the array slice APIs;
  it returns a string and uses the same negative-index and out-of-range rules.
- The traversal/mapping procedures accept one or more strings of equal length, validate the
  procedure and every string, pass corresponding characters to the procedure, and pass the
  zero-based index first for `/i` variants. Mapping callbacks must return a character;
  mapping returns a string, while for-each returns an unspecified value.
- Documents all four traversal/mapping procedures, `string-split`, search, containment,
  prefix/suffix, empty, and trim procedures with the repository's required tags and parameter
  meanings.
- Keeps character operations in ChezScheme; does not create `src/cspp/char.ss`.
- Preserves current search conventions (`#f` when no match; overlapping matches for
  `string-search-all`) and accepts either a character or non-empty string pattern where
  currently documented.

- [ ] **Step 1: Add failing string tests.**

```scheme
(mat string-edit-distance
     (= 0 (edit-distance "" ""))
     (= 3 (edit-distance "kitten" "sitting"))
     (= 2 (edit-distance "flaw" "lawn")))

(mat string-for-each-indexed
     (let ([seen '()])
       (string-for-each/i
        (lambda (i ch) (set! seen (cons (cons i ch) seen)))
       "ab")
       (equal? '((1 . #\b) (0 . #\a)) seen)))

(mat string-sequence-procedures
     (let ([seen '()])
       (string-for-each (lambda (ch) (set! seen (cons ch seen))) "ab")
       (equal? '(#\b #\a) seen))
     (string=? "AB" (string-map char-upcase "ab"))
     (string=? "AbCd" (string-map/i
                         (lambda (i ch) (if (even? i) (char-upcase ch) ch))
                         "abcd")))

(mat string-slice
     (string=? "01234" (string-slice "0123456789" 5))
     (string=? "864" (string-slice "0123456789" 8 2 -2))
     (string=? "" (string-slice "0123456789" 2 9 -1)))
```

Add negative tests for non-string inputs and empty multi-character delimiters, with a
comment immediately above each negative case.

- [ ] **Step 2: Run string tests before implementation.**

Run: `make -C tests test-some TEST='string'`

Expected: FAIL because `edit-distance`, `string-for-each`, `string-map`, `string-map/i`,
and `string-slice` are not implemented/exported; the documentation checker also reports
the old `#|doc` blocks.

- [ ] **Step 3: Implement `edit-distance` with bounded dynamic programming.** Validate
  both string arguments with `pcheck`, preallocate the shorter dimension’s numeric row,
  use fixnum arithmetic where lengths permit, and return the minimum insert/delete/replace
  count. Export the procedure and add its `#|proc:edit-distance` block directly above it.

- [ ] **Step 4: Normalize string documentation and argument checks.** Convert every
  exported `#|doc` block to `#|proc:name` or `#|macro:name`, describe every parameter and
  return value, rename unary predicate parameters to `pred`, and validate all string and
  delimiter arguments before indexing. Implement and export `string-for-each`, `string-map`,
  `string-map/i`, and `string-slice` rather than delegating to ChezScheme for missing
  operations. Implement `string-slice` with the same optional-argument dispatch and index
  normalization as `list:slice`, `vslice`, and array slices. Preserve equal-length
  multi-string behavior and the existing overlapping search behavior.

- [ ] **Step 5: Run string tests and the documentation checker.**

Run: `make -C tests test-some TEST='string'`

Run: `scheme --script tools/check-public-api-docs.ss src/cspp/string.ss`

Expected: PASS with no string documentation diagnostics.

- [ ] **Step 6: Commit the string review.**

```bash
git add src/cspp/string.ss tests/string.ss
git commit -m "string: complete edit distance and public API docs"
```

### Task 6: Normalize public documentation across the reviewed data structures

**Files:**
- Modify: `src/cspp/array.ss`
- Modify: `src/cspp/bittree.ss`
- Modify: `src/cspp/bitvec.ss`
- Modify: `src/cspp/dlist.ss`
- Modify: `src/cspp/dset.ss`
- Modify: `src/cspp/hashset.ss`
- Modify: `src/cspp/heap.ss`
- Modify: `src/cspp/list.ss`
- Modify: `src/cspp/queue.ss`
- Modify: `src/cspp/stack.ss`
- Modify: `src/cspp/treeset.ss`
- Modify: `src/cspp/treemap.ss`
- Modify: `src/cspp/vector.ss`
- Modify: `src/cspp/string.ss`
- Modify: `tests/net-docs.ss`

**Interfaces:**
- Every exported implementation recognized by `tools/check-public-api-docs.ss` has an
  adjacent required tag, including generated public array procedures and record accessors.
- Documentation explicitly states mutator return values, traversal order, callback
  signatures, empty behavior, and absent-delete behavior.

- [ ] **Step 1: Capture the current diagnostic inventory.**

Run:

```bash
scheme --script tools/check-public-api-docs.ss \
  src/cspp/array.ss src/cspp/bittree.ss src/cspp/bitvec.ss src/cspp/dlist.ss \
  src/cspp/dset.ss src/cspp/hashset.ss src/cspp/heap.ss src/cspp/list.ss \
  src/cspp/queue.ss src/cspp/stack.ss src/cspp/treeset.ss src/cspp/treemap.ss \
  src/cspp/vector.ss src/cspp/string.ss
```

Save the command output in the terminal only; do not add a generated report file.

- [ ] **Step 2: Replace stale tags and fill missing API contracts.** For each reported
  export, place a concise `#|proc:name`/`#|macro:name` block immediately before its
  implementation. Keep each line under 100 characters and remove vague return phrases
  such as “returns a value” or “returns unspecified”.

- [ ] **Step 3: Add regression coverage for the checker.** Extend `tests/net-docs.ss` to
  assert the reviewed libraries no longer report missing docs, overlong lines, or vague
  return phrases, while preserving its existing fixture checks for intentionally bad docs.

- [ ] **Step 4: Run the checker and focused tests.**

Run:

```bash
scheme --script tools/check-public-api-docs.ss \
  src/cspp/array.ss src/cspp/bittree.ss src/cspp/bitvec.ss src/cspp/dlist.ss \
  src/cspp/dset.ss src/cspp/hashset.ss src/cspp/heap.ss src/cspp/list.ss \
  src/cspp/queue.ss src/cspp/stack.ss src/cspp/treeset.ss src/cspp/treemap.ss \
  src/cspp/vector.ss src/cspp/string.ss
```

Run:

```bash
make -C tests test-some \
  TEST='net-docs array dlist queue stack heap hashset treemap treeset bittree bitvec dset string'
```

Expected: the checker exits successfully and all selected tests pass.

- [ ] **Step 5: Commit the documentation normalization.**

```bash
git add cspp tests/net-docs.ss
git commit -m "docs: normalize reviewed data-structure APIs"
```

### Task 7: Final integration verification

**Files:**
- Verify: `src/cspp/c/scheme.h` (copied into this worktree before implementation)
- Verify: `docs/data-structure-api-review.md`
- Verify: `docs/superpowers/plans/2026-09-16-data-structure-api-review.md`

**Interfaces:**
- The implementation branch contains the requested ChezScheme header copy and the plan’s
  review source document, without modifying the original worktree.

- [ ] **Step 1: Verify the requested header copy.**

```bash
cmp -s /home/maoif/SSD/Projects/src/cspp/src/cspp/c/scheme.h src/cspp/c/scheme.h
```

Expected: exit status 0.

- [ ] **Step 2: Run the required clean build.**

Run: `make clean && make`

Expected: successful C compilation and ChezScheme library compilation.

- [ ] **Step 3: Run all reviewed tests.**

Run:

```bash
make -C tests test-some \
  TEST='iter transducer navigator string array dlist queue stack heap hashset treemap \
        treeset bittree bitvec dset net-docs'
```

Expected: no test errors and no unexpected stdout/stderr diagnostics.

- [ ] **Step 4: Check Scheme balance and the final worktree.**

Run:

```bash
python3 check_parentheses.py src/cspp/iter.ss src/cspp/transducer.ss \
  src/cspp/navigator/data.ss \
  src/cspp/navigator/basic.ss src/cspp/string.ss
```

Run: `git status --short --branch`

Expected: balanced Scheme files, only intentional implementation/test/docs changes, and
the copied `src/cspp/c/scheme.h` present in the worktree.

- [ ] **Step 5: Commit the verified integration state.**

```bash
git add cspp tests docs
git commit -m "data-structure: complete API review integration"
```
