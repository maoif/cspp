# Data structures and traversal

CSPP provides immutable and mutable sequences, sets, maps, queues, stacks, heaps,
and disjoint sets. Each library requires an explicit import unless it is part of
`(cspp)`'s default language environment. List, string, and vector APIs are included
in that environment.

## API Summary

| Family | Structures | Main operations |
| --- | --- | --- |
| Immutable sequence | `list` | map/fold/scan, uniqueness, slicing, sorting predicates, set-like list algebra |
| Fixed sequence | `vector` | map/fold/scan, filter/partition, membership, reverse, zip, shuffle, sort, arithmetic |
| Mutable sequence | `array`, `dlist` | ref/set/add/delete, slices/copies, two-ended push/pop, filter/search, map/fold, sort |
| Text | `string` | indexed traversal, search, prefix/suffix, split, trim |
| Adapters | `queue`, `stack` | push/pop/peek, bulk pop, size/empty, clear, contains, copy, list conversion |
| Priority | `heap` | comparator, bounded/unbounded construction, push/pop/peek, priority extraction, copy |
| Unordered set | `hashset` | add/delete, search/filter/partition, map/for-each, set algebra, conversions |
| Ordered set | `treeset` | hashset-like operations plus order queries, ordered folds, and set algebra |
| Bit set | `bitvec`, `bittree` | set/unset/flip, population count, ordered traversal; dense versus sparse storage |
| Key-value | `treemap` | set/ref/delete, keys/values/cells, search/filter, order queries, ordered traversal |
| Equivalence classes | `dset` | union-find: same-set, union, size |

## API conventions

Sequence `X-contains?` procedures return booleans. Use `X-index-of` and
`X-find-index` for indexed lookup. Indexed deletion reports range errors;
absent set and map deletion is idempotent. Constructors, observers, and mutators
are documented immediately above their implementations.

Adapters have defined traversal orders: queues traverse oldest first, stacks
traverse newest first, heaps use priority-pop order, and ordered trees use in-order
traversal by default. `bitvec-bound` reports the bound of a dense bit set; sparse
bittrees support union, intersection, and xor.

## Iterators and transducers

Collection libraries register iterator and navigator adapters when imported.
Transducers use the iterator registry for custom sources. Register a custom source
with `iter-register-source!`, supplying a predicate and a procedure that creates an
iterator for matching values. `iter-source->iter` converts a source to an iterator;
the transducer library's `source->iter` supplies the transducer-facing wrapper.

Indexed iterators support directional slice bounds and reject zero steps. List
iterators support forward traversal. Iterators with finalized resources reject further
next and reset operations. Port iterators leave caller-provided ports open; file iterators
own their ports and close them when finalized.

Registered mutable sources traverse live storage and reset against current contents.
Mutation during an active pass is unspecified. Chez Scheme hashtables and hashsets
snapshot keys at the start of each pass, reading values from the source as needed.
The registry rejects duplicate predicate procedure objects atomically; distinct
overlapping predicates use newest-first precedence.

```scheme
(import (cspp)
        (cspp array)
        (cspp transducer))

(transduce (tcompose (tfilter even?) (tmap add1))
           (rffxsum)
           (array 1 2 3 4 5 6))
;; => 15
```

## Navigators

Navigator adapters support indexed, keyed, and set-like containers. Arrays and
dlists use indexed access, treemaps use keyed access, and treesets, hashsets,
bitvecs, and bittrees use the set protocol. Set clearing uses deletion callbacks,
including for custom set protocols. Disjoint sets do not provide navigator traversal.
