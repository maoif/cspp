# Control and utility APIs

`(cspp control)` provides composition, sections, loop escape, and continuation bindings.
`(cspp utils)` provides argument checks, small predicates/macros, procedure inspection, random
data generators, and fixnum flags. Both are included by `(cspp)`.

These libraries keep reusable descriptions separate from invocation state. Compositions and
sections capture their callbacks and fixed values; each call has its own traversal state.
Shared mutable callback state still requires caller synchronization.

The public list builder lives in `(cspp list)`. Its low-level implementation is shared with
utility generators and procedure inspection through `(cspp internal)`, avoiding both duplicate
implementations and a circular import between the list and utility libraries.

## Composition and sections

| API | Order and callback contract |
| --- | --- |
| `(compose left ... right)` | Right to left; each callback consumes all values from its neighbor |
| `(compose1 left ... right)` | Right to left; intermediate results are single values |
| `(>>> value first ... last)` | Left to right; callbacks pass all returned values onward |
| `(>>>1 value first ... last)` | Left to right; intermediate results are single values |

Each form requires at least one callback and checks every callback before invoking any.
The rightmost callback in a composition accepts the returned procedure's arguments; the first
callback in `>>>`/`>>>1` accepts one value. Final callbacks may return multiple values.
Composing just one procedure returns that procedure unchanged.
`compose1` prepares callback order at construction and traverses it in constant space when called;
callbacks may use additional space themselves. Two-callback compositions use a direct call path.

```scheme
(import (cspp))

(define sum-of-neighbors (compose + (lambda (value) (values value (+ value 1)))))
(= 7 (sum-of-neighbors 3)) ; => #t
(= 14 (>>>1 3 add1 (lambda (value) (* value 2)) (lambda (value) (+ value 6)))) ; => #t

(define add-ten (sect + 10 _))
(= 15 (add-ten 5)) ; => #t
(((sect+ list _ 'middle _) 'first) 'last) ; => (first middle last)
```

`sect` captures its callback and fixed arguments once, then fills `_` placeholders in argument
order on each call. A trailing `...` permits extra arguments. A section without slots is a
zero-argument procedure. `sect+` instead returns one unary procedure per slot; without slots
or `...`, it calls the callback immediately. Callback arity is checked when it is invoked.
`((sect list))` and `(sect+ list)` therefore both return the empty list.

`forever` repeats a body until `(break value ...)` exits, returning those values. Optional
`(index name)` binds a zero-based iteration count. `let/cc` exposes a reusable continuation,
while `let/1cc` exposes one that may be used at most once. Escaping or resuming control does not
roll back mutable objects, I/O, or callback effects.

## Argument checks and mutations

Use `(pcheck ([predicate argument ...] ...) body ...)` on public procedure arguments.
Predicates are unary, run in clause order, and accept any nonfalse result. Each predicate
expression is evaluated once per clause. A false result raises an argument error before the
body; successful checks preserve the body's return values. Convenience `pcheck-*` forms select
common predicates. Input/output binary/textual port checks additionally require open ports.
`pcheck` attributes failures to `pcheck`. Use
`(pcheck-who who ([predicate argument ...] ...) body ...)` to report the caller's name.
The caller must be a variable reference whose value is a symbol. Its value is read once before
argument checks; nonsymbol values are rejected. Inside `define-who`, use its `who` binding.
Public APIs in control, utils, and system supply their names explicitly; aliases reuse the
validation of their target procedures.

```scheme
(define-who repeat-character
  (lambda (character count)
    (pcheck-who who ([char? character] [natural? count])
      (make-string count character))))
(string=? "aaa" (repeat-character #\a 3)) ; => #t
```

`natural?` accepts exact nonnegative integers, including bignums; `positive-natural?` excludes
zero. Lengths used by the random generators must additionally fit a fixnum.
`p/and`, `p/or`, and `p/not` construct unary predicates and short-circuit in callback order.
`p/and` preserves the final successful result, `p/or` the first nonfalse result, and `p/not`
returns a boolean. Empty `p/and` is true; empty `p/or` is false.

`pcase` tests a bound identifier against unary predicates in clause order. Use
`[number? body ...]` for one named predicate or `[(number? string?) body ...]` for alternatives.
The first successful clause returns its body's values; `else` supplies the fallback.
Predicate expressions are evaluated once, left to right, only when their test is reached;
each is called before the next expression is evaluated. A match skips remaining alternatives
and clauses, including their predicate expressions.
Wrap a computed predicate in a singleton list so its expression is not parsed as alternatives:
`[((p/not number?)) body ...]` or `[((lambda (object) (number? object))) body ...]`.

`incr!`/`decr!` and aliases `add1!`/`sub1!` mutate numeric variables. `fx+!`, `fx-!`, `fx*!`, and
`fx/!` require fixnum variables and operands. They evaluate operands once and preserve arithmetic
checks even at optimization level 3; type errors, overflow, and division errors do not assign
the target. Effects performed while evaluating operand expressions are not undone.
`cons!` prepends to a variable; `cdr!` requires a pair and advances the variable to its cdr.
These updates are not atomic.

## Procedure inspection and random data

`procedure-arity` inspects without calling its procedure. It returns a number for a single fixed
arity, a list for several fixed arities, or a list ending in `*` for a variadic tail. The number
immediately before `*` is the tail's minimum arity; earlier numbers are separate fixed arities.
A procedure accepting all arities yields `(*)`; an empty `case-lambda` yields `()`.
`procedure-variable-arity?` returns a boolean; `procedure-name` returns a name string or `#f`.

```scheme
(equal? '(1 3 *)
  (procedure-arity (case-lambda [(one) one] [(first second third . rest) rest]))) ; => #t
(string=? "aaa" (random-string (lambda () #\a) 3 4)) ; => #t
(eq? 'name (random-symbol (lambda () "name") 1 5)) ; => #t
```

Random ranges are half-open: lower bounds are included, upper bounds excluded. Length bounds
are increasing nonnegative fixnums. Zero-length results are possible when zero is in the range.
`random-char` uses character bounds and skips Unicode surrogates without retries.

| Generator | Callback and defaults |
| --- | --- |
| `random-string` | Optional `() -> character`, called per character; default `random-char`, length `[0,10)` |
| `random-list` | Optional `() -> object`, called in list order; default `random-datum`, length `[0,10)` |
| `random-symbol` | Optional `() -> string`, called once; result name must fit bounds, default length `[1,10)` |
| `random-box` | Optional `() -> object`, called once; default `random-datum` |
| `random-bytevector`, `random-u8vec` | Random octets, default length `[0,10)` |

For variable-length generators, no bounds selects the defaults, one bound is an upper bound,
and two bounds are lower/upper. A callback, when supported, precedes both bounds.
Invalid callback results raise without retrying. These are test-data helpers backed by Chez's
`random`, not cryptographic randomness; use `(cspp crypto)` for security-sensitive random bytes.
The result of `random-list` is undefined under replay of a continuation captured in its generator.

## Flags

`define-flags` generates a flag-combining macro and a `name->symbols` decoder. Masks are fixnums;
composites can refer to earlier flags using `or`. Decoding selects every flag with any overlapping
bit, including aliases and composite masks, in declaration order. Zero masks are never listed.
The macro name and flag names must be identifiers; nonidentifier names raise syntax errors.
Flag names must also be distinct; duplicate declarations are rejected during expansion.

```scheme
(define-flags permissions (none 0) (read 1) (write 2) (both (or read write)))
(= 3 (permissions read write)) ; => #t
(equal? '(read both) (permissions->symbols 1)) ; => #t
(and (flag-any-set? 3 1) (not (flag-all-set? 3 1))) ; => #t
```

Flag operations validate fixnums even in release code. `flag-set` and `flag-reset` return new
integer values without mutating inputs; `flag-all-set?` accepts a zero mask as true.
The generated decoder ignores unknown bits.
