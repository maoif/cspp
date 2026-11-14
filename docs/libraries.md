# Library imports

`(cspp)` provides the Chez Scheme environment with CSPP's language extensions.
It re-exports exactly these libraries:

- `(cspp chez)`, `(cspp utils)`, `(cspp control)`, `(cspp match)`, `(cspp adt)`
- `(cspp list)`, `(cspp string)`, `(cspp vector)`, `(cspp for)`, `(cspp iter)`

All other CSPP libraries require explicit imports. They remain included in the
built `cspp.lib`; using a focused import does not require a separate build.
For example:

```scheme
(import (cspp)
        (cspp io)
        (cspp file)
        (cspp format toml))

(displayln (parse-toml (read-string "settings.toml")))
```

## Formats and parser construction

| Library | Purpose |
| --- | --- |
| `(cspp format csv)` | CSV documents |
| `(cspp format json5)` | JSON5 documents |
| `(cspp format toml)` | TOML documents |
| `(cspp format xml)` | XML documents |
| `(cspp format scheme)` | Scheme data |
| `(cspp format elf)` | ELF binaries |
| `(cspp format wasm)` | WebAssembly binary and text modules |
| `(cspp format protobuf)` | Protocol Buffers wire data and descriptors |
| `(cspp format)` | All of the format APIs above |
| `(cspp parser)` | Parser construction, with implementation helpers hidden |
| `(cspp parser combinator)` | The complete combinator API |

Protobuf's focused wire and descriptor APIs are available from
`(cspp format protobuf wire)` and `(cspp format protobuf descriptor)`.

## Timing

`(cspp utils)` provides `sleep`, `milisleep`, `nanosleep`, `sleep-seconds`,
`sleep-milliseconds`, and `sleep-nanoseconds`; these are also available from `(cspp)`.
These procedures take exact nonnegative integers directly in the named units.
Chez's original sleeper is internal and is not exported by `(cspp chez)` or `(cspp)`.

## Environment access

`(cspp utils)` provides synchronized `getenv` and `putenv`; both are also available from
`(cspp)`. `(cspp chez)` supplies the Chez primitives and does not load the native runtime.

## Private implementation libraries

Private libraries use `(cspp private name)` for shared infrastructure, or
`(cspp subsystem private name)` within a subsystem. There is one component
after `private`, such as `(cspp system private process-backend)`.
Applications should import public libraries such as `(cspp system process)`,
`(cspp net http)`, and `(cspp navigator)`.
