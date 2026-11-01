# Cryptography

Cryptographic digests are available from `(cspp crypto digest)`, `(cspp crypto)`, or `(cspp)`.
Use `digest-bytevector`, `digest-string`, and `digest-file` with an algorithm symbol, or helpers
such as `sha256-bytevector`. Incremental operations use `make-digester`, `digester-update-*`,
`digester-get`, `digester-reset!`, `digester-finalize!`, and `digester-destroy!`.
`digester-get` preserves the state; finalization releases it. `call-with-digester` passes a
managed digester to a callback and returns the digest, ignoring callback results.

## String encodings

String digest APIs encode each character as **native-endian UTF-32 without a byte-order mark**.
Slice indices count characters for strings and bytes for bytevectors and files; `stop` is
exclusive. To hash UTF-8 text, use `(digest-bytevector 'sha256 (string->utf8 text))`.

String encodings in the crypto APIs are explicit:

| API | String encoding |
| --- | --- |
| Digests, legacy unsafe hashes, HMAC | Native-endian UTF-32, without a byte-order mark |
| Signing, KDFs, passwords, envelopes | UTF-8 |

Pass bytevectors for another encoding. Ed25519 accepts `#f` for its optional external
digest argument; an explicit digest symbol raises an error.
OpenSSL supplies MD5, SHA-2, SHA-3, and BLAKE2; BLAKE3 uses its independently optional library.
MD5 is retained for legacy interoperability and is unsuitable for security-sensitive hashing.

## Concurrency and resource lifetimes

Digest, HMAC, cipher, AEAD, signing, and verification states serialize individual operations
on a shared state. Concurrent updates take effect in lock-acquisition order; use separate states
for independent messages. User callbacks run without holding a state lock.
Keys, certificates, and trust stores also serialize use and destruction. Multi-resource
operations acquire their locks in a fixed order. A call racing with destruction either
finishes before destruction or raises a destroyed-resource error.
Callers must keep mutable input buffers unchanged until an operation returns. AEAD state
constructors copy their key and nonce buffers, matching the ownership of cipher and HMAC states.
Errors reach caller exception handlers after internal state/resource locks have been released.
Public crypto procedures validate argument types before optimized primitive or native access.
Slice indices must be nonnegative fixnums; inexact integers and bignums are rejected.

## Migrating older imports

The old `(cspp digest)` and `(cspp crypto hash)` imports have been removed. Replace them with
`(cspp crypto digest)` and replace the former crypto `hash-*` names with `digest-*` or
`digester-*` as appropriate. Algorithm-specific helpers retain their names. Noncryptographic
xxHash operations remain in `(cspp hash)`.

See the [developer guide](developer-guide.md#cryptographic-resource-implementation) for
native resource ownership, interrupt handling, and continuation restrictions.
