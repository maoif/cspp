# CSPP Examples and Benchmarks

Run commands from the repository root with `cspp --script=FILE -- [OPTIONS]`. Every command accepts `-h` and
`--help`. File arguments are paths relative to the current directory.

## Parsers

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/parser/xml.ss` | `--file=tests/data/xml-basic.xml` | XML document parser |
| `examples/parser/json.ss` | `--file=tests/data/json5-example.json5` | JSON5 parser |
| `examples/parser/toml.ss` | `--file=tests/data/pyproj.toml` | TOML parser |
| `examples/parser/scheme.ss` | `--file=examples/data/sample.scm` | Scheme datum parser |
| `examples/parser/elf.ss` | `--file=.chezscheme-install/bin/scheme` | ELF parser; input must be an ELF file |

## Data Processing

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/navigator/traversal.ss` | `--value='((name . Ada) (scores . #(2 4 6)))'` | select and transform |
| `examples/transducer/pipeline.ss` | `--limit=4` | composed map/filter/take pipeline |
| `examples/iter/operations.ss` | `--limit=4` | iterator map, filter, zip, fold, cleanup |
| `examples/match/patterns.ss` | `--value='(move 3 4)'` | literals, guards, repetition, records, nested datatypes, regex, catamorphisms, bindings |
| `examples/for/iteration.ss` | `--limit=4` | parallel/nested loops, typed sources, index scopes, bindings, slices, folds, exit results |
| `examples/glob/files.ss` | `--pattern='examples/**/*.ss' --limit=10` | matching, expansion, lazy iteration |

The `match` and `for` scripts print labeled results for each feature using small built-in inputs.
Try `--value='(say "hello")'` or `--value='(move north 4)'` to exercise different match clauses,
and `--limit=0` to see empty bounded collection.

The matching example includes named and positional datatype fields, singleton variants, nested
variant patterns, repeated variants, and native Chez records. Catamorphisms show recursive
expression evaluation, a datatype tree returning sum and leaf count, repeated multi-value
recursion, explicit procedures, and traversal for effects only.

The iteration example starts with simple `for` loops that print range values, list items, string
characters, and paired items. It also covers strings, lists, vectors, fixnum/flonum vectors,
signed and multi-byte bytevectors, IEEE doubles, hashtable entries/keys/values, numeric ranges,
and existing iterators.
Folds include products, dot products, left/right subtraction, record statistics, a mutable vector
histogram, datatype actions, empty input, and boolean short-circuiting. Source, guard, global,
and nested local indexes illustrate their different counting scopes. Exit examples contrast
inner and outer `:break`, whole-loop `:stop`, accumulator replacement, and results that bypass
`:finish`.

## Containers

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/containers/treeset.ss` | `--values=3,1,2,2` | generic and fixnum treesets |
| `examples/containers/treemap.ss` | `--values=3,1,2` | generic and fixnum treemaps |
| `examples/containers/heap.ss` | `--values=3,1,2` | heap priority operations |
| `examples/containers/dlist.ss` | `--values=3,1,2` | mutable doubly-linked list |
| `examples/containers/array.ss` | `--size=8` | generic, fixnum, and flonum arrays |

Heap and dlist currently provide generic APIs only. Typed variants are shown only for collections
that currently expose them.

## Networking

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/net/http-file-server.ss` | `--dir=examples/data --port=8080` | local HTTP/1 file server |
| `examples/net/http2-file-server.ss` | `--dir=examples/data --port=8443` | cleartext H2 with prior knowledge |
| `examples/net/https-file-server.ss` | `--dir=examples/data --port=8443 --cert=cert.pem --key=key.pem` | TLS file server |
| `examples/net/wget.ss` | `--uri=http://127.0.0.1:8080/sample.scm --output=sample.scm` | HTTP downloader |
| `examples/net/ssh-open-repl.ss` | `HOST` | interactive remote shell |
| `examples/net/ssh-run-cmd.ss` | `HOST --command='uname -a'` | remote command and streams |
| `examples/net/ssh-port-forward.ss` | `HOST --direction=local --target-host=127.0.0.1 --target-port=22` | SSH TCP forwarding |
| `examples/net/scp.ss` | `HOST --direction=upload --kind=file --local=FILE --remote=PATH` | SCP files and directories |

SSH takes a required positional `HOST`, before or after named options. `--user` defaults to the
current local username and `--auth` defaults to `publickey` (discover keys in `~/.ssh`).
SSH checks `$HOME/.ssh/known_hosts`. Use `--private-key=PATH` for an explicit key, or choose
`--auth=password` or `--auth=agent`. Set `CSPP_SSH_PASSWORD` or `CSPP_SSH_PASSPHRASE`
for secrets; agent authentication uses `SSH_AUTH_SOCK`. The shell uses `--pty=auto` by default:
terminal input gets a remote PTY with the local TERM and window size; piped input uses no PTY.
Use `--pty=yes|no` to override it. Terminal sessions forward keys and Ctrl-C to the remote shell,
follow window resizing, and restore the local terminal settings when the session ends.
Remote command stdout/stderr remain separate and its exit status becomes the local exit status.

Forwarding accepts `--direction=local|remote`, `--listen-host`, and `--listen-port`; local mode
listens locally and connects the target through SSH, while remote mode listens remotely and
connects the target locally. `--listen-port=0` requests a free port, logged to stderr. It runs until
Ctrl-C unless `--duration=MILLISECONDS` is supplied. SCP accepts both directions and kinds, plus
`--overwrite=error|replace|skip`. Remote paths can be absolute or relative to the remote login
directory. Directory copies accept `--symlinks=reject|preserve` (default: `reject`):
`preserve` copies literal links, including dangling links. Source and destination root paths and
destination parent paths must not contain symlinks. For example, add `--symlinks=preserve` when
copying a source checkout with links.
For directory copies, `--overwrite=replace` recreates read-only regular destination files when
their parent directory permits deletion, including read-only Git objects in an existing checkout.
Unsupported resume modes are rejected. Recursive SCP copies reuse a channel
for up to 32 sibling files at a time, while retaining per-file progress and overwrite policies.

`wget.ss` and `scp.ss` display progress bars on stderr by default. Pass `--progress=no` to disable
progress, or `--progress=yes` to enable it explicitly; lifecycle logs remain enabled. SCP shows
bytes and percentage per copied file, including recursive uploads and downloads. Wget uses
`(cspp rich)` progress tasks and a live display on stderr. It uses the GET response's
`Content-Length` to show current/total bytes and percentage during the download,
including after redirects. If the server omits the length, it shows received bytes with an unknown
total until the final size is known. Wget also shows current download speed in `B/s`, `KiB/s`, `MiB/s`,
or `GiB/s`, sampled between progress updates using monotonic time. A successful download ends at
100%.
Displays update about ten times per second, with immediate updates for the first bytes, file
transitions, and completion.

| Server mode | Requirements | Verified request |
|---|---|---|
| HTTP/1 | libwebsockets | `curl --http1.1 http://127.0.0.1:8080/sample.scm` |
| Cleartext H2 | libwebsockets with HTTP/2 | `curl --http2-prior-knowledge http://127.0.0.1:8443/sample.scm` |
| HTTPS / TLS H2 | libwebsockets TLS/HTTP2 and OpenSSL; certificate and key | `curl --http2 --cacert=cert.pem https://localhost:8443/sample.scm` |

The H2 example selects prior knowledge explicitly; it requires an H2 client. The HTTPS example
negotiates HTTP/1 or H2 through ALPN. Tests verify the negotiated version and certificate trust.
Server roots and requested paths reject symlinks and traversal, and only GET regular files are served.

`wget.ss` streams into a temporary file beside the destination and publishes it only after a
successful HTTP response. It sends `User-Agent: cspp-wget` by default; override it with
`--user-agent=TEXT`. Its total request timeout is one hour; use `--timeout=SECONDS` with a positive
integer for a different limit. Large files stream to disk without retaining the entire body in
memory. Interrupted downloads are removed; resume is not supported.

Existing positional examples also accept both help flags through `(cspp cli)`. These include
`http-download.ss URI [auto|http/1.1|h2]`, `echo-{client,server}-grpc.ss [HOST PORT]`,
`ftp-client.ss HOST PORT USER PASSWORD`, `sftp-client.ss HOST PORT USER AGENT-OR-PRIVATE-KEY`,
and the commands under `examples/net/file-transfer/`. Their lifecycle logs are opt-in with
`CSPP_EXAMPLE_LOG_LEVEL=debug`, preserving binary output and the transfer verifiers' quiet mode.
The `*-common.ss` and protocol implementation modules are loaded helpers, not commands. The
`verify-*.ss` and shell drivers under `file-transfer/` are test fixtures/verifiers.

## Cryptography

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/crypto/encrypt-file.ss` | `--mode=encrypt --input=input --output=input.enc --method=aes-256-gcm` | authenticated file encryption/decryption |
| `examples/crypto/hash-file.ss` | `--file=input --method=sha256` | cryptographic hashes and checksums |

Set `CSPP_EXAMPLE_PASSWORD` before encryption/decryption. Both methods authenticate the envelope
and ciphertext before creating a plaintext output; output must be a new path. Modern digests use
OpenSSL; BLAKE3 and xxhash checksums require their corresponding optional libraries.

## Benchmarks

| Script | Invocation | Compares |
|---|---|---|
| `benchmarks/treeset.ss` | `--size=10000 --samples=10` | treeset and fxtreeset |
| `benchmarks/treemap.ss` | `--size=10000 --samples=10` | treemap and fxtreemap |
| `benchmarks/vectors.ss` | `--size=100000 --samples=10` | vector, fxvector, and flvector |
| `benchmarks/arrays.ss` | `--size=100000 --samples=10` | array, fxarray, and flarray |

Each benchmark accepts `--size`, `--samples`, `--warmup`, `--min-time`, `--max-iterations`,
`--filter=NAME-SUBSTRING`, `--format=text|csv|json|datum`, `--output=PATH`, and `--cost-center`.
Reports include build/runtime metadata, arguments, per-iteration timing/allocation summaries
(mean, median, range, standard deviation, confidence interval), and raw sample CPU/real time,
allocated bytes, GC count/bytes/time, and iteration counts. Unavailable cost-center measurements
are labeled explicitly. No report file is created unless `--output` is supplied.

Sets/maps measure populate, successful lookup, traversal, update, and delete. Vectors measure
fill/read/map/fold; arrays measure append/index/map/fold. Generic flonum cases provide a matching
domain for flvector/flarray, alongside generic fixnum cases for fxvector/fxarray. Setup and mutation
resets stay outside measured regions; no cross-profile specialization ratios are reported.

## Local Verification

After `make clean && make`, `make -C tests test examples.ss net-examples.ss benchmarks.ss`
runs parser/data/container/crypto/benchmark examples, both CLI help forms, and local HTTP/SSH
fixtures. Network fixtures require curl with HTTP/2, OpenSSL tools, and OpenSSH tools. These
checks use temporary files, host keys, certificates, loopback listeners, and bounded subprocesses.

Run `CSPP_EXAMPLE_TEST_VERBOSE=1 python3 tests/integration/examples.py` for individual example
checks, `python3 tests/ssh-examples-integration.py` for SSH/SCP/forwarding, and
`CSPP_TRANSFER_MIB=1 examples/net/file-transfer/verify-local-transfers.sh` for the existing ten
protocol transfer matrix. `python3 tests/ssh-examples-integration.py --password` additionally uses
the optional Python Paramiko package to verify password auth without modifying a system account.
