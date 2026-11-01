# Chez Scheme Programming Platform (CSPP)

Chez Scheme Programming Platform (CSPP) is a collection of Chez Scheme libraries and a
Make-based build that produces the `cspp` launcher. The public umbrella library is
`(cspp)`; individual libraries can be imported when a smaller dependency set is useful.

## ✨ Features

- **Language tools:** algebraic data types and records, pattern matching, comprehensions,
  `for` loops, iterators, transducers, data navigator, control helpers, and list, string, and vector utilities.
- **Collections:** arrays, bit vectors, bit trees, doubly linked lists, sets, heaps, queues,
  stacks, and ordered maps and sets.
- **Parsing and formats:** parser combinators, CSV, JSON5, TOML, XML, Scheme, ELF, WebAssembly,
  and Protocol Buffers.
- **Files and system APIs:** file and port helpers, paths, globbing, processes,
  signals, filesystem and user information, UUIDs, hashes, and optional-library probes.
- **Networking:** sockets, IP and DNS helpers, URIs, TLS, HTTP/HTTPS, FTP/FTPS, SSH, SFTP, SCP,
  WebSocket, and gRPC APIs.
- **Cryptography:** random data, digests, encodings, MACs, KDFs, AEAD, ciphers, keys, signatures,
  key agreement, passwords, envelopes, and certificates.
- **Application support:** command-line parsing, logging, terminal rendering, benchmarking, and
  test helpers.
- **Concurrency:** threads, thread pools, atomic boxes, spinlocks, futures, and fiber libraries.
  Fibers and fiber-aware network operations are available as `(cspp concurrency fiber)` and
  `(cspp concurrency fiber-net)`.

Some networking, cryptography, hashing, compression, and UUID features use optional native
libraries selected at build time. `(cspp optional-library)` reports which features were enabled.

Cryptographic digests are available from `(cspp crypto digest)`, `(cspp crypto)`, or `(cspp)`.
Use `digest-bytevector`, `digest-string`, and `digest-file` with an algorithm symbol, or helpers
such as `sha256-bytevector`. Incremental operations use `make-digester`, `digester-update-*`,
`digester-get`, `digester-reset!`, `digester-finalize!`, and `digester-destroy!`.
`digester-get` preserves the state; finalization releases it. `call-with-digester` passes a
managed digester to a callback and returns the digest, ignoring callback results.

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

Digest, HMAC, cipher, AEAD, signing, and verification states serialize individual operations
on a shared state. Concurrent updates take effect in lock-acquisition order; use separate states
for independent messages. User callbacks run without holding a state lock.
Keys, certificates, and trust stores also serialize use and destruction. Multi-resource
operations acquire their locks in a fixed order. A call racing with destruction either
finishes before destruction or raises a destroyed-resource error.
Callers must keep mutable input buffers unchanged until an operation returns. AEAD state
constructors copy their key and nonce buffers, matching the ownership of cipher and HMAC states.
Errors reach caller exception handlers after internal state/resource locks have been released.
Internal native-resource operations defer timer, keyboard, and collection interrupts until
their handles and ownership changes are consistent. An exited internal operation cannot be
reentered, including through the continuation attached to a Chez error condition. Public
`call-with-*` callbacks run with the caller's interrupt settings; their states are destroyed
on continuation escape. All managed crypto state scopes reject reentry after exit.
Public crypto procedures validate argument types before optimized primitive or native access.
Slice indices must be nonnegative fixnums; inexact integers and bignums are rejected.

The old `(cspp digest)` and `(cspp crypto hash)` imports have been removed. Replace them with
`(cspp crypto digest)` and replace the former crypto `hash-*` names with `digest-*` or
`digester-*` as appropriate. Algorithm-specific helpers retain their names. Noncryptographic
xxHash operations remain in `(cspp hash)`.

## 🔧 Build

Build requirements:

- GNU Make
- GCC or Clang
- Git **2.36.0 or newer**
- ncurses headers and libraries, for ChezScheme's terminal expression editor
- X11 headers and client libraries, for ChezScheme's X11 clipboard support

`pkg-config` is recommended for detecting optional CSPP libraries. The bundled ChezScheme
build enables ncurses and X11 support by default and builds its own zlib and LZ4 from submodules.
You do not need to install a system ChezScheme package.

### Install dependencies on Linux

Install the packages for your distribution, then follow [Build and run](#build-and-run).
These packages cover the build tools and bundled ChezScheme dependencies; optional CSPP
features are described under [Optional native libraries](#optional-native-libraries).

#### Ubuntu / Debian

For Ubuntu 24.04 or newer and Debian 12 or newer:

```sh
sudo apt-get update
sudo apt-get install build-essential git pkg-config libncurses-dev libx11-dev
```

Older releases may ship Git below the required version. In particular, Ubuntu 22.04 ships
Git 2.34; upgrade Git to at least 2.36.0 before building.

#### Arch Linux / Manjaro

```sh
sudo pacman -Syu --needed base-devel git pkgconf ncurses libx11
```

#### Fedora

```sh
sudo dnf install gcc make git pkgconf-pkg-config ncurses-devel libX11-devel
```

#### RHEL / Rocky Linux / AlmaLinux

On RHEL 9 and compatible distributions, enable the standard repositories and install:

```sh
sudo dnf group install "Development Tools"
sudo dnf install git pkgconf-pkg-config ncurses-devel libX11-devel
```

#### openSUSE Leap / Tumbleweed

```sh
sudo zypper refresh
sudo zypper install gcc make git pkg-config ncurses-devel libX11-devel
```

#### Alpine Linux

```sh
sudo apk add build-base git pkgconf ncurses-dev libx11-dev
```

### Build and run

Check the installed Git version, clone the repository and its pinned submodules, then build
the library and launcher. The following steps apply to all distributions above:

```sh
git --version  # Must be 2.36.0 or newer.
git clone --depth=1 https://github.com/maoif/cspp.git
cd cspp
git submodule update --init --depth=1 --filter=blob:none --recursive
make clean && make -j8
make run
```

`-j8` allows up to eight parallel build jobs; adjust the number for your machine. Run the build
as your normal user. `make run` starts the CSPP REPL; exit it with `(exit)` or Ctrl + d.
See [Test](#-test) to run the test suites and [Install](#-install) to install the built toolchain.

The repository pins a release ChezScheme in `vendor/ChezScheme`. `make -j8` builds and
installs that bundled compiler under `.chezscheme-build` and `.chezscheme-install` before building
CSPP. Make checks Git’s recursive submodule status and initializes missing submodules with a
filtered shallow checkout, so the explicit submodule update above can also be omitted.
A `scheme` symlink in the repository root points at the generated compiler.

Use a different C compiler when needed:

```sh
make clean && make -j8 CC=clang
```

The bundled ChezScheme build supplies its compiler and matching development header together.
`make clean` removes CSPP outputs while preserving the submodule and generated ChezScheme
toolchain. `make clean-all` performs that cleanup and removes `.chezscheme-build`,
`.chezscheme-install`, and the generated root `scheme` symlink.

### Optional native libraries

Optional development packages are not required to build or import CSPP. Each dependency has
a `WITH_<NAME>` switch: `auto` (the default) enables it when its headers and library pass a
compile/link probe, `1` requires it and reports a build error if unavailable, and `0` disables
it without probing or linking it.

| Library / feature | Switch | Compiler flags | Linker flags |
| --- | --- | --- | --- |
| c-ares / DNS | `WITH_CARES` | `CARES_CFLAGS` | `CARES_LIBS` |
| cURL / FTP | `WITH_CURL` | `CURL_CFLAGS` | `CURL_LIBS` |
| gRPC | `WITH_GRPC` | `GRPC_CFLAGS` | `GRPC_LIBS` |
| libidn2 / IDNA | `WITH_IDN2` | `IDN2_CFLAGS` | `IDN2_LIBS` |
| libssh / SSH, SFTP, SCP | `WITH_LIBSSH` | `LIBSSH_CFLAGS` | `LIBSSH_LIBS` |
| libwebsockets / HTTP, WebSocket | `WITH_WEBSOCKETS` | `WEBSOCKETS_CFLAGS` | `WEBSOCKETS_LIBS` |
| zlib / compression | `WITH_ZLIB` | `ZLIB_CFLAGS` | `ZLIB_LIBS` |
| OpenSSL / TLS, crypto, digests | `WITH_OPENSSL` | `OPENSSL_CFLAGS` | `OPENSSL_LIBS` |
| libuuid / UUID | `WITH_UUID` | `UUID_CFLAGS` | `UUID_LIBS` |
| xxHash | `WITH_XXHASH` | `XXHASH_CFLAGS` | `XXHASH_LIBS` |
| BLAKE3 | `WITH_BLAKE3` | `BLAKE3_CFLAGS` | `BLAKE3_LIBS` |

Detection uses `pkg-config`; set `PKG_CONFIG` to choose its executable. A supplied compiler or
linker variable overrides that side of the package flags, and the missing side comes from
`pkg-config`. Without usable package metadata, supply both variables; an explicitly empty
value counts as supplied. `auto` disables dependencies it cannot detect.

```sh
# Detect available dependencies automatically.
make clean && make -j8 WITH_CURL=auto

# Build with FTP support disabled.
make clean && make -j8 WITH_CURL=0

# Require cURL using its installed pkg-config metadata.
make clean && make -j8 WITH_CURL=1

# Require cURL using manual flags, including when pkg-config is unavailable.
make clean && make -j8 WITH_CURL=1 \
  CURL_CFLAGS='-I/opt/curl/include' CURL_LIBS='-L/opt/curl/lib -lcurl'
```

Enabled dependencies use ordinary dynamic linking and must also be present at runtime.
Disabled features remain importable; calling an API that needs one raises a Scheme error
identifying build-time disablement. Feature choices and dependency flags are recorded in the
build signature, so changing them triggers a clean rebuild. `make print-build-options` shows
the requested and resolved choices.

## ▶️ Use

Start a CSPP REPL:

```sh
make run
```

The generated `cspp` launcher also runs scripts and inline code:

```sh
./cspp --script=path/to/program.ss
./cspp -c='(display "Hello from CSPP") (newline)'
./cspp --version
```

Use `./cspp --help` (or `-h`) to show launcher options. Use `-q` to suppress the
startup greeting and REPL prompt. `-s` is an alias for `--script`.

A transducer example:

```scheme
(import (cspp))

(displayln
  (into 'list
        (tmap string-upcase)
        '("chez" "scheme")))
```

This prints:

```text
(CHEZ SCHEME)
```

## 📦 Install

Install the compiled library and launcher under an absolute prefix:

```sh
make install PREFIX=/absolute/path/to/install
```

`PREFIX` must be an absolute path. The install includes the bundled ChezScheme compiler and
runtime alongside CSPP: `<prefix>/bin/scheme`, `<prefix>/bin/petite`,
`<prefix>/bin/cspp`, and the upstream `lib/csv10.4.1/<machine>` boot and header files,
plus `<prefix>/lib/cspp.lib` and `<prefix>/lib/libcspp.so`. The generated launcher uses
these installed paths and does not refer back to the source checkout. The bundled compiler's
relative runtime links are preserved.

`make clean` removes local CSPP build outputs while preserving the generated bundled compiler.
`make clean-all` also removes `.chezscheme-build`, `.chezscheme-install`, and the generated
root `scheme` link. Removing the installation prefix removes both CSPP and the bundled
ChezScheme files installed by this target.

### Linux master snapshots

Build a local archive with the same recursive dependency packaging used by CI:

```sh
make clean && make
make package
# Choose another output directory.
make package PACKAGE_DIR=/absolute/path/to/packages
```

`make package` stages `make install` in a temporary directory and writes a `.tar.gz`
archive and SHA-256 checksum to `dist/` by default. It detects Linux x64, arm64,
riscv64, or loongarch64 and records the current Git commit, build host, and glibc requirement.
It requires
`readelf`, `ldd`, `patchelf`, GNU `tar`, and GNU coreutils. Packaging runs through `cspp`
using `tools/package-release.ss`. Dependencies are collected recursively,
including ChezScheme's X11 runtime library; host glibc and its loader remain external.
The target packages the selected build configuration; run tests before packaging as needed.
Pass custom build settings to the target as well, for example
`make package VARIANT=debug WITH_OPENSSL=0`; plain `make package` uses the default settings.
Run the packaging regression tests with `make -C tests test-release-package`.

Every push to `master` builds and runs all non-net tests on Ubuntu 24.04 for x64,
arm64, and riscv64, and on a pinned Loongnix Debian image for loongarch64. x64 and arm64
use native runners; riscv64 and loongarch64 use QEMU. Only after
all four jobs pass does CI update the rolling
[`master` prerelease](https://github.com/maoif/cspp/releases/tag/master).
The workflow can also be run manually against `master`.

Each `.tar.gz` contains the `make install` artifacts, including bundled ChezScheme,
and recursively resolved shared-library dependencies. Extract it anywhere and run
`bin/cspp`. The package requires the matching Linux architecture and the glibc version
listed in its `README.txt` and `BUILD-INFO.json` (Ubuntu packages require glibc 2.39;
the pinned Loongnix Debian package requires glibc 2.43). glibc and its dynamic loader
come from the host. CI publishes only the archives; local packaging also writes SHA-256 checksums.
`BUILD-INFO.json` records the source commit, version, build settings,
and dependency provenance; bundled library licenses are under `share/licenses/`.

CI enables the supported distribution native dependencies and builds pinned BLAKE3 1.8.2.
gRPC remains subject to automatic detection because support depends on the distribution's ABI.
Network APIs are compiled and packaged; their tests are excluded. Regular version tags
retain the `major.minor.patch@chezMajor-chezMinor-chezPatch` format.

## 🧪 Test

Build first, then run all test suites:

```sh
make clean && make -j8
make test
```

Run every registered non-network test from the project root or `tests/`:

```sh
make test-non-net
```

This selects the existing test suite with all `net-*.ss` files excluded.

In `tests/`, run all tests or select individual test source files:

```sh
make test
make test vector.ss list.ss
make test net-http.ss net-lws-http2.ss net-lws-server.ss
```

Only files in the test suite are accepted as positional arguments; support files are rejected.

Tests retain the native feature settings from the root build. Clauses that require disabled
libraries report `Skipped mat ...` individually; unrelated clauses and disabled-API error
checks continue to run. Run the build and direct-link regression fixtures with
`make -C tests test-optional-library-build`.
