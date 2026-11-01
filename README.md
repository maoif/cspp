# Chez Scheme Programming Platform (CSPP)

CSPP provides libraries for Chez Scheme and a Make-based build with a `cspp` launcher.
Import `(cspp)` for the public libraries, or import individual libraries as needed.

## Download and run

Download a Linux `.tar.gz` archive for your architecture from the
[`master` prerelease](https://github.com/maoif/cspp/releases/tag/master): x64, arm64,
riscv64, or loongarch64. Extract it, enter the extracted directory, and start CSPP:

```sh
./bin/cspp
```

The archive includes Chez Scheme and native dependencies. Your system must meet the glibc
requirement listed in the package's `README.txt` and `BUILD-INFO.json`.
See [Linux packages](docs/releases.md) for compatibility and release details.

## Features

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

Some features use optional native libraries detected at build time. See the
[build guide](docs/build.md#optional-native-libraries) for dependencies and configuration.

## Build

Requires GNU Make, GCC or Clang, Git 2.36.0 or newer, and ncurses and X11 development packages.
`pkg-config` is recommended. CSPP builds its bundled Chez Scheme; a system installation is
not required.

On Ubuntu 24.04+ or Debian 12+:

```sh
sudo apt-get update
sudo apt-get install build-essential git pkg-config libncurses-dev libx11-dev
```

```sh
git clone --depth=1 https://github.com/maoif/cspp.git
cd cspp
git submodule update --init --depth=1 --filter=blob:none --recursive
make clean && make -j8
```

Adjust `-j8` for your machine. See the [build guide](docs/build.md) for other Linux distributions,
compiler options, and optional libraries.

## Use

Start the REPL, run a script, or evaluate inline code:

```sh
make run
./cspp --script=path/to/program.ss
./cspp -c='(display "Hello from CSPP") (newline)'
./cspp --help
```

Exit the REPL with `(exit)` or Ctrl + d. Import CSPP in a Scheme program:

```scheme
(import (cspp))

(displayln
  (into 'list
        (tmap string-upcase)
        '("chez" "scheme")))
;; Prints (CHEZ SCHEME).
```

See [examples and benchmarks](examples/README.md) for runnable programs, including `match`
and `for` examples.

## Install

Install the built library, launcher, and bundled Chez Scheme under an absolute prefix:

```sh
make install PREFIX=/absolute/path/to/install
```

See [installation details](docs/build.md#install) for the installed layout.

## Test

After building, run all tests or select individual suites:

```sh
make test
make -C tests test vector.ss list.ss
```

Use `make test-non-net` for all registered non-network suites.
See [testing details](docs/build.md#tests) for optional features and regression fixtures.
See [test and benchmark usage requirements](docs/test-and-benchmark.md) for controlling-thread
APIs, assertions in worker threads and fibers, failure propagation, and measurement scope.

## Documentation

- [Build, installation, and testing](docs/build.md)
- [Examples and benchmarks](examples/README.md)
- [Cryptography and API migration](docs/crypto.md)
- [Command-line parsing](docs/cli.md)
- [HTTP transport support](docs/http-lws.md)
- [Linux packages and releases](docs/releases.md)
- [Developer guide](docs/developer-guide.md)
