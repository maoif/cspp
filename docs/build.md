# Building, Installing, and Testing CSPP

Run commands from the repository root unless a section specifies another directory.
For a shorter introduction, see the [project README](../README.md).

Build requirements:

- GNU Make
- GCC or Clang
- Git **2.36.0 or newer**
- ncurses headers and libraries, for ChezScheme's terminal expression editor
- X11 headers and client libraries, for ChezScheme's X11 clipboard support

`pkg-config` is recommended for detecting optional CSPP libraries. The bundled ChezScheme
build enables ncurses and X11 support by default and builds its own zlib and LZ4 from submodules.
You do not need to install a system ChezScheme package.

## Install dependencies on Linux

Install the packages for your distribution, then follow [Build and run](#build-and-run).
These packages cover the build tools and bundled ChezScheme dependencies; optional CSPP
features are described under [Optional native libraries](#optional-native-libraries).

### Ubuntu / Debian

For Ubuntu 24.04 or newer and Debian 12 or newer:

```sh
sudo apt-get update
sudo apt-get install build-essential git pkg-config libncurses-dev libx11-dev
```

Older releases may ship Git below the required version. In particular, Ubuntu 22.04 ships
Git 2.34; upgrade Git to at least 2.36.0 before building.

### Arch Linux / Manjaro

```sh
sudo pacman -Syu --needed base-devel git pkgconf ncurses libx11
```

### Fedora

```sh
sudo dnf install gcc make git pkgconf-pkg-config ncurses-devel libX11-devel
```

### RHEL / Rocky Linux / AlmaLinux

On RHEL 9 and compatible distributions, enable the standard repositories and install:

```sh
sudo dnf group install "Development Tools"
sudo dnf install git pkgconf-pkg-config ncurses-devel libX11-devel
```

### openSUSE Leap / Tumbleweed

```sh
sudo zypper refresh
sudo zypper install gcc make git pkg-config ncurses-devel libX11-devel
```

### Alpine Linux

```sh
sudo apk add build-base git pkgconf ncurses-dev libx11-dev
```

## Build and run

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
See [Tests](#tests) to run the test suites and [Install](#install) to install the built toolchain.

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

## Optional native libraries

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

The `(cspp optional-library)` library reports which optional features were enabled.

## Launcher options

`make run` starts the CSPP REPL. Exit with `(exit)` or Ctrl + d. The generated `cspp` launcher
also runs scripts and inline code:

```sh
./cspp --script=path/to/program.ss
./cspp -c='(display "Hello from CSPP") (newline)'
./cspp --version
```

Use `./cspp --help` (or `-h`) to show launcher options. Use `-q` to suppress the
startup greeting and REPL prompt. `-s` is an alias for `--script`.

## Install

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

For binary archives and local packaging, see [Linux packages](releases.md).

## Tests

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
