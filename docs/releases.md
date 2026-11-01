# Linux Packages and Master Snapshots

For build dependencies and source installation, see the [build guide](build.md).
Run packaging commands from the repository root.

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
