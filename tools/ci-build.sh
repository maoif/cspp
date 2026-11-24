#!/bin/bash
# Run inside the target architecture's CI image, with root access for setup.
set -euo pipefail

architecture=${1:?usage: ci-build.sh x64|arm64|riscv64|loongarch64 full-commit-sha}
commit=${2:?missing commit SHA}
case "$architecture:$(uname -m)" in
    x64:x86_64|arm64:aarch64|riscv64:riscv64|loongarch64:loongarch64) ;;
    *) echo "architecture mismatch: $architecture on $(uname -m)" >&2; exit 1 ;;
esac
[[ $commit =~ ^[0-9a-f]{40}$ ]] || { echo 'invalid commit SHA' >&2; exit 1; }
# shellcheck source=/dev/null
source /etc/os-release
case "$architecture:$ID:${VERSION_ID:-}" in
    loongarch64:debian:*|x64:ubuntu:24.04|arm64:ubuntu:24.04|riscv64:ubuntu:24.04) ;;
    *) echo 'requires Ubuntu 24.04, or Loongnix Debian for loongarch64' >&2; exit 1 ;;
esac

export DEBIAN_FRONTEND=noninteractive
apt=(apt-get)
if [[ $architecture == loongarch64 ]]; then
    apt+=(-o "Dir::Etc::sourcelist=$PWD/tools/ci-loongarch64.sources" -o Dir::Etc::sourceparts=-)
fi
"${apt[@]}" update
"${apt[@]}" install -y --no-install-recommends \
    build-essential git pkg-config libncurses-dev libx11-dev \
    patchelf cmake curl ca-certificates python3 \
    libssl-dev uuid-dev libxxhash-dev zlib1g-dev libc-ares-dev \
    libcurl4-openssl-dev libgrpc-dev libidn2-dev libssh-dev libwebsockets-dev

# Pin and verify the same portable BLAKE3 C library on every architecture.
workspace=$(mktemp -d)
trap 'rm -rf "$workspace"' EXIT
blake3_commit=df610ddc3b93841ffc59a87e3da659a15910eb46 # 1.8.2
curl -fsSL --retry 3 --connect-timeout 30 --max-time 300 \
    "https://codeload.github.com/BLAKE3-team/BLAKE3/tar.gz/$blake3_commit" \
    -o "$workspace/blake3.tar.gz"
printf '%s  %s\n' \
    3b5aab142e171a166eb530876d37eb70c4112ee3a0c9e18562f9c7401048dc2b \
    "$workspace/blake3.tar.gz" | sha256sum --check
tar xzf "$workspace/blake3.tar.gz" -C "$workspace"
blake3_source="$workspace/BLAKE3-$blake3_commit"
cmake -S "$blake3_source/c" -B "$workspace/blake3-build" \
    -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release
cmake --build "$workspace/blake3-build" --parallel 2
cmake --install "$workspace/blake3-build"
install -d /usr/local/share/doc/blake3
install -m644 "$blake3_source"/LICENSE_* /usr/local/share/doc/blake3/
ldconfig

# The source has already been checked out with recursive, pinned submodules.
# Declare the mounted Git repositories trusted for this root-owned container.
repository=$(pwd -P)
git config --global --add safe.directory "$repository"
git config --global --add safe.directory "$repository/vendor/ChezScheme"
make clean && make -j2 \
    WITH_CARES=1 WITH_CURL=1 WITH_GRPC=auto WITH_IDN2=1 WITH_LIBSSH=1 WITH_WEBSOCKETS=1 \
    WITH_ZLIB=1 WITH_OPENSSL=1 WITH_UUID=1 WITH_XXHASH=1 WITH_BLAKE3=1
make -C tests test-non-net
make -C tests test-release-package

# Use the public package target with the same native choices that passed testing.
make package PACKAGE_COMMIT="$commit" \
    NATIVE_OPTIONS_REPLAY="$repository/.cspp-native-options.mk"
version=$(.chezscheme-install/bin/scheme --script tools/print-library-version.ss cspp.lib)
version=${version#CSPP version: }

# Run the archive from a different directory, independent of the install prefix and cwd.
archive="cspp-$version-linux-$architecture"
mkdir "$workspace/relocated package"
tar xzf "dist/$archive.tar.gz" -C "$workspace/relocated package"
.chezscheme-install/bin/scheme --script tools/check-scheme-balance.ss tools/package-smoke.ss
(cd / && "$workspace/relocated package/$archive/bin/cspp" --script="$repository/tools/package-smoke.ss")
(cd dist && sha256sum --check "$archive.tar.gz.sha256")
