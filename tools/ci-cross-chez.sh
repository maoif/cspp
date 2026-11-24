#!/bin/bash
set -euo pipefail

architecture=${1:-}
case $architecture in
    riscv64)
        machine=trv64le
        triplet=riscv64-linux-gnu
        compiler=$triplet-gcc
        cross_packages=(gcc-riscv64-linux-gnu libc6-dev-riscv64-cross)
        deb_arch=riscv64
        ;;
    loongarch64)
        machine=tla64le
        triplet=loongarch64-linux-gnu
        compiler=$triplet-gcc-14
        cross_packages=(gcc-14-loongarch64-linux-gnu libc6-dev-loong64-cross debian-archive-keyring)
        deb_arch=loong64
        ;;
    *) echo 'usage: ci-cross-chez.sh riscv64|loongarch64' >&2; exit 2 ;;
esac

target_packages=(libncurses-dev libncurses6 libtinfo6 libx11-dev x11proto-dev)
if [[ $deb_arch == loong64 ]]; then
    target_packages+=(libc6 libc6-dev)
fi

repository=$(pwd -P)
source_dir=$repository/vendor/ChezScheme
submodules=$(git submodule status --recursive -- vendor/ChezScheme)
if [[ -z $submodules || $submodules =~ (^|$'\n')[-+U] ]]; then
    echo 'error: cross-compiled Chez requires the pinned submodules' >&2
    exit 1
fi
commit=$(git -C "$source_dir" rev-parse HEAD)

sudo apt-get update
sudo apt-get install -y --no-install-recommends \
    build-essential libncurses-dev libx11-dev "${cross_packages[@]}"

workspace=$(mktemp -d)
trap 'rm -rf "$workspace"' EXIT
mkdir -p "$workspace"/{lists,packages,sysroot,build}
if [[ $architecture == loongarch64 ]]; then
    sources=$repository/tools/ci-loongarch64.sources
else
    sources=$workspace/sources.list
    printf '%s\n' 'deb [signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] https://ports.ubuntu.com/ubuntu-ports noble main' > "$sources"
fi
target_apt=(apt-get -o "APT::Architecture=$deb_arch" -o "APT::Architectures=$deb_arch"
    -o "Dir::Etc::sourcelist=$sources" -o Dir::Etc::sourceparts=-
    -o "Dir::State::lists=$workspace/lists" -o Dir::Cache::pkgcache= -o Dir::Cache::srcpkgcache=
    -o Acquire::https::Timeout=30 -o Acquire::Retries=3)
"${target_apt[@]}" update
(
    cd "$workspace/packages"
    "${target_apt[@]}" download "${target_packages[@]}"
    for package in ./*.deb; do
        dpkg-deb -x "$package" "$workspace/sysroot"
    done
)

install_dir=$repository/.chezscheme-install
build_dir=$repository/.chezscheme-build
sysroot=$workspace/sysroot
target_libs=$sysroot/usr/lib/$triplet
ldflags="-L$target_libs -Wl,-rpath-link,$target_libs"
if [[ $deb_arch == loong64 ]]; then
    ln -s usr/lib64 "$sysroot/lib64"
    ldflags="--sysroot=$sysroot -B$target_libs/ $ldflags"
fi
(
    cd "$workspace/build"
    "$source_dir/configure" --cross -m="$machine" --toolprefix="$triplet-" \
        "CC=$compiler" CC_FOR_BUILD=cc --installprefix="$install_dir" \
        "CFLAGS+=-I$sysroot/usr/include/$triplet -I$sysroot/usr/include" \
        "LDFLAGS=$ldflags"
    make -j"$(nproc)"
    make install DESTDIR="$workspace/installed"
)
rm -rf "$install_dir"
mv "$workspace/installed$install_dir" "$install_dir"
mkdir -p "$build_dir"
printf 'cross commit=%s arch=%s\n' "$commit" "$architecture" > "$build_dir/.chezscheme-signature"
