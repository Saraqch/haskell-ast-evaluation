#!/usr/bin/env bash
# Proof of concept: build GHC's official in-browser playground
# (testsuite/tests/ghc-api-browser, BSD-3) with the ghc-wasm-meta 9.14
# flavour and add load-time measurements. Run inside Linux/WSL.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"

# Separate prefix so the 9.6 toolchain used by ../ghc-lib-parser-wasm stays intact.
export PREFIX="$HOME/.ghc-wasm-9.14"
is_914() { command -v wasm32-wasi-ghc > /dev/null && [[ "$(wasm32-wasi-ghc --numeric-version)" == 9.14.* ]]; }
# shellcheck disable=SC1091
[ -f "$PREFIX/env" ] && source "$PREFIX/env"
if ! is_914; then
  echo ">> Installing ghc-wasm-meta (FLAVOUR=9.14) into $PREFIX"
  rm -rf "$PREFIX"
  curl -fsSL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.14 PREFIX="$PREFIX" sh
  # shellcheck disable=SC1091
  source "$PREFIX/env"
fi
echo ">> Using $(wasm32-wasi-ghc --version)"

SRC=https://gitlab.haskell.org/haskell-wasm/ghc/-/raw/ghc-9.14
WORK="$HOME/.cache/ghc-in-browser-poc"
rm -rf "$WORK"
mkdir -p "$WORK"
cd "$WORK"

for f in index.html playground001.hs playground001.sh; do
  curl -fsSL "$SRC/testsuite/tests/ghc-api-browser/$f" -o "$f"
done
# The JS loader must match the installed GHC exactly, so prefer its libdir copy.
LIBDIR="$(wasm32-wasi-ghc --print-libdir)"
for f in dyld.mjs post-link.mjs prelude.mjs; do
  if [ -f "$LIBDIR/$f" ]; then cp "$LIBDIR/$f" .; else curl -fsSL "$SRC/utils/jsffi/$f" -o "$f"; fi
done
# The upstream script ends by launching a puppeteer test; we serve the page instead.
sed -i '/^exec \.\/playground001\.js/d' playground001.sh

start=$(date +%s)
TEST_HC="$(command -v wasm32-wasi-ghc)" \
TEST_CC="$(command -v wasm32-wasi-clang)" \
GHC_PKG="$(command -v wasm32-wasi-ghc-pkg)" \
  bash playground001.sh
build_seconds=$(( $(date +%s) - start ))

python3 "$HERE/patch-timings.py" index.html

rm -rf "$HERE/web"
mkdir -p "$HERE/web"
cp index.html dyld.mjs post-link.mjs prelude.mjs rootfs.tar.zst "$HERE/web/"

{
  echo "date:              $(date -Iseconds)"
  echo "ghc:               $(wasm32-wasi-ghc --version)"
  echo "build seconds:     $build_seconds"
  echo "rootfs.tar.zst:    $(stat -c %s rootfs.tar.zst) bytes"
  echo "rootfs extracted:  $(zstd -dc rootfs.tar.zst | wc -c) bytes"
} | tee -a "$HERE/build-times.txt"
echo ">> Done. Serve $HERE/web and open index.html"
