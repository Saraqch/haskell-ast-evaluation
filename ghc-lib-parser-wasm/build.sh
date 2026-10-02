#!/usr/bin/env bash
# Builds esp-wasm (ghc-lib-parser + prose generator) with the GHC WebAssembly backend (run inside Linux/WSL).
# Usage: ./build.sh            -> incremental build
#        CLEAN=1 ./build.sh    -> also wipes the cabal store (true cold build)
set -euo pipefail
cd "$(dirname "$0")"

missing=""
for tool in curl jq unzip zstd make gzip alex happy; do
  command -v "$tool" > /dev/null || missing="$missing $tool"
done
if [ -n "$missing" ]; then
  echo ">> Missing tools:$missing"
  echo ">> Run: sudo apt install -y$missing"
  exit 1
fi

# shellcheck disable=SC1091
[ -f "$HOME/.ghc-wasm/env" ] && source "$HOME/.ghc-wasm/env"
if ! command -v wasm32-wasi-ghc > /dev/null || ! command -v wasm32-wasi-cabal > /dev/null; then
  echo ">> Installing ghc-wasm-meta (FLAVOUR=9.6, matches ghc-lib-parser 9.6.7)"
  rm -rf "$HOME/.ghc-wasm"
  curl -fsSL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.6 sh
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi

BUILD_DIR="$HOME/.cache/esp-wasm/dist-newstyle"
if [ "${CLEAN:-0}" = 1 ]; then
  rm -rf "$BUILD_DIR" "$HOME/.ghc-wasm/.cabal/store"
fi

wasm32-wasi-cabal update
start=$(date +%s)
wasm32-wasi-cabal build --builddir="$BUILD_DIR" exe:esp-wasm
build_seconds=$(( $(date +%s) - start ))

raw=$(wasm32-wasi-cabal list-bin --builddir="$BUILD_DIR" exe:esp-wasm)
cp "$raw" web/esp-raw.wasm
wasm-opt -Oz --strip-debug web/esp-raw.wasm -o web/esp.wasm
gzip -9 -k -f web/esp.wasm

echo "hello :: Int -> Int
hello x = x + 1" | wasmtime run web/esp.wasm > /dev/null && echo ">> smoke test OK"

{
  echo "date:            $(date -Iseconds)"
  echo "ghc:             $(wasm32-wasi-ghc --version)"
  echo "build seconds:   $build_seconds"
  echo "raw wasm bytes:  $(stat -c %s web/esp-raw.wasm)"
  echo "opt wasm bytes:  $(stat -c %s web/esp.wasm)"
  echo "gzip wasm bytes: $(stat -c %s web/esp.wasm.gz)"
} | tee -a build-times.txt
