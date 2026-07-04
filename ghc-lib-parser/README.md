# Test: ghc-lib-parser

## Library Information

| Field | Detail |
|---|---|
| **Name** | `ghc-lib-parser` |
| **Tested version** | `9.6.7.20250325` |
| **Hackage** | https://hackage.haskell.org/package/ghc-lib-parser |
| **Source repository** | https://github.com/digital-asset/ghc-lib |
| **Last tested** | June 2026 |
| **Status** | Functional |

## What does this library do?

`ghc-lib-parser` is the subset of the GHC API required to parse Haskell code.
It uses the **real GHC parser** (not an alternative one such as `haskell-src-exts`),
which guarantees full compatibility with all language extensions.

Modules used in this test:

- `GHC.Parser` — `parseModule` function, the parser entry point
- `GHC.Parser.Lexer` — `P` monad, parser state, and parse result (`ParseResult`)
- `GHC.Data.StringBuffer` — converts a `String` into the buffer consumed by the parser
- `GHC.Types.SrcLoc` — source code locations
- `GHC.Hs` — GHC AST types in parsing phase (`GhcPs`)
- `GHC.Utils.Outputable` — AST pretty-printing through the `Outputable` class

---

## Prerequisites

- **GHC** `9.6.7`
- **Cabal** `3.10.2.0` or newer

If you do not have them installed, use [GHCup](https://www.haskell.org/ghcup/):

**Windows:** download the installer from https://www.haskell.org/ghcup/ and run it.

Verify installation:
```bash
ghc --version    # should show 9.6.7
cabal --version  # should show 3.10.x or newer
```
## How to Run 

> Run all commands from the `ghc-lib-parser/` folder.

**1. Move to the correct folder**
```bash
cd ghc-lib-parser
```

**2. Update the package index** _(only the first time)_
```bash
cabal update
```

**3. Build and run** _(the first build takes several minutes: ghc-lib-parser is large)_
```bash
cabal run ghc-lib-parser-prueba -- ./Ejemplo.hs
```

> **Note:** `ghc-lib-parser` is about ~150 MB of source code. The first build
> may take between 5 and 15 minutes. Subsequent runs are near-instant.

> **Windows:** it is recommended to keep the repository in a path without spaces for example, `C:\Projects\`.

---

## Project Structure

```
ghc-lib-parser/
├── ghc-lib-parser-prueba.cabal    project definition and dependencies
├── Ejemplo.hs                     input Haskell file for the test
└── src/
    └── Main.hs                    main program
```

---

 