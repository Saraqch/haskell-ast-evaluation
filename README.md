# Library Evaluation for Abstract Syntax Tree Extraction

**Undergraduate Thesis** — Automatic generation of explanatory prose for didactic support in understanding Haskell programs.

## Objective

Evaluate specialized parsing libraries to select the most suitable tool for extracting the Abstract Syntax Tree (AST) from Haskell source code.

---

## Analyzed Libraries

| # | Library | Tested Version | Status | Folder |
|---|---|---|---|---|
| 1 | `haskell-src-exts` | `1.23.1` | Functional | [`/haskell-src-exts`](./haskell-src-exts/) |
| 2 | `ghc-lib-parser` | `9.6.7.20250325` | Functional | [`/ghc-lib-parser`](./ghc-lib-parser/) |

## Repository Structure

```
haskell-ast-evaluation/
├── README.md                        ← this file
│
├── haskell-src-exts/                ← test with haskell-src-exts
│   ├── README.md                    ← findings, instructions, conclusions
│   ├── haskell-src-exts.cabal
│   ├── Ejemplo.hs
│   └── src/
│       └── Main.hs
│
└── ghc-lib-parser/                  ← test with ghc-lib-parser (HLS infrastructure)
    ├── README.md                    ← findings, instructions, conclusions
    ├── ghc-lib-parser-prueba.cabal
    ├── Ejemplo.hs
    └── src/
        └── Main.hs
```

---

## Prerequisites

To run any of the tests you will need:

- **GHC** `9.6.7`
- **Cabal** `3.10.2.1` or higher

Install with [GHCup](https://www.haskell.org/ghcup/) (recommended):

**Windows:** download and run the installer from https://www.haskell.org/ghcup/

## How to Run Each Test

### Test 1 — haskell-src-exts

```bash
cd haskell-src-exts
cabal update        # first time only
cabal run haskell-src-exts-prueba -- ./Ejemplo.hs
```

### Test 2 — ghc-lib-parser

```bash
cd ghc-lib-parser
cabal update        # first time only
cabal run ghc-lib-parser-prueba -- ./Ejemplo.hs
```

> The first build of `ghc-lib-parser` takes between 5 and 15 minutes
> due to the size of the library (~150 MB). Subsequent builds are instant.

---

## Author

Sara Quispe Choque
Undergraduate Thesis — Universidad Mayor de San Simón
2026