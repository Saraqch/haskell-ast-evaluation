# Test: haskell-src-exts

## Library Information

| Field | Detail |
|---|---|
| **Name** | `haskell-src-exts` |
| **Tested version** | `1.23.1` |
| **Hackage** | https://hackage.haskell.org/package/haskell-src-exts |
| **Last update** | June 21, 2026 |
| **Status** | Functional |

## What does this library do?

haskell-src-exts provides support for manipulating Haskell source code. It includes a lexer, parser, pretty-printer, and a complete Haskell AST definition with extensions.

Main modules used:
- `Language.Haskell.Exts.Parser` - parses source code into an AST
- `Language.Haskell.Exts.Syntax` - defines syntax tree types
- `Language.Haskell.Exts.Pretty` - prints the AST back to source code

## How to Run

```bash
# From the repository root folder:
cd haskell-src-exts

cabal update        # only the first time
cabal run haskell-src-exts-test -- ./Example.hs
```

