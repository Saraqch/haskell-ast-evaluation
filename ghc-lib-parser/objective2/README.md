# Objective 2 Prototype

This directory contains the technical prototype used to evaluate whether
Abstract Syntax Tree (AST) information alone is sufficient to generate
explanatory prose for Haskell programs, or whether inferred static type
information should be included.

The prototype provides two independent executables:

- `ghc-lib-ast-prose` generates structural explanatory prose from the parsed
  AST. Its output does not include inferred type facts.
- `ghc-lib-typed-ast-prose` preserves the structural prose and adds inferred
  types for top-level functions that do not declare an explicit type signature.

Both executables analyse the same input file. The AST-only output uses parsed
syntax and fixity-resolved guard information, while the type-enriched output
adds the available top-level type facts produced by GHC.

## Requirements

- GHC 9.6.7
- Cabal
- The dependencies declared in `ghc-lib-parser-test.cabal`

Run the following commands from the `ghc-lib-parser` directory.

## Build

```text
cabal build ghc-lib-ast-prose
cabal build ghc-lib-typed-ast-prose
```

## Run the AST-only generator

```text
cabal run ghc-lib-ast-prose -- .\objective2\examples\case-studies\NumericOperators.hs
```

## Run the type-enriched generator

```text
cabal run ghc-lib-typed-ast-prose -- .\objective2\examples\case-studies\NumericOperators.hs
```

Replace the input path with any supported Haskell module that GHC can parse
and typecheck in the current project configuration.

## Included examples

| Group | Source file | Purpose |
| --- | --- | --- |
| Case study A | `examples/case-studies/NumericOperators.hs` | Numeric operators and ad-hoc polymorphism. |
| Case study B | `examples/case-studies/OptionalHead.hs` | List patterns and an optional result. |
| Case study C | `examples/case-studies/ApplyTwice.hs` | Higher-order functions. |
| Paper listing 1 | `examples/paper-listings/Listing1.hs` | Guards and infix operators. |
| Paper listing 2 | `examples/paper-listings/Listing2.hs` | List patterns and recursion. |
| Paper listing 3 | `examples/paper-listings/Listing3.hs` | Guards and applicative combinators. |

The three case-study files do not declare explicit type signatures. Therefore,
they can be used to observe the additional information reported by the
type-enriched executable. The paper-listing files preserve their explicit type
signatures and are used to validate the structural description of explicit
source syntax.

## Current scope

The prototype generates explanatory prose for the supported syntactic forms
implemented in `src/Prose/AstProseGenerator.hs`, including function
definitions, type signatures, patterns, guards, selected list patterns,
infix expressions, function applications, and `case` alternatives.

Type-enriched prose is currently limited to inferred types of top-level
functions. It does not yet generate explanations for the types of all internal
subexpressions or for every complex polymorphic type.
