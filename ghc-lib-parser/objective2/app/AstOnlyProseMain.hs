module Main where

import Analysis.RenamedAstFacts (extractResolvedGuards)
import Analysis.Typechecker (analyzeAndTypecheck, parsedAst, renamedAst)
import GHC.Types.SrcLoc (unLoc)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prose.AstProseGenerator (generateAstProseWithResolvedGuards)
import System.Environment (getArgs)

-- | Official AST-only entry point for Objective 2. The generated prose does
-- not include facts inferred from GHC's typechecker.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> explainAstOnly path
    _ -> putStrLn "Usage: cabal run ghc-lib-ast-prose -- path/to/file.hs"

explainAstOnly :: FilePath -> IO ()
explainAstOnly path = do
  result <- analyzeAndTypecheck path
  case result of
    Left err -> do
      putStrLn "Analysis and typechecking error:"
      putStrLn err
    Right report -> do
      let resolvedGuards = maybe [] extractResolvedGuards (renamedAst report)
          astProse = generateAstProseWithResolvedGuards resolvedGuards (unLoc (parsedAst report))
      putStrLn ("Input file: " <> path)
      putStrLn (renderProse astProse)

renderProse :: Doc ann -> String
renderProse = renderString . layoutPretty defaultLayoutOptions
