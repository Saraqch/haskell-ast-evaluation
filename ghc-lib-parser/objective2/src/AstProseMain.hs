module Main where

import AstProseGenerator (generateAstProseWithResolvedGuards)
import GHC.Types.SrcLoc (unLoc)
import RenamedAstFacts (extractResolvedGuards)
import System.Environment (getArgs)
import Typechecker (analyzeAndTypecheck, parsedAst, renamedAst)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> explainFile path
    _ -> putStrLn "Usage: cabal run ghc-lib-ast-prose -- path/to/file.hs"

explainFile :: FilePath -> IO ()
explainFile path = do
  result <- analyzeAndTypecheck path
  case result of
    Left err -> do
      putStrLn "Analysis error:"
      putStrLn err
    Right report -> do
      let resolvedGuards = maybe [] extractResolvedGuards (renamedAst report)
      putStrLn $ "Input file: " ++ path
      putStrLn "\nAST-only explanatory prose:"
      mapM_ putStrLn (generateAstProseWithResolvedGuards resolvedGuards (unLoc (parsedAst report)))
