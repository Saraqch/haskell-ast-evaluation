module Main where

import AstProseGenerator (generateAstProseWithTypes)
import Data.List (find)
import GHC.Types.SrcLoc (unLoc)
import RenamedAstFacts (extractResolvedGuards)
import System.Environment (getArgs)
import Typechecker (analyzeAndTypecheck, inferredFunctions, parsedAst, renamedAst, typedBindings)
import TypeProseGenerator (describeInferredFunctionType)
import qualified TypedAstFacts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> explainFileWithTypes path
    _ -> putStrLn "Usage: cabal run ghc-lib-typed-ast-prose -- path/to/file.hs"

explainFileWithTypes :: FilePath -> IO ()
explainFileWithTypes path = do
  result <- analyzeAndTypecheck path
  case result of
    Left err -> do
      putStrLn "Typechecking error:"
      putStrLn err
    Right report -> do
      let facts = TypedAstFacts.extractTypedFunctionFacts (typedBindings report) (inferredFunctions report)
          resolvedGuards = maybe [] extractResolvedGuards (renamedAst report)
      putStrLn "AST and type-enriched explanatory prose:"
      mapM_ putStrLn (generateAstProseWithTypes (typeProseForFunction facts) resolvedGuards (unLoc (parsedAst report)))

typeProseForFunction :: [TypedAstFacts.TypedFunctionFact] -> String -> [String]
typeProseForFunction facts functionName =
  case find ((== functionName) . TypedAstFacts.typedFunctionName) facts of
    Nothing -> []
    Just fact ->
      describeInferredFunctionType
        (TypedAstFacts.typedFunctionName fact)
        (TypedAstFacts.typedFunctionType fact)
