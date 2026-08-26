module Main where

import AstProseGenerator
  ( generateAstProseWithResolvedGuards,
    sourceSignatureFunctionNames,
  )
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
          declaredFunctions = sourceSignatureFunctionNames (unLoc (parsedAst report))
          inferredFacts = filter ((`notElem` declaredFunctions) . TypedAstFacts.typedFunctionName) facts
      putStrLn "AST and type-enriched explanatory prose:"
      mapM_ putStrLn (generateAstProseWithResolvedGuards resolvedGuards (unLoc (parsedAst report)))
      printTypeEnrichedInformation inferredFacts

printTypeEnrichedInformation :: [TypedAstFacts.TypedFunctionFact] -> IO ()
printTypeEnrichedInformation [] = return ()
printTypeEnrichedInformation facts = do
  putStrLn ""
  putStrLn "Type-enriched information:"
  mapM_ (mapM_ putStrLn . typeProseForFunction) facts

typeProseForFunction :: TypedAstFacts.TypedFunctionFact -> [String]
typeProseForFunction fact =
  describeInferredFunctionType
    (TypedAstFacts.typedFunctionName fact)
    (TypedAstFacts.typedFunctionType fact)
