module Main where

import AstProseGenerator (generateAstProseWithTypes)
import Data.List (find)
import GHC.Types.SrcLoc (unLoc)
import System.Environment (getArgs)
import Typechecker (analyzeAndTypecheck, inferredFunctions, parsedAst, typedBindings)
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
      putStrLn "AST and type-enriched explanatory prose:"
      mapM_ putStrLn (generateAstProseWithTypes (typeForFunction facts) (unLoc (parsedAst report)))

typeForFunction :: [TypedAstFacts.TypedFunctionFact] -> String -> Maybe String
typeForFunction facts functionName =
  TypedAstFacts.typedFunctionType
    <$> find ((== functionName) . TypedAstFacts.typedFunctionName) facts
