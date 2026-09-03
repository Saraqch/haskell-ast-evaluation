module Main where

import System.Environment (getArgs)
import Analysis.Typechecker (analyzeAndTypecheck, inferredFunctions, typedBindings)
import Analysis.TypedAstFacts (TypedFunctionFact (..), extractTypedFunctionFacts)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> inspectTypedAst path
    _ -> putStrLn "Usage: run this diagnostic with a Haskell source-file path."

inspectTypedAst :: FilePath -> IO ()
inspectTypedAst path = do
  result <- analyzeAndTypecheck path
  case result of
    Left err -> do
      putStrLn "Typechecking error:"
      putStrLn err
    Right report -> do
      let facts = extractTypedFunctionFacts (typedBindings report) (inferredFunctions report)
      putStrLn "Generalized function types associated with the GhcTc AST:"
      mapM_ printFact facts

printFact :: TypedFunctionFact -> IO ()
printFact fact =
  putStrLn (typedFunctionName fact ++ " :: " ++ typedFunctionType fact)
