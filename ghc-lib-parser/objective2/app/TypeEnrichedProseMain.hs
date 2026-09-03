module Main where

import Analysis.RenamedAstFacts (extractResolvedGuards)
import Analysis.Typechecker (analyzeAndTypecheck, inferredFunctions, parsedAst, renamedAst, typedBindings)
import qualified Analysis.TypedAstFacts as TypedAstFacts
import GHC.Types.SrcLoc (unLoc)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prose.AstProseGenerator
  ( generateAstProseWithResolvedGuards,
    sourceSignatureFunctionNames,
  )
import Prose.TypeProseGenerator (describeInferredFunctionType)
import System.Environment (getArgs)

-- | Official type-enriched entry point for Objective 2. It preserves the
-- structural prose and adds available static type facts inferred by GHC.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> explainWithTypes path
    _ -> putStrLn "Usage: cabal run ghc-lib-typed-ast-prose -- path/to/file.hs"

explainWithTypes :: FilePath -> IO ()
explainWithTypes path = do
  result <- analyzeAndTypecheck path
  case result of
    Left err -> do
      putStrLn "Analysis and typechecking error:"
      putStrLn err
    Right report -> do
      let parsedModule = unLoc (parsedAst report)
          resolvedGuards = maybe [] extractResolvedGuards (renamedAst report)
          astProse = generateAstProseWithResolvedGuards resolvedGuards parsedModule
          typedFacts = TypedAstFacts.extractTypedFunctionFacts (typedBindings report) (inferredFunctions report)
          declaredFunctions = sourceSignatureFunctionNames parsedModule
          inferredFacts = filter ((`notElem` declaredFunctions) . TypedAstFacts.typedFunctionName) typedFacts
      putStrLn ("Input file: " <> path)
      putStrLn "\nType-enriched structural explanatory prose:"
      putStrLn (renderProse astProse)
      putStrLn "\nAdditional static type information inferred by GHC:"
      printTypeEnrichedInformation declaredFunctions inferredFacts

renderProse :: Doc ann -> String
renderProse = renderString . layoutPretty defaultLayoutOptions

printTypeEnrichedInformation :: [String] -> [TypedAstFacts.TypedFunctionFact] -> IO ()
printTypeEnrichedInformation declaredFunctions []
  | null declaredFunctions =
      putStrLn "No top-level inferred function type was available for this input."
  | otherwise =
      putStrLn "No additional type information is shown because the source declares its function type explicitly."
printTypeEnrichedInformation _ facts =
  mapM_ (mapM_ putStrLn . typeProseForFunction) facts

typeProseForFunction :: TypedAstFacts.TypedFunctionFact -> [String]
typeProseForFunction fact =
  describeInferredFunctionType
    (TypedAstFacts.typedFunctionName fact)
    (TypedAstFacts.typedFunctionType fact)
