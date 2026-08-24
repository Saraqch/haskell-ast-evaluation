{-# LANGUAGE ImportQualifiedPost #-}

module Typechecker
  ( FunTypeInfo (..),
    TypecheckReport (..),
    analyzeAndTypecheck,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Data)
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import GHC
  ( LoadHowMuch (LoadAllTargets),
    Module,
    backend,
    failed,
    getModuleGraph,
    getSessionDynFlags,
    guessTarget,
    load,
    mgModSummaries,
    ml_hs_file,
    modInfoTyThings,
    moduleInfo,
    ms_location,
    ms_mod,
    noBackend,
    parseModule,
    pm_parsed_source,
    runGhc,
    setSessionDynFlags,
    setTargets,
    typecheckModule,
  )
import GHC.Hs.Dump
  ( BlankEpAnnotations (..),
    BlankSrcSpan (..),
    showAstData,
    showAstDataFull,
  )
import GHC.Paths (libdir)
import GHC.Types.Id (idName, idType)
import GHC.Types.Name (Name, getOccString, nameModule_maybe)
import GHC.Types.TyThing (TyThing (..))
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import System.Directory (canonicalizePath)

data FunTypeInfo = FunTypeInfo
  { functionName :: String,
    inferredType :: String
  }
  deriving (Show, Eq)

data TypecheckReport = TypecheckReport
  { rawAst :: String,
    readableAst :: String,
    inferredFunctions :: [FunTypeInfo]
  }
  deriving (Show)

analyzeAndTypecheck :: FilePath -> IO (Either String TypecheckReport)
analyzeAndTypecheck targetFile = do
  result <- try (runAnalysis targetFile) :: IO (Either SomeException TypecheckReport)
  case result of
    Left err -> return (Left (show err))
    Right report -> return (Right report)

runAnalysis :: FilePath -> IO TypecheckReport
runAnalysis targetFile = do
  canonicalTarget <- canonicalizePath targetFile
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags {backend = noBackend}
    _ <- setSessionDynFlags dflags'

    target <- guessTarget canonicalTarget Nothing Nothing
    setTargets [target]

    loadResult <- load LoadAllTargets
    if failed loadResult
      then fail "Could not load target module in GHC"
      else do
        modGraph <- getModuleGraph
        let summaries = mgModSummaries modGraph
        matching <-
          liftIO $
            mapM
              ( \sm ->
                  case ml_hs_file (ms_location sm) of
                    Nothing -> return Nothing
                    Just path -> do
                      canonical <- canonicalizePath path
                      return (if canonical == canonicalTarget then Just sm else Nothing)
              )
              summaries

        summary <-
          case catMaybes matching of
            sm : _ -> return sm
            [] -> fail "Could not find ModSummary for target file."

        parsedMod <- parseModule summary
        let ast = pm_parsed_source parsedMod
            rawAstText = renderRawAst ast
            readableAstText = renderReadableAst ast

        typedModule <- typecheckModule parsedMod
        let currentModule = ms_mod summary
            tyThings = modInfoTyThings (moduleInfo typedModule)
            inferred = extractFunctions currentModule tyThings

        return
          TypecheckReport
            { rawAst = rawAstText,
              readableAst = readableAstText,
              inferredFunctions = inferred
            }

extractFunctions :: Module -> [TyThing] -> [FunTypeInfo]
extractFunctions currentModule =
  sortOn functionName . mapMaybe (toFunType currentModule)

toFunType :: Module -> TyThing -> Maybe FunTypeInfo
toFunType currentModule (AnId ident)
  | belongsToCurrentModule && not (isInternalOccName name) =
      Just
        FunTypeInfo
          { functionName = getOccString name,
            inferredType = renderType (idType ident)
          }
  | otherwise = Nothing
  where
    name = idName ident
    belongsToCurrentModule =
      case nameModule_maybe name of
        Just m -> m == currentModule
        Nothing -> False
toFunType _ _ = Nothing

isInternalOccName :: Name -> Bool
isInternalOccName name =
  case getOccString name of
    '$' : _ -> True
    _ -> False

renderRawAst :: (Data a) => a -> String
renderRawAst = showSDocUnsafe . showAstDataFull

renderReadableAst :: (Data a) => a -> String
renderReadableAst = showSDocUnsafe . showAstData BlankSrcSpan BlankEpAnnotations

renderType :: (Outputable a) => a -> String
renderType = showSDocUnsafe . ppr