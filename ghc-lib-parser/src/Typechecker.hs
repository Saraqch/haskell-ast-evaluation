{-# LANGUAGE ImportQualifiedPost #-}

import Control.Exception (SomeException, try)
import GHC (runGhc, getSessionDynFlags, setSessionDynFlags, backend, noBackend)
import GHC.Paths (libdir)
import System.Directory (canonicalizePath)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import GHC
  (
    LoadHowMuch (LoadAllTargets),
    failed,
    getModuleGraph,
    guessTarget,
    load,
    mgModSummaries,
    ml_hs_file,
    ms_location,
    setTargets
  )

module Typechecker
  ( FunTypeInfo (..),
    TypecheckReport (..),
    analyzeAndTypecheck,
  )
where
 
data FunTypeInfo = FunTypeInfo
  {
    functionName :: String,
    inferredType :: String
  }
  deriving (Show, Eq)

data TypecheckReport = TypecheckReport
  {
    rawAst :: String,
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
    let dflags' = dflags { backend = noBackend}
    _ <- setSessionDynFlags dflags'

  target <- guessTarget canonicalTarget Nothing Nothing
  setTargets [target]

  loadResult <- load LoadAllTargets
  if failed loadResult
    then fail "Could not load target module in GHC"
    else do 
      modGrapg <- getModuleGraph
      let summaries = mgModSummaries modGraph
      matching <-
         liftIO $ 
            mapM 
              (
                \sm -> 
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

     error "Step 4 completed: sesion GHC started"
