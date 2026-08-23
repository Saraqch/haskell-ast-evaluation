{-# LANGUAGE ImportQualifiedPost #-}

import Control.Exception (SomeException, try)
import GHC (runGhc, getSessionDynFlags, setSessionDynFlags, backend, noBackend)
import GHC.Paths (libdir)
import System.Directory (canonicalizePath)

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

    error "Step 3 completed: sesion GHC started"
    