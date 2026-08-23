{-# LANGUAGE ImportQualifiedPost #-}

import Control.Exception (SomeException, try)

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
    
