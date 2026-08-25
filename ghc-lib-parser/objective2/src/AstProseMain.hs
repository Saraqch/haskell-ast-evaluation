module Main where

import AstProseGenerator (generateAstProse)
import GHC.Types.SrcLoc (unLoc)
import HaskellSourceParser
  ( ParseFailure (..),
    ParsedSource (..),
    parseHaskellFile,
  )
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> explainFile path
    _ -> putStrLn "Usage: cabal run ghc-lib-ast-prose -- path/to/file.hs"

explainFile :: FilePath -> IO ()
explainFile path = do
  result <- parseHaskellFile path
  case result of
    Left failure -> do
      putStrLn $ "Input file: " ++ failedFilePath failure
      putStrLn "Parse error:"
      putStrLn (parseFailureMessage failure)
    Right source -> do
      putStrLn $ "Input file: " ++ sourceFilePath source
      putStrLn "\nAST-only explanatory prose:"
      mapM_ putStrLn (generateAstProse (unLoc (parsedModule source)))
