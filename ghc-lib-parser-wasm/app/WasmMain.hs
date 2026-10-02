module Main (main) where

import GHC.Types.SrcLoc (unLoc)
import HaskellSourceParser (ParseFailure (..), ParsedSource (..), parseHaskellSource)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Prose.AstProseGenerator (generateAstProse)
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- WASI command: Haskell source on stdin, prose on stdout, parse errors on stderr.
main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  source <- getContents
  case parseHaskellSource "Input.hs" source of
    Left failure -> do
      hPutStrLn stderr (parseFailureMessage failure)
      exitWith (ExitFailure 1)
    Right parsed ->
      putStrLn (renderString (layoutPretty defaultLayoutOptions (generateAstProse (unLoc (parsedModule parsed)))))
