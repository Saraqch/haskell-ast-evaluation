{-# LANGUAGE ImportQualifiedPost #-}

module HaskellSourceParser
  ( ParseFailure (..),
    ParsedSource (..),
    parseHaskellFile,
    parseHaskellSource,
  )
where

import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.SrcLoc (Located, mkRealSrcLoc)
import GHC.Utils.Error (DiagOpts (..))
import GHC.Utils.Outputable (Outputable, defaultSDocContext, ppr, showSDocUnsafe)
import System.Directory (canonicalizePath)

data ParseFailure = ParseFailure
  { failedFilePath :: FilePath,
    parseFailureMessage :: String
  }

data ParsedSource = ParsedSource
  { sourceFilePath :: FilePath,
    parsedModule :: Located (HsModule GhcPs)
  }

parseHaskellFile :: FilePath -> IO (Either ParseFailure ParsedSource)
parseHaskellFile inputPath = do
  absolutePath <- canonicalizePath inputPath
  content <- readFile absolutePath
  return (parseHaskellSource absolutePath content)

parseHaskellSource :: FilePath -> String -> Either ParseFailure ParsedSource
parseHaskellSource sourcePath content =
  let opts =
        mkParserOpts
          EnumSet.empty
          emptyDiagOpts
          []
          False
          False
          False
          False
      buffer = stringToStringBuffer content
      loc = mkRealSrcLoc (mkFastString sourcePath) 1 1
      parserState = initParserState opts buffer loc
   in case unP parseModule parserState of
        PFailed parserState' ->
          Left
            ParseFailure
              { failedFilePath = sourcePath,
                parseFailureMessage = render (getPsErrorMessages parserState')
              }
        POk _ ast ->
          Right
            ParsedSource
              { sourceFilePath = sourcePath,
                parsedModule = ast
              }

emptyDiagOpts :: DiagOpts
emptyDiagOpts =
  DiagOpts
    { diag_warning_flags = EnumSet.empty,
      diag_fatal_warning_flags = EnumSet.empty,
      diag_warn_is_error = False,
      diag_reverse_errors = False,
      diag_max_errors = Nothing,
      diag_ppr_ctx = defaultSDocContext
    }

render :: (Outputable a) => a -> String
render = showSDocUnsafe . ppr
