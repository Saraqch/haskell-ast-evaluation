{-# LANGUAGE ImportQualifiedPost #-}

module Main where

-- ghc-lib-parser 9.6.7.20250325
-- Prueba de extraccion del AST usando el parser real de GHC
-- (la misma infraestructura interna que usa haskell-language-server)

import Data.Data (Data)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.Hs.Dump (BlankEpAnnotations (..), BlankSrcSpan (..), showAstData, showAstDataFull)
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    ParserOpts,
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.SrcLoc (GenLocated (..), Located, mkRealSrcLoc, unLoc)
import GHC.Utils.Error (DiagOpts (..))
import GHC.Utils.Outputable (Outputable, defaultSDocContext, ppr, showSDocUnsafe)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [ruta] -> procesarArchivo ruta
    _ -> putStrLn "Uso: cabal run ghc-lib-parser-prueba -- ruta/al/archivo.hs"

procesarArchivo :: FilePath -> IO ()
procesarArchivo ruta = do
  rutaAbsoluta <- canonicalizePath ruta
  putStrLn $ "Archivo de entrada: " ++ rutaAbsoluta
  contenido <- readFile rutaAbsoluta

  let opts =
        mkParserOpts
          EnumSet.empty -- sin extensiones extra
          diagOptsVacio -- opciones de diagnostico minimas
          [] -- pragmas
          False -- safe imports
          False -- keep Haddock tokens
          False -- keep comments
          False -- magic hash
      buffer = stringToStringBuffer contenido
      loc = mkRealSrcLoc (mkFastString rutaAbsoluta) 1 1
      estado = initParserState opts buffer loc

  case unP parseModule estado of
    PFailed pst -> do
      putStrLn "Error de parseo:"
      putStrLn (mostrar (getPsErrorMessages pst))
    POk _ ast -> do
      putStrLn "\n 1.AST crudo generado por la libreria"
      putStrLn (mostrarAstCrudo ast)
      putStrLn "\n 2.AST simplificado para lectura"
      putStrLn (mostrarAstLegible ast)
      putStrLn "\n 3.Recorrido manual del AST"
      explorarModulo (unLoc ast)

diagOptsVacio :: DiagOpts
diagOptsVacio =
  DiagOpts
    { diag_warning_flags = EnumSet.empty,
      diag_fatal_warning_flags = EnumSet.empty,
      diag_warn_is_error = False,
      diag_reverse_errors = False,
      diag_max_errors = Nothing,
      diag_ppr_ctx = defaultSDocContext
    }

mostrar :: (Outputable a) => a -> String
mostrar = showSDocUnsafe . ppr

mostrarAstCrudo :: (Data a) => a -> String
mostrarAstCrudo = showSDocUnsafe . showAstDataFull

mostrarAstLegible :: (Data a) => a -> String
mostrarAstLegible = showSDocUnsafe . showAstData BlankSrcSpan BlankEpAnnotations

explorarModulo :: HsModule GhcPs -> IO ()
explorarModulo modulo = do
  let decls = hsmodDecls modulo
  putStrLn $ "Declaraciones encontradas: " ++ show (length decls)
  mapM_ (explorarDeclaracion . unLoc) decls

explorarDeclaracion :: HsDecl GhcPs -> IO ()
explorarDeclaracion (ValD _ (FunBind _ nombre mg)) = do
  putStrLn "* Tipo     : definicion de funcion"
  putStrLn $ "* Nombre   : " ++ mostrar nombre
  mapM_ (explorarMatch . unLoc) (unLoc (mg_alts mg))
explorarDeclaracion otra =
  putStrLn $ "* Declaracion no cubierta: " ++ mostrar otra

explorarMatch :: Match GhcPs (LHsExpr GhcPs) -> IO ()
explorarMatch match = do
  putStrLn $ "* Parametros: " ++ show (length (m_pats match))
  explorarGRHSs (m_grhss match)

explorarGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> IO ()
explorarGRHSs (GRHSs _ grhss _) = mapM_ (explorarGRHS . unLoc) grhss

explorarGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> IO ()
explorarGRHS (GRHS _ _ cuerpo) = do
  putStrLn $ "* Cuerpo    : " ++ mostrar cuerpo
  explorarExpresion (unLoc cuerpo)

explorarExpresion :: HsExpr GhcPs -> IO ()
explorarExpresion (OpApp _ izq op der) = do
  putStrLn "* Expresion : aplicacion infija (operador binario)"
  putStrLn $ "  - Izquierdo : " ++ mostrar izq
  putStrLn $ "  - Operador  : " ++ mostrar op
  putStrLn $ "  - Derecho   : " ++ mostrar der
explorarExpresion otra =
  putStrLn $ "* Expresion no cubierta: " ++ mostrar otra