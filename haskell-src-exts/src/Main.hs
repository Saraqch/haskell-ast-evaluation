module Main where

import Control.Monad (void)
import Language.Haskell.Exts.Parser (ParseResult (..), parseModule)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ruta] -> procesarArchivo ruta
    _ -> putStrLn "Uso: cabal run haskell-src-exts-prueba -- ruta/al/archivo.hs"

procesarArchivo :: FilePath -> IO ()
procesarArchivo ruta = do
  putStrLn ("Archivo de entrada: " ++ ruta)
  contenido <- readFile ruta
  case parseModule contenido of
    ParseFailed loc msg ->
      putStrLn ("Error de parseo en " ++ show loc ++ ": " ++ msg)
    ParseOk ast -> do
      putStrLn "\n 1.AST crudo con posiciones de origen"
      print ast
      putStrLn "\n 2.AST simplificado sin posiciones con void"
      print (void ast)
      putStrLn "\n 3.Reconstruccion del codigo fuente con prettyPrint "
      putStrLn (prettyPrint ast)
      putStrLn "\n 4.Recorrido manual del AST"
      explorarModulo ast

explorarModulo :: Module l -> IO ()
explorarModulo (Module _ _ _ _ declaraciones) = do
  putStrLn ("Declaraciones encontradas: " ++ show (length declaraciones))
  mapM_ explorarDeclaracion declaraciones
explorarModulo _ =
  putStrLn "Forma de modulo no cubierta (XmlPage / XmlHybrid)."

explorarDeclaracion :: Decl l -> IO ()
explorarDeclaracion (FunBind _ [Match _ nombre patrones (UnGuardedRhs _ cuerpo) _]) = do
  putStrLn "* Tipo          : definicion de funcion"
  putStrLn ("* Nombre        : " ++ nombreDe nombre)
  putStrLn ("* Parametros    : " ++ show (length patrones))
  putStrLn ("* Cuerpo        : " ++ prettyPrint cuerpo)
  explorarExpresion cuerpo
explorarDeclaracion otra =
  putStrLn ("* Declaracion no cubierta: " ++ prettyPrint otra)

nombreDe :: Name l -> String
nombreDe (Ident _ s) = s
nombreDe (Symbol _ s) = s

explorarExpresion :: Exp l -> IO ()
explorarExpresion (InfixApp _ izq op der) = do
  putStrLn "* Expresion     : aplicacion infija (operador binario)"
  putStrLn ("  - Izquierdo  : " ++ prettyPrint izq)
  putStrLn ("  - Operador   : " ++ prettyPrint op)
  putStrLn ("  - Derecho    : " ++ prettyPrint der)
explorarExpresion otra =
  putStrLn ("* Expresion no cubierta: " ++ prettyPrint otra)
