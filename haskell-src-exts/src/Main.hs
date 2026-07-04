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
    [path] -> processFile path
    _ -> putStrLn "Usage: cabal run haskell-src-exts-test -- path/to/file.hs"

processFile :: FilePath -> IO ()
processFile path = do
  putStrLn ("Input file: " ++ path)
  content <- readFile path
  case parseModule content of
    ParseFailed loc msg ->
      putStrLn ("Parse error at " ++ show loc ++ ": " ++ msg)
    ParseOk ast -> do
      putStrLn "\n 1. Raw AST with source positions"
      print ast
      putStrLn "\n 2. Simplified AST without positions using void"
      print (void ast)
      putStrLn "\n 3. Source code reconstruction with prettyPrint"
      putStrLn (prettyPrint ast)
      putStrLn "\n 4. Manual AST traversal"
      exploreModule ast

exploreModule :: Module l -> IO ()
exploreModule (Module _ _ _ _ declarations) = do
  putStrLn ("Declarations found: " ++ show (length declarations))
  mapM_ exploreDeclaration declarations
exploreModule _ =
  putStrLn "Unsupported module shape (XmlPage / XmlHybrid)."

exploreDeclaration :: Decl l -> IO ()
exploreDeclaration (FunBind _ [Match _ name patterns (UnGuardedRhs _ body) _]) = do
  putStrLn "* Type          : function definition"
  putStrLn ("* Name          : " ++ nameOf name)
  putStrLn ("* Parameters    : " ++ show (length patterns))
  putStrLn ("* Body          : " ++ prettyPrint body)
  exploreExpression body
exploreDeclaration other =
  putStrLn ("* Declaration not covered: " ++ prettyPrint other)

nameOf :: Name l -> String
nameOf (Ident _ s) = s
nameOf (Symbol _ s) = s

exploreExpression :: Exp l -> IO ()
exploreExpression (InfixApp _ lhs op rhs) = do
  putStrLn "* Expression    : infix application (binary operator)"
  putStrLn ("  - Left       : " ++ prettyPrint lhs)
  putStrLn ("  - Operator   : " ++ prettyPrint op)
  putStrLn ("  - Right      : " ++ prettyPrint rhs)
exploreExpression other =
  putStrLn ("* Expression not covered: " ++ prettyPrint other)
