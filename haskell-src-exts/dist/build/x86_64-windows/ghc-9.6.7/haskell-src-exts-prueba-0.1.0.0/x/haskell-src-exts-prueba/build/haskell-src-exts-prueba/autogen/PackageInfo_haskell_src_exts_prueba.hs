{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskell_src_exts_prueba (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskell_src_exts_prueba"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Prueba de obtenci\243n de AST con haskell-src-exts"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
