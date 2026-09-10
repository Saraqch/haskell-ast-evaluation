module Analysis.TypedAstFacts
  ( TypedFunctionFact (..),
    extractTypedFunctionFacts,
  )
where

import Data.List (sortOn)
import GHC (TypecheckedSource)
import GHC.Data.Bag (bagToList)
import GHC.Hs (AbsBinds (..), GhcTc, HsBindLR (..), LHsBind)
import GHC.Types.Name (getOccString)
import GHC.Types.SrcLoc (unLoc)
import Analysis.Typechecker (FunTypeInfo (..))

data TypedFunctionFact = TypedFunctionFact
  { typedFunctionName :: String,
    typedFunctionType :: String
  }
  deriving (Show, Eq)

extractTypedFunctionFacts :: TypecheckedSource -> [FunTypeInfo] -> [TypedFunctionFact]
extractTypedFunctionFacts bindings inferredFunctions =
  sortOn typedFunctionName
    [ TypedFunctionFact
        { typedFunctionName = functionName inferred,
          typedFunctionType = inferredType inferred
        }
      | inferred <- inferredFunctions,
        functionName inferred `elem` typedFunctionNames bindings
    ]

typedFunctionNames :: TypecheckedSource -> [String]
typedFunctionNames = concatMap extractFromBinding . bagToList

extractFromBinding :: LHsBind GhcTc -> [String]
extractFromBinding locatedBinding =
  case unLoc locatedBinding of
    FunBind {fun_id = locatedIdentifier} ->
      let identifier = unLoc locatedIdentifier
          name = getOccString identifier
       in if isInternalName name then [] else [name]
    XHsBindsLR absBinds ->
      concatMap extractFromBinding (bagToList (abs_binds absBinds))
    _ -> []

isInternalName :: String -> Bool
isInternalName ('$' : _) = True
isInternalName _ = False
