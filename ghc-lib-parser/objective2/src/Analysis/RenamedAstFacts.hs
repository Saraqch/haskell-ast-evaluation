module Analysis.RenamedAstFacts
  ( ResolvedGuard (..),
    extractResolvedGuards,
  )
where

import GHC (RenamedSource)
import GHC.Data.Bag (bagToList)
import GHC.Hs
import GHC.Hs.Extension (GhcRn)
import GHC.Parser.Annotation (getLocA, realSrcSpan)
import GHC.Types.Name (getOccString)
import GHC.Types.SrcLoc (srcSpanStartLine, unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)

-- | A guard and right-hand expression after GHC has resolved infix fixities.
data ResolvedGuard = ResolvedGuard
  { resolvedGuardLine :: Int,
    resolvedCondition :: String,
    resolvedExpression :: String
  }
  deriving (Show, Eq)

extractResolvedGuards :: RenamedSource -> [ResolvedGuard]
extractResolvedGuards (group, _, _, _) =
  case hs_valds group of
    ValBinds _ bindings _ -> concatMap guardsFromBinding (bagToList bindings)
    XValBindsLR (NValBinds recursiveBindings _) ->
      concatMap (concatMap guardsFromBinding . bagToList . snd) recursiveBindings

guardsFromBinding :: LHsBind GhcRn -> [ResolvedGuard]
guardsFromBinding locatedBinding =
  case unLoc locatedBinding of
    FunBind _ _ matches -> concatMap guardsFromMatch (unLoc (mg_alts matches))
    _ -> []

guardsFromMatch :: LMatch GhcRn (LHsExpr GhcRn) -> [ResolvedGuard]
guardsFromMatch locatedMatch =
  case unLoc locatedMatch of
    Match _ _ _ (GRHSs _ guardedRhss _) -> concatMap guardFromRhs guardedRhss

guardFromRhs :: LGRHS GhcRn (LHsExpr GhcRn) -> [ResolvedGuard]
guardFromRhs locatedRhs =
  case unLoc locatedRhs of
    GRHS _ guards body
      | not (null guards) ->
          [ ResolvedGuard
              { resolvedGuardLine = sourceLine locatedRhs,
                resolvedCondition = describeGuardSequence guards,
                resolvedExpression = structuredExpression (unLoc body)
              }
          ]
      | otherwise -> []

describeGuardSequence :: [GuardLStmt GhcRn] -> String
describeGuardSequence [] = "True"
describeGuardSequence guards = foldr1 (\first rest -> first ++ " && " ++ rest) (map describeGuardStatement guards)

describeGuardStatement :: GuardLStmt GhcRn -> String
describeGuardStatement locatedStatement =
  case unLoc locatedStatement of
    BodyStmt _ condition _ _ -> describeGuardExpression (unLoc condition)
    statement -> render statement

describeGuardExpression :: HsExpr GhcRn -> String
describeGuardExpression (HsVar _ name)
  | getOccString (unLoc name) == "otherwise" = "otherwise (synonym for True)"
describeGuardExpression expression = structuredExpression expression

structuredExpression :: HsExpr GhcRn -> String
structuredExpression (HsPar _ _ expression _) = structuredExpression (unLoc expression)
structuredExpression (OpApp _ left operator right) =
  "("
    ++ structuredExpression (unLoc left)
    ++ " "
    ++ operatorText operator
    ++ " "
    ++ structuredExpression (unLoc right)
    ++ ")"
structuredExpression (HsApp _ function argument) =
  structuredExpression (unLoc function) ++ " " ++ applicationArgument (unLoc argument)
structuredExpression (HsVar _ name) = getOccString (unLoc name)
structuredExpression expression@(HsOverLit _ _) = render expression
structuredExpression expression = render expression

applicationArgument :: HsExpr GhcRn -> String
applicationArgument expression@(OpApp _ _ _ _) = "(" ++ structuredExpression expression ++ ")"
applicationArgument expression = structuredExpression expression

operatorText :: LHsExpr GhcRn -> String
operatorText locatedOperator =
  case unLoc locatedOperator of
    HsVar _ name -> getOccString (unLoc name)
    expression -> render expression

sourceLine located = srcSpanStartLine (realSrcSpan (getLocA located))

render :: (Outputable a) => a -> String
render = showSDocUnsafe . ppr
