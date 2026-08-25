module AstProseGenerator
  ( generateAstProse,
  )
where

import GHC.Hs
import GHC.Hs.Extension (GhcPs)
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)

generateAstProse :: HsModule GhcPs -> [String]
generateAstProse hsModule =
  concatMap (describeDeclaration . unLoc) (hsmodDecls hsModule)

describeDeclaration :: HsDecl GhcPs -> [String]
describeDeclaration (ValD _ (FunBind _ name matches)) =
  concatMap (describeMatch (render name) . unLoc) (unLoc (mg_alts matches))
describeDeclaration declaration =
  ["The declaration " ++ quoted (render declaration) ++ " is not covered by this prototype."]

describeMatch :: String -> Match GhcPs (LHsExpr GhcPs) -> [String]
describeMatch name match =
  functionSentence : prefixFirst "Its body " (describeGuardedRhss (m_grhss match))
  where
    parameters = map render (m_pats match)
    functionSentence =
      "The function "
        ++ quoted name
        ++ " receives "
        ++ describeParameters parameters
        ++ "."

describeGuardedRhss :: GRHSs GhcPs (LHsExpr GhcPs) -> [String]
describeGuardedRhss (GRHSs _ guardedRhss _) =
  concatMap (describeGuardedRhs . unLoc) guardedRhss

describeGuardedRhs :: GRHS GhcPs (LHsExpr GhcPs) -> [String]
describeGuardedRhs (GRHS _ guards body)
  | null guards = describeExpression (unLoc body)
  | otherwise =
      [ "Its guarded body "
          ++ quoted (render body)
          ++ " is not covered by this prototype."
      ]

describeExpression :: HsExpr GhcPs -> [String]
describeExpression (OpApp _ left operator right) =
  [ "applies the infix operator "
      ++ quoted (operatorText (render operator))
      ++ " to "
      ++ quoted (render left)
      ++ " and "
      ++ quoted (render right)
      ++ "."
  ]
    ++ describeNestedExpression (unLoc left)
    ++ describeNestedExpression (unLoc right)
describeExpression (HsApp _ function argument) =
  [ "applies "
      ++ quoted (render function)
      ++ " to the argument "
      ++ quoted (render argument)
      ++ "."
  ]
    ++ describeNestedExpression (unLoc function)
    ++ describeNestedExpression (unLoc argument)
describeExpression (HsCase _ scrutinee matches) =
  [ "evaluates the expression "
      ++ quoted (render scrutinee)
      ++ " using pattern matching with "
      ++ show (length (unLoc (mg_alts matches)))
      ++ " alternatives."
  ]
    ++ concatMap (describeCaseAlternative . unLoc) (unLoc (mg_alts matches))
describeExpression expression =
  [ "uses the expression "
      ++ quoted (render expression)
      ++ "."
  ]

describeNestedExpression :: HsExpr GhcPs -> [String]
describeNestedExpression expression@(OpApp _ _ _ _) =
  prefixFirst "Within that expression, it " (describeExpression expression)
describeNestedExpression expression@(HsApp _ _ _) =
  prefixFirst "Within that expression, it " (describeExpression expression)
describeNestedExpression expression@(HsCase _ _ _) =
  prefixFirst "Within that expression, it " (describeExpression expression)
describeNestedExpression (HsPar _ _ expression _) =
  describeNestedExpression (unLoc expression)
describeNestedExpression _ = []

describeCaseAlternative :: Match GhcPs (LHsExpr GhcPs) -> [String]
describeCaseAlternative match =
  case m_pats match of
    [pattern'] ->
      prefixFirst
        ( "For the pattern "
          ++ quoted (render pattern')
          ++ ", the branch "
        )
        (describeGuardedRhss (m_grhss match))
    patterns ->
      [ "The alternative with patterns "
          ++ quoted (commaSeparated (map render patterns))
          ++ " is not covered by this prototype."
      ]

describeParameters :: [String] -> String
describeParameters [] = "no parameters"
describeParameters [parameter] = "the parameter " ++ quoted parameter
describeParameters parameters =
  "the parameters " ++ andSeparated (map quoted parameters)

commaSeparated :: [String] -> String
commaSeparated [] = ""
commaSeparated items = foldr1 (\item rest -> item ++ ", " ++ rest) items

andSeparated :: [String] -> String
andSeparated [] = ""
andSeparated [item] = item
andSeparated [first, second] = first ++ " and " ++ second
andSeparated items = commaSeparated (init items) ++ ", and " ++ last items

operatorText :: String -> String
operatorText ('(' : operator) =
  case reverse operator of
    ')' : reversedOperator -> reverse reversedOperator
    _ -> '(' : operator
operatorText operator = operator

prefixFirst :: String -> [String] -> [String]
prefixFirst _ [] = []
prefixFirst prefix (sentence : remaining) = (prefix ++ sentence) : remaining

quoted :: String -> String
quoted text = "\"" ++ text ++ "\""

render :: (Outputable a) => a -> String
render = showSDocUnsafe . ppr
