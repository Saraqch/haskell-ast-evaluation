module AstProseGenerator
  ( generateAstProse,
    generateAstProseWithTypes,
  )
where

import GHC.Hs
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation (SrcSpanAnnA, getLocA, realSrcSpan)
import GHC.Types.SrcLoc (GenLocated, srcSpanStartLine, unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)

-- | Generates a deliberately limited Explicit Source Prose prototype from the
-- parsed AST only. It does not use name resolution or inferred type information.
generateAstProse :: HsModule GhcPs -> [String]
generateAstProse = generateAstProseWithTypes (const Nothing)

-- | Generate the same structural prose as 'generateAstProse' and add an
--   inferred-type sentence only when the supplied lookup finds one.
generateAstProseWithTypes :: (String -> Maybe String) -> HsModule GhcPs -> [String]
generateAstProseWithTypes typeForFunction hsModule =
  concatMap (describeLocatedDeclaration typeForFunction) (hsmodDecls hsModule)

describeLocatedDeclaration :: (String -> Maybe String) -> LHsDecl GhcPs -> [String]
describeLocatedDeclaration typeForFunction declaration =
  case unLoc declaration of
    SigD _ _ ->
      [ linePrefix declaration
          ++ "Declare the type signature "
          ++ quoted (render (unLoc declaration))
          ++ "."
      ]
    ValD _ (FunBind _ name matches) ->
      let functionName = render name
       in describeInferredType declaration functionName typeForFunction
            ++ concatMap (describeLocatedFunctionMatch functionName) (unLoc (mg_alts matches))
    other ->
      [ linePrefix declaration
          ++ "The declaration "
          ++ quoted (render other)
          ++ " is not expanded by this AST-only prototype."
      ]

describeInferredType :: LHsDecl GhcPs -> String -> (String -> Maybe String) -> [String]
describeInferredType declaration functionName typeForFunction =
  case typeForFunction functionName of
    Nothing -> []
    Just inferredType ->
      [ linePrefix declaration
          ++ "GHC infers the type "
          ++ quoted inferredType
          ++ " for the function "
          ++ quoted functionName
          ++ "."
      ]

describeLocatedFunctionMatch :: String -> LMatch GhcPs (LHsExpr GhcPs) -> [String]
describeLocatedFunctionMatch name match =
  describeFunctionMatch (linePrefix match) name (unLoc match)

describeFunctionMatch :: String -> String -> Match GhcPs (LHsExpr GhcPs) -> [String]
describeFunctionMatch prefix name match =
  concatMap (describeLocatedFunctionRhs prefix functionIntro) guardedRhss
  where
    functionIntro =
      "Define the function "
        ++ quoted name
        ++ " with "
        ++ describeParameters (map render (m_pats match))
        ++ " to"
    GRHSs _ guardedRhss _ = m_grhss match

describeLocatedFunctionRhs :: String -> String -> LGRHS GhcPs (LHsExpr GhcPs) -> [String]
describeLocatedFunctionRhs prefix intro guardedRhs =
  describeFunctionRhs prefix intro (unLoc guardedRhs)

describeFunctionRhs :: String -> String -> GRHS GhcPs (LHsExpr GhcPs) -> [String]
describeFunctionRhs prefix intro (GRHS _ guards body)
  | null guards = describeReturnedBody prefix intro (unLoc body)
  | otherwise =
      [ prefix
          ++ intro
          ++ " return the expression "
          ++ describeExpression (unLoc body)
          ++ " when the guard sequence "
          ++ quoted (render guards)
          ++ " succeeds."
      ]

describeReturnedBody :: String -> String -> HsExpr GhcPs -> [String]
describeReturnedBody prefix intro expression =
  case expression of
    HsCase _ scrutinee matches ->
      [ prefix
          ++ intro
          ++ " return the expression belonging to the first alternative whose pattern matches "
          ++ describeExpression (unLoc scrutinee)
          ++ "."
      ]
        ++ concatMap describeLocatedCaseAlternative (unLoc (mg_alts matches))
    _ ->
      [ prefix
          ++ intro
          ++ " return "
          ++ describeExpression expression
          ++ "."
      ]

describeLocatedCaseAlternative :: LMatch GhcPs (LHsExpr GhcPs) -> [String]
describeLocatedCaseAlternative match =
  describeCaseAlternative (linePrefix match) (unLoc match)

describeCaseAlternative :: String -> Match GhcPs (LHsExpr GhcPs) -> [String]
describeCaseAlternative prefix match =
  concatMap (describeLocatedCaseRhs prefix patternDescription) guardedRhss
  where
    patternDescription =
      case m_pats match of
        [pattern'] -> "the pattern " ++ quoted (render pattern')
        patterns -> "the patterns " ++ quoted (commaSeparated (map render patterns))
    GRHSs _ guardedRhss _ = m_grhss match

describeLocatedCaseRhs :: String -> String -> LGRHS GhcPs (LHsExpr GhcPs) -> [String]
describeLocatedCaseRhs prefix patternDescription guardedRhs =
  describeCaseRhs prefix patternDescription (unLoc guardedRhs)

describeCaseRhs :: String -> String -> GRHS GhcPs (LHsExpr GhcPs) -> [String]
describeCaseRhs prefix patternDescription (GRHS _ guards body)
  | null guards =
      [ prefix
          ++ "When the scrutinized expression matches "
          ++ patternDescription
          ++ ", return "
          ++ describeExpression (unLoc body)
          ++ "."
      ]
  | otherwise =
      [ prefix
          ++ "The alternative with "
          ++ patternDescription
          ++ " and guard sequence "
          ++ quoted (render guards)
          ++ " is not expanded by this AST-only prototype."
      ]

describeExpression :: HsExpr GhcPs -> String
describeExpression (HsPar _ _ expression _) = describeExpression (unLoc expression)
describeExpression (OpApp _ left operator right) =
  "the expression formed by applying the infix operator "
    ++ quoted (operatorText (render operator))
    ++ " to (1) ["
    ++ describeExpression (unLoc left)
    ++ "]; and (2) ["
    ++ describeExpression (unLoc right)
    ++ "]"
describeExpression (HsApp _ function argument) =
  "the result of applying "
    ++ describeExpression (unLoc function)
    ++ " to "
    ++ describeExpression (unLoc argument)
describeExpression (HsVar _ name) =
  "the identifier " ++ quoted (render name)
describeExpression expression@(HsOverLit _ _) =
  "the literal value " ++ quoted (render expression)
describeExpression expression =
  "the source expression " ++ quoted (render expression)

describeParameters :: [String] -> String
describeParameters [] = "no parameters"
describeParameters [parameter] = "the parameter " ++ quoted parameter
describeParameters parameters =
  "the parameters " ++ andSeparated (map quoted parameters)

linePrefix :: GenLocated SrcSpanAnnA a -> String
linePrefix located =
  "[line " ++ show (srcSpanStartLine (realSrcSpan (getLocA located))) ++ "] "

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

quoted :: String -> String
quoted text = "\"" ++ text ++ "\""

render :: (Outputable a) => a -> String
render = showSDocUnsafe . ppr
