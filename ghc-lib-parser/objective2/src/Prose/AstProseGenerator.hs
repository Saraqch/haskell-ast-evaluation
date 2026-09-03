module Prose.AstProseGenerator
  ( generateAstProse,
    generateAstProseWithResolvedGuards,
    sourceSignatureFunctionNames,
  )
where

import Data.List (find)
import GHC.Hs
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation (SrcSpanAnn', getLocA, realSrcSpan)
import GHC.Types.SrcLoc (GenLocated, srcSpanStartLine, unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Prettyprinter
  ( Doc,
    comma,
    dquotes,
    hsep,
    indent,
    pretty,
    punctuate,
    vcat,
    (<+>),
    (<>),
  )
import Analysis.RenamedAstFacts (ResolvedGuard (..))

-- | Generates a deliberately limited Explicit Source Prose prototype from the
-- parsed AST only. It does not include inferred type information.
generateAstProse :: HsModule GhcPs -> Doc ann
generateAstProse = generateAstProseWithResolvedGuards []

-- | Generate AST-only prose using fixity-resolved guard facts when available.
generateAstProseWithResolvedGuards :: [ResolvedGuard] -> HsModule GhcPs -> Doc ann
generateAstProseWithResolvedGuards resolvedGuards hsModule =
  vcat (map (describeLocatedDeclaration resolvedGuards) (hsmodDecls hsModule))

describeLocatedDeclaration :: [ResolvedGuard] -> LHsDecl GhcPs -> Doc ann
describeLocatedDeclaration resolvedGuards declaration =
  case unLoc declaration of
    SigD _ signature -> describeTypeSignature declaration signature
    ValD _ (FunBind _ name matches) ->
      vcat
        ( map
            (describeLocatedFunctionMatch (renderText name) resolvedGuards)
            (unLoc (mg_alts matches))
        )
    other ->
      vcat
        [ linePrefix declaration,
          pretty "The declaration"
            <+> quoted (renderText other)
            <+> pretty "is not expanded by this AST-only prototype."
        ]

sourceSignatureFunctionNames :: HsModule GhcPs -> [String]
sourceSignatureFunctionNames hsModule =
  foldr
    (\declaration names -> signatureFunctionNames declaration <> names)
    []
    (hsmodDecls hsModule)

signatureFunctionNames :: LHsDecl GhcPs -> [String]
signatureFunctionNames declaration =
  case unLoc declaration of
    SigD _ (TypeSig _ names _) -> map renderText names
    _ -> []

describeTypeSignature :: LHsDecl GhcPs -> Sig GhcPs -> Doc ann
describeTypeSignature declaration (TypeSig _ [locatedName] signature) =
  case signatureFunctionType signature of
    Just (argumentTypes, resultType) ->
      vcat
        [ linePrefix declaration,
          pretty "Declare a function with name"
            <+> quoted (renderText locatedName)
            <+> describeFunctionContract argumentTypes resultType
            <> pretty "."
        ]
    Nothing -> fallbackTypeSignature declaration
describeTypeSignature declaration _ = fallbackTypeSignature declaration

fallbackTypeSignature :: LHsDecl GhcPs -> Doc ann
fallbackTypeSignature declaration =
  vcat
    [ linePrefix declaration,
      pretty "Declare the type signature"
        <+> quoted (renderText (unLoc declaration))
        <> pretty "."
    ]

signatureFunctionType :: LHsSigWcType GhcPs -> Maybe ([LHsType GhcPs], LHsType GhcPs)
signatureFunctionType (HsWC _ locatedSignature) =
  let HsSig _ _ typeBody = unLoc locatedSignature
   in Just (splitFunctionType typeBody)

splitFunctionType :: LHsType GhcPs -> ([LHsType GhcPs], LHsType GhcPs)
splitFunctionType locatedType =
  case unLoc locatedType of
    HsParTy _ innerType -> splitFunctionType innerType
    HsFunTy _ _ argumentType resultType ->
      let (remainingArguments, finalResult) = splitFunctionType resultType
       in (argumentType : remainingArguments, finalResult)
    _ -> ([], locatedType)

describeFunctionContract :: [LHsType GhcPs] -> LHsType GhcPs -> Doc ann
describeFunctionContract argumentTypes resultType =
  case argumentTypes of
    [] -> pretty "which returns" <+> describeValueType resultType
    [argumentType] ->
      pretty "which, given"
        <+> describeValueType argumentType
        <+> pretty "argument, returns"
        <+> describeValueType resultType
    _ ->
      pretty "which, given"
        <+> andSeparatedDocs (map (\argumentType -> describeValueType argumentType <+> pretty "argument") argumentTypes)
        <> comma
        <+> pretty "returns"
        <+> describeValueType resultType

describeValueType :: LHsType GhcPs -> Doc ann
describeValueType locatedType =
  case unLoc locatedType of
    HsParTy _ innerType -> describeValueType innerType
    HsTyVar _ _ name ->
      case renderText name of
        "Int" -> pretty "an integer"
        "Integer" -> pretty "an integer"
        "Bool" -> pretty "a Boolean value"
        "Char" -> pretty "a character"
        "String" -> pretty "a string"
        typeName -> pretty "a value of type" <+> quoted typeName
    HsListTy _ elementType -> pretty "a list of" <+> describeValueType elementType
    otherType -> pretty "a value of type" <+> quoted (renderText otherType)

describeLocatedFunctionMatch :: String -> [ResolvedGuard] -> LMatch GhcPs (LHsExpr GhcPs) -> Doc ann
describeLocatedFunctionMatch name resolvedGuards match =
  describeFunctionMatch (linePrefix match) name resolvedGuards (unLoc match)

describeFunctionMatch :: Doc ann -> String -> [ResolvedGuard] -> Match GhcPs (LHsExpr GhcPs) -> Doc ann
describeFunctionMatch prefix name resolvedGuards match
  | hasGuards =
      vcat
        [ prefix,
          pretty "Define the function with name"
            <+> quoted name
            <+> pretty "to return the expression belonging to the first satisfied condition:",
          indent 4 (vcat (zipWith (describeGuardedFunctionRhs resolvedGuards) [1 :: Int ..] guardedRhss)),
          describeArgumentPatterns prefix (m_pats match)
        ]
  | otherwise =
      vcat
        [ vcat (map (describeLocatedFunctionRhs prefix functionIntro) guardedRhss),
          describeArgumentPatterns prefix (m_pats match)
        ]
  where
    hasGuards = any containsGuards guardedRhss
    containsGuards locatedRhs =
      case unLoc locatedRhs of
        GRHS _ guards _ -> not (null guards)
    functionIntro =
      pretty "Define the function"
        <+> quoted name
        <+> pretty "with"
        <+> describeParameters (map renderText (m_pats match))
        <+> pretty "to"
    GRHSs _ guardedRhss _ = m_grhss match

describeGuardedFunctionRhs :: [ResolvedGuard] -> Int -> LGRHS GhcPs (LHsExpr GhcPs) -> Doc ann
describeGuardedFunctionRhs resolvedGuards position locatedRhs =
  case unLoc locatedRhs of
    GRHS _ guards body ->
      let resolvedGuard = find ((== sourceLine locatedRhs) . resolvedGuardLine) resolvedGuards
          condition = maybe (describeGuardSequence guards) (pretty . resolvedCondition) resolvedGuard
          expression = maybe (structuredExpression (unLoc body)) (pretty . resolvedExpression) resolvedGuard
       in vcat
            [ pretty position <> pretty "." <+> linePrefix locatedRhs,
              indent 4
                ( vcat
                    [ pretty "Condition:" <+> condition,
                      pretty "Expression:" <+> expression
                    ]
                )
            ]

describeGuardSequence :: [GuardLStmt GhcPs] -> Doc ann
describeGuardSequence [] = pretty "True"
describeGuardSequence guardStatements = andSeparatedDocs (map describeGuardStatement guardStatements)

describeGuardStatement :: GuardLStmt GhcPs -> Doc ann
describeGuardStatement locatedStatement =
  case unLoc locatedStatement of
    BodyStmt _ condition _ _ -> describeGuardExpression (unLoc condition)
    statement -> quoted (renderText statement)

describeGuardExpression :: HsExpr GhcPs -> Doc ann
describeGuardExpression (HsVar _ name)
  | renderText name == "otherwise" = pretty "otherwise (synonym for True)"
describeGuardExpression expression = structuredExpression expression

describeArgumentPatterns :: Doc ann -> [LPat GhcPs] -> Doc ann
describeArgumentPatterns _ [] = mempty
describeArgumentPatterns prefix patterns =
  vcat
    [ prefix,
      pretty "when:",
      indent 4 (vcat (zipWith describeArgumentPattern [1 :: Int ..] patterns))
    ]

describeArgumentPattern :: Int -> LPat GhcPs -> Doc ann
describeArgumentPattern position pattern' =
  pretty "The"
    <+> ordinal position
    <+> pretty "argument matches the pattern"
    <+> quoted (renderText pattern')
    <> pretty "."

ordinal :: Int -> Doc ann
ordinal 1 = pretty "first"
ordinal 2 = pretty "second"
ordinal 3 = pretty "third"
ordinal position = pretty position <> pretty "th"

describeLocatedFunctionRhs :: Doc ann -> Doc ann -> LGRHS GhcPs (LHsExpr GhcPs) -> Doc ann
describeLocatedFunctionRhs prefix intro guardedRhs =
  describeFunctionRhs prefix intro (unLoc guardedRhs)

describeFunctionRhs :: Doc ann -> Doc ann -> GRHS GhcPs (LHsExpr GhcPs) -> Doc ann
describeFunctionRhs prefix intro (GRHS _ guards body)
  | null guards =
      vcat
        [ prefix,
          intro <+> pretty "return:",
          indent 4 (describeReturnedBody (unLoc body))
        ]
  | otherwise =
      vcat
        [ prefix,
          intro <+> pretty "return the expression when the following guard sequence succeeds:",
          indent 4 (describeGuardSequence guards),
          indent 4 (describeExpression (unLoc body))
        ]

describeReturnedBody :: HsExpr GhcPs -> Doc ann
describeReturnedBody expression =
  case expression of
    HsCase _ scrutinee matches ->
      vcat
        [ pretty "the expression belonging to the first alternative whose pattern matches:",
          indent 4 (describeExpression (unLoc scrutinee)),
          indent 4 (vcat (map describeLocatedCaseAlternative (unLoc (mg_alts matches))))
        ]
    _ -> describeExpression expression

describeLocatedCaseAlternative :: LMatch GhcPs (LHsExpr GhcPs) -> Doc ann
describeLocatedCaseAlternative match =
  describeCaseAlternative (linePrefix match) (unLoc match)

describeCaseAlternative :: Doc ann -> Match GhcPs (LHsExpr GhcPs) -> Doc ann
describeCaseAlternative prefix match =
  vcat (map (describeLocatedCaseRhs prefix patternDescription) guardedRhss)
  where
    patternDescription =
      case m_pats match of
        [pattern'] -> pretty "the pattern" <+> quoted (renderText pattern')
        patterns -> pretty "the patterns" <+> commaSeparatedDocs (map (quoted . renderText) patterns)
    GRHSs _ guardedRhss _ = m_grhss match

describeLocatedCaseRhs :: Doc ann -> Doc ann -> LGRHS GhcPs (LHsExpr GhcPs) -> Doc ann
describeLocatedCaseRhs prefix patternDescription guardedRhs =
  describeCaseRhs prefix patternDescription (unLoc guardedRhs)

describeCaseRhs :: Doc ann -> Doc ann -> GRHS GhcPs (LHsExpr GhcPs) -> Doc ann
describeCaseRhs prefix patternDescription (GRHS _ guards body)
  | null guards =
      vcat
        [ prefix,
          pretty "When the scrutinized expression matches" <+> patternDescription <> comma,
          indent 4 (pretty "return:"),
          indent 8 (describeExpression (unLoc body))
        ]
  | otherwise =
      vcat
        [ prefix,
          pretty "The alternative with" <+> patternDescription,
          indent 4 (pretty "has a guard sequence not expanded by this AST-only prototype:") ,
          indent 8 (describeGuardSequence guards)
        ]

describeExpression :: HsExpr GhcPs -> Doc ann
describeExpression (HsPar _ _ expression _) = describeExpression (unLoc expression)
describeExpression (OpApp _ left operator right) =
  vcat
    [ pretty "the expression formed by applying the infix operator"
        <+> quoted (operatorText (renderText operator))
        <+> pretty "to:",
      indent 4
        ( vcat
            [ pretty "(1)" <+> describeExpression (unLoc left),
              pretty "(2)" <+> describeExpression (unLoc right)
            ]
        )
    ]
describeExpression (HsApp _ function argument) =
  vcat
    [ pretty "the result of applying:",
      indent 4
        ( vcat
            [ pretty "function:" <+> describeExpression (unLoc function),
              pretty "argument:" <+> describeExpression (unLoc argument)
            ]
        )
    ]
describeExpression (HsVar _ name) = pretty "the identifier" <+> quoted (renderText name)
describeExpression expression@(HsOverLit _ _) = pretty "the literal value" <+> quoted (renderText expression)
describeExpression expression = pretty "the source expression" <+> quoted (renderText expression)

-- | Render a source-like expression while making each nested operator group
-- explicit and visually nested.
structuredExpression :: HsExpr GhcPs -> Doc ann
structuredExpression (HsPar _ _ expression _) = structuredExpression (unLoc expression)
structuredExpression (OpApp _ left operator right) =
  vcat
    [ pretty "the infix expression with operator" <+> quoted (operatorText (renderText operator)) <> pretty ":",
      indent 4
        ( vcat
            [ pretty "(1)" <+> structuredExpression (unLoc left),
              pretty "(2)" <+> structuredExpression (unLoc right)
            ]
        )
    ]
structuredExpression (HsApp _ function argument) =
  vcat
    [ pretty "the function application:",
      indent 4
        ( vcat
            [ pretty "function:" <+> structuredExpression (unLoc function),
              pretty "argument:" <+> applicationArgument (unLoc argument)
            ]
        )
    ]
structuredExpression (HsVar _ name) = pretty (renderText name)
structuredExpression expression@(HsOverLit _ _) = pretty (renderText expression)
structuredExpression expression = pretty (renderText expression)

applicationArgument :: HsExpr GhcPs -> Doc ann
applicationArgument expression@(OpApp _ _ _ _) = indent 4 (structuredExpression expression)
applicationArgument expression = structuredExpression expression

describeParameters :: [String] -> Doc ann
describeParameters [] = pretty "no parameters"
describeParameters [parameter] = pretty "the parameter" <+> quoted parameter
describeParameters parameters = pretty "the parameters" <+> andSeparatedDocs (map quoted parameters)

linePrefix :: GenLocated (SrcSpanAnn' annotation) a -> Doc ann
linePrefix located = pretty "[line" <+> pretty (sourceLine located) <> pretty "]"

sourceLine :: GenLocated (SrcSpanAnn' annotation) a -> Int
sourceLine = srcSpanStartLine . realSrcSpan . getLocA

andSeparatedDocs :: [Doc ann] -> Doc ann
andSeparatedDocs [] = mempty
andSeparatedDocs [item] = item
andSeparatedDocs [first, second] = first <+> pretty "and" <+> second
andSeparatedDocs items =
  hsep (punctuate comma (init items)) <> comma <+> pretty "and" <+> last items

operatorText :: String -> String
operatorText ('(' : operator) =
  case reverse operator of
    ')' : reversedOperator -> reverse reversedOperator
    _ -> '(' : operator
operatorText operator = operator

quoted :: String -> Doc ann
quoted = dquotes . pretty

commaSeparatedDocs :: [Doc ann] -> Doc ann
commaSeparatedDocs = hsep . punctuate comma

renderText :: (Outputable a) => a -> String
renderText = showSDocUnsafe . ppr
