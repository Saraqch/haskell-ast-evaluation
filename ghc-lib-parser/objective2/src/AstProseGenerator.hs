module AstProseGenerator
  ( generateAstProse,
    generateAstProseWithResolvedGuards,
    generateAstProseWithTypes,
  )
where

import GHC.Hs
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation (SrcSpanAnn', getLocA, realSrcSpan)
import GHC.Types.SrcLoc (GenLocated, srcSpanStartLine, unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Data.List (find)
import RenamedAstFacts (ResolvedGuard (..))

-- | Generates a deliberately limited Explicit Source Prose prototype from the
-- parsed AST only. It does not use name resolution or inferred type information.
generateAstProse :: HsModule GhcPs -> [String]
generateAstProse = generateAstProseWithTypes (const []) []

-- | Generate AST-only prose using fixity-resolved guard facts when available.
generateAstProseWithResolvedGuards :: [ResolvedGuard] -> HsModule GhcPs -> [String]
generateAstProseWithResolvedGuards = generateAstProseWithTypes (const [])

-- | Generate the same structural prose as 'generateAstProse' and add an
--   inferred-type prose only when the supplied lookup provides it.
generateAstProseWithTypes :: (String -> [String]) -> [ResolvedGuard] -> HsModule GhcPs -> [String]
generateAstProseWithTypes typeForFunction resolvedGuards hsModule =
  concatMap
    (describeLocatedDeclaration declaredFunctionNames typeForFunction resolvedGuards)
    (hsmodDecls hsModule)
  where
    declaredFunctionNames = concatMap signatureFunctionNames (hsmodDecls hsModule)

describeLocatedDeclaration :: [String] -> (String -> [String]) -> [ResolvedGuard] -> LHsDecl GhcPs -> [String]
describeLocatedDeclaration declaredFunctionNames typeForFunction resolvedGuards declaration =
  case unLoc declaration of
    SigD _ signature -> describeTypeSignature declaration signature
    ValD _ (FunBind _ name matches) ->
      let functionName = render name
          inferredType =
            if functionName `elem` declaredFunctionNames
              then []
              else typeForFunction functionName
       in describeInferredType declaration functionName inferredType
            ++ concatMap (describeLocatedFunctionMatch functionName resolvedGuards) (unLoc (mg_alts matches))
    other ->
      [ linePrefix declaration
          ++ "The declaration "
          ++ quoted (render other)
          ++ " is not expanded by this AST-only prototype."
      ]

signatureFunctionNames :: LHsDecl GhcPs -> [String]
signatureFunctionNames declaration =
  case unLoc declaration of
    SigD _ (TypeSig _ names _) -> map render names
    _ -> []

describeInferredType :: LHsDecl GhcPs -> String -> [String] -> [String]
describeInferredType declaration _ = map (\sentence -> linePrefix declaration ++ sentence)

describeTypeSignature :: LHsDecl GhcPs -> Sig GhcPs -> [String]
describeTypeSignature declaration (TypeSig _ [locatedName] signature) =
  case signatureFunctionType signature of
    Just (argumentTypes, resultType) ->
      [ linePrefix declaration
          ++ "Declare a function with name "
          ++ quoted (render locatedName)
          ++ describeFunctionContract argumentTypes resultType
          ++ "."
      ]
    Nothing -> [fallbackTypeSignature declaration]
describeTypeSignature declaration _ = [fallbackTypeSignature declaration]

fallbackTypeSignature :: LHsDecl GhcPs -> String
fallbackTypeSignature declaration =
  linePrefix declaration
    ++ "Declare the type signature "
    ++ quoted (render (unLoc declaration))
    ++ "."

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

describeFunctionContract :: [LHsType GhcPs] -> LHsType GhcPs -> String
describeFunctionContract argumentTypes resultType =
  case argumentTypes of
    [] -> " which returns " ++ describeValueType resultType
    [argumentType] ->
      " which, given "
        ++ describeValueType argumentType
        ++ " argument, returns "
        ++ describeValueType resultType
    _ ->
      " which, given "
        ++ andSeparated (map (\argumentType -> describeValueType argumentType ++ " argument") argumentTypes)
        ++ ", returns "
        ++ describeValueType resultType

describeValueType :: LHsType GhcPs -> String
describeValueType locatedType =
  case unLoc locatedType of
    HsParTy _ innerType -> describeValueType innerType
    HsTyVar _ _ name ->
      case render name of
        "Int" -> "an integer"
        "Integer" -> "an integer"
        "Bool" -> "a Boolean value"
        "Char" -> "a character"
        "String" -> "a string"
        typeName -> "a value of type " ++ quoted typeName
    HsListTy _ elementType -> "a list of " ++ describeValueType elementType
    otherType -> "a value of type " ++ quoted (render otherType)

describeLocatedFunctionMatch :: String -> [ResolvedGuard] -> LMatch GhcPs (LHsExpr GhcPs) -> [String]
describeLocatedFunctionMatch name resolvedGuards match =
  describeFunctionMatch (linePrefix match) name resolvedGuards (unLoc match)

describeFunctionMatch :: String -> String -> [ResolvedGuard] -> Match GhcPs (LHsExpr GhcPs) -> [String]
describeFunctionMatch prefix name resolvedGuards match
  | hasGuards =
      [ prefix
          ++ "Define the function with name "
          ++ quoted name
          ++ " to return the expression belonging to the first satisfied condition:"
      ]
        ++ zipWith (describeGuardedFunctionRhs resolvedGuards) [1 :: Int ..] guardedRhss
        ++ describeArgumentPatterns prefix (m_pats match)
  | otherwise =
      concatMap (describeLocatedFunctionRhs prefix functionIntro) guardedRhss
        ++ describeArgumentPatterns prefix (m_pats match)
  where
    hasGuards = any containsGuards guardedRhss
    containsGuards locatedRhs =
      case unLoc locatedRhs of
        GRHS _ guards _ -> not (null guards)
    functionIntro =
      "Define the function "
        ++ quoted name
        ++ " with "
        ++ describeParameters (map render (m_pats match))
        ++ " to"
    GRHSs _ guardedRhss _ = m_grhss match

describeGuardedFunctionRhs :: [ResolvedGuard] -> Int -> LGRHS GhcPs (LHsExpr GhcPs) -> String
describeGuardedFunctionRhs resolvedGuards position locatedRhs =
  case unLoc locatedRhs of
    GRHS _ guards body ->
      let resolvedGuard = find ((== sourceLine locatedRhs) . resolvedGuardLine) resolvedGuards
          condition = maybe (describeGuardSequence guards) resolvedCondition resolvedGuard
          expression = maybe (structuredExpression (unLoc body)) resolvedExpression resolvedGuard
       in show position
            ++ ". "
            ++ linePrefix locatedRhs
            ++ "Condition: "
            ++ condition
            ++ ". Expression: "
            ++ expression
            ++ "."

describeGuardSequence :: [GuardLStmt GhcPs] -> String
describeGuardSequence [] = "True"
describeGuardSequence guardStatements =
  andSeparated (map describeGuardStatement guardStatements)

describeGuardStatement :: GuardLStmt GhcPs -> String
describeGuardStatement locatedStatement =
  case unLoc locatedStatement of
    BodyStmt _ condition _ _ -> describeGuardExpression (unLoc condition)
    statement -> quoted (render statement)

describeGuardExpression :: HsExpr GhcPs -> String
describeGuardExpression (HsVar _ name)
  | render name == "otherwise" = "otherwise (synonym for True)"
describeGuardExpression expression = structuredExpression expression

describeArgumentPatterns :: String -> [LPat GhcPs] -> [String]
describeArgumentPatterns prefix patterns =
  zipWith describeArgumentPattern [1 :: Int ..] patterns
  where
    describeArgumentPattern position pattern' =
      prefix
        ++ "The "
        ++ ordinal position
        ++ " argument matches the pattern "
        ++ quoted (render pattern')
        ++ "."

ordinal :: Int -> String
ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal position = show position ++ "th"

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

-- | Render a source-like expression while making every binary operator group
-- explicit. This representation is used for guards and guarded expressions.
structuredExpression :: HsExpr GhcPs -> String
structuredExpression (HsPar _ _ expression _) = structuredExpression (unLoc expression)
structuredExpression (OpApp _ left operator right) =
  "("
    ++ structuredExpression (unLoc left)
    ++ " "
    ++ operatorText (render operator)
    ++ " "
    ++ structuredExpression (unLoc right)
    ++ ")"
structuredExpression (HsApp _ function argument) =
  structuredExpression (unLoc function)
    ++ " "
    ++ applicationArgument (unLoc argument)
structuredExpression (HsVar _ name) = render name
structuredExpression expression@(HsOverLit _ _) = render expression
structuredExpression expression = render expression

applicationArgument :: HsExpr GhcPs -> String
applicationArgument expression@(OpApp _ _ _ _) = "(" ++ structuredExpression expression ++ ")"
applicationArgument expression = structuredExpression expression

describeParameters :: [String] -> String
describeParameters [] = "no parameters"
describeParameters [parameter] = "the parameter " ++ quoted parameter
describeParameters parameters =
  "the parameters " ++ andSeparated (map quoted parameters)

linePrefix :: GenLocated (SrcSpanAnn' annotation) a -> String
linePrefix located =
  "[line " ++ show (srcSpanStartLine (realSrcSpan (getLocA located))) ++ "] "

sourceLine :: GenLocated (SrcSpanAnn' annotation) a -> Int
sourceLine = srcSpanStartLine . realSrcSpan . getLocA

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
