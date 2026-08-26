module TypeProseGenerator
  ( describeInferredFunctionType,
  )
where

import Data.List (stripPrefix)

-- | Translate selected inferred function-type patterns into student-oriented
--   prose. The technical type is retained as traceable supporting evidence.
describeInferredFunctionType :: String -> String -> [String]
describeInferredFunctionType functionName rawType =
  [ explanatorySentence functionName normalizedType,
    "Technical inferred type: " ++ quoted normalizedType ++ "."
  ]
  where
    normalizedType = removeImplicitForall rawType

explanatorySentence :: String -> String -> String
explanatorySentence functionName inferredType
  | isNumericBinaryType inferredType =
      "GHC infers that function "
        ++ quoted functionName
        ++ " receives two numeric values of the same type and returns a numeric value of that same type."
  | isIntegralUnaryType inferredType =
      "GHC infers that function "
        ++ quoted functionName
        ++ " receives one integral value and returns an integral value."
  | isOptionalHeadType inferredType =
      "GHC infers that function "
        ++ quoted functionName
        ++ " receives a list of values and returns either no value or one value having the list element type."
  | isApplyTwiceType inferredType =
      "GHC infers that function "
        ++ quoted functionName
        ++ " receives a function that maps a value to another value of the same type, then a value of that type, and returns a value of that type."
  | otherwise =
      "GHC infers a function type for "
        ++ quoted functionName
        ++ "."

isNumericBinaryType :: String -> Bool
isNumericBinaryType typeText =
  case stripPrefix "Num " typeText of
    Just remaining ->
      case breakOnConstraint remaining of
        Just variable -> typeText == "Num " ++ variable ++ " => " ++ variable ++ " -> " ++ variable ++ " -> " ++ variable
        Nothing -> False
    Nothing -> False

isIntegralUnaryType :: String -> Bool
isIntegralUnaryType typeText =
  case stripPrefix "Integral " typeText of
    Just remaining ->
      case breakOnConstraint remaining of
        Just variable -> typeText == "Integral " ++ variable ++ " => " ++ variable ++ " -> " ++ variable
        Nothing -> False
    Nothing -> False

breakOnConstraint :: String -> Maybe String
breakOnConstraint remaining =
  case words remaining of
    variable : "=>" : _ -> Just variable
    _ -> Nothing

isOptionalHeadType :: String -> Bool
isOptionalHeadType typeText =
  case stripPrefix "[" typeText of
    Just remaining ->
      case span (/= ']') remaining of
        (variable, ']' : rest) -> rest == " -> Maybe " ++ variable
        _ -> False
    Nothing -> False

isApplyTwiceType :: String -> Bool
isApplyTwiceType typeText =
  case stripPrefix "(" typeText of
    Just remaining ->
      case span (/= ')') remaining of
        (functionType, ')' : rest) ->
          case words functionType of
            [inputType, "->", outputType]
              | inputType == outputType -> rest == " -> " ++ inputType ++ " -> " ++ inputType
            _ -> False
        _ -> False
    Nothing -> False

removeImplicitForall :: String -> String
removeImplicitForall typeText =
  case stripPrefix "forall {" typeText of
    Just afterForall ->
      case dropUntilPeriod afterForall of
        Just remaining -> remaining
        Nothing -> typeText
    Nothing -> typeText

dropUntilPeriod :: String -> Maybe String
dropUntilPeriod text =
  case break (== '.') text of
    (_, '.' : ' ' : remaining) -> Just remaining
    _ -> Nothing

quoted :: String -> String
quoted text = "\"" ++ text ++ "\""
