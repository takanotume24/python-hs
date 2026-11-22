module PythonHS.VM.CallStringPredicateBuiltin (callStringPredicateBuiltin) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallStringBuiltinConfig (CallStringBuiltinConfig (..))

-- | Character-class predicates for string method calls.
callStringPredicateBuiltin :: CallStringBuiltinConfig -> Maybe (Either String Value)
callStringPredicateBuiltin config =
  case name of
    "isalnum" -> Just (evalPredicate (not . null) (all isAlphaNum) args)
    "isalpha" -> Just (evalPredicate (not . null) (all isAlpha) args)
    "isdigit" -> Just (evalPredicate (not . null) (all isDigit) args)
    "isdecimal" -> Just (evalPredicate (not . null) (all isDigit) args)
    "isnumeric" -> Just (evalPredicate (not . null) (all isDigit) args)
    "isspace" -> Just (evalPredicate (not . null) (all isSpace) args)
    "islower" -> Just (evalPredicate hasLower isLowerFree args)
    "isupper" -> Just (evalPredicate hasUpper isUpperFree args)
    "istitle" -> Just (evalPredicate (not . null) isTitleCase args)
    _ -> Nothing
  where
    name = callStringBuiltinName config
    args = callStringBuiltinArgs config
    pos = callStringBuiltinPos config

    evalPredicate requires predicate values =
      case values of
        [StringValue {stringValue = text}]
          | requires text && predicate text -> Right (IntValue {intValue = 1})
          | otherwise -> Right (IntValue {intValue = 0})
        [_] -> Left ("Type error: " ++ name ++ " expects str at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling " ++ name ++ " at " ++ showPos pos)

    hasLower text = any isLower text
    hasUpper text = any isUpper text

    isLowerFree text = not (any isUpper text)
    isUpperFree text = not (any isLower text)

    isTitleCase text =
      all titleWord (splitWords text)

    splitWords text =
      case dropWhile (not . isAlpha) text of
        [] -> []
        rest -> word : splitWords remainder
          where
            (word, remainder) = span isAlpha rest

    titleWord word =
      case word of
        [] -> True
        firstChar : restChars -> isUpper firstChar && all (not . isUpper) restChars
