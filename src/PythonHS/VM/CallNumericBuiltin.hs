module PythonHS.VM.CallNumericBuiltin (callNumericBuiltin) where

import Control.Monad (foldM)
import Data.List (sortOn)
import PythonHS.Evaluator.NumberSortPair (NumberSortPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallNumericBuiltinConfig (CallNumericBuiltinConfig (..))
import PythonHS.VM.NumberPairs (numberPairs)

callNumericBuiltin :: CallNumericBuiltinConfig -> Maybe (Either String Value)
callNumericBuiltin config =
  case name of
    "abs" -> Just (evalAbs name args)
    "min" -> Just (evalMinMax name True args)
    "max" -> Just (evalMinMax name False args)
    "sum" -> Just (evalSum name args)
    "sorted" -> Just (evalSorted name args)
    _ -> Nothing
  where
    name = callNumericBuiltinName config
    args = callNumericBuiltinArgs config
    pos = callNumericBuiltinPos config

    evalAbs fname values =
      case values of
        [IntValue {intValue = n}] -> Right (IntValue {intValue = abs n})
        [FloatValue {floatValue = n}] -> Right (FloatValue {floatValue = abs n})
        [_] -> Left ("Type error: " ++ fname ++ " expects int or float at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    evalMinMax fname isMin values =
      case values of
        [] -> Left (countMismatch fname)
        [ListValue {listValueItems = items}] -> pickFrom fname isMin items
        [TupleValue {tupleValueItems = items}] -> pickFrom fname isMin items
        [_] -> Left ("Type error: " ++ fname ++ " expects list or tuple at " ++ showPos pos)
        _ -> pickFrom fname isMin values

    pickFrom fname isMin items =
      case items of
        [] -> Left ("Value error: " ++ fname ++ "() arg is an empty sequence at " ++ showPos pos)
        _ ->
          case numberPairs items of
            Just pairs -> Right (numberSortPairValue (foldr1 (chooseNumber isMin) pairs))
            Nothing ->
              case collectStrings items of
                Just texts -> Right (StringValue {stringValue = foldr1 (chooseString isMin) texts})
                Nothing -> Left ("Type error: " ++ fname ++ " expects int, float, or string at " ++ showPos pos)

    chooseNumber isMin left right
      | isMin = if numberSortPairSortKey left <= numberSortPairSortKey right then left else right
      | otherwise = if numberSortPairSortKey left >= numberSortPairSortKey right then left else right

    chooseString isMin left right
      | isMin = if left <= right then left else right
      | otherwise = if left >= right then left else right

    collectStrings [] = Just []
    collectStrings (StringValue {stringValue = text} : restTexts) = fmap (text :) (collectStrings restTexts)
    collectStrings (_ : _) = Nothing

    evalSum fname values =
      case values of
        [ListValue {listValueItems = items}] -> sumItems fname items (IntValue {intValue = 0})
        [TupleValue {tupleValueItems = items}] -> sumItems fname items (IntValue {intValue = 0})
        [ListValue {listValueItems = items}, startValue] -> sumItems fname items startValue
        [TupleValue {tupleValueItems = items}, startValue] -> sumItems fname items startValue
        [_, _] -> Left ("Type error: " ++ fname ++ " expects list or tuple at " ++ showPos pos)
        [_] -> Left ("Type error: " ++ fname ++ " expects list or tuple at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    sumItems fname items startValue =
      case numberValue startValue of
        Nothing -> Left ("Type error: " ++ fname ++ " expects number as second argument at " ++ showPos pos)
        Just _ -> foldM (addValue fname) startValue items

    addValue _ (IntValue {intValue = acc}) (IntValue {intValue = n}) = Right (IntValue {intValue = acc + n})
    addValue fname acc value =
      case (numberValue acc, numberValue value) of
        (Just accNumber, Just number) -> Right (FloatValue {floatValue = accNumber + number})
        _ -> Left ("Type error: " ++ fname ++ " expects list of number at " ++ showPos pos)

    evalSorted fname values =
      case values of
        [ListValue {listValueItems = items}] -> sortItems fname items
        [TupleValue {tupleValueItems = items}] -> sortItems fname items
        [_] -> Left ("Type error: " ++ fname ++ " expects list or tuple at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    sortItems fname items =
      case numberPairs items of
        Nothing -> Left ("Type error: " ++ fname ++ " expects list of number at " ++ showPos pos)
        Just pairs -> Right (ListValue {listValueItems = map numberSortPairValue (sortOn numberSortPairSortKey pairs)})

    numberValue value =
      case value of
        IntValue {intValue = n} -> Just (fromIntegral n)
        FloatValue {floatValue = n} -> Just n
        _ -> Nothing

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
