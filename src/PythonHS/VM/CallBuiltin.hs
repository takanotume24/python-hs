module PythonHS.VM.CallBuiltin (callBuiltin) where

import Data.List (sortOn)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.Lexer.Position (Position)

callBuiltin :: String -> [Value] -> Position -> Maybe (Either String Value)
callBuiltin name args pos =
  case name of
    "len" -> Just $ case args of
      [StringValue s] -> Right (IntValue (length s))
      [ListValue vals] -> Right (IntValue (length vals))
      [_] -> Left ("Type error: len expects string or list at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling len at " ++ showPos pos)
    "bool" -> Just $ case args of
      [IntValue n] -> Right (IntValue (if n == 0 then 0 else 1))
      [FloatValue n] -> Right (IntValue (if n == 0 then 0 else 1))
      [NoneValue] -> Right (IntValue 0)
      [StringValue s] -> Right (IntValue (if null s then 0 else 1))
      [ListValue vals] -> Right (IntValue (if null vals then 0 else 1))
      [DictValue pairs] -> Right (IntValue (if null pairs then 0 else 1))
      _ -> Left ("Argument count mismatch when calling bool at " ++ showPos pos)
    "range" -> Just $ case args of
      [IntValue n] -> Right (ListValue (map IntValue (rangeOne n)))
      [IntValue start, IntValue stop] -> Right (ListValue (map IntValue (rangeWithStep start stop 1)))
      [IntValue start, IntValue stop, IntValue step] ->
        if step == 0
          then Left ("Value error: range step must not be zero at " ++ showPos pos)
          else Right (ListValue (map IntValue (rangeWithStep start stop step)))
      [_] -> Left ("Type error: range expects int at " ++ showPos pos)
      [_, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling range at " ++ showPos pos)
    "append" -> Just $ case args of
      [ListValue vals, value] -> Right (ListValue (vals ++ [value]))
      [_, _] -> Left ("Type error: append expects list as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling append at " ++ showPos pos)
    "sort" -> Just $ case args of
      [ListValue vals] ->
        case numberPairs vals of
          Just pairs -> Right (ListValue (map snd (sortOn fst pairs)))
          Nothing -> Left ("Type error: sort expects list of number at " ++ showPos pos)
      [_] -> Left ("Type error: sort expects list as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling sort at " ++ showPos pos)
    "reverse" -> Just $ case args of
      [ListValue vals] -> Right (ListValue (reverse vals))
      [_] -> Left ("Type error: reverse expects list as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling reverse at " ++ showPos pos)
    "remove" -> Just $ case args of
      [ListValue vals, target] ->
        case removeFirstValue vals target of
          Just newVals -> Right (ListValue newVals)
          Nothing -> Left ("Value error: remove value not found at " ++ showPos pos)
      [_, _] -> Left ("Type error: remove expects list as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling remove at " ++ showPos pos)
    "insert" -> Just $ case args of
      [ListValue vals, IntValue index, value] -> Right (ListValue (insertAtIndex vals index value))
      [ListValue _, _, _] -> Left ("Type error: insert expects int index at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: insert expects list as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling insert at " ++ showPos pos)
    "pop" -> Just $ case args of
      [ListValue []] -> Left ("Value error: pop from empty list at " ++ showPos pos)
      [ListValue vals] -> Right (last vals)
      [DictValue pairs, key] ->
        case lookupDictValue pairs key of
          Just value -> Right value
          Nothing -> Left ("Key not found in pop at " ++ showPos pos)
      [DictValue pairs, key, defaultValue] ->
        case lookupDictValue pairs key of
          Just value -> Right value
          Nothing -> Right defaultValue
      [_] -> Left ("Type error: pop expects list at " ++ showPos pos)
      [ListValue _, _] -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
      [ListValue _, _, _] -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
      [_, _] -> Left ("Type error: pop expects dict as first argument at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: pop expects dict as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
    "clear" -> Just $ case args of
      [ListValue _] -> Right (ListValue [])
      [DictValue _] -> Right (DictValue [])
      [_] -> Left ("Type error: clear expects list or dict at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling clear at " ++ showPos pos)
    "keys" -> Just $ case args of
      [DictValue pairs] -> Right (ListValue (map fst pairs))
      [_] -> Left ("Type error: keys expects dict at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling keys at " ++ showPos pos)
    "get" -> Just $ case args of
      [DictValue pairs, key] ->
        case lookupDictValue pairs key of
          Just value -> Right value
          Nothing -> Left ("Key not found in get at " ++ showPos pos)
      [DictValue pairs, key, defaultValue] ->
        case lookupDictValue pairs key of
          Just value -> Right value
          Nothing -> Right defaultValue
      [_, _] -> Left ("Type error: get expects dict as first argument at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: get expects dict as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling get at " ++ showPos pos)
    "update" -> Just $ case args of
      [DictValue pairs, DictValue otherPairs] -> Right (DictValue (mergeDictValues pairs otherPairs))
      [DictValue _, _] -> Left ("Type error: update expects dict as second argument at " ++ showPos pos)
      [DictValue pairs, key, value] -> Right (DictValue (updateDictValue pairs key value))
      [_, _, _] -> Left ("Type error: update expects dict as first argument at " ++ showPos pos)
      [_, _] -> Left ("Type error: update expects dict as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling update at " ++ showPos pos)
    "setdefault" -> Just $ case args of
      [DictValue pairs, key] -> Right (DictValue (setDefaultDictValue pairs key NoneValue))
      [DictValue pairs, key, defaultValue] -> Right (DictValue (setDefaultDictValue pairs key defaultValue))
      [_, _] -> Left ("Type error: setdefault expects dict as first argument at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: setdefault expects dict as first argument at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling setdefault at " ++ showPos pos)
    "values" -> Just $ case args of
      [DictValue pairs] -> Right (ListValue (map snd pairs))
      [_] -> Left ("Type error: values expects dict at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling values at " ++ showPos pos)
    "items" -> Just $ case args of
      [DictValue pairs] -> Right (ListValue (map pairToList pairs))
      [_] -> Left ("Type error: items expects dict at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling items at " ++ showPos pos)
    "__python_hs_repl_repr__" -> Just $ case args of
      [value] -> Right (StringValue (valueToReplOutput value))
      _ -> Left ("Argument count mismatch when calling __python_hs_repl_repr__ at " ++ showPos pos)
    _ -> Nothing
  where
    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]

    rangeWithStep start stop step
      | step > 0 = takeWhile (< stop) [start, start + step ..]
      | step < 0 = takeWhile (> stop) [start, start + step ..]
      | otherwise = []

    numberPairs [] = Just []
    numberPairs (IntValue n : restVals) = fmap ((fromIntegral n, IntValue n) :) (numberPairs restVals)
    numberPairs (FloatValue n : restVals) = fmap ((n, FloatValue n) :) (numberPairs restVals)
    numberPairs (_ : _) = Nothing

    lookupDictValue [] _ = Nothing
    lookupDictValue ((k, v) : restPairs) target
      | k == target = Just v
      | otherwise = lookupDictValue restPairs target

    removeFirstValue [] _ = Nothing
    removeFirstValue (v : restVals) target
      | v == target = Just restVals
      | otherwise = fmap (v :) (removeFirstValue restVals target)

    insertAtIndex values index value =
      let clampedIndex = max 0 (min index (length values))
          (leftValues, rightValues) = splitAt clampedIndex values
       in leftValues ++ (value : rightValues)

    updateDictValue [] key value = [(key, value)]
    updateDictValue ((k, v) : restPairs) key value
      | k == key = (k, value) : restPairs
      | otherwise = (k, v) : updateDictValue restPairs key value

    mergeDictValues pairs [] = pairs
    mergeDictValues pairs ((key, value) : restPairs) =
      mergeDictValues (updateDictValue pairs key value) restPairs

    setDefaultDictValue pairs key defaultValue =
      case lookupDictValue pairs key of
        Just _ -> pairs
        Nothing -> pairs ++ [(key, defaultValue)]

    pairToList (k, v) = ListValue [k, v]
