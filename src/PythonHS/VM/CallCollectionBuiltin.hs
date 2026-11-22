module PythonHS.VM.CallCollectionBuiltin (callCollectionBuiltin) where

import Data.List (sort, sortOn)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.NumberSortPair (NumberSortPair (..))
import PythonHS.Evaluator.PairToList (pairToList)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.VM.CallCollectionBuiltinConfig (CallCollectionBuiltinConfig (..))
import PythonHS.VM.DelitemValue (delitemValue)
import PythonHS.VM.DelitemValueConfig (DelitemValueConfig (..))
import PythonHS.VM.DelsliceValue (delsliceValue)
import PythonHS.VM.DelsliceValueConfig (DelsliceValueConfig (..))
import PythonHS.VM.GetitemValue (getitemValue)
import PythonHS.VM.GetitemValueConfig (GetitemValueConfig (..))
import PythonHS.VM.NumberPairs (numberPairs)
import PythonHS.VM.SetitemValue (setitemValue)
import PythonHS.VM.SetitemValueConfig (SetitemValueConfig (..))
import PythonHS.VM.SetsliceValue (setsliceValue)
import PythonHS.VM.SetsliceValueConfig (SetsliceValueConfig (..))
import PythonHS.VM.SliceValue (sliceValue)
import PythonHS.VM.SliceValueConfig (SliceValueConfig (..))
import PythonHS.VM.StringItems (stringItems)

callCollectionBuiltin :: CallCollectionBuiltinConfig -> Maybe (Either String Value)
callCollectionBuiltin config =
  let name = callCollectionBuiltinName config
      args = callCollectionBuiltinArgs config
      pos = callCollectionBuiltinPos config
   in case name of
        "len" -> Just $ case args of
          [StringValue {stringValue = s}] -> Right (IntValue {intValue = fromIntegral (length s)})
          [ListValue {listValueItems = vals}] -> Right (IntValue {intValue = fromIntegral (length vals)})
          [TupleValue {tupleValueItems = vals}] -> Right (IntValue {intValue = fromIntegral (length vals)})
          [_] -> Left ("Type error: len expects string or list at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling len at " ++ showPos pos)
        "bool" -> Just $ case args of
          [IntValue {intValue = n}] -> Right (IntValue {intValue = if n == 0 then 0 else 1})
          [FloatValue {floatValue = n}] -> Right (IntValue {intValue = if n == 0 then 0 else 1})
          [NoneValue] -> Right (IntValue {intValue = 0})
          [StringValue {stringValue = s}] -> Right (IntValue {intValue = if null s then 0 else 1})
          [ListValue {listValueItems = vals}] -> Right (IntValue {intValue = if null vals then 0 else 1})
          [TupleValue {tupleValueItems = vals}] -> Right (IntValue {intValue = if null vals then 0 else 1})
          [DictValue {dictValuePairs = pairs}] -> Right (IntValue {intValue = if null pairs then 0 else 1})
          _ -> Left ("Argument count mismatch when calling bool at " ++ showPos pos)
        "__python_hs_getitem__" -> Just $ case args of
          [seqValue, indexValue] -> getitemValue GetitemValueConfig {getitemValuePos = pos, getitemValueSeqValue = seqValue, getitemValueIndexValue = indexValue}
          _ -> Left ("Argument count mismatch when calling __python_hs_getitem__ at " ++ showPos pos)
        "__python_hs_setitem__" -> Just $ case args of
          [seqValue, indexValue, newValue] -> setitemValue SetitemValueConfig {setitemValuePos = pos, setitemValueSeqValue = seqValue, setitemValueIndexValue = indexValue, setitemValueNewValue = newValue}
          _ -> Left ("Argument count mismatch when calling __python_hs_setitem__ at " ++ showPos pos)
        "__python_hs_setslice__" -> Just $ case args of
          [seqValue, startVal, endVal, newItems] -> setsliceValue SetsliceValueConfig {setsliceValuePos = pos, setsliceValueSeqValue = seqValue, setsliceValueStartVal = startVal, setsliceValueEndVal = endVal, setsliceValueNewItems = newItems}
          _ -> Left ("Argument count mismatch when calling __python_hs_setslice__ at " ++ showPos pos)
        "__python_hs_delitem__" -> Just $ case args of
          [seqValue, indexValue] -> delitemValue DelitemValueConfig {delitemValuePos = pos, delitemValueSeqValue = seqValue, delitemValueIndexValue = indexValue}
          _ -> Left ("Argument count mismatch when calling __python_hs_delitem__ at " ++ showPos pos)
        "__python_hs_delslice__" -> Just $ case args of
          [seqValue, startVal, endVal] -> delsliceValue DelsliceValueConfig {delsliceValuePos = pos, delsliceValueSeqValue = seqValue, delsliceValueStartVal = startVal, delsliceValueEndVal = endVal}
          _ -> Left ("Argument count mismatch when calling __python_hs_delslice__ at " ++ showPos pos)
        "__python_hs_slice__" -> Just $ case args of
          [seqValue, startVal, endVal] -> sliceValue SliceValueConfig {sliceValuePos = pos, sliceValueSeqValue = seqValue, sliceValueStartVal = startVal, sliceValueEndVal = endVal}
          _ -> Left ("Argument count mismatch when calling __python_hs_slice__ at " ++ showPos pos)
        "append" -> Just $ case args of
          [ListValue {listValueItems = vals}, value] -> Right (ListValue {listValueItems = vals ++ [value]})
          [_, _] -> Left ("Type error: append expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling append at " ++ showPos pos)
        "extend" -> Just $ case args of
          [ListValue {listValueItems = vals}, ListValue {listValueItems = otherVals}] -> Right (ListValue {listValueItems = vals ++ otherVals})
          [ListValue {listValueItems = vals}, IntValue {intValue = n}] -> Right (ListValue {listValueItems = vals ++ [IntValue {intValue = i} | i <- [0 .. n - 1]]})
          [ListValue {listValueItems = vals}, DictValue {dictValuePairs = pairs}] -> Right (ListValue {listValueItems = vals ++ map dictPairKey pairs})
          [ListValue {listValueItems = _}, _] -> Left ("Type error: extend expects iterable as second argument at " ++ showPos pos)
          [_, _] -> Left ("Type error: extend expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling extend at " ++ showPos pos)
        "sort" -> Just $ case args of
          [ListValue {listValueItems = vals}] -> sortValues pos vals
          [_] -> Left ("Type error: sort expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling sort at " ++ showPos pos)
        "reverse" -> Just $ case args of
          [ListValue {listValueItems = vals}] -> Right (ListValue {listValueItems = reverse vals})
          [_] -> Left ("Type error: reverse expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling reverse at " ++ showPos pos)
        "remove" -> Just $ case args of
          [ListValue {listValueItems = vals}, target] ->
            case removeFirstValue vals target of
              Just newVals -> Right (ListValue {listValueItems = newVals})
              Nothing -> Left ("Value error: remove value not found at " ++ showPos pos)
          [_, _] -> Left ("Type error: remove expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling remove at " ++ showPos pos)
        "insert" -> Just $ case args of
          [ListValue {listValueItems = vals}, IntValue {intValue = index}, value] -> Right (ListValue {listValueItems = insertAtIndex vals index value})
          [ListValue {listValueItems = _}, _, _] -> Left ("Type error: insert expects int index at " ++ showPos pos)
          [_, _, _] -> Left ("Type error: insert expects list as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling insert at " ++ showPos pos)
        "pop" -> Just $ case args of
          [ListValue {listValueItems = []}] -> Left ("Value error: pop from empty list at " ++ showPos pos)
          [ListValue {listValueItems = vals}] -> Right (last vals)
          [DictValue {dictValuePairs = pairs}, key] ->
            case lookupDictValue pairs key of
              Just value -> Right value
              Nothing -> Left ("Key not found in pop at " ++ showPos pos)
          [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right value
              Nothing -> Right defaultValue
          [_] -> Left ("Type error: pop expects list at " ++ showPos pos)
          [ListValue {listValueItems = _}, _] -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
          [ListValue {listValueItems = _}, _, _] -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
          [_, _] -> Left ("Type error: pop expects dict as first argument at " ++ showPos pos)
          [_, _, _] -> Left ("Type error: pop expects dict as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling pop at " ++ showPos pos)
        "clear" -> Just $ case args of
          [ListValue {listValueItems = _}] -> Right (ListValue {listValueItems = []})
          [DictValue {dictValuePairs = _}] -> Right (DictValue {dictValuePairs = []})
          [_] -> Left ("Type error: clear expects list or dict at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling clear at " ++ showPos pos)
        "keys" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}] -> Right (ListValue {listValueItems = map dictPairKey pairs})
          [_] -> Left ("Type error: keys expects dict at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling keys at " ++ showPos pos)
        "get" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}, key] ->
            case lookupDictValue pairs key of
              Just value -> Right value
              Nothing -> Left ("Key not found in get at " ++ showPos pos)
          [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
            case lookupDictValue pairs key of
              Just value -> Right value
              Nothing -> Right defaultValue
          [_, _] -> Left ("Type error: get expects dict as first argument at " ++ showPos pos)
          [_, _, _] -> Left ("Type error: get expects dict as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling get at " ++ showPos pos)
        "update" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}, DictValue {dictValuePairs = otherPairs}] -> Right (DictValue {dictValuePairs = mergeDictValues pairs otherPairs})
          [DictValue {dictValuePairs = _}, _] -> Left ("Type error: update expects dict as second argument at " ++ showPos pos)
          [DictValue {dictValuePairs = pairs}, key, value] -> Right (DictValue {dictValuePairs = updateDictValue pairs key value})
          [_, _, _] -> Left ("Type error: update expects dict as first argument at " ++ showPos pos)
          [_, _] -> Left ("Type error: update expects dict as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling update at " ++ showPos pos)
        "setdefault" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}, key] -> Right (DictValue {dictValuePairs = setDefaultDictValue pairs key NoneValue})
          [DictValue {dictValuePairs = pairs}, key, defaultValue] -> Right (DictValue {dictValuePairs = setDefaultDictValue pairs key defaultValue})
          [_, _] -> Left ("Type error: setdefault expects dict as first argument at " ++ showPos pos)
          [_, _, _] -> Left ("Type error: setdefault expects dict as first argument at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling setdefault at " ++ showPos pos)
        "values" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}] -> Right (ListValue {listValueItems = map dictPairValue pairs})
          [_] -> Left ("Type error: values expects dict at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling values at " ++ showPos pos)
        "items" -> Just $ case args of
          [DictValue {dictValuePairs = pairs}] -> Right (ListValue {listValueItems = map pairToList pairs})
          [_] -> Left ("Type error: items expects dict at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling items at " ++ showPos pos)
        "__python_hs_repl_repr__" -> Just $ case args of
          [value] -> Right (StringValue {stringValue = valueToReplOutput value})
          _ -> Left ("Argument count mismatch when calling __python_hs_repl_repr__ at " ++ showPos pos)
        _ -> Nothing
  where
    sortValues pos vals =
      case numberPairs vals of
        Just pairs -> Right (ListValue {listValueItems = map numberSortPairValue (sortOn numberSortPairSortKey pairs)})
        Nothing ->
          case stringItems vals of
            Just texts -> Right (ListValue {listValueItems = map textValue (sort texts)})
            Nothing -> Left ("Type error: sort expects list of number or string at " ++ showPos pos)

    textValue text = StringValue {stringValue = text}

    lookupDictValue [] _ = Nothing
    lookupDictValue (entry : restPairs) target
      | dictPairKey entry == target = Just (dictPairValue entry)
      | otherwise = lookupDictValue restPairs target

    removeFirstValue [] _ = Nothing
    removeFirstValue (v : restVals) target
      | v == target = Just restVals
      | otherwise = fmap (v :) (removeFirstValue restVals target)

    insertAtIndex values index value =
      let maxIndex = fromIntegral (length values)
          clampedIndex = max 0 (min index maxIndex)
          splitIndex = fromIntegral clampedIndex
          leftValues = take splitIndex values
          rightValues = drop splitIndex values
       in leftValues ++ (value : rightValues)

    updateDictValue [] key value = [DictPair {dictPairKey = key, dictPairValue = value}]
    updateDictValue (entry : restPairs) key value
      | dictPairKey entry == key = DictPair {dictPairKey = key, dictPairValue = value} : restPairs
      | otherwise = entry : updateDictValue restPairs key value

    mergeDictValues pairs [] = pairs
    mergeDictValues pairs (entry : restPairs) =
      mergeDictValues (updateDictValue pairs (dictPairKey entry) (dictPairValue entry)) restPairs

    setDefaultDictValue pairs key defaultValue =
      case lookupDictValue pairs key of
        Just _ -> pairs
        Nothing -> pairs ++ [DictPair {dictPairKey = key, dictPairValue = defaultValue}]
