module PythonHS.VM.CallCollectionMethodBuiltin (callCollectionMethodBuiltin) where

import Data.List (findIndex)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallCollectionMethodBuiltinConfig (CallCollectionMethodBuiltinConfig (..))
import PythonHS.VM.IterableItems (iterableItems)

callCollectionMethodBuiltin :: CallCollectionMethodBuiltinConfig -> Maybe (Either String Value)
callCollectionMethodBuiltin config =
  case name of
    "index" -> Just (evalIndex args)
    "copy" -> Just (evalCopy args)
    "fromkeys" -> Just (evalFromKeys args)
    "popitem" -> Just (evalPopItem args)
    _ -> Nothing
  where
    name = callCollectionMethodBuiltinName config
    args = callCollectionMethodBuiltinArgs config
    pos = callCollectionMethodBuiltinPos config

    evalIndex values =
      case values of
        [ListValue {listValueItems = items}, targetValue] ->
          case findIndex (== targetValue) items of
            Just index -> Right (IntValue {intValue = fromIntegral index})
            Nothing -> Left ("Value error: index value not found at " ++ showPos pos)
        [_, _] -> Left ("Type error: index expects list as first argument at " ++ showPos pos)
        _ -> Left (countMismatch "index")

    evalCopy values =
      case values of
        [ListValue {listValueItems = items}] -> Right (ListValue {listValueItems = items})
        [DictValue {dictValuePairs = pairs}] -> Right (DictValue {dictValuePairs = pairs})
        [_] -> Left ("Type error: copy expects list or dict at " ++ showPos pos)
        _ -> Left (countMismatch "copy")

    evalFromKeys values =
      case values of
        [seqValue] -> buildFromKeys seqValue NoneValue
        [seqValue, defaultValue] -> buildFromKeys seqValue defaultValue
        _ -> Left (countMismatch "fromkeys")

    buildFromKeys seqValue defaultValue =
      case iterableItems seqValue of
        Nothing -> Left ("Type error: fromkeys expects iterable at " ++ showPos pos)
        Just keys -> Right (DictValue {dictValuePairs = foldl insertKey [] keys})
      where
        insertKey pairs key = updateDictValue pairs key defaultValue

    evalPopItem values =
      case values of
        [DictValue {dictValuePairs = pairs}] ->
          case reverse pairs of
            [] -> Left ("Value error: popitem from empty dict at " ++ showPos pos)
            (lastPair : _) ->
              Right (TupleValue {tupleValueItems = [dictPairKey lastPair, dictPairValue lastPair]})
        [_] -> Left ("Type error: popitem expects dict at " ++ showPos pos)
        _ -> Left (countMismatch "popitem")

    updateDictValue [] key value = [DictPair {dictPairKey = key, dictPairValue = value}]
    updateDictValue (entry : restPairs) key value
      | dictPairKey entry == key = DictPair {dictPairKey = key, dictPairValue = value} : restPairs
      | otherwise = entry : updateDictValue restPairs key value

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
