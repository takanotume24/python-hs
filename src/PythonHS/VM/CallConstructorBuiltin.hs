module PythonHS.VM.CallConstructorBuiltin (callConstructorBuiltin) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallConstructorBuiltinConfig (CallConstructorBuiltinConfig (..))
import PythonHS.VM.IterableItems (iterableItems)

callConstructorBuiltin :: CallConstructorBuiltinConfig -> Maybe (Either String Value)
callConstructorBuiltin config =
  case name of
    "list" -> Just (evalList name args)
    "tuple" -> Just (evalTuple name args)
    "dict" -> Just (evalDict name args)
    _ -> Nothing
  where
    name = callConstructorBuiltinName config
    args = callConstructorBuiltinArgs config
    pos = callConstructorBuiltinPos config

    evalList fname values =
      case values of
        [] -> Right (ListValue {listValueItems = []})
        [value] -> toList fname value
        _ -> Left (countMismatch fname)

    toList fname value =
      case iterableItems value of
        Nothing -> Left ("Type error: " ++ fname ++ " expects iterable at " ++ showPos pos)
        Just items -> Right (ListValue {listValueItems = items})

    evalTuple fname values =
      case values of
        [] -> Right (TupleValue {tupleValueItems = []})
        [value] -> toTuple fname value
        _ -> Left (countMismatch fname)

    toTuple fname value =
      case iterableItems value of
        Nothing -> Left ("Type error: " ++ fname ++ " expects iterable at " ++ showPos pos)
        Just items -> Right (TupleValue {tupleValueItems = items})

    evalDict fname values =
      case values of
        [] -> Right (DictValue {dictValuePairs = []})
        [DictValue {dictValuePairs = pairs}] -> Right (DictValue {dictValuePairs = pairs})
        [ListValue {listValueItems = items}] -> buildDict items
        [TupleValue {tupleValueItems = items}] -> buildDict items
        [_] -> Left ("Type error: dict expects dict or list of pairs at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    buildDict items =
      fmap (\pairs -> DictValue {dictValuePairs = pairs}) (collectPairs [] items)

    collectPairs pairs [] = Right pairs
    collectPairs pairs (item : restItems) =
      case pairFromItem item of
        Left message -> Left message
        Right pair -> collectPairs (updateDictValue pairs (dictPairKey pair) (dictPairValue pair)) restItems

    pairFromItem value =
      case value of
        ListValue {listValueItems = items} -> pairFromItems items
        TupleValue {tupleValueItems = items} -> pairFromItems items
        _ -> Left notPairError
      where
        pairFromItems items =
          case items of
            [keyValue, valueValue] ->
              Right (DictPair {dictPairKey = keyValue, dictPairValue = valueValue})
            _ -> Left (lengthError items)

    lengthError items =
      "Value error: dict update sequence element has length "
        ++ show (length items)
        ++ "; 2 is required at "
        ++ showPos pos

    notPairError = "Type error: dict update sequence element is not a pair at " ++ showPos pos

    updateDictValue [] key value = [DictPair {dictPairKey = key, dictPairValue = value}]
    updateDictValue (entry : restPairs) key value
      | dictPairKey entry == key = DictPair {dictPairKey = key, dictPairValue = value} : restPairs
      | otherwise = entry : updateDictValue restPairs key value

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
