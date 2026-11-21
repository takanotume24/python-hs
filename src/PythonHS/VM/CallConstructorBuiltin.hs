module PythonHS.VM.CallConstructorBuiltin (callConstructorBuiltin) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallConstructorBuiltinConfig (CallConstructorBuiltinConfig (..))

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
        [value] -> fmap (\items -> ListValue {listValueItems = items}) (iterableItems fname value)
        _ -> Left (countMismatch fname)

    evalTuple fname values =
      case values of
        [] -> Right (TupleValue {tupleValueItems = []})
        [value] -> fmap (\items -> TupleValue {tupleValueItems = items}) (iterableItems fname value)
        _ -> Left (countMismatch fname)

    iterableItems fname value =
      case value of
        ListValue {listValueItems = items} -> Right items
        TupleValue {tupleValueItems = items} -> Right items
        StringValue {stringValue = text} -> Right (map charValue text)
        DictValue {dictValuePairs = pairs} -> Right (map dictPairKey pairs)
        _ -> Left ("Type error: " ++ fname ++ " expects iterable at " ++ showPos pos)

    charValue character = StringValue {stringValue = [character]}

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
