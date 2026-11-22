module PythonHS.VM.CallSetBuiltin (callSetBuiltin) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.CallSetBuiltinConfig (CallSetBuiltinConfig (..))

callSetBuiltin :: CallSetBuiltinConfig -> Maybe (Either String Value)
callSetBuiltin config =
  case callSetBuiltinName config of
    "set" -> Just (constructorSet False)
    "frozenset" -> Just (constructorSet True)
    "add" -> setOne "add" $ \items frozen value -> Right (makeSet frozen (items ++ [value]))
    "discard" -> setOne "discard" $ \items frozen value -> Right (makeSet frozen (filter (not . valuesEqual value) items))
    "remove" -> setOne "remove" $ \items frozen value ->
      if any (valuesEqual value) items
        then Right (makeSet frozen (filter (not . valuesEqual value) items))
        else Left ("Value error: remove value not found at " ++ showPos pos)
    "clear" -> setNone "clear" $ \_ frozen -> Right (makeSet frozen [])
    "pop" -> setNone "pop" $ \items _ ->
      case items of
        [] -> Left ("Value error: pop from empty set at " ++ showPos pos)
        firstItem : _ -> Right firstItem
    "update" -> setOne "update" $ \items frozen value -> do
      extraItems <- iterableItems value
      Right (makeSet frozen (items ++ extraItems))
    "union" -> foldSet "union" (\items extra -> items ++ extra)
    "intersection" -> foldSet "intersection" filterIntersection
    "difference" -> foldSet "difference" filterDifference
    _ -> Nothing
  where
    name = callSetBuiltinName config
    args = callSetBuiltinArgs config
    pos = callSetBuiltinPos config

    constructorSet frozen =
      let constructorName = if frozen then "frozenset" else "set"
       in case args of
            [] -> Right (makeSet frozen [])
            [value] -> do
              items <- iterableItems value
              Right (makeSet frozen items)
            _ -> Left ("Argument count mismatch when calling " ++ constructorName ++ " at " ++ showPos pos)

    setOne expectedName apply =
      case args of
        SetValue {setValueItems = items, setValueIsFrozen = frozen} : rest ->
          Just $ case rest of
            [value] -> apply items frozen value
            _ -> Left ("Argument count mismatch when calling " ++ expectedName ++ " at " ++ showPos pos)
        _ -> Nothing

    setNone expectedName apply =
      case args of
        SetValue {setValueItems = items, setValueIsFrozen = frozen} : rest ->
          Just $ case rest of
            [] -> apply items frozen
            _ -> Left ("Argument count mismatch when calling " ++ expectedName ++ " at " ++ showPos pos)
        _ -> Nothing

    foldSet expectedName combine =
      case args of
        SetValue {setValueItems = items, setValueIsFrozen = frozen} : restArgs -> Just $ do
          extraItemLists <- mapM iterableItems restArgs
          Right (makeSet frozen (foldl combine items extraItemLists))
        _ -> Nothing

    filterIntersection items extra = filter (\item -> any (valuesEqual item) extra) items
    filterDifference items extra = filter (\item -> not (any (valuesEqual item) extra)) items

    iterableItems iterableValue =
      case iterableValue of
        ListValue {listValueItems = items} -> Right items
        TupleValue {tupleValueItems = items} -> Right items
        SetValue {setValueItems = items} -> Right items
        DictValue {dictValuePairs = pairs} -> Right (map dictPairKey pairs)
        StringValue {stringValue = text} -> Right (map charValue text)
        IntValue {intValue = upper} -> Right (map IntValue [0 .. max 0 upper - 1])
        _ -> Left ("Type error: " ++ name ++ " expects iterable at " ++ showPos pos)

    charValue character = StringValue {stringValue = [character]}

    makeSet frozen values =
      SetValue {setValueItems = foldl insertUnique [] values, setValueIsFrozen = frozen}

    insertUnique acc value =
      if any (valuesEqual value) acc then acc else acc ++ [value]
