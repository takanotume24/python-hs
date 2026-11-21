module PythonHS.VM.CallIterableBuiltin (callIterableBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallIterableBuiltinConfig (CallIterableBuiltinConfig (..))
import PythonHS.VM.IterableItems (iterableItems)

callIterableBuiltin :: CallIterableBuiltinConfig -> Maybe (Either String Value)
callIterableBuiltin config =
  case name of
    "enumerate" -> Just (evalEnumerate args)
    "zip" -> Just (evalZip args)
    _ -> Nothing
  where
    name = callIterableBuiltinName config
    args = callIterableBuiltinArgs config
    pos = callIterableBuiltinPos config

    evalEnumerate values =
      case values of
        [value] -> enumerateFrom 0 value
        [value, IntValue {intValue = start}] -> enumerateFrom start value
        [_, _] -> Left ("Type error: enumerate start must be int at " ++ showPos pos)
        _ -> Left (countMismatch "enumerate")

    enumerateFrom start value =
      case iterableItems value of
        Nothing -> Left ("Type error: enumerate expects iterable at " ++ showPos pos)
        Just items ->
          Right
            ( ListValue
                { listValueItems =
                    [ TupleValue {tupleValueItems = [IntValue {intValue = start + offset}, item]}
                    | (offset, item) <- zip [0 ..] items
                    ]
                }
            )

    evalZip values =
      case mapM iterableItems values of
        Nothing -> Left ("Type error: zip expects iterable at " ++ showPos pos)
        Just itemLists ->
          Right
            ( ListValue
                { listValueItems = map tupleRow (shortestRows itemLists)
                }
            )

    tupleRow row = TupleValue {tupleValueItems = row}

    shortestRows [] = []
    shortestRows itemLists =
      case mapM headOrNothing itemLists of
        Nothing -> []
        Just items -> items : shortestRows (map tailOrEmpty itemLists)

    headOrNothing [] = Nothing
    headOrNothing (item : _) = Just item

    tailOrEmpty [] = []
    tailOrEmpty (_ : restItems) = restItems

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
