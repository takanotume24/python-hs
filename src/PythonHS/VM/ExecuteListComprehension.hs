module PythonHS.VM.ExecuteListComprehension (executeListComprehension) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value (ListValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.ToForIterable (toForIterable)

executeListComprehension ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  String ->
  [Instruction] ->
  [Instruction] ->
  Position ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String (Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeListComprehension execute loopName iterCode valueCode pos globalsEnv localEnv functions outputs = do
  (maybeIter, globalsAfterIter, functionsAfterIter, outputsAfterIter) <-
    execute iterCode 0 [] globalsEnv localEnv functions Set.empty Map.empty Map.empty [] outputs False
  iterValue <- requireValue maybeIter
  iterItems <- toForIterable iterValue pos
  (items, globalsAfterItems, functionsAfterItems, outputsAfterItems) <-
    evalItems iterItems [] globalsAfterIter functionsAfterIter outputsAfterIter
  Right (ListValue items, globalsAfterItems, functionsAfterItems, outputsAfterItems)
  where
    requireValue (Just value) = Right value
    requireValue Nothing = Left "VM runtime error: list comprehension iterable did not produce value"

    evalItems [] acc globalsNow functionsNow outputsNow =
      Right (acc, globalsNow, functionsNow, outputsNow)
    evalItems (item : restItems) acc globalsNow functionsNow outputsNow = do
      let compLocals = Map.insert loopName item localEnv
      (maybeValue, globalsAfterValue, functionsAfterValue, outputsAfterValue) <-
        execute valueCode 0 [] globalsNow compLocals functionsNow Set.empty Map.empty Map.empty [] outputsNow False
      value <- requireElement maybeValue
      evalItems restItems (acc ++ [value]) globalsAfterValue functionsAfterValue outputsAfterValue

    requireElement (Just value) = Right value
    requireElement Nothing = Left "VM runtime error: list comprehension value did not produce value"
