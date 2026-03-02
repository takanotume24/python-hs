module PythonHS.VM.ExecuteListComprehension (executeListComprehension) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value (ListValue), Value)
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.ToForIterable (toForIterable)

executeListComprehension ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  [(String, [Instruction], Maybe [Instruction])] ->
  [Instruction] ->
  Position ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String (Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeListComprehension execute clauses valueCode pos globalsEnv localEnv functions outputs = do
  (items, globalsAfterItems, functionsAfterItems, outputsAfterItems) <-
    evalClauses clauses localEnv [] globalsEnv functions outputs
  Right (ListValue items, globalsAfterItems, functionsAfterItems, outputsAfterItems)
  where
    requireValue (Just value) = Right value
    requireValue Nothing = Left "VM runtime error: list comprehension iterable did not produce value"

    requireCondition (Just value) = Right value
    requireCondition Nothing = Left "VM runtime error: list comprehension condition did not produce value"

    requireElement (Just value) = Right value
    requireElement Nothing = Left "VM runtime error: list comprehension value did not produce value"

    evalClauses [] localsNow acc globalsNow functionsNow outputsNow = do
      (maybeValue, globalsAfterValue, functionsAfterValue, outputsAfterValue) <-
        execute valueCode 0 [] globalsNow localsNow functionsNow Set.empty Map.empty Map.empty [] outputsNow False
      value <- requireElement maybeValue
      Right (acc ++ [value], globalsAfterValue, functionsAfterValue, outputsAfterValue)
    evalClauses ((loopName, iterCode, maybeCondCode) : restClauses) localsNow acc globalsNow functionsNow outputsNow = do
      (maybeIter, globalsAfterIter, functionsAfterIter, outputsAfterIter) <-
        execute iterCode 0 [] globalsNow localsNow functionsNow Set.empty Map.empty Map.empty [] outputsNow False
      iterValue <- requireValue maybeIter
      iterItems <- toForIterable iterValue pos
      evalClauseItems loopName maybeCondCode restClauses localsNow iterItems acc globalsAfterIter functionsAfterIter outputsAfterIter

    evalClauseItems _ _ _ _ [] acc globalsNow functionsNow outputsNow =
      Right (acc, globalsNow, functionsNow, outputsNow)
    evalClauseItems loopName maybeCondCode restClauses localsNow (item : restItems) acc globalsNow functionsNow outputsNow = do
      let nextLocals = Map.insert loopName item localsNow
      (shouldInclude, globalsAfterCond, functionsAfterCond, outputsAfterCond) <-
        case maybeCondCode of
          Nothing -> Right (True, globalsNow, functionsNow, outputsNow)
          Just condCode -> do
            (maybeCond, globalsAfterCond, functionsAfterCond, outputsAfterCond) <-
              execute condCode 0 [] globalsNow nextLocals functionsNow Set.empty Map.empty Map.empty [] outputsNow False
            condValue <- requireCondition maybeCond
            Right (isTruthy condValue, globalsAfterCond, functionsAfterCond, outputsAfterCond)
      (accAfterItem, globalsAfterItem, functionsAfterItem, outputsAfterItem) <-
        if shouldInclude
          then evalClauses restClauses nextLocals acc globalsAfterCond functionsAfterCond outputsAfterCond
          else Right (acc, globalsAfterCond, functionsAfterCond, outputsAfterCond)
      evalClauseItems loopName maybeCondCode restClauses localsNow restItems accAfterItem globalsAfterItem functionsAfterItem outputsAfterItem
