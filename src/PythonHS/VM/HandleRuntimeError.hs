module PythonHS.VM.HandleRuntimeError (handleRuntimeError) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value (StringValue), Value)
import PythonHS.VM.Instruction (Instruction)

handleRuntimeError ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  [Instruction] ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  Set.Set String ->
  Map.Map Int [Value] ->
  Map.Map Int Int ->
  [Int] ->
  [String] ->
  Bool ->
  Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String]) ->
  Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
handleRuntimeError execute code stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel result =
  case result of
    Right value -> Right value
    Left err ->
      case exceptionHandlers of
        handlerIp : restHandlers ->
          if isFinallyHandler handlerIp
            then
              let finalIp = decodeFinallyHandler handlerIp
                  newLocals = Map.insert pendingErrorName (StringValue err) localEnv
               in execute code finalIp stack globalsEnv newLocals functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
            else
              let newLocals = Map.delete pendingErrorName localEnv
               in execute code handlerIp stack globalsEnv newLocals functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
        [] -> Left err
  where
    pendingErrorName = "__python_hs_pending_finally_error__"
    decodeFinallyHandler encoded = negate encoded - 1
    isFinallyHandler handlerIp = handlerIp < 0
