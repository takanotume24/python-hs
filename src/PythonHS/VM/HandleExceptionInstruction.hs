module PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction)
import PythonHS.VM.Instruction (Instruction(..))

handleExceptionInstruction ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe a, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  [Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Instruction -> Either String (Maybe a, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction =
  case instruction of
    PopExceptionHandler ->
      if null exceptionHandlers
        then Left "Runtime error: attempting to pop from empty exception handler stack"
        else processExceptionInstruction
    _ -> processExceptionInstruction
  where
    processExceptionInstruction =
      case applyExceptionInstruction ip instruction stack localEnv exceptionHandlers of
        Left err -> Left err
        Right (Just err, _, _, _) -> Left err
        Right (Nothing, nextIp, nextStack, nextHandlers) ->
          execute code nextIp nextStack globalsEnv localEnv functions globalDecls forStates loopCounts nextHandlers outputs isTopLevel
