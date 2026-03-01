module PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value (NoneValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (ReturnTop), Instruction)

evaluateBuiltinArgs ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  Map.Map String Value ->
  [([Instruction], Maybe String, Position)] ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  [Value] ->
  Either String ([Value], Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
evaluateBuiltinArgs executeFn currentLocalEnv remainingArgs currentGlobals currentFunctions currentOutputs accValues =
  case remainingArgs of
    [] -> Right (accValues, currentGlobals, currentFunctions, currentOutputs)
    (argCode, _, _) : restArgs -> do
      (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
        evalArgCode argCode currentGlobals currentFunctions currentOutputs
      evaluateBuiltinArgs executeFn currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg (accValues ++ [argValue])
  where
    evalArgCode argCode globalsNow functionsNow outputsNow = do
      (maybeArgValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
        executeFn (argCode ++ [ReturnTop]) 0 [] globalsNow currentLocalEnv functionsNow Set.empty Map.empty Map.empty outputsNow False
      let argValue =
            case maybeArgValue of
              Just value -> value
              Nothing -> NoneValue
      Right (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg)
