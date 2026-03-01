module PythonHS.VM.BindDefaults (bindDefaults) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (NoneValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

bindDefaults ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  String ->
  Position ->
  [String] ->
  [(String, [Instruction])] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String (Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
bindDefaults execute fname pos params defaultCodes initialLocals globalsNow functionsNow outputsNow =
  fill params initialLocals globalsNow functionsNow outputsNow
  where
    defaultMap = Map.fromList defaultCodes

    fill remainingParams currentLocals currentGlobals currentFunctions currentOutputs =
      case remainingParams of
        [] -> Right (currentLocals, currentGlobals, currentFunctions, currentOutputs)
        paramName : restParams ->
          case Map.lookup paramName currentLocals of
            Just _ -> fill restParams currentLocals currentGlobals currentFunctions currentOutputs
            Nothing ->
              case Map.lookup paramName defaultMap of
                Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)
                Just defaultCode -> do
                  (maybeDefaultValue, globalsAfterDefault, functionsAfterDefault, outputsAfterDefault) <-
                    execute defaultCode 0 [] currentGlobals currentLocals currentFunctions Set.empty Map.empty Map.empty [] currentOutputs False
                  let defaultValue =
                        case maybeDefaultValue of
                          Just value -> value
                          Nothing -> NoneValue
                  let newLocals = Map.insert paramName defaultValue currentLocals
                  fill restParams newLocals globalsAfterDefault functionsAfterDefault outputsAfterDefault
