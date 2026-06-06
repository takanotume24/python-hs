module PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value(ClassValue))
import PythonHS.VM.Instruction (Instruction(..))

executeDefineClassInstruction ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  [Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Instruction -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeDefineClassInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction =
  case instruction of
    DefineClass className maybeBase methods ->
      let classValue = ClassValue className maybeBase methods
       in if isTopLevel || Set.member className globalDecls
            then
              let newGlobals = Map.insert className classValue globalsEnv
                  newLocals = if isTopLevel then newGlobals else localEnv
               in execute code (ip + 1) stack newGlobals newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            else
              let newLocals = Map.insert className classValue localEnv
               in execute code (ip + 1) stack globalsEnv newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    _ -> Left "VM runtime error: unexpected instruction in executeDefineClassInstruction"
