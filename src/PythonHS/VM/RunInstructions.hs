module PythonHS.VM.RunInstructions (runInstructions) where

import qualified Data.Map.Strict as Map
import PythonHS.VM.ExecuteOneInstruction (executeOneInstruction)
import PythonHS.VM.HandleRuntimeError (handleRuntimeError)
import PythonHS.VM.Instruction (Instruction)

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  (_, _, _, outputs) <- execute instructions 0 [] Map.empty Map.empty Map.empty mempty Map.empty Map.empty [] [] True
  pure outputs
  where
    execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
      | ip < 0 || ip >= length code = Right (Nothing, globalsEnv, functions, outputs)
      | otherwise = do
          handleRuntimeError execute code stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel $
            case code !! ip of
              instruction -> executeOneInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction
