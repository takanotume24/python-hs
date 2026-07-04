module PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (ClassValue))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.VMState (VMState (..))

executeDefineClassInstruction ::
  (VMState -> Either String VMState) ->
  VMState ->
  Instruction ->
  Either String VMState
executeDefineClassInstruction execute state instruction =
  case instruction of
    DefineClass className maybeBase methods ->
      let classValue = ClassValue className maybeBase methods
          env = vmEnv state
       in if vmIsTopLevel state || Set.member className (envGlobalDecls env)
            then
              let newGlobals = Map.insert className classValue (envGlobals env)
                  newEnv = env {envGlobals = newGlobals, envLocals = if vmIsTopLevel state then newGlobals else envLocals env}
               in execute state {vmIp = vmIp state + 1, vmEnv = newEnv}
            else
              let newLocals = Map.insert className classValue (envLocals env)
                  newEnv = env {envLocals = newLocals}
               in execute state {vmIp = vmIp state + 1, vmEnv = newEnv}
    _ -> Left "VM runtime error: unexpected instruction in executeDefineClassInstruction"
