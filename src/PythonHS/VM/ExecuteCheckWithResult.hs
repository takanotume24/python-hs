module PythonHS.VM.ExecuteCheckWithResult (executeCheckWithResult) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (StringValue))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteCheckWithResultConfig (ExecuteCheckWithResultConfig (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.VMState (VMState (..))

executeCheckWithResult :: ExecuteCheckWithResultConfig -> Either String VMState
executeCheckWithResult config =
  let state = executeCheckWithResultState config
      execute = executeCheckWithResultExecute config
   in case vmStack state of
        resultValue : rest ->
          if isTruthy resultValue
            then execute state {vmIp = vmIp state + 1, vmStack = rest}
            else case exceptionHandlers (vmException state) of
              handlerIp : restHandlers ->
                let err = case Map.lookup "__python_hs_pending_except_error__" (envLocals (vmEnv state)) of
                      Just (StringValue s) -> s
                      _ -> "Runtime error: error at 9:3"
                    newLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) (envLocals (vmEnv state))
                    newEnv = (vmEnv state) {envLocals = newLocals}
                    newException = (vmException state) {exceptionHandlers = restHandlers}
                 in execute state {vmIp = handlerIp, vmStack = rest, vmEnv = newEnv, vmException = newException}
              [] -> Left "Runtime error: unhandled exception in with statement"
        _ -> Left "VM runtime error: check with result requires one value on stack"
