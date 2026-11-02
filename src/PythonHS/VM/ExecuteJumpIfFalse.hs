module PythonHS.VM.ExecuteJumpIfFalse (executeJumpIfFalse) where

import PythonHS.VM.ExecuteJumpIfFalseConfig (ExecuteJumpIfFalseConfig (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.VMState (VMState (..))

executeJumpIfFalse :: ExecuteJumpIfFalseConfig -> Either String VMState
executeJumpIfFalse config =
  case vmStack (executeJumpIfFalseState config) of
    value : rest ->
      if isTruthy value
        then
          executeJumpIfFalseExecute
            config
            (executeJumpIfFalseState config) {vmIp = vmIp (executeJumpIfFalseState config) + 1, vmStack = rest}
        else
          executeJumpIfFalseExecute
            config
            (executeJumpIfFalseState config) {vmIp = executeJumpIfFalseTarget config, vmStack = rest}
    _ -> Left "VM runtime error: conditional jump requires one value on stack"
