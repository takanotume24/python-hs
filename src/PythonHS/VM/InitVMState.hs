module PythonHS.VM.InitVMState (initVMState) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

initVMState :: [Instruction] -> VMState
initVMState instructions =
  VMState
    { vmCode = instructions,
      vmIp = 0,
      vmStack = [],
      vmEnv =
        EnvState
          { envGlobals = Map.empty,
            envLocals = Map.empty,
            envFunctions = Map.empty,
            envGlobalDecls = Set.empty
          },
      vmLoop =
        LoopState
          { loopForStates = Map.empty,
            loopCounts = Map.empty
          },
      vmException =
        ExceptionState
          { exceptionHandlers = [],
            exceptionOutputs = []
          },
      vmIsTopLevel = True,
      vmOutputs = []
    }
