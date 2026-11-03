module PythonHS.VM.ExecuteLoopGuard (executeLoopGuard) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.ExecuteLoopGuardConfig (ExecuteLoopGuardConfig (..))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

executeLoopGuard :: ExecuteLoopGuardConfig -> Either String VMState
executeLoopGuard config =
  let state = executeLoopGuardState config
      currentCount = Map.findWithDefault 0 (vmIp state) (loopCounts (vmLoop state))
   in if currentCount >= maxLoopIterations
        then Left ("Value error: iteration limit exceeded at " ++ showPos (executeLoopGuardPos config))
        else
          Right
            state
              { vmIp = vmIp state + 1,
                vmLoop = (vmLoop state) {loopCounts = Map.insert (vmIp state) (currentCount + 1) (loopCounts (vmLoop state))}
              }
