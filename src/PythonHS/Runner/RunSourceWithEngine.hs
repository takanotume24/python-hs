module PythonHS.Runner.RunSourceWithEngine (runSourceWithEngine) where

import PythonHS.RunSource (runSource)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))
import PythonHS.Runner.RunSourceWithEngineConfig (RunSourceWithEngineConfig (..))

runSourceWithEngine :: RunSourceWithEngineConfig -> Either String [String]
runSourceWithEngine config =
  case runSourceWithEngineEngine config of
    AstEngine -> runSource (runSourceWithEngineSource config)
    VmEngine -> runSourceVm (runSourceWithEngineSource config)
