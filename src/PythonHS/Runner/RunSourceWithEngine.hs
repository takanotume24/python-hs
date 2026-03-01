module PythonHS.Runner.RunSourceWithEngine (runSourceWithEngine) where

import PythonHS.RunSource (runSource)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))

runSourceWithEngine :: RunnerEngine -> String -> Either String [String]
runSourceWithEngine engine source =
  case engine of
    AstEngine -> runSource source
    VmEngine -> runSourceVm source
