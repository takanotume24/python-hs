module PythonHS.Runner.ResolveRunnerEngine (resolveRunnerEngine) where

import Data.Char (toLower)
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))

resolveRunnerEngine :: Maybe String -> RunnerEngine
resolveRunnerEngine envEngine =
  case fmap (map toLower) envEngine of
    Just "vm" -> VmEngine
    _ -> AstEngine
