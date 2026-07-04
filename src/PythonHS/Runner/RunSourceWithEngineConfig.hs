module PythonHS.Runner.RunSourceWithEngineConfig (RunSourceWithEngineConfig (..)) where

import PythonHS.Runner.RunnerEngine (RunnerEngine)

-- | Configuration for running source code with a specific engine.
data RunSourceWithEngineConfig = RunSourceWithEngineConfig
  { runSourceWithEngineEngine :: RunnerEngine,
    runSourceWithEngineSource :: String
  }
