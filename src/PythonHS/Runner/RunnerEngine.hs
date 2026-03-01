module PythonHS.Runner.RunnerEngine (RunnerEngine (..)) where

data RunnerEngine
  = AstEngine
  | VmEngine
  deriving (Eq, Show)
