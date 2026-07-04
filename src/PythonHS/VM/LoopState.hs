module PythonHS.VM.LoopState (LoopState (..)) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)

data LoopState = LoopState
  { loopForStates :: Map.Map Int [Value],
    loopCounts :: Map.Map Int Int
  }
  deriving (Show)
