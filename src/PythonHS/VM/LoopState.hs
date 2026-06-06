module PythonHS.VM.LoopState (LoopState(..)) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Value (Value)

data LoopState = LoopState
  { loopForStates  :: Map.Map Int [Value]
  , loopCounts     :: Map.Map Int Int
  } deriving (Show)
