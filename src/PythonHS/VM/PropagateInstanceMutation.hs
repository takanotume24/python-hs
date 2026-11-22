module PythonHS.VM.PropagateInstanceMutation (propagateInstanceMutation) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)

-- | Replace every binding that still refers to the old instance value with the
-- updated instance produced by a method call.
propagateInstanceMutation :: Value -> Value -> Map.Map String Value -> Map.Map String Value
propagateInstanceMutation oldValue newValue
  | oldValue == newValue = id
  | otherwise = Map.map replace
  where
    replace value
      | value == oldValue = newValue
      | otherwise = value
