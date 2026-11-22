module PythonHS.VM.FindInstanceMutation (findInstanceMutation) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.InstanceMutation (InstanceMutation (..))

-- | Detect that the first parameter of a method was rebound to a new value,
-- which means the bound receiver was mutated.
findInstanceMutation :: [String] -> [(String, Value)] -> Map.Map String Value -> Maybe InstanceMutation
findInstanceMutation params capturedBindings finalLocals =
  case (params, lookup "__python_hs_bound_self__" capturedBindings) of
    (firstParam : _, Just oldValue) ->
      case Map.lookup firstParam finalLocals of
        Just newValue -> Just (InstanceMutation {instanceMutationOldValue = oldValue, instanceMutationNewValue = newValue})
        Nothing -> Nothing
    _ -> Nothing
