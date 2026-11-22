module PythonHS.VM.InstanceMutation (InstanceMutation (..)) where

import PythonHS.Evaluator.Value (Value)

-- | The instance value before and after a method call mutated it.
data InstanceMutation = InstanceMutation
  { instanceMutationOldValue :: Value,
    instanceMutationNewValue :: Value
  }
  deriving (Eq, Show)
