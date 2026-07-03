module PythonHS.CLI.VmReplState (VmReplState (..)) where

-- | State for a single VM REPL evaluation cycle.
data VmReplState = VmReplState
  { vmLines :: [String],
    vmOutputs :: [String],
    vmAcc :: [String]
  }
