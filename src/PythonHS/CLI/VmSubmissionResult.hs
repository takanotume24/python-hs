module PythonHS.CLI.VmSubmissionResult (VmSubmissionResult (..)) where

-- | Result of processing a VM REPL submission.
data VmSubmissionResult = VmSubmissionResult
  { vmResultLines :: [String],
    vmResultOutputs :: [String],
    vmResultDeltaOutputs :: [String]
  }
