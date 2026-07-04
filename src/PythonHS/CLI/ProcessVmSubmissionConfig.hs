module PythonHS.CLI.ProcessVmSubmissionConfig (ProcessVmSubmissionConfig (..)) where

data ProcessVmSubmissionConfig = ProcessVmSubmissionConfig
  { processVmSubmissionAcceptedSourceLines :: [String],
    processVmSubmissionAcceptedOutputs :: [String],
    processVmSubmissionSubmissionLines :: [String]
  }
