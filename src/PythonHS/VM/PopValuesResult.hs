module PythonHS.VM.PopValuesResult (PopValuesResult (..)) where

import PythonHS.Evaluator.Value (Value)

data PopValuesResult = PopValuesResult
  { popValuesResultPopped :: [Value],
    popValuesResultRest :: [Value]
  }
  deriving (Eq, Show)
