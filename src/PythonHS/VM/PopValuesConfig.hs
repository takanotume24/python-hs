module PythonHS.VM.PopValuesConfig (PopValuesConfig (..)) where

import PythonHS.Evaluator.Value (Value)

data PopValuesConfig = PopValuesConfig
  { popValuesCount :: Int,
    popValuesStack :: [Value]
  }
