module PythonHS.Evaluator.NumberPairsConfig (NumberPairsConfig (..)) where

import PythonHS.Evaluator.Value (Value)

data NumberPairsConfig = NumberPairsConfig
  { numberPairsConfigValues :: [Value]
  }
