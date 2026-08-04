module PythonHS.Evaluator.NumberSortPair (NumberSortPair (..)) where

import PythonHS.Evaluator.Value (Value)

data NumberSortPair = NumberSortPair
  { numberSortPairSortKey :: Double,
    numberSortPairValue :: Value
  }
  deriving (Eq, Show)
