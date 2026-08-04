module PythonHS.Evaluator.RemoveFirstValueConfig (RemoveFirstValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)

data RemoveFirstValueConfig = RemoveFirstValueConfig
  { removeFirstValueConfigValues :: [Value],
    removeFirstValueConfigTarget :: Value
  }
