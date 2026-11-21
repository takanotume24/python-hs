module PythonHS.VM.EvalBinaryOpEqualityConfig (EvalBinaryOpEqualityConfig (..)) where

import PythonHS.Evaluator.Value (Value)

data EvalBinaryOpEqualityConfig = EvalBinaryOpEqualityConfig
  { evalBinaryOpEqualityConfigLeft :: Value,
    evalBinaryOpEqualityConfigRight :: Value,
    evalBinaryOpEqualityConfigNegate :: Bool
  }
