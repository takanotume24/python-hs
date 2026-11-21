module PythonHS.VM.EvalBinaryOpIdentityConfig (EvalBinaryOpIdentityConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpIdentityConfig = EvalBinaryOpIdentityConfig
  { evalBinaryOpIdentityConfigLeft :: Value,
    evalBinaryOpIdentityConfigRight :: Value,
    evalBinaryOpIdentityConfigNegate :: Bool,
    evalBinaryOpIdentityConfigPos :: Position
  }
