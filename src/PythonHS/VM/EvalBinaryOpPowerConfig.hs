module PythonHS.VM.EvalBinaryOpPowerConfig (EvalBinaryOpPowerConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpPowerConfig = EvalBinaryOpPowerConfig
  { evalBinaryOpPowerConfigLeft :: Value,
    evalBinaryOpPowerConfigRight :: Value,
    evalBinaryOpPowerConfigPos :: Position,
    evalBinaryOpPowerConfigExpectNumber :: Position -> Value -> Either String Double
  }
