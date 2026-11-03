module PythonHS.VM.EvalBinaryOpDivideConfig (EvalBinaryOpDivideConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpDivideConfig = EvalBinaryOpDivideConfig
  { evalBinaryOpDivideConfigLeft :: Value,
    evalBinaryOpDivideConfigRight :: Value,
    evalBinaryOpDivideConfigPos :: Position,
    evalBinaryOpDivideConfigExpectNumber :: Position -> Value -> Either String Double
  }
