module PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpConfig = EvalBinaryOpConfig
  { evalBinaryOpOp :: BinaryOperator,
    evalBinaryOpLeft :: Value,
    evalBinaryOpRight :: Value,
    evalBinaryOpPos :: Position
  }
