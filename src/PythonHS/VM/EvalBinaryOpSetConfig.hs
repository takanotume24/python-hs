module PythonHS.VM.EvalBinaryOpSetConfig (EvalBinaryOpSetConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpSetConfig = EvalBinaryOpSetConfig
  { evalBinaryOpSetLeft :: Value,
    evalBinaryOpSetRight :: Value,
    evalBinaryOpSetOp :: BinaryOperator,
    evalBinaryOpSetPos :: Position
  }
