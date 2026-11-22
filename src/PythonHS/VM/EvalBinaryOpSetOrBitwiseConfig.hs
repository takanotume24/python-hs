module PythonHS.VM.EvalBinaryOpSetOrBitwiseConfig (EvalBinaryOpSetOrBitwiseConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpSetOrBitwiseConfig = EvalBinaryOpSetOrBitwiseConfig
  { evalBinaryOpSetOrBitwiseLeft :: Value,
    evalBinaryOpSetOrBitwiseRight :: Value,
    evalBinaryOpSetOrBitwiseOp :: BinaryOperator,
    evalBinaryOpSetOrBitwisePos :: Position
  }
