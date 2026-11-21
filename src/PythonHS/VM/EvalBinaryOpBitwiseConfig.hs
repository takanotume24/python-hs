module PythonHS.VM.EvalBinaryOpBitwiseConfig (EvalBinaryOpBitwiseConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpBitwiseConfig = EvalBinaryOpBitwiseConfig
  { evalBinaryOpBitwiseConfigLeft :: Value,
    evalBinaryOpBitwiseConfigRight :: Value,
    evalBinaryOpBitwiseConfigOp :: BinaryOperator,
    evalBinaryOpBitwiseConfigPos :: Position
  }
