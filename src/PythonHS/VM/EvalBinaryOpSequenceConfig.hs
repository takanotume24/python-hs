module PythonHS.VM.EvalBinaryOpSequenceConfig (EvalBinaryOpSequenceConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

-- | Inputs for sequence concatenation and repetition.
data EvalBinaryOpSequenceConfig = EvalBinaryOpSequenceConfig
  { evalBinaryOpSequenceOp :: BinaryOperator,
    evalBinaryOpSequenceLeft :: Value,
    evalBinaryOpSequenceRight :: Value,
    evalBinaryOpSequencePos :: Position
  }
