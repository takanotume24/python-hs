module PythonHS.VM.EvalBinaryOpMembershipConfig (EvalBinaryOpMembershipConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalBinaryOpMembershipConfig = EvalBinaryOpMembershipConfig
  { evalBinaryOpMembershipConfigLeft :: Value,
    evalBinaryOpMembershipConfigRight :: Value,
    evalBinaryOpMembershipConfigPos :: Position,
    evalBinaryOpMembershipConfigNegate :: Bool
  }
