module PythonHS.Evaluator.ComputeAssignInput (ComputeAssignInput (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data ComputeAssignInput = ComputeAssignInput
  { computeAssignInputOp :: String,
    computeAssignInputPos :: Position,
    computeAssignInputLeft :: Value,
    computeAssignInputRight :: Value
  }
