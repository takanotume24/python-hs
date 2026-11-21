module PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data ExpectTruthyInput = ExpectTruthyInput
  { expectTruthyInputContext :: String,
    expectTruthyInputPos :: Position,
    expectTruthyInputValue :: Value
  }
