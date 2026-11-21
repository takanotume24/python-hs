module PythonHS.Evaluator.ExpectNumberInput (ExpectNumberInput (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data ExpectNumberInput = ExpectNumberInput
  { expectNumberInputContext :: String,
    expectNumberInputPos :: Position,
    expectNumberInputValue :: Value
  }
