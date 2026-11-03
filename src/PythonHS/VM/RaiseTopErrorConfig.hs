module PythonHS.VM.RaiseTopErrorConfig (RaiseTopErrorConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data RaiseTopErrorConfig = RaiseTopErrorConfig
  { raiseTopErrorValue :: Value,
    raiseTopErrorPos :: Position
  }
