module PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data ToForIterableConfig = ToForIterableConfig
  { toForIterableValue :: Value,
    toForIterablePos :: Position
  }
