module PythonHS.VM.FormatValueConfig (FormatValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data FormatValueConfig = FormatValueConfig
  { formatValueValue :: Value,
    formatValueSpec :: String,
    formatValuePos :: Position
  }
