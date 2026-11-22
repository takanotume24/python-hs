module PythonHS.VM.SetitemValueConfig (SetitemValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data SetitemValueConfig = SetitemValueConfig
  { setitemValuePos :: Position,
    setitemValueSeqValue :: Value,
    setitemValueIndexValue :: Value,
    setitemValueNewValue :: Value
  }
