module PythonHS.VM.GetitemValueConfig (GetitemValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data GetitemValueConfig = GetitemValueConfig
  { getitemValuePos :: Position,
    getitemValueSeqValue :: Value,
    getitemValueIndexValue :: Value
  }
