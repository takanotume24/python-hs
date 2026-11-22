module PythonHS.VM.DelitemValueConfig (DelitemValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data DelitemValueConfig = DelitemValueConfig
  { delitemValuePos :: Position,
    delitemValueSeqValue :: Value,
    delitemValueIndexValue :: Value
  }
