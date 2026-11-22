module PythonHS.VM.DelsliceValueConfig (DelsliceValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data DelsliceValueConfig = DelsliceValueConfig
  { delsliceValuePos :: Position,
    delsliceValueSeqValue :: Value,
    delsliceValueStartVal :: Value,
    delsliceValueEndVal :: Value
  }
