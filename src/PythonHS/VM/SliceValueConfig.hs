module PythonHS.VM.SliceValueConfig (SliceValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data SliceValueConfig = SliceValueConfig
  { sliceValuePos :: Position,
    sliceValueSeqValue :: Value,
    sliceValueStartVal :: Value,
    sliceValueEndVal :: Value
  }
