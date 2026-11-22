module PythonHS.VM.SliceRangeConfig (SliceRangeConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data SliceRangeConfig = SliceRangeConfig
  { sliceRangePos :: Position,
    sliceRangeLength :: Int,
    sliceRangeStartVal :: Value,
    sliceRangeEndVal :: Value
  }
