module PythonHS.VM.SetsliceValueConfig (SetsliceValueConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data SetsliceValueConfig = SetsliceValueConfig
  { setsliceValuePos :: Position,
    setsliceValueSeqValue :: Value,
    setsliceValueStartVal :: Value,
    setsliceValueEndVal :: Value,
    setsliceValueNewItems :: Value
  }
