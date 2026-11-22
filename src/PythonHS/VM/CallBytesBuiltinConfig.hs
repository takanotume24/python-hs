module PythonHS.VM.CallBytesBuiltinConfig (CallBytesBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallBytesBuiltinConfig = CallBytesBuiltinConfig
  { callBytesBuiltinName :: String,
    callBytesBuiltinArgs :: [Value],
    callBytesBuiltinPos :: Position
  }
