module PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallBuiltinConfig = CallBuiltinConfig
  { callBuiltinName :: String,
    callBuiltinArgs :: [Value],
    callBuiltinPos :: Position
  }
