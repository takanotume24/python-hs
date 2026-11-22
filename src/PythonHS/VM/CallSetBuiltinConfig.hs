module PythonHS.VM.CallSetBuiltinConfig (CallSetBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallSetBuiltinConfig = CallSetBuiltinConfig
  { callSetBuiltinName :: String,
    callSetBuiltinArgs :: [Value],
    callSetBuiltinPos :: Position
  }
