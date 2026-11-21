module PythonHS.VM.CallStringBuiltinConfig (CallStringBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallStringBuiltinConfig = CallStringBuiltinConfig
  { callStringBuiltinName :: String,
    callStringBuiltinArgs :: [Value],
    callStringBuiltinPos :: Position
  }
