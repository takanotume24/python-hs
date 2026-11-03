module PythonHS.VM.CallExceptionClassBuiltinConfig (CallExceptionClassBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallExceptionClassBuiltinConfig = CallExceptionClassBuiltinConfig
  { callExceptionClassBuiltinName :: String,
    callExceptionClassBuiltinArgs :: [Value],
    callExceptionClassBuiltinPos :: Position
  }
