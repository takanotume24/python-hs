module PythonHS.VM.CallNumericBuiltinConfig (CallNumericBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallNumericBuiltinConfig = CallNumericBuiltinConfig
  { callNumericBuiltinName :: String,
    callNumericBuiltinArgs :: [Value],
    callNumericBuiltinPos :: Position
  }
