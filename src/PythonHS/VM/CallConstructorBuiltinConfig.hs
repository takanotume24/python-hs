module PythonHS.VM.CallConstructorBuiltinConfig (CallConstructorBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallConstructorBuiltinConfig = CallConstructorBuiltinConfig
  { callConstructorBuiltinName :: String,
    callConstructorBuiltinArgs :: [Value],
    callConstructorBuiltinPos :: Position
  }
