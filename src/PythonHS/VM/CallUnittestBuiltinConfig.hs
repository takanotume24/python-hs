module PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

-- | Configuration for the minimal unittest assertion builtins.
data CallUnittestBuiltinConfig = CallUnittestBuiltinConfig
  { callUnittestBuiltinName :: String,
    callUnittestBuiltinArgs :: [Value],
    callUnittestBuiltinPos :: Position
  }
