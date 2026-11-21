module PythonHS.VM.CallConversionBuiltinConfig (CallConversionBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallConversionBuiltinConfig = CallConversionBuiltinConfig
  { callConversionBuiltinName :: String,
    callConversionBuiltinArgs :: [Value],
    callConversionBuiltinPos :: Position
  }
