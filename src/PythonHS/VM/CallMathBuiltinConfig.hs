module PythonHS.VM.CallMathBuiltinConfig (CallMathBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallMathBuiltinConfig = CallMathBuiltinConfig
  { callMathBuiltinName :: String,
    callMathBuiltinArgs :: [Value],
    callMathBuiltinPos :: Position
  }
