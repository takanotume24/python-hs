module PythonHS.VM.CallStdlibBuiltinConfig (CallStdlibBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallStdlibBuiltinConfig = CallStdlibBuiltinConfig
  { callStdlibBuiltinName :: String,
    callStdlibBuiltinArgs :: [Value],
    callStdlibBuiltinPos :: Position
  }
