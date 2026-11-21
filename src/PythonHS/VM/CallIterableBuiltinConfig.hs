module PythonHS.VM.CallIterableBuiltinConfig (CallIterableBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallIterableBuiltinConfig = CallIterableBuiltinConfig
  { callIterableBuiltinName :: String,
    callIterableBuiltinArgs :: [Value],
    callIterableBuiltinPos :: Position
  }
