module PythonHS.VM.CallCollectionBuiltinConfig (CallCollectionBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallCollectionBuiltinConfig = CallCollectionBuiltinConfig
  { callCollectionBuiltinName :: String,
    callCollectionBuiltinArgs :: [Value],
    callCollectionBuiltinPos :: Position
  }
