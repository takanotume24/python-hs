module PythonHS.VM.CallCollectionMethodBuiltinConfig (CallCollectionMethodBuiltinConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallCollectionMethodBuiltinConfig = CallCollectionMethodBuiltinConfig
  { callCollectionMethodBuiltinName :: String,
    callCollectionMethodBuiltinArgs :: [Value],
    callCollectionMethodBuiltinPos :: Position
  }
