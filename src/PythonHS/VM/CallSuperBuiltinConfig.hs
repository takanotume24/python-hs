module PythonHS.VM.CallSuperBuiltinConfig (CallSuperBuiltinConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data CallSuperBuiltinConfig = CallSuperBuiltinConfig
  { callSuperBuiltinName :: String,
    callSuperBuiltinArgs :: [Value],
    callSuperBuiltinPos :: Position,
    callSuperBuiltinLocals :: Map String Value,
    callSuperBuiltinGlobals :: Map String Value
  }
