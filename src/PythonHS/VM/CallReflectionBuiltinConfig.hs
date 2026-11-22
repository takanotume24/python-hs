module PythonHS.VM.CallReflectionBuiltinConfig (CallReflectionBuiltinConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallReflectionBuiltinConfig = CallReflectionBuiltinConfig
  { callReflectionBuiltinName :: String,
    callReflectionBuiltinArgs :: [Value],
    callReflectionBuiltinPos :: Position,
    callReflectionBuiltinGlobals :: Map String Value,
    callReflectionBuiltinLocals :: Map String Value,
    callReflectionBuiltinFunctions :: Map String FunctionInfo
  }
