module PythonHS.VM.CallFileBuiltinConfig (CallFileBuiltinConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallFileBuiltinConfig = CallFileBuiltinConfig
  { callFileBuiltinIsTopLevel :: Bool,
    callFileBuiltinName :: String,
    callFileBuiltinArgs :: [Value],
    callFileBuiltinPos :: Position,
    callFileBuiltinStack :: [Value],
    callFileBuiltinGlobals :: Map String Value,
    callFileBuiltinLocals :: Map String Value,
    callFileBuiltinFunctions :: Map String FunctionInfo,
    callFileBuiltinOutputs :: [String]
  }
