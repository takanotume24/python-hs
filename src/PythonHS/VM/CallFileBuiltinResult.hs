module PythonHS.VM.CallFileBuiltinResult (CallFileBuiltinResult (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallFileBuiltinResult = CallFileBuiltinResult
  { callFileBuiltinResultStack :: [Value],
    callFileBuiltinResultGlobals :: Map String Value,
    callFileBuiltinResultLocals :: Map String Value,
    callFileBuiltinResultFunctions :: Map String FunctionInfo,
    callFileBuiltinResultOutputs :: [String]
  }
