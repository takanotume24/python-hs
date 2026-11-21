module PythonHS.VM.CallMapFilterBuiltinResult (CallMapFilterBuiltinResult (..)) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallMapFilterBuiltinResult = CallMapFilterBuiltinResult
  { callMapFilterBuiltinResultStack :: [Value],
    callMapFilterBuiltinResultGlobals :: Map.Map String Value,
    callMapFilterBuiltinResultLocals :: Map.Map String Value,
    callMapFilterBuiltinResultFunctions :: Map.Map String FunctionInfo,
    callMapFilterBuiltinResultOutputs :: [String]
  }
