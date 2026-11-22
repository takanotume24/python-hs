module PythonHS.VM.CallIteratorBuiltinResult (CallIteratorBuiltinResult (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallIteratorBuiltinResult = CallIteratorBuiltinResult
  { callIteratorBuiltinResultStack :: [Value],
    callIteratorBuiltinResultGlobals :: Map String Value,
    callIteratorBuiltinResultLocals :: Map String Value,
    callIteratorBuiltinResultFunctions :: Map String FunctionInfo,
    callIteratorBuiltinResultOutputs :: [String]
  }
