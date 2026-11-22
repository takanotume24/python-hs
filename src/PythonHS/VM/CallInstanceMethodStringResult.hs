module PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallInstanceMethodStringResult = CallInstanceMethodStringResult
  { callInstanceMethodStringResultStack :: [Value],
    callInstanceMethodStringResultGlobals :: Map String Value,
    callInstanceMethodStringResultLocals :: Map String Value,
    callInstanceMethodStringResultFunctions :: Map String FunctionInfo,
    callInstanceMethodStringResultOutputs :: [String]
  }
