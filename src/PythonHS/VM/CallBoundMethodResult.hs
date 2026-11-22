module PythonHS.VM.CallBoundMethodResult (CallBoundMethodResult (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data CallBoundMethodResult = CallBoundMethodResult
  { callBoundMethodResultValue :: Value,
    callBoundMethodResultGlobals :: Map String Value,
    callBoundMethodResultLocals :: Map String Value,
    callBoundMethodResultFunctions :: Map String FunctionInfo,
    callBoundMethodResultOutputs :: [String]
  }
