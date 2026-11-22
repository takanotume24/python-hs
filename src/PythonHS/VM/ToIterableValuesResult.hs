module PythonHS.VM.ToIterableValuesResult (ToIterableValuesResult (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.FunctionInfo (FunctionInfo)

data ToIterableValuesResult = ToIterableValuesResult
  { toIterableValuesResultValues :: [Value],
    toIterableValuesResultGlobals :: Map String Value,
    toIterableValuesResultLocals :: Map String Value,
    toIterableValuesResultFunctions :: Map String FunctionInfo,
    toIterableValuesResultOutputs :: [String]
  }
