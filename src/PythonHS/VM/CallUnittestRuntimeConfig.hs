module PythonHS.VM.CallUnittestRuntimeConfig (CallUnittestRuntimeConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

-- | Configuration for unittest helpers that need to execute user code.
data CallUnittestRuntimeConfig = CallUnittestRuntimeConfig
  { callUnittestRuntimeExecute :: VMState -> Either String VMState,
    callUnittestRuntimeIsTopLevel :: Bool,
    callUnittestRuntimeName :: String,
    callUnittestRuntimeArgs :: [Value],
    callUnittestRuntimePos :: Position,
    callUnittestRuntimeStack :: [Value],
    callUnittestRuntimeGlobals :: Map String Value,
    callUnittestRuntimeLocals :: Map String Value,
    callUnittestRuntimeFunctions :: Map String FunctionInfo,
    callUnittestRuntimeOutputs :: [String]
  }
