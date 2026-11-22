module PythonHS.VM.CallStrConversionOrPassConfig (CallStrConversionOrPassConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data CallStrConversionOrPassConfig = CallStrConversionOrPassConfig
  { callStrConversionOrPassExecute :: VMState -> Either String VMState,
    callStrConversionOrPassIsTopLevel :: Bool,
    callStrConversionOrPassName :: String,
    callStrConversionOrPassArgs :: [Value],
    callStrConversionOrPassPos :: Position,
    callStrConversionOrPassStack :: [Value],
    callStrConversionOrPassGlobals :: Map String Value,
    callStrConversionOrPassLocals :: Map String Value,
    callStrConversionOrPassFunctions :: Map String FunctionInfo,
    callStrConversionOrPassOutputs :: [String]
  }
