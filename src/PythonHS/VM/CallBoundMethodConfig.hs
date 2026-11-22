module PythonHS.VM.CallBoundMethodConfig (CallBoundMethodConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data CallBoundMethodConfig = CallBoundMethodConfig
  { callBoundMethodExecute :: VMState -> Either String VMState,
    callBoundMethodIsTopLevel :: Bool,
    callBoundMethodPos :: Position,
    callBoundMethodInstance :: Value,
    callBoundMethodName :: String,
    callBoundMethodMissingMessage :: String,
    callBoundMethodGlobals :: Map String Value,
    callBoundMethodLocals :: Map String Value,
    callBoundMethodFunctions :: Map String FunctionInfo,
    callBoundMethodOutputs :: [String]
  }
