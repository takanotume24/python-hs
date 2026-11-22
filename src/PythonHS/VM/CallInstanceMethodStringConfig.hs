module PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data CallInstanceMethodStringConfig = CallInstanceMethodStringConfig
  { callInstanceMethodStringExecute :: VMState -> Either String VMState,
    callInstanceMethodStringIsTopLevel :: Bool,
    callInstanceMethodStringPos :: Position,
    callInstanceMethodStringStack :: [Value],
    callInstanceMethodStringInstance :: Value,
    callInstanceMethodStringGlobals :: Map String Value,
    callInstanceMethodStringLocals :: Map String Value,
    callInstanceMethodStringFunctions :: Map String FunctionInfo,
    callInstanceMethodStringOutputs :: [String]
  }
