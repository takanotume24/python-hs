module PythonHS.VM.CallIteratorBuiltinConfig (CallIteratorBuiltinConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data CallIteratorBuiltinConfig = CallIteratorBuiltinConfig
  { callIteratorBuiltinExecute :: VMState -> Either String VMState,
    callIteratorBuiltinIsTopLevel :: Bool,
    callIteratorBuiltinName :: String,
    callIteratorBuiltinArgs :: [Value],
    callIteratorBuiltinPos :: Position,
    callIteratorBuiltinStack :: [Value],
    callIteratorBuiltinGlobals :: Map String Value,
    callIteratorBuiltinLocals :: Map String Value,
    callIteratorBuiltinFunctions :: Map String FunctionInfo,
    callIteratorBuiltinOutputs :: [String]
  }
