module PythonHS.VM.CallMapFilterBuiltinConfig (CallMapFilterBuiltinConfig (..)) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data CallMapFilterBuiltinConfig = CallMapFilterBuiltinConfig
  { callMapFilterBuiltinExecute :: VMState -> Either String VMState,
    callMapFilterBuiltinIsTopLevel :: Bool,
    callMapFilterBuiltinName :: String,
    callMapFilterBuiltinArgs :: [Value],
    callMapFilterBuiltinPos :: Position,
    callMapFilterBuiltinStack :: [Value],
    callMapFilterBuiltinGlobals :: Map.Map String Value,
    callMapFilterBuiltinLocals :: Map.Map String Value,
    callMapFilterBuiltinFunctions :: Map.Map String FunctionInfo,
    callMapFilterBuiltinOutputs :: [String]
  }
