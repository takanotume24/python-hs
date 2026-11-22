module PythonHS.VM.ToIterableValuesConfig (ToIterableValuesConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.VMState (VMState)

data ToIterableValuesConfig = ToIterableValuesConfig
  { toIterableValuesExecute :: VMState -> Either String VMState,
    toIterableValuesIsTopLevel :: Bool,
    toIterableValuesValue :: Value,
    toIterableValuesPos :: Position,
    toIterableValuesGlobals :: Map String Value,
    toIterableValuesLocals :: Map String Value,
    toIterableValuesFunctions :: Map String FunctionInfo,
    toIterableValuesOutputs :: [String]
  }
