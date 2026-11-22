module PythonHS.VM.CallBuiltinOrFailConfig (CallBuiltinOrFailConfig (..)) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompiledCallArg (CompiledCallArg)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data CallBuiltinOrFailConfig = CallBuiltinOrFailConfig
  { callBuiltinOrFailExecute :: VMState -> Either String VMState,
    callBuiltinOrFailIsTopLevel :: Bool,
    callBuiltinOrFailName :: String,
    callBuiltinOrFailCompiledArgs :: [CompiledCallArg Instruction],
    callBuiltinOrFailPos :: Position,
    callBuiltinOrFailStack :: [Value],
    callBuiltinOrFailLocals :: Map String Value,
    callBuiltinOrFailArgs :: [Value],
    callBuiltinOrFailGlobals :: Map String Value,
    callBuiltinOrFailFunctions :: Map String FunctionInfo,
    callBuiltinOrFailOutputs :: [String]
  }
