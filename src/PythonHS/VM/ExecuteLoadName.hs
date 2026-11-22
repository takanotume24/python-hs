module PythonHS.VM.ExecuteLoadName (executeLoadName) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..), unboundLocalValueName)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExecuteLoadNameConfig (ExecuteLoadNameConfig (..))
import PythonHS.VM.IsBuiltinCallableName (isBuiltinCallableName)
import PythonHS.VM.IsBuiltinExceptionClass (isBuiltinExceptionClass)
import PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr)
import PythonHS.VM.VMState (VMState (..))

executeLoadName :: ExecuteLoadNameConfig -> Either String VMState
executeLoadName config =
  case lookupNameWithAttr name pos (envLocals env) (envGlobals env) of
    Left attrError -> Left attrError
    Right Nothing ->
      case Map.lookup name (envFunctions env) of
        Just _ -> push (FunctionRefValue name [])
        Nothing
          | isBuiltinCallableName name ->
              push (FunctionRefValue name [])
          | isBuiltinExceptionClass name ->
              push
                ClassValue
                  { classValueName = name,
                    classValueBase = Nothing,
                    classValueMethods = [],
                    classValueAttrs = []
                  }
          | otherwise -> Left ("Name error: undefined identifier " ++ name ++ " at " ++ showPos pos)
    Right (Just value) ->
      case value of
        UnboundLocalValue {unboundLocalValueName = unboundName} ->
          Left ("UnboundLocalError: local variable " ++ unboundName ++ " referenced before assignment at " ++ showPos pos)
        _ -> push value
  where
    state = executeLoadNameState config
    name = executeLoadNameName config
    pos = executeLoadNamePos config
    env = vmEnv state
    push value = Right state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
