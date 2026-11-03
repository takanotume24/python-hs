module PythonHS.VM.ExecuteStoreClassAttr (executeStoreClassAttr) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (ClassValue))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.Instruction (Instruction (StoreClassAttr))
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.VMState (VMState (..))

executeStoreClassAttr ::
  VMState ->
  Instruction ->
  Either String VMState
executeStoreClassAttr state instruction =
  case instruction of
    StoreClassAttr className attrName ->
      case vmStack state of
        value : rest ->
          case lookupName className (envLocals (vmEnv state)) (envGlobals (vmEnv state)) of
            Just (ClassValue baseName maybeBase methods classAttrs) ->
              let newAttrs = upsertAttr attrName value classAttrs
                  newClassValue = ClassValue baseName maybeBase methods newAttrs
                  env = vmEnv state
                  newEnv
                    | Map.member className (envLocals env) =
                        let newLocals = Map.insert className newClassValue (envLocals env)
                         in if vmIsTopLevel state
                              then env {envLocals = newLocals, envGlobals = newLocals}
                              else env {envLocals = newLocals}
                    | otherwise = env {envGlobals = Map.insert className newClassValue (envGlobals env)}
               in Right state {vmIp = vmIp state + 1, vmStack = rest, vmEnv = newEnv}
            Just _ -> Left "Type error: class attribute store expects class value"
            Nothing -> Left ("Name error: undefined class " ++ className ++ " at 0:0")
        _ -> Left "VM runtime error: StoreClassAttr requires one value on stack"
    _ -> Left "VM runtime error: unexpected instruction in executeStoreClassAttr"
  where
    upsertAttr key newValue pairs =
      case pairs of
        [] -> [(key, newValue)]
        (k, v) : restPairs ->
          if k == key
            then (k, newValue) : restPairs
            else (k, v) : upsertAttr key newValue restPairs
