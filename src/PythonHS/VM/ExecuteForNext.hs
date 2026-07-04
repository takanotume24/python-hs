module PythonHS.VM.ExecuteForNext (executeForNext) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.VMScopeContext (VMScopeContext (vmScopeContextGlobalDecls, vmScopeContextIsTopLevel))

executeForNext ::
  VMScopeContext ->
  Int ->
  String ->
  Int ->
  Map.Map Int [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Either String (Int, Map.Map Int [Value], Map.Map String Value, Map.Map String Value)
executeForNext scopeCtx ip name loopEndIndex forStates globalsEnv localEnv =
  let isTopLevel = vmScopeContextIsTopLevel scopeCtx
      globalDecls = vmScopeContextGlobalDecls scopeCtx
   in case Map.lookup ip forStates of
        Nothing -> Left "VM runtime error: missing for-loop state"
        Just [] ->
          let newForStates = Map.delete ip forStates
           in Right (loopEndIndex, newForStates, globalsEnv, localEnv)
        Just (nextValue : remainingValues) ->
          let newForStates = Map.insert ip remainingValues forStates
           in if isTopLevel || Set.member name globalDecls
                then
                  let newGlobals = Map.insert name nextValue globalsEnv
                      newLocals = if isTopLevel then newGlobals else localEnv
                   in Right (ip + 1, newForStates, newGlobals, newLocals)
                else
                  let newLocals = Map.insert name nextValue localEnv
                   in Right (ip + 1, newForStates, globalsEnv, newLocals)
