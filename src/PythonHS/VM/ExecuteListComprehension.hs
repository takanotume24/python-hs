module PythonHS.VM.ExecuteListComprehension (executeListComprehension) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (ListValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))
import PythonHS.VM.VMState (VMState (..))

executeListComprehension ::
  (VMState -> Either String VMState) ->
  [([String], [Instruction], [[Instruction]])] ->
  [Instruction] ->
  Position ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String (Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeListComprehension execute clauses valueCode pos globalsEnv localEnv functions outputs = do
  (items, globalsAfterItems, functionsAfterItems, outputsAfterItems) <-
    evalClauses clauses localEnv [] globalsEnv functions outputs
  Right (ListValue items, globalsAfterItems, functionsAfterItems, outputsAfterItems)
  where
    requireValue (Just value) = Right value
    requireValue Nothing = Left "VM runtime error: list comprehension iterable did not produce value"

    requireCondition (Just value) = Right value
    requireCondition Nothing = Left "VM runtime error: list comprehension condition did not produce value"

    requireElement (Just value) = Right value
    requireElement Nothing = Left "VM runtime error: list comprehension value did not produce value"

    emptyState code globals locals funcs outs =
      VMState
        { vmCode = code,
          vmIp = 0,
          vmStack = [],
          vmEnv =
            EnvState
              { envGlobals = globals,
                envLocals = locals,
                envFunctions = funcs,
                envGlobalDecls = Set.empty
              },
          vmLoop = LoopState {loopForStates = Map.empty, loopCounts = Map.empty},
          vmException = ExceptionState {exceptionHandlers = [], exceptionOutputs = []},
          vmIsTopLevel = False,
          vmOutputs = outs
        }

    extractReturnValue finalState =
      case vmStack finalState of
        (value : _) -> Just value
        [] -> Nothing

    evalClauses [] localsNow acc globalsNow functionsNow outputsNow = do
      let state = emptyState valueCode globalsNow localsNow functionsNow outputsNow
      finalState <- execute state
      value <- requireElement (extractReturnValue finalState)
      Right (acc ++ [value], envGlobals (vmEnv finalState), envFunctions (vmEnv finalState), vmOutputs finalState)
    evalClauses ((loopTargets, iterCode, condCodes) : restClauses) localsNow acc globalsNow functionsNow outputsNow = do
      let iterState = emptyState iterCode globalsNow localsNow functionsNow outputsNow
      finalIterState <- execute iterState
      iterValue <- requireValue (extractReturnValue finalIterState)
      iterItems <- toForIterable ToForIterableConfig {toForIterableValue = iterValue, toForIterablePos = pos}
      evalClauseItems loopTargets condCodes restClauses localsNow iterItems acc (envGlobals (vmEnv finalIterState)) (envFunctions (vmEnv finalIterState)) (vmOutputs finalIterState)

    evalClauseItems _ _ _ _ [] acc globalsNow functionsNow outputsNow =
      Right (acc, globalsNow, functionsNow, outputsNow)
    evalClauseItems loopTargets condCodes restClauses localsNow (item : restItems) acc globalsNow functionsNow outputsNow = do
      nextLocals <- bindTargets loopTargets item localsNow
      (shouldInclude, globalsAfterCond, functionsAfterCond, outputsAfterCond) <-
        evaluateConditions condCodes nextLocals globalsNow functionsNow outputsNow
      (accAfterItem, globalsAfterItem, functionsAfterItem, outputsAfterItem) <-
        if shouldInclude
          then evalClauses restClauses nextLocals acc globalsAfterCond functionsAfterCond outputsAfterCond
          else Right (acc, globalsAfterCond, functionsAfterCond, outputsAfterCond)
      evalClauseItems loopTargets condCodes restClauses localsNow restItems accAfterItem globalsAfterItem functionsAfterItem outputsAfterItem

    evaluateConditions [] _ globalsNow functionsNow outputsNow =
      Right (True, globalsNow, functionsNow, outputsNow)
    evaluateConditions (condCode : restCodes) localsNow globalsNow functionsNow outputsNow = do
      let state = emptyState condCode globalsNow localsNow functionsNow outputsNow
      finalState <- execute state
      condValue <- requireCondition (extractReturnValue finalState)
      if isTruthy condValue
        then evaluateConditions restCodes localsNow (envGlobals (vmEnv finalState)) (envFunctions (vmEnv finalState)) (vmOutputs finalState)
        else Right (False, envGlobals (vmEnv finalState), envFunctions (vmEnv finalState), vmOutputs finalState)

    bindTargets [] _ _ = Left ("Value error: empty comprehension target at " ++ showPos pos)
    bindTargets [name] value localsNow = Right (Map.insert name value localsNow)
    bindTargets names value localsNow =
      case value of
        ListValue values ->
          if length values == length names
            then Right (foldl bindOne localsNow (zip names values))
            else Left ("Value error: unpacking mismatch in comprehension at " ++ showPos pos)
        _ -> Left ("Type error: unpacking expects list value in comprehension at " ++ showPos pos)

    bindOne envNow (name, value) = Map.insert name value envNow
