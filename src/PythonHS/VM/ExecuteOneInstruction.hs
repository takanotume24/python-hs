module PythonHS.VM.ExecuteOneInstruction (executeOneInstruction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FunctionRefValue, ListValue, StringValue, TupleValue, UnboundLocalValue), unboundLocalValueName)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteArithmeticInstruction (executeArithmeticInstruction)
import PythonHS.VM.ExecuteArithmeticInstructionConfig (ExecuteArithmeticInstructionConfig (..))
import PythonHS.VM.ExecuteAssertInstruction (executeAssertInstruction)
import PythonHS.VM.ExecuteAssertInstructionConfig (ExecuteAssertInstructionConfig (..))
import PythonHS.VM.ExecuteCallFunction (executeCallFunction)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.ExecuteCheckWithResult (executeCheckWithResult)
import PythonHS.VM.ExecuteCheckWithResultConfig (ExecuteCheckWithResultConfig (..))
import PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction)
import PythonHS.VM.ExecuteDeleteName (executeDeleteName)
import PythonHS.VM.ExecuteForNext (executeForNext)
import PythonHS.VM.ExecuteJumpIfFalse (executeJumpIfFalse)
import PythonHS.VM.ExecuteJumpIfFalseConfig (ExecuteJumpIfFalseConfig (..))
import PythonHS.VM.ExecuteListComprehension (executeListComprehension)
import PythonHS.VM.ExecuteLoopGuard (executeLoopGuard)
import PythonHS.VM.ExecuteLoopGuardConfig (ExecuteLoopGuardConfig (..))
import PythonHS.VM.ExecuteMatchPattern (executeMatchPattern)
import PythonHS.VM.ExecutePushWithHandler (executePushWithHandler)
import PythonHS.VM.ExecutePushWithHandlerConfig (ExecutePushWithHandlerConfig (..))
import PythonHS.VM.ExecuteStackInstruction (executeStackInstruction)
import PythonHS.VM.ExecuteStackInstructionConfig (ExecuteStackInstructionConfig (..))
import PythonHS.VM.ExecuteStoreClassAttr (executeStoreClassAttr)
import PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames)
import PythonHS.VM.FunctionInfo (FunctionInfo (..))
import PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction)
import PythonHS.VM.HandleExceptionInstructionConfig (HandleExceptionInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.PopValues (popValues)
import PythonHS.VM.PopValuesConfig (PopValuesConfig (..))
import PythonHS.VM.PopValuesResult (PopValuesResult (..))
import PythonHS.VM.RaiseTopError (raiseTopError)
import PythonHS.VM.RaiseTopErrorConfig (RaiseTopErrorConfig (..))
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))
import PythonHS.VM.ToPairs (toPairs)
import PythonHS.VM.VMScopeContext (VMScopeContext (VMScopeContext))
import PythonHS.VM.VMState (VMState (..))

executeOneInstruction :: (VMState -> Either String VMState) -> VMState -> Instruction -> Either String VMState
executeOneInstruction execute state instruction =
  case executeArithmeticInstruction ExecuteArithmeticInstructionConfig {executeArithmeticInstructionState = state, executeArithmeticInstructionInstruction = instruction} of
    Just result -> result
    Nothing ->
      let scopeCtx = VMScopeContext (vmIsTopLevel state) (envGlobalDecls (vmEnv state))
       in case instruction of
            PushConst value -> Right state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
            LoadName name pos ->
              case lookupNameWithAttr name pos (envLocals (vmEnv state)) (envGlobals (vmEnv state)) of
                Left attrError -> Left attrError
                Right Nothing ->
                  case Map.lookup name (envFunctions (vmEnv state)) of
                    Just _ -> Right state {vmIp = vmIp state + 1, vmStack = FunctionRefValue name [] : vmStack state}
                    Nothing -> Left ("Name error: undefined identifier " ++ name ++ " at " ++ showPos pos)
                Right (Just value) ->
                  case value of
                    UnboundLocalValue {unboundLocalValueName = unboundName} ->
                      Left ("UnboundLocalError: local variable " ++ unboundName ++ " referenced before assignment at " ++ showPos pos)
                    _ -> Right state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
            DeclareGlobal name ->
              Right state {vmIp = vmIp state + 1, vmEnv = (vmEnv state) {envGlobalDecls = Set.insert name (envGlobalDecls (vmEnv state))}}
            StoreName name ->
              case vmStack state of
                value : rest ->
                  case storeNameWithAttr scopeCtx name value (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                    Left err -> Left err
                    Right (newGlobals, newLocals) ->
                      let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
                       in Right state {vmIp = vmIp state + 1, vmStack = rest, vmEnv = newEnv}
                _ -> Left "VM runtime error: store requires one value on stack"
            DeleteName target -> executeDeleteName state (DeleteName target)
            BuildList count ->
              case popValues PopValuesConfig {popValuesCount = count, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  Right state {vmIp = vmIp state + 1, vmStack = ListValue (popValuesResultPopped popResult) : popValuesResultRest popResult}
            BuildTuple count ->
              case popValues PopValuesConfig {popValuesCount = count, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  Right state {vmIp = vmIp state + 1, vmStack = TupleValue (popValuesResultPopped popResult) : popValuesResultRest popResult}
            BuildDict count ->
              case popValues PopValuesConfig {popValuesCount = count * 2, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  case toPairs (popValuesResultPopped popResult) of
                    Left err -> Left err
                    Right pairs -> Right state {vmIp = vmIp state + 1, vmStack = DictValue pairs : popValuesResultRest popResult}
            MatchPattern pat _ -> do
              (newStack, newGlobals, newLocals) <-
                executeMatchPattern scopeCtx pat (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state))
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
              Right state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv}
            Jump target -> Right state {vmIp = target}
            JumpIfFalse target ->
              executeJumpIfFalse ExecuteJumpIfFalseConfig {executeJumpIfFalseState = state, executeJumpIfFalseTarget = target}
            LoopGuard pos ->
              executeLoopGuard ExecuteLoopGuardConfig {executeLoopGuardState = state, executeLoopGuardPos = pos}
            ForSetup forNextIndex pos ->
              case vmStack state of
                iterableValue : rest -> do
                  iterableValues <- toForIterable ToForIterableConfig {toForIterableValue = iterableValue, toForIterablePos = pos}
                  Right state {vmIp = vmIp state + 1, vmStack = rest, vmLoop = (vmLoop state) {loopForStates = Map.insert forNextIndex iterableValues (loopForStates (vmLoop state))}}
                _ -> Left "VM runtime error: for setup requires iterable value on stack"
            ForNext name loopEndIndex _ ->
              case executeForNext scopeCtx (vmIp state) name loopEndIndex (loopForStates (vmLoop state)) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                Left err -> Left err
                Right (nextIp, newForStates, newGlobals, newLocals) ->
                  let newLoop = (vmLoop state) {loopForStates = newForStates}
                      newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
                   in Right state {vmIp = nextIp, vmLoop = newLoop, vmEnv = newEnv}
            PushWithHandler handlerIp -> executePushWithHandler ExecutePushWithHandlerConfig {executePushWithHandlerState = state, executePushWithHandlerIp = handlerIp}
            CheckWithResult -> executeCheckWithResult ExecuteCheckWithResultConfig {executeCheckWithResultState = state}
            DupTop ->
              case vmStack state of
                value : rest -> Right state {vmIp = vmIp state + 1, vmStack = value : value : rest}
                _ -> Left "VM runtime error: dup requires one value on stack"
            DefineFunction name params defaultCodes functionCode ->
              Right state {vmIp = vmIp state + 1, vmEnv = (vmEnv state) {envFunctions = Map.insert name FunctionInfo {functionInfoParams = params, functionInfoDefaults = defaultCodes, functionInfoCode = functionCode} (envFunctions (vmEnv state))}}
            CreateLambda name params defaultCodes functionCode ->
              let captured = Map.toList (envLocals (vmEnv state))
               in Right
                    state
                      { vmIp = vmIp state + 1,
                        vmStack = FunctionRefValue name captured : vmStack state,
                        vmEnv = (vmEnv state) {envFunctions = Map.insert name FunctionInfo {functionInfoParams = params, functionInfoDefaults = defaultCodes, functionInfoCode = functionCode} (envFunctions (vmEnv state))}
                      }
            DefineClass className maybeBase methods ->
              executeDefineClassInstruction state (DefineClass className maybeBase methods)
            StoreClassAttr className attrName ->
              executeStoreClassAttr state (StoreClassAttr className attrName)
            CallFunction fname compiledArgs pos -> do
              (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
                executeCallFunction execute (vmIsTopLevel state) fname compiledArgs pos (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocalEnv, envFunctions = newFunctions}
              Right state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv, vmOutputs = newOutputs}
            CallValueFunction compiledArgs pos -> do
              (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
                executeCallValueFunction execute (vmIsTopLevel state) compiledArgs pos (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocalEnv, envFunctions = newFunctions}
              Right state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv, vmOutputs = newOutputs}
            UnpackToNames names pos ->
              case vmStack state of
                value : rest ->
                  case executeUnpackToNames scopeCtx pos names value (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                    Left err -> Left err
                    Right (newGlobals, newLocals) ->
                      Right state {vmIp = vmIp state + 1, vmStack = rest, vmEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}}
                _ -> Left "VM runtime error: unpack requires one value on stack"
            BuildListComprehension clauses valueCode pos -> do
              (listValue, newGlobals, newFunctions, newOutputs) <-
                executeListComprehension execute clauses valueCode pos (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              Right
                state
                  { vmIp = vmIp state + 1,
                    vmStack = listValue : vmStack state,
                    vmEnv = (vmEnv state) {envGlobals = newGlobals, envFunctions = newFunctions},
                    vmOutputs = newOutputs
                  }
            RaiseTop pos ->
              case vmStack state of
                value : rest ->
                  let err = raiseTopError RaiseTopErrorConfig {raiseTopErrorValue = value, raiseTopErrorPos = pos}
                      newEnv = (vmEnv state) {envLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) (envLocals (vmEnv state))}
                   in case exceptionHandlers (vmException state) of
                        handlerIp : restHandlers ->
                          Right state {vmIp = handlerIp, vmStack = rest, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
                        [] -> Left err
                _ -> Left "VM runtime error: raise requires one value on stack"
            ReturnTop ->
              case vmStack state of
                value : _ -> Right state {vmStack = [value]}
                _ -> Left "VM runtime error: return requires one value on stack"
            PrintTop -> executeStackInstruction ExecuteStackInstructionConfig {executeStackInstructionState = state, executeStackInstructionInstruction = PrintTop}
            PopTop -> executeStackInstruction ExecuteStackInstructionConfig {executeStackInstructionState = state, executeStackInstructionInstruction = PopTop}
            AssertTop pos ->
              executeAssertInstruction ExecuteAssertInstructionConfig {executeAssertInstructionState = state, executeAssertInstructionInstruction = AssertTop pos}
            Halt -> Right state
            instruction'@(PushExceptionHandler _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@(PushFinallyHandler _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@PopExceptionHandler -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@LoadPendingException -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@(MatchExceptionType _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@RaisePendingException -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@RaisePendingError -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            _ -> Left "VM runtime error: unknown instruction"
