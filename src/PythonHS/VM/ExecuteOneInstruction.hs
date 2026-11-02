module PythonHS.VM.ExecuteOneInstruction (executeOneInstruction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FunctionRefValue, ListValue, StringValue, TupleValue, UnboundLocalValue), unboundLocalValueName)
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteArithmeticInstruction (executeArithmeticInstruction)
import PythonHS.VM.ExecuteArithmeticInstructionConfig (ExecuteArithmeticInstructionConfig (..))
import PythonHS.VM.ExecuteCallFunction (executeCallFunction)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.ExecuteCheckWithResult (executeCheckWithResult)
import PythonHS.VM.ExecuteCheckWithResultConfig (ExecuteCheckWithResultConfig (..))
import PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction)
import PythonHS.VM.ExecuteDeleteName (executeDeleteName)
import PythonHS.VM.ExecuteForNext (executeForNext)
import PythonHS.VM.ExecuteListComprehension (executeListComprehension)
import PythonHS.VM.ExecuteMatchPattern (executeMatchPattern)
import PythonHS.VM.ExecuteStackInstruction (executeStackInstruction)
import PythonHS.VM.ExecuteStackInstructionConfig (ExecuteStackInstructionConfig (..))
import PythonHS.VM.ExecuteStoreClassAttr (executeStoreClassAttr)
import PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames)
import PythonHS.VM.FunctionInfo (FunctionInfo (..))
import PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction)
import PythonHS.VM.HandleExceptionInstructionConfig (HandleExceptionInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.PopValues (popValues)
import PythonHS.VM.PopValuesConfig (PopValuesConfig (..))
import PythonHS.VM.PopValuesResult (PopValuesResult (..))
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))
import PythonHS.VM.ToPairs (toPairs)
import PythonHS.VM.VMScopeContext (VMScopeContext (VMScopeContext))
import PythonHS.VM.VMState (VMState (..))

executeOneInstruction :: (VMState -> Either String VMState) -> VMState -> Instruction -> Either String VMState
executeOneInstruction execute state instruction =
  case executeArithmeticInstruction ExecuteArithmeticInstructionConfig {executeArithmeticInstructionExecute = execute, executeArithmeticInstructionState = state, executeArithmeticInstructionInstruction = instruction} of
    Just result -> result
    Nothing ->
      let scopeCtx = VMScopeContext (vmIsTopLevel state) (envGlobalDecls (vmEnv state))
       in case instruction of
            PushConst value -> execute state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
            LoadName name pos ->
              case lookupNameWithAttr name (envLocals (vmEnv state)) (envGlobals (vmEnv state)) of
                Just value ->
                  case value of
                    UnboundLocalValue {unboundLocalValueName = unboundName} ->
                      Left ("UnboundLocalError: local variable " ++ unboundName ++ " referenced before assignment at " ++ showPos pos)
                    _ -> execute state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
                Nothing ->
                  case Map.lookup name (envFunctions (vmEnv state)) of
                    Just _ -> execute state {vmIp = vmIp state + 1, vmStack = FunctionRefValue name [] : vmStack state}
                    Nothing -> Left ("Name error: undefined identifier " ++ name ++ " at " ++ showPos pos)
            DeclareGlobal name ->
              execute state {vmIp = vmIp state + 1, vmEnv = (vmEnv state) {envGlobalDecls = Set.insert name (envGlobalDecls (vmEnv state))}}
            StoreName name ->
              case vmStack state of
                value : rest ->
                  case storeNameWithAttr scopeCtx name value (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                    Left err -> Left err
                    Right (newGlobals, newLocals) ->
                      let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
                       in execute state {vmIp = vmIp state + 1, vmStack = rest, vmEnv = newEnv}
                _ -> Left "VM runtime error: store requires one value on stack"
            DeleteName target ->
              executeDeleteName execute state (DeleteName target)
            BuildList count ->
              case popValues PopValuesConfig {popValuesCount = count, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  execute state {vmIp = vmIp state + 1, vmStack = ListValue (popValuesResultPopped popResult) : popValuesResultRest popResult}
            BuildTuple count ->
              case popValues PopValuesConfig {popValuesCount = count, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  execute state {vmIp = vmIp state + 1, vmStack = TupleValue (popValuesResultPopped popResult) : popValuesResultRest popResult}
            BuildDict count ->
              case popValues PopValuesConfig {popValuesCount = count * 2, popValuesStack = vmStack state} of
                Left err -> Left err
                Right popResult ->
                  case toPairs (popValuesResultPopped popResult) of
                    Left err -> Left err
                    Right pairs -> execute state {vmIp = vmIp state + 1, vmStack = DictValue pairs : popValuesResultRest popResult}
            MatchPattern pat _ -> do
              (newStack, newGlobals, newLocals) <-
                executeMatchPattern scopeCtx pat (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state))
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
              execute state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv}
            Jump target -> execute state {vmIp = target}
            JumpIfFalse target ->
              case vmStack state of
                value : rest ->
                  if isTruthy value
                    then execute state {vmIp = vmIp state + 1, vmStack = rest}
                    else execute state {vmIp = target, vmStack = rest}
                _ -> Left "VM runtime error: conditional jump requires one value on stack"
            LoopGuard pos ->
              let currentCount = Map.findWithDefault 0 (vmIp state) (loopCounts (vmLoop state))
               in if currentCount >= maxLoopIterations
                    then Left ("Value error: iteration limit exceeded at " ++ showPos pos)
                    else execute state {vmIp = vmIp state + 1, vmLoop = (vmLoop state) {loopCounts = Map.insert (vmIp state) (currentCount + 1) (loopCounts (vmLoop state))}}
            ForSetup forNextIndex pos ->
              case vmStack state of
                iterableValue : rest -> do
                  iterableValues <- toForIterable ToForIterableConfig {toForIterableValue = iterableValue, toForIterablePos = pos}
                  execute state {vmIp = vmIp state + 1, vmStack = rest, vmLoop = (vmLoop state) {loopForStates = Map.insert forNextIndex iterableValues (loopForStates (vmLoop state))}}
                _ -> Left "VM runtime error: for setup requires iterable value on stack"
            ForNext name loopEndIndex _ ->
              case executeForNext scopeCtx (vmIp state) name loopEndIndex (loopForStates (vmLoop state)) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                Left err -> Left err
                Right (nextIp, newForStates, newGlobals, newLocals) ->
                  let newLoop = (vmLoop state) {loopForStates = newForStates}
                      newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}
                   in execute state {vmIp = nextIp, vmLoop = newLoop, vmEnv = newEnv}
            PushWithHandler handlerIp ->
              if handlerIp < 0 || handlerIp >= length (vmCode state)
                then Left ("Runtime error: invalid handler IP " ++ show handlerIp ++ " for PushWithHandler")
                else execute state {vmIp = vmIp state + 1, vmException = (vmException state) {exceptionHandlers = handlerIp : exceptionHandlers (vmException state)}}
            CheckWithResult -> executeCheckWithResult ExecuteCheckWithResultConfig {executeCheckWithResultState = state, executeCheckWithResultExecute = execute}
            DupTop ->
              case vmStack state of
                value : rest -> execute state {vmIp = vmIp state + 1, vmStack = value : value : rest}
                _ -> Left "VM runtime error: dup requires one value on stack"
            DefineFunction name params defaultCodes functionCode ->
              execute state {vmIp = vmIp state + 1, vmEnv = (vmEnv state) {envFunctions = Map.insert name FunctionInfo {functionInfoParams = params, functionInfoDefaults = defaultCodes, functionInfoCode = functionCode} (envFunctions (vmEnv state))}}
            CreateLambda name params defaultCodes functionCode ->
              let captured = Map.toList (envLocals (vmEnv state))
               in execute
                    state
                      { vmIp = vmIp state + 1,
                        vmStack = FunctionRefValue name captured : vmStack state,
                        vmEnv = (vmEnv state) {envFunctions = Map.insert name FunctionInfo {functionInfoParams = params, functionInfoDefaults = defaultCodes, functionInfoCode = functionCode} (envFunctions (vmEnv state))}
                      }
            DefineClass className maybeBase methods ->
              executeDefineClassInstruction execute state (DefineClass className maybeBase methods)
            StoreClassAttr className attrName ->
              executeStoreClassAttr execute state (StoreClassAttr className attrName)
            CallFunction fname compiledArgs pos -> do
              (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
                executeCallFunction execute (vmIsTopLevel state) fname compiledArgs pos (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocalEnv, envFunctions = newFunctions}
              execute state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv, vmOutputs = newOutputs}
            CallValueFunction compiledArgs pos -> do
              (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
                executeCallValueFunction execute (vmIsTopLevel state) compiledArgs pos (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              let newEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocalEnv, envFunctions = newFunctions}
              execute state {vmIp = vmIp state + 1, vmStack = newStack, vmEnv = newEnv, vmOutputs = newOutputs}
            UnpackToNames names pos ->
              case vmStack state of
                value : rest ->
                  case executeUnpackToNames scopeCtx pos names value (envGlobals (vmEnv state)) (envLocals (vmEnv state)) of
                    Left err -> Left err
                    Right (newGlobals, newLocals) ->
                      execute state {vmIp = vmIp state + 1, vmStack = rest, vmEnv = (vmEnv state) {envGlobals = newGlobals, envLocals = newLocals}}
                _ -> Left "VM runtime error: unpack requires one value on stack"
            BuildListComprehension clauses valueCode pos -> do
              (listValue, newGlobals, newFunctions, newOutputs) <-
                executeListComprehension execute clauses valueCode pos (envGlobals (vmEnv state)) (envLocals (vmEnv state)) (envFunctions (vmEnv state)) (vmOutputs state)
              execute
                state
                  { vmIp = vmIp state + 1,
                    vmStack = listValue : vmStack state,
                    vmEnv = (vmEnv state) {envGlobals = newGlobals, envFunctions = newFunctions},
                    vmOutputs = newOutputs
                  }
            RaiseTop pos ->
              case vmStack state of
                value : rest ->
                  let err = "Runtime error: " ++ valueToOutput value ++ " at " ++ showPos pos
                      newEnv = (vmEnv state) {envLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) (envLocals (vmEnv state))}
                   in case exceptionHandlers (vmException state) of
                        handlerIp : restHandlers ->
                          execute state {vmIp = handlerIp, vmStack = rest, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
                        [] -> Left err
                _ -> Left "VM runtime error: raise requires one value on stack"
            ReturnTop ->
              case vmStack state of
                value : _ -> Right state {vmStack = [value]}
                _ -> Left "VM runtime error: return requires one value on stack"
            instruction'@(PrintTop) ->
              executeStackInstruction ExecuteStackInstructionConfig {executeStackInstructionExecute = execute, executeStackInstructionState = state, executeStackInstructionInstruction = instruction'}
            instruction'@(PopTop) ->
              executeStackInstruction ExecuteStackInstructionConfig {executeStackInstructionExecute = execute, executeStackInstructionState = state, executeStackInstructionInstruction = instruction'}
            Halt -> Right state
            instruction'@(PushExceptionHandler _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@(PushFinallyHandler _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@PopExceptionHandler -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@LoadPendingException -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@(MatchExceptionType _) -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@RaisePendingException -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            instruction'@RaisePendingError -> handleExceptionInstruction HandleExceptionInstructionConfig {handleExceptionInstructionExecute = execute, handleExceptionInstructionState = state, handleExceptionInstructionInstruction = instruction'}
            _ -> Left "VM runtime error: unknown instruction"
