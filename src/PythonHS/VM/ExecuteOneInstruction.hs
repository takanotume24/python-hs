module PythonHS.VM.ExecuteOneInstruction (executeOneInstruction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FunctionRefValue, ListValue, StringValue, TupleValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.ExecuteArithmeticInstruction (executeArithmeticInstruction)
import PythonHS.VM.ExecuteCallFunction (executeCallFunction)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction)
import PythonHS.VM.ExecuteForNext (executeForNext)
import PythonHS.VM.ExecuteListComprehension (executeListComprehension)
import PythonHS.VM.ExecuteMatchPattern (executeMatchPattern)
import PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames)
import PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction)
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr)
import PythonHS.VM.PopValues (popValues)
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToPairs (toPairs)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))
import PythonHS.VM.VMScopeContext (VMScopeContext (VMScopeContext))

executeOneInstruction :: (VMState -> Either String VMState) -> VMState -> Instruction -> Either String VMState
executeOneInstruction execute state instruction =
  case executeArithmeticInstruction execute state instruction of
    Just result -> result
    Nothing ->
      let scopeCtx = VMScopeContext (vmIsTopLevel state) (envGlobalDecls (vmEnv state))
       in case instruction of
            PushConst value -> execute state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
            LoadName name pos ->
              case lookupNameWithAttr name (envLocals (vmEnv state)) (envGlobals (vmEnv state)) of
                Just value -> execute state {vmIp = vmIp state + 1, vmStack = value : vmStack state}
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
            BuildList count ->
              case popValues count (vmStack state) of
                Left err -> Left err
                Right (values, rest) ->
                  execute state {vmIp = vmIp state + 1, vmStack = ListValue values : rest}
            BuildTuple count ->
              case popValues count (vmStack state) of
                Left err -> Left err
                Right (values, rest) ->
                  execute state {vmIp = vmIp state + 1, vmStack = TupleValue values : rest}
            BuildDict count ->
              case popValues (count * 2) (vmStack state) of
                Left err -> Left err
                Right (flatValues, rest) ->
                  case toPairs flatValues of
                    Left err -> Left err
                    Right pairs -> execute state {vmIp = vmIp state + 1, vmStack = DictValue pairs : rest}
            MatchPattern pattern _ -> do
              (newStack, newGlobals, newLocals) <-
                executeMatchPattern scopeCtx pattern (vmStack state) (envGlobals (vmEnv state)) (envLocals (vmEnv state))
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
                  iterableValues <- toForIterable iterableValue pos
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
            CheckWithResult ->
              case vmStack state of
                resultValue : rest ->
                  if isTruthy resultValue
                    then execute state {vmIp = vmIp state + 1, vmStack = rest}
                    else
                      case exceptionHandlers (vmException state) of
                        handlerIp : restHandlers ->
                          let err = case Map.lookup "__python_hs_pending_except_error__" (envLocals (vmEnv state)) of
                                Just (StringValue s) -> s
                                _ -> "Runtime error: error at 9:3"
                              newLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) (envLocals (vmEnv state))
                              newEnv = (vmEnv state) {envLocals = newLocals}
                              newException = (vmException state) {exceptionHandlers = restHandlers}
                           in execute state {vmIp = handlerIp, vmStack = rest, vmEnv = newEnv, vmException = newException}
                        [] -> Left "Runtime error: unhandled exception in with statement"
                _ -> Left "VM runtime error: check with result requires one value on stack"
            DupTop ->
              case vmStack state of
                value : rest -> execute state {vmIp = vmIp state + 1, vmStack = value : value : rest}
                _ -> Left "VM runtime error: dup requires one value on stack"
            DefineFunction name params defaultCodes functionCode ->
              execute state {vmIp = vmIp state + 1, vmEnv = (vmEnv state) {envFunctions = Map.insert name (params, defaultCodes, functionCode) (envFunctions (vmEnv state))}}
            CreateLambda name params defaultCodes functionCode ->
              let captured = Map.toList (envLocals (vmEnv state))
               in execute state {vmIp = vmIp state + 1, vmStack = FunctionRefValue name captured : vmStack state,
                    vmEnv = (vmEnv state) {envFunctions = Map.insert name (params, defaultCodes, functionCode) (envFunctions (vmEnv state))}}
            DefineClass className maybeBase methods ->
              executeDefineClassInstruction execute state (DefineClass className maybeBase methods)
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
              execute state {vmIp = vmIp state + 1, vmStack = listValue : vmStack state,
                vmEnv = (vmEnv state) {envGlobals = newGlobals, envFunctions = newFunctions}, vmOutputs = newOutputs}
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
            PrintTop ->
              case vmStack state of
                value : rest -> execute state {vmIp = vmIp state + 1, vmStack = rest, vmOutputs = vmOutputs state ++ [valueToOutput value]}
                _ -> Left "VM runtime error: print requires one value on stack"
            Halt -> Right state
            instruction'@(PushExceptionHandler _) -> handleExceptionInstruction execute state instruction'
            instruction'@(PushFinallyHandler _) -> handleExceptionInstruction execute state instruction'
            instruction'@PopExceptionHandler -> handleExceptionInstruction execute state instruction'
            instruction'@LoadPendingException -> handleExceptionInstruction execute state instruction'
            instruction'@(MatchExceptionType _) -> handleExceptionInstruction execute state instruction'
            instruction'@RaisePendingException -> handleExceptionInstruction execute state instruction'
            instruction'@RaisePendingError -> handleExceptionInstruction execute state instruction'
