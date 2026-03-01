module PythonHS.VM.RunInstructions (runInstructions) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs)
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.FirstKeywordArg (firstKeywordArg)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, CallFunction, DeclareGlobal, DefineFunction, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoadName, LoopGuard, PopExceptionHandler, PrintTop, PushConst, PushExceptionHandler, RaiseTop, ReturnTop, StoreName))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.PopValues (popValues)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToPairs (toPairs)

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  (_, _, _, outputs) <- execute instructions 0 [] Map.empty Map.empty Map.empty Set.empty Map.empty Map.empty [] [] True
  pure outputs
  where
    execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
      | ip < 0 || ip >= length code = Right (Nothing, globalsEnv, functions, outputs)
      | otherwise =
          handleRuntimeError code stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel $
            case code !! ip of
            PushConst value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            LoadName name pos ->
              case lookupName name localEnv globalsEnv of
                Just value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                Nothing -> Left ("Name error: undefined identifier " ++ name ++ " at " ++ showPos pos)
            DeclareGlobal name ->
              let newGlobalDecls = Set.insert name globalDecls
               in execute code (ip + 1) stack globalsEnv localEnv functions newGlobalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            StoreName name ->
              case stack of
                value : rest ->
                  if isTopLevel || Set.member name globalDecls
                    then
                      let newGlobals = Map.insert name value globalsEnv
                          newLocals = if isTopLevel then newGlobals else localEnv
                       in execute code (ip + 1) rest newGlobals newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                    else
                      let newLocals = Map.insert name value localEnv
                       in execute code (ip + 1) rest globalsEnv newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                _ -> Left "VM runtime error: store requires one value on stack"
            BuildList count ->
              case popValues count stack of
                Left err -> Left err
                Right (values, rest) ->
                  execute code (ip + 1) (ListValue values : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            BuildDict count ->
              case popValues (count * 2) stack of
                Left err -> Left err
                Right (flatValues, rest) ->
                  case toPairs flatValues of
                    Left err -> Left err
                    Right pairs -> execute code (ip + 1) (DictValue pairs : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            Jump target -> execute code target stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            JumpIfFalse target ->
              case stack of
                value : rest ->
                  if isTruthy value
                    then execute code (ip + 1) rest globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                    else execute code target rest globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                _ -> Left "VM runtime error: conditional jump requires one value on stack"
            LoopGuard pos ->
              let currentCount = Map.findWithDefault 0 ip loopCounts
               in if currentCount >= maxLoopIterations
                    then Left ("Value error: iteration limit exceeded at " ++ showPos pos)
                    else
                      let newCounts = Map.insert ip (currentCount + 1) loopCounts
                       in execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates newCounts exceptionHandlers outputs isTopLevel
            ForSetup forNextIndex pos ->
              case stack of
                iterableValue : rest -> do
                  iterableValues <- toForIterable iterableValue pos
                  let newForStates = Map.insert forNextIndex iterableValues forStates
                  execute code (ip + 1) rest globalsEnv localEnv functions globalDecls newForStates loopCounts exceptionHandlers outputs isTopLevel
                _ -> Left "VM runtime error: for setup requires iterable value on stack"
            ForNext name loopEndIndex _ ->
              case Map.lookup ip forStates of
                Nothing -> Left "VM runtime error: missing for-loop state"
                Just [] ->
                  let newForStates = Map.delete ip forStates
                   in execute code loopEndIndex stack globalsEnv localEnv functions globalDecls newForStates loopCounts exceptionHandlers outputs isTopLevel
                Just (nextValue : remainingValues) ->
                  if isTopLevel || Set.member name globalDecls
                    then
                      let newGlobals = Map.insert name nextValue globalsEnv
                          newLocals = if isTopLevel then newGlobals else localEnv
                          newForStates = Map.insert ip remainingValues forStates
                       in execute code (ip + 1) stack newGlobals newLocals functions globalDecls newForStates loopCounts exceptionHandlers outputs isTopLevel
                    else
                      let newLocals = Map.insert name nextValue localEnv
                          newForStates = Map.insert ip remainingValues forStates
                       in execute code (ip + 1) stack globalsEnv newLocals functions globalDecls newForStates loopCounts exceptionHandlers outputs isTopLevel
            PushExceptionHandler handlerIp ->
              execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates loopCounts (handlerIp : exceptionHandlers) outputs isTopLevel
            PopExceptionHandler ->
              case exceptionHandlers of
                _ : restHandlers -> execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
                [] -> execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates loopCounts [] outputs isTopLevel
            DefineFunction name params defaultCodes functionCode ->
              let newFunctions = Map.insert name (params, defaultCodes, functionCode) functions
               in execute code (ip + 1) stack globalsEnv localEnv newFunctions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            CallFunction fname compiledArgs pos ->
              case Map.lookup fname functions of
                Nothing ->
                  case firstKeywordArg compiledArgs of
                    Just (_, argPos) -> Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
                    Nothing -> do
                      (args, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
                        evaluateBuiltinArgs execute localEnv compiledArgs globalsEnv functions outputs []
                      case callBuiltin fname args pos of
                        Just (Left err) -> Left err
                        Just (Right builtinValue) ->
                          let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
                           in execute code (ip + 1) (builtinValue : stack) globalsAfterArgs newLocalEnv functionsAfterArgs globalDecls forStates loopCounts exceptionHandlers outputsAfterArgs isTopLevel
                        Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)
                Just (params, defaultCodes, functionCode) ->
                  do
                    (argValues, argKinds, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
                      evaluateUserArgs execute fname pos localEnv compiledArgs globalsEnv functions outputs False Set.empty [] []
                    initialLocals <- bindCallArguments fname pos params argValues argKinds
                    (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
                      bindDefaults execute fname pos params defaultCodes initialLocals globalsAfterArgs functionsAfterArgs outputsAfterArgs
                    let functionGlobalDecls = collectFunctionGlobalDecls functionCode
                    (maybeValue, newGlobals, newFunctions, newOutputs) <-
                      execute functionCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults functionGlobalDecls Map.empty Map.empty [] outputsAfterDefaults False
                    let returnValue =
                          case maybeValue of
                            Just value -> value
                            Nothing -> IntValue 0
                    let newLocalEnv = if isTopLevel then newGlobals else localEnv
                    execute code (ip + 1) (returnValue : stack) newGlobals newLocalEnv newFunctions globalDecls forStates loopCounts exceptionHandlers newOutputs isTopLevel
            ApplyBinary op pos ->
              case stack of
                right : left : rest ->
                  case evalBinaryOp op left right pos of
                    Left err -> Left err
                    Right value -> execute code (ip + 1) (value : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                _ -> Left "VM runtime error: add requires two values on stack"
            ApplyUnaryMinus pos ->
              case stack of
                value : rest ->
                  case value of
                    IntValue n -> execute code (ip + 1) (IntValue (negate n) : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                    FloatValue n -> execute code (ip + 1) (FloatValue (negate n) : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                    _ -> Left ("Type error: unary - expects int at " ++ showPos pos)
                _ -> Left "VM runtime error: unary - requires one value on stack"
            ApplyNot _ ->
              case stack of
                value : rest ->
                  let result = if isTruthy value then 0 else 1
                   in execute code (ip + 1) (IntValue result : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
                _ -> Left "VM runtime error: not requires one value on stack"
            RaiseTop pos ->
              case stack of
                value : rest ->
                  let err = "Runtime error: " ++ valueToOutput value ++ " at " ++ showPos pos
                   in case exceptionHandlers of
                        handlerIp : restHandlers -> execute code handlerIp rest globalsEnv localEnv functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
                        [] -> Left err
                _ -> Left "VM runtime error: raise requires one value on stack"
            ReturnTop ->
              case stack of
                value : _ -> Right (Just value, globalsEnv, functions, outputs)
                _ -> Left "VM runtime error: return requires one value on stack"
            PrintTop ->
              case stack of
                value : rest -> execute code (ip + 1) rest globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers (outputs ++ [valueToOutput value]) isTopLevel
                _ -> Left "VM runtime error: print requires one value on stack"
            Halt -> Right (Nothing, globalsEnv, functions, outputs)

    handleRuntimeError code stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel result =
      case result of
        Right value -> Right value
        Left err ->
          case exceptionHandlers of
            handlerIp : restHandlers -> execute code handlerIp stack globalsEnv localEnv functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
            [] -> Left err
