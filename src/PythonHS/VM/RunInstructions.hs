module PythonHS.VM.RunInstructions (runInstructions) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.FirstKeywordArg (firstKeywordArg)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, BuildDict, BuildList, CallFunction, DeclareGlobal, DefineFunction, ForNext, ForSetup, Halt, Jump, JumpIfFalse, LoadName, LoopGuard, PrintTop, PushConst, ReturnTop, StoreName))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToPairs (toPairs)

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  (_, _, _, outputs) <- execute instructions 0 [] Map.empty Map.empty Map.empty Set.empty Map.empty Map.empty [] True
  pure outputs
  where
    execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
      | ip < 0 || ip >= length code = Right (Nothing, globalsEnv, functions, outputs)
      | otherwise =
          case code !! ip of
            PushConst value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
            LoadName name pos ->
              case lookupName name localEnv globalsEnv of
                Just value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                Nothing -> Left ("Name error: undefined variable " ++ name ++ " at " ++ showPos pos)
            DeclareGlobal name ->
              let newGlobalDecls = Set.insert name globalDecls
               in execute code (ip + 1) stack globalsEnv localEnv functions newGlobalDecls forStates loopCounts outputs isTopLevel
            StoreName name ->
              case stack of
                value : rest ->
                  if isTopLevel || Set.member name globalDecls
                    then
                      let newGlobals = Map.insert name value globalsEnv
                          newLocals = if isTopLevel then newGlobals else localEnv
                       in execute code (ip + 1) rest newGlobals newLocals functions globalDecls forStates loopCounts outputs isTopLevel
                    else
                      let newLocals = Map.insert name value localEnv
                       in execute code (ip + 1) rest globalsEnv newLocals functions globalDecls forStates loopCounts outputs isTopLevel
                _ -> Left "VM runtime error: store requires one value on stack"
            BuildList count ->
              case popValues count stack of
                Left err -> Left err
                Right (values, rest) ->
                  execute code (ip + 1) (ListValue values : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
            BuildDict count ->
              case popValues (count * 2) stack of
                Left err -> Left err
                Right (flatValues, rest) ->
                  case toPairs flatValues of
                    Left err -> Left err
                    Right pairs -> execute code (ip + 1) (DictValue pairs : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
            Jump target -> execute code target stack globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
            JumpIfFalse target ->
              case stack of
                value : rest ->
                  if isTruthy value
                    then execute code (ip + 1) rest globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                    else execute code target rest globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                _ -> Left "VM runtime error: conditional jump requires one value on stack"
            LoopGuard pos ->
              let currentCount = Map.findWithDefault 0 ip loopCounts
               in if currentCount >= maxLoopIterations
                    then Left ("Value error: iteration limit exceeded at " ++ showPos pos)
                    else
                      let newCounts = Map.insert ip (currentCount + 1) loopCounts
                       in execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates newCounts outputs isTopLevel
            ForSetup forNextIndex pos ->
              case stack of
                iterableValue : rest -> do
                  iterableValues <- toForIterable iterableValue pos
                  let newForStates = Map.insert forNextIndex iterableValues forStates
                  execute code (ip + 1) rest globalsEnv localEnv functions globalDecls newForStates loopCounts outputs isTopLevel
                _ -> Left "VM runtime error: for setup requires iterable value on stack"
            ForNext name loopEndIndex _ ->
              case Map.lookup ip forStates of
                Nothing -> Left "VM runtime error: missing for-loop state"
                Just [] ->
                  let newForStates = Map.delete ip forStates
                   in execute code loopEndIndex stack globalsEnv localEnv functions globalDecls newForStates loopCounts outputs isTopLevel
                Just (nextValue : remainingValues) ->
                  if isTopLevel || Set.member name globalDecls
                    then
                      let newGlobals = Map.insert name nextValue globalsEnv
                          newLocals = if isTopLevel then newGlobals else localEnv
                          newForStates = Map.insert ip remainingValues forStates
                       in execute code (ip + 1) stack newGlobals newLocals functions globalDecls newForStates loopCounts outputs isTopLevel
                    else
                      let newLocals = Map.insert name nextValue localEnv
                          newForStates = Map.insert ip remainingValues forStates
                       in execute code (ip + 1) stack globalsEnv newLocals functions globalDecls newForStates loopCounts outputs isTopLevel
            DefineFunction name params defaultCodes functionCode ->
              let newFunctions = Map.insert name (params, defaultCodes, functionCode) functions
               in execute code (ip + 1) stack globalsEnv localEnv newFunctions globalDecls forStates loopCounts outputs isTopLevel
            CallFunction fname argKinds pos ->
              case popArgs (length argKinds) stack of
                Left err -> Left err
                Right (args, rest) ->
                  case Map.lookup fname functions of
                    Nothing ->
                      case firstKeywordArg argKinds of
                        Just (_, argPos) -> Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
                        Nothing ->
                          case callBuiltin fname args pos of
                            Just (Left err) -> Left err
                            Just (Right builtinValue) ->
                              execute code (ip + 1) (builtinValue : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                            Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)
                    Just (params, defaultCodes, functionCode) ->
                      do
                        initialLocals <- bindCallArguments fname pos params args argKinds
                        (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
                          bindDefaults fname pos params defaultCodes initialLocals globalsEnv functions outputs
                        (maybeValue, newGlobals, newFunctions, newOutputs) <-
                          execute functionCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults Set.empty Map.empty Map.empty outputsAfterDefaults False
                        let returnValue =
                              case maybeValue of
                                Just value -> value
                                Nothing -> NoneValue
                        let newLocalEnv = if isTopLevel then newGlobals else localEnv
                        execute code (ip + 1) (returnValue : rest) newGlobals newLocalEnv newFunctions globalDecls forStates loopCounts newOutputs isTopLevel
            ApplyBinary op pos ->
              case stack of
                right : left : rest ->
                  case evalBinaryOp op left right pos of
                    Left err -> Left err
                    Right value -> execute code (ip + 1) (value : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                _ -> Left "VM runtime error: add requires two values on stack"
            ApplyUnaryMinus pos ->
              case stack of
                value : rest ->
                  case value of
                    IntValue n -> execute code (ip + 1) (IntValue (negate n) : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                    FloatValue n -> execute code (ip + 1) (FloatValue (negate n) : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                    _ -> Left ("Type error: unary - expects int at " ++ showPos pos)
                _ -> Left "VM runtime error: unary - requires one value on stack"
            ApplyNot _ ->
              case stack of
                value : rest ->
                  let result = if isTruthy value then 0 else 1
                   in execute code (ip + 1) (IntValue result : rest) globalsEnv localEnv functions globalDecls forStates loopCounts outputs isTopLevel
                _ -> Left "VM runtime error: not requires one value on stack"
            ReturnTop ->
              case stack of
                value : _ -> Right (Just value, globalsEnv, functions, outputs)
                _ -> Left "VM runtime error: return requires one value on stack"
            PrintTop ->
              case stack of
                value : rest -> execute code (ip + 1) rest globalsEnv localEnv functions globalDecls forStates loopCounts (outputs ++ [valueToOutput value]) isTopLevel
                _ -> Left "VM runtime error: print requires one value on stack"
            Halt -> Right (Nothing, globalsEnv, functions, outputs)

    lookupName name localEnv globalsEnv =
      case Map.lookup name localEnv of
        Just value -> Just value
        Nothing -> Map.lookup name globalsEnv

    popArgs argCount stack =
      let (popped, rest) = splitAt argCount stack
       in if length popped /= argCount
            then Left "VM runtime error: call requires enough argument values on stack"
            else Right (reverse popped, rest)

    popValues count stack =
      let (popped, rest) = splitAt count stack
       in if length popped /= count
            then Left "VM runtime error: collection build requires enough values on stack"
            else Right (reverse popped, rest)

    bindDefaults fname pos params defaultCodes initialLocals globalsNow functionsNow outputsNow =
      fill params initialLocals globalsNow functionsNow outputsNow
      where
        defaultMap = Map.fromList defaultCodes

        fill remainingParams currentLocals currentGlobals currentFunctions currentOutputs =
          case remainingParams of
            [] -> Right (currentLocals, currentGlobals, currentFunctions, currentOutputs)
            paramName : restParams ->
              case Map.lookup paramName currentLocals of
                Just _ -> fill restParams currentLocals currentGlobals currentFunctions currentOutputs
                Nothing ->
                  case Map.lookup paramName defaultMap of
                    Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)
                    Just defaultCode -> do
                      (maybeDefaultValue, globalsAfterDefault, functionsAfterDefault, outputsAfterDefault) <-
                        execute defaultCode 0 [] currentGlobals currentLocals currentFunctions Set.empty Map.empty Map.empty currentOutputs False
                      let defaultValue =
                            case maybeDefaultValue of
                              Just value -> value
                              Nothing -> NoneValue
                      let newLocals = Map.insert paramName defaultValue currentLocals
                      fill restParams newLocals globalsAfterDefault functionsAfterDefault outputsAfterDefault
