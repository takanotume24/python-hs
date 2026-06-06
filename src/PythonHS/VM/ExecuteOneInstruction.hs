module PythonHS.VM.ExecuteOneInstruction (executeOneInstruction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value(DictValue, FloatValue, FunctionRefValue, IntValue, ListValue, StringValue, TupleValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.ExecuteCallFunction (executeCallFunction)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.ExecuteDefineClassInstruction (executeDefineClassInstruction)
import PythonHS.VM.ExecuteForNext (executeForNext)
import PythonHS.VM.ExecuteListComprehension (executeListComprehension)
import PythonHS.VM.ExecuteMatchPattern (executeMatchPattern)
import PythonHS.VM.ExecuteUnpackToNames (executeUnpackToNames)
import PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction)
import PythonHS.VM.Instruction (Instruction(..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr)
import PythonHS.VM.PopValues (popValues)
import PythonHS.VM.StoreNameWithAttr (storeNameWithAttr)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToPairs (toPairs)

executeOneInstruction ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  [Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Instruction -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeOneInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction =
  case instruction of
    PushConst value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    LoadName name pos ->
      case lookupNameWithAttr name localEnv globalsEnv of
        Just value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
        Nothing ->
          case Map.lookup name functions of
            Just _ -> execute code (ip + 1) (FunctionRefValue name [] : stack) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            Nothing -> Left ("Name error: undefined identifier " ++ name ++ " at " ++ showPos pos)
    DeclareGlobal name ->
      let newGlobalDecls = Set.insert name globalDecls
       in execute code (ip + 1) stack globalsEnv localEnv functions newGlobalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    StoreName name ->
      case stack of
        value : rest ->
          case storeNameWithAttr isTopLevel globalDecls name value globalsEnv localEnv of
            Left err -> Left err
            Right (newGlobals, newLocals) ->
              execute code (ip + 1) rest newGlobals newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
        _ -> Left "VM runtime error: store requires one value on stack"
    BuildList count ->
      case popValues count stack of
        Left err -> Left err
        Right (values, rest) ->
          execute code (ip + 1) (ListValue values : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    BuildTuple count ->
      case popValues count stack of
        Left err -> Left err
        Right (values, rest) ->
          execute code (ip + 1) (TupleValue values : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    BuildDict count ->
      case popValues (count * 2) stack of
        Left err -> Left err
        Right (flatValues, rest) ->
            case toPairs flatValues of
              Left err -> Left err
              Right pairs -> execute code (ip + 1) (DictValue pairs : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    MatchPattern pattern _ -> do
      (newStack, newGlobals, newLocals) <- executeMatchPattern isTopLevel globalDecls pattern stack globalsEnv localEnv
      execute code (ip + 1) newStack newGlobals newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
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
      case executeForNext isTopLevel globalDecls ip name loopEndIndex forStates globalsEnv localEnv of
        Left err -> Left err
        Right (nextIp, newForStates, newGlobals, newLocals) ->
          execute code nextIp stack newGlobals newLocals functions globalDecls newForStates loopCounts exceptionHandlers outputs isTopLevel
    instruction'@(PushExceptionHandler _) -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    instruction'@(PushFinallyHandler _) -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    PushWithHandler handlerIp ->
      if handlerIp < 0 || handlerIp >= length code
        then Left ("Runtime error: invalid handler IP " ++ show handlerIp ++ " for PushWithHandler")
        else
          let newHandlers = handlerIp : exceptionHandlers
          in execute code (ip + 1) stack globalsEnv localEnv functions globalDecls forStates loopCounts newHandlers outputs isTopLevel
    instruction'@PopExceptionHandler ->
      if null exceptionHandlers
        then Left "Runtime error: attempting to pop from empty exception handler stack"
        else handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    instruction'@LoadPendingException -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    instruction'@(MatchExceptionType _) -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    instruction'@RaisePendingException -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    instruction'@RaisePendingError -> handleExceptionInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel instruction'
    CheckWithResult ->
      case stack of
        resultValue : rest ->
          if isTruthy resultValue
            then execute code (ip + 1) rest globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
            else
              case exceptionHandlers of
                handlerIp : restHandlers ->
                  let err = case Map.lookup "__python_hs_pending_except_error__" localEnv of
                        Just (StringValue s) -> s
                        _ -> "Runtime error: error at 9:3"
                      newLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) localEnv
                   in execute code handlerIp rest globalsEnv newLocals functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
                [] -> Left "Runtime error: unhandled exception in with statement"
        _ -> Left "VM runtime error: check with result requires one value on stack"
    DupTop ->
      case stack of
        value : rest -> execute code (ip + 1) (value : value : rest) globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
        _ -> Left "VM runtime error: dup requires one value on stack"
    DefineFunction name params defaultCodes functionCode ->
      let newFunctions = Map.insert name (params, defaultCodes, functionCode) functions
       in execute code (ip + 1) stack globalsEnv localEnv newFunctions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    CreateLambda name params defaultCodes functionCode ->
      let newFunctions = Map.insert name (params, defaultCodes, functionCode) functions
          captured = Map.toList localEnv
       in execute code (ip + 1) (FunctionRefValue name captured : stack) globalsEnv localEnv newFunctions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
    DefineClass className maybeBase methods ->
      executeDefineClassInstruction execute code ip stack globalsEnv localEnv functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel (DefineClass className maybeBase methods)
    CallFunction fname compiledArgs pos -> do
      (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
        executeCallFunction execute isTopLevel fname compiledArgs pos stack globalsEnv localEnv functions outputs
      execute code (ip + 1) newStack newGlobals newLocalEnv newFunctions globalDecls forStates loopCounts exceptionHandlers newOutputs isTopLevel
    CallValueFunction compiledArgs pos -> do
      (newStack, newGlobals, newLocalEnv, newFunctions, newOutputs) <-
        executeCallValueFunction execute isTopLevel compiledArgs pos stack globalsEnv localEnv functions outputs
      execute code (ip + 1) newStack newGlobals newLocalEnv newFunctions globalDecls forStates loopCounts exceptionHandlers newOutputs isTopLevel
    UnpackToNames names pos ->
      case stack of
        value : rest ->
          case executeUnpackToNames isTopLevel globalDecls pos names value globalsEnv localEnv of
            Left err -> Left err
            Right (newGlobals, newLocals) ->
              execute code (ip + 1) rest newGlobals newLocals functions globalDecls forStates loopCounts exceptionHandlers outputs isTopLevel
        _ -> Left "VM runtime error: unpack requires one value on stack"
    BuildListComprehension clauses valueCode pos -> do
      (listValue, newGlobals, newFunctions, newOutputs) <-
        executeListComprehension execute clauses valueCode pos globalsEnv localEnv functions outputs
      execute code (ip + 1) (listValue : stack) newGlobals localEnv newFunctions globalDecls forStates loopCounts exceptionHandlers newOutputs isTopLevel
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
              newLocals = Map.insert "__python_hs_pending_except_error__" (StringValue err) localEnv
          in case exceptionHandlers of
               handlerIp : restHandlers -> execute code handlerIp rest globalsEnv newLocals functions globalDecls forStates loopCounts restHandlers outputs isTopLevel
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
