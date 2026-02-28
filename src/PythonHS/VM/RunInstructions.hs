module PythonHS.VM.RunInstructions (runInstructions) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, ApplyNot, ApplyUnaryMinus, CallFunction, DefineFunction, Halt, Jump, JumpIfFalse, LoadName, PrintTop, PushConst, ReturnTop, StoreName))

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  (_, _, _, outputs) <- execute instructions 0 [] Map.empty Map.empty Map.empty [] True
  pure outputs
  where
    execute code ip stack globalsEnv localEnv functions outputs isTopLevel
      | ip < 0 || ip >= length code = Right (Nothing, globalsEnv, functions, outputs)
      | otherwise =
          case code !! ip of
            PushConst value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions outputs isTopLevel
            LoadName name pos ->
              case lookupName name localEnv globalsEnv of
                Just value -> execute code (ip + 1) (value : stack) globalsEnv localEnv functions outputs isTopLevel
                Nothing -> Left ("Name error: undefined variable " ++ name ++ " at " ++ showPos pos)
            StoreName name ->
              case stack of
                value : rest ->
                  if isTopLevel
                    then
                      let newGlobals = Map.insert name value globalsEnv
                       in execute code (ip + 1) rest newGlobals newGlobals functions outputs isTopLevel
                    else
                      let newLocals = Map.insert name value localEnv
                       in execute code (ip + 1) rest globalsEnv newLocals functions outputs isTopLevel
                _ -> Left "VM runtime error: store requires one value on stack"
            Jump target -> execute code target stack globalsEnv localEnv functions outputs isTopLevel
            JumpIfFalse target ->
              case stack of
                value : rest ->
                  if isTruthy value
                    then execute code (ip + 1) rest globalsEnv localEnv functions outputs isTopLevel
                    else execute code target rest globalsEnv localEnv functions outputs isTopLevel
                _ -> Left "VM runtime error: conditional jump requires one value on stack"
            DefineFunction name params functionCode ->
              let newFunctions = Map.insert name (params, functionCode) functions
               in execute code (ip + 1) stack globalsEnv localEnv newFunctions outputs isTopLevel
            CallFunction fname argCount pos ->
              case popArgs argCount stack of
                Left err -> Left err
                Right (args, rest) ->
                  case Map.lookup fname functions of
                    Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)
                    Just (params, functionCode) ->
                      if length params /= length args
                        then Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)
                        else do
                          let functionLocals = Map.fromList (zip params args)
                          (maybeValue, newGlobals, newFunctions, newOutputs) <-
                            execute functionCode 0 [] globalsEnv functionLocals functions outputs False
                          let returnValue =
                                case maybeValue of
                                  Just value -> value
                                  Nothing -> NoneValue
                          execute code (ip + 1) (returnValue : rest) newGlobals localEnv newFunctions newOutputs isTopLevel
            ApplyBinary op pos ->
              case stack of
                right : left : rest ->
                  case evalBinaryOp op left right pos of
                    Left err -> Left err
                    Right value -> execute code (ip + 1) (value : rest) globalsEnv localEnv functions outputs isTopLevel
                _ -> Left "VM runtime error: add requires two values on stack"
            ApplyUnaryMinus pos ->
              case stack of
                value : rest ->
                  case value of
                    IntValue n -> execute code (ip + 1) (IntValue (negate n) : rest) globalsEnv localEnv functions outputs isTopLevel
                    FloatValue n -> execute code (ip + 1) (FloatValue (negate n) : rest) globalsEnv localEnv functions outputs isTopLevel
                    _ -> Left ("Type error: unary - expects int at " ++ showPos pos)
                _ -> Left "VM runtime error: unary - requires one value on stack"
            ApplyNot _ ->
              case stack of
                value : rest ->
                  let result = if isTruthy value then 0 else 1
                   in execute code (ip + 1) (IntValue result : rest) globalsEnv localEnv functions outputs isTopLevel
                _ -> Left "VM runtime error: not requires one value on stack"
            ReturnTop ->
              case stack of
                value : _ -> Right (Just value, globalsEnv, functions, outputs)
                _ -> Left "VM runtime error: return requires one value on stack"
            PrintTop ->
              case stack of
                value : rest -> execute code (ip + 1) rest globalsEnv localEnv functions (outputs ++ [valueToOutput value]) isTopLevel
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

    isTruthy value =
      case value of
        IntValue n -> n /= 0
        FloatValue n -> n /= 0
        StringValue s -> not (null s)
        NoneValue -> False
        ListValue vals -> not (null vals)
        DictValue pairs -> not (null pairs)
        BreakValue -> True
        ContinueValue -> True
