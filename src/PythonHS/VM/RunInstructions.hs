module PythonHS.VM.RunInstructions (runInstructions) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.Instruction (Instruction (AddValues, Halt, Jump, JumpIfFalse, LoadName, PrintTop, PushConst, StoreName))

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = execute 0 [] Map.empty []
  where
    execute ip stack env outputs
      | ip < 0 || ip >= length instructions = Right outputs
      | otherwise =
          case instructions !! ip of
            PushConst value -> execute (ip + 1) (value : stack) env outputs
            LoadName name pos ->
              case Map.lookup name env of
                Just value -> execute (ip + 1) (value : stack) env outputs
                Nothing -> Left ("Name error: undefined variable " ++ name ++ " at " ++ showPos pos)
            StoreName name ->
              case stack of
                value : rest -> execute (ip + 1) rest (Map.insert name value env) outputs
                _ -> Left "VM runtime error: store requires one value on stack"
            Jump target -> execute target stack env outputs
            JumpIfFalse target ->
              case stack of
                value : rest ->
                  if isTruthy value
                    then execute (ip + 1) rest env outputs
                    else execute target rest env outputs
                _ -> Left "VM runtime error: conditional jump requires one value on stack"
            AddValues pos ->
              case stack of
                right : left : rest ->
                  case (left, right) of
                    (IntValue l, IntValue r) -> execute (ip + 1) (IntValue (l + r) : rest) env outputs
                    (StringValue l, StringValue r) -> execute (ip + 1) (StringValue (l ++ r) : rest) env outputs
                    _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
                _ -> Left "VM runtime error: add requires two values on stack"
            PrintTop ->
              case stack of
                value : rest -> execute (ip + 1) rest env (outputs ++ [valueToOutput value])
                _ -> Left "VM runtime error: print requires one value on stack"
            Halt -> Right outputs

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
