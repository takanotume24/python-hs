module PythonHS.Evaluator.ComputeAssign (computeAssign) where

import PythonHS.Evaluator.ComputeAssignInput (ComputeAssignInput (..))
import PythonHS.Evaluator.ExpectNumber (expectNumber)
import PythonHS.Evaluator.ExpectNumberInput (ExpectNumberInput (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

computeAssign :: ComputeAssignInput -> Either String Value
computeAssign input =
  let op = computeAssignInputOp input
      pos = computeAssignInputPos input
      current = computeAssignInputLeft input
      rhs = computeAssignInputRight input
   in case op of
        "+=" ->
          case (current, rhs) of
            (IntValue {intValue = li}, IntValue {intValue = ri}) -> Right (IntValue (li + ri))
            (FloatValue {floatValue = li}, FloatValue {floatValue = ri}) -> Right (FloatValue (li + ri))
            (IntValue {intValue = li}, FloatValue {floatValue = ri}) -> Right (FloatValue (fromIntegral li + ri))
            (FloatValue {floatValue = li}, IntValue {intValue = ri}) -> Right (FloatValue (li + fromIntegral ri))
            (StringValue {stringValue = ls}, StringValue {stringValue = rs}) -> Right (StringValue (ls ++ rs))
            _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
        "-=" -> numericOp pos "-=" (-) current rhs
        "*=" -> numericOp pos "*=" (*) current rhs
        "/=" -> divOp pos current rhs
        "//=" -> floorDivOp pos current rhs
        "%=" -> modOp pos current rhs
        _ -> Left $ "Unsupported augmented assignment " ++ op ++ " at " ++ showPos pos
  where
    numericOp :: Position -> String -> (Double -> Double -> Double) -> Value -> Value -> Either String Value
    numericOp pos context opFn currentValue rhsValue = do
      leftNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = context, expectNumberInputPos = pos, expectNumberInputValue = currentValue}
      rightNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = context, expectNumberInputPos = pos, expectNumberInputValue = rhsValue}
      let newValue = case (currentValue, rhsValue) of (IntValue _, IntValue _) -> IntValue (round (opFn leftNumber rightNumber)); _ -> FloatValue (opFn leftNumber rightNumber)
      Right newValue

    divOp :: Position -> Value -> Value -> Either String Value
    divOp pos currentValue rhsValue = do
      leftNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "/=", expectNumberInputPos = pos, expectNumberInputValue = currentValue}
      rightNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "/=", expectNumberInputPos = pos, expectNumberInputValue = rhsValue}
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else Right (FloatValue (leftNumber / rightNumber))

    floorDivOp :: Position -> Value -> Value -> Either String Value
    floorDivOp pos currentValue rhsValue = do
      leftNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "//=", expectNumberInputPos = pos, expectNumberInputValue = currentValue}
      rightNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "//=", expectNumberInputPos = pos, expectNumberInputValue = rhsValue}
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else do
          let quotient = leftNumber / rightNumber
              newValue = case (currentValue, rhsValue) of (IntValue _, IntValue _) -> IntValue (floor quotient); _ -> FloatValue (fromIntegral (floor quotient :: Int))
          Right newValue

    modOp :: Position -> Value -> Value -> Either String Value
    modOp pos currentValue rhsValue = do
      leftNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "%=", expectNumberInputPos = pos, expectNumberInputValue = currentValue}
      rightNumber <- expectNumber ExpectNumberInput {expectNumberInputContext = "%=", expectNumberInputPos = pos, expectNumberInputValue = rhsValue}
      if rightNumber == 0
        then Left $ "Value error: modulo by zero at " ++ showPos pos
        else do
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
              newValue = case (currentValue, rhsValue) of (IntValue li, IntValue ri) -> IntValue (li `mod` ri); _ -> FloatValue remainder
          Right newValue
