module PythonHS.VM.EvalBinaryOpBitwise (evalBinaryOpBitwise) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import PythonHS.AST.BinaryOperator (BinaryOperator (BitwiseAndOperator, BitwiseOrOperator, BitwiseXorOperator, LeftShiftOperator, RightShiftOperator))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOpBitwiseConfig (EvalBinaryOpBitwiseConfig (..))

evalBinaryOpBitwise :: EvalBinaryOpBitwiseConfig -> Either String Value
evalBinaryOpBitwise config = evalForLeft (evalBinaryOpBitwiseConfigLeft config)
  where
    pos = evalBinaryOpBitwiseConfigPos config
    op = evalBinaryOpBitwiseConfigOp config
    opName = operatorName op

    evalForLeft IntValue {intValue = leftInt} = evalForRight leftInt (evalBinaryOpBitwiseConfigRight config)
    evalForLeft _ = Left ("Type error: " ++ opName ++ " expects int at " ++ showPos pos)

    evalForRight leftInt IntValue {intValue = rightInt} = evalBitwise op leftInt rightInt pos opName
    evalForRight _ _ = Left ("Type error: " ++ opName ++ " expects int at " ++ showPos pos)

    evalBitwise BitwiseAndOperator leftInt rightInt _ _ = Right (IntValue {intValue = leftInt .&. rightInt})
    evalBitwise BitwiseOrOperator leftInt rightInt _ _ = Right (IntValue {intValue = leftInt .|. rightInt})
    evalBitwise BitwiseXorOperator leftInt rightInt _ _ = Right (IntValue {intValue = leftInt `xor` rightInt})
    evalBitwise LeftShiftOperator leftInt rightInt pos' _ =
      if rightInt < 0
        then Left ("Value error: negative shift count at " ++ showPos pos')
        else Right (IntValue {intValue = leftInt `shiftL` fromIntegral rightInt})
    evalBitwise RightShiftOperator leftInt rightInt pos' _ =
      if rightInt < 0
        then Left ("Value error: negative shift count at " ++ showPos pos')
        else Right (IntValue {intValue = leftInt `shiftR` fromIntegral rightInt})
    evalBitwise _ _ _ pos' opName' = Left ("Type error: " ++ opName' ++ " expects int at " ++ showPos pos')

    operatorName BitwiseAndOperator = "&"
    operatorName BitwiseOrOperator = "|"
    operatorName BitwiseXorOperator = "^"
    operatorName LeftShiftOperator = "<<"
    operatorName RightShiftOperator = ">>"
    operatorName _ = "bitwise op"
