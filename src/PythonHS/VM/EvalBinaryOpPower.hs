module PythonHS.VM.EvalBinaryOpPower (evalBinaryOpPower) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOpPowerConfig (EvalBinaryOpPowerConfig (..))

evalBinaryOpPower :: EvalBinaryOpPowerConfig -> Either String Value
evalBinaryOpPower config =
  case left of
    IntValue {intValue = leftInt} ->
      case right of
        IntValue {intValue = rightInt} ->
          if rightInt >= 0
            then Right (IntValue {intValue = leftInt ^ rightInt})
            else
              if leftInt == 0
                then Left ("Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos)
                else Right (FloatValue {floatValue = fromIntegral leftInt ** fromIntegral rightInt})
        _ -> evalPowerFloat left right pos
    _ -> evalPowerFloat left right pos
  where
    left = evalBinaryOpPowerConfigLeft config
    right = evalBinaryOpPowerConfigRight config
    pos = evalBinaryOpPowerConfigPos config
    expectNumber = evalBinaryOpPowerConfigExpectNumber config

    evalPowerFloat l r pos'' = do
      leftNumber <- expectNumber pos'' l
      rightNumber <- expectNumber pos'' r
      if leftNumber == 0 && rightNumber < 0
        then Left ("Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos'')
        else Right (FloatValue {floatValue = leftNumber ** rightNumber})
