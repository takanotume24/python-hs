module PythonHS.Evaluator.ExpectNumber (expectNumber) where

import PythonHS.Evaluator.ExpectNumberInput (ExpectNumberInput (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

expectNumber :: ExpectNumberInput -> Either String Double
expectNumber input = case expectNumberInputValue input of
  IntValue {intValue = n} -> Right (fromIntegral n)
  FloatValue {floatValue = n} -> Right n
  NoneValue -> Right 0
  _ -> Left $ "Type error: expected int in " ++ expectNumberInputContext input ++ " at " ++ showPos (expectNumberInputPos input)
