module PythonHS.VM.CallNumericExtraBuiltin (callNumericExtraBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))

callNumericExtraBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callNumericExtraBuiltin config =
  case name of
    "pow" -> Just (evalPow args)
    "divmod" -> Just (evalDivMod args)
    _ -> Nothing
  where
    name = callBuiltinName config
    args = callBuiltinArgs config
    pos = callBuiltinPos config

    evalPow values =
      case values of
        [IntValue {intValue = base}, IntValue {intValue = exponent}] -> integerPower base exponent
        [IntValue {intValue = base}, IntValue {intValue = exponent}, IntValue {intValue = modulus}] -> modularPower base exponent modulus
        [base, exponent] -> floatPower base exponent
        [_, _, _] -> Left ("Type error: pow with modulus expects int arguments at " ++ showPos pos)
        _ -> Left (countMismatch "pow")

    integerPower base exponent
      | exponent >= 0 = Right (IntValue {intValue = base ^ exponent})
      | base == 0 = Left ("Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos)
      | otherwise = Right (FloatValue {floatValue = fromIntegral base ** fromIntegral exponent})

    modularPower base exponent modulus
      | modulus == 0 = Left ("Value error: pow modulus must not be zero at " ++ showPos pos)
      | exponent < 0 = Left ("Value error: pow with modulus requires a non-negative exponent at " ++ showPos pos)
      | otherwise = Right (IntValue {intValue = base ^ exponent `mod` modulus})

    floatPower base exponent =
      case (numberValue base, numberValue exponent) of
        (Just baseNumber, Just exponentNumber)
          | baseNumber == 0 && exponentNumber < 0 -> Left ("Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos)
          | otherwise -> Right (FloatValue {floatValue = baseNumber ** exponentNumber})
        _ -> Left ("Type error: pow expects number arguments at " ++ showPos pos)

    evalDivMod values =
      case values of
        [IntValue {intValue = dividend}, IntValue {intValue = divisor}] -> integerDivMod dividend divisor
        [dividend, divisor] -> floatDivMod dividend divisor
        _ -> Left (countMismatch "divmod")

    integerDivMod dividend divisor
      | divisor == 0 = Left ("Value error: division by zero at " ++ showPos pos)
      | otherwise =
          Right
            ( TupleValue
                { tupleValueItems =
                    [ IntValue {intValue = dividend `div` divisor},
                      IntValue {intValue = dividend `mod` divisor}
                    ]
                }
            )

    floatDivMod dividend divisor =
      case (numberValue dividend, numberValue divisor) of
        (Just dividendNumber, Just divisorNumber)
          | divisorNumber == 0 -> Left ("Value error: division by zero at " ++ showPos pos)
          | otherwise ->
              let quotient = fromIntegral (floor (dividendNumber / divisorNumber) :: Integer)
                  remainder = dividendNumber - quotient * divisorNumber
               in Right
                    ( TupleValue
                        { tupleValueItems =
                            [ FloatValue {floatValue = quotient},
                              FloatValue {floatValue = remainder}
                            ]
                        }
                    )
        _ -> Left ("Type error: divmod expects number arguments at " ++ showPos pos)

    numberValue value =
      case value of
        IntValue {intValue = n} -> Just (fromIntegral n)
        FloatValue {floatValue = n} -> Just n
        _ -> Nothing

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
