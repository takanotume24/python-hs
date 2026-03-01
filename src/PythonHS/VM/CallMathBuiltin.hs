module PythonHS.VM.CallMathBuiltin (callMathBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue, StringValue))
import PythonHS.Lexer.Position (Position)

callMathBuiltin :: String -> [Value] -> Position -> Maybe (Either String Value)
callMathBuiltin name args pos =
  case name of
    "sqrt" -> Just $ evalUnary "sqrt" sqrt args
    "sin" -> Just $ evalUnary "sin" sin args
    "cos" -> Just $ evalUnary "cos" cos args
    "tan" -> Just $ evalUnary "tan" tan args
    "log" -> Just $ evalUnary "log" log args
    "exp" -> Just $ evalUnary "exp" exp args
    "pi" -> Just $ case args of
      [StringValue moduleName]
        | moduleName == "<module:math>" -> Right (FloatValue pi)
      [_] -> Left ("Type error: pi expects math module receiver at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling pi at " ++ showPos pos)
    "e" -> Just $ case args of
      [StringValue moduleName]
        | moduleName == "<module:math>" -> Right (FloatValue (exp 1))
      [_] -> Left ("Type error: e expects math module receiver at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling e at " ++ showPos pos)
    _ -> Nothing
  where
    evalUnary fname op values =
      case values of
        [StringValue moduleName, value]
          | moduleName == "<module:math>" ->
              case value of
                IntValue n -> Right (FloatValue (op (fromIntegral n)))
                FloatValue n -> Right (FloatValue (op n))
                _ -> Left ("Type error: " ++ fname ++ " expects number at " ++ showPos pos)
        [_, _] -> Left ("Type error: " ++ fname ++ " expects math module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)