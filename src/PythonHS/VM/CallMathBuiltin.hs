module PythonHS.VM.CallMathBuiltin (callMathBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
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
      [StringValue {stringValue = moduleName}]
        | moduleName == "<module:math>" -> Right (FloatValue {floatValue = pi})
      [ModuleValue {moduleValueName = moduleName}]
        | moduleName == "math" -> Right (FloatValue {floatValue = pi})
      [_] -> Left ("Type error: pi expects math module receiver at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling pi at " ++ showPos pos)
    "e" -> Just $ case args of
      [StringValue {stringValue = moduleName}]
        | moduleName == "<module:math>" -> Right (FloatValue {floatValue = exp 1})
      [ModuleValue {moduleValueName = moduleName}]
        | moduleName == "math" -> Right (FloatValue {floatValue = exp 1})
      [_] -> Left ("Type error: e expects math module receiver at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling e at " ++ showPos pos)
    _ -> Nothing
  where
    evalUnary fname op values =
      case values of
        [StringValue {stringValue = moduleName}, value]
          | moduleName == "<module:math>" ->
              case value of
                IntValue {intValue = n} -> Right (FloatValue {floatValue = op (fromIntegral n)})
                FloatValue {floatValue = n} -> Right (FloatValue {floatValue = op n})
                _ -> Left ("Type error: " ++ fname ++ " expects number at " ++ showPos pos)
        [ModuleValue {moduleValueName = moduleName}, value]
          | moduleName == "math" ->
              case value of
                IntValue {intValue = n} -> Right (FloatValue {floatValue = op (fromIntegral n)})
                FloatValue {floatValue = n} -> Right (FloatValue {floatValue = op n})
                _ -> Left ("Type error: " ++ fname ++ " expects number at " ++ showPos pos)
        [_, _] -> Left ("Type error: " ++ fname ++ " expects math module receiver at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)
