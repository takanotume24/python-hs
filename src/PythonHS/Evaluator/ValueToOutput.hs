module PythonHS.Evaluator.ValueToOutput (valueToOutput) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Value (Value (..))

valueToOutput :: Value -> String
valueToOutput value =
  case value of
    IntValue {intValue = n} -> show n
    FloatValue {floatValue = n} -> show n
    StringValue {stringValue = s} -> s
    NoneValue -> "None"
    ListValue {listValueItems = vals} -> "[" ++ joinWithCommaSpace (map valueToOutput vals) ++ "]"
    TupleValue {tupleValueItems = vals} -> tupleToOutput vals
    DictValue {dictValuePairs = pairs} -> "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    ModuleValue {moduleValueName} -> "<module:" ++ moduleValueName ++ ">"
    FunctionRefValue {functionRefValueName = name} -> "<function " ++ name ++ ">"
    ClassValue {classValueName = name} -> "<class " ++ name ++ ">"
    InstanceValue {instanceValueClass = className, instanceValueAttrs = attrs} ->
      case lookup "message" attrs of
        Just (StringValue {stringValue = message}) -> message
        _ -> "<" ++ className ++ " instance>"
    ExceptionValue {exceptionValueMessage = message} -> message
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
    UnboundLocalValue {unboundLocalValueName = name} -> "<unbound local " ++ name ++ ">"
  where
    pairToOutput pair = valueToOutput (dictPairKey pair) ++ ": " ++ valueToOutput (dictPairValue pair)

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

    tupleToOutput values =
      case values of
        [] -> "()"
        [single] -> "(" ++ valueToOutput single ++ ",)"
        _ -> "(" ++ joinWithCommaSpace (map valueToOutput values) ++ ")"
