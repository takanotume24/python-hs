module PythonHS.Evaluator.ValueToOutput (valueToOutput) where

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
    InstanceValue {instanceValueClass = className} -> "<" ++ className ++ " instance>"
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
  where
    pairToOutput (key, itemValue) = valueToOutput key ++ ": " ++ valueToOutput itemValue

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

    tupleToOutput values =
      case values of
        [] -> "()"
        [single] -> "(" ++ valueToOutput single ++ ",)"
        _ -> "(" ++ joinWithCommaSpace (map valueToOutput values) ++ ")"
