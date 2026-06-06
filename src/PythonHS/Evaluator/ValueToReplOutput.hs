module PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput) where

import PythonHS.Evaluator.Value (Value (..))

valueToReplOutput :: Value -> String
valueToReplOutput value =
  case value of
    IntValue {intValue = n} -> show n
    FloatValue {floatValue = n} -> show n
    StringValue {stringValue = s} -> "'" ++ escapeString s ++ "'"
    NoneValue -> "None"
    ListValue {listValueItems = vals} -> "[" ++ joinWithCommaSpace (map valueToReplOutput vals) ++ "]"
    TupleValue {tupleValueItems = vals} -> tupleToOutput vals
    DictValue {dictValuePairs = pairs} -> "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    ModuleValue {moduleValueName} -> "<module:" ++ moduleValueName ++ ">"
    FunctionRefValue {functionRefValueName = name} -> "<function " ++ name ++ ">"
    ClassValue {classValueName = name} -> "<class " ++ name ++ ">"
    InstanceValue {instanceValueClass = className} -> "<" ++ className ++ " instance>"
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
  where
    pairToOutput (key, itemValue) = valueToReplOutput key ++ ": " ++ valueToReplOutput itemValue

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

    tupleToOutput values =
      case values of
        [] -> "()"
        [single] -> "(" ++ valueToReplOutput single ++ ",)"
        _ -> "(" ++ joinWithCommaSpace (map valueToReplOutput values) ++ ")"

    escapeString [] = []
    escapeString ('\\' : rest) = "\\\\" ++ escapeString rest
    escapeString ('\'' : rest) = "\\'" ++ escapeString rest
    escapeString ('\n' : rest) = "\\n" ++ escapeString rest
    escapeString ('\t' : rest) = "\\t" ++ escapeString rest
    escapeString ('\r' : rest) = "\\r" ++ escapeString rest
    escapeString (c : rest) = c : escapeString rest
