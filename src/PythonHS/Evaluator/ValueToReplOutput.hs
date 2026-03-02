module PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput) where

import PythonHS.Evaluator.Value (Value (BreakValue, ClassValue, ContinueValue, DictValue, FloatValue, FunctionRefValue, InstanceValue, IntValue, ListValue, NoneValue, StringValue, TupleValue))

valueToReplOutput :: Value -> String
valueToReplOutput value =
  case value of
    IntValue n -> show n
    FloatValue n -> show n
    StringValue s -> "'" ++ escapeString s ++ "'"
    NoneValue -> "None"
    ListValue vals -> "[" ++ joinWithCommaSpace (map valueToReplOutput vals) ++ "]"
    TupleValue vals -> tupleToOutput vals
    DictValue pairs -> "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    FunctionRefValue name _ -> "<function " ++ name ++ ">"
    ClassValue name _ _ -> "<class " ++ name ++ ">"
    InstanceValue className _ -> "<" ++ className ++ " instance>"
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
