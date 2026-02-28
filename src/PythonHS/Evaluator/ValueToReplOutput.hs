module PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput) where

import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))

valueToReplOutput :: Value -> String
valueToReplOutput value =
  case value of
    IntValue n -> show n
    FloatValue n -> show n
    StringValue s -> "'" ++ escapeString s ++ "'"
    NoneValue -> "None"
    ListValue vals -> "[" ++ joinWithCommaSpace (map valueToReplOutput vals) ++ "]"
    DictValue pairs -> "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
  where
    pairToOutput (key, itemValue) = valueToReplOutput key ++ ": " ++ valueToReplOutput itemValue

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

    escapeString [] = []
    escapeString ('\\' : rest) = "\\\\" ++ escapeString rest
    escapeString ('\'' : rest) = "\\'" ++ escapeString rest
    escapeString ('\n' : rest) = "\\n" ++ escapeString rest
    escapeString ('\t' : rest) = "\\t" ++ escapeString rest
    escapeString ('\r' : rest) = "\\r" ++ escapeString rest
    escapeString (c : rest) = c : escapeString rest
