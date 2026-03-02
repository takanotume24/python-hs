module PythonHS.Evaluator.ValueToOutput (valueToOutput) where

import PythonHS.Evaluator.Value (Value (BreakValue, ClassValue, ContinueValue, DictValue, FloatValue, InstanceValue, IntValue, ListValue, NoneValue, StringValue))

valueToOutput :: Value -> String
valueToOutput value =
  case value of
    IntValue n -> show n
    FloatValue n -> show n
    StringValue s -> s
    NoneValue -> "None"
    ListValue vals -> "[" ++ joinWithCommaSpace (map valueToOutput vals) ++ "]"
    DictValue pairs -> "{" ++ joinWithCommaSpace (map pairToOutput pairs) ++ "}"
    ClassValue name _ _ -> "<class " ++ name ++ ">"
    InstanceValue className _ -> "<" ++ className ++ " instance>"
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
  where
    pairToOutput (key, itemValue) = valueToOutput key ++ ": " ++ valueToOutput itemValue

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs
