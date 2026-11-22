module PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput) where

import PythonHS.Evaluator.BytesRepr (bytesRepr)
import PythonHS.Evaluator.DictPair (DictPair (..))
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
    SetValue {setValueItems = items, setValueIsFrozen = frozen} -> setToOutput items frozen
    BytesValue {bytesValueItems = items, bytesValueIsMutable = isMutable} -> bytesRepr isMutable items
    ModuleValue {moduleValueName} -> "<module:" ++ moduleValueName ++ ">"
    FunctionRefValue {functionRefValueName = name} -> "<function " ++ name ++ ">"
    ClassValue {classValueName = name} -> "<class " ++ name ++ ">"
    InstanceValue {instanceValueClass = className} -> "<" ++ className ++ " instance>"
    ExceptionValue {exceptionValueClass = className, exceptionValueMessage = message} ->
      "<" ++ className ++ ": " ++ message ++ ">"
    BreakValue -> "<break>"
    ContinueValue -> "<continue>"
    UnboundLocalValue {unboundLocalValueName = name} -> "<unbound local " ++ name ++ ">"
  where
    pairToOutput pair = valueToReplOutput (dictPairKey pair) ++ ": " ++ valueToReplOutput (dictPairValue pair)

    joinWithCommaSpace [] = ""
    joinWithCommaSpace [x] = x
    joinWithCommaSpace (x : xs) = x ++ ", " ++ joinWithCommaSpace xs

    tupleToOutput values =
      case values of
        [] -> "()"
        [single] -> "(" ++ valueToReplOutput single ++ ",)"
        _ -> "(" ++ joinWithCommaSpace (map valueToReplOutput values) ++ ")"

    setToOutput items frozen =
      let body = "{" ++ joinWithCommaSpace (map valueToReplOutput items) ++ "}"
       in if frozen
            then if null items then "frozenset()" else "frozenset(" ++ body ++ ")"
            else if null items then "set()" else body

    escapeString [] = []
    escapeString ('\\' : rest) = "\\\\" ++ escapeString rest
    escapeString ('\'' : rest) = "\\'" ++ escapeString rest
    escapeString ('\n' : rest) = "\\n" ++ escapeString rest
    escapeString ('\t' : rest) = "\\t" ++ escapeString rest
    escapeString ('\r' : rest) = "\\r" ++ escapeString rest
    escapeString (c : rest) = c : escapeString rest
