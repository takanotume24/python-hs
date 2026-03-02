module PythonHS.Evaluator.Value (Value (..)) where

data Value
  = IntValue Integer
  | FloatValue Double
  | StringValue String
  | NoneValue
  | ListValue [Value]
  | DictValue [(Value, Value)]
  | ClassValue String (Maybe String) [(String, String)]
  | InstanceValue String [(String, Value)]
  | BreakValue
  | ContinueValue
  deriving (Eq, Show)
