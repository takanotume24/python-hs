module PythonHS.Evaluator.Value (Value (..)) where

data Value
  = IntValue Int
  | StringValue String
  | NoneValue
  | ListValue [Value]
  | DictValue [(Value, Value)]
  | BreakValue
  | ContinueValue
  deriving (Eq, Show)
