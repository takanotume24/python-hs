module PythonHS.Evaluator.Value (Value (..)) where

data Value
  = IntValue { intValue :: Integer }
  | FloatValue { floatValue :: Double }
  | StringValue { stringValue :: String }
  | NoneValue
  | ListValue { listValueItems :: [Value] }
  | TupleValue { tupleValueItems :: [Value] }
  | DictValue { dictValuePairs :: [(Value, Value)] }
  | ModuleValue { moduleValueName :: String, moduleValueAttrs :: [(String, Value)] }
  | FunctionRefValue { functionRefValueName :: String, functionRefValueBindings :: [(String, Value)] }
  | ClassValue
      { classValueName :: String,
        classValueBase :: Maybe String,
        classValueMethods :: [(String, String)]
      }
  | InstanceValue
      { instanceValueClass :: String,
        instanceValueAttrs :: [(String, Value)]
      }
  | BreakValue
  | ContinueValue
  deriving (Eq, Show)
