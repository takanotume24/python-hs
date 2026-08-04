module PythonHS.Evaluator.Value (Value (..)) where

import PythonHS.Evaluator.DictPair (DictPair)
import PythonHS.Evaluator.ModuleAttr (ModuleAttr)

data Value
  = IntValue {intValue :: Integer}
  | FloatValue {floatValue :: Double}
  | StringValue {stringValue :: String}
  | NoneValue
  | ListValue {listValueItems :: [Value]}
  | TupleValue {tupleValueItems :: [Value]}
  | DictValue {dictValuePairs :: [DictPair Value Value]}
  | ModuleValue {moduleValueName :: String, moduleValueAttrs :: [ModuleAttr Value]}
  | FunctionRefValue {functionRefValueName :: String, functionRefValueBindings :: [(String, Value)]}
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
