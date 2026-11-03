module PythonHS.Evaluator.Value (Value (..)) where

import PythonHS.Evaluator.DictPair (DictPair)
import PythonHS.Evaluator.ModuleAttr (ModuleAttr)
import PythonHS.VM.MethodKind (MethodKind)
import PythonHS.VM.MethodNamePair (MethodNamePair)

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
        classValueMethods :: [MethodNamePair],
        classValueAttrs :: [(String, Value)]
      }
  | InstanceValue
      { instanceValueClass :: String,
        instanceValueAttrs :: [(String, Value)]
      }
  | ExceptionValue
      { exceptionValueClass :: String,
        exceptionValueMessage :: String
      }
  | BreakValue
  | ContinueValue
  | UnboundLocalValue {unboundLocalValueName :: String}
  deriving (Eq, Show)
