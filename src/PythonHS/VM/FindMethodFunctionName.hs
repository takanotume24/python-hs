module PythonHS.VM.FindMethodFunctionName (findMethodFunctionName) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (ClassValue))
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.MethodNamePair (MethodNamePair (..))

findMethodFunctionName :: Map.Map String Value -> Map.Map String Value -> String -> String -> Maybe MethodNamePair
findMethodFunctionName globalsNow localsNow className methodName =
  case lookupName className localsNow globalsNow of
    Just (ClassValue _ maybeParent methods _) ->
      case filter (\methodPair -> methodNamePairOriginal methodPair == methodName) methods of
        (methodPair : _) -> Just methodPair
        [] ->
          case maybeParent of
            Just parentName -> findMethodFunctionName globalsNow localsNow parentName methodName
            Nothing -> Nothing
    _ -> Nothing
