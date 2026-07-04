module PythonHS.VM.FindMethodFunctionName (findMethodFunctionName) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (ClassValue))
import PythonHS.VM.LookupName (lookupName)

findMethodFunctionName :: Map.Map String Value -> Map.Map String Value -> String -> String -> Maybe String
findMethodFunctionName globalsNow localsNow className methodName =
  case lookupName className localsNow globalsNow of
    Just (ClassValue _ maybeParent methods) ->
      case lookup methodName methods of
        Just functionName -> Just functionName
        Nothing ->
          case maybeParent of
            Just parentName -> findMethodFunctionName globalsNow localsNow parentName methodName
            Nothing -> Nothing
    _ -> Nothing
