module PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Value (Value (ClassValue, FunctionRefValue, InstanceValue, ModuleValue), Value)
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)

lookupNameWithAttr :: String -> Map.Map String Value -> Map.Map String Value -> Maybe Value
lookupNameWithAttr name localNow globalsNow =
  case break (== '.') name of
    (_, []) -> lookupName name localNow globalsNow
    (rootName, '.' : attrTail) ->
      case lookupName rootName localNow globalsNow of
        Nothing -> Nothing
        Just rootValue -> loadAttrPath rootValue (splitAttrPath attrTail)
    _ -> Nothing
  where
    splitAttrPath path =
      case break (== '.') path of
        (segment, []) -> [segment]
        (segment, '.' : rest) -> segment : splitAttrPath rest
        _ -> [path]

    loadAttrPath currentValue attrs =
      case attrs of
        [] -> Just currentValue
        attrName : restAttrs ->
          case currentValue of
            InstanceValue className attrPairs ->
              case lookup attrName attrPairs of
                Just attrValue -> loadAttrPath attrValue restAttrs
                Nothing ->
                  case findMethodFunctionName globalsNow localNow className attrName of
                    Just functionName ->
                      loadAttrPath (FunctionRefValue functionName [("__python_hs_bound_self__", currentValue)]) restAttrs
                    Nothing -> Nothing
            ClassValue className _ methods ->
              case lookup attrName methods of
                Just functionName -> loadAttrPath (FunctionRefValue functionName []) restAttrs
                Nothing ->
                  case findMethodFunctionName globalsNow localNow className attrName of
                    Just functionName -> loadAttrPath (FunctionRefValue functionName []) restAttrs
                    Nothing -> Nothing
            ModuleValue moduleName moduleAttrs ->
              case lookup attrName moduleAttrs of
                Just attrValue -> loadAttrPath attrValue restAttrs
                Nothing ->
                  let mappedName = modulePrefixFor (splitAttrPath moduleName) ++ attrName
                   in case lookupName mappedName localNow globalsNow of
                        Just attrValue -> loadAttrPath attrValue restAttrs
                        Nothing -> Nothing
            _ -> Nothing
