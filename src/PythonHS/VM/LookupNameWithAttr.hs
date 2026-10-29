module PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ModuleAttr (ModuleAttr (..))
import PythonHS.Evaluator.Value (Value (ClassValue, FunctionRefValue, InstanceValue, ModuleValue))
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.MethodKind (MethodKind (..))
import PythonHS.VM.MethodNamePair (MethodNamePair (..))
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

    methodRefForInstance methodPair instanceValue className =
      case methodNamePairKind methodPair of
        StaticMethod -> FunctionRefValue (methodNamePairMangled methodPair) []
        ClassMethod ->
          case lookupName className localNow globalsNow of
            Just classValue@ClassValue {} -> FunctionRefValue (methodNamePairMangled methodPair) [("__python_hs_bound_self__", classValue)]
            _ -> FunctionRefValue (methodNamePairMangled methodPair) []
        InstanceMethod -> FunctionRefValue (methodNamePairMangled methodPair) [("__python_hs_bound_self__", instanceValue)]

    methodRefForClass methodPair classValue =
      case methodNamePairKind methodPair of
        StaticMethod -> FunctionRefValue (methodNamePairMangled methodPair) []
        ClassMethod -> FunctionRefValue (methodNamePairMangled methodPair) [("__python_hs_bound_self__", classValue)]
        InstanceMethod -> FunctionRefValue (methodNamePairMangled methodPair) []

    loadAttrPath currentValue attrs =
      case attrs of
        [] -> Just currentValue
        attrName : restAttrs ->
          case currentValue of
            InstanceValue className attrPairs ->
              case lookup attrName attrPairs of
                Just attrValue -> loadAttrPath attrValue restAttrs
                Nothing ->
                  case lookupName className localNow globalsNow of
                    Just (ClassValue _ _ _ classAttrs) ->
                      case lookup attrName classAttrs of
                        Just attrValue -> loadAttrPath attrValue restAttrs
                        Nothing ->
                          case findMethodFunctionName globalsNow localNow className attrName of
                            Just methodPair ->
                              loadAttrPath (methodRefForInstance methodPair currentValue className) restAttrs
                            Nothing -> Nothing
                    _ ->
                      case findMethodFunctionName globalsNow localNow className attrName of
                        Just methodPair ->
                          loadAttrPath (methodRefForInstance methodPair currentValue className) restAttrs
                        Nothing -> Nothing
            ClassValue className _ methods classAttrs ->
              case lookup attrName classAttrs of
                Just attrValue -> loadAttrPath attrValue restAttrs
                Nothing ->
                  case filter (\methodPair -> methodNamePairOriginal methodPair == attrName) methods of
                    (methodPair : _) -> loadAttrPath (methodRefForClass methodPair currentValue) restAttrs
                    [] ->
                      case findMethodFunctionName globalsNow localNow className attrName of
                        Just methodPair -> loadAttrPath (methodRefForClass methodPair currentValue) restAttrs
                        Nothing -> Nothing
            ModuleValue moduleName moduleAttrs ->
              case filter (\attr -> moduleAttrName attr == attrName) moduleAttrs of
                (attr : _) -> loadAttrPath (moduleAttrValue attr) restAttrs
                [] ->
                  let mappedName = modulePrefixFor (splitAttrPath moduleName) ++ attrName
                   in case lookupName mappedName localNow globalsNow of
                        Just attrValue -> loadAttrPath attrValue restAttrs
                        Nothing -> Nothing
            _ -> Nothing
