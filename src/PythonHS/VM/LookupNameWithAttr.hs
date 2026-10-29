module PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ModuleAttr (ModuleAttr (..))
import PythonHS.Evaluator.Value (Value (ClassValue, FunctionRefValue, InstanceValue, ModuleValue))
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

    findClassAttrOrMethod className attrName =
      case lookupName className localNow globalsNow of
        Just (ClassValue _ maybeBase methods classAttrs) ->
          case lookup attrName classAttrs of
            Just attrValue -> Just (Right attrValue)
            Nothing ->
              case filter (\methodPair -> methodNamePairOriginal methodPair == attrName) methods of
                (methodPair : _) -> Just (Left methodPair)
                [] ->
                  case maybeBase of
                    Just baseName -> findClassAttrOrMethod baseName attrName
                    Nothing -> Nothing
        _ -> Nothing

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
                  case findClassAttrOrMethod className attrName of
                    Just (Right attrValue) -> loadAttrPath attrValue restAttrs
                    Just (Left methodPair) -> loadAttrPath (methodRefForInstance methodPair currentValue className) restAttrs
                    Nothing -> Nothing
            ClassValue className _ _ _ ->
              case findClassAttrOrMethod className attrName of
                Just (Right attrValue) -> loadAttrPath attrValue restAttrs
                Just (Left methodPair) -> loadAttrPath (methodRefForClass methodPair currentValue) restAttrs
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
