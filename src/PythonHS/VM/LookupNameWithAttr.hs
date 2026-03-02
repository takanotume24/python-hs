module PythonHS.VM.LookupNameWithAttr (lookupNameWithAttr) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Value (Value (InstanceValue), Value)
import PythonHS.VM.LookupName (lookupName)

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
            InstanceValue _ attrPairs ->
              case lookup attrName attrPairs of
                Just attrValue -> loadAttrPath attrValue restAttrs
                Nothing -> Nothing
            _ -> Nothing
