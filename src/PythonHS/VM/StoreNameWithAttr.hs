module PythonHS.VM.StoreNameWithAttr (storeNameWithAttr) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value (InstanceValue, IntValue), Value)
import PythonHS.VM.LookupName (lookupName)

storeNameWithAttr :: Bool -> Set.Set String -> String -> Value -> Map.Map String Value -> Map.Map String Value -> Either String (Map.Map String Value, Map.Map String Value)
storeNameWithAttr isTopLevelNow globalDeclsNow name value globalsNow localsNow =
  case break (== '.') name of
    (_, []) ->
      if isTopLevelNow || Set.member name globalDeclsNow
        then
          let newGlobals = Map.insert name value globalsNow
              newLocals = if isTopLevelNow then newGlobals else localsNow
           in Right (newGlobals, newLocals)
        else Right (globalsNow, Map.insert name value localsNow)
    (rootName, '.' : attrTail) ->
      case lookupName rootName localsNow globalsNow of
        Nothing -> Left ("Name error: undefined identifier " ++ rootName ++ " at 0:0")
        Just rootValue ->
          case storeAttrPath rootValue (splitAttrPath attrTail) value of
            Left err -> Left err
            Right newRoot ->
              if Map.member rootName localsNow
                then Right (globalsNow, Map.insert rootName newRoot localsNow)
                else Right (Map.insert rootName newRoot globalsNow, localsNow)
    _ -> Left "VM runtime error: invalid store target"
  where
    splitAttrPath path =
      case break (== '.') path of
        (segment, []) -> [segment]
        (segment, '.' : rest) -> segment : splitAttrPath rest
        _ -> [path]

    storeAttrPath currentValue attrs valueToStore =
      case attrs of
        [] -> Right valueToStore
        [attrName] ->
          case currentValue of
            InstanceValue className attrPairs ->
              if isFrozenInstance attrPairs && attrName /= "__python_hs_frozen__"
                then Left ("Type error: cannot assign to frozen dataclass field " ++ attrName ++ " at 0:0")
                else
                  let newPairs = upsertAttr attrName valueToStore attrPairs
                   in Right (InstanceValue className newPairs)
            _ -> Left "Type error: attribute store expects instance at 0:0"
        attrName : restAttrs ->
          case currentValue of
            InstanceValue className attrPairs ->
              case lookup attrName attrPairs of
                Just nestedValue ->
                  case storeAttrPath nestedValue restAttrs valueToStore of
                    Left err -> Left err
                    Right newNested ->
                      let newPairs = upsertAttr attrName newNested attrPairs
                       in Right (InstanceValue className newPairs)
                Nothing -> Left ("Name error: undefined identifier " ++ attrName ++ " at 0:0")
            _ -> Left "Type error: attribute store expects instance at 0:0"

    upsertAttr key newValue pairs =
      case pairs of
        [] -> [(key, newValue)]
        (k, v) : restPairs ->
          if k == key
            then (k, newValue) : restPairs
            else (k, v) : upsertAttr key newValue restPairs

    isFrozenInstance attrPairs =
      case lookup "__python_hs_frozen__" attrPairs of
        Just (IntValue n) -> n /= 0
        _ -> False
