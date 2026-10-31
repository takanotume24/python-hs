module PythonHS.VM.ExecuteDeleteName (executeDeleteName) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.Instruction (Instruction (DeleteName))
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.VMScopeContext (VMScopeContext (..))
import PythonHS.VM.VMState (VMState (..))

executeDeleteName ::
  (VMState -> Either String VMState) ->
  VMState ->
  Instruction ->
  Either String VMState
executeDeleteName execute state instruction =
  case instruction of
    DeleteName target ->
      case deleteTarget (VMScopeContext (vmIsTopLevel state) (envGlobalDecls (vmEnv state))) target (vmEnv state) of
        Left err -> Left err
        Right newEnv -> execute state {vmIp = vmIp state + 1, vmEnv = newEnv}
    _ -> Left "VM runtime error: unexpected instruction in executeDeleteName"
  where
    -- The rest of the implementation lives in the where clause to keep the
    -- top-level function single and focused on instruction dispatch.

    deleteTarget scopeCtx target env =
      case break (== '.') target of
        (_, []) -> deleteSimpleName scopeCtx target env
        (rootName, '.' : attrTail) -> deleteAttrPath scopeCtx rootName attrTail env
        _ -> Left "VM runtime error: invalid delete target"

    deleteSimpleName scopeCtx name env =
      let isTopLevelNow = vmScopeContextIsTopLevel scopeCtx
          globalDeclsNow = vmScopeContextGlobalDecls scopeCtx
          unboundSentinel = UnboundLocalValue {unboundLocalValueName = name}
       in if isTopLevelNow || Set.member name globalDeclsNow
            then
              let newGlobals = Map.delete name (envGlobals env)
               in Right env {envGlobals = newGlobals, envLocals = if isTopLevelNow then newGlobals else envLocals env}
            else
              if Map.member name (envLocals env)
                then Right env {envLocals = Map.insert name unboundSentinel (Map.delete name (envLocals env))}
                else
                  if Map.member name (envGlobals env)
                    then Right env {envGlobals = Map.delete name (envGlobals env)}
                    else Left ("Name error: undefined identifier " ++ name ++ " at 0:0")

    splitAttrPath path =
      case break (== '.') path of
        (segment, []) -> [segment]
        (segment, '.' : rest) -> segment : splitAttrPath rest
        _ -> [path]

    deleteAttrPath scopeCtx rootName attrTail env =
      case lookupName rootName (envLocals env) (envGlobals env) of
        Nothing -> Left ("Name error: undefined identifier " ++ rootName ++ " at 0:0")
        Just rootValue ->
          case deleteAttrPathValue rootValue (splitAttrPath attrTail) of
            Left err -> Left err
            Right newRoot -> Right (updateEnvWithRoot scopeCtx rootName newRoot env)

    deleteAttrPathValue currentValue attrs =
      case attrs of
        [] -> Right currentValue
        [attrName] ->
          case currentValue of
            InstanceValue className attrPairs ->
              Right (InstanceValue className (filter (\(k, _) -> k /= attrName) attrPairs))
            ClassValue className maybeBase methods classAttrs ->
              Right (ClassValue className maybeBase methods (filter (\(k, _) -> k /= attrName) classAttrs))
            _ -> Left "Type error: attribute delete expects instance or class at 0:0"
        attrName : restAttrs ->
          case currentValue of
            InstanceValue className attrPairs ->
              case lookup attrName attrPairs of
                Just nestedValue ->
                  case deleteAttrPathValue nestedValue restAttrs of
                    Left err -> Left err
                    Right newNested ->
                      let newPairs = upsertAttr attrName newNested attrPairs
                       in Right (InstanceValue className newPairs)
                Nothing -> Left ("Name error: undefined identifier " ++ attrName ++ " at 0:0")
            _ -> Left "Type error: attribute delete expects instance at 0:0"

    upsertAttr key newValue pairs =
      case pairs of
        [] -> [(key, newValue)]
        (k, v) : restPairs ->
          if k == key
            then (k, newValue) : restPairs
            else (k, v) : upsertAttr key newValue restPairs

    updateEnvWithRoot scopeCtx rootName newRoot env =
      let isTopLevelNow = vmScopeContextIsTopLevel scopeCtx
          classNameMaybe = classNameFromValue rootName newRoot
          localsWithRoot = Map.insert rootName newRoot (envLocals env)
          globalsWithRoot = Map.insert rootName newRoot (envGlobals env)
          localsWithClass = insertClassName classNameMaybe newRoot localsWithRoot
          globalsWithClass = insertClassName classNameMaybe newRoot globalsWithRoot
       in if isTopLevelNow
            then env {envLocals = localsWithClass, envGlobals = globalsWithClass}
            else
              if Map.member rootName (envLocals env)
                then env {envLocals = localsWithClass, envGlobals = insertClassName classNameMaybe newRoot (envGlobals env)}
                else env {envGlobals = globalsWithClass}

    classNameFromValue rootName value =
      case value of
        ClassValue {classValueName = className} ->
          if className == rootName then Nothing else Just className
        _ -> Nothing

    insertClassName maybeName newValue envMap =
      case maybeName of
        Just className -> Map.insert className newValue envMap
        Nothing -> envMap
