module PythonHS.VM.CallSuperBuiltin (callSuperBuiltin) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallSuperBuiltinConfig (CallSuperBuiltinConfig (..))
import PythonHS.VM.LookupName (lookupName)

-- | Minimal implementation of @super()@: it returns a proxy instance whose
-- class is the base class of the current (or explicitly given) class.
callSuperBuiltin :: CallSuperBuiltinConfig -> Maybe (Either String Value)
callSuperBuiltin config
  | callSuperBuiltinName config /= "super" = Nothing
  | otherwise =
      case callSuperBuiltinArgs config of
        [] -> Just zeroArgumentSuper
        [ClassValue {classValueName = startName}, InstanceValue {instanceValueAttrs = attrs}] ->
          Just (superProxy startName attrs)
        _ -> Just (Left ("Type error: super() expects no arguments or (class, object) at " ++ showPos pos))
  where
    pos = callSuperBuiltinPos config
    localsNow = callSuperBuiltinLocals config
    globalsNow = callSuperBuiltinGlobals config

    zeroArgumentSuper =
      case (Map.lookup "__python_hs_current_class__" localsNow, Map.lookup "__python_hs_current_instance__" localsNow) of
        (Just (StringValue currentName), Just (InstanceValue _ attrs)) -> superProxy currentName attrs
        _ -> Left ("Runtime error: super(): no arguments at " ++ showPos pos)

    superProxy startName attrs =
      case lookupName startName localsNow globalsNow of
        Just (ClassValue _ maybeBase _ _) ->
          case maybeBase of
            Just baseName -> Right (InstanceValue {instanceValueClass = baseName, instanceValueAttrs = attrs})
            Nothing -> Left ("Type error: super() called for a class without a base class at " ++ showPos pos)
        _ -> Left ("Name error: undefined class " ++ startName ++ " at " ++ showPos pos)
