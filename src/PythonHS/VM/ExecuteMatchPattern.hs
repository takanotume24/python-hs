module PythonHS.VM.ExecuteMatchPattern (executeMatchPattern) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.VM.BindPatternCaptures (bindPatternCaptures)
import PythonHS.VM.MatchPatternBindings (matchPatternBindings)
import PythonHS.VM.MatchPatternBindingsConfig (MatchPatternBindingsConfig (..))
import PythonHS.VM.VMScopeContext (VMScopeContext)

executeMatchPattern ::
  VMScopeContext ->
  Pattern ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Either String ([Value], Map.Map String Value, Map.Map String Value)
executeMatchPattern scopeCtx patternValue stack globalsEnv localEnv =
  case stack of
    value : rest ->
      case matchPatternBindings MatchPatternBindingsConfig {matchPatternBindingsPattern = patternValue, matchPatternBindingsSubject = value} of
        Just captures ->
          let (newGlobals, newLocals) = bindPatternCaptures scopeCtx captures globalsEnv localEnv
           in Right (IntValue 1 : rest, newGlobals, newLocals)
        Nothing ->
          Right (IntValue 0 : rest, globalsEnv, localEnv)
    _ -> Left "VM runtime error: match pattern requires one value on stack"
