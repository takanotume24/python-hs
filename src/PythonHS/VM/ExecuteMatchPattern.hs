module PythonHS.VM.ExecuteMatchPattern (executeMatchPattern) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value (IntValue), Value)
import PythonHS.VM.BindPatternCaptures (bindPatternCaptures)
import PythonHS.VM.MatchPatternBindings (matchPatternBindings)

executeMatchPattern ::
  Bool ->
  Set.Set String ->
  Pattern ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Either String ([Value], Map.Map String Value, Map.Map String Value)
executeMatchPattern isTopLevel globalDecls patternValue stack globalsEnv localEnv =
  case stack of
    value : rest ->
      case matchPatternBindings patternValue value of
        Just captures ->
          let (newGlobals, newLocals) = bindPatternCaptures isTopLevel globalDecls captures globalsEnv localEnv
           in Right (IntValue 1 : rest, newGlobals, newLocals)
        Nothing ->
          Right (IntValue 0 : rest, globalsEnv, localEnv)
    _ -> Left "VM runtime error: match pattern requires one value on stack"
