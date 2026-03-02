module PythonHS.VM.BindPatternCaptures (bindPatternCaptures) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value)

bindPatternCaptures ::
  Bool ->
  Set.Set String ->
  [(String, Value)] ->
  Map.Map String Value ->
  Map.Map String Value ->
  (Map.Map String Value, Map.Map String Value)
bindPatternCaptures isTopLevel globalDecls captures globalsEnv localEnv =
  apply captures globalsEnv localEnv
  where
    apply [] globals locals = (globals, locals)
    apply ((name, value) : rest) globals locals =
      if isTopLevel || Set.member name globalDecls
        then
          let newGlobals = Map.insert name value globals
              newLocals = if isTopLevel then newGlobals else locals
           in apply rest newGlobals newLocals
        else
          let newLocals = Map.insert name value locals
           in apply rest globals newLocals
