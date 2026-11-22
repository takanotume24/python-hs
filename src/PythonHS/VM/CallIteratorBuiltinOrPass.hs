module PythonHS.VM.CallIteratorBuiltinOrPass (callIteratorBuiltinOrPass) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.CallBuiltinOrFailConfig (CallBuiltinOrFailConfig (..))
import PythonHS.VM.CallIteratorBuiltin (callIteratorBuiltin)
import PythonHS.VM.CallIteratorBuiltinConfig (CallIteratorBuiltinConfig (..))
import PythonHS.VM.CallIteratorBuiltinResult (CallIteratorBuiltinResult (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)

-- | Run an iterator-related builtin when applicable, otherwise return Nothing so
-- the caller can fall back to the ordinary builtin dispatch.
callIteratorBuiltinOrPass ::
  CallBuiltinOrFailConfig ->
  Maybe (Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String FunctionInfo, [String]))
callIteratorBuiltinOrPass config =
  case callIteratorBuiltin iteratorConfig of
    Nothing -> Nothing
    Just (Left err) -> Just (Left err)
    Just (Right result) ->
      Just
        ( Right
            ( callIteratorBuiltinResultStack result,
              callIteratorBuiltinResultGlobals result,
              callIteratorBuiltinResultLocals result,
              callIteratorBuiltinResultFunctions result,
              callIteratorBuiltinResultOutputs result
            )
        )
  where
    iteratorConfig =
      CallIteratorBuiltinConfig
        { callIteratorBuiltinExecute = callBuiltinOrFailExecute config,
          callIteratorBuiltinIsTopLevel = callBuiltinOrFailIsTopLevel config,
          callIteratorBuiltinName = callBuiltinOrFailName config,
          callIteratorBuiltinArgs = callBuiltinOrFailArgs config,
          callIteratorBuiltinPos = callBuiltinOrFailPos config,
          callIteratorBuiltinStack = callBuiltinOrFailStack config,
          callIteratorBuiltinGlobals = callBuiltinOrFailGlobals config,
          callIteratorBuiltinLocals = callBuiltinOrFailLocals config,
          callIteratorBuiltinFunctions = callBuiltinOrFailFunctions config,
          callIteratorBuiltinOutputs = callBuiltinOrFailOutputs config
        }
