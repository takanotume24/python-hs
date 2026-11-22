module PythonHS.VM.CallFileBuiltinOrPass (callFileBuiltinOrPass) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.CallBuiltinOrFailConfig (CallBuiltinOrFailConfig (..))
import PythonHS.VM.CallFileBuiltin (callFileBuiltin)
import PythonHS.VM.CallFileBuiltinConfig (CallFileBuiltinConfig (..))
import PythonHS.VM.CallFileBuiltinResult (CallFileBuiltinResult (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)

-- | Run a file I/O builtin when applicable, otherwise return Nothing so the
-- caller can fall back to the ordinary builtin dispatch.
callFileBuiltinOrPass ::
  CallBuiltinOrFailConfig ->
  Maybe (Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String FunctionInfo, [String]))
callFileBuiltinOrPass config =
  case callFileBuiltin fileConfig of
    Nothing -> Nothing
    Just (Left err) -> Just (Left err)
    Just (Right result) ->
      Just
        ( Right
            ( callFileBuiltinResultStack result,
              callFileBuiltinResultGlobals result,
              callFileBuiltinResultLocals result,
              callFileBuiltinResultFunctions result,
              callFileBuiltinResultOutputs result
            )
        )
  where
    fileConfig =
      CallFileBuiltinConfig
        { callFileBuiltinIsTopLevel = callBuiltinOrFailIsTopLevel config,
          callFileBuiltinName = callBuiltinOrFailName config,
          callFileBuiltinArgs = callBuiltinOrFailArgs config,
          callFileBuiltinPos = callBuiltinOrFailPos config,
          callFileBuiltinStack = callBuiltinOrFailStack config,
          callFileBuiltinGlobals = callBuiltinOrFailGlobals config,
          callFileBuiltinLocals = callBuiltinOrFailLocals config,
          callFileBuiltinFunctions = callBuiltinOrFailFunctions config,
          callFileBuiltinOutputs = callBuiltinOrFailOutputs config
        }
