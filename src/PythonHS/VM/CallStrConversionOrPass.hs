module PythonHS.VM.CallStrConversionOrPass (callStrConversionOrPass) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..))
import PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult (..))
import PythonHS.VM.CallStrConversion (callStrConversion)
import PythonHS.VM.CallStrConversionConfig (CallStrConversionConfig (..))
import PythonHS.VM.CallStrConversionOrPassConfig (CallStrConversionOrPassConfig (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)

callStrConversionOrPass :: CallStrConversionOrPassConfig -> Maybe (Either String ([Value], Map String Value, Map String Value, Map String FunctionInfo, [String]))
callStrConversionOrPass config =
  case callStrConversion strConversionConfig of
    Nothing -> Nothing
    Just (Left err) -> Just (Left err)
    Just (Right result) ->
      Just
        ( Right
            ( callInstanceMethodStringResultStack result,
              callInstanceMethodStringResultGlobals result,
              callInstanceMethodStringResultLocals result,
              callInstanceMethodStringResultFunctions result,
              callInstanceMethodStringResultOutputs result
            )
        )
  where
    strConversionConfig =
      CallStrConversionConfig
        { callStrConversionName = callStrConversionOrPassName config,
          callStrConversionArgs = callStrConversionOrPassArgs config,
          callStrConversionMethodConfig =
            CallInstanceMethodStringConfig
              { callInstanceMethodStringExecute = callStrConversionOrPassExecute config,
                callInstanceMethodStringIsTopLevel = callStrConversionOrPassIsTopLevel config,
                callInstanceMethodStringPos = callStrConversionOrPassPos config,
                callInstanceMethodStringStack = callStrConversionOrPassStack config,
                callInstanceMethodStringInstance = IntValue 0,
                callInstanceMethodStringGlobals = callStrConversionOrPassGlobals config,
                callInstanceMethodStringLocals = callStrConversionOrPassLocals config,
                callInstanceMethodStringFunctions = callStrConversionOrPassFunctions config,
                callInstanceMethodStringOutputs = callStrConversionOrPassOutputs config
              }
        }
