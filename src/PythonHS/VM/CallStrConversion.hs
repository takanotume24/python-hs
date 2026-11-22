module PythonHS.VM.CallStrConversion (callStrConversion) where

import PythonHS.VM.CallInstanceMethodString (callInstanceMethodString)
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..))
import PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult)
import PythonHS.VM.CallStrConversionConfig (CallStrConversionConfig (..))

-- | Implements @str(instance)@ via a user-defined @__str__@ or @__repr__@.
-- Returns Nothing for any other call so the normal builtin dispatch can run.
callStrConversion :: CallStrConversionConfig -> Maybe (Either String CallInstanceMethodStringResult)
callStrConversion config =
  case (callStrConversionName config, callStrConversionArgs config) of
    ("str", [instanceValue]) ->
      callInstanceMethodString ((callStrConversionMethodConfig config) {callInstanceMethodStringInstance = instanceValue})
    _ -> Nothing
