module PythonHS.VM.CallStrConversionConfig (CallStrConversionConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig)

data CallStrConversionConfig = CallStrConversionConfig
  { callStrConversionName :: String,
    callStrConversionArgs :: [Value],
    callStrConversionMethodConfig :: CallInstanceMethodStringConfig
  }
