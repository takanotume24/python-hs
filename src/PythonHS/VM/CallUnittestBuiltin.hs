module PythonHS.VM.CallUnittestBuiltin (callUnittestBuiltin) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.CallUnittestBinaryAssertion (callUnittestBinaryAssertion)
import PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig)
import PythonHS.VM.CallUnittestContextBuiltin (callUnittestContextBuiltin)

-- | Minimal assertion helpers used by CPython's unittest test cases.
callUnittestBuiltin :: CallUnittestBuiltinConfig -> Maybe (Either String Value)
callUnittestBuiltin config =
  case callUnittestBinaryAssertion config of
    Just result -> Just result
    Nothing -> callUnittestContextBuiltin config
