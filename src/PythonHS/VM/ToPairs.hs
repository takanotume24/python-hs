module PythonHS.VM.ToPairs (toPairs) where

import PythonHS.Evaluator.Value (Value)

toPairs :: [Value] -> Either String [(Value, Value)]
toPairs values =
  case values of
    [] -> Right []
    key : value : rest -> do
      remaining <- toPairs rest
      Right ((key, value) : remaining)
    _ -> Left "VM runtime error: dictionary build expects key/value pairs"
