module PythonHS.VM.PopValues (popValues) where

import PythonHS.Evaluator.Value (Value)

popValues :: Int -> [Value] -> Either String ([Value], [Value])
popValues count stack =
  let (popped, rest) = splitAt count stack
   in if length popped /= count
        then Left "VM runtime error: collection build requires enough values on stack"
        else Right (reverse popped, rest)
