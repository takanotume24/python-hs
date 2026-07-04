module PythonHS.VM.PopValues (popValues) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.PopValuesConfig (PopValuesConfig (..))

popValues :: PopValuesConfig -> Either String ([Value], [Value])
popValues config =
  let count = popValuesCount config
      stack = popValuesStack config
      (popped, rest) = splitAt count stack
   in if length popped /= count
        then Left "VM runtime error: collection build requires enough values on stack"
        else Right (reverse popped, rest)
