module PythonHS.VM.PopValues (popValues) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.PopValuesConfig (PopValuesConfig (..))
import PythonHS.VM.PopValuesResult (PopValuesResult (..))

popValues :: PopValuesConfig -> Either String PopValuesResult
popValues config =
  let count = popValuesCount config
      stack = popValuesStack config
      splitResult = splitAt count stack
   in if length (fst splitResult) /= count
        then Left "VM runtime error: collection build requires enough values on stack"
        else Right PopValuesResult {popValuesResultPopped = reverse (fst splitResult), popValuesResultRest = snd splitResult}
