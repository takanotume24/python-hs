module PythonHS.VM.InsertAtIndex (insertAtIndex) where

import PythonHS.Evaluator.Value (Value)

insertAtIndex :: [Value] -> Integer -> Value -> [Value]
insertAtIndex values index value =
  let maxIndex = fromIntegral (length values)
      clampedIndex = max 0 (min index maxIndex)
      splitIndex = fromIntegral clampedIndex
      leftValues = take splitIndex values
      rightValues = drop splitIndex values
   in leftValues ++ (value : rightValues)
