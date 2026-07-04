module PythonHS.Evaluator.InsertAtIndex (insertAtIndex) where

import PythonHS.Evaluator.InsertAtIndexConfig (InsertAtIndexConfig (..))

insertAtIndex :: InsertAtIndexConfig a -> [a]
insertAtIndex config =
  let values = insertAtIndexValues config
      index = insertAtIndexIndex config
      value = insertAtIndexValue config
      maxIndex = fromIntegral (length values)
      clampedIndex = max 0 (min (fromIntegral index) maxIndex)
      splitIndex = fromIntegral clampedIndex
      (leftValues, rightValues) = splitAt splitIndex values
   in leftValues ++ (value : rightValues)
