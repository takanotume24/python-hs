module PythonHS.Evaluator.InsertAtIndex (insertAtIndex) where

import PythonHS.Evaluator.InsertAtIndexConfig (InsertAtIndexConfig (..))

insertAtIndex :: InsertAtIndexConfig a -> [a]
insertAtIndex config =
  let values = insertAtIndexValues config
      index = insertAtIndexIndex config
      value = insertAtIndexValue config
      maxIndex = fromIntegral (length values) :: Integer
      clampedIndex = max 0 (min index maxIndex)
      splitIndex = fromIntegral clampedIndex :: Int
   in take splitIndex values ++ (value : drop splitIndex values)
