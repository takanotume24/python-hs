module PythonHS.Evaluator.InsertAtIndexConfig (InsertAtIndexConfig (..)) where

data InsertAtIndexConfig a = InsertAtIndexConfig
  { insertAtIndexValues :: [a],
    insertAtIndexIndex :: Integer,
    insertAtIndexValue :: a
  }
