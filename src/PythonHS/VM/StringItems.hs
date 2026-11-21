module PythonHS.VM.StringItems (stringItems) where

import PythonHS.Evaluator.Value (Value (..))

stringItems :: [Value] -> Maybe [String]
stringItems [] = Just []
stringItems (StringValue {stringValue = text} : restItems) = fmap (text :) (stringItems restItems)
stringItems (_ : _) = Nothing
