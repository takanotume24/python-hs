module PythonHS.Structure.ViolationCategory (ViolationCategory (..)) where

import Data.Aeson (ToJSON (..))

-- | Category of positional argument usage that violates the record-syntax guideline.
data ViolationCategory
  = DataConCategory
  | FunDeclCategory
  | TupleCategory
  deriving (Eq, Show)

instance ToJSON ViolationCategory where
  toJSON DataConCategory = toJSON ("data_constructor" :: String)
  toJSON FunDeclCategory = toJSON ("function_declaration" :: String)
  toJSON TupleCategory = toJSON ("tuple" :: String)
