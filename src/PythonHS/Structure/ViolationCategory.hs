module PythonHS.Structure.ViolationCategory (ViolationCategory (..)) where

import Data.Aeson (ToJSON (..))

-- | Category of positional argument usage that violates the record-syntax guideline.
data ViolationCategory
  = DataConCategory
  | FunDeclCategory
  | TupleCategory
  | PositionalRecordConCategory
  deriving (Eq, Show)

instance ToJSON ViolationCategory where
  toJSON DataConCategory = toJSON ("data_constructor" :: String)
  toJSON FunDeclCategory = toJSON ("function_declaration" :: String)
  toJSON TupleCategory = toJSON ("tuple" :: String)
  toJSON PositionalRecordConCategory = toJSON ("positional_record_con" :: String)
