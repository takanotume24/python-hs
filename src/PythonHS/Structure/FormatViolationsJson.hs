module PythonHS.Structure.FormatViolationsJson (formatViolationsJson) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

formatViolationsJson :: [PositionalArgViolation] -> String
formatViolationsJson = BSL.unpack . encode
