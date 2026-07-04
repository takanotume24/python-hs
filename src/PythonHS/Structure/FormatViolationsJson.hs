module PythonHS.Structure.FormatViolationsJson (formatViolationsJson) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)

formatViolationsJson :: [PositionalArgViolation] -> String
formatViolationsJson = BSL.unpack . encode
