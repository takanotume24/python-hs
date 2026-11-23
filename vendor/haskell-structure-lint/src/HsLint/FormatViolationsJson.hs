module HsLint.FormatViolationsJson (formatViolationsJson) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import HsLint.PositionalArgViolation (PositionalArgViolation)

formatViolationsJson :: [PositionalArgViolation] -> String
formatViolationsJson = BSL.unpack . encode
