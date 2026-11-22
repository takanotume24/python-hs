module PythonHS.Evaluator.EncodeText (encodeText) where

import Data.Char (ord, toLower)
import Data.Word (Word8)
import PythonHS.Evaluator.Utf8Encode (utf8Encode)

-- | Encode text with one of the supported Python encodings.
encodeText :: String -> String -> Either String [Word8]
encodeText text encoding =
  case normalizeEncoding encoding of
    "utf8" -> Right (utf8Encode text)
    "latin1" -> encodeNarrow 255 "latin-1"
    "ascii" -> encodeNarrow 127 "ascii"
    other -> Left ("LookupError: unknown encoding: " ++ other)
  where
    encodeNarrow limit encodingName =
      let codes = map ord text
       in if all (<= limit) codes
            then Right (map fromIntegral codes)
            else Left ("UnicodeEncodeError: '" ++ encodingName ++ "' codec cannot encode the string")

    normalizeEncoding = map toLower . filter (/= '-')
