module PythonHS.Evaluator.DecodeText (decodeText) where

import Data.Char (chr, toLower)
import Data.Word (Word8)
import PythonHS.Evaluator.Utf8Decode (utf8Decode)

-- | Decode bytes with one of the supported Python encodings.
decodeText :: [Word8] -> String -> Either String String
decodeText bytes encoding =
  case normalizeEncoding encoding of
    "utf8" -> utf8Decode bytes
    "latin1" -> Right (map (chr . fromIntegral) bytes)
    "ascii" ->
      if all (< 128) bytes
        then Right (map (chr . fromIntegral) bytes)
        else Left "UnicodeDecodeError: 'ascii' codec cannot decode the bytes"
    other -> Left ("LookupError: unknown encoding: " ++ other)
  where
    normalizeEncoding = map toLower . filter (/= '-')
