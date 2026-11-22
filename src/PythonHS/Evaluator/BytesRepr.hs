module PythonHS.Evaluator.BytesRepr (bytesRepr) where

import Data.Char (intToDigit)
import Data.Word (Word8)

-- | Render bytes or a bytearray the way Python's @repr@ does.
bytesRepr :: Bool -> [Word8] -> String
bytesRepr isMutable items =
  let literal = bytesLiteral items
   in if isMutable then "bytearray(" ++ literal ++ ")" else literal
  where
    bytesLiteral values =
      let quote = chooseQuote values
       in "b" ++ [quote] ++ concatMap (escapeByte quote) values ++ [quote]

    chooseQuote values
      | 39 `elem` values && 34 `notElem` values = '"'
      | otherwise = '\''

    escapeByte quote byte
      | byte == 92 = "\\\\"
      | byte == fromIntegral (fromEnum quote) = ['\\', quote]
      | byte == 10 = "\\n"
      | byte == 9 = "\\t"
      | byte == 13 = "\\r"
      | byte >= 32 && byte < 127 = [toEnum (fromIntegral byte)]
      | otherwise = "\\x" ++ twoHex byte

    twoHex byte =
      let high = fromIntegral byte `div` 16
          low = fromIntegral byte `mod` 16
       in [intToDigit high, intToDigit low]
