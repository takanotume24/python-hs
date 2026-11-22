module PythonHS.Evaluator.Utf8Decode (utf8Decode) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Word (Word8)

-- | Decode UTF-8 bytes, rejecting malformed sequences.
utf8Decode :: [Word8] -> Either String String
utf8Decode = go
  where
    go [] = Right []
    go (firstByte : rest)
      | firstByte < 0x80 = prepend (toEnum (fromIntegral firstByte)) (go rest)
      | firstByte >= 0xC2 && firstByte <= 0xDF = twoByte firstByte rest
      | firstByte >= 0xE0 && firstByte <= 0xEF = threeByte firstByte rest
      | firstByte >= 0xF0 && firstByte <= 0xF4 = fourByte firstByte rest
      | otherwise = Left "UnicodeDecodeError: invalid start byte"

    twoByte firstByte (secondByte : rest) = do
      secondValue <- continuation secondByte
      let code = (fromIntegral (firstByte .&. 0x1F) `shiftL` 6) .|. secondValue
      prepend (toEnum code) (go rest)
    twoByte _ _ = Left "UnicodeDecodeError: truncated sequence"

    threeByte firstByte (secondByte : thirdByte : rest) = do
      secondValue <- continuation secondByte
      thirdValue <- continuation thirdByte
      let code = (fromIntegral (firstByte .&. 0x0F) `shiftL` 12) .|. (secondValue `shiftL` 6) .|. thirdValue
      prepend (toEnum code) (go rest)
    threeByte _ _ = Left "UnicodeDecodeError: truncated sequence"

    fourByte firstByte (secondByte : thirdByte : fourthByte : rest) = do
      secondValue <- continuation secondByte
      thirdValue <- continuation thirdByte
      fourthValue <- continuation fourthByte
      let code = (fromIntegral (firstByte .&. 0x07) `shiftL` 18) .|. (secondValue `shiftL` 12) .|. (thirdValue `shiftL` 6) .|. fourthValue
      prepend (toEnum code) (go rest)
    fourByte _ _ = Left "UnicodeDecodeError: truncated sequence"

    continuation byte
      | byte .&. 0xC0 == 0x80 = Right (fromIntegral (byte .&. 0x3F))
      | otherwise = Left "UnicodeDecodeError: invalid continuation byte"

    prepend character restResult = fmap (character :) restResult
