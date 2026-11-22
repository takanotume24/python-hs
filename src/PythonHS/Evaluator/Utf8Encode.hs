module PythonHS.Evaluator.Utf8Encode (utf8Encode) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8)

-- | Encode a Haskell string as UTF-8 bytes.
utf8Encode :: String -> [Word8]
utf8Encode = concatMap encodeChar
  where
    encodeChar character
      | code < 0x80 = [fromIntegral code]
      | code < 0x800 =
          [ 0xC0 .|. fromIntegral (code `shiftR` 6),
            0x80 .|. fromIntegral (code .&. 0x3F)
          ]
      | code < 0x10000 =
          [ 0xE0 .|. fromIntegral (code `shiftR` 12),
            0x80 .|. fromIntegral ((code `shiftR` 6) .&. 0x3F),
            0x80 .|. fromIntegral (code .&. 0x3F)
          ]
      | otherwise =
          [ 0xF0 .|. fromIntegral (code `shiftR` 18),
            0x80 .|. fromIntegral ((code `shiftR` 12) .&. 0x3F),
            0x80 .|. fromIntegral ((code `shiftR` 6) .&. 0x3F),
            0x80 .|. fromIntegral (code .&. 0x3F)
          ]
      where
        code = fromEnum character
