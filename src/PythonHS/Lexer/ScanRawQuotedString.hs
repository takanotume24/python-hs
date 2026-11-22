module PythonHS.Lexer.ScanRawQuotedString (scanRawQuotedString) where

import PythonHS.Lexer.QuotedStringResult (QuotedStringResult (..))

-- | Scan the body of a raw single-quoted string.
--
-- Backslashes are kept verbatim, but a backslash still prevents the quote
-- character from terminating the literal, matching Python's raw strings.
scanRawQuotedString :: Char -> String -> Maybe QuotedStringResult
scanRawQuotedString quote = go []
  where
    go _ [] = Nothing
    go accumulated ('\\' : escaped : rest) =
      go (escaped : '\\' : accumulated) rest
    go accumulated (character : rest)
      | character == quote =
          Just
            QuotedStringResult
              { quotedStringContent = reverse accumulated,
                quotedStringRemaining = rest
              }
      | character == '\n' = Nothing
      | otherwise = go (character : accumulated) rest
