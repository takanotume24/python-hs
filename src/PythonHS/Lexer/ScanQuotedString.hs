module PythonHS.Lexer.ScanQuotedString (scanQuotedString) where

import PythonHS.Lexer.QuotedStringResult (QuotedStringResult (..))

-- | Scan the body of a single-quoted string, honouring backslash escapes.
scanQuotedString :: Char -> String -> Maybe QuotedStringResult
scanQuotedString quote = go []
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
