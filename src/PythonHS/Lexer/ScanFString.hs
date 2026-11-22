module PythonHS.Lexer.ScanFString (scanFString) where

-- | Scans the contents of an f-string after the opening quote.
-- Returns Nothing when the closing quote is missing on the same line.
scanFString :: Char -> String -> Maybe (String, String)
scanFString quote = go []
  where
    go _ [] = Nothing
    go acc ('\\' : escapedChar : rest) = go (escapedChar : '\\' : acc) rest
    go acc (character : rest)
      | character == quote = Just (reverse acc, rest)
      | character == '\n' = Nothing
      | otherwise = go (character : acc) rest
