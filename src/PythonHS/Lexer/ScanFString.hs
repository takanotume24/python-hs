module PythonHS.Lexer.ScanFString (scanFString) where

-- | Scans the contents of an f-string after the opening quote.
--
-- Braces inside the f-string open an interpolation expression. Quotes that
-- appear inside interpolation braces belong to the expression and must not
-- terminate the outer string, which allows nested f-strings and quote reuse.
-- Returns Nothing when the closing quote is missing on the same line.
scanFString :: Char -> String -> Maybe (String, String)
scanFString quote input =
  case go [] (0 :: Int) Nothing input of
    Just result -> Just result
    Nothing -> scanWithoutNesting input
  where
    -- Fallback for strings with an unterminated interpolation. Stopping at the
    -- next quote lets the parser report a precise interpolation error instead
    -- of a stray-character lexer error.
    scanWithoutNesting = simpleScan []
      where
        simpleScan _ [] = Nothing
        simpleScan acc ('\\' : escapedChar : rest) = simpleScan (escapedChar : '\\' : acc) rest
        simpleScan acc (character : rest)
          | character == quote = Just (reverse acc, rest)
          | character == '\n' = Nothing
          | otherwise = simpleScan (character : acc) rest

    go _ _ _ [] = Nothing
    go acc depth innerQuote remaining =
      case innerQuote of
        Just innerQuoteChar -> consumeInnerString acc depth innerQuoteChar remaining
        Nothing -> consumeOuterText acc depth remaining

    consumeInnerString acc depth innerQuoteChar (character : rest)
      | character == '\\' = consumeEscape acc depth (Just innerQuoteChar) rest
      | character == innerQuoteChar = go (character : acc) depth Nothing rest
      | character == '\n' = Nothing
      | otherwise = go (character : acc) depth (Just innerQuoteChar) rest
    consumeInnerString _ _ _ [] = Nothing

    consumeOuterText acc depth (character : rest)
      | character == quote && depth == 0 = Just (reverse acc, rest)
      | character == '\\' = consumeEscape acc depth Nothing rest
      | character == '{' =
          case rest of
            '{' : rest' | depth == 0 -> go ('{' : '{' : acc) depth Nothing rest'
            _ -> go (character : acc) (depth + 1) Nothing rest
      | character == '}' =
          case rest of
            '}' : rest' | depth == 0 -> go ('}' : '}' : acc) depth Nothing rest'
            _
              | depth > 0 -> go (character : acc) (depth - 1) Nothing rest
              | otherwise -> go (character : acc) depth Nothing rest
      | character == '\n' = Nothing
      | depth > 0 && (character == '\'' || character == '"') =
          go (character : acc) depth (Just character) rest
      | otherwise = go (character : acc) depth Nothing rest
    consumeOuterText _ _ [] = Nothing

    consumeEscape acc depth innerQuote rest =
      case rest of
        escapedChar : rest' -> go (escapedChar : '\\' : acc) depth innerQuote rest'
        [] -> Nothing
