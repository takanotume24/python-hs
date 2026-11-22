module PythonHS.Lexer.ScanTripleQuotedString (scanTripleQuotedString) where

import PythonHS.Lexer.TripleQuotedStringResult (TripleQuotedStringResult (..))

-- | Scan a triple-quoted string literal from the beginning of the source.
scanTripleQuotedString :: String -> Maybe TripleQuotedStringResult
scanTripleQuotedString source =
  case source of
    firstQuote : secondQuote : thirdQuote : rest
      | firstQuote == secondQuote,
        secondQuote == thirdQuote,
        firstQuote == '"' || firstQuote == '\'' ->
          case scanContent firstQuote rest [] of
            Nothing -> Nothing
            Just (content, remaining) ->
              let consumedPrefix = [firstQuote, secondQuote, thirdQuote] ++ content ++ [firstQuote, firstQuote, firstQuote]
               in Just
                    TripleQuotedStringResult
                      { tripleQuotedStringResultContent = content,
                        tripleQuotedStringResultRemaining = remaining,
                        tripleQuotedStringResultLineCount = length (filter (== '\n') consumedPrefix),
                        tripleQuotedStringResultEndColumn = 1 + lastLineLength consumedPrefix
                      }
    _ -> Nothing
  where
    scanContent _ [] _ = Nothing
    scanContent quote ('\\' : escapedChar : rest) accumulated = scanContent quote rest (escapedChar : '\\' : accumulated)
    scanContent quote (firstChar : secondChar : thirdChar : rest) accumulated
      | firstChar == quote,
        secondChar == quote,
        thirdChar == quote =
          Just (reverse accumulated, rest)
    scanContent quote (character : rest) accumulated = scanContent quote rest (character : accumulated)

    lastLineLength text =
      case break (== '\n') (reverse text) of
        (lastLine, _) -> length lastLine
