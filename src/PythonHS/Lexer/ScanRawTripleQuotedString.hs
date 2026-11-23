module PythonHS.Lexer.ScanRawTripleQuotedString (scanRawTripleQuotedString) where

import PythonHS.Lexer.ScanTripleQuotedString (scanTripleQuotedString)
import PythonHS.Lexer.TripleQuotedStringResult (TripleQuotedStringResult)

-- | Scan a raw triple-quoted string such as @r\"\"\"...\"\"\"@.
--
-- Raw strings still honour backslash escaped quotes while scanning, which is
-- also how 'scanTripleQuotedString' treats ordinary triple-quoted strings.
scanRawTripleQuotedString :: String -> Maybe TripleQuotedStringResult
scanRawTripleQuotedString source =
  case source of
    prefix : quote1 : quote2 : quote3 : rest
      | prefix == 'r' || prefix == 'R',
        quote1 == quote2,
        quote2 == quote3,
        quote1 == '"' || quote1 == '\'' ->
          scanTripleQuotedString (quote1 : quote2 : quote3 : rest)
    _ -> Nothing
