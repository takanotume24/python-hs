module PythonHS.VM.ParseFormatSpec (parseFormatSpec) where

import Data.Char (isDigit)
import PythonHS.VM.FormatSpec (FormatSpec (..))

-- | Parse a format specifier such as @>8.2f@ or @,05d@.
--
-- Returns the original specifier on failure so callers can report it.
parseFormatSpec :: String -> Either String FormatSpec
parseFormatSpec raw =
  let (fill, align, afterAlign) = parseAlign raw
      (sign, afterSign) = parseSign afterAlign
      (zeroPad, afterZero) = parseZero afterSign
      (width, afterWidth) = parseWidth afterZero
      (thousands, afterThousands) = parseThousands afterWidth
   in case parsePrecision afterThousands of
        Nothing -> Left raw
        Just (precision, afterPrecision) ->
          case afterPrecision of
            [] ->
              Right
                FormatSpec
                  { formatSpecFill = fill,
                    formatSpecAlign = align,
                    formatSpecSign = sign,
                    formatSpecZeroPad = zeroPad,
                    formatSpecWidth = width,
                    formatSpecThousands = thousands,
                    formatSpecPrecision = precision,
                    formatSpecType = Nothing
                  }
            [typeChar]
              | typeChar `elem` "dfs" ->
                  Right
                    FormatSpec
                      { formatSpecFill = fill,
                        formatSpecAlign = align,
                        formatSpecSign = sign,
                        formatSpecZeroPad = zeroPad,
                        formatSpecWidth = width,
                        formatSpecThousands = thousands,
                        formatSpecPrecision = precision,
                        formatSpecType = Just typeChar
                      }
            _ -> Left raw
  where
    parseAlign (fillChar : alignChar : rest)
      | alignChar `elem` "<>^=" = (Just fillChar, Just alignChar, rest)
    parseAlign (alignChar : rest)
      | alignChar `elem` "<>^=" = (Nothing, Just alignChar, rest)
    parseAlign rest = (Nothing, Nothing, rest)

    parseSign (signChar : rest)
      | signChar `elem` "+- " = (Just signChar, rest)
    parseSign rest = (Nothing, rest)

    parseZero ('0' : rest) = (True, rest)
    parseZero rest = (False, rest)

    parseWidth rest =
      let (digits, after) = span isDigit rest
       in case digits of
            [] -> (Nothing, rest)
            _ -> (Just (read digits), after)

    parseThousands (',' : rest) = (True, rest)
    parseThousands rest = (False, rest)

    parsePrecision ('.' : rest) =
      case span isDigit rest of
        ([], _) -> Nothing
        (digits, after) -> Just (Just (read digits), after)
    parsePrecision rest = Just (Nothing, rest)
