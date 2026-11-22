module PythonHS.Lexer.UnescapeStringLiteral (unescapeStringLiteral) where

import Data.Char (chr, digitToInt, isHexDigit, isOctDigit)

-- | Decode the common Python string escape sequences in a literal.
unescapeStringLiteral :: String -> String
unescapeStringLiteral text =
  case text of
    [] -> []
    '\\' : character : rest ->
      case character of
        'n' -> '\n' : unescapeStringLiteral rest
        't' -> '\t' : unescapeStringLiteral rest
        'r' -> '\r' : unescapeStringLiteral rest
        '\\' -> '\\' : unescapeStringLiteral rest
        '\'' -> '\'' : unescapeStringLiteral rest
        '"' -> '"' : unescapeStringLiteral rest
        '\n' -> unescapeStringLiteral rest
        'x' -> hexEscape 2 rest
        'u' -> hexEscape 4 rest
        'U' -> hexEscape 8 rest
        _ | isOctDigit character -> octalEscape (character : rest)
        _ -> '\\' : character : unescapeStringLiteral rest
    character : rest -> character : unescapeStringLiteral rest
  where
    hexEscape digitCount remainingText =
      let (digits, afterDigits) = splitAt digitCount remainingText
       in if length digits == digitCount && all isHexDigit digits
            then chr (foldl (\acc digit -> acc * 16 + digitToInt digit) 0 digits) : unescapeStringLiteral afterDigits
            else '\\' : 'x' : unescapeStringLiteral remainingText

    octalEscape remainingText =
      let (digits, _) = splitAt 3 (takeWhile isOctDigit remainingText)
          value = foldl (\acc digit -> acc * 8 + digitToInt digit) 0 digits
       in chr value : unescapeStringLiteral (drop (length digits) remainingText)
