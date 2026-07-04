module PythonHS.Lexer.SpanDigits (spanDigits) where

import Data.Char (isDigit)
import PythonHS.Lexer.ParseExponentResult (ParseExponentResult (..))

spanDigits :: String -> ParseExponentResult
spanDigits input =
  let result = span isDigit input
   in ParseExponentResult {parseExponentResultDigits = fst result, parseExponentResultRemaining = snd result}
