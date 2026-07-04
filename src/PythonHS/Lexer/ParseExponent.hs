module PythonHS.Lexer.ParseExponent (parseExponent, ParseExponentResult (..)) where

import PythonHS.Lexer.ParseExponentResult (ParseExponentResult (..))
import PythonHS.Lexer.SpanDigits (spanDigits)

parseExponent :: String -> ParseExponentResult
parseExponent input =
  case input of
    (e : restInput)
      | e == 'e' || e == 'E' ->
          case restInput of
            (signChar : afterSign)
              | signChar == '+' || signChar == '-' ->
                  let result = spanDigits afterSign
                   in if null (parseExponentResultDigits result)
                        then ParseExponentResult {parseExponentResultDigits = "", parseExponentResultRemaining = input}
                        else ParseExponentResult {parseExponentResultDigits = e : signChar : parseExponentResultDigits result, parseExponentResultRemaining = parseExponentResultRemaining result}
            _ ->
              let result = spanDigits restInput
               in if null (parseExponentResultDigits result)
                    then ParseExponentResult {parseExponentResultDigits = "", parseExponentResultRemaining = input}
                    else ParseExponentResult {parseExponentResultDigits = e : parseExponentResultDigits result, parseExponentResultRemaining = parseExponentResultRemaining result}
    _ -> ParseExponentResult {parseExponentResultDigits = "", parseExponentResultRemaining = input}
