module PythonHS.Lexer.ParseExponent (parseExponent) where

import Data.Char (isDigit)

parseExponent :: String -> (String, String)
parseExponent input =
  case input of
    (e : restInput)
      | e == 'e' || e == 'E' ->
          case restInput of
            (signChar : afterSign)
              | signChar == '+' || signChar == '-' ->
                  let (expDigits, remaining) = span isDigit afterSign
                   in if null expDigits
                        then ("", input)
                        else (e : signChar : expDigits, remaining)
            _ ->
              let (expDigits, remaining) = span isDigit restInput
               in if null expDigits
                    then ("", input)
                    else (e : expDigits, remaining)
    _ -> ("", input)
