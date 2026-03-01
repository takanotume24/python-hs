module PythonHS.Lexer.ScanTokenStep (scanTokenStep) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier)
import PythonHS.Lexer.LexerError (LexerError (UnexpectedCharacter))
import PythonHS.Lexer.ParseExponent (parseExponent)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        PlusAssignToken,
        MinusAssignToken,
        StarAssignToken,
        SlashAssignToken,
        PercentAssignToken,
        DoubleSlashAssignToken,
        ColonToken,
        CommaToken,
        DotToken,
        IntegerToken,
        FloatToken,
        StringToken,
        LParenToken,
        MinusToken,
        PlusToken,
        RParenToken,
        EqToken,
        NotEqToken,
        LtToken,
        GtToken,
        LteToken,
        GteToken,
        LBracketToken,
        RBracketToken,
        LBraceToken,
        RBraceToken,
        SlashToken,
        DoubleSlashToken,
        PercentToken,
        StarToken
      )
  )

scanTokenStep :: String -> Int -> Int -> Either LexerError (Token, String, Int)
scanTokenStep src ln col =
  case src of
    [] -> Left (UnexpectedCharacter ' ')
    (c : rest)
      | c == '=' ->
          case rest of
            ('=' : rest') -> Right (Token EqToken "==" (Position ln col), rest', col + 2)
            _ -> Right (Token AssignToken "=" (Position ln col), rest, col + 1)
      | c == '!' ->
          case rest of
            ('=' : rest') -> Right (Token NotEqToken "!=" (Position ln col), rest', col + 2)
            _ -> Left (UnexpectedCharacter '!')
      | c == '<' ->
          case rest of
            ('=' : rest') -> Right (Token LteToken "<=" (Position ln col), rest', col + 2)
            _ -> Right (Token LtToken "<" (Position ln col), rest, col + 1)
      | c == '>' ->
          case rest of
            ('=' : rest') -> Right (Token GteToken ">=" (Position ln col), rest', col + 2)
            _ -> Right (Token GtToken ">" (Position ln col), rest, col + 1)
      | c == '+' ->
          case rest of
            ('=' : rest') -> Right (Token PlusAssignToken "+=" (Position ln col), rest', col + 2)
            _ -> Right (Token PlusToken "+" (Position ln col), rest, col + 1)
      | c == '-' ->
          case rest of
            ('=' : rest') -> Right (Token MinusAssignToken "-=" (Position ln col), rest', col + 2)
            _ -> Right (Token MinusToken "-" (Position ln col), rest, col + 1)
      | c == '*' ->
          case rest of
            ('=' : rest') -> Right (Token StarAssignToken "*=" (Position ln col), rest', col + 2)
            _ -> Right (Token StarToken "*" (Position ln col), rest, col + 1)
      | c == '/' ->
          case rest of
            ('/' : '=' : rest') -> Right (Token DoubleSlashAssignToken "//=" (Position ln col), rest', col + 3)
            ('/' : rest') -> Right (Token DoubleSlashToken "//" (Position ln col), rest', col + 2)
            ('=' : rest') -> Right (Token SlashAssignToken "/=" (Position ln col), rest', col + 2)
            _ -> Right (Token SlashToken "/" (Position ln col), rest, col + 1)
      | c == '%' ->
          case rest of
            ('=' : rest') -> Right (Token PercentAssignToken "%=" (Position ln col), rest', col + 2)
            _ -> Right (Token PercentToken "%" (Position ln col), rest, col + 1)
      | c == '(' -> Right (Token LParenToken "(" (Position ln col), rest, col + 1)
      | c == ')' -> Right (Token RParenToken ")" (Position ln col), rest, col + 1)
      | c == '[' -> Right (Token LBracketToken "[" (Position ln col), rest, col + 1)
      | c == ']' -> Right (Token RBracketToken "]" (Position ln col), rest, col + 1)
      | c == '{' -> Right (Token LBraceToken "{" (Position ln col), rest, col + 1)
      | c == '}' -> Right (Token RBraceToken "}" (Position ln col), rest, col + 1)
      | c == ':' -> Right (Token ColonToken ":" (Position ln col), rest, col + 1)
      | c == ',' -> Right (Token CommaToken "," (Position ln col), rest, col + 1)
      | c == '.' ->
          case rest of
            (nextChar : _)
              | isDigit nextChar ->
                  let (fractionDigits, afterFraction) = span isDigit rest
                      withFraction = "." ++ fractionDigits
                      (exponentPart, tailInput) = parseExponent afterFraction
                      lexeme = withFraction ++ exponentPart
                   in Right (Token FloatToken lexeme (Position ln col), tailInput, col + length lexeme)
            _ -> Right (Token DotToken "." (Position ln col), rest, col + 1)
      | c == '"' ->
          let (strContent, tailInput) = span (\x -> x /= '"' && x /= '\n') rest
              len = length strContent
           in case tailInput of
                ('"' : rest') -> Right (Token StringToken strContent (Position ln col), rest', col + len + 2)
                _ -> Left (UnexpectedCharacter '"')
      | isDigit c ->
          let (digits, afterDigits) = span isDigit (c : rest)
           in case afterDigits of
                ('.' : afterDot) ->
                  case afterDot of
                    (nextChar : _)
                      | isAlpha nextChar || nextChar == '_' -> Right (Token IntegerToken digits (Position ln col), afterDigits, col + length digits)
                    _ ->
                      let (fractionDigits, afterFraction) = span isDigit afterDot
                          withFraction = digits ++ "." ++ fractionDigits
                          (exponentPart, tailInput) = parseExponent afterFraction
                          lexeme = withFraction ++ exponentPart
                       in Right (Token FloatToken lexeme (Position ln col), tailInput, col + length lexeme)
                _ ->
                  let (exponentPart, tailInput) = parseExponent afterDigits
                   in if null exponentPart
                        then Right (Token IntegerToken digits (Position ln col), tailInput, col + length digits)
                        else
                          let lexeme = digits ++ exponentPart
                           in Right (Token FloatToken lexeme (Position ln col), tailInput, col + length lexeme)
      | isAlpha c || c == '_' ->
          let (word, tailInput) = span (\x -> isAlphaNum x || x == '_') (c : rest)
              len = length word
           in Right (Token (keywordOrIdentifier word) word (Position ln col), tailInput, col + len)
      | otherwise -> Left (UnexpectedCharacter c)
