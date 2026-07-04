module PythonHS.Lexer.ScanTokenStep (scanTokenStep) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier)
import PythonHS.Lexer.LexerError (LexerError (..))
import PythonHS.Lexer.ParseExponent (ParseExponentResult (..), parseExponent)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        AtToken,
        ColonAssignToken,
        ColonToken,
        CommaToken,
        DotToken,
        DoubleSlashAssignToken,
        DoubleSlashToken,
        EqToken,
        FloatToken,
        GtToken,
        GteToken,
        IntegerToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        LtToken,
        LteToken,
        MinusAssignToken,
        MinusToken,
        NotEqToken,
        PercentAssignToken,
        PercentToken,
        PipeToken,
        PlusAssignToken,
        PlusToken,
        RBraceToken,
        RBracketToken,
        RParenToken,
        SlashAssignToken,
        SlashToken,
        StarAssignToken,
        StarToken,
        StringToken
      ),
  )

scanTokenStep :: ScanTokenStepConfig -> Either LexerError ScanTokenStepResult
scanTokenStep config =
  let src = scanTokenStepSource config
      Position ln col = scanTokenStepPosition config
   in case src of
        [] -> Left (UnexpectedCharacter {unexpectedChar = ' '})
        (c : rest)
          | c == '=' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token EqToken "==" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token AssignToken "=" (Position ln col)) rest (Position ln (col + 1)))
          | c == '!' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token NotEqToken "!=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Left (UnexpectedCharacter {unexpectedChar = '!'})
          | c == '<' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token LteToken "<=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token LtToken "<" (Position ln col)) rest (Position ln (col + 1)))
          | c == '>' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token GteToken ">=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token GtToken ">" (Position ln col)) rest (Position ln (col + 1)))
          | c == '+' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token PlusAssignToken "+=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token PlusToken "+" (Position ln col)) rest (Position ln (col + 1)))
          | c == '-' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token MinusAssignToken "-=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token MinusToken "-" (Position ln col)) rest (Position ln (col + 1)))
          | c == '*' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token StarAssignToken "*=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token StarToken "*" (Position ln col)) rest (Position ln (col + 1)))
          | c == '/' ->
              case rest of
                ('/' : '=' : rest') -> Right (ScanTokenStepResult (Token DoubleSlashAssignToken "//=" (Position ln col)) rest' (Position ln (col + 3)))
                ('/' : rest') -> Right (ScanTokenStepResult (Token DoubleSlashToken "//" (Position ln col)) rest' (Position ln (col + 2)))
                ('=' : rest') -> Right (ScanTokenStepResult (Token SlashAssignToken "/=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token SlashToken "/" (Position ln col)) rest (Position ln (col + 1)))
          | c == '%' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token PercentAssignToken "%=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token PercentToken "%" (Position ln col)) rest (Position ln (col + 1)))
          | c == '|' -> Right (ScanTokenStepResult (Token PipeToken "|" (Position ln col)) rest (Position ln (col + 1)))
          | c == '@' -> Right (ScanTokenStepResult (Token AtToken "@" (Position ln col)) rest (Position ln (col + 1)))
          | c == '(' -> Right (ScanTokenStepResult (Token LParenToken "(" (Position ln col)) rest (Position ln (col + 1)))
          | c == ')' -> Right (ScanTokenStepResult (Token RParenToken ")" (Position ln col)) rest (Position ln (col + 1)))
          | c == '[' -> Right (ScanTokenStepResult (Token LBracketToken "[" (Position ln col)) rest (Position ln (col + 1)))
          | c == ']' -> Right (ScanTokenStepResult (Token RBracketToken "]" (Position ln col)) rest (Position ln (col + 1)))
          | c == '{' -> Right (ScanTokenStepResult (Token LBraceToken "{" (Position ln col)) rest (Position ln (col + 1)))
          | c == '}' -> Right (ScanTokenStepResult (Token RBraceToken "}" (Position ln col)) rest (Position ln (col + 1)))
          | c == ':' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token ColonAssignToken ":=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token ColonToken ":" (Position ln col)) rest (Position ln (col + 1)))
          | c == ',' -> Right (ScanTokenStepResult (Token CommaToken "," (Position ln col)) rest (Position ln (col + 1)))
          | c == '.' ->
              case rest of
                (nextChar : _)
                  | isDigit nextChar ->
                      let spanRes = span isDigit rest
                          fractionDigits = fst spanRes
                          afterFraction = snd spanRes
                          withFraction = "." ++ fractionDigits
                          ParseExponentResult {parseExponentResultDigits = exponentPart, parseExponentResultRemaining = tailInput} = parseExponent afterFraction
                          lexeme = withFraction ++ exponentPart
                       in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
                _ -> Right (ScanTokenStepResult (Token DotToken "." (Position ln col)) rest (Position ln (col + 1)))
          | c == '"' ->
              let spanRes = span (\x -> x /= '"' && x /= '\n') rest
                  strContent = fst spanRes
                  tailInput = snd spanRes
                  len = length strContent
               in case tailInput of
                    ('"' : rest') -> Right (ScanTokenStepResult (Token StringToken strContent (Position ln col)) rest' (Position ln (col + len + 2)))
                    _ -> Left (UnexpectedCharacter {unexpectedChar = '"'})
          | c == '\'' ->
              let spanRes = span (\x -> x /= '\'' && x /= '\n') rest
                  strContent = fst spanRes
                  tailInput = snd spanRes
                  len = length strContent
               in case tailInput of
                    ('\'' : rest') -> Right (ScanTokenStepResult (Token StringToken strContent (Position ln col)) rest' (Position ln (col + len + 2)))
                    _ -> Left (UnexpectedCharacter {unexpectedChar = '\''})
          | isDigit c ->
              let spanResDigits = span isDigit (c : rest)
                  digits = fst spanResDigits
                  afterDigits = snd spanResDigits
               in case afterDigits of
                    ('.' : afterDot) ->
                      case afterDot of
                        (nextChar : _)
                          | isAlpha nextChar || nextChar == '_' -> Right (ScanTokenStepResult (Token IntegerToken digits (Position ln col)) afterDigits (Position ln (col + length digits)))
                        _ ->
                          let spanResFraction = span isDigit afterDot
                              fractionDigits = fst spanResFraction
                              afterFraction = snd spanResFraction
                              withFraction = digits ++ "." ++ fractionDigits
                              ParseExponentResult {parseExponentResultDigits = exponentPart, parseExponentResultRemaining = tailInput} = parseExponent afterFraction
                              lexeme = withFraction ++ exponentPart
                           in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
                    _ ->
                      let ParseExponentResult {parseExponentResultDigits = exponentPart, parseExponentResultRemaining = tailInput} = parseExponent afterDigits
                       in if null exponentPart
                            then Right (ScanTokenStepResult (Token IntegerToken digits (Position ln col)) tailInput (Position ln (col + length digits)))
                            else
                              let lexeme = digits ++ exponentPart
                               in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
          | isAlpha c || c == '_' ->
              let spanRes = span (\x -> isAlphaNum x || x == '_') (c : rest)
                  word = fst spanRes
                  tailInput = snd spanRes
                  len = length word
               in Right (ScanTokenStepResult (Token (keywordOrIdentifier word) word (Position ln col)) tailInput (Position ln (col + len)))
          | otherwise -> Left (UnexpectedCharacter {unexpectedChar = c})
