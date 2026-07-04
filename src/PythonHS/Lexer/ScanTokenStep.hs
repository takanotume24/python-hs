module PythonHS.Lexer.ScanTokenStep (scanTokenStep) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier)
import PythonHS.Lexer.LexerError (LexerError (UnexpectedCharacter))
import PythonHS.Lexer.ParseExponent (parseExponent)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
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
         ColonAssignToken,
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
         PipeToken,
         AtToken,
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

scanTokenStep :: ScanTokenStepConfig -> Either LexerError ScanTokenStepResult
scanTokenStep config =
  let src = scanTokenStepSource config
      Position ln col = scanTokenStepPosition config
   in case src of
        [] -> Left (UnexpectedCharacter ' ')
        (c : rest)
          | c == '=' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token EqToken "==" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Right (ScanTokenStepResult (Token AssignToken "=" (Position ln col)) rest (Position ln (col + 1)))
          | c == '!' ->
              case rest of
                ('=' : rest') -> Right (ScanTokenStepResult (Token NotEqToken "!=" (Position ln col)) rest' (Position ln (col + 2)))
                _ -> Left (UnexpectedCharacter '!')
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
                      let (fractionDigits, afterFraction) = span isDigit rest
                          withFraction = "." ++ fractionDigits
                          (exponentPart, tailInput) = parseExponent afterFraction
                          lexeme = withFraction ++ exponentPart
                       in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
                _ -> Right (ScanTokenStepResult (Token DotToken "." (Position ln col)) rest (Position ln (col + 1)))
          | c == '"' ->
              let (strContent, tailInput) = span (\x -> x /= '"' && x /= '\n') rest
                  len = length strContent
               in case tailInput of
                    ('"' : rest') -> Right (ScanTokenStepResult (Token StringToken strContent (Position ln col)) rest' (Position ln (col + len + 2)))
                    _ -> Left (UnexpectedCharacter '"')
          | c == '\'' ->
              let (strContent, tailInput) = span (\x -> x /= '\'' && x /= '\n') rest
                  len = length strContent
               in case tailInput of
                    ('\'' : rest') -> Right (ScanTokenStepResult (Token StringToken strContent (Position ln col)) rest' (Position ln (col + len + 2)))
                    _ -> Left (UnexpectedCharacter '\'')
          | isDigit c ->
              let (digits, afterDigits) = span isDigit (c : rest)
               in case afterDigits of
                    ('.' : afterDot) ->
                      case afterDot of
                        (nextChar : _)
                          | isAlpha nextChar || nextChar == '_' -> Right (ScanTokenStepResult (Token IntegerToken digits (Position ln col)) afterDigits (Position ln (col + length digits)))
                        _ ->
                          let (fractionDigits, afterFraction) = span isDigit afterDot
                              withFraction = digits ++ "." ++ fractionDigits
                              (exponentPart, tailInput) = parseExponent afterFraction
                              lexeme = withFraction ++ exponentPart
                           in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
                    _ ->
                      let (exponentPart, tailInput) = parseExponent afterDigits
                       in if null exponentPart
                            then Right (ScanTokenStepResult (Token IntegerToken digits (Position ln col)) tailInput (Position ln (col + length digits)))
                            else
                              let lexeme = digits ++ exponentPart
                               in Right (ScanTokenStepResult (Token FloatToken lexeme (Position ln col)) tailInput (Position ln (col + length lexeme)))
          | isAlpha c || c == '_' ->
              let (word, tailInput) = span (\x -> isAlphaNum x || x == '_') (c : rest)
                  len = length word
               in Right (ScanTokenStepResult (Token (keywordOrIdentifier word) word (Position ln col)) tailInput (Position ln (col + len)))
          | otherwise -> Left (UnexpectedCharacter c)
