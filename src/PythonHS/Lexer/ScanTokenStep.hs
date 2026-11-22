module PythonHS.Lexer.ScanTokenStep (scanTokenStep) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier)
import PythonHS.Lexer.LexerError (LexerError (..))
import PythonHS.Lexer.ParseExponent (ParseExponentResult (..), parseExponent)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.QuotedStringResult (QuotedStringResult (..))
import PythonHS.Lexer.ScanFString (scanFString)
import PythonHS.Lexer.ScanOperatorToken (scanOperatorToken)
import PythonHS.Lexer.ScanOperatorTokenConfig (ScanOperatorTokenConfig (..))
import PythonHS.Lexer.ScanQuotedString (scanQuotedString)
import PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AtToken,
        ColonAssignToken,
        ColonToken,
        CommaToken,
        DotToken,
        FStringToken,
        FloatToken,
        IntegerToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        RBraceToken,
        RBracketToken,
        RParenToken,
        StringToken
      ),
  )
import PythonHS.Lexer.UnescapeStringLiteral (unescapeStringLiteral)

scanTokenStep :: ScanTokenStepConfig -> Either LexerError ScanTokenStepResult
scanTokenStep config =
  let src = scanTokenStepSource config
      Position ln col = scanTokenStepPosition config
   in case src of
        [] -> Left (UnexpectedCharacter {unexpectedChar = ' '})
        (c : rest)
          | Just result <-
              scanOperatorToken
                ScanOperatorTokenConfig
                  { scanOperatorTokenCharacter = c,
                    scanOperatorTokenRest = rest,
                    scanOperatorTokenPosition = Position ln col
                  } ->
              result
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
          | c == '"' || c == '\'' -> case scanQuotedString c rest of
              Just result ->
                let rawContent = quotedStringContent result
                 in Right (ScanTokenStepResult (Token StringToken (unescapeStringLiteral rawContent) (Position ln col)) (quotedStringRemaining result) (Position ln (col + length rawContent + 2)))
              Nothing -> Left (UnexpectedCharacter {unexpectedChar = c})
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
          | (c == 'f' || c == 'F'),
            (quoteChar : afterQuote) <- rest,
            quoteChar == '"' || quoteChar == '\'' ->
              case scanFString quoteChar afterQuote of
                Just (strContent, tailInput) -> Right (ScanTokenStepResult (Token FStringToken strContent (Position ln col)) tailInput (Position ln (col + length strContent + 3)))
                Nothing -> Left (UnexpectedCharacter {unexpectedChar = quoteChar})
          | isAlpha c || c == '_' ->
              let spanRes = span (\x -> isAlphaNum x || x == '_') (c : rest)
                  word = fst spanRes
                  tailInput = snd spanRes
                  len = length word
               in Right (ScanTokenStepResult (Token (keywordOrIdentifier word) word (Position ln col)) tailInput (Position ln (col + len)))
          | otherwise -> Left (UnexpectedCharacter {unexpectedChar = c})
