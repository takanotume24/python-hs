module PythonHS.Parser.ParseUnpackNames (parseUnpackNames) where

import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( CommaToken,
        IdentifierToken
      ),
  )
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..))

parseUnpackNames :: ParseUnpackNamesConfig -> Either ParseError (ParseResult [String])
parseUnpackNames config =
  let acc = unpackNamesAcc config
   in case unpackNamesTokens config of
        Token {tokenType = IdentifierToken, lexeme = name} : Token {tokenType = CommaToken} : rest ->
          parseUnpackNames (ParseUnpackNamesConfig {unpackNamesAcc = acc ++ [name], unpackNamesTokens = rest})
        Token {tokenType = IdentifierToken, lexeme = name} : rest ->
          Right (ParseResult {parseResultValue = acc ++ [name], parseResultRemaining = rest})
        Token {position = pos'} : _ ->
          Left (ExpectedExpression {parseErrorPosition = pos'})
        _ ->
          Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
