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
import PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..))

parseUnpackNames :: ParseUnpackNamesConfig -> Either ParseError ([String], [Token])
parseUnpackNames config =
  case (unpackNamesAcc config, unpackNamesTokens config) of
    (acc, Token {tokenType = IdentifierToken, lexeme = name} : Token {tokenType = CommaToken} : rest) ->
      parseUnpackNames (ParseUnpackNamesConfig {unpackNamesAcc = acc ++ [name], unpackNamesTokens = rest})
    (acc, Token {tokenType = IdentifierToken, lexeme = name} : rest) ->
      Right (acc ++ [name], rest)
    (_, Token {position = pos'} : _) ->
      Left (ExpectedExpression {parseErrorPosition = pos'})
    _ ->
      Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
