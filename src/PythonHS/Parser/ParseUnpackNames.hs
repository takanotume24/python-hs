module PythonHS.Parser.ParseUnpackNames (parseUnpackNames) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( CommaToken,
        IdentifierToken
      ),
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..))

parseUnpackNames :: ParseUnpackNamesConfig -> Either ParseError ([String], [Token])
parseUnpackNames config =
  case (unpackNamesAcc config, unpackNamesTokens config) of
    (acc, Token IdentifierToken name _ : Token CommaToken _ _ : rest) ->
      parseUnpackNames (ParseUnpackNamesConfig (acc ++ [name]) rest)
    (acc, Token IdentifierToken name _ : rest) ->
      Right (acc ++ [name], rest)
    (_, Token _ _ pos' : _) ->
      Left (ExpectedExpression pos')
    _ ->
      Left (ExpectedExpression (Position 0 0))
