module PythonHS.Parser.ParseUnpackNames (parseUnpackNames) where

import PythonHS.Lexer.Position (Position(Position))
import PythonHS.Lexer.Token (Token(Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( CommaToken
      , IdentifierToken
      )
  )
import PythonHS.Parser.ParseError (ParseError(ExpectedExpression))

parseUnpackNames :: [String] -> [Token] -> Either ParseError ([String], [Token])
parseUnpackNames acc (Token IdentifierToken name _ : Token CommaToken _ _ : rest) =
  parseUnpackNames (acc ++ [name]) rest
parseUnpackNames acc (Token IdentifierToken name _ : rest) =
  Right (acc ++ [name], rest)
parseUnpackNames _ (Token _ _ pos' : _) = Left (ExpectedExpression pos')
parseUnpackNames _ _ = Left (ExpectedExpression (Position 0 0))
