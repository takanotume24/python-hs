module PythonHS.Parser.ParseIdentifier (parseIdentifier) where

import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (IdentifierToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseIdentifier :: [Token] -> Either ParseError (ParseResult String)
parseIdentifier ts =
  case ts of
    Token IdentifierToken name _ : rest -> Right (ParseResult {parseResultValue = name, parseResultRemaining = rest})
    tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
    [] -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
