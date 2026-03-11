module PythonHS.Parser.ParseExceptSuites (parseExceptSuites) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, ExceptToken, IdentifierToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseExceptSuites :: ([Token] -> Either ParseError ([Stmt], [Token])) -> [Token] -> Either ParseError ([(Maybe String, Maybe String, [Stmt], Position)], [Token])
parseExceptSuites parseSuite ts =
  case ts of
    Token ExceptToken _ exceptPos : restAfterExcept -> do
      (maybeTypeName, maybeAliasName, afterExceptHeader) <- parseExceptHeader restAfterExcept
      (exceptSuite, afterExceptSuite) <- parseSuite afterExceptHeader
      let afterCurrent = dropLeadingNewlines afterExceptSuite
      case parseExceptSuites parseSuite afterCurrent of
        Right (otherSuites, finalRest) -> Right ((maybeTypeName, maybeAliasName, exceptSuite, exceptPos) : otherSuites, finalRest)
        Left _ -> Right ([(maybeTypeName, maybeAliasName, exceptSuite, exceptPos)], afterExceptSuite)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    parseExceptHeader tokens =
      case tokens of
        Token ColonToken _ _ : afterColon -> Right (Nothing, Nothing, afterColon)
        Token IdentifierToken typeName _ : Token AsToken _ _ : Token IdentifierToken aliasName _ : Token ColonToken _ _ : afterColon ->
          Right (Just typeName, Just aliasName, afterColon)
        Token IdentifierToken typeName _ : Token ColonToken _ _ : afterColon ->
          Right (Just typeName, Nothing, afterColon)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))

    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
