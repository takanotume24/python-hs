module PythonHS.Parser.ParseExceptSuites (parseExceptSuites) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, ExceptToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseExceptSuites :: ([Token] -> Either ParseError ([Stmt], [Token])) -> [Token] -> Either ParseError ([[Stmt]], [Token])
parseExceptSuites parseSuite ts =
  case ts of
    Token ExceptToken _ _ : Token ColonToken _ _ : afterExceptColon -> do
      (exceptSuite, afterExceptSuite) <- parseSuite afterExceptColon
      let afterCurrent = dropLeadingNewlines afterExceptSuite
      case parseExceptSuites parseSuite afterCurrent of
        Right (otherSuites, finalRest) -> Right (exceptSuite : otherSuites, finalRest)
        Left _ -> Right ([exceptSuite], afterExceptSuite)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))
  where
    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
