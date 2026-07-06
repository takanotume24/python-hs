module PythonHS.Parser.ParseExceptSuites (parseExceptSuites) where

import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, ExceptToken, IdentifierToken, NewlineToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExceptSuitesConfig (ParseExceptSuitesConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseExceptSuites :: ParseExceptSuitesConfig -> Either ParseError (ParseResult [ExceptSuite Stmt])
parseExceptSuites config =
  let parseSuite = parseExceptSuitesSuite config
      ts = parseExceptSuitesTokenStream config
   in case ts of
        Token ExceptToken _ exceptPos : restAfterExcept -> do
          clauseResult <- parseExceptClause parseSuite exceptPos restAfterExcept
          let afterCurrent = dropLeadingNewlines (parseResultRemaining clauseResult)
          case parseExceptSuites (config {parseExceptSuitesTokenStream = afterCurrent}) of
            Right suitesResult ->
              Right (ParseResult {parseResultValue = parseResultValue clauseResult : parseResultValue suitesResult, parseResultRemaining = parseResultRemaining suitesResult})
            Left _ ->
              Right (ParseResult {parseResultValue = [parseResultValue clauseResult], parseResultRemaining = parseResultRemaining clauseResult})
        Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
  where
    parseExceptClause parseSuite exceptPos tokens =
      case tokens of
        Token ColonToken _ _ : afterColon -> do
          suiteResult <- parseSuite afterColon
          Right (ParseResult {parseResultValue = ExceptSuite {exceptSuiteType = Nothing, exceptSuiteAlias = Nothing, exceptSuiteBody = parseResultValue suiteResult, exceptSuitePos = exceptPos}, parseResultRemaining = parseResultRemaining suiteResult})
        Token IdentifierToken typeName _ : Token AsToken _ _ : Token IdentifierToken aliasName _ : Token ColonToken _ _ : afterColon -> do
          suiteResult <- parseSuite afterColon
          Right (ParseResult {parseResultValue = ExceptSuite {exceptSuiteType = Just typeName, exceptSuiteAlias = Just aliasName, exceptSuiteBody = parseResultValue suiteResult, exceptSuitePos = exceptPos}, parseResultRemaining = parseResultRemaining suiteResult})
        Token IdentifierToken typeName _ : Token ColonToken _ _ : afterColon -> do
          suiteResult <- parseSuite afterColon
          Right (ParseResult {parseResultValue = ExceptSuite {exceptSuiteType = Just typeName, exceptSuiteAlias = Nothing, exceptSuiteBody = parseResultValue suiteResult, exceptSuitePos = exceptPos}, parseResultRemaining = parseResultRemaining suiteResult})
        Token _ _ pos' : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})

    dropLeadingNewlines (Token NewlineToken _ _ : rest) = dropLeadingNewlines rest
    dropLeadingNewlines rest = rest
