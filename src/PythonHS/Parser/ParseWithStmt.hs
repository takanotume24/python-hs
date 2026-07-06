module PythonHS.Parser.ParseWithStmt (parseWithStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))

parseWithStmt :: ParseWithStmtConfig -> Either ParseError (ParseResult Stmt)
parseWithStmt config = do
  let parseStatementFn = parseWithStmtStatement config
      pos = parseWithStmtPos config
      rest = parseWithStmtTokenStream config
  exprResult <- parseExpr rest
  let contextManager = parseResultValue exprResult
      afterContextManager = parseResultRemaining exprResult
  case afterContextManager of
    Token {tokenType = AsToken} : Token {tokenType = IdentifierToken, lexeme = varName} : Token {tokenType = ColonToken} : afterColon -> do
      suiteResult <- parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatementFn, parseSuiteTokenStream = afterColon})
      let bodySuite = parseResultValue suiteResult
          finalRest = parseResultRemaining suiteResult
      Right (ParseResult {parseResultValue = WithStmt {withStmtContextManager = contextManager, withStmtVarName = Just varName, withStmtBody = bodySuite, withStmtPos = pos}, parseResultRemaining = finalRest})
    Token {tokenType = ColonToken} : afterColon -> do
      suiteResult <- parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatementFn, parseSuiteTokenStream = afterColon})
      let bodySuite = parseResultValue suiteResult
          finalRest = parseResultRemaining suiteResult
      Right (ParseResult {parseResultValue = WithStmt {withStmtContextManager = contextManager, withStmtVarName = Nothing, withStmtBody = bodySuite, withStmtPos = pos}, parseResultRemaining = finalRest})
    Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
