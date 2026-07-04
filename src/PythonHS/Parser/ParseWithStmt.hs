module PythonHS.Parser.ParseWithStmt (parseWithStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseSuiteConfig (ParseSuiteConfig (..))
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))

parseWithStmt :: ParseWithStmtConfig -> Either ParseError (Stmt, [Token])
parseWithStmt config = do
  let parseStatementFn = parseWithStmtStatement config
      pos = parseWithStmtPos config
      rest = parseWithStmtTokenStream config
  (contextManager, afterContextManager) <- parseExpr rest
  case afterContextManager of
    Token {tokenType = AsToken} : Token {tokenType = IdentifierToken, lexeme = varName} : Token {tokenType = ColonToken} : afterColon -> do
      (bodySuite, finalRest) <- parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatementFn, parseSuiteTokenStream = afterColon})
      Right (WithStmt {withStmtContextManager = contextManager, withStmtVarName = Just varName, withStmtBody = bodySuite, withStmtPos = pos}, finalRest)
    Token {tokenType = ColonToken} : afterColon -> do
      (bodySuite, finalRest) <- parseSuite (ParseSuiteConfig {parseSuiteStatement = parseStatementFn, parseSuiteTokenStream = afterColon})
      Right (WithStmt {withStmtContextManager = contextManager, withStmtVarName = Nothing, withStmtBody = bodySuite, withStmtPos = pos}, finalRest)
    Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
