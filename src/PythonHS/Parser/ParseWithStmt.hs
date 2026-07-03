module PythonHS.Parser.ParseWithStmt (parseWithStmt) where

import PythonHS.AST.Stmt (Stmt (WithStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseSuite (parseSuite)
import PythonHS.Parser.ParseWithStmtConfig (ParseWithStmtConfig (..))

parseWithStmt :: ParseWithStmtConfig -> [Token] -> Either ParseError (Stmt, [Token])
parseWithStmt config rest = do
  let parseStatementFn = parseWithStmtStatement config
      pos = parseWithStmtPos config
  (contextManager, afterContextManager) <- parseExpr rest
  case afterContextManager of
    Token AsToken _ _ : Token IdentifierToken varName _ : Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite parseStatementFn afterColon
      Right (WithStmt contextManager (Just varName) bodySuite pos, finalRest)
    Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite parseStatementFn afterColon
      Right (WithStmt contextManager Nothing bodySuite pos, finalRest)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))
