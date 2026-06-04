module PythonHS.Parser.ParseWithStmt (parseWithStmt) where

import PythonHS.AST.Stmt (Stmt (WithStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, ColonToken, IdentifierToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseSuite (parseSuite)

parseWithStmt :: ([Token] -> Either ParseError (Stmt, [Token])) -> Position -> [Token] -> Either ParseError (Stmt, [Token])
parseWithStmt parseStatement pos rest = do
  (contextManager, afterContextManager) <- parseExpr rest
  case afterContextManager of
    Token AsToken _ _ : Token IdentifierToken varName _ : Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite parseStatement afterColon
      Right (WithStmt contextManager (Just varName) bodySuite pos, finalRest)
    Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite parseStatement afterColon
      Right (WithStmt contextManager Nothing bodySuite pos, finalRest)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))