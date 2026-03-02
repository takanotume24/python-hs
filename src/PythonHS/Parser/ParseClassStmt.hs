module PythonHS.Parser.ParseClassStmt (parseClassStmt) where

import PythonHS.AST.Stmt (Stmt (ClassDefStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, IdentifierToken, LParenToken, RParenToken))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseClassStmt :: ([Token] -> Either ParseError ([Stmt], [Token])) -> Position -> String -> [Token] -> Either ParseError (Stmt, [Token])
parseClassStmt parseSuite posClass className rest =
  case rest of
    Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite afterColon
      Right (ClassDefStmt className Nothing bodySuite posClass, finalRest)
    Token LParenToken _ _ : Token IdentifierToken baseName _ : Token RParenToken _ _ : Token ColonToken _ _ : afterColon -> do
      (bodySuite, finalRest) <- parseSuite afterColon
      Right (ClassDefStmt className (Just baseName) bodySuite posClass, finalRest)
    Token _ _ pos' : _ -> Left (ExpectedExpression pos')
    _ -> Left (ExpectedExpression (Position 0 0))
