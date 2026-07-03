module PythonHS.Parser.ParseClassStmt (parseClassStmt) where

import PythonHS.AST.Stmt (Stmt (ClassDefStmt), Stmt)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, IdentifierToken, LParenToken, RParenToken))
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))

parseClassStmt :: ParseClassStmtConfig -> [Token] -> Either ParseError (Stmt, [Token])
parseClassStmt config rest =
  let parseSuiteFn = parseClassStmtSuite config
      posClass = parseClassStmtPos config
      className = parseClassStmtName config
   in case rest of
        Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteFn afterColon
          Right (ClassDefStmt className Nothing bodySuite posClass, finalRest)
        Token LParenToken _ _ : Token IdentifierToken baseName _ : Token RParenToken _ _ : Token ColonToken _ _ : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteFn afterColon
          Right (ClassDefStmt className (Just baseName) bodySuite posClass, finalRest)
        Token _ _ pos' : _ -> Left (ExpectedExpression pos')
        _ -> Left (ExpectedExpression (Position 0 0))
