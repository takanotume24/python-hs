module PythonHS.Parser.ParseClassStmt (parseClassStmt) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (ColonToken, IdentifierToken, LParenToken, RParenToken))
import PythonHS.Parser.ParseClassStmtConfig (ParseClassStmtConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))

parseClassStmt :: ParseClassStmtConfig -> [Token] -> Either ParseError (Stmt, [Token])
parseClassStmt config rest =
  let parseSuiteFn = parseClassStmtSuite config
      posClass = parseClassStmtPos config
      className = parseClassStmtName config
   in case rest of
        Token {tokenType = ColonToken} : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteFn afterColon
          Right (ClassDefStmt {classDefStmtName = className, classDefStmtBase = Nothing, classDefStmtBody = bodySuite, classDefStmtPos = posClass}, finalRest)
        Token {tokenType = LParenToken} : Token {tokenType = IdentifierToken, lexeme = baseName} : Token {tokenType = RParenToken} : Token {tokenType = ColonToken} : afterColon -> do
          (bodySuite, finalRest) <- parseSuiteFn afterColon
          Right (ClassDefStmt {classDefStmtName = className, classDefStmtBase = Just baseName, classDefStmtBody = bodySuite, classDefStmtPos = posClass}, finalRest)
        Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
