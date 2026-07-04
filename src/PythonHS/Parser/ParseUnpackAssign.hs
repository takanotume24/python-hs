module PythonHS.Parser.ParseUnpackAssign (parseUnpackAssign) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken
      ),
  )
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..))
import PythonHS.Parser.ParseUnpackNames (parseUnpackNames)
import PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..))

parseUnpackAssign :: ParseUnpackAssignConfig -> Either ParseError (Stmt, [Token])
parseUnpackAssign config = do
  let firstName = parseUnpackAssignFirstName config
      pos = parseUnpackAssignPos config
      rest = parseUnpackAssignTokenStream config
  (names, afterNames) <- parseUnpackNames (ParseUnpackNamesConfig {unpackNamesAcc = [firstName], unpackNamesTokens = rest})
  case afterNames of
    Token {tokenType = AssignToken} : afterAssign -> do
      (valueExpr, remaining) <- parseExpr afterAssign
      Right (AssignUnpackStmt {assignUnpackStmtNames = names, assignUnpackStmtValue = valueExpr, assignUnpackStmtPos = pos}, remaining)
    Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
