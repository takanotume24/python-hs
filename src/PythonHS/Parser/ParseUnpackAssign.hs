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
import PythonHS.Parser.ParseResult (ParseResult (..))
import PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..))
import PythonHS.Parser.ParseUnpackNames (parseUnpackNames)
import PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..))

parseUnpackAssign :: ParseUnpackAssignConfig -> Either ParseError (ParseResult Stmt)
parseUnpackAssign config = do
  let firstName = parseUnpackAssignFirstName config
      pos = parseUnpackAssignPos config
      rest = parseUnpackAssignTokenStream config
  namesResult <- parseUnpackNames (ParseUnpackNamesConfig {unpackNamesAcc = [firstName], unpackNamesTokens = rest})
  let names = parseResultValue namesResult
      afterNames = parseResultRemaining namesResult
  case afterNames of
    Token {tokenType = AssignToken} : afterAssign -> do
      valueResult <- parseExpr afterAssign
      let valueExpr = parseResultValue valueResult
          remaining = parseResultRemaining valueResult
      Right (ParseResult {parseResultValue = AssignUnpackStmt {assignUnpackStmtNames = names, assignUnpackStmtValue = valueExpr, assignUnpackStmtPos = pos}, parseResultRemaining = remaining})
    Token {position = pos'} : _ -> Left (ExpectedExpression {parseErrorPosition = pos'})
    _ -> Left (ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}})
