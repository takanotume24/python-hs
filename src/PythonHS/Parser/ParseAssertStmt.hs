module PythonHS.Parser.ParseAssertStmt (parseAssertStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (AssertToken))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseAssertStmt :: [Token] -> Either ParseError (ParseResult Stmt)
parseAssertStmt (Token {tokenType = AssertToken, position = pos} : rest) = do
  conditionResult <- parseExpr rest
  let condition = parseResultValue conditionResult
      remaining = parseResultRemaining conditionResult
  Right (ParseResult {parseResultValue = AssertStmt {assertStmtCond = condition, assertStmtPos = pos}, parseResultRemaining = remaining})
parseAssertStmt _ =
  Left $ ExpectedExpression {parseErrorPosition = Position {line = 0, column = 0}}
