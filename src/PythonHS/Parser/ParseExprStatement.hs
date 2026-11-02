module PythonHS.Parser.ParseExprStatement (parseExprStatement) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseExprStatement :: [Token] -> Maybe (ParseResult Stmt)
parseExprStatement tokens =
  case parseExpr tokens of
    Right exprResult
      | isStatementExpr (parseResultValue exprResult) && isStatementEnd (parseResultRemaining exprResult) ->
          Just (ParseResult {parseResultValue = ExprStmt {exprStmtValue = parseResultValue exprResult, exprStmtPos = exprPos (parseResultValue exprResult)}, parseResultRemaining = parseResultRemaining exprResult})
    _ -> Nothing
  where
    isStatementExpr expr = case expr of
      IdentifierExpr {identifierExprName = identifierName} -> elem '.' identifierName
      CallExpr {} -> True
      CallValueExpr {} -> True
      _ -> False

    isStatementEnd restTokens = case restTokens of
      [] -> True
      Token {tokenType = NewlineToken} : _ -> True
      Token {tokenType = EOFToken} : _ -> True
      _ -> False
