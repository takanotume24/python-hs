module PythonHS.Parser.ParseCallStatement (parseCallStatement) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, NewlineToken))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.Parser.ParseExpr (parseExpr)
import PythonHS.Parser.ParseResult (ParseResult (..))

parseCallStatement :: [Token] -> Maybe (ParseResult Stmt)
parseCallStatement tokens =
  case parseExpr tokens of
    Right exprResult
      | isCallExpr (parseResultValue exprResult) && isStatementEnd (parseResultRemaining exprResult) ->
          Just (ParseResult {parseResultValue = makeExprStmt (parseResultValue exprResult), parseResultRemaining = parseResultRemaining exprResult})
    _ -> Nothing
  where
    makeExprStmt expr =
      ExprStmt {exprStmtValue = expr, exprStmtPos = exprPos expr}

    isCallExpr expr = case expr of
      CallExpr {} -> True
      CallValueExpr {} -> True
      _ -> False

    isStatementEnd restTokens = case restTokens of
      [] -> True
      Token {tokenType = NewlineToken} : _ -> True
      Token {tokenType = EOFToken} : _ -> True
      _ -> False
