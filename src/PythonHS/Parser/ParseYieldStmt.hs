module PythonHS.Parser.ParseYieldStmt (parseYieldStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (YieldFromStmt, YieldStmt))
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (FromToken))
import PythonHS.Parser.ParseError (ParseError)

parseYieldStmt :: ([Token] -> Either ParseError (Expr, [Token])) -> Position -> [Token] -> Either ParseError (Stmt, [Token])
parseYieldStmt parseExpr pos tokenStream =
  case tokenStream of
    Token FromToken _ _ : rest -> do
      (valueExpr, remaining) <- parseExpr rest
      Right (YieldFromStmt valueExpr pos, remaining)
    _ -> do
      (valueExpr, remaining) <- parseExpr tokenStream
      Right (YieldStmt valueExpr pos, remaining)
