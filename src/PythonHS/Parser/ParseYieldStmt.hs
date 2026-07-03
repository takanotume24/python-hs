module PythonHS.Parser.ParseYieldStmt (parseYieldStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (YieldFromStmt, YieldStmt))
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (FromToken))
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..))

parseYieldStmt :: ParseYieldStmtConfig -> [Token] -> Either ParseError (Stmt, [Token])
parseYieldStmt config tokenStream =
  let parseExprFn = parseYieldStmtExpr config
      pos = parseYieldStmtPos config
   in case tokenStream of
        Token FromToken _ _ : rest -> do
          (valueExpr, remaining) <- parseExprFn rest
          Right (YieldFromStmt valueExpr pos, remaining)
        _ -> do
          (valueExpr, remaining) <- parseExprFn tokenStream
          Right (YieldStmt valueExpr pos, remaining)
