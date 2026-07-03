module PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

-- | Configuration for parsing a match statement.
data ParseMatchStmtConfig = ParseMatchStmtConfig
  { parseMatchStmtExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseMatchStmtSuite :: [Token] -> Either ParseError ([Stmt], [Token])
  }
