module PythonHS.Parser.ParseCompoundStmtConfig (ParseCompoundStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseCompoundStmtConfig = ParseCompoundStmtConfig
  { parseCompoundStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseCompoundStmtStatement :: [Token] -> Either ParseError (ParseResult Stmt),
    parseCompoundStmtTokenStream :: [Token]
  }
