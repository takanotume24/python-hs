module PythonHS.Parser.ParseMatchStmtConfig (ParseMatchStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

-- | Configuration for parsing a match statement.
data ParseMatchStmtConfig = ParseMatchStmtConfig
  { parseMatchStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseMatchStmtSuite :: [Token] -> Either ParseError (ParseResult [Stmt]),
    parseMatchStmtPos :: Position,
    parseMatchStmtTokenStream :: [Token]
  }
