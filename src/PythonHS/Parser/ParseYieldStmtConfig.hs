module PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

-- | Configuration for parsing a yield statement.
data ParseYieldStmtConfig = ParseYieldStmtConfig
  { parseYieldStmtExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseYieldStmtPos :: Position,
    parseYieldStmtTokenStream :: [Token]
  }
