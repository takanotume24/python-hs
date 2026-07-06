module PythonHS.Parser.ParseYieldStmtConfig (ParseYieldStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

-- | Configuration for parsing a yield statement.
data ParseYieldStmtConfig = ParseYieldStmtConfig
  { parseYieldStmtExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseYieldStmtPos :: Position,
    parseYieldStmtTokenStream :: [Token]
  }
