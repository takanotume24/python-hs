module PythonHS.Parser.ParseDecoratedStmtConfig (ParseDecoratedStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

-- | Configuration for parsing a decorated statement.
data ParseDecoratedStmtConfig = ParseDecoratedStmtConfig
  { parseDecoratedStmtExpr :: [Token] -> Either ParseError (Expr, [Token]),
    parseDecoratedStmtStatement :: [Token] -> Either ParseError (Stmt, [Token]),
    parseDecoratedStmtPos :: Position
  }
